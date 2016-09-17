unit FB25Events;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows, {$ENDIF}Classes, SysUtils, IB, FBClientAPI, FB25ClientAPI, FB25Attachment, IBExternals,
  IBHeader, syncobjs, FBActivityMonitor;

type
  { TFB25Events }

  TFB25Events = class(TActivityReporter,IEvents)
  private
    FEventBuffer: PChar;
    FEventBufferLen: integer;
    FEventID: ISC_LONG;
    FResultBuffer: PChar;
    FEvents: TStringList;
    FDBHandle: TISC_DB_HANDLE;
    FAttachment: IAttachment;
    FCriticalSection: TCriticalSection;
    FEventHandlerThread: TObject;
    FEventHandler: TEventHandler;
    FInWaitState: boolean;
    procedure CancelEvents(Force: boolean = false);
    procedure EventSignaled;
    procedure CreateEventBlock;
  public
    constructor Create(DBAttachment: TFB25Attachment; Events: TStrings);
    destructor Destroy; override;

    {IEvents}
    procedure GetEvents(EventNames: TStrings);
    procedure SetEvents(EventNames: TStrings); overload;
    procedure SetEvents(Event: string); overload;
    procedure Cancel;
    function ExtractEventCounts: TEventCounts;
    procedure WaitForEvent;
    procedure AsyncWaitForEvent(EventHandler: TEventHandler);
    function GetAttachment: IAttachment;
  end;

implementation

uses  FBMessages;

const
  MaxEvents = 15;

type

  { TEventHandlerThread }

  TEventHandlerThread = class(TThread)
  private
    FOwner: TFB25Events;
     {$IFDEF WINDOWS}
    {Make direct use of Windows API as TEventObject don't seem to work under
     Windows!}
    FEventHandler: THandle;
    {$ELSE}
    FEventWaiting: TEventObject;
    {$ENDIF}
    procedure HandleEventSignalled(length: short; updated: PChar);
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TFB25Events);
    destructor Destroy; override;
    procedure Terminate;
  end;

  {This procedure is used for the event call back - note the cdecl }

 procedure IBEventCallback( ptr: pointer; length: short; updated: PChar); cdecl;
 begin
   if (ptr = nil) or (length = 0) or (updated = nil) then
     Exit;
   { Handle events asynchronously in second thread }
   TEventHandlerThread(ptr).HandleEventSignalled(length,updated);
 end;

 { TEventHandler }

procedure TEventHandlerThread.HandleEventSignalled(length: short; updated: PChar);
begin
  FOwner.FCriticalSection.Enter;
  try
    Move(Updated[0], FOwner.FResultBuffer[0], Length);
    {$IFDEF WINDOWS}
    SetEvent(FEventHandler);
    {$ELSE}
    FEventWaiting.SetEvent;
    {$ENDIF}
  finally
    FOwner.FCriticalSection.Leave
  end;
end;

procedure TEventHandlerThread.Execute;
begin
  while not Terminated do
  begin
    {$IFDEF WINDOWS}
    WaitForSingleObject(FEventHandler,INFINITE);
    {$ELSE}
    FEventWaiting.WaitFor(INFINITE);
    {$ENDIF}

    if not Terminated  then
      FOwner.EventSignaled;
  end;
end;

constructor TEventHandlerThread.Create(Owner: TFB25Events);
var
  PSa : PSecurityAttributes;
{$IFDEF WINDOWS}
  Sd : TSecurityDescriptor;
  Sa : TSecurityAttributes;
begin
  InitializeSecurityDescriptor(@Sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@Sd,true,nil,false);
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := @Sd;
  Sa.bInheritHandle := true;
  PSa := @Sa;
{$ELSE}
  GUID : TGUID;
begin
  PSa:= nil;
{$ENDIF}
  inherited Create(true);
  FOwner := Owner;
  {$IFDEF WINDOWS}
  FEventHandler := CreateEvent(PSa,false,true,nil);
  {$ELSE}
  CreateGuid(GUID);
  FEventWaiting := TEventObject.Create(PSa,false,false,GUIDToString(GUID));
  {$ENDIF}
  FreeOnTerminate := true;
  Start;
end;

destructor TEventHandlerThread.Destroy;
begin
{$IFDEF WINDOWS}
  CloseHandle(FEventHandler);
{$ELSE}
  if assigned(FEventWaiting) then FEventWaiting.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TEventHandlerThread.Terminate;
begin
  inherited Terminate;
{$IFDEF WINDOWS}
  SetEvent(FEventHandler);
{$ELSE}
  FEventWaiting.SetEvent;
{$ENDIF}
end;

  { TFB25Events }

function TFB25Events.ExtractEventCounts: TEventCounts;
var EventCountList: TStatusVector;
    i: integer;
    j: integer;
begin
  SetLength(Result,0);
  if FResultBuffer = nil then Exit;

  with Firebird25ClientAPI do
     isc_event_counts( @EventCountList, FEventBufferLen, FEventBuffer, FResultBuffer);
  j := 0;
  for i := 0 to FEvents.Count - 1 do
  begin
    if EventCountList[i] > 0 then
    begin
      Inc(j);
      SetLength(Result,j);
      Result[j-1].EventName := FEvents[i];
      Result[j-1].Count := EventCountList[i];
    end;
  end;
end;

procedure TFB25Events.CancelEvents(Force: boolean);
begin
  FCriticalSection.Enter;
  try
    FInWaitState := false;
    with Firebird25ClientAPI do
      if (Call(isc_Cancel_events( StatusVector, @FDBHandle, @FEventID),false) > 0) and not Force then
        IBDatabaseError;

    FEventHandler := nil;
  finally
    FCriticalSection.Leave
  end;
end;

procedure TFB25Events.EventSignaled;
var Handler: TEventHandler;
begin
  FCriticalSection.Enter;
  try
    if not FInWaitState then Exit;
    FInWaitState := false;
    if assigned(FEventHandler)  then
    begin
      Handler := FEventHandler;
      FEventHandler := nil;
      Handler(self);
    end;
  finally
    FCriticalSection.Leave
  end;
end;

procedure TFB25Events.CreateEventBlock;
var
  i: integer;
  EventNames: array of PChar;
begin
  with Firebird25ClientAPI do
  begin
    if FEventBuffer <> nil then
      isc_free( FEventBuffer);
    FEventBuffer := nil;
    if FResultBuffer <> nil then
      isc_free( FResultBuffer);
    FResultBuffer := nil;

    setlength(EventNames,MaxEvents);
    try
      for i := 0 to FEvents.Count-1 do
        EventNames[i] := PChar(FEvents[i]);

      FEventBufferlen := isc_event_block(@FEventBuffer,@FResultBuffer,
                          FEvents.Count,
                          EventNames[0],EventNames[1],EventNames[2],
                          EventNames[3],EventNames[4],EventNames[5],
                          EventNames[6],EventNames[7],EventNames[8],
                          EventNames[9],EventNames[10],EventNames[11],
                          EventNames[12],EventNames[13],EventNames[14]
                          );
    finally
      SetLength(EventNames,0)
    end;
  end;
end;

constructor TFB25Events.Create(DBAttachment: TFB25Attachment; Events: TStrings);
begin
  inherited Create(DBAttachment);
  FAttachment := DBAttachment;
  if Events.Count > MaxEvents then
    IBError(ibxeMaximumEvents, [nil]);


  FCriticalSection := TCriticalSection.Create;
  FEvents := TStringList.Create;
  FDBHandle := DBAttachment.Handle;
  FEventHandlerThread := TEventHandlerThread.Create(self);
  FEvents.Assign(Events);
  CreateEventBlock;
end;

destructor TFB25Events.Destroy;
begin
  if assigned(FEventHandlerThread) then
    TEventHandlerThread(FEventHandlerThread).Terminate;
  if assigned(FCriticalSection) then FCriticalSection.Free;
  if assigned(FEvents) then FEvents.Free;
  with Firebird25ClientAPI do
  begin
    if FEventBuffer <> nil then
      isc_free( FEventBuffer);
    if FResultBuffer <> nil then
      isc_free( FResultBuffer);
  end;
  inherited Destroy;
end;

procedure TFB25Events.GetEvents(EventNames: TStrings);
begin
  EventNames.Assign(FEvents);
end;

procedure TFB25Events.SetEvents(EventNames: TStrings);
begin
  if EventNames.Text <> FEvents.Text then
  begin
    Cancel;
    FEvents.Assign(EventNames);
    CreateEventBlock;
  end;
end;

procedure TFB25Events.SetEvents(Event: string);
var S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Add(Event);
    SetEvents(S);
  finally
    S.Free;
  end;
end;

procedure TFB25Events.Cancel;
begin
  if assigned(FEventHandler) then
    CancelEvents;
end;

procedure TFB25Events.AsyncWaitForEvent(EventHandler: TEventHandler);
var callback: pointer;
begin
  FCriticalSection.Enter;
  try
    if FInWaitState then
      IBError(ibxeInEventWait,[nil]);

    CreateEventBlock;
    FEventHandler := EventHandler;
    callback := @IBEventCallback;
    with Firebird25ClientAPI do
      Call(isc_que_events( StatusVector, @FDBHandle, @FEventID, FEventBufferLen,
                     FEventBuffer, TISC_CALLBACK(callback), PVoid(FEventHandlerThread)));
    FInWaitState := true;
  finally
    FCriticalSection.Leave
  end;
end;

function TFB25Events.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

procedure TFB25Events.WaitForEvent;
begin
  if FInWaitState then
    IBError(ibxeInEventWait,[nil]);

  FInWaitState := true;
  try
    CreateEventBlock;
    with Firebird25ClientAPI do
       Call(isc_wait_for_event(StatusVector,@FDBHandle, FEventBufferlen,FEventBuffer,FResultBuffer));
  finally
    FInWaitState := false;
  end;
end;

end.

