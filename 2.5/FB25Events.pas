unit FB25Events;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBLibrary, FB25ClientAPI, FB25Attachment, IBExternals,
  IBHeader, syncobjs;

type
  { TFBEvents }

  TFBEvents = class(TInterfacedObject,IEvents)
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
    procedure CancelEvents(Force: boolean = false);
    procedure EventSignaled;
    procedure CreateEventBlock;
  public
    constructor Create(DBAttachment: TFBAttachment; Events: TStrings);
    destructor Destroy; override;
    procedure EndAttachment(Sender: TFBAttachment; Force: boolean);

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

uses FB25Status, FBErrorMessages;

const
  MaxEvents = 15;

type

  { v }

  { TEventHandlerThread }

  TEventHandlerThread = class(TThread)
  private
    FOwner: TFBEvents;
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
    constructor Create(Owner: TFBEvents);
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
    SetEVent(FEventHandler);
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

constructor TEventHandlerThread.Create(Owner: TFBEvents);
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
  FEventWaiting := TEventObject.Create(PSa,false,true,GUIDToString(GUID));
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

  { TFBEvents }

function TFBEvents.ExtractEventCounts: TEventCounts;
var EventCountList: TStatusVector;
    i: integer;
    j: integer;
begin
  with Firebird25ClientAPI do
     isc_event_counts( @EventCountList, FEventBufferLen, FEventBuffer, FResultBuffer);
  SetLength(Result,0);
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

procedure TFBEvents.CancelEvents(Force: boolean);
begin
  FCriticalSection.Enter;
  try
    with Firebird25ClientAPI do
      if (isc_Cancel_events( StatusVector, @FDBHandle, @FEventID) > 0) and not Force then
        IBDatabaseError;

    FEventHandler := nil;
  finally
    FCriticalSection.Leave
  end;
end;

procedure TFBEvents.EventSignaled;
begin
  if assigned(FEventHandler)  then
  begin
    FEventHandler(self);
  end;
  FEventHandler := nil;
end;

procedure TFBEvents.CreateEventBlock;
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

constructor TFBEvents.Create(DBAttachment: TFBAttachment; Events: TStrings);
begin
  inherited Create;
  DBAttachment.RegisterObj(self);
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

destructor TFBEvents.Destroy;
begin
  (FAttachment as TFBAttachment).UnRegisterObj(self);
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

procedure TFBEvents.EndAttachment(Sender: TFBAttachment; Force: boolean);
begin
  if Sender <> FAttachment then Exit;

  CancelEvents(Force);
end;

procedure TFBEvents.GetEvents(EventNames: TStrings);
begin
  EventNames.Assign(FEvents);
end;

procedure TFBEvents.SetEvents(EventNames: TStrings);
begin
  if EventNames.Text <> FEvents.Text then
  begin
    Cancel;
    FEvents.Assign(EventNames);
    CreateEventBlock;
  end;
end;

procedure TFBEvents.SetEvents(Event: string);
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

procedure TFBEvents.Cancel;
begin
  if assigned(FEventHandler) then
    CancelEvents;
end;

procedure TFBEvents.AsyncWaitForEvent(EventHandler: TEventHandler);
var callback: pointer;
begin
  if assigned(FEventHandler) then
    CancelEvents;

  FEventHandler := EventHandler;
  FCriticalSection.Enter;
  try
    callback := @IBEventCallback;
    with Firebird25ClientAPI do
      if isc_que_events( StatusVector, @FDBHandle, @FEventID, FEventBufferLen,
                     FEventBuffer, TISC_CALLBACK(callback), PVoid(FEventHandlerThread)) > 0 then
        IBDatabaseError;
  finally
    FCriticalSection.Leave
  end;
end;

function TFBEvents.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

procedure TFBEvents.WaitForEvent;
begin
  with Firebird25ClientAPI do
     if isc_wait_for_event(StatusVector,@FDBHandle, FEventBufferlen,FEventBuffer,FResultBuffer) > 0 then
       IBDatabaseError;
end;

end.

