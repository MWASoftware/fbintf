unit FB25Events;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB;

type

  { TFBEvents }

  TFBEvents = class(TInterfacedObject,IEvents)
  private
    FClientAPI: TFBClientAPI;
    FEventBuffer: PChar;
    FEventBufferLen: integer;
    FEventID: ISC_LONG;
    FResultBuffer: PChar;
    FEventBufferLen: integer;
    FEvents: TStringList;
    FDBHandle: TISC_DB_HANDLE;
    FCriticalSection: TCriticalSection;
    FEventHandlerThread: TObject;
    FEventHandler: TEventHandler;
    procedure ExtractEventCounts(var EventCounts: array of TEventInfo);
    procedure CancelEvents;
    procedure EventSignaled;
  public
    constructor Create(ClientAPI: TFBClientAPI; DBHandle: TISC_DB_HANDLE; Events: TStrings);
    destructor Destroy; override;

    {IEvents}
    function GetStatus: IStatus;
    procedure Cancel;
    procedure WaitForEvent(var EventCounts: array of TEventInfo);
    procedure AsyncWaitForEvent(EventHandler: TEventHandler);
  end;

implementation

uses FBStatus;

const
  MaxEvents = 15;

type

  { v }

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
    procedure DoEventSignalled;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TIBEvents);
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

procedure TEventHandlerThread.DoEventSignalled;
begin
  FOwner.EventSignaled;
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
      Synchronize(DoEventSignalled)
  end;
end;

constructor TEventHandlerThread.Create(Owner: TIBEvents);
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
begin
  PSa:= nil;
{$ENDIF}
  inherited Create(true);
  FOwner := Owner;
  {$IFDEF WINDOWS}
  FEventHandler := CreateEvent(PSa,false,true,nil);
  {$ELSE}
  FEventWaiting := TEventObject.Create(PSa,false,true,FOwner.Name+'.Events');
  {$ENDIF}
  FreeOnTerminate := true;
  Resume
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

procedure TFBEvents.ExtractEventCounts(var EventCounts: array of TEventInfo);
var EventCountList: TStatusVector;
    i: integer;
    j: integer;
begin
  with FClientAPI do
     isc_event_counts( @EventCountList, FEventBufferLen, FEventBuffer, FResultBuffer);
  SetLength(EventsCounts,0);
  j := 0;
  for i := 0 to FEvents.Count - 1 do
  begin
    if EventCountList[i] > 0 then
    begin
      Inc(j);
      SetLength(EventsCounts,j);
      EventsCounts[j].EventName := FEvents[i];
      EventsCounts[j].Count := EventCountList[i];
    end;
  end;
end;

procedure TFBEvents.CancelEvents;
begin
  FCriticalSection.Enter;
  try
    with FClientAPI do
      Call(isc_Cancel_events( StatusVector, @FDBHandle, @FEventID));
    FEventHandler := nil;
  finally
    FCriticalSection.Leave
  end;
end;

procedure TFBEvents.EventSignaled;
var EventCounts: array of TEventInfo;
begin
  if assigned(FEventHandler)  then
  begin
    ExtractEventCounts(EventCounts);
    FEventHandler(EventCounts);
  end;
  FEventHandler := nil;
end;

constructor TFBEvents.Create(ClientAPI: TFBClientAPI; DBHandle: TISC_DB_HANDLE;
  Events: TStrings);
var
  i: integer;
  EventNames: array of PChar;
begin
  FClientAPI := ClientAPI;
  if Events.Count > MaxEvents then
    IBError(ibxeMaximumEvents, [nil]);

  FCriticalSection := TCriticalSection.Create;
  FEvents := TStringList.Create;
  FDBHandle := DBHandle;
  FEventHandlerThread := TEventHandlerThread.Create(self);
  setlength(EventNames,MaxEvents);
  try
    for i := 0 to Events.Count-1 do
      EventNames[i] := PChar(Events[i]);
    FEvents.Assign(Events);
    with FClientAPI do
       FEventBufferlen := isc_event_block(@FEventBuffer,@FResultBuffer,
                        Events.Count,
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

destructor TFBEvents.Destroy;
begin
  if assigned(FEventHandlerThread) then
    TEventHandlerThread(FEventHandlerThread).Terminate;
  if assigned(FCriticalSection) then FCriticalSection.Free;
  if assigned(FEvents) then FEvents.Free;
  if FEventBuffer <> nil then
    isc_free( FEventBuffer);
  if FResultBuffer <> nil then
    isc_free( FResultBuffer);
  inherited Destroy;
end;

function TFBEvents.GetStatus: IStatus;
begin
  Result := FClientAPI.Status;
end;

procedure TFBEvents.Cancel;
begin
  if assigned(FEventHandler) then
    CancelEvents;
  Free;
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
    with FClientAPI do
      Call(isc_que_events( StatusVector, @FDBHandle, @FEventID, FEventBufferLen,
                     FEventBuffer, TISC_CALLBACK(callback), PVoid(FEventHandlerThread));
  finally
    FCriticalSection.Leave
  end;
end;


procedure TFBEvents.WaitForEvent(var EventCounts: array of TEventInfo);
begin
  with FClientAPI do
     Call(isc_wait_for_event(StatusVector,@FDBHandle, FEventBufferlen,@FEventBuffer,@FResultBuffer));
  ExtractEventCounts(EventCounts);
end;

end.

