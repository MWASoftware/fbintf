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
    FClientAPI: TFBClientAPI;
    FOwner: TObjectOwner;
    FEventBuffer: PChar;
    FEventBufferLen: integer;
    FEventID: ISC_LONG;
    FResultBuffer: PChar;
    FEvents: TStringList;
    FDBHandle: TISC_DB_HANDLE;
    FCriticalSection: TCriticalSection;
    FEventHandlerThread: TObject;
    FEventHandler: TEventHandler;
    function ExtractEventCounts: TEventCounts;
    procedure CancelEvents;
    procedure EventSignaled;
  public
    constructor Create(DBAttachment: TFBAttachment; Events: TStrings);
    destructor Destroy; override;

    {IEvents}
    function GetStatus: IStatus;
    procedure Cancel;
    procedure WaitForEvent(var EventCounts: TEventCounts);
    procedure AsyncWaitForEvent(EventHandler: TEventHandler);
  end;

implementation

uses FBStatus, FBErrorMessages;

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
    procedure DoEventSignalled;
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
      Synchronize(@DoEventSignalled)
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
  with FClientAPI do
     isc_event_counts( @EventCountList, FEventBufferLen, FEventBuffer, FResultBuffer);
  SetLength(Result,0);
  j := 0;
  for i := 0 to FEvents.Count - 1 do
  begin
    if EventCountList[i] > 0 then
    begin
      Inc(j);
      SetLength(Result,j);
      Result[j].EventName := FEvents[i];
      Result[j].Count := EventCountList[i];
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
begin
  if assigned(FEventHandler)  then
  begin
    FEventHandler(ExtractEventCounts);
  end;
  FEventHandler := nil;
end;

constructor TFBEvents.Create(DBAttachment: TFBAttachment; Events: TStrings);
var
  i: integer;
  EventNames: array of PChar;
begin
  inherited Create;
  FOwner := DBAttachment;
  DBAttachment.RegisterObj(self);
  FOwner := DBAttachment.ClientAPI;
  if Events.Count > MaxEvents then
    IBError(ibxeMaximumEvents, [nil]);


  FCriticalSection := TCriticalSection.Create;
  FEvents := TStringList.Create;
  FDBHandle := DBAttachment.Handle;
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
  with FClientAPI do
  begin
    if FEventBuffer <> nil then
      isc_free( FEventBuffer);
    if FResultBuffer <> nil then
      isc_free( FResultBuffer);
  end;
  if assigned(FOwner) then
    FOwner.UnRegisterObj(self);
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
                     FEventBuffer, TISC_CALLBACK(callback), PVoid(FEventHandlerThread)));
  finally
    FCriticalSection.Leave
  end;
end;


procedure TFBEvents.WaitForEvent(var EventCounts: TEventCounts);
begin
  with FClientAPI do
     Call(isc_wait_for_event(StatusVector,@FDBHandle, FEventBufferlen,@FEventBuffer,@FResultBuffer));
  EventCounts := ExtractEventCounts
end;

end.

