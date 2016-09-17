unit FB25Events;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows, {$ENDIF}Classes, SysUtils, IB, FB25ClientAPI, FB25Attachment,
  IBExternals, IBHeader, syncobjs, FBEvents;

type
  { TFB25Events }

  TFB25Events = class(TFBEvents,IEvents)
  private
    FEventID: ISC_LONG;
    FDBHandle: TISC_DB_HANDLE;
    FEventHandlerThread: TObject;
  protected
    procedure CancelEvents(Force: boolean = false); override;
    function GetIEvents: IEvents; override;
  public
    constructor Create(DBAttachment: TFB25Attachment; Events: TStrings);
    destructor Destroy; override;

    {IEvents}
    procedure WaitForEvent;
    procedure AsyncWaitForEvent(EventHandler: TEventHandler);
  end;

implementation

uses  FBMessages;

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

procedure TFB25Events.CancelEvents(Force: boolean);
begin
  FCriticalSection.Enter;
  try
    if not FInWaitState then Exit;
    with Firebird25ClientAPI do
      if (Call(isc_Cancel_events( StatusVector, @FDBHandle, @FEventID),false) > 0) and not Force then
        IBDatabaseError;

    FInWaitState := false;
    inherited CancelEvents(Force);
  finally
    FCriticalSection.Leave
  end;
end;

function TFB25Events.GetIEvents: IEvents;
begin
  Result := self;
end;

constructor TFB25Events.Create(DBAttachment: TFB25Attachment; Events: TStrings);
begin
  inherited Create(DBAttachment,DBAttachment,Events);
  FDBHandle := DBAttachment.Handle;
  FEventHandlerThread := TEventHandlerThread.Create(self);
end;

destructor TFB25Events.Destroy;
begin
  CancelEvents(true);
  if assigned(FEventHandlerThread) then
    TEventHandlerThread(FEventHandlerThread).Terminate;
  inherited Destroy;
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

