unit FB30Events;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows, {$ENDIF} Classes, SysUtils, Firebird, IB, FBClientAPI, FB30ClientAPI, FB30Attachment,
  IBExternals, syncobjs, FBActivityMonitor;

type
  TFB30Events = class;

  { TEventhandlerInterface }

  TEventhandlerInterface = class(Firebird.IEventCallbackImpl)
  private
    FOwner: TFB30Events;
    FName: string;
    FRef: integer;
    {$IFDEF WINDOWS}
    {Make direct use of Windows API as TEventObject don't seem to work under
     Windows!}
    FEventHandler: THandle;
    {$ELSE}
    FEventWaiting: TEventObject;
    {$ENDIF}
  public
    constructor Create(aOwner: TFB30Events; aName: string);
    destructor Destroy; override;
    procedure addRef();  override;
    function release(): Integer; override;
    procedure eventCallbackFunction(length: Cardinal; events: BytePtr); override;
    procedure WaitForEvent;
    procedure CancelWait;
 end;

  { TFB30Events }

  TFB30Events = class(TActivityReporter,IEvents)
  private
    FEventBuffer: PChar;
    FEventBufferLen: integer;
    FEventID: ISC_LONG;
    FResultBuffer: PChar;
    FEvents: TStringList;
    FAttachment: IAttachment;
    FCriticalSection: TCriticalSection;
    FEventHandlerThread: TObject;
    FEventHandler: TEventHandler;
    FEventsIntf: Firebird.IEvents;
    FAsyncEventCallback: TEventhandlerInterface;
    FSyncEventCallback: TEventhandlerInterface;
    FInWaitState: boolean;
    procedure CancelEvents(Force: boolean = false);
    procedure EventSignaled;
    procedure CreateEventBlock;
    procedure InternalAsyncWaitForEvent(EventHandler: TEventHandler; EventCallBack: TEventhandlerInterface);
    procedure ReleaseIntf;
  public
    constructor Create(DBAttachment: TFB30Attachment; Events: TStrings);
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
    FOwner: TFB30Events;
    FEventHandler: TEventhandlerInterface;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TFB30Events; EventHandler: TEventhandlerInterface);
    procedure Terminate;
  end;

constructor TEventhandlerInterface.Create(aOwner: TFB30Events; aName: string);
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
  inherited Create;
{$IFDEF WINDOWS}
  FEventHandler := CreateEvent(PSa,false,true,nil);
{$ELSE}
  CreateGuid(GUID);
  FEventWaiting := TEventObject.Create(PSa,false,false,GUIDToString(GUID));
{$ENDIF}
  FOWner := aOwner;
  FName := aName;
  addRef;
end;

destructor TEventhandlerInterface.Destroy;
begin
{$IFDEF WINDOWS}
  CloseHandle(FEventHandler);
{$ELSE}
  if assigned(FEventWaiting) then FEventWaiting.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TEventhandlerInterface.addRef;
begin
  Inc(FRef);
//  writeln(FName,': ref count = ',FRef);
end;

function TEventhandlerInterface.release: Integer;
begin
  Dec(FRef);
//  writeln(FName,': ref count = ',FRef);
  if FRef = 0 then Free;
end;

procedure TEventhandlerInterface.eventCallbackFunction(length: Cardinal;
  events: BytePtr);
begin
  FOwner.FCriticalSection.Enter;
  try
    if FOwner.FResultBuffer <> nil then
      Move(events[0], FOwner.FResultBuffer[0], Length);
    {$IFDEF WINDOWS}
    SetEvent(FEventHandler);
    {$ELSE}
    FEventWaiting.SetEvent;
    {$ENDIF}
  finally
    FOwner.FCriticalSection.Leave
  end;
end;

procedure TEventhandlerInterface.WaitForEvent;
begin
  {$IFDEF WINDOWS}
  WaitForSingleObject(FEventHandler,INFINITE);
  {$ELSE}
  FEventWaiting.WaitFor(INFINITE);
  {$ENDIF}
end;

procedure TEventhandlerInterface.CancelWait;
begin
  {$IFDEF WINDOWS}
    SetEvent(FEventHandler);
  {$ELSE}
    FEventWaiting.SetEvent;
  {$ENDIF}
end;

 { TEventHandler }

procedure TEventHandlerThread.Execute;
begin
  while not Terminated do
  begin
    FEventHandler.WaitForEvent;

    if not Terminated  then
      FOwner.EventSignaled;
  end;
end;

constructor TEventHandlerThread.Create(Owner: TFB30Events;
  EventHandler: TEventhandlerInterface);
begin
  inherited Create(true);
  FOwner := Owner;
  FEventHandler := EventHandler;
  FreeOnTerminate := true;
  Start;
end;

procedure TEventHandlerThread.Terminate;
begin
  inherited Terminate;
  FEventHandler.CancelWait;
end;

  { TFB30Events }

function TFB30Events.ExtractEventCounts: TEventCounts;
var EventCountList: TStatusVector;
    i: integer;
    j: integer;
begin
  SetLength(Result,0);
  if FResultBuffer = nil then Exit;

  with Firebird30ClientAPI do
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

procedure TFB30Events.CancelEvents(Force: boolean);
begin
  FCriticalSection.Enter;
  try
    if not FInWaitState then Exit;
    if FEventsIntf <> nil then
    with Firebird30ClientAPI do
    begin
      FEventsIntf.Cancel(StatusIntf);
      if not Force then
        Check4DataBaseError;
    end;
    FInWaitState := false;
    ReleaseIntf;
  finally
    FCriticalSection.Leave
  end;
end;

procedure TFB30Events.EventSignaled;
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

procedure TFB30Events.CreateEventBlock;
var
  i: integer;
  EventNames: array of PChar;
begin
  with Firebird30ClientAPI do
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

procedure TFB30Events.InternalAsyncWaitForEvent(EventHandler: TEventHandler;
  EventCallBack: TEventhandlerInterface);
begin
  FCriticalSection.Enter;
  try
    if FInWaitState then
      IBError(ibxeInEventWait,[nil]);

    CreateEventBlock;
    FEventHandler := EventHandler;
    ReleaseIntf;
    with Firebird30ClientAPI do
    begin
      FEventsIntf := (FAttachment as TFB30Attachment).AttachmentIntf.queEvents(
                                StatusIntf,EventCallBack,
                                FEventBufferLen, BytePtr(FEventBuffer));
      Check4DataBaseError;
    end;
    FInWaitState := true;

  finally
    FCriticalSection.Leave
  end;
end;

procedure TFB30Events.ReleaseIntf;
begin
  if FEventsIntf <> nil then
    FEventsIntf.release;
  FEventsIntf := nil;
end;

constructor TFB30Events.Create(DBAttachment: TFB30Attachment; Events: TStrings);
begin
  inherited Create(DBAttachment);
  FAttachment := DBAttachment;
  if Events.Count > MaxEvents then
    IBError(ibxeMaximumEvents, [nil]);


  FCriticalSection := TCriticalSection.Create;
  FEvents := TStringList.Create;
  FAsyncEventCallback := TEventhandlerInterface.Create(self,'Async');
  FEventHandlerThread := TEventHandlerThread.Create(self,FAsyncEventCallback);
  FSyncEventCallback := TEventhandlerInterface.Create(self,'Sync');
  FEvents.Assign(Events);
  CreateEventBlock;
end;

destructor TFB30Events.Destroy;
begin
  CancelEvents(true);
  if assigned(FEventHandlerThread) then
    TEventHandlerThread(FEventHandlerThread).Terminate;
  if assigned(FCriticalSection) then FCriticalSection.Free;
  if assigned(FEvents) then FEvents.Free;
  if assigned(FAsyncEventCallback) then TEventhandlerInterface(FAsyncEventCallback).release;
  if assigned(FSyncEventCallback) then TEventhandlerInterface(FSyncEventCallback).release;
  ReleaseIntf;
  with Firebird30ClientAPI do
  begin
    if FEventBuffer <> nil then
      isc_free( FEventBuffer);
    if FResultBuffer <> nil then
      isc_free( FResultBuffer);
  end;
  inherited Destroy;
end;

procedure TFB30Events.GetEvents(EventNames: TStrings);
begin
  EventNames.Assign(FEvents);
end;

procedure TFB30Events.SetEvents(EventNames: TStrings);
begin
  if EventNames.Text <> FEvents.Text then
  begin
    Cancel;
    FEvents.Assign(EventNames);
    CreateEventBlock;
  end;
end;

procedure TFB30Events.SetEvents(Event: string);
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

procedure TFB30Events.Cancel;
begin
  if FInWaitState then
    CancelEvents;
end;

procedure TFB30Events.AsyncWaitForEvent(EventHandler: TEventHandler);
begin
  InternalAsyncWaitForEvent(EventHandler,FAsyncEventCallback);
end;

function TFB30Events.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

procedure TFB30Events.WaitForEvent;
begin
  InternalAsyncWaitForEvent(nil,FSyncEventCallback);
  FSyncEventCallback.WaitForEvent;
end;

end.

