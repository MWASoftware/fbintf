unit FB30Events;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Firebird, IB, FBClientAPI, FB30ClientAPI, FB30Attachment,
  IBExternals, syncobjs, FBActivityMonitor;

type
  { TFBEvents }

  TFBEvents = class(TActivityReporter,IEvents)
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
    FEventCallback: TObject;
    FSignalFired: boolean;
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

uses  FBMessages;

const
  MaxEvents = 15;

type
  TEventHandlerThread = class;

  { TEventhandlerInterface }

  TEventhandlerInterface = class(Firebird.IEventCallbackImpl)
  private
    FOwner: TEventHandlerThread;
    FRef: integer;
  public
    constructor Create(aOwner: TEventHandlerThread);
    procedure addRef();  override;
    function release(): Integer; override;
    procedure eventCallbackFunction(length: Cardinal; events: BytePtr); override;
 end;

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
    procedure HandleEventSignalled(length: short; updated: BytePtr);
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TFBEvents);
    destructor Destroy; override;
    procedure Terminate;
  end;

constructor TEventhandlerInterface.Create(aOwner: TEventHandlerThread);
begin
  inherited Create;
  FOWner := aOwner;
end;

procedure TEventhandlerInterface.addRef;
begin
  Inc(FRef);
end;

function TEventhandlerInterface.release: Integer;
begin
  Dec(FRef);
  if FRef = 0 then Free;
end;

procedure TEventhandlerInterface.eventCallbackFunction(length: Cardinal;
  events: BytePtr);
begin
  FOwner.HandleEventSignalled(length,events);
end;

 { TEventHandler }

procedure TEventHandlerThread.HandleEventSignalled(length: short;
  updated: BytePtr);
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
  with Firebird30ClientAPI do
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
  if FEventsIntf = nil then Exit;
  FCriticalSection.Enter;
  try
    with Firebird30ClientAPI do
    begin
      FEventsIntf.Cancel(StatusIntf);
      Check4DataBaseError;
    end;

    FEventHandler := nil;
    FSignalFired := false;
  finally
    FCriticalSection.Leave
  end;
end;

procedure TFBEvents.EventSignaled;
var Handler: TEventHandler;
begin
  if not FSignalFired then
    FSignalFired := true {ignore first event}
  else
  if assigned(FEventHandler)  then
  begin
    Handler := FEventHandler;
    FEventHandler := nil;
    Handler(self);
  end;
end;

procedure TFBEvents.CreateEventBlock;
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

constructor TFBEvents.Create(DBAttachment: TFBAttachment; Events: TStrings);
begin
  inherited Create(DBAttachment);
  FAttachment := DBAttachment;
  if Events.Count > MaxEvents then
    IBError(ibxeMaximumEvents, [nil]);


  FCriticalSection := TCriticalSection.Create;
  FEvents := TStringList.Create;
  FEventHandlerThread := TEventHandlerThread.Create(self);
  FEvents.Assign(Events);
  FEventCallback := TEventhandlerInterface.Create(TEventHandlerThread(FEventHandlerThread));
  CreateEventBlock;
end;

destructor TFBEvents.Destroy;
begin
  if assigned(FEventHandlerThread) then
    TEventHandlerThread(FEventHandlerThread).Terminate;
  if assigned(FCriticalSection) then FCriticalSection.Free;
  if assigned(FEvents) then FEvents.Free;
  if assigned(FEventCallback) then TEventhandlerInterface(FEventCallback).release;
  with Firebird30ClientAPI do
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
  if FEventHandler <> nil then
    CancelEvents;
end;

procedure TFBEvents.AsyncWaitForEvent(EventHandler: TEventHandler);
begin
  if assigned(FEventHandler) then
    CancelEvents;

  if assigned(FEventsIntf) then
    FEventsIntf.release;
  FEventsIntf := nil;
  CreateEventBlock;
  FEventHandler := EventHandler;
  FCriticalSection.Enter;
  try
    with Firebird30ClientAPI do
    begin
      FEventsIntf := (FAttachment as TFBAttachment).AttachmentIntf.queEvents(
                                StatusIntf,TEventhandlerInterface(FEventCallback),
                                FEventBufferLen, BytePtr(FEventBuffer));
      Check4DataBaseError;
    end;
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
  IBError(ibxeNotSupported,[nil]);
end;

end.

