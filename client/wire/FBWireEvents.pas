(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  This file is part of the pure Pascal wire protocol implementation
 *  (no fbclient required) and is subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBWireEvents;

{ The IEvents implementation for the pure Pascal wire protocol client.

  Firebird delivers events on a second connection. The client sends
  op_connect_request (P_REQ_async) on the main connection and the server
  answers with the address of an auxiliary port; the client opens a plain
  TCP connection to it - same host as the main connection, the port from
  the response (the address in the response is the server's own view of
  itself, which behind NAT is not reachable; taking only the port is what
  the stock client does, see aux_connect in src/remote/inet.cpp). The
  auxiliary connection carries no handshake, no authentication and no
  encryption: the server associates it with the session by the accept, and
  it only ever delivers op_event packets.

  One auxiliary connection and one listener thread serve all the IEvents
  instances of an attachment. Interest is registered with op_que_events on
  the main connection, cancelled with op_cancel_events, and notifications
  are dispatched by the client supplied event id.

  All of the event block machinery - building the EPB that op_que_events
  carries, diffing the counts, the callback dispatch - is inherited from
  TFBEvents; this unit supplies the transport.

  The event handler is called from the listener thread, exactly as the
  2.5 provider's handler is called from its AST thread. A handler must
  not call back into the same attachment from that thread; Synchronize
  or Queue the work to another thread first (the test suite's Test 10
  shows the pattern).}

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, IB, FBEvents, FBWireClientAPI, FBWireProtocol,
  FBWireStream, FBWireAttachment;

type
  TFBWireEventManager = class;

  { TFBWireEvents }

  TFBWireEvents = class(TFBEvents,IEvents)
  private
    FManager: TFBWireEventManager;
    FEventID: integer;
    FSyncSignal: TSimpleEvent;
    FSyncWait: boolean;
    function EPBBytes: TBytes;
  protected
    procedure CancelEvents(Force: boolean = false); override;
    function GetIEvents: IEvents; override;
  public
    constructor Create(DBAttachment: TFBWireAttachment;
                aManager: TFBWireEventManager; Events: TStrings);
    destructor Destroy; override;

    {called by the listener thread with the updated event buffer}
    procedure EventArrived(const aItems: TBytes);

    {IEvents}
    procedure AsyncWaitForEvent(EventHandler: TEventHandler); override;
    procedure WaitForEvent; override;

    property EventID: integer read FEventID;
  end;

  { TFBWireEventManager

    Owned by the attachment. Created lazily on the first GetEventHandler
    call, freed on disconnect. Owns the auxiliary connection and the
    listener thread.}

  TFBWireEventManager = class
  private
    FAttachment: TFBWireAttachment;
    FTransport: TFBWireTransport;
    FXDR: TXDRStream;
    FListener: TThread;
    FEventsList: TThreadList;
    FNextEventID: integer;
    function GetConnection: TFBWireConnection;
  public
    constructor Create(aAttachment: TFBWireAttachment);
    destructor Destroy; override;
    procedure RegisterEvents(aEvents: TFBWireEvents);
    procedure UnregisterEvents(aEvents: TFBWireEvents);
    {each op_que_events uses a fresh event id, as the stock client does -
     the interest is one shot and the new id identifies the new interest.
     The id is assigned to aEvents before the request is sent.}
    procedure QueEvents(aEvents: TFBWireEvents; const aEPB: TBytes);
    procedure CancelEvents(aEventID: integer);
    procedure DispatchEvent(aEventID: integer; const aItems: TBytes);
    property Connection: TFBWireConnection read GetConnection;
  end;

implementation

uses FBMessages, FBWireConst;

type
  { TFBWireEventListener - drains the auxiliary connection }

  TFBWireEventListener = class(TThread)
  private
    FManager: TFBWireEventManager;
  protected
    procedure Execute; override;
  public
    constructor Create(aManager: TFBWireEventManager);
  end;

{ TFBWireEventListener }

constructor TFBWireEventListener.Create(aManager: TFBWireEventManager);
begin
  FManager := aManager;
  inherited Create(false);
end;

procedure TFBWireEventListener.Execute;
var op: integer;
    dbHandle, eventID: integer;
    items: TBytes;
begin
  try
    repeat
      op := FManager.FXDR.ReadInt32;
      case op of
      op_dummy: ;
      op_event:
        begin
          dbHandle := FManager.FXDR.ReadInt32;
          items := FManager.FXDR.ReadString;
          FManager.FXDR.ReadInt32;   {p_event_ast - a raw memory image}
          FManager.FXDR.ReadInt32;   {p_event_arg - ditto}
          eventID := FManager.FXDR.ReadInt32;
          FManager.DispatchEvent(eventID,items);
        end;
      else
        break;
      end;
    until Terminated;
  except
    {the socket is closed under this thread on shutdown}
    on E: EFBWireError do ;
  end;
end;

{ TFBWireEventManager }

function TFBWireEventManager.GetConnection: TFBWireConnection;
begin
  Result := FAttachment.Connection;
end;

constructor TFBWireEventManager.Create(aAttachment: TFBWireAttachment);
var aPort: integer;
begin
  inherited Create;
  FAttachment := aAttachment;
  FEventsList := TThreadList.Create;
  try
    aPort := Connection.ConnectRequest(aAttachment.Handle);
    FTransport := TFBWireTransport.Create;
    FTransport.ConnectTo(aAttachment.Host,aPort);
    FXDR := TXDRStream.Create(FTransport);
    FListener := TFBWireEventListener.Create(self);
  except
    on E: Exception do
    begin
      if FTransport <> nil then
        FTransport.Disconnect;
      FreeAndNil(FXDR);
      FreeAndNil(FTransport);
      FreeAndNil(FEventsList);
      WireIBError(aAttachment.WireAPI,E);
    end;
  end;
end;

destructor TFBWireEventManager.Destroy;
begin
  if FListener <> nil then
  begin
    FListener.Terminate;
    {shut the socket down under the listener so that its blocking read
     returns, then wait for it}
    if FTransport <> nil then
      FTransport.Abort;
    FListener.WaitFor;
    FreeAndNil(FListener);
  end;
  if FTransport <> nil then
    FTransport.Disconnect;
  FreeAndNil(FXDR);
  FreeAndNil(FTransport);
  FreeAndNil(FEventsList);
  inherited Destroy;
end;

procedure TFBWireEventManager.RegisterEvents(aEvents: TFBWireEvents);
begin
  with FEventsList.LockList do
  try
    Add(aEvents);
  finally
    FEventsList.UnlockList;
  end;
end;

procedure TFBWireEventManager.UnregisterEvents(aEvents: TFBWireEvents);
begin
  FEventsList.Remove(aEvents);
end;

procedure TFBWireEventManager.QueEvents(aEvents: TFBWireEvents;
  const aEPB: TBytes);
var aEventID: integer;
begin
  with FEventsList.LockList do
  try
    Inc(FNextEventID);
    aEventID := FNextEventID;
    aEvents.FEventID := aEventID;
  finally
    FEventsList.UnlockList;
  end;
  try
    Connection.QueEvents(FAttachment.Handle,aEPB,aEventID);
  except
    on E: Exception do WireIBError(FAttachment.WireAPI,E);
  end;
end;

procedure TFBWireEventManager.CancelEvents(aEventID: integer);
begin
  Connection.CancelEvents(FAttachment.Handle,aEventID);
end;

procedure TFBWireEventManager.DispatchEvent(aEventID: integer;
  const aItems: TBytes);
var i: integer;
    aEvents: TFBWireEvents;
    Pin: IEvents;
begin
  {find the interested party under the list lock and pin it with an
   interface reference before calling out, so that it cannot be freed
   while the notification is delivered}
  aEvents := nil;
  with FEventsList.LockList do
  try
    for i := 0 to Count - 1 do
      if TFBWireEvents(Items[i]).EventID = aEventID then
      begin
        aEvents := TFBWireEvents(Items[i]);
        Pin := aEvents;
        break;
      end;
  finally
    FEventsList.UnlockList;
  end;
  if aEvents <> nil then
    aEvents.EventArrived(aItems);
end;

{ TFBWireEvents }

function TFBWireEvents.EPBBytes: TBytes;
begin
  SetLength(Result,FEventBufferLen);
  if FEventBufferLen > 0 then
    Move(FEventBuffer^,Result[0],FEventBufferLen);
end;

procedure TFBWireEvents.CancelEvents(Force: boolean);
begin
  FCriticalSection.Enter;
  try
    if not FInWaitState then Exit;
    try
      FManager.CancelEvents(FEventID);
    except
      on E: Exception do
        if not Force then
          WireIBError(FManager.FAttachment.WireAPI,E);
    end;
    FInWaitState := false;
    if FSyncWait then
    begin
      FSyncWait := false;
      FSyncSignal.SetEvent;
    end;
    inherited CancelEvents(Force);
  finally
    FCriticalSection.Leave;
  end;
end;

function TFBWireEvents.GetIEvents: IEvents;
begin
  Result := self;
end;

constructor TFBWireEvents.Create(DBAttachment: TFBWireAttachment;
  aManager: TFBWireEventManager; Events: TStrings);
begin
  inherited Create(DBAttachment,DBAttachment,Events);
  FManager := aManager;
  FSyncSignal := TSimpleEvent.Create;
  FEventID := -1;   {assigned by each QueEvents}
  aManager.RegisterEvents(self);
end;

destructor TFBWireEvents.Destroy;
begin
  CancelEvents(true);
  if FManager <> nil then
    FManager.UnregisterEvents(self);
  FreeAndNil(FSyncSignal);
  inherited Destroy;
end;

procedure TFBWireEvents.EventArrived(const aItems: TBytes);
var n: integer;
    SignalSync: boolean;
begin
  FCriticalSection.Enter;
  try
    n := Length(aItems);
    if n > FEventBufferLen then
      n := FEventBufferLen;
    if n > 0 then
      Move(aItems[0],FResultBuffer^,n);
    SignalSync := FSyncWait;
    if SignalSync then
    begin
      {a synchronous wait ends on the first delivery, like
       isc_wait_for_event}
      ProcessEventCounts;
      FSyncWait := false;
      FInWaitState := false;
    end;
  finally
    FCriticalSection.Leave;
  end;
  if SignalSync then
    FSyncSignal.SetEvent
  else
    EventSignaled;
end;

procedure TFBWireEvents.AsyncWaitForEvent(EventHandler: TEventHandler);
begin
  FCriticalSection.Enter;
  try
    if FInWaitState then
      IBError(ibxeInEventWait,[nil]);
    FEventHandler := EventHandler;
    FManager.QueEvents(self,EPBBytes);
    FInWaitState := true;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TFBWireEvents.WaitForEvent;
begin
  FCriticalSection.Enter;
  try
    if FInWaitState then
      IBError(ibxeInEventWait,[nil]);
    FSyncSignal.ResetEvent;
    FSyncWait := true;
    FManager.QueEvents(self,EPBBytes);
    FInWaitState := true;
  finally
    FCriticalSection.Leave;
  end;
  FSyncSignal.WaitFor(INFINITE);
end;

end.
