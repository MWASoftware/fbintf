(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  The contents of this file are subject to the Initial Developer's
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
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2016 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FB30Events;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$interfaces COM}
{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}Windows, {$ENDIF} Classes, SysUtils, Firebird, IB, FB30ClientAPI, FB30Attachment,
  syncobjs, FBEvents;

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

  TFB30Events = class(TFBEvents,IEvents)
  private
    FAttachmentIntf: Firebird.IAttachment;
    FEventHandlerThread: TObject;
    FEventsIntf: Firebird.IEvents;
    FAsyncEventCallback: TEventhandlerInterface;
    FSyncEventCallback: TEventhandlerInterface;
    procedure InternalAsyncWaitForEvent(EventHandler: TEventHandler; EventCallBack: TEventhandlerInterface);
    procedure ReleaseIntf;
  protected
    procedure CancelEvents(Force: boolean = false); override;
    function GetIEvents: IEvents; override;
  public
    constructor Create(DBAttachment: TFB30Attachment; Events: TStrings);
    destructor Destroy; override;

    {IEvents}
    procedure WaitForEvent;
    procedure AsyncWaitForEvent(EventHandler: TEventHandler); override;
  end;

implementation

uses  FBMessages, FBClientAPI;

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
  FEventHandler := CreateEvent(PSa,false,false,nil);
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
  Result := FRef;
end;

procedure TEventhandlerInterface.eventCallbackFunction(length: Cardinal;
  events: BytePtr);
begin
  FOwner.FCriticalSection.Enter;
  try
    if FOwner.FResultBuffer <> nil then
      Move(events[0], FOwner.FResultBuffer[0], Length);
  finally
    FOwner.FCriticalSection.Leave
  end;
//  writeln('Set Event');
  {$IFDEF WINDOWS}
  SetEvent(FEventHandler);
  {$ELSE}
  FEventWaiting.SetEvent;
  {$ENDIF}
end;

procedure TEventhandlerInterface.WaitForEvent;
begin
  {$IFDEF WINDOWS}
  WaitForSingleObject(FEventHandler,INFINITE);
  {$ELSE}
  FEventWaiting.WaitFor(INFINITE);
  {$ENDIF}
//  writeln('Event Wait Ends');
end;

procedure TEventhandlerInterface.CancelWait;
begin
  {$IFDEF WINDOWS}
    SetEvent(FEventHandler);
  {$ELSE}
    FEventWaiting.SetEvent;
  {$ENDIF}
end;

 { TEventHandlerThread }

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
    inherited CancelEvents(Force);
  finally
    FCriticalSection.Leave
  end;
end;

function TFB30Events.GetIEvents: IEvents;
begin
  Result := self;
end;

procedure TFB30Events.InternalAsyncWaitForEvent(EventHandler: TEventHandler;
  EventCallBack: TEventhandlerInterface);
begin
  FCriticalSection.Enter;
  try
    if FInWaitState then
      IBError(ibxeInEventWait,[nil]);

    FEventHandler := EventHandler;
    ReleaseIntf;
    with Firebird30ClientAPI do
    begin
      FEventsIntf := FAttachmentIntf.queEvents(
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
  inherited Create(DBAttachment,DBAttachment,Events);
  FAttachmentIntf := DBAttachment.AttachmentIntf;
  FSyncEventCallback := TEventhandlerInterface.Create(self,'Sync');
end;

destructor TFB30Events.Destroy;
begin
  CancelEvents(true);
  if assigned(FEventHandlerThread) then
    TEventHandlerThread(FEventHandlerThread).Terminate;
  if assigned(FAsyncEventCallback) then TEventhandlerInterface(FAsyncEventCallback).release;
  if assigned(FSyncEventCallback) then TEventhandlerInterface(FSyncEventCallback).release;
  ReleaseIntf;
  inherited Destroy;
end;

procedure TFB30Events.AsyncWaitForEvent(EventHandler: TEventHandler);
begin
  {Seems like we have to create a new callback object each time to avoid empty events}
  if assigned(FEventHandlerThread) then
    TEventHandlerThread(FEventHandlerThread).Terminate;
  if assigned(FAsyncEventCallback) then TEventhandlerInterface(FAsyncEventCallback).release;
  FAsyncEventCallback := TEventhandlerInterface.Create(self,'Async');
  FEventHandlerThread := TEventHandlerThread.Create(self,FAsyncEventCallback);
  InternalAsyncWaitForEvent(EventHandler,FAsyncEventCallback);
end;

procedure TFB30Events.WaitForEvent;
begin
  InternalAsyncWaitForEvent(nil,FSyncEventCallback);
  FSyncEventCallback.WaitForEvent;
end;

end.

