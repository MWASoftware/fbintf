(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API. Although predominantly
 *  a new development they include source code taken from IBX and may be
 *  considered a derived product. This software thus also includes the copyright
 *  notice and license conditions from IBX.
 *
 *  Except for those parts dervied from IBX, contents of this file are subject
 *  to the Initial Developer's Public License Version 1.0 (the "License"); you
 *  may not use this file except in compliance with the License. You may obtain a
 *  copy of the License here:
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
{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011 - 2015                                                }
{                                                                        }
{************************************************************************}
unit FB25Events;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$interfaces COM}
{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}Windows, {$ENDIF}Classes, SysUtils, IB, FB25ClientAPI, FB25Attachment,
  IBExternals, IBHeader, syncobjs, FBEvents;

type
  TFB25Events = class;

  { TEventhandlerInterface }

  TEventhandlerInterface = class
  private
    FOwner: TFB25Events;
    {$IFDEF WINDOWS}
    {Make direct use of Windows API as TEventObject don't seem to work under
     Windows!}
    FEventHandler: THandle;
    {$ELSE}
    FEventWaiting: TEventObject;
    {$ENDIF}
  public
    constructor Create(aOwner: TFB25Events);
    destructor Destroy; override;
    procedure eventCallbackFunction(length: short; updated: PChar);
    procedure WaitForEvent;
    procedure CancelWait;
 end;

  { TFB25Events }

  TFB25Events = class(TFBEvents,IEvents)
  private
    FEventID: ISC_LONG;
    FDBHandle: TISC_DB_HANDLE;
    FEventHandlerThread: TObject;
    FAsyncEventCallback: TEventhandlerInterface;
  protected
    procedure CancelEvents(Force: boolean = false); override;
    function GetIEvents: IEvents; override;
  public
    constructor Create(DBAttachment: TFB25Attachment; Events: TStrings);
    destructor Destroy; override;

    {IEvents}
    procedure WaitForEvent;
    procedure AsyncWaitForEvent(EventHandler: TEventHandler); override;
  end;

implementation

uses  FBMessages;

type

  { TEventHandlerThread }

  TEventHandlerThread = class(TThread)
  private
    FOwner: TFB25Events;
    FEventHandler: TEventhandlerInterface;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TFB25Events; EventHandler: TEventhandlerInterface);
    procedure Terminate;
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

 constructor TEventHandlerThread.Create(Owner: TFB25Events;
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

  {This procedure is used for the event call back - note the cdecl }

 procedure IBEventCallback( ptr: pointer; length: short; updated: PChar); cdecl;
 begin
   if (ptr = nil) or (length = 0) or (updated = nil) then
     Exit;
   { Handle events asynchronously in second thread }
   TEventhandlerInterface(ptr).eventCallbackFunction(length,updated);
 end;

{ TEventhandlerInterface }

constructor TEventhandlerInterface.Create(aOwner: TFB25Events);
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

procedure TEventhandlerInterface.eventCallbackFunction(length: short;
  updated: PChar);
begin
  FOwner.FCriticalSection.Enter;
  try
    if FOwner.FResultBuffer <> nil then
      Move(updated[0], FOwner.FResultBuffer[0], length);
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
  FAsyncEventCallback := TEventhandlerInterface.Create(self);
  FEventHandlerThread := TEventHandlerThread.Create(self,FAsyncEventCallback);
end;

destructor TFB25Events.Destroy;
begin
  CancelEvents(true);
  if assigned(FEventHandlerThread) then
    TEventHandlerThread(FEventHandlerThread).Terminate;
  if assigned(FAsyncEventCallback) then
    TEventhandlerInterface(FAsyncEventCallback).Free;
  inherited Destroy;
end;

procedure TFB25Events.AsyncWaitForEvent(EventHandler: TEventHandler);
var callback: pointer;
begin
  FCriticalSection.Enter;
  try
    if FInWaitState then
      IBError(ibxeInEventWait,[nil]);

    FEventHandler := EventHandler;
    callback := @IBEventCallback;
    with Firebird25ClientAPI do
      Call(isc_que_events( StatusVector, @FDBHandle, @FEventID, FEventBufferLen,
                     FEventBuffer, TISC_CALLBACK(callback), PVoid(FAsyncEventCallback)));
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
    with Firebird25ClientAPI do
       Call(isc_wait_for_event(StatusVector,@FDBHandle, FEventBufferlen,FEventBuffer,FResultBuffer));
  finally
    FInWaitState := false;
  end;
end;

end.

