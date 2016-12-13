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
unit FBEvents;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBClientAPI, syncobjs, FBActivityMonitor;

type

  { TFBEvents }

  TFBEvents = class(TActivityReporter)
  private
    FEvents: TStringList;
    FAttachment: IAttachment;
    FEventCounts: TEventCounts;
  protected
    FEventBuffer: PChar;
    FEventBufferLen: integer;
    FResultBuffer: PChar;
    FEventHandler: TEventHandler;
    FCriticalSection: TCriticalSection;
    FInWaitState: boolean;
    procedure CreateEventBlock;
    procedure CancelEvents(Force: boolean = false); virtual;
    procedure EventSignaled;
    function GetIEvents: IEvents; virtual; abstract;
    procedure ProcessEventCounts;
  public
    constructor Create(DBAttachment: IAttachment; aMonitor: IActivityMonitor; Events: TStrings);
    destructor Destroy; override;

    {IEvents}
    procedure GetEvents(EventNames: TStrings);
    procedure SetEvents(EventNames: TStrings); overload;
    procedure SetEvents(Event: string); overload;
    procedure Cancel;
    function ExtractEventCounts: TEventCounts;
    function GetAttachment: IAttachment;
    procedure AsyncWaitForEvent(EventHandler: TEventHandler); virtual; abstract;
  end;


implementation

uses FBMessages, IBExternals;

const
  MaxEvents = 15;

{ TFBEvents }

procedure TFBEvents.CreateEventBlock;
var
  i: integer;
  EventNames: array of PChar;
begin
  with FirebirdClientAPI do
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

procedure TFBEvents.CancelEvents(Force: boolean);
begin
  FEventHandler := nil;
end;

procedure TFBEvents.EventSignaled;
var Handler: TEventHandler;
begin
  Handler := nil;
  FCriticalSection.Enter;
  try
    if not FInWaitState then Exit;
    FInWaitState := false;
    ProcessEventCounts;
    if assigned(FEventHandler)  then
    begin
      Handler := FEventHandler;
      FEventHandler := nil;
    end;
  finally
    FCriticalSection.Leave;
  end;
  if assigned(Handler) then
    Handler(GetIEvents);
end;

procedure TFBEvents.ProcessEventCounts;
var P: PISC_LONG;
    EventCountList: array[0..19] of ISC_LONG;
    i: integer;
    j: integer;
begin
  SetLength(FEventCounts,0);
  if FResultBuffer = nil then Exit;

  FillChar(EventCountList,sizeof(EventCountList),0);

  with FirebirdClientAPI do
     isc_event_counts( @EventCountList, FEventBufferLen, FEventBuffer, FResultBuffer);
  j := 0;
  P := EventCountList;
  for i := 0 to FEvents.Count - 1 do
  begin
    if EventCountList[i] <> 0 then
    begin
      Inc(j);
      SetLength(FEventCounts,j);
      FEventCounts[j-1].EventName := FEvents[i];
      FEventCounts[j-1].Count := P^;
      Inc(P);
//      writeln('Event: ',FEventCounts[j-1].EventName,' Count = ',FEventCounts[j-1].Count);
    end;
  end;
end;

constructor TFBEvents.Create(DBAttachment: IAttachment;
  aMonitor: IActivityMonitor; Events: TStrings);
begin
  inherited Create(aMonitor);
  FAttachment := DBAttachment;
  if Events.Count > MaxEvents then
    IBError(ibxeMaximumEvents, [nil]);

  FCriticalSection := TCriticalSection.Create;
  FEvents := TStringList.Create;
  FEvents.Assign(Events);
  CreateEventBlock;
end;

destructor TFBEvents.Destroy;
begin
  if assigned(FCriticalSection) then FCriticalSection.Free;
  if assigned(FEvents) then FEvents.Free;
  with FirebirdClientAPI do
  begin
    if FEventBuffer <> nil then
      isc_free( FEventBuffer);
    if FResultBuffer <> nil then
      isc_free( FResultBuffer);
  end;
  inherited Destroy;
end;

procedure TFBEvents.GetEvents(EventNames: TStrings);
begin
  EventNames.Assign(FEvents)
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

function TFBEvents.ExtractEventCounts: TEventCounts;
begin
  Result := FEventCounts;
end;

function TFBEvents.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

end.

