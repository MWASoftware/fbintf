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
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBClientAPI, syncobjs, FBActivityMonitor;

type

  { TFBEvents }

  {Firebird Event and Result buffer syntax is:

    record
      version: byte;
      event: array of packed record
        strlen: byte;
        strchars: array of AnsiChar; //no of chars given by strlen
        EventCounts: long;
      end;
    end;

  }

  TFBEvents = class(TActivityReporter)
  private
    FEvents: TStringList;
    FAttachment: IAttachment;
    FEventCounts: TEventCounts;
    FFirebirdClientAPI: TFBClientAPI;
  protected
    FEventBuffer: PByte;
    FEventBufferLen: integer;
    FResultBuffer: PByte;
    FEventHandler: TEventHandler;
    FCriticalSection: TCriticalSection;
    FInWaitState: boolean;
    procedure CreateEventBlock;
    procedure CancelEvents(Force: boolean = false); virtual;
    procedure EventSignaled;
    function GetIEvents: IEvents; virtual; abstract;
    procedure ProcessEventCounts;
  public
    const EPB_version1 = 1;
  public
    constructor Create(DBAttachment: IAttachment; aMonitor: IActivityMonitor; Events: TStrings);
    destructor Destroy; override;

    {IEvents}
    procedure GetEvents(EventNames: TStrings);
    procedure SetEvents(EventNames: TStrings); overload;
    procedure SetEvents(Event: AnsiString); overload;
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

(* Original Firebird 'C' code

SLONG API_ROUTINE_VARARG isc_event_block(UCHAR** event_buffer,
  UCHAR** result_buffer,
  USHORT count, ...)
{
/**************************************
 *
 *      i s c _ e v e n t _ b l o c k
 *
 **************************************
 *
 * Functional description
 *      Create an initialized event parameter block from a
 *      variable number of input arguments.
 *      Return the size of the block.
 *
 *	Return 0 if any error occurs.
 *
 **************************************/
	va_list ptr;

	va_start(ptr, count);

	// calculate length of event parameter block, setting initial length to include version
	// and counts for each argument

	SLONG length = 1;
	USHORT i = count;
	while (i--)
	{
		const char* q = va_arg(ptr, SCHAR * );
		length += static_cast<SLONG>(strlen(q)) + 5;
	}
	va_end(ptr);

	UCHAR* p = *event_buffer = (UCHAR * ) gds__alloc((SLONG) length);
	// FREE: apparently never freed
	if (!*event_buffer)			// NOMEM:
		return 0;
	if ((*result_buffer = (UCHAR * ) gds__alloc((SLONG) length)) == NULL)
	{
		// NOMEM:
		// FREE: apparently never freed
		gds__free(*event_buffer);
		*event_buffer = NULL;
		return 0;
	}

	// initialize the block with event names and counts

	*p++ = EPB_version1;

	va_start(ptr, count);

	i = count;
	while (i--)
	{
		const char* q = va_arg(ptr, SCHAR * );

		// Strip the blanks from the ends
		const char* end = q + strlen(q);
		while (--end >= q && *end == ' ')
			;
		*p++ = end - q + 1;
		while (q <= end)
			*p++ = *q++;
		*p++ = 0;
		*p++ = 0;
		*p++ = 0;
		*p++ = 0;
	}
	va_end(ptr);

	return static_cast<SLONG>(p - *event_buffer);
}
*)

{CreateEventBlock effectively replaces isc_event_block}

procedure TFBEvents.CreateEventBlock;
var i: integer;
    P: PByte;
begin
  {calculate length of event parameter block, setting initial length to include version
   and counts for each argument}

  FEventBufferLen := 1;
  for i := 0 to FEvents.Count - 1 do
    FEventBufferLen := FEventBufferLen + length(FEvents[i]) + 1 + sizeof(Long);

  with FFirebirdClientAPI do
  begin
    IBAlloc(FEventBuffer,0,FEventBufferLen);
    if FEventBuffer = nil then Exit;
    FillByte(FEventBuffer^,FEventBufferLen,0);
    IBAlloc(FResultBuffer,0,FEventBufferLen);
    if FResultBuffer = nil then
    begin
      FreeMem(FEventBuffer);
      Exit;
    end;

    P := FEventBuffer;
    P^ := EPB_version1;
    Inc(P);
    SetLength(FEventCounts,FEvents.Count);

    for i := 0 to FEvents.Count - 1 do
    begin
      P^ := Length(FEvents[i]);
      Inc(P);
      Move(FEvents[i][1],P^,Length(FEvents[i]));
      Inc(P,Length(FEvents[i])+sizeof(Long));
      FEventCounts[i].EventName := FEvents[i];
    end;
  end;

{  for i := 0 to FEventBufferLen - 1 do
  write(Format('%x ', [FEventBuffer[i]]));
   writeln;}
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

(*
  Original Firebird 'C' code for isc_event_counts

void API_ROUTINE isc_event_counts(ULONG* result_vector,
								  SSHORT buffer_length,
								  UCHAR* event_buffer,
								  const UCHAR* result_buffer)
{
/**************************************
 *
 *	g d s _ $ e v e n t _ c o u n t s
 *
 **************************************
 *
 * Functional description
 *	Get the delta between two events in an event
 *	parameter block.  Used to update gds_events
 *	for GPRE support of events.
 *
 **************************************/
	ULONG* vec = result_vector;
	const UCHAR* p = event_buffer;
	const UCHAR* q = result_buffer;
	USHORT length = buffer_length;
	const UCHAR* const end = p + length;

	// analyze the event blocks, getting the delta for each event

	p++;
	q++;
	while (p < end)
	{
		// skip over the event name

		const USHORT i = (USHORT)* p++;
		p += i;
		q += i + 1;

		// get the change in count

		const ULONG initial_count = gds__vax_integer(p, sizeof(SLONG));
		p += sizeof(SLONG);
		const ULONG new_count = gds__vax_integer(q, sizeof(SLONG));
		q += sizeof(SLONG);
		*vec++ = new_count - initial_count;
	}

	// copy over the result to the initial block to prepare
	// for the next call to gds__event_wait

	memcpy(event_buffer, result_buffer, length);
}
*)

{ProcessEventCounts effectively replaces isc_event_counts}

procedure TFBEvents.ProcessEventCounts;

var i: integer;
    P, Q: PByte;
    initial_count: Long;
    new_count: Long;
    len: byte;
begin
  P := FEventBuffer;
  Q := FResultBuffer;
  Inc(P); {skip past version byte}
  Inc(Q);
  for i := 0 to Length(FEventCounts) - 1 do
  with FFirebirdClientAPI do
  begin
    {skip over the event name}
    len := P^;
    P := P + len + 1;
    Q := Q + len + 1; {event name length in P^}
    initial_count := DecodeInteger(P,sizeof(Long));
    Inc(P,sizeof(Long));
    new_count := DecodeInteger(Q,sizeof(Long));
    Inc(Q,sizeof(Long));
    FEventCounts[i].Count := new_count - initial_count;
  end;
  Move(FResultBuffer^,FEventBuffer^,FEventBufferLen);
end;

constructor TFBEvents.Create(DBAttachment: IAttachment;
  aMonitor: IActivityMonitor; Events: TStrings);
begin
  inherited Create(aMonitor);
  FAttachment := DBAttachment;
  FFirebirdClientAPI := DBAttachment.getFirebirdAPI as TFBClientAPI;
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
  with FFirebirdClientAPI do
  begin
    if FEventBuffer <> nil then
      FreeMem( FEventBuffer);
    if FResultBuffer <> nil then
      FreeMem( FResultBuffer);
  end;
  inherited Destroy;
end;

procedure TFBEvents.GetEvents(EventNames: TStrings);
begin
  EventNames.Assign(FEvents)
end;

procedure TFBEvents.SetEvents(EventNames: TStrings);
var i: integer;
begin
  {$ifdef Unix}
  if (EventNames.Count > 0) and not IsMultiThread then
    IBError(ibxeMultiThreadRequired,['Firebird Events Handling']);
  {$endif}
  if EventNames.Text <> FEvents.Text then
  begin
    Cancel;
    for i := 0 to EventNames.Count - 1 do
      FEvents[i] := Trim(EventNames[i]);
    CreateEventBlock;
  end;
end;

procedure TFBEvents.SetEvents(Event: AnsiString);
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

