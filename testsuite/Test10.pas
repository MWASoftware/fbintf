(*
 *  Firebird Interface (fbintf) Test suite. This program is used to
 *  test the Firebird Pascal Interface and provide a semi-automated
 *  pass/fail check for each test.
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

unit Test10;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{Test 10: Event Handling}

{
  This test opens the employee example databases with the supplied user name/password
  and then tests event handling.

  1. Simple wait for async event.

  2. Signal two more events to show that events counts are maintained.

  3. Async Event wait followed by signal event. Event Counts should include all
     previous events.

  4. Demonstrate event cancel by waiting for event, cancelling it and then signalling
     event. No change to signal flag after waiting in a tight loop implies event cancelled.

  5. Wait for sync Event.
}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type

{ TTest10 }

  TTest10 = class(TFBTestBase)
  private
    FEventSignalled: boolean;
    procedure EventsTest(Attachment: IAttachment);
    procedure EventReport(Sender: IEvents);
    procedure ShowEventReport;
    procedure ShowEventCounts(Intf: IEvents);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest10 }

const
  sqlEvent = 'Execute Block As Begin Post_Event(''TESTEVENT''); End';

procedure TTest10.EventsTest(Attachment: IAttachment);
var EventHandler: IEvents;
    WaitCount: integer;
begin
  FEventSignalled := false;
  EventHandler := Attachment.GetEventHandler('TESTEVENT');
  writeln(OutFile,'Call Async Wait');
  EventHandler.AsyncWaitForEvent(EventReport);
  writeln(OutFile,'Async Wait Called');
  sleep(500);
  CheckSynchronize;
  if FEventSignalled then
  begin
    writeln(OutFile,'First Event - usually ignored');
    FEventSignalled := false;
    EventHandler.AsyncWaitForEvent(EventReport);
    sleep(100);
    CheckSynchronize;
    if FEventSignalled then
    begin
      writeln(OutFile,'Unexpected Event 1');
      Exit;
    end;
  end;
  writeln(OutFile,'Signal Event');
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  while not FEventSignalled do
  begin
    Sleep(50);
    CheckSynchronize;
  end;
  ShowEventCounts(EventHandler);
  FEventSignalled := false;

  writeln(OutFile,'Two more events');
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  if FEventSignalled then
  begin
    writeln(OutFile,'Unexpected Event 2');
    FEventSignalled := false
  end;
  writeln(OutFile,'Call Async Wait');
  EventHandler.AsyncWaitForEvent(EventReport);
  writeln(OutFile,'Async Wait Called');
  sleep(500);
  CheckSynchronize;
  if FEventSignalled then
  begin
    writeln(OutFile,'Deferred Events Caught');
    ShowEventCounts(EventHandler);
    FEventSignalled := false;
    EventHandler.AsyncWaitForEvent(EventReport);
    CheckSynchronize;
    sleep(100);
    if FEventSignalled then
      writeln(OutFile,'Unexpected Event 3');
  end;
  writeln(OutFile,'Signal Event');
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  while not FEventSignalled do;
  ShowEventCounts(EventHandler);

  FEventSignalled := false;
  writeln(OutFile,'Async Wait: Test Cancel');
  EventHandler.AsyncWaitForEvent(EventReport);
  CheckSynchronize;
  writeln(OutFile,'Async Wait Called');
  EventHandler.Cancel;
  writeln(OutFile,'Event Cancelled');
  FEventSignalled := false;
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  WaitCount := 100000000;
  while not FEventSignalled and (WaitCount > 0) do Dec(WaitCount);
  if WaitCount = 0 then writeln(OutFile,'Time Out - Cancel Worked!')
  else
    writeln(OutFile,'Event called - so Cancel failed');

  writeln(OutFile,'Sync wait');
  CheckSynchronize;
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  CheckSynchronize;
  EventHandler.WaitForEvent;
  writeln(OutFile,'Event Signalled');
  ShowEventCounts(EventHandler);
  EventHandler := nil;
  CheckSynchronize;
end;

procedure TTest10.EventReport(Sender: IEvents);
begin
  FEventSignalled := true;
  TThread.Synchronize(nil,ShowEventReport);
end;

procedure TTest10.ShowEventReport;
begin
  writeln(OutFile,'Event Signalled');
end;

procedure TTest10.ShowEventCounts(Intf: IEvents);
var
  i: integer;
  EventCounts: TEventCounts;
begin
  EventCounts := Intf.ExtractEventCounts;
  for i := 0 to length(EventCounts) - 1 do
    writeln(OutFile,'Event Counts: ',EventCounts[i].EventName,', Count = ',EventCounts[i].Count);
end;

function TTest10.TestTitle: AnsiString;
begin
  Result := 'Test 10: Event Handling';
end;

procedure TTest10.RunTest(CharSet: AnsiString; SQLDialect: integer);
var Attachment: IAttachment;
    DPB: IDPB;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(' ');
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Find(isc_dpb_password).setAsString(Owner.GetPassword);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  EventsTest(Attachment);
  Attachment.Disconnect;
end;

initialization
  RegisterTest(TTest10);

end.

