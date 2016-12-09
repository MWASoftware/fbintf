unit Test10;

{$mode objfpc}{$H+}
{$codepage utf8}

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
  Classes, SysUtils, TestManager, IB;

type

{ TTest10 }

  TTest10 = class(TTestBase)
  private
    FEventSignalled: boolean;
    procedure EventsTest(Attachment: IAttachment);
    procedure EventReport(Sender: IEvents);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;


implementation

{ TTest10 }

const
  sqlEvent = 'Execute Block As Begin Post_Event(''TESTEVENT''); End';

procedure TTest10.EventsTest(Attachment: IAttachment);
var EventHandler: IEvents;
    EventCounts: TEventCounts;
    i: integer;
    WaitCount: integer;
begin
  EventHandler := Attachment.GetEventHandler('TESTEVENT');
  writeln(OutFile,'Call Async Wait');
  EventHandler.AsyncWaitForEvent(@EventReport);
  writeln(OutFile,'Async Wait Called');

  writeln(OutFile,'Signal Event');
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  while not FEventSignalled do;
  writeln(OutFile,'Event Signalled');
  EventCounts := EventHandler.ExtractEventCounts;
  for i := 0 to length(EventCounts) - 1 do
    writeln(OutFile,'Event: ',EventCounts[i].EventName,', Count = ',EventCounts[i].Count);
  FEventSignalled := false;

  writeln(OutFile,'Two more events');
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  writeln(OutFile,'Call Async Wait');
  EventHandler.AsyncWaitForEvent(@EventReport);
  writeln(OutFile,'Async Wait Called');
  writeln(OutFile,'Signal Event');
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  while not FEventSignalled do;
  writeln(OutFile,'Event Signalled');
  EventCounts := EventHandler.ExtractEventCounts;
  for i := 0 to length(EventCounts) - 1 do
    writeln(OutFile,'Event: ',EventCounts[i].EventName,', Count = ',EventCounts[i].Count);

  FEventSignalled := false;
  writeln(OutFile,'Async Wait: Test Cancel');
  EventHandler.AsyncWaitForEvent(@EventReport);
  writeln(OutFile,'Async Wait Called');
  EventHandler.Cancel;
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  WaitCount := 100000000;
  while not FEventSignalled and (WaitCount > 0) do Dec(WaitCount);
  if WaitCount = 0 then writeln(OutFile,'Time Out - Cancel Worked!')
  else
    writeln(OutFile,'Event called - so Cancel failed');

  writeln(OutFile,'Sync wait');
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  EventHandler.WaitForEvent;
  writeln(OutFile,'Event Signalled');
  EventCounts := EventHandler.ExtractEventCounts;
  for i := 0 to length(EventCounts) - 1 do
    writeln(OutFile,'Event: ',EventCounts[i].EventName,', Count = ',EventCounts[i].Count);
end;

procedure TTest10.EventReport(Sender: IEvents);
begin
  FEventSignalled := true;
end;

function TTest10.TestTitle: string;
begin
  Result := 'Test 10: Event Handling';
end;

procedure TTest10.RunTest(CharSet: string; SQLDialect: integer);
var Attachment: IAttachment;
    DPB: IDPB;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(' ');
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  DPB.Find(isc_dpb_password).setAsString(Owner.GetPassword);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  EventsTest(Attachment);
  Attachment.Disconnect;
end;

initialization
  RegisterTest(TTest10);

end.

