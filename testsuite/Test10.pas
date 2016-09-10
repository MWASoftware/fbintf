unit Test10;

{$mode objfpc}{$H+}

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
  EventHandler.AsyncWaitForEvent(@EventReport);

  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  while not FEventSignalled do;
  writeln('Event Signalled');
  EventCounts := EventHandler.ExtractEventCounts;
  for i := 0 to length(EventCounts) - 1 do
    writeln('Event: ',EventCounts[i].EventName,', Count = ',EventCounts[i].Count);
  FEventSignalled := false;

  writeln('Repeat');
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  EventHandler.AsyncWaitForEvent(@EventReport);
  while not FEventSignalled do;
  writeln('Event Signalled');
  EventCounts := EventHandler.ExtractEventCounts;
  for i := 0 to length(EventCounts) - 1 do
    writeln('Event: ',EventCounts[i].EventName,', Count = ',EventCounts[i].Count);

  FEventSignalled := false;
  EventHandler.AsyncWaitForEvent(@EventReport);
  EventHandler.Cancel;
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
  WaitCount := 100000000;
  while not FEventSignalled and (WaitCount > 0) do Dec(WaitCount);
  if WaitCount = 0 then writeln('Time Out - Cancel Worked!');

  if FirebirdAPI.HasSynchronousEventWait then
  begin
    writeln('Sync wait');
    Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlEvent);
    EventHandler.WaitForEvent;
    writeln('Event Signalled');
    EventCounts := EventHandler.ExtractEventCounts;
    for i := 0 to length(EventCounts) - 1 do
      writeln('Event: ',EventCounts[i].EventName,', Count = ',EventCounts[i].Count);
  end;
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

