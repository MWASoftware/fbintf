unit Test17;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

{Test 17: Date/Time tests and Firebird 4 extensions}

{
  This test provides a test of the ISQLTimestamp interface both for pre-Firebird 4
  attachments and with the Time Zone SQL types introduced in Firebird 4.

  1. A new temporary database is created and a single table added containing
     columns for each time zone type (all Firebird versions).

  2. Data insert is performed for the various ways of setting the column values.

  3. A Select query is used to read back the rows, testing out the data read variations.

  4. If the client library and attached server are Firbird 4 or later, then the
     tests are repeated with time zones added.
}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest17 }

  TTest17 = class(TTestBase)
  private
    procedure UpdateDatabase(Attachment: IAttachment);
    procedure QueryDatabase(Attachment: IAttachment; aTableName: AnsiString);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses IBUtils;

const
    sqlCreateTable =
    'Create Table TestData ('+
    'RowID Integer not null,'+
    'DateCol DATE,'+
    'TimeCol TIME,'+
    'TimestampCol TIMESTAMP,'+
    'Primary Key(RowID)'+
    ')';


    sqlInsert2 = 'Insert into TestData(RowID,DateCol,TimeCol,TimestampCol) VALUES(?,?,?,?)';


{ TTest17 }

procedure TTest17.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    Timestamp: ISQLParamTimestamp;
    FPCTimestamp: TTimestamp;
    sqlInsert: AnsiString;
    FormatSettings: TFormatSettings;
    SysTime: TSystemTime;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DateSeparator := '.'; {Firebird convention}
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  sqlInsert := 'Insert into TestData(RowID,DateCol,TimeCol,TimestampCol) Values(1,'''+
              DateToStr(EncodeDate(2019,4,1),FormatSettings)+
              ''',''11:31:05.0001'','''+
              DateTimeToStr(EncodeDate(2016,2,29) + EncodeTime(22,2,35,10),FormatSettings) + ''')';
  Attachment.ExecuteSQL(Transaction,sqlInsert,[]);

  Statement := Attachment.Prepare(Transaction,SQLInsert2);

  Statement.SQLParams[0].AsInteger := 2;
  Statement.SQLParams[1].AsDate := EncodeDate(1939,9,3);
  Statement.SQLParams[2].AsTime := EncodeTime(15,40,0,0);
  Statement.SQLParams[3].AsDateTime := EncodeDate(1918,11,11) + EncodeTime(11,11,0,0);
  Statement.Execute;

  Statement.SQLParams[0].AsInteger := 3;
  Statement.SQLParams[1].AsDate := EncodeDate(1939,9,3);
  Statement.SQLParams[2].AsTime := EncodeTime(15,40,0,0);
  with SysTime do
  begin
    Year := 1918;
    Month := 11;
    Day := 11;
    Hour := 11;
    Minute := 11;
    Second := 0;
    Millisecond := 0;
  end;
  Timestamp := FirebirdAPI.GetSQLTimestampParam;
  Timestamp.SetAsSystemTime(SysTime);
  Statement.SQLParams[3].SetAsSQLTimestamp(Timestamp);
  Statement.Execute;

  Statement.SQLParams[0].AsInteger := 4;

  Timestamp.AsDate :=  EncodeDate(2019,12,25);
  Statement.SQLParams[1].SetAsSQLTimestamp(Timestamp);
  Timestamp.Clear;

  Timestamp.SetAsTime(0,1,40,1115);
  Statement.SQLParams[2].SetAsSQLTimestamp(Timestamp);
  Timestamp.Clear;

  FPCTimestamp.Date := Trunc(EncodeDate(2019,12,12)) + DateDelta;
  FPCTimestamp.Time :=  ((22*MinsPerHour + 10)*SecsPerMin)*MSecsPerSec + 1; {22:10:0.001)}
  Timestamp.SetAsTimestamp(FPCTimeStamp);
  Statement.SQLParams[3].SetAsSQLTimestamp(Timestamp);
  Statement.Execute;

end;

procedure TTest17.QueryDatabase(Attachment: IAttachment; aTableName: AnsiString);
var Transaction: ITransaction;
    Statement: IStatement;
    Results: IResultSet;
    SysTime: TSystemTime;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from  '+aTableName);
  writeln(OutFile);
  writeln(OutFile,'Testdata');
  writeln(OutFile);
  ReportResults(Statement);
  Statement := Attachment.Prepare(Transaction,'Select * from  '+aTableName);
  writeln(OutFile);
  writeln(OutFile,'Testdata - second pass');
  writeln(OutFile);
  Results := Statement.OpenCursor;
  if Results.FetchNext then
    writeln(OutFile,Results[0].AsInteger,', ',DateToStr(Results[1].AsDate),', ',
      TimeToStr(Results[2].AsTime),', ', DateTimeToStr(Results[3].AsDateTime));
  if Results.FetchNext then
    writeln(OutFile,Results[0].AsInteger,', ',DateToStr(Results[1].AsDate),', ',
      FormatDateTime('hh:nn:ss.zzz',Results[2].AsTime),', ', DateTimeToStr(Results[3].AsDateTime));
  if Results.FetchNext then
    writeln(OutFile,Results[0].AsInteger,', ',DateToStr(Results[1].AsDate),', ',
      FormatDateTime('hh:nn:ss.zzz',Results[2].AsTime),', ', DateTimeToStr(Results[3].AsDateTime));
  if Results.FetchNext then
  begin
    writeln(OutFile,Results[0].AsInteger,', ',DateToStr(Results[1].AsDate),', ',
      FormatDateTime('hh:nn:ss.zzz',Results[2].AsTime),', ', DateTimeToStr(Results[3].AsDateTime));
    SysTime := Results[3].GetAsSQLTimestamp.GetAsSystemTime;
    with SysTime do
    begin
      writeln(OutFile,'Sys Time = ',Year, ', ',Month, ', ',Day, ', ',DayOfWeek, ', ',Hour, ', ',Minute, ', ',Second, ', ',MilliSecond);
    end;
  end;
end;

function TTest17.TestTitle: AnsiString;
begin
  Result := 'Test 17: Date/Time tests and Firebird 4 extensions';
end;

procedure TTest17.RunTest(CharSet: AnsiString; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
  UpdateDatabase(Attachment);
  QueryDatabase(Attachment,'TestData');

  if FirebirdAPI.GetClientMajor < 4 then
    writeln(OutFile,'Skipping Firebird 4 and later test part')
  else
  begin
//    Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable2);

//    UpdateDatabase(Attachment);
 //   QueryDatabase(Attachment);
  end;
//  Attachment.DropDatabase;
end;

initialization
  RegisterTest(TTest17);
end.

