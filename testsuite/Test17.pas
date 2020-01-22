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
  attachments and with the Time Zone SQL types introduced in Firebird 4. It also
  provides tests for the new DECFloat16 and DECFloat34 types.

  1. A new temporary database is created and a single table added containing
     columns for each time zone type (all Firebird versions).

  2. Data insert is performed for the various ways of setting the column values.

  3. A Select query is used to read back the rows, testing out the data read variations.

  4. If the client library and attached server are Firebird 4 or later, then the
     tests are repeated with time zones added.
}

interface

uses
  Classes, SysUtils, TestManager, IB {$IFDEF WINDOWS},Windows{$ENDIF};

type

  { TTest17 }

  TTest17 = class(TTestBase)
  private
    procedure QueryDatabase4_DECFloat(Attachment: IAttachment);
    procedure TestFBTimezoneSettings(Attachment: IAttachment);
    procedure UpdateDatabase(Attachment: IAttachment);
    procedure UpdateDatabase4_DECFloat(Attachment: IAttachment);
    procedure UpdateDatabase4_TZ(Attachment: IAttachment);
    procedure QueryDatabase(Attachment: IAttachment);
    procedure QueryDatabase4_TZ(Attachment: IAttachment);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses IBUtils, FmtBCD;

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

    sqlCreateTable2 =
    'Create Table FB4TestData_TZ ('+
    'RowID Integer not null,'+
    'TimeCol TIME WITH TIME ZONE,'+
    'TimestampCol TIMESTAMP WITH TIME ZONE,'+
    'Primary Key(RowID)'+
    ')';

    sqlCreateTable3 =
    'Create Table FB4TestData_DECFloat ('+
    'RowID Integer not null,'+
    'Float16 DecFloat(16),'+
    'Float34 DecFloat(34),'+
    'BigNumber NUMERIC(24,6),'+
    'BiggerNumber NUMERIC(26,4),'+
    'Primary Key(RowID)'+
    ')';

{ TTest17 }

procedure TTest17.TestFBTimezoneSettings(Attachment: IAttachment);
begin
  writeln(OutFile,'Test Time Zone Setting');
  with Attachment.GetSQLTimestampParam do
  begin
    SetTimeZoneID(65535);
    writeln(OutFile,'Time Zone GMT = ',TimeZone);
    SetTimeZoneID(65037);
    writeln(OutFile,'Time Zone Zagreb = ',TimeZone);
    SetTimeZoneID(959);
    writeln(OutFile,'Time Zone Offset = ',TimeZone);
    clear;
    SetTimeZoneID(2878);
    writeln(OutFile,'Time Zone Offset = ',TimeZone);
    clear;
    TimeZone := '+06:00';
    writeln(OutFile,'Time Zone ID = ',GetTimezoneID);
    clear;
    TimeZone := 'US/Arizona';
    writeln(OutFile,'Time Zone ID = ',GetTimezoneID);
  end;
end;

procedure TTest17.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    Timestamp: ISQLParamTimestamp;
    FPCTimestamp: TTimestamp;
    sqlInsert: AnsiString;
    {$IF defined(TFormatSettings)}
    FormatSettings: TFormatSettings;
    {$IFEND}
    SysTime: TSystemTime;
begin
  {$IF defined(TFormatSettings)}
  FormatSettings := DefaultFormatSettings;
  {$IFEND}
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
  {$IFDEF FPC}
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
  {$ELSE}
  with SysTime do
  begin
    wYear := 1918;
    wMonth := 11;
    wDay := 11;
    wHour := 11;
    wMinute := 11;
    wSecond := 0;
    wMilliseconds := 0;
  end;
  {$ENDIF}
  Timestamp := Attachment.GetSQLTimestampParam;
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
  FormatSettings.DateSeparator := '/'; {restore}
end;

procedure TTest17.UpdateDatabase4_TZ(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    sqlInsert: AnsiString;
    {$IF defined(TFormatSettings)}
    FormatSettings: TFormatSettings;
    {$IFEND}
    Timestamp: ISQLParamTimestamp;
begin
  {$IF defined(TFormatSettings)}
  FormatSettings := DefaultFormatSettings;
  {$IFEND}
  FormatSettings.DateSeparator := '.'; {Firebird convention}
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  sqlInsert := 'Insert into FB4TestData_TZ(RowID,TimeCol,TimestampCol) ' +
               'Values(1,''11:32:10.0002 -05:00'','''+
               DateToStr(EncodeDate(2020,4,1),FormatSettings)+
               ' 11:31:05.0001 +01:00'')';
  Attachment.ExecuteSQL(Transaction,sqlInsert,[]);


  Statement := Attachment.Prepare(Transaction,'Insert into FB4TestData_TZ(RowID,TimeCol,TimestampCol) Values(?,?,?)');

  Timestamp := Attachment.GetSQLTimestampParam;
  Statement.SQLParams[0].AsInteger := 2;
  Timestamp.AsTime := EncodeTime(14,02,10,05);
  Timestamp.Timezone := '-08:00';
  Statement.SQLParams[1].SetAsSQLTimestamp(Timestamp);
  Timestamp.Clear;
  Timestamp.AsDateTime := EncodeDate(1918,11,11) + EncodeTime(11,11,0,0);
  Timestamp.Timezone := 'Europe/London';
  Statement.SQLParams[2].SetAsSQLTimestamp(Timestamp);
  Statement.Execute;
  FormatSettings.DateSeparator := '/'; {restore}
end;

procedure TTest17.UpdateDatabase4_DECFloat(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    sqlInsert: AnsiString;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  sqlInsert := 'Insert into FB4TestData_DECFLoat(RowID,Float16,Float34,BigNumber) ' +
               'Values(1,64000000000.01,123456789123456789.12345678,123456123456.123456)';
  Attachment.ExecuteSQL(Transaction,sqlInsert,[]);
  sqlInsert := 'Insert into FB4TestData_DECFLoat(RowID,Float16,Float34,BigNumber) '+
               'Values(2,-64000000000.01,-123456789123456789.12345678,-123456123456.123456)';
  Attachment.ExecuteSQL(Transaction,sqlInsert,[]);


  Statement := Attachment.Prepare(Transaction,'Insert into FB4TestData_DECFLoat(RowID,Float16,Float34,BigNumber,BiggerNumber) VALUES (?,?,?,?,?)');

  Statement.SQLParams[0].AsInteger := 3;
  Statement.SQLParams[1].AsBCD := StrToBCD('64100000000.011');
  Statement.SQLParams[2].AsCurrency := 12345678912.12;
  Statement.SQLParams[3].AsString := '1234561234567.123456';
  Statement.SQLParams[4].AsBCD := StrToBCD('123456123456123456123456.123456');
  Statement.Execute;
end;

procedure TTest17.QueryDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    Results: IResultSet;
    SysTime: TSystemTime;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from  TestData');
  writeln(OutFile);
  writeln(OutFile,'Testdata');
  writeln(OutFile);
  PrintMetaData(Statement.MetaData);
  ReportResults(Statement);
  Statement := Attachment.Prepare(Transaction,'Select * from  TestData');
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
      {$IFDEF FPC}
      writeln(OutFile,'Sys Time = ',Year, ', ',Month, ', ',Day, ', ',DayOfWeek, ', ',Hour, ', ',Minute, ', ',Second, ', ',MilliSecond);
      {$ELSE}
      writeln(OutFile,'Sys Time = ',wYear, ', ',wMonth, ', ',wDay, ', ',wDayOfWeek, ', ',wHour, ', ',wMinute, ', ',wSecond, ', ',wMilliSeconds);
      {$ENDIF}
    end;
  end;
end;

procedure TTest17.QueryDatabase4_TZ(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    Results: IResultSet;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from  FB4TestData_TZ');
  writeln(OutFile);
  writeln(OutFile,'FB4 Testdata_TZ');
  writeln(OutFile);
  PrintMetaData(Statement.MetaData);
  Results := Statement.OpenCursor;
  try
    while Results.FetchNext do
    begin
      writeln(OutFile,'ROW ID = ',Results[0].AsInteger);
      write(OutFile,'TimeCol = ');
      if not Results[1].IsNull then
        with Results[1].GetAsSQLTimestamp do
          writeln(OutFile,GetAsString,', TimeZoneID = ',GetTimezoneID,', Time Zone Name = ',TimeZone)
      else
        writeln(OutFile,'NULL');
      write(OutFile,'TimeStampCol = ');
      if not Results[2].IsNull then
        with Results[2].GetAsSQLTimestamp do
          writeln(OutFile,GetAsString,', TimeZoneID = ',GetTimezoneID,', Time Zone Name = ',TimeZone)
      else
        writeln(OutFile,'NULL');
    end;
  finally
    Results.Close;
  end;
end;

procedure TTest17.QueryDatabase4_DECFloat(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    Results: IResultSet;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from  FB4TestData_DECFloat');
  writeln(OutFile);
  writeln(OutFile,'FB4 Testdata_DECFloat');
  writeln(OutFile);
  PrintMetaData(Statement.MetaData);
  Results := Statement.OpenCursor;
  try
    while Results.FetchNext do
    with Results do
    begin
      writeln(OutFile,'RowID = ',ByName('ROWID').GetAsString);
      writeln(OutFile,'Float16 = ', ByName('Float16').GetAsString);
      DumpBCD(ByName('Float16').GetAsBCD);
      writeln(OutFile,'Float34 = ', ByName('Float34').GetAsString);
      DumpBCD(ByName('Float34').GetAsBCD);
      writeln(OutFile,'BigNumber = ', ByName('BigNumber').GetAsString);
      DumpBCD(ByName('BigNumber').GetAsBCD);
      if not ByName('BiggerNumber').IsNull then
      begin
        writeln(OutFile,'BiggerNumber = ', ByName('BiggerNumber').GetAsString);
        DumpBCD(ByName('BiggerNumber').GetAsBCD);
      end;
      writeln;
    end;
  finally
    Results.Close;
  end;
end;

function TTest17.TestTitle: AnsiString;
begin
  Result := 'Test 17: Date/Time tests and Firebird 4 extensions';
end;

procedure TTest17.RunTest(CharSet: AnsiString; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
    VerStrings: TStringList;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  VerStrings := TStringList.Create;
  try
    Attachment.getFBVersion(VerStrings);
    writeln(OutFile,' FBVersion = ',VerStrings[0]);
  finally
    VerStrings.Free;
  end;
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
  UpdateDatabase(Attachment);
  QueryDatabase(Attachment);

  if (FirebirdAPI.GetClientMajor < 4) or (Attachment.GetODSMajorVersion < 13) then
    writeln(OutFile,'Skipping Firebird 4 and later test part')
  else
  begin
    writeln(OutFile,'Firebird 4 extension types');
    writeln(OutFile);
    TestFBTimezoneSettings(Attachment);

    Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable2);
    UpdateDatabase4_TZ(Attachment);
    QueryDatabase4_TZ(Attachment);
    Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable3);
    UpdateDatabase4_DECFloat(Attachment);
    QueryDatabase4_DECFloat(Attachment);
  end;
  Attachment.DropDatabase;
end;

initialization
  RegisterTest(TTest17);
end.

