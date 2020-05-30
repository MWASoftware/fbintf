unit Test18;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

{ $DEFINE TESTINT128ARRAY}  {Depends on resolution of CORE-6302}

{Test 18: Firebird 4 extensions: DecFloat data types}

{
  This test provides  tests for the new DECFloat16 and DECFloat34 types. The test
  is skipped if not a Firebird 4 Client.

  1. A new temporary database is created and a single table added containing
     columns for each DecFloat data type.

  2. Data insert is performed for the various ways of setting the column values.

  3. A Select query is used to read back the rows, testing out the data read variations.

}

interface

uses
  Classes, SysUtils, TestManager, IB {$IFDEF WINDOWS},Windows{$ENDIF};

type

  { TTest18 }

  TTest18 = class(TTestBase)
  private
    procedure QueryDatabase4_DECFloat(Attachment: IAttachment);
    procedure UpdateDatabase4_DECFloat(Attachment: IAttachment);
    procedure ArrayTest(Attachment: IAttachment);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses IBUtils, FmtBCD;

const
    sqlCreateTable =
    'Create Table FB4TestData_DECFloat ('+
    'RowID Integer not null,'+
    'Float16 DecFloat(16),'+
    'Float34 DecFloat(34),'+
    'BigNumber NUMERIC(24,6),'+
    'BiggerNumber NUMERIC(34,4),'+
    'Primary Key(RowID)'+
    ')';

    sqlCreateTable2 =
    'Create Table FB4TestData_DECFloat_AR ('+
    'RowID Integer not null,'+
    'Float16 DecFloat(16) [0:16],'+
    'Float34 DecFloat(34) [0:16],'+
    'BigNumber NUMERIC(24,6) [0:16],'+
    'Primary Key(RowID)'+
    ')';

{ TTest18 }

procedure TTest18.UpdateDatabase4_DECFloat(Attachment: IAttachment);
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
  Statement.SQLParams[4].AsBCD := StrToBCD('11123456123456123456123456123456.123456');
  Statement.Execute;

  Statement.SQLParams[0].AsInteger := 4;
  Statement.SQLParams[1].AsBCD := 0;
  Statement.SQLParams[2].AsBCD := -1;
  Statement.SQLParams[3].AsBCD := 0;
  Statement.SQLParams[4].AsBCD := 0;
  Statement.Execute;
end;

procedure TTest18.ArrayTest(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
    ar: IArray;
    value: tBCD;
    i: integer;
begin
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable2);
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * From FB4TestData_DECFloat_AR');
  Printmetadata(Statement.MetaData);
  Attachment.Prepare(Transaction,'Insert into FB4TestData_DECFloat_AR (RowID) Values(1)').Execute;

  {Float16}
  ar := Attachment.CreateArray(Transaction,'FB4TestData_DECFloat_AR','Float16');
  value := StrToBCD('64100000000.011');
  for i := 0 to 16 do
  begin
    ar.SetAsBcd(i,value);
    value := value + 1;
  end;

  Statement := Attachment.Prepare(Transaction,'Update FB4TestData_DECFloat_AR Set Float16 = ? Where RowID = 1');
  Statement.SQLParams[0].AsArray := ar;
  Statement.Execute;

  {Float 34}
  ar := Attachment.CreateArray(Transaction,'FB4TestData_DECFloat_AR','Float34');
  value := StrToBCD('123456789123456789.12345678');
  for i := 0 to 16 do
  begin
    ar.SetAsBcd(i,value);
    value := value + 1;
  end;

  Statement := Attachment.Prepare(Transaction,'Update FB4TestData_DECFloat_AR Set Float34 = ? Where RowID = 1');
  Statement.SQLParams[0].AsArray := ar;
  Statement.Execute;

  {NUMERIC(24,6)}
  {$IFDEF TESTINT128ARRAY}
  ar := Attachment.CreateArray(Transaction,'FB4TestData_DECFloat_AR','BigNumber');
  value := StrToBCD('123456123400.123456');
  for i := 0 to 16 do
  begin
    ar.SetAsBcd(i,value);
    value := value + 1.5;
  end;

  Statement := Attachment.Prepare(Transaction,'Update FB4TestData_DECFloat_AR Set BigNumber = ? Where RowID = 1');
  Statement.SQLParams[0].AsArray := ar;
  Statement.Execute;
  {$ENDIF}

  Statement := Attachment.Prepare(Transaction,'Select RowID, Float16, Float34,BigNumber From FB4TestData_DECFloat_AR');
  writeln(OutFile);
  writeln(OutFile,'Decfloat Arrays');
  ResultSet := Statement.OpenCursor;
  while ResultSet.FetchNext do
  begin
    writeln('Row No ',ResultSet[0].AsInteger);
    write(OutFile,'Float16 ');
    ar := ResultSet[1].AsArray;
    WriteArray(ar);
    write(OutFile,'Float34 ');
    ar := ResultSet[2].AsArray;
    WriteArray(ar);
    {$IFDEF TESTINT128ARRAY}
    write(OutFile,'BigNumber ');
    ar := ResultSet[3].AsArray;
    WriteArray(ar);
    {$ENDIF}
  end;
end;

procedure TTest18.QueryDatabase4_DECFloat(Attachment: IAttachment);
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

function TTest18.TestTitle: AnsiString;
begin
  Result := 'Test 18: Firebird 4 Decfloat extensions';
end;

procedure TTest18.RunTest(CharSet: AnsiString; SQLDialect: integer);
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

  if (FirebirdAPI.GetClientMajor < 4) or (Attachment.GetODSMajorVersion < 13) then
    writeln(OutFile,'Skipping test for Firebird 4 and later')
  else
  begin
    Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
    UpdateDatabase4_DECFloat(Attachment);
    QueryDatabase4_DECFloat(Attachment);
    ArrayTest(Attachment);
  end;
  Attachment.DropDatabase;
end;

initialization
  RegisterTest(TTest18);
end.


