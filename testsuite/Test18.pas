unit Test18;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

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
    'BiggerNumber NUMERIC(26,4),'+
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
  Statement.SQLParams[4].AsBCD := StrToBCD('123456123456123456123456.123456');
  Statement.Execute;

  Statement.SQLParams[0].AsInteger := 4;
  Statement.SQLParams[1].AsBCD := 0;
  Statement.SQLParams[2].AsBCD := -1;
  Statement.SQLParams[3].AsBCD := 0;
  Statement.SQLParams[4].AsBCD := 0;
  Statement.Execute;
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
    writeln(OutFile,'Skipping Firebird 4 and later test part')
  else
  begin
    writeln(OutFile,'Firebird 4 extension types');
    writeln(OutFile);
    Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
    UpdateDatabase4_DECFloat(Attachment);
    QueryDatabase4_DECFloat(Attachment);
  end;
  Attachment.DropDatabase;
end;

initialization
  RegisterTest(TTest18);
end.


