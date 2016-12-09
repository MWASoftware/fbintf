unit Test6;

{$mode objfpc}{$H+}
{$codepage UTF8}

{Test 6: Blob Handling}

{
  1. Create an empty database and populate with a single table.

  2. Show the character sets available (List RDB$CHARACTER_SETS)

  3. Select all from new table and show metadata.

  4. Insert row and include WIN1252 characters known to be in two byte UTF8, plus Fixed point

  5. Select all from new table

  6. Use Update Query to set blob field with plain text loaded from file

  7. Select all from new table

  8. Add another row with a null blob

  9. Update this row's blob field with a copy of the first row (demo of blob assignment)

  10. Select all from new table.

  11. Drop Database and repeat above but with no default connection character set.
}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest6 }

  TTest6 = class(TTestBase)
  private
    procedure UpdateDatabase(Attachment: IAttachment);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;

implementation

const
  sqlCreateTable =
    'Create Table TestData ('+
    'RowID Integer not null,'+
    'FixedPoint Decimal(8,2), '+
    'FloatingPoint Double Precision, '+
    'Title VarChar(32) Character Set UTF8,'+
    'BlobData Blob sub_type 1 Character Set UTF8,'+
    'Primary Key(RowID)'+
    ')';

  sqlGetCharSets = 'Select RDB$CHARACTER_SET_NAME,RDB$CHARACTER_SET_ID from RDB$CHARACTER_SETS order by 2';

  sqlInsert = 'Insert into TestData(RowID,Title,FixedPoint,FloatingPoint) Values(:RowID,:Title,:FP, :DP)';

  sqlUpdate = 'Update TestData Set BlobData = ? Where RowID = ?';


{ TTest6 }

procedure TTest6.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement,
    Statement2: IStatement;
    ResultSet: IResultSet;
    i: integer;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);

  Statement := Attachment.Prepare(Transaction,sqlGetCharSets);
  PrintMetaData(Statement.GetMetaData);
  ReportResults(Statement);
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  PrintMetaData(Statement.GetMetaData);
  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlInsert);
  ParamInfo(Statement.SQLParams);
  with Statement.GetSQLParams do
  begin
    ByName('rowid').AsInteger := 1;
    ByName('title').AsString := 'Blob Test ©€';
    ByName('Fp').AsDouble := 20.28;
    ByName('DP').AsDouble := 3.142;
  end;
  Statement.Execute;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);


  Statement := Attachment.Prepare(Transaction,sqlUpdate);
  ParamInfo(Statement.SQLParams);
  Statement.SQLParams[0].AsBlob := Attachment.CreateBlob(Transaction,'TestData','BlobData').LoadFromFile('testtext.txt');
  Statement.SQLParams[1].AsInteger := 1;
  Statement.Execute;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);

  {second row}
  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlInsert);
  ParamInfo(Statement.SQLParams);
  with Statement.GetSQLParams do
  begin
    ByName('rowid').AsInteger := 2;
    ByName('title').AsString := 'Blob Test ©€';
  end;
  Statement.Execute;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData Where rowid = 1');
  ResultSet := Statement.OpenCursor;
  if ResultSet.FetchNext then
  begin
    Statement2 := Attachment.Prepare(Transaction,sqlUpdate);
    Statement2.SQLParams[0].AsBlob := ResultSet.ByName('BlobData').AsBlob; {test duplication of blob}
    Statement2.SQLParams[1].AsInteger := 2;
    Statement2.Execute;
    Statement := Attachment.Prepare(Transaction,'Select * from TestData');
    ReportResults(Statement);
  end;
end;

function TTest6.TestTitle: string;
begin
  Result := 'Test 6: Blob Handling';
end;

procedure TTest6.RunTest(CharSet: string; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
  UpdateDatabase(Attachment);

  Attachment.DropDatabase;

  {Repeat with no lc_ctype}
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
  UpdateDatabase(Attachment);

  Attachment.DropDatabase;
end;

initialization
  RegisterTest(TTest6);
end.

