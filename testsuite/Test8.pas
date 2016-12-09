unit Test8;

{$mode objfpc}{$H+}
{$codepage utf8}

{Test 8: Create and read back an Array with 2 dimensions}

{
1. Create an empty database and populate with a single table including a two
   dimensional array of varchar(16) column.

2. Select all and show metadata including array metadata.

3. Insert a row but leave array null

4. Show result.

5. Update row with a populated array and show results.
}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest8 }

  TTest8 = class(TTestBase)
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
    'Title VarChar(32) Character Set UTF8,'+
    'MyArray VarChar(16) [0:16, -1:7] Character Set ISO8859_2,'+
    'Primary Key(RowID)'+
    ')';

  sqlInsert = 'Insert into TestData(RowID,Title) Values(:RowID,:Title)';

  sqlUpdate = 'Update TestData Set MyArray = ? Where RowID = 1';

{ TTest8 }

procedure TTest8.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
    i,j,k : integer;
    ar: IArray;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  PrintMetaData(Statement.GetMetaData);
  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlInsert);
  ParamInfo(Statement.GetSQLParams);
  with Statement.GetSQLParams do
  begin
    ByName('rowid').AsInteger := 1;
    ByName('title').AsString := '2D Array';
  end;
  Statement.Execute;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);

  Statement := Attachment.Prepare(Transaction,sqlUpdate);
  ar := Attachment.CreateArray(Transaction,'TestData','MyArray');
  if ar <> nil then
  begin
    k := 50;
    for i := 0 to 16 do
      for j := -1 to 7 do
      begin
        ar.SetAsString([i,j],'A' + IntToStr(k));
        Inc(k);
      end;
    Statement.SQLParams[0].AsArray := ar;
    Statement.Execute;
  end;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);
end;

function TTest8.TestTitle: string;
begin
  Result := 'Test 8: Create and read back an Array with 2 dimensions';
end;

procedure TTest8.RunTest(CharSet: string; SQLDialect: integer);
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
end;

initialization
  RegisterTest(TTest8);

end.

