unit Test8;

{$mode objfpc}{$H+}

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
    'MyArray VarChar(16) [0:16, -1:7],'+
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
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],tcCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  PrintMetaData(Statement.GetMetaData);
  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlInsert);
  with Statement.GetSQLParams do
  begin
    for i := 0 to GetCount - 1 do
      writeln('Param Name = ',Params[i].getName);
    ByName('rowid').AsInteger := 1;
    ByName('title').AsString := '2D Array';
  end;
  Statement.Execute;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);

  ar := Statement.CreateArray('MyArray');
  if ar <> nil then
  begin
    k := 50;
    for i := 0 to 16 do
      for j := -1 to 7 do
      begin
        ar.GetElement([i,j]).AsString := 'A' + IntToStr(k);
        Inc(k);
      end;
    Statement := Attachment.Prepare(Transaction,sqlUpdate);
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
    CreateParams: string;
    Attachment: IAttachment;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  CreateParams := 'USER ''' + Owner.GetUserName + ''' PASSWORD ''' + Owner.GetPassword + ''' ' +
      'DEFAULT CHARACTER SET ' + CharSet;
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,SQLDialect,CreateParams,DPB);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
  UpdateDatabase(Attachment);

  Attachment.DropDatabase;
end;

initialization
  RegisterTest(TTest8);

end.
