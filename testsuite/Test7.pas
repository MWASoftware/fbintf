unit Test7;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest7 }

  TTest7 = class(TTestBase)
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
    'Notes VarChar(64) Character Set ISO8859_1,'+
    'MyArray Integer [0:16],'+
    'Primary Key(RowID)'+
    ')';

  sqlInsert = 'Insert into TestData(RowID,Title,Notes) Values(:RowID,:Title,:Notes)';

  sqlUpdate = 'Update TestData Set MyArray = ? Where RowID = 1';

{ TTest7 }

procedure TTest7.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
    i,j: integer;
    ar: IArray;
    col: IColumnMetaData;
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
    ByName('title').AsString := 'Blob Test ©€';
    ByName('Notes').AsString := 'Écoute moi';
  end;
  Statement.Execute;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);

  col := Statement.GetMetaData.ByName('MyArray');
  ar := Statement.CreateArray(col);
  j := 100;
  for i := 0 to 16 do
  begin
    ar.SetAsInteger([i],j);
    dec(j);
  end;
  Statement := Attachment.Prepare(Transaction,sqlUpdate);
  Statement.SQLParams[0].AsArray := ar;
  Statement.Execute;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);

  ResultSet := Statement.OpenCursor;
  if Resultset.FetchNext then
  begin
    ar := ResultSet.ByName('MyArray').AsArray;
    ar.SetBounds(0,10,2);
    writeln('Shrink to 2:10');
    WriteArray(ar);
  end
  else
    writeln('Unable to reopen cursor');
end;

function TTest7.TestTitle: string;
begin
  Result := 'Test 7: Create and read back an Array';
end;

procedure TTest7.RunTest(CharSet: string; SQLDialect: integer);
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
  RegisterTest(TTest7);

end.

