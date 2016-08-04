unit Test6;

{$mode objfpc}{$H+}

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
    'Title VarChar(32) Character Set UTF8,'+
    'Notes VarChar(64) Character Set ISO8859_1,'+
    'BlobData Blob sub_type 1 Character Set UTF8,'+
    'Primary Key(RowID)'+
    ')';

  sqlGetCharSets = 'Select RDB$CHARACTER_SET_NAME,RDB$CHARACTER_SET_ID from RDB$CHARACTER_SETS order by 2';

  sqlInsert = 'Insert into TestData(RowID,Title,Notes) Values(:RowID,:Title,:Notes)';

  sqlUpdate = 'Update TestData Set BlobData = ? Where RowID = 1';

{ TTest6 }

procedure TTest6.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
    B: IBlob;
    i: integer;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],tcCommit);
  Statement := Attachment.Prepare(Transaction,sqlGetCharSets);
  ReportResults(Statement);
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

  Statement := Attachment.Prepare(Transaction,sqlUpdate);
  B := Statement.CreateBlob;
  B.LoadFromFile('testtext.txt');
  Statement.SQLParams[0].AsBlob := B;
  Statement.Execute;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);
end;

function TTest6.TestTitle: string;
begin
  Result := 'Test 6: Blob Handling';
end;

procedure TTest6.RunTest(CharSet: string; SQLDialect: integer);
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
  RegisterTest(TTest6);
end.

