unit Test13;

{$mode objfpc}{$H+}
{$codepage UTF8}

{Test 13: Tranasction over two databases}

{
  The objective of this test is to test multi-database transactions. Two new
  databases are created and both are populated with a table and data. The data
  insert uses the same transaction. This is committed. Both are then read back
  to ensure that the data has been written to both.
}


interface

uses
  Classes, SysUtils, TestManager, IB;

type
    { TTest13 }

  TTest13 = class(TTestBase)
  private
    procedure UpdateDatabase(Attachment: IAttachment);
    procedure QueryDatabase(Attachment: IAttachment);
    procedure ModifyDatabase1(Attachment: IAttachment; Transaction: ITransaction);
    procedure ModifyDatabase2(Attachment: IAttachment; Transaction: ITransaction);
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
    'BlobData Blob sub_type 1 Character Set WIN1252, '+
    'BlobData2 Blob sub_type 1 Character Set UTF8, '+
    'InClear VarChar(16) Character Set OCTETS, '+
    'Primary Key(RowID)'+
    ')';


  sqlInsert = 'Insert into TestData(RowID,Title,Notes, BlobData,BlobData2,InClear) Values(:RowID,:Title,:Notes,:BlobData,:BlobData2,:InClear)';


{ TTest13 }

procedure TTest13.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlInsert);
  with Statement.GetSQLParams do
  begin
    ByName('rowid').AsInteger := 1;
    ByName('title').AsString := 'Blob Test ©€';
    ByName('Notes').AsString := 'Écoute moi';
    ByName('BlobData').AsString := 'Some German Special Characters like ÖÄÜöäüß';
    ByName('BlobData2').AsBlob := Attachment.CreateBlob(Transaction,'TestData','BlobData').SetString('Some German Special Characters like ÖÄÜöäüß');
    ByName('InClear').AsString := #$01'Test'#$0D#$C3;
  end;
  Statement.Execute;
end;

procedure TTest13.QueryDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);
end;

procedure TTest13.ModifyDatabase1(Attachment: IAttachment;
  Transaction: ITransaction);
var Statement: IStatement;
begin
  Statement := Attachment.Prepare(Transaction,'Update TestData Set Title = ''Database1''');
  Statement.Execute;
end;

procedure TTest13.ModifyDatabase2(Attachment: IAttachment;
  Transaction: ITransaction);
var Statement: IStatement;
begin
  Statement := Attachment.Prepare(Transaction,'Update TestData Set Title = ''Database2''');
  Statement.Execute;
end;

function TTest13.TestTitle: string;
begin
  Result := 'Test 13: Transaction over two databases';
end;

procedure TTest13.RunTest(CharSet: string; SQLDialect: integer);
var DPB: IDPB;
    Attachment, Attachment2: IAttachment;
    Transaction: ITransaction;
begin
  FHexStrings := true;
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);

  writeln(OutFile,'Init Database 1');
  UpdateDatabase(Attachment);
  QueryDatabase(Attachment);

  {Now create second identical database}

  Attachment2 := FirebirdAPI.CreateDatabase(Owner.GetSecondNewDatabaseName,DPB);
  Attachment2.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);

  writeln(OutFile,'Init Database 2');
  UpdateDatabase(Attachment2);
  QueryDatabase(Attachment2);

  Transaction := FirebirdAPI.StartTransaction(
                   [Attachment,Attachment2],
                   [isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency], taCommit);

  ModifyDatabase1(Attachment,Transaction);
  ModifyDatabase2(Attachment2,Transaction);

  Transaction.PrepareForCommit;
  Transaction.Commit;

  QueryDatabase(Attachment);
  QueryDatabase(Attachment2);

  Attachment.DropDatabase;
  Attachment2.DropDatabase;

end;

initialization
  RegisterTest(TTest13);
end.

