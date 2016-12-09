unit Test14;

{$mode objfpc}{$H+}
{$codepage UTF8}

{Test 14: Non select procedures}

{ this test creates a new database with a table and two stored procedures.

  1. The first stored procedure is run to populate the table

  2. The second returns data from the table, which is written out.
}

interface

uses
  Classes, SysUtils, TestManager, IB;

type
    { TTest14 }

  TTest14 = class(TTestBase)
  private
    procedure UpdateDatabase(Attachment: IAttachment);
    procedure QueryDatabase(Attachment: IAttachment);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;

implementation

{ TTest14 }

const
  SQLInsert = 'Execute Procedure InsertData';

procedure TTest14.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);

  Statement := Attachment.Prepare(Transaction,sqlInsert);
  Statement.Execute;
end;

const
  sqlCallQueryProc = 'Execute Procedure ShowData';

procedure TTest14.QueryDatabase(Attachment: IAttachment);
var Transaction, Transaction2: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,sqlCallQueryProc);
  PrintMetaData(Statement.MetaData);
  ReportResult(Statement.Execute);
  writeln(OutFile);
  writeln(OutFile,'Repeat with a different execute transaction');
  writeln(OutFile);
  Transaction2 := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  ReportResult(Statement.Execute(Transaction2));
  writeln(OutFile);
  writeln(OutFile,'Repeat with a original transaction');
  writeln(OutFile);
  ReportResult(Statement.Execute);
end;

function TTest14.TestTitle: string;
begin
  Result := 'Test 14: Non select procedures';
end;

const
  sqlCreateTable =
    'Create Table TestData( '+
    'RowID Integer not null,'+
    'Title VarChar(32),'+
    'Primary Key(RowID)'+
    ')';

  sqlCreateProc1 =
    'Create Procedure InsertData As '+
    'Begin ' +
    'Insert into TestData(RowID,Title) VALUES (1,''Testing''); '+
    'End';

  sqlCreateProc2 =
    'Create Procedure ShowData Returns (RowID Integer, Title VarChar(32)) '+
    'As Begin '+
    'Select First 1 RowID,Title From TestData Into :RowID,:Title; '+
    'End';



procedure TTest14.RunTest(CharSet: string; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  writeln(OutFile,'Default Character set Name = ',Attachment.OpenCursorAtStart('Select RDB$CHARACTER_SET_NAME From RDB$Database')[0].AsString);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateProc1);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateProc2);

  UpdateDatabase(Attachment);
  QueryDatabase(Attachment);
  Attachment.DropDatabase;
end;

initialization
  RegisterTest(TTest14);
end.

