(*
 *  Firebird Interface (fbintf) Test suite. This program is used to
 *  test the Firebird Pascal Interface and provide a semi-automated
 *  pass/fail check for each test.
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2016 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)

unit Test14;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

{Test 14: Non select procedures}

{ this test creates a new database with a table and two stored procedures.

  1. The first stored procedure is run to populate the table

  2. The second returns data from the table, which is written out.
}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type
    { TTest14 }

  TTest14 = class(TFBTestBase)
  private
    procedure UpdateDatabase(Attachment: IAttachment);
    procedure QueryDatabase(Attachment: IAttachment);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
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

function TTest14.TestTitle: AnsiString;
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



procedure TTest14.RunTest(CharSet: AnsiString; SQLDialect: integer);
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

