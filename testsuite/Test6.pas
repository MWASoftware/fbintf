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

unit Test6;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

{Test 6: Blob Handling}

{
  1. Create an empty database and populate with a single table and stored procedure
     returning a blob.

  2. Show the character sets available (List RDB$CHARACTER_SETS)

  3. Select all from new table and show metadata.

  4. Insert row and include WIN1252 characters known to be in two byte UTF8, plus Fixed point

  5. Select all from new table

  6. Use Update Query to set blob field with plain text loaded from file

  7. Select all from new table

  8. Add another row with a null blob

  9. Update this row's blob field with a copy of the first row (demo of blob assignment)

  10. Select all from new table.

  11. Execute Stored proc and display results

  12. Drop Database and repeat above but with WIN1252 and  no default connection character set.
}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type

  { TTest6 }

  TTest6 = class(TFBTestBase)
  private
    procedure UpdateDatabase(Attachment: IAttachment);
    procedure ExecProc(Attachment: IAttachment);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
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

  sqlCreateProc =
    'Create Procedure TestProc (RowID Integer) '+
    'Returns (BlobData Blob sub_type 1 Character Set UTF8) '+
    'As ' +
    'Begin ' +
    ' Select BlobData From TestData Where RowID = :RowID Into :BlobData; '+
    'End';


  sqlGetCharSets = 'Select RDB$CHARACTER_SET_NAME,RDB$CHARACTER_SET_ID from RDB$CHARACTER_SETS order by 2';

  sqlInsert = 'Insert into TestData(RowID,Title,FixedPoint,FloatingPoint) Values(:RowID,:Title,:FP, :DP)';

  sqlUpdate = 'Update TestData Set BlobData = ? Where RowID = ?';

  sqlExecProc = 'Execute Procedure TestProc ?';


{ TTest6 }

procedure TTest6.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement,
    Statement2: IStatement;
    ResultSet: IResultSet;
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
    ByName('Fp').Clear;
    ByName('DP').Clear;
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

procedure TTest6.ExecProc(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    Results: IResults;
begin
  writeln(OutFile,'Testing Blob as stored proc parameter');
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);

  Statement := Attachment.Prepare(Transaction,sqlExecProc);
  PrintMetaData(Statement.GetMetaData);
  Statement.SQLParams[0].AsInteger := 1;
  Results := Statement.Execute;
  ReportResult(Results);
end;

function TTest6.TestTitle: AnsiString;
begin
  Result := 'Test 6: Blob Handling';
end;

procedure TTest6.RunTest(CharSet: AnsiString; SQLDialect: integer);
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
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateProc);
  UpdateDatabase(Attachment);
  ExecProc(Attachment);

  Attachment.DropDatabase;

  {Repeat with WIN1252}
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('WIN1252');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateProc);
  UpdateDatabase(Attachment);
  ExecProc(Attachment);

  Attachment.DropDatabase;

  {Repeat with no lc_ctype}
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateProc);
  UpdateDatabase(Attachment);
  ExecProc(Attachment);

  Attachment.DropDatabase;
end;

initialization
  RegisterTest(TTest6);
end.

