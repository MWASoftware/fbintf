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

unit Test2;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{Test 2: Open the employee database and run a query}

{ This test opens the employee example databases with the supplied user name/password
  and runs a simple query with no parameters. Note that the password parameter
  is first supplied empty and then updated.

  Both the output metadata and the query plan are printed out, followed by the results of the query.

  A specific employee record is then queried, first using a positional parameter
  and then a parameter by name. In each case, the SQL Parameter metadata is also
  printed followed by the query results.

  Finally, the database is explicitly disconnected.
}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type

{ TTest2 }

TTest2 = class(TFBTestBase)
private
  procedure DoQuery(Attachment: IAttachment);
  procedure DoScrollableQuery(Attachment: IAttachment);
public
  function TestTitle: AnsiString; override;
  procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
end;


implementation

{ TTest2 }

procedure TTest2.DoQuery(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
begin
    Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
    PrintTPB(Transaction.getTPB);
    Statement := Attachment.Prepare(Transaction,
    '-- SQL style inline comment' + LineEnding +
    '/* this is a comment */ '+
    'Select First 3 * from EMPLOYEE'
    ,3);
    PrintMetaData(Statement.GetMetaData);
    writeln(OutFile,'Plan = ' ,Statement.GetPlan);
    writeln(OutFile,Statement.GetSQLText);
    writeln(OutFile);
    ReportResults(Statement);
    Statement := Attachment.Prepare(Transaction,'Select * from EMPLOYEE Where EMP_NO = ?',3);
    writeln(OutFile,Statement.GetSQLText);
    ParamInfo(Statement.SQLParams);
    Statement.GetSQLParams[0].AsInteger := 8;
    ReportResults(Statement);
    writeln(OutFile,'With param names');
    Statement := Attachment.PrepareWithNamedParameters(Transaction,
    'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3,false,false,'Test Cursor');
    Statement.SetRetainInterfaces(true);
    try
      writeln(OutFile,Statement.GetSQLText);
      ParamInfo(Statement.SQLParams);
      Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 8;
      ReportResults(Statement,true);
    finally
      Statement.SetRetainInterfaces(false);
    end;
end;

procedure TTest2.DoScrollableQuery(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    Results: IResultSet;
begin
  writeln(Outfile,'Scollable Cursors');
  WriteAttachmentInfo(Attachment);
  Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from EMPLOYEE order by EMP_NO',3);
  Results := Statement.OpenCursor(true);
  writeln(Outfile,'Do Fetch Next:');
  if Results.FetchNext then
    ReportResult(Results);
  writeln(Outfile,'Do Fetch Last:');
  if Results.FetchLast then
    ReportResult(Results);
  writeln(Outfile,'Do Fetch Prior:');
  if Results.FetchPrior then
    ReportResult(Results);
  writeln(Outfile,'Do Fetch First:');
  if Results.FetchFirst then
    ReportResult(Results);
  writeln(Outfile,'Do Fetch Abs 8 :');
  if Results.FetchAbsolute(8) then
    ReportResult(Results);
  writeln(Outfile,'Do Fetch Relative -2 :');
  if Results.FetchRelative(-2) then
    ReportResult(Results);
  writeln(Outfile,'Do Fetch beyond EOF :');
  if Results.FetchAbsolute(150) then
      ReportResult(Results)
  else
    writeln(Outfile,'Fetch returned false');
end;

function TTest2.TestTitle: AnsiString;
begin
  Result := 'Test 2: Open the employee database and run a query';
end;

procedure TTest2.RunTest(CharSet: AnsiString; SQLDialect: integer);
var Attachment: IAttachment;
    DPB: IDPB;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(' ');
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  try
    Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  except on e: Exception do
    writeln(OutFile,'Open Database fails ',E.Message);
  end;
  DPB.Find(isc_dpb_password).setAsString(Owner.GetPassword);
  writeln(OutFile,'Opening ',Owner.GetEmployeeDatabaseName);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  writeln(OutFile,'Database Open, SQL Dialect = ',Attachment.GetSQLDialect);
  DoQuery(Attachment);
  if Attachment.HasScollableCursors then
  try
    DoScrollableQuery(Attachment);
  except on e: Exception do
    writeln(OutFile,'Remote Scrollable cursors test fails ',E.Message);
  end;
  Attachment.Disconnect;
  writeln(OutFile,'Now open the employee database as a local database');
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  if FirebirdAPI.GetClientMajor < 3 then
    DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  try
    Attachment := FirebirdAPI.OpenDatabase(ExtractDBName(Owner.GetEmployeeDatabaseName),DPB);
  except on e: Exception do
    writeln(OutFile,'Open Local Database fails ',E.Message);
  end;
  DoQuery(Attachment);
  if Attachment.HasScollableCursors then
    DoScrollableQuery(Attachment);
  Attachment.Disconnect;
end;

initialization
  RegisterTest(TTest2);

end.

