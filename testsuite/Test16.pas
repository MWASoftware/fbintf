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

unit Test16;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

{Test 16: Error handling}

{ This test tests for correct responses to various error conditions:

  - Malformed database name.
  - Invalid User Name
  - Invalid password
  - Invalid Update SQL Statement
  - Invalid Select SQL
  - Transaction not started
  - Invalid parameter by name when should be positional
  - Invalid server name
  - invalid user name - logon to server
  - invalid password
}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type

  { TTest16 }

  TTest16 = class(TFBTestBase)
  private
    procedure DBTests(CharSet: AnsiString; SQLDialect: integer);
    procedure ServiceTests(CharSet: AnsiString; SQLDialect: integer);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest16 }

procedure TTest16.DBTests(CharSet: AnsiString; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
    Transaction: ITransaction;
    Statement: IStatement;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  try
    writeln(OutFile,'Invalid Database Name Test');
    Attachment := FirebirdAPI.OpenDatabase('localhost:Malformed Name',DPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  DPB.Find(isc_dpb_user_name).setAsString('Captain Nemo');
  try
    writeln(OutFile,'Invalid User Name Test');
    Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  DPB.Find(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Find(isc_dpb_password).setAsString('not a pwd');
  try
    writeln(OutFile,'Invalid password Test');
    Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  DPB.Find(isc_dpb_password).setAsString(Owner.GetPassword);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
  try
    writeln(OutFile,'Invalid Prepare SQL Test');
    Statement := Attachment.Prepare(Transaction,'Update Employee Set Unknown_Date = ? Where EMP_NO = ?',3);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  try
    writeln(OutFile,'Invalid Open Cursor SQL Test');
    Attachment.OpenCursorAtStart(Transaction,
           'Select X,count(*) As Counter from EMPLOYEE',3);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  Transaction.Rollback;
  try
    writeln(OutFile,'Transaction not started Test');
    Attachment.OpenCursorAtStart(Transaction,
           'Select count(*) As Counter from EMPLOYEE',3);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  Transaction.Start;
  try
    writeln(OutFile,'Invalid Param SQL Type Test');
    Statement := Attachment.Prepare(Transaction,'Update Employee Set Hire_Date = ? Where EMP_NO = ?',3);
    Statement.SQLParams.ByName('EMP_NO').AsDate := EncodeDate(2016,11,5);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  Transaction.Rollback;
  Transaction.Start;
  try
    writeln(OutFile,'Case sensitive Param SQL  Test');
    Statement := Attachment.PrepareWithNamedParameters(Transaction,'Update Employee Set Hire_Date = :Hire_Date Where emp_no = :emp_no',3,false,true);
    Statement.SQLParams.ByName('Hire_Date').AsDate := EncodeDate(2016,11,5);
    Statement.SQLParams.ByName('emp_no').AsInteger := 1;
    Statement.SQLParams.ByName('EMP_NO').AsInteger := 1;
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  Transaction.Rollback;
  Transaction.Start;
  writeln(Outfile,'Stale Reference Check');
  writeln(Outfile,'First test correct usage');
  Statement :=  Attachment.Prepare(Transaction,'Select count(*) As Counter from EMPLOYEE Where Hire_Date < ?',3);
  Statement.SQLParams[0].AsDate := EncodeDate(2016,11,5);
  ReportResults(Statement);
  try
    writeln(Outfile,'New Transaction before param set');
    Statement.Prepare(Transaction);
    Transaction.Rollback;
    Transaction.Start;
    Statement.SQLParams[0].AsDate := EncodeDate(2016,11,5);
    ReportResults(Statement);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  Transaction.Rollback;
  Transaction.Start;
  try
    writeln(Outfile,'New Transaction before Open Cursor');
    Statement.Prepare(Transaction);
    Statement.SQLParams[0].AsDate := EncodeDate(2016,11,5);
    Transaction.Rollback;
    Transaction.Start;
    ReportResults(Statement);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  Transaction.Rollback;
  Transaction.Start;
  writeln(Outfile,'Stop Stale Reference Checks');
  Statement.SetStaleReferenceChecks(false);
  try
    writeln(Outfile,'New Transaction before param set');
    Statement.Prepare(Transaction);
    Transaction.Rollback;
    Transaction.Start;
    Statement.SQLParams[0].AsDate := EncodeDate(2016,11,5);
    ReportResults(Statement);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  try
    writeln(Outfile,'New Transaction before Open Cursor');
    Statement.Prepare(Transaction);
    Statement.SQLParams[0].AsDate := EncodeDate(2016,11,5);
    Transaction.Rollback;
    Transaction.Start;
    ReportResults(Statement);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
end;

procedure TTest16.ServiceTests(CharSet: AnsiString; SQLDialect: integer);
var SPB: ISPB;
    Service: IServiceManager;
    ServerName: AnsiString;
    DBName: AnsiString;
begin
  if not FirebirdAPI.HasServiceAPI then Exit;

  DBName := ExtractDBName(Owner.GetEmployeeDatabaseName);
  ServerName := Owner.Server;

  SPB := FirebirdAPI.AllocateSPB;
  SPB.Add(isc_spb_user_name).setAsString(Owner.GetUserName);
  SPB.Add(isc_spb_password).setAsString(Owner.GetPassword);
  try
    writeln(OutFile,'Invalid Server Name Test');
    Service := FirebirdAPI.GetServiceManager('unknown',TCP,SPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;

  SPB.Find(isc_spb_user_name).setAsString('Captain Nemo');
  try
    writeln(OutFile,'Invalid User Name Test');
    Service := FirebirdAPI.GetServiceManager(ServerName,TCP,SPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  SPB.Find(isc_spb_user_name).setAsString(Owner.GetUserName);
  SPB.Find(isc_spb_password).setAsString('Bad pwd');
  try
    writeln(OutFile,'Invalid password Test');
    Service := FirebirdAPI.GetServiceManager(ServerName,TCP,SPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
end;

function TTest16.TestTitle: AnsiString;
begin
  Result := 'Test 16: Error handling';
end;

procedure TTest16.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  FirebirdAPI.GetStatus.SetIBDataBaseErrorMessages([ShowSQLCode,ShowSQLMessage,ShowIBMessage]);;
  DBTests(Charset,SQLDialect);
  ServiceTests(Charset,SQLDialect);
end;

initialization
  RegisterTest(TTest16);
end.

