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

unit Test19;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{Test 19: Batch Update and Insert Queries}

{ This test opens the employee example databases with the supplied user name/password
  and runs several queries:

  1. Update two employee records in a single operation and report affected rows.

  2. Show Changed Records

  3. Repeat update above but add a dummy row and ignore on execute.

  4. Insert new employee records and report affected rows.

  5. Show inserted records and total records

  6. Repeat above insert but add a dumn row and ignore on execute

  7. Insert with inline blob

  8. Insert with explicit blob

  9. Implicit Rollback and disconnect.

}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type

  { TTest19 }

  TTest19 = class(TFBTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
    procedure WriteBatchCompletion(bc: IBatchCompletion);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest19 }

procedure TTest19.DoQuery(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    stats: TPerfCounters;
    ar: IArray;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
  Statement := Attachment.Prepare(Transaction,'Select * from EMPLOYEE Where EMP_NO in (8,2)',3);
  writeln(Outfile,'Rows before update');
  ReportResults(Statement);
  Statement := Attachment.Prepare(Transaction,'Update Employee Set HIRE_DATE = ? Where EMP_NO = ?',3);
  Statement.GetSQLParams[0].AsDateTime := EncodeDate(2016,1,31);;
  Statement.GetSQLParams[1].AsInteger := 8;
  Statement.Execute(eaDefer);
  Statement.GetSQLParams[0].AsDateTime := EncodeDate(2018,5,28);;
  Statement.GetSQLParams[1].AsInteger := 2;
  Statement.Execute;
  WriteAffectedRows(Statement);
  WriteBatchCompletion(Statement.GetBatchCompletion);
  Statement := Attachment.Prepare(Transaction,'Select * from EMPLOYEE Where EMP_NO in (8,2)',3);
  writeln(Outfile,'Rows after update');
  ReportResults(Statement);
  Transaction.Rollback;
  Transaction.Start(taRollback);

  writeln(Outfile);
  writeln(Outfile,'Repeat but with a last dummy row that is ignored');
  Statement := Attachment.Prepare(Transaction,'Update Employee Set HIRE_DATE = ? Where EMP_NO = ?',3);
  Statement.GetSQLParams[0].AsDateTime := EncodeDate(2016,1,31);;
  Statement.GetSQLParams[1].AsInteger := 8;
  Statement.Execute(eaDefer);
  Statement.GetSQLParams[0].AsDateTime := EncodeDate(2018,5,28);;
  Statement.GetSQLParams[1].AsInteger := 2;
  Statement.Execute(eaDefer);
  Statement.GetSQLParams[0].AsDateTime := EncodeDate(2019,5,28);;
  Statement.GetSQLParams[1].AsInteger := 2;
  Statement.Execute(eaApplyIgnoreCurrent);
  WriteAffectedRows(Statement);
  WriteBatchCompletion(Statement.GetBatchCompletion);
  Statement := Attachment.Prepare(Transaction,'Select * from EMPLOYEE Where EMP_NO in (8,2)',3);
  writeln(Outfile,'Rows after update');
  ReportResults(Statement);
  Transaction.Rollback;
  Transaction.Start(taRollback);

  writeln(Outfile);
  writeln(Outfile,'Insert rows');
  Statement := Attachment.PrepareWithNamedParameters(Transaction,'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, HIRE_DATE,' +
      'DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY) '+
      'VALUES (:EMP_NO, :FIRST_NAME, :LAST_NAME, :PHONE_EXT, :HIRE_DATE,' +
      ':DEPT_NO, :JOB_CODE, :JOB_GRADE, :JOB_COUNTRY, :SALARY)',3);
  with Statement.GetSQLParams do
  begin
    ByName('EMP_NO').AsInteger := 150;
    ByName('FIRST_NAME').AsString := 'John';
    ByName('LAST_NAME').AsString := 'Doe';
    ByName('PHONE_EXT').AsString := '';
    ByName('HIRE_DATE').AsDateTime := EncodeDate(2015,4,1);
    ByName('DEPT_NO').AsString := '600';
    ByName('JOB_CODE').AsString := 'Eng';
    ByName('JOB_GRADE').AsInteger := 4;
    ByName('JOB_COUNTRY').AsString := 'England';
    ByName('SALARY').AsFloat := 41000.89;
  end;
  Statement.Execute(eaDefer);
  with Statement.GetSQLParams do
  begin
    ByName('EMP_NO').AsInteger := 151;
    ByName('FIRST_NAME').AsString := 'Jane';
    ByName('LAST_NAME').AsString := 'Doe';
    ByName('PHONE_EXT').AsString := '';
    ByName('HIRE_DATE').AsDateTime := EncodeDate(2015,4,2);
    ByName('DEPT_NO').AsString := '600';
    ByName('JOB_CODE').AsString := 'Eng';
    ByName('JOB_GRADE').AsInteger := 4;
    ByName('JOB_COUNTRY').AsString := 'England';
    ByName('SALARY').AsFloat := 42000.89;
  end;
  Statement.Execute(eaDefer);
  with Statement.GetSQLParams do
  begin
    ByName('EMP_NO').AsInteger := 152;
    ByName('FIRST_NAME').AsString := 'John';
    ByName('LAST_NAME').AsString := 'SmithAndJonesFamily1';  //Longest Name
    ByName('PHONE_EXT').AsString := '';
    ByName('HIRE_DATE').AsDateTime := EncodeDate(2015,4,3);
    ByName('DEPT_NO').AsString := '600';
    ByName('JOB_CODE').AsString := 'Eng';
    ByName('JOB_GRADE').AsInteger := 4;
    ByName('JOB_COUNTRY').AsString := 'England';
    ByName('SALARY').AsFloat := 41000.99;
  end;
  Statement.Execute;
  WriteAffectedRows(Statement);
  WriteBatchCompletion(Statement.GetBatchCompletion);
  Statement := Attachment.Prepare(Transaction,'Select * from EMPLOYEE Where EMP_NO >= 150',3);
  writeln(Outfile,'Rows after insert');
  ReportResults(Statement);
  Transaction.Rollback;
  Transaction.Start(taRollback);
  writeln(Outfile);
  writeln(Outfile,'Insert rows - ignore last row');
  Statement := Attachment.PrepareWithNamedParameters(Transaction,'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, HIRE_DATE,' +
      'DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY) '+
      'VALUES (:EMP_NO, :FIRST_NAME, :LAST_NAME, :PHONE_EXT, :HIRE_DATE,' +
      ':DEPT_NO, :JOB_CODE, :JOB_GRADE, :JOB_COUNTRY, :SALARY)',3);
  with Statement.GetSQLParams do
  begin
    ByName('EMP_NO').AsInteger := 150;
    ByName('FIRST_NAME').AsString := 'John';
    ByName('LAST_NAME').AsString := 'Doe';
    ByName('PHONE_EXT').AsString := '';
    ByName('HIRE_DATE').AsDateTime := EncodeDate(2015,4,1);
    ByName('DEPT_NO').AsString := '600';
    ByName('JOB_CODE').AsString := 'Eng';
    ByName('JOB_GRADE').AsInteger := 4;
    ByName('JOB_COUNTRY').AsString := 'England';
    ByName('SALARY').AsFloat := 41000.89;
  end;
  Statement.Execute(eaDefer);
  with Statement.GetSQLParams do
  begin
    ByName('EMP_NO').AsInteger := 151;
    ByName('FIRST_NAME').AsString := 'Jane';
    ByName('LAST_NAME').AsString := 'Doe';
    ByName('PHONE_EXT').AsString := '';
    ByName('HIRE_DATE').AsDateTime := EncodeDate(2015,4,2);
    ByName('DEPT_NO').AsString := '600';
    ByName('JOB_CODE').AsString := 'Eng';
    ByName('JOB_GRADE').AsInteger := 4;
    ByName('JOB_COUNTRY').AsString := 'England';
    ByName('SALARY').AsFloat := 42000.89;
  end;
  Statement.Execute(eaDefer);
  with Statement.GetSQLParams do
  begin
    ByName('EMP_NO').AsInteger := 152;
    ByName('FIRST_NAME').AsString := 'John';
    ByName('LAST_NAME').AsString := 'SmithAndJonesFamily1';  //Longest Name
    ByName('PHONE_EXT').AsString := '';
    ByName('HIRE_DATE').AsDateTime := EncodeDate(2015,4,3);
    ByName('DEPT_NO').AsString := '600';
    ByName('JOB_CODE').AsString := 'Eng';
    ByName('JOB_GRADE').AsInteger := 4;
    ByName('JOB_COUNTRY').AsString := 'England';
    ByName('SALARY').AsFloat := 41000.99;
  end;
  Statement.Execute(eaApplyIgnoreCurrent);
  WriteAffectedRows(Statement);
  WriteBatchCompletion(Statement.GetBatchCompletion);
  Statement := Attachment.Prepare(Transaction,'Select * from EMPLOYEE Where EMP_NO >= 150',3);
  writeln(Outfile,'Rows after insert');
  ReportResults(Statement);
  Transaction.Rollback;
  Transaction.Start(taRollback);

  writeln(Outfile,'Insert with inline blob');
  Statement := Attachment.Prepare(Transaction,
          'INSERT INTO JOB (JOB_CODE, JOB_GRADE, JOB_COUNTRY, JOB_TITLE, MIN_SALARY,' +
                            'MAX_SALARY, JOB_REQUIREMENT, LANGUAGE_REQ) Values(?,?,?,?,?,?,?,?)');
  with Statement.GetSQLParams do
  begin
    Params[0].AsString := 'ABC';
    Params[1].AsInteger := 3;
    Params[2].AsString := 'England';
    Params[3].AsString := 'Chief Tester';
    Params[4].AsFloat := 21000;
    Params[5].AsString := '24000.99';
    Params[6].AsString := 'The quick brown fox jumped over the lazy dog';
    ar := Attachment.CreateArray(Transaction,'JOB','LANGUAGE_REQ');
    ar.SetAsString([1],'Eng');
    Params[7].AsArray := ar;
  end;
  Statement.Execute(eaDefer);;
  with Statement.GetSQLParams do
  begin
    Params[0].AsString := 'DEF';
    Params[1].AsInteger := 3;
    Params[2].AsString := 'England';
    Params[3].AsString := 'Deputy Tester';
    Params[4].AsFloat := 21000;
    Params[5].AsString := '24000.99';
    Params[6].AsString := 'The quick brown fox jumped over the running dog';
    ar := Attachment.CreateArray(Transaction,'JOB','LANGUAGE_REQ');
    ar.SetAsString([1],'Eng');
    ar.SetAsString([2],'Fra');
    Params[7].AsArray := ar;
  end;
  Statement.Execute;
  WriteAffectedRows(Statement);
  WriteBatchCompletion(Statement.GetBatchCompletion);
  Statement := Attachment.Prepare(Transaction,'Select * from JOB Where JOB_CODE in (''ABC'',''DEF'')',3);
  writeln(Outfile,'Rows after insert');
  ReportResults(Statement);
  Transaction.Rollback;
  Transaction.Start(taRollback);
  writeln(Outfile);
  writeln(Outfile,'Insert with explicit blob');
  Statement := Attachment.Prepare(Transaction,
          'INSERT INTO JOB (JOB_CODE, JOB_GRADE, JOB_COUNTRY, JOB_TITLE, MIN_SALARY,' +
                            'MAX_SALARY, JOB_REQUIREMENT, LANGUAGE_REQ) Values(?,?,?,?,?,?,?,?)');
  with Statement.GetSQLParams do
  begin
    Params[0].AsString := 'ABC';
    Params[1].AsInteger := 3;
    Params[2].AsString := 'England';
    Params[3].AsString := 'Chief Tester';
    Params[4].AsFloat := 21000;
    Params[5].AsString := '24000.99';
    Params[6].AsBlob := Attachment.CreateBlob(Transaction,'JOB','JOB_REQUIREMENT').SetString(
                 'The quick brown fox jumped over the lazy dog');
    ar := Attachment.CreateArray(Transaction,'JOB','LANGUAGE_REQ');
    ar.SetAsString([1],'Eng');
    Params[7].AsArray := ar;
  end;
  Statement.Execute(eaDefer);;
  with Statement.GetSQLParams do
  begin
    Params[0].AsString := 'DEF';
    Params[1].AsInteger := 3;
    Params[2].AsString := 'England';
    Params[3].AsString := 'Deputy Tester';
    Params[4].AsFloat := 21000;
    Params[5].AsString := '24000.99';
    Params[6].AsBlob := Attachment.CreateBlob(Transaction,'JOB','JOB_REQUIREMENT').SetString(
               'The quick brown fox jumped over the running dog');
    ar := Attachment.CreateArray(Transaction,'JOB','LANGUAGE_REQ');
    ar.SetAsString([1],'Eng');
    ar.SetAsString([2],'Fra');
    Params[7].AsArray := ar;
  end;
  Statement.Execute;
  WriteAffectedRows(Statement);
  WriteBatchCompletion(Statement.GetBatchCompletion);
  Statement := Attachment.Prepare(Transaction,'Select * from JOB Where JOB_CODE in (''ABC'',''DEF'')',3);
  writeln(Outfile,'Rows after insert');
  ReportResults(Statement);
  Transaction.Rollback;
end;

procedure TTest19.WriteBatchCompletion(bc: IBatchCompletion);
var i: integer;
begin
if bc <> nil then
  with bc do
  begin
    writeln(OutFile,'Batch Completion Info');
    writeln(OutFile,'Total rows processed = ',getTotalProcessed);
    writeln(Outfile,'Updated Records = ',getUpdated);
    for i := 0 to getTotalProcessed -1 do
      writeln(Outfile,'Row ',i+1,' State = ',getState(i),' Msg = ',getStatusMessage(i));
  end;
end;

function TTest19.TestTitle: AnsiString;
begin
  Result := 'Test 19: Batch Update and Insert Queries';
end;

procedure TTest19.RunTest(CharSet: AnsiString; SQLDialect: integer);
var Attachment: IAttachment;
    DPB: IDPB;
    S: TStrings;
    i: integer;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  DPB.Add(isc_dpb_config).SetAsString('WireCompression=true');

  writeln(OutFile,'Opening ',Owner.GetEmployeeDatabaseName);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);

  if (FirebirdAPI.GetClientMajor < 4) or (Attachment.GetODSMajorVersion < 13) then
    writeln(OutFile,'Skipping test for Firebird 4 and later')
  else
  begin
  writeln(OutFile,'Database Open');
  S := TStringList.Create;
  try
    Attachment.getFBVersion(S);
    for i := 0 to S.Count -1 do
      writeln(OutFile,S[i]);
  finally
    S.Free;
  end;
  DoQuery(Attachment);
  end;
end;

initialization
  RegisterTest(TTest19);

end.

