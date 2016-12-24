unit Test4;

{$mode objfpc}{$H+}
{$codepage utf8}

{Test 4: Update, Insert and Delete Queries}

{ This test opens the employee example databases with the supplied user name/password
  and runs several queries:

  1. Update an employee record and report affected rows.

  2. Show Changed Record

  3. Insert new employee record and report affected rows Repeat with a duplicated
     parameter name.

  4. Show inserted record and then delete it and report affected rows

  5. Repeat insert with a null PHONE_EXT.

  6. Show inserted record and total records

  7. Prepare query again and report results

  8. Prepare query with a different transaction and report results.

  9. Open Cursor with a different transaction and report results.

  10. Implicit Rollback and disconnect.

}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest4 }

  TTest4 = class(TTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;


implementation

{ TTest4 }

procedure TTest4.DoQuery(Attachment: IAttachment);
var Transaction, Transaction2, Transaction3: ITransaction;
    Statement: IStatement;
    Rows: IResultSet;
    stats: TPerfCounters;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
  Statement := Attachment.Prepare(Transaction,'Update Employee Set Hire_Date = ? Where EMP_NO = ?',3);
  Statement.GetSQLParams[0].AsDAteTime := EncodeDate(2016,1,31);;
  Statement.GetSQLParams[1].AsInteger := 8;
  Statement.Execute;
  WriteAffectedRows(Statement);
  Transaction.Rollback;
  Transaction.Start(TARollback);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
  Statement.EnableStatistics(true);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 8;
  ReportResults(Statement);
  if Statement.GetPerfStatistics(stats) then
    WritePerfStats(stats);

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
  Statement.Execute;
  WriteAffectedRows(Statement);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
  writeln(OutFile,'Relation Name = ',Statement.Metadata.GetUniqueRelationName);
  PrintMetaData(Statement.GetMetaData);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 150;
  ReportResults(Statement);

  {Now repeat but with a non-unique parameter name}
  Statement := Attachment.PrepareWithNamedParameters(Transaction,'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, HIRE_DATE,' +
      'DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY) '+
      'VALUES (:EMP_NO, :FIRST_NAME, :FIRST_NAME, :PHONE_EXT, :HIRE_DATE,' +
      ':DEPT_NO, :JOB_CODE, :JOB_GRADE, :JOB_COUNTRY, :SALARY)',3);
  with Statement.GetSQLParams do
  begin
    ByName('EMP_NO').AsInteger := 151;
    ByName('FIRST_NAME').AsString := 'Major';
    ByName('PHONE_EXT').AsString := '';
    ByName('HIRE_DATE').AsDateTime := EncodeDate(2015,4,1);;
    ByName('DEPT_NO').AsString := '600';
    ByName('JOB_CODE').AsString := 'Eng';
    ByName('JOB_GRADE').AsInteger := 4;
    ByName('JOB_COUNTRY').AsString := 'England';
    ByName('SALARY').AsFloat := 40000.59;
  end;
  Statement.Execute;
  WriteAffectedRows(Statement);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 151;
  ReportResults(Statement);

  writeln(OutFile,'Now Delete the rows');
  Statement := Attachment.Prepare(Transaction,'Delete From Employee Where EMP_NO = ?',3);
  Statement.GetSQLParams[0].AsInteger := 150;
  Statement.Execute;
  WriteAffectedRows(Statement);
  Statement.GetSQLParams[0].AsInteger := 151;
  Statement.Execute;
  WriteAffectedRows(Statement);

  {Now again but with a null}
  Statement := Attachment.PrepareWithNamedParameters(Transaction,'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, HIRE_DATE,' +
      'DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY) '+
      'VALUES (:EMP_NO, :FIRST_NAME, :LAST_NAME, :PHONE_EXT, :HIRE_DATE,' +
      ':DEPT_NO, :JOB_CODE, :JOB_GRADE, :JOB_COUNTRY, :SALARY)',3);
  with Statement.GetSQLParams do
  begin
    ByName('EMP_NO').AsInteger := 150;
    ByName('FIRST_NAME').AsString := 'Jane';
    ByName('LAST_NAME').AsString := 'Doe';
    ByName('PHONE_EXT').Clear;
    ByName('HIRE_DATE').AsDateTime := EncodeDate(2015,4,1);;
    ByName('DEPT_NO').AsString := '600';
    ByName('JOB_CODE').AsString := 'Eng';
    ByName('JOB_GRADE').AsInteger := 4;
    ByName('JOB_COUNTRY').AsString := 'England';
    ByName('SALARY').AsFloat := 41000.89;
  end;
  writeln(OutFile,'Inserting');
  Statement.Execute;
  WriteAffectedRows(Statement);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 150;
  ReportResults(Statement);

  writeln(OutFile,'Employee Count = ', Attachment.OpenCursorAtStart(Transaction,
         'Select count(*) from EMPLOYEE',3)[0].AsInteger);

  writeln(OutFile,'Prepare Query again');
  writeln(OutFile);
  Statement.Prepare;
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 150;
  ReportResults(Statement);

  Transaction2 := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
  writeln(OutFile,'Prepare Query again with a different transaction');
  writeln(OutFile);
  Statement.Prepare(Transaction2);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 8;
  ReportResults(Statement);

  Transaction3 := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
  writeln(OutFile,'Open Cursor with a different transaction');
  writeln(OutFile);
  Rows := Statement.OpenCursor(Transaction3);
  try
    while Rows.FetchNext do
      ReportResult(Rows);
  finally
    Rows.Close;
  end;
  writeln(OutFile,'Same Statement - updated params');
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 9;
  ReportResults(Statement);
end;

function TTest4.TestTitle: string;
begin
  Result := 'Test 4: Update, Insert and Delete Queries';
end;

procedure TTest4.RunTest(CharSet: string; SQLDialect: integer);
var Attachment: IAttachment;
    DPB: IDPB;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);

  writeln(OutFile,'Opening ',Owner.GetEmployeeDatabaseName);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  writeln(OutFile,'Database Open');
  DoQuery(Attachment);
end;

initialization
  RegisterTest(TTest4);

end.

