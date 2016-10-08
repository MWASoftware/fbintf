unit Test4;

{$mode objfpc}{$H+}
{$codepage utf8}

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
var Transaction: ITransaction;
    Statement: IStatement;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
  Statement := Attachment.Prepare(Transaction,'Update Employee Set Hire_Date = ? Where EMP_NO = ?',3);
  Statement.GetSQLParams[0].AsDAteTime := EncodeDate(2016,1,31);;
  Statement.GetSQLParams[1].AsInteger := 9;
  Statement.Execute;
  WriteAffectedRows(Statement);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 9;
  ReportResults(Statement);

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
    ByName('HIRE_DATE').AsDateTime := EncodeDate(2015,4,1);;
    ByName('DEPT_NO').AsString := '600';
    ByName('JOB_CODE').AsString := 'Eng';
    ByName('JOB_GRADE').AsInteger := 4;
    ByName('JOB_COUNTRY').AsString := 'England';
    ByName('SALARY').AsFloat := 41000.89;
  end;
  Statement.Execute;
  WriteAffectedRows(Statement);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 150;
  ReportResults(Statement);

  writeln('Now Delete the row');
  Statement := Attachment.Prepare(Transaction,'Delete From Employee Where EMP_NO = ?',3);
  Statement.GetSQLParams[0].AsInteger := 150;
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
  writeln('Inserting');
  Statement.Execute;
  WriteAffectedRows(Statement);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 150;
  ReportResults(Statement);

  writeln('Employee Count = ', Attachment.OpenCursorAtStart(Transaction,
         'Select count(*) from EMPLOYEE',3)[0].AsInteger);

end;

function TTest4.TestTitle: string;
begin
  Result := 'Test 4: Update Queries';
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

  writeln('Opening ',Owner.GetEmployeeDatabaseName);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  writeln('Database Open');
  DoQuery(Attachment);
end;

initialization
  RegisterTest(TTest4);

end.

