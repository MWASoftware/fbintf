unit Test5;

{$mode objfpc}{$H+}
{$codepage utf8}

{Test 5: Update/Insert Returning and Activity Check}

{ This test opens the employee example databases with the supplied user name/password,
  reconnects, and runs several queries:

  1. Update an employee record, return LAST_NAME,and report affected rows.

  2. Show Changed Record

  3. Insert new employee record, return FULL_NAME and report affected rows.

  4. Show inserted record

  5. Check attachment and transaction activity and ensure reset.

  6. Show total records and confirm attachment and transaction activity

  7. Implicit Rollback and disconnect.

}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest5 }

  TTest5 = class(TTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;

implementation

{ TTest5 }

procedure TTest5.DoQuery(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    Results: IResults;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
  Statement := Attachment.Prepare(Transaction,'Update Employee Set Hire_Date = ? Where EMP_NO = ? Returning LAST_NAME',3);
  Statement.GetSQLParams[0].AsDAteTime := EncodeDate(2016,1,31);;
  Statement.GetSQLParams[1].AsInteger := 9;
  Results := Statement.Execute;
  WriteAffectedRows(Statement);
  writeln('Last Name = ',Results[0].AsString);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 9;
  ReportResults(Statement);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, HIRE_DATE,' +
      'DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY) '+
      'VALUES (:EMP_NO, :FIRST_NAME, :LAST_NAME, :PHONE_EXT, :HIRE_DATE,' +
      ':DEPT_NO, :JOB_CODE, :JOB_GRADE, :JOB_COUNTRY, :SALARY) Returning FULL_NAME',3);
  Transaction.RollbackRetaining;
  with Statement.GetSQLParams do
  begin
    ByName('EMP_NO').AsInteger := 150;
    ByName('FIRST_NAME').AsString := 'John';
    ByName('LAST_NAME').AsString := 'Doe';
    ByName('PHONE_EXT').AsString := '666';
    ByName('HIRE_DATE').AsDateTime := EncodeDate(2015,4,1);;
    ByName('DEPT_NO').AsString := '600';
    ByName('JOB_CODE').AsString := 'Eng';
    ByName('JOB_GRADE').AsInteger := 4;
    ByName('JOB_COUNTRY').AsString := 'England';
    ByName('SALARY').AsFloat := 41000.89;
  end;
  writeln('Inserting');
  Results := Statement.Execute;
  writeln('Full Name = ',Results[0].AsString);
  WriteAffectedRows(Statement);
  CheckActivity(Statement.GetAttachment);
  CheckActivity(Transaction);
  CheckActivity(Statement.GetAttachment);
  CheckActivity(Transaction);

  writeln('Employee Count = ', Attachment.OpenCursorAtStart(Transaction,
         'Select count(*) from EMPLOYEE',3)[0].AsInteger);
  CheckActivity(Statement.GetAttachment);
  CheckActivity(Transaction);
  if Transaction.InTransaction then
    writeln('Transaction Active')
  else
    writeln('Transaction inactive');
  Transaction.Rollback;
  if Transaction.InTransaction then
    writeln('Transaction Active')
  else
    writeln('Transaction inactive');
end;

function TTest5.TestTitle: string;
begin
  Result := 'Test 5: Update Returning and Activity Check';
end;

procedure TTest5.RunTest(CharSet: string; SQLDialect: integer);
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
  Attachment.Disconnect;
  writeln('Database Closed');
  Attachment.Connect;
  writeln('Database Open');
  DoQuery(Attachment);
end;

initialization
  RegisterTest(TTest5);


end.

