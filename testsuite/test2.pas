unit test2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

{ TTest2 }

TTest2 = class(TTestBase)
private
  procedure DoQuery(Attachment: IAttachment);
public
  function TestTitle: string; override;
  procedure RunTest(CharSet: string; SQLDialect: integer); override;
end;


implementation

{ TTest2 }

procedure TTest2.DoQuery(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
begin
    Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],tcCommit);
    Statement := Attachment.Prepare(Transaction,'Select First 3 * from EMPLOYEE',3);
    PrintMetaData(Statement.GetMetaData);
    writeln('Plan = ' ,Statement.GetPlan);
    writeln(Statement.GetSQLText);
    ReportResults(Statement);
    Statement := Attachment.Prepare(Transaction,'Select * from EMPLOYEE Where EMP_NO = ?',3);
    writeln(Statement.GetSQLText);
    Statement.GetSQLParams[0].AsInteger := 9;
    ReportResults(Statement);
    writeln('With param names');
    Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
    writeln(Statement.GetSQLText);
    Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 9;
    ReportResults(Statement);
end;

function TTest2.TestTitle: string;
begin
  Result := 'Test 2: Open the employee database and run a query';
end;

procedure TTest2.RunTest(CharSet: string; SQLDialect: integer);
var Attachment: IAttachment;
    DPB: IDPB;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(' ');
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  DPB.Remove(isc_dpb_password);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  try
    Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  except on e: Exception do
    writeln('Create Database fails ',E.Message);
  end;
  writeln('Opening ',Owner.GetEmployeeDatabaseName);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  writeln('Database Open');
  DoQuery(Attachment);
  Attachment.Disconnect;
end;

initialization
  RegisterTest(TTest2);

end.

