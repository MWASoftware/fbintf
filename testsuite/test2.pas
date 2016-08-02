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
  procedure ReportResults(Statement: IStatement);
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

procedure TTest2.ReportResults(Statement: IStatement);
var ResultSet: IResultSet;
    i: integer;
begin
  ResultSet := Statement.OpenCursor;
  try
    while ResultSet.FetchNext do
    begin
      for i := 0 to ResultSet.getCount - 1 do
        writeln(ResultSet[i].Name,' = ',ResultSet[i].AsString);
    end;
  finally
    ResultSet.Close;
  end;
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
  writeln('Opening ',Owner.GetEmployeeDatabaseName);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  writeln('Database Open');
  DoQuery(Attachment);
end;

initialization
  RegisterTest(TTest2);

end.

