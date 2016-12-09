unit test2;

{$mode objfpc}{$H+}
{$codepage utf8}

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
    Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
    Statement := Attachment.Prepare(Transaction,'Select First 3 * from EMPLOYEE',3);
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
    Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
    Statement.SetRetainInterfaces(true);
    try
      writeln(OutFile,Statement.GetSQLText);
      ParamInfo(Statement.SQLParams);
      Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 8;
      ReportResults(Statement);
    finally
      Statement.SetRetainInterfaces(false);
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
  DPB.Find(isc_dpb_password).setAsString(Owner.GetPassword);
  try
    Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  except on e: Exception do
    writeln(OutFile,'Open Database fails ',E.Message);
  end;
  writeln(OutFile,'Opening ',Owner.GetEmployeeDatabaseName);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  writeln(OutFile,'Database Open');
  DoQuery(Attachment);
  Attachment.Disconnect;
end;

initialization
  RegisterTest(TTest2);

end.

