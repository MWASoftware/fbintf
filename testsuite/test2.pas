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
    Params: TStringList;
    ResultSet: IResultSet;
begin
  Params := TStringList.Create;
  try
    Params.Add('read');
    Params.Add('nowait');
    Params.Add('concurrency');
    Transaction := Attachment.StartTransaction(Params,tcCommit);
    Statement := Attachment.Prepare(Transaction,'Select First 3 * from EMPLOYEE',3);
    writeln(Statement.GetSQLText);
    ReportResults(Statement);
    Statement := Attachment.Prepare(Transaction,'Select * from EMPLOYEE Where EMP_NO = ?',3);
    writeln(Statement.GetSQLText);
    Statement.GetSQLParams[0].AsInteger := 9;
    ReportResults(Statement);
  finally
    Params.Free;
  end;
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
    Params: TStringList;
begin
  Params := TStringList.Create;
  try
    Params.Add('user_name='+ Owner.GetUserName);
    Params.Add('password='+ Owner.GetPassword);
    Params.Add('lc_ctype='+ CharSet);
    Params.Add('sql_dialect='+IntToStr(SQLDialect));
    writeln('Opening ',Owner.GetEmployeeDatabaseName);
    Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,Params);
    writeln('Database Open');
  finally
    Params.Free;
  end;
  DoQuery(Attachment);
end;

initialization
  RegisterTest(TTest2);

end.

