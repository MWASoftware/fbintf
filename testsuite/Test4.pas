unit Test4;

{$mode objfpc}{$H+}

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
    InsertCount, UpdateCount, DeleteCount: integer;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],tcRollback);
  Statement := Attachment.Prepare(Transaction,'Update Employee Set Hire_Date = ? Where EMP_NO = ?',3);
  Statement.GetSQLParams[0].AsDAteTime := EncodeDate(2016,1,31);;
  Statement.GetSQLParams[1].AsInteger := 9;
  Statement.Execute;
  Statement.GetRowsAffected(InsertCount, UpdateCount, DeleteCount);
  writeln('InsertCount = ',InsertCount,' UpdateCount = ', UpdateCount, ' DeleteCount = ',DeleteCount);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',3);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 9;
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

