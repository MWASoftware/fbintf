unit Test3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestManager, IB;

type
  { TTest3 }

  TTest3 = class(TTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;


implementation

{ TTest3 }

procedure TTest3.DoQuery(Attachment: IAttachment);
var Transaction: ITransaction;
    ResultSet: IResultSet;
    Statement: IStatement;
begin
  ResultSet := Attachment.OpenCursorAtStart(
         Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],tcCommit),
         'Select count(*) from EMPLOYEE',3);

  writeln('Employee Count = ',ResultSet[0].AsInteger);

  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],tcCommit);
  Statement := Attachment.Prepare(Transaction,'Execute Procedure DELETE_EMPLOYEE ?',3);
  Statement.GetSQLParams[0].AsInteger := 9;
  Statement.Execute;

  ResultSet := Attachment.OpenCursorAtStart(
         Transaction,
         'Select count(*) from EMPLOYEE',3);

  writeln('Employee Count = ',ResultSet[0].AsInteger);

  Transaction.Rollback;

  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],tcCommit);
  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Execute Procedure DELETE_EMPLOYEE :EMP_NO',3);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 9;
  Statement.Execute;

  ResultSet := Attachment.OpenCursorAtStart(
         Transaction,
         'Select count(*) from EMPLOYEE',3);

  writeln('Employee Count = ',ResultSet[0].AsInteger);

  Transaction.Rollback;


  ResultSet := Attachment.OpenCursorAtStart(
         Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],tcCommit),
         'Select count(*) As Counter from EMPLOYEE',3);

  writeln('Employee Count = ',ResultSet.ByName('COUNTER').AsInteger);
end;

function TTest3.TestTitle: string;
begin
  Result := 'Test 3: ad hoc queries';
end;

procedure TTest3.RunTest(CharSet: string; SQLDialect: integer);
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
  RegisterTest(TTest3);

end.
