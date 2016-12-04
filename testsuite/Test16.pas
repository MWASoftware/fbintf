unit Test16;

{$mode objfpc}{$H+}
{$codepage UTF8}

{Test 16: Error handling}

{ This test tests for correct responses to various error conditions:

  - Malformed database name.
  - Invalid User Name
  - Invalid password
  - Invalid Update SQL Statement
  - Invalid Select SQL
  - Transaction not started
  - Invalid parameter by name when should be positional
  - Invalid server name
  - invalid user name - logon to server
  - invalid password
}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest16 }

  TTest16 = class(TTestBase)
  private
    procedure DBTests(CharSet: string; SQLDialect: integer);
    procedure ServiceTests(CharSet: string; SQLDialect: integer);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;


implementation

{ TTest16 }

procedure TTest16.DBTests(CharSet: string; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
    Transaction: ITransaction;
    Statement: IStatement;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  try
    writeln(OutFile,'Invalid Database Name Test');
    Attachment := FirebirdAPI.OpenDatabase('localhost:Malformed Name',DPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  DPB.Find(isc_dpb_user_name).setAsString('Captain Nemo');
  try
    writeln(OutFile,'Invalid User Name Test');
    Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  DPB.Find(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Find(isc_dpb_password).setAsString('not a pwd');
  try
    writeln(OutFile,'Invalid password Test');
    Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  DPB.Find(isc_dpb_password).setAsString(Owner.GetPassword);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
  try
    writeln(OutFile,'Invalid Prepare SQL Test');
    Statement := Attachment.Prepare(Transaction,'Update Employee Set Unknown_Date = ? Where EMP_NO = ?',3);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  try
    writeln(OutFile,'Invalid Open Cursor SQL Test');
    Attachment.OpenCursorAtStart(Transaction,
           'Select X,count(*) As Counter from EMPLOYEE',3);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  Transaction.Rollback;
  try
    writeln(OutFile,'Transaction not started Test');
    Attachment.OpenCursorAtStart(Transaction,
           'Select count(*) As Counter from EMPLOYEE',3);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  Transaction.Start;
  try
    writeln(OutFile,'Invalid Param SQL Type Test');
    Statement := Attachment.Prepare(Transaction,'Update Employee Set Hire_Date = ? Where EMP_NO = ?',3);
    Statement.SQLParams.ByName('EMP_NO').AsDate := EncodeDate(2016,11,5);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
end;

procedure TTest16.ServiceTests(CharSet: string; SQLDialect: integer);
var SPB: ISPB;
    Service: IServiceManager;
    I: integer;
    ServerName: string;
    DBName: string;
begin
  if not FirebirdAPI.HasServiceAPI then Exit;

  ServerName := Owner.GetEmployeeDatabaseName;
  I := Pos(':',ServerName);
  if i > 0 then
    DBName := system.copy(ServerName,i+1,length(ServerName) - 2);
  system.Delete(ServerName,i,Length(ServerName)-i+1);

  SPB := FirebirdAPI.AllocateSPB;
  SPB.Add(isc_spb_user_name).setAsString(Owner.GetUserName);
  SPB.Add(isc_spb_password).setAsString(Owner.GetPassword);
  try
    writeln(OutFile,'Invalid Server Name Test');
    Service := FirebirdAPI.GetServiceManager('unknown',TCP,SPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;

  SPB.Find(isc_spb_user_name).setAsString('Captain Nemo');
  try
    writeln(OutFile,'Invalid User Name Test');
    Service := FirebirdAPI.GetServiceManager(ServerName,TCP,SPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
  SPB.Find(isc_spb_user_name).setAsString(Owner.GetUserName);
  SPB.Find(isc_spb_password).setAsString('Bad pwd');
  try
    writeln(OutFile,'Invalid password Test');
    Service := FirebirdAPI.GetServiceManager(ServerName,TCP,SPB);
  except on E: Exception do
    writeln(OutFile,'Error Handled: ',E.Message);
  end;
end;

function TTest16.TestTitle: string;
begin
  Result := 'Test 16: Error handling';
end;

procedure TTest16.RunTest(CharSet: string; SQLDialect: integer);
begin
  DBTests(Charset,SQLDialect);
  ServiceTests(Charset,SQLDialect);
end;

initialization
  RegisterTest(TTest16);
end.

