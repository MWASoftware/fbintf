unit Test1;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{Create and Drop a Database}
{
 This test first attempts to create a database without specifying any parameters
 (should fail). It then goes on to create and drop a database, print out the
 parameters and then creates it again (will fail if the dropdatabase failed silently.

 Some basic database info is then accessed and printed.

 A basic query is performed and finally the database dropped.
}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$define HASREQEX}
{$ENDIF}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest1 }

  TTest1 = class(TTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
    procedure WriteAttachmentInfo(Attachment: IAttachment);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;

implementation

{ TTest1 }

procedure TTest1.DoQuery(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
    i: integer;
begin
    Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
    Statement := Attachment.Prepare(Transaction,'Select * from RDB$Database',3);
    ResultSet := Statement.OpenCursor;
    ResultSet.SetRetainInterfaces(true);
    try
      while ResultSet.FetchNext do
      begin
        for i := 0 to ResultSet.getCount - 1 do
          writeln(OutFile,ResultSet[i].Name,' = ',Trim(ResultSet[i].AsString));
      end;
    finally
      ResultSet.Close;
      ResultSet.SetRetainInterfaces(false);
    end;
end;

procedure TTest1.WriteAttachmentInfo(Attachment: IAttachment);
begin
  writeln(outfile,'DB Connect String = ',Attachment.GetConnectString);
  writeln(outfile,'DB Charset ID = ',Attachment.GetDefaultCharSetID);
  writeln(outfile,'DB SQL Dialect = ',Attachment.GetSQLDialect);
  writeln(outfile,'DB Remote Protocol = ', Attachment.GetRemoteProtocol);
  writeln(outfile,'DB ODS Major Version = ',Attachment.GetODSMajorVersion);
  writeln(outfile,'DB ODS Minor Version = ',Attachment.GetODSMinorVersion);
  writeln(outfile,'User Authentication Method = ',Attachment.GetAuthenticationMethod);
  writeln(outfile,'Firebird Library Path = ',Attachment.getFirebirdAPI.GetFBLibrary.GetLibraryFilePath);
  writeln(outfile,'DB Client Implementation Version = ',Attachment.getFirebirdAPI.GetImplementationVersion);
end;

function TTest1.TestTitle: AnsiString;
begin
  Result := 'Test 1: Create and Drop a Database';
end;

procedure TTest1.RunTest(CharSet: AnsiString; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
    createSQL: AnsiString;
    libpath: string;
    FBLibrary: IFirebirdLibrary;
begin
  writeln(OutFile,'Creating a Database with empty parameters');
  Attachment := FirebirdAPI.CreateDatabase('',nil,false);
  if Attachment = nil then
    writeln(OutFile,'Create Database fails (as expected): ',FirebirdAPI.GetStatus.GetMessage)
  else
    Attachment.DropDatabase;

  writeln(OutFile,'Creating a Database using an SQL Statement');
  createSQL := Format('create database ''%s'' USER ''%s'' PASSWORD ''%s'' DEFAULT CHARACTER SET %s',
                      [ExtractDBName(Owner.GetNewDatabaseName), Owner.GetUserName, Owner.GetPassword, CharSet]);
  Attachment := FirebirdAPI.CreateDatabase(createSQL,SQLDialect);
  WriteDBInfo(Attachment.GetDBInformation([isc_info_db_id,isc_info_db_SQL_Dialect]));
  WriteAttachmentInfo(Attachment);
  PrintDPB(Attachment.getDPB);

  {$IFDEF HASREQEX}
  {Demonstrate reconnect when database created with SQL Statement}
  try
    Attachment.Disconnect;
    Attachment.Connect;
  except on E:Exception do
    writeln(OutFile,'Error reconnecting to Database: ',E.Message);
  end;
  {$ENDIF}

  writeln(OutFile,'Dropping Database');
  if Attachment <> nil then
    Attachment.DropDatabase;

  writeln(OutFile,'Creating a Database with a DPD');
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);

  Attachment := FirebirdAPI.CreateDatabase(ExtractDBName(Owner.GetNewDatabaseName),DPB);

  WriteAttachmentInfo(Attachment);

  writeln(OutFile,'Dropping Database');
  if Attachment <> nil then
    Attachment.DropDatabase;

  {Open Database}

  PrintDPB(DPB);
  writeln(OutFile,'Creating a Database with a DPD');
  Attachment := FirebirdAPI.CreateDatabase(ExtractDBName(Owner.GetNewDatabaseName),DPB);
  if Attachment = nil then
  begin
    writeln(OutFile,'Create Database Failed');
    Exit;
  end;
  WriteDBInfo(Attachment.GetDBInformation([isc_info_db_id,isc_info_ods_version,isc_info_ods_minor_version]));
  WriteAttachmentInfo(Attachment);

  {Querying Database}
  DoQuery(Attachment);

  writeln(OutFile,'Dropping Database');
  Attachment.DropDatabase;

  libpath := GetEnvironmentVariable('TESTFIREBIRDLIBRARY');
  if libpath <> '' then
  begin
    FBLibrary := LoadFBLibrary(libpath);

    writeln(OutFile,'Creating a Database with a DPD using Firebird Library in ',libpath);
    Attachment := FBLibrary.GetFirebirdAPI.CreateDatabase(ExtractDBName(Owner.GetNewDatabaseName),DPB);
    if Attachment = nil then
    begin
      writeln(OutFile,'Create Database Failed');
      Exit;
    end;
    WriteDBInfo(Attachment.GetDBInformation([isc_info_db_id,isc_info_ods_version,isc_info_ods_minor_version]));
    WriteAttachmentInfo(Attachment);

    {Querying Database}
    DoQuery(Attachment);

    writeln(OutFile,'Dropping Database');
    Attachment.DropDatabase;
  end;
end;


initialization
  RegisterTest(TTest1);

end.

