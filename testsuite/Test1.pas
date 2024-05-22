(*
 *  Firebird Interface (fbintf) Test suite. This program is used to
 *  test the Firebird Pascal Interface and provide a semi-automated
 *  pass/fail check for each test.
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2016 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)

unit Test1;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{Test 1: Create and Drop a Database}
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
  Classes, SysUtils, TestApplication, FBTestApp, Firebird, IB;

type

  { TTest1 }

  TTest1 = class(TFBTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
    procedure GetFBVersion(Attachment: IAttachment);
    procedure TestIsImplementationObject;
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

procedure TTest1.GetFBVersion(Attachment: IAttachment);
var Version: TStrings;
    i: integer;
begin
  Version := TStringList.Create;
  try
    Attachment.getFBVersion(Version);
    for i := 0 to Version.Count - 1 do
      writeln(OutFile,Version[i]);
  finally
    Version.Free;
  end;
end;

type
TVersionCallback = class(Firebird.IVersionCallbackImpl)
public
  procedure callback(status: Firebird.IStatus; text: PAnsiChar); override;
end;

{ TVersionCallback }

procedure TVersionCallback.callback(status : Firebird.IStatus; text : PAnsiChar
);
begin
end;

procedure TTest1.TestIsImplementationObject;
var objectType: TFirebirdImplementationClass;
    impl: TObject;
    intf: Firebird.IVersionCallback;
    TestObj:  TVersionCallback;
begin
  if not FirebirdAPI.HasMasterIntf then Exit; {Only if OOAPI available}
  writeln(OutFile,'Test Firebird Helper function "IsImplementationObject"');
  TestObj := TVersionCallback.Create;
  try
    intf := TestObj.asIVersionCallback;
    if Firebird.IsImplementationObject(intf,objectType,impl) then
      writeln(OutFile,'Success: implementation class name is ',objectType.ClassName,', returned object is a ',impl.ClassName)
    else
      writeln(OutFile,'IsImplementationObject failed');
  finally
    TestObj.Free;
  end;
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
    writeln(OutFile,'Create Database fails (as expected): ',FirebirdAPI.GetStatus.GetMessage(cp_acp))
  else
    Attachment.DropDatabase;

  writeln(OutFile,'Creating a Database using an SQL Statement');
  createSQL := Format('create database ''%s'' USER ''%s'' PASSWORD ''%s'' DEFAULT CHARACTER SET %s',
                      [Owner.GetNewDatabaseName, Owner.GetUserName, Owner.GetPassword, CharSet]);
  Attachment := FirebirdAPI.CreateDatabase(createSQL,SQLDialect);
  WriteDBInfo(Attachment.GetDBInformation([isc_info_db_id,isc_info_db_SQL_Dialect]));
  WriteAttachmentInfo(Attachment);
  PrintDPB(Attachment.getDPB);
  writeln(OutFile,'Firebird Server Version Info');
  GetFBVersion(Attachment);
  writeln(OutFile);

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

  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);

  WriteAttachmentInfo(Attachment);

  writeln(OutFile,'Dropping Database');
  if Attachment <> nil then
    Attachment.DropDatabase;

  {Open Database}

  PrintDPB(DPB);
  writeln(OutFile,'Creating a Database with a DPD');
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  if Attachment = nil then
  begin
    writeln(OutFile,'Create Database Failed');
    Exit;
  end;
  WriteDBInfo(Attachment.GetDBInformation([isc_info_db_id,isc_info_ods_version,isc_info_ods_minor_version]));
  writeln(OutFile,'Attachment ID = ',Attachment.GetAttachmentID);
  WriteAttachmentInfo(Attachment);

  {Querying Database}
  DoQuery(Attachment);

  TestIsImplementationObject;

  writeln(OutFile,'Dropping Database');
  Attachment.DropDatabase;

  libpath := GetEnvironmentVariable('TESTFIREBIRDLIBRARY');
  if libpath <> '' then
  begin
    FBLibrary := LoadFBLibrary(libpath);

    writeln(OutFile,'Creating a Database with a DPD using Firebird Library in ',libpath);
    Attachment := FBLibrary.GetFirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
    if Attachment = nil then
    begin
      writeln(OutFile,'Create Database Failed');
      Exit;
    end;
    WriteDBInfo(Attachment.GetDBInformation([isc_info_db_id,isc_info_ods_version,isc_info_ods_minor_version]));
    writeln(OutFile,'Attachment ID = ',Attachment.GetAttachmentID);
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

