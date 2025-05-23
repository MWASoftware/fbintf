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

unit Test11;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{Test 11: Services API}

{
  This test attaches to the Server Manager associated with the Employee Database
  and then accesses and prints out:

  Version Info
  Config Params
  Database Information
  User Information
  Statistics
  Licence Info
  Licence Mask Info
  Capabilities
  limbo transactions

  Note that two exceptions are normal as Firebird does not provide licence info.
}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type

  { TTest11 }

  TTest11 = class(TFBTestBase)
  private
    procedure GetStatistics(Service: IServiceManager; DBName: AnsiString);
    procedure BackupRestore(Service: IServiceManager; DBName: AnsiString);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;

implementation

{ TTest11 }

procedure TTest11.GetStatistics(Service: IServiceManager; DBName: AnsiString);
var Req: ISRB;
    Results: IServiceQueryResults;
begin
  {Version Info}
  Req := Service.AllocateSRB;
  Req.Add(isc_info_svc_version);
  Req.Add(isc_info_svc_server_version);
  Req.Add(isc_info_svc_implementation);
  Results := Service.Query(nil,Req);
  WriteServiceQueryResult(Results);

  {Config Params}
  Req := Service.AllocateSRB;
  Req.Add(isc_info_svc_get_env_lock);
  Req.Add(isc_info_svc_get_config);
  Req.Add(isc_info_svc_get_env_msg);
  Req.Add(isc_info_svc_user_dbpath);
  Results := Service.Query(nil,Req);
  WriteServiceQueryResult(Results);

  {Database Information}
  Req := Service.AllocateSRB;
  Req.Add(isc_info_svc_svr_db_info);
  Results := Service.Query(nil,Req);
  WriteServiceQueryResult(Results);

  {User Information}
  Req := Service.AllocateSRB;
  Req.Add(isc_action_svc_display_user);
  Service.Start(Req);
  Req := Service.AllocateSRB;
  Req.Add(isc_info_svc_get_users);
  Results := Service.Query(nil,Req);
  WriteServiceQueryResult(Results);

  Service.Detach;
  Service.Attach;

  {Statistics}

  if Owner.ShowStatistics then
  begin
    writeln(OutFile,'Get Statistics');
    writeln(OutFile);
    Req := Service.AllocateSRB;
    Req.Add(isc_action_svc_db_stats);
    Req.Add(isc_spb_dbname).SetAsString(DBName);
    Req.Add(isc_spb_options).SetAsInteger(isc_spb_sts_data_pages or {
                                          isc_spb_sts_hdr_pages or} isc_spb_sts_idx_pages or
                                          isc_spb_sts_sys_relations);

    try
      Service.Start(Req);
      Req := Service.AllocateSRB;
      Req.Add(isc_info_svc_line);
      repeat
        Results := Service.Query(nil,Req);
      until not WriteServiceQueryResult(Results);
    except on E: Exception do
      writeln(OutFile,'Statistics Service Start: ',E.Message);
    end;
    writeln(OutFile);
  end;

  {Licence Info}
  Req := Service.AllocateSRB;
  Req.Add(isc_info_svc_get_license);
  Req.Add(isc_info_svc_get_licensed_users);
  try
    Results := Service.Query(nil,Req);
    WriteServiceQueryResult(Results);
  except on E: Exception do
    writeln(OutFile,'Licence Info: ',E.Message);
  end;
  writeln(OutFile);

  {Licence Mask Info}
  Req := Service.AllocateSRB;
  Req.Add(isc_info_svc_get_license_mask);
  try
    Results := Service.Query(nil,Req);
    WriteServiceQueryResult(Results);
  except on E: Exception do
    writeln(OutFile,'Licence Mask Info: ',E.Message);
  end;
  writeln(OutFile);

  {Capabilities}
  Req := Service.AllocateSRB;
  Req.Add(isc_info_svc_capabilities);
  try
    Results := Service.Query(nil,Req);
    WriteServiceQueryResult(Results);
  except on E: Exception do
    writeln(OutFile,'Capabilities: ',E.Message);
  end;
  writeln(OutFile);

  {limbo transactions}

  writeln(OutFile,'Get Limbo transactions');
  writeln(OutFile);
  Req := Service.AllocateSRB;
  Req.Add(isc_info_svc_limbo_trans);
  try
    Results := Service.Query(nil,Req);
    WriteServiceQueryResult(Results);
  except on E: Exception do
    writeln(OutFile,'limbo transactions: ',E.Message);
  end;
  writeln(OutFile);
end;

procedure TTest11.BackupRestore(Service: IServiceManager; DBName: AnsiString);
var Req: ISRB;
    Results: IServiceQueryResults;
    BakFile: TFileStream;
    QueryResultsItem: IServiceQueryResultItem;
    ReqLength: integer;
    SQPB: ISQPB;
    bytesWritten: integer;
    bytesAvailable: integer;
    RestoreDBName: AnsiString;
    Attachment: IAttachment;
    DPB: IDPB;
begin
  {Local Backup}

  writeln(OutFile,'Local Backup');
  Req := Service.AllocateSRB;
  Req.Add(isc_action_svc_backup);
  Req.Add(isc_spb_dbname).AsString := DBName;
  Req.Add(isc_spb_bkp_file).AsString := 'stdout';
  try
    BakFile := TFileStream.Create(Owner.GetBackupFileName,fmCreate);
    try
      Service.Start(Req);
      Req := Service.AllocateSRB;
      Req.Add(isc_info_svc_to_eof);
      repeat
        bytesWritten := 0;
        Results := Service.Query(Req);
        QueryResultsItem := Results.Find(isc_info_svc_to_eof);
        if QueryResultsItem <> nil then
          bytesWritten := QueryResultsItem.CopyTo(BakFile,0);
      until ((bytesWritten = 0) and (Results.Find(isc_info_svc_timeout) = nil))
        or not WriteServiceQueryResult(Results);
      writeln(OutFile,'Local Backup Complete');
    finally
      BakFile.Free;
    end;
  except on E: Exception do
    writeln(OutFile,'Local Backup Service: ',E.Message);
  end;
  writeln(OutFile);

  {Local Restore}
  writeln(OutFile,'Local Restore');
  RestoreDBName := ExtractDBName(Owner.GetNewDatabaseName);
  Req := Service.AllocateSRB;
  Req.Add(isc_action_svc_restore);
  Req.Add(isc_spb_dbname).AsString := RestoreDBName;
  Req.Add(isc_spb_verbose);
  Req.Add(isc_spb_bkp_file).AsString := 'stdin';
  Req.Add(isc_spb_res_access_mode).AsByte := isc_spb_prp_am_readwrite;
  Req.Add(isc_spb_options).SetAsInteger(isc_spb_res_create);
  BakFile := TFileStream.Create(Owner.GetBackupFileName,fmOpenRead);
  try
    bytesAvailable := BakFile.Size;
    try
      Service.Start(Req);
      ReqLength := 0;
      repeat
        SQPB := Service.AllocateSQPB;
        if ReqLength > 0 then
            bytesWritten := SQPB.Add(isc_info_svc_line).CopyFrom(BakFile,ReqLength);
        bytesAvailable := bytesAvailable - bytesWritten;
        Req := Service.AllocateSRB;
        Req.Add(isc_info_svc_stdin);
        Req.Add(isc_info_svc_line);
        Results := Service.Query(SQPB,Req);
        QueryResultsItem := Results.Find(isc_info_svc_stdin);
        if QueryResultsItem <> nil then
          ReqLength := QueryResultsItem.AsInteger;
        WriteServiceQueryResult(Results);
      until (ReqLength = 0) ;
      writeln(OutFile,'Local Restore Complete');
    except on E: Exception do
      writeln(OutFile,'Local Restore Service: ',E.Message);
    end;
  finally
    BakFile.free;
  end;
  writeln(OutFile);
  writeln(OutFile,'Open Database Check');
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).AsString := Owner.GetUserName;
  DPB.Add(isc_dpb_password).AsString := Owner.GetPassword;
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetNewDatabaseName,DPB);
  if Attachment <> nil then
    writeln(OutFile,'Database OK');
  Attachment.DropDatabase;
  writeln(OutFile,'Database Dropped');
end;

function TTest11.TestTitle: AnsiString;
begin
  Result := 'Test 11: Services API';
end;

procedure TTest11.RunTest(CharSet: AnsiString; SQLDialect: integer);
var SPB: ISPB;
    Service: IServiceManager;
    ServerName: AnsiString;
    DBName: AnsiString;
begin
  if not FirebirdAPI.HasServiceAPI then Exit;

  DBName := ExtractDBName(Owner.GetEmployeeDatabaseName);
  ServerName := Owner.Server;

  SPB := FirebirdAPI.AllocateSPB;
  SPB.Add(isc_spb_user_name).setAsString(Owner.GetUserName);
  SPB.Add(isc_spb_password).setAsString(Owner.GetPassword);
  Service := FirebirdAPI.GetServiceManager(ServerName,TCP,SPB);
  PrintSPB(Service.getSPB);

  GetStatistics(Service,DBName);
  BackupRestore(Service,DBName);

end;

initialization
  RegisterTest(TTest11);

end.

