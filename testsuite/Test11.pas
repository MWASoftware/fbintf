unit Test11;

{$mode objfpc}{$H+}
{$codepage utf8}

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
  Classes, SysUtils, TestManager, IB;

type

  { TTest11 }

  TTest11 = class(TTestBase)
  private
    procedure GetStatistics(Service: IServiceManager; DBName: string);
    procedure BackupRestore(Service: IServiceManager; DBName: string);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;

implementation

{ TTest11 }

procedure TTest11.GetStatistics(Service: IServiceManager; DBName: string);
var Req: ISRB;
    Results: IServiceQueryResults;
begin
  {Version Info}
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_info_svc_version);
  Req.Add(isc_info_svc_server_version);
  Req.Add(isc_info_svc_implementation);
  Results := Service.Query(nil,Req);
  WriteServiceQueryResult(Results);

  {Config Params}
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_info_svc_get_env_lock);
  Req.Add(isc_info_svc_get_config);
  Req.Add(isc_info_svc_get_env_msg);
  Req.Add(isc_info_svc_user_dbpath);
  Results := Service.Query(nil,Req);
  WriteServiceQueryResult(Results);

  {Database Information}
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_info_svc_svr_db_info);
  Results := Service.Query(nil,Req);
  WriteServiceQueryResult(Results);

  {User Information}
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_action_svc_display_user);
  Service.Start(Req);
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_info_svc_get_users);
  Results := Service.Query(nil,Req);
  WriteServiceQueryResult(Results);

  Service.Detach;
  Service.Attach;

  {Statistics}

  writeln('Get Statistics');
  writeln;
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_action_svc_db_stats);
  Req.Add(isc_spb_dbname).SetAsString(DBName);
  Req.Add(isc_spb_options).SetAsInteger(isc_spb_sts_data_pages or {
                                        isc_spb_sts_hdr_pages or} isc_spb_sts_idx_pages or
                                        isc_spb_sts_sys_relations);

  try
    Service.Start(Req);
    Req := Service.AllocateRequestBuffer;
    Req.Add(isc_info_svc_line);
    repeat
      Results := Service.Query(nil,Req);
    until not WriteServiceQueryResult(Results);
  except on E: Exception do
    writeln('Statistics Service Start: ',E.Message);
  end;
  writeln;

  {Licence Info}
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_info_svc_get_license);
  Req.Add(isc_info_svc_get_licensed_users);
  try
    Results := Service.Query(nil,Req);
    WriteServiceQueryResult(Results);
  except on E: Exception do
    writeln('Licence Info: ',E.Message);
  end;
  writeln;

  {Licence Mask Info}
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_info_svc_get_license_mask);
  try
    Results := Service.Query(nil,Req);
    WriteServiceQueryResult(Results);
  except on E: Exception do
    writeln('Licence Mask Info: ',E.Message);
  end;
  writeln;

  {Capabilities}
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_info_svc_capabilities);
  try
    Results := Service.Query(nil,Req);
    WriteServiceQueryResult(Results);
  except on E: Exception do
    writeln('Capabilities: ',E.Message);
  end;
  writeln;

  {limbo transactions}

  writeln('Get Limbo transactions');
  writeln;
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_info_svc_limbo_trans);
  try
    Results := Service.Query(nil,Req);
    WriteServiceQueryResult(Results);
  except on E: Exception do
    writeln('limbo transactions: ',E.Message);
  end;
  writeln;
end;

procedure TTest11.BackupRestore(Service: IServiceManager; DBName: string);
var Req: ISRB;
    Results: IServiceQueryResults;
    BakFile: TFileStream;
    QueryResultsItem: IServiceQueryResultItem;
    ReqLength: integer;
    SQPB: ISQPB;
    bytesWritten: integer;
    bytesAvailable: integer;
    i: integer;
    RestoreDBName: string;
    Attachment: IAttachment;
    DPB: IDPB;
begin
  {Local Backup}

  writeln('Local Backup');
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_action_svc_backup);
  Req.Add(isc_spb_dbname).AsString := DBName;
  Req.Add(isc_spb_bkp_file).AsString := 'stdout';
  try
    SelectOutputFile(Owner.GetBackupFileName);
    Service.Start(Req);
    Req := Service.AllocateRequestBuffer;
    Req.Add(isc_info_svc_to_eof);
    repeat
      Results := Service.Query(nil,Req);
    until not WriteServiceQueryResult(Results);
    writeln('Local Backup Complete');
  except on E: Exception do
    writeln('Local Backup Service: ',E.Message);
  end;
  writeln;

  {Local Restore}
  writeln('Local Restore');
  RestoreDBName := Owner.GetNewDatabaseName;
  i := Pos(':',RestoreDBName);
  if i > 0 then
    system.Delete(RestoreDBName,1,i);
  Req := Service.AllocateRequestBuffer;
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
        SQPB.Add(isc_info_svc_timeout).asInteger := 1;  {one second timeout}
        if ReqLength > 0 then
            bytesWritten := SQPB.Add(isc_info_svc_line).CopyFrom(BakFile,ReqLength);
        bytesAvailable -= bytesWritten;
        Req := Service.AllocateRequestBuffer;
        Req.Add(isc_info_svc_stdin);
        Req.Add(isc_info_svc_line);
        Results := Service.Query(SQPB,Req);
        QueryResultsItem := Results.Find(isc_info_svc_stdin);
        if QueryResultsItem <> nil then
          ReqLength := QueryResultsItem.AsInteger;
        WriteServiceQueryResult(Results);
      until (ReqLength = 0) ;
      writeln('Local Restore Complete');
    except on E: Exception do
      writeln('Local Restore Service: ',E.Message);
    end;
  finally
    BakFile.free;
  end;
  writeln;
  writeln('Open Database Check');
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).AsString := Owner.GetUserName;
  DPB.Add(isc_dpb_password).AsString := Owner.GetPassword;
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetNewDatabaseName,DPB);
  if Attachment <> nil then
    writeln('Database OK');
  Attachment.DropDatabase;
  writeln('Database Dropped');
end;

function TTest11.TestTitle: string;
begin
  Result := 'Test 11: Services API';
end;

procedure TTest11.RunTest(CharSet: string; SQLDialect: integer);
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
  Service := FirebirdAPI.GetServiceManager(ServerName,TCP,SPB);

  GetStatistics(Service,DBName);
  BackupRestore(Service,DBName);

end;

initialization
  RegisterTest(TTest11);

end.

