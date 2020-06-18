unit FBTestApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestApplication, IB;

type

  { TFBTestBase }

  TFBTestBase = class(TTestBase)
  protected
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    function WriteServiceQueryResult(QueryResult: IServiceQueryResults): boolean;
    procedure writeLicence(Item: IServiceQueryResultItem);
    procedure WriteConfig(config: IServiceQueryResultItem);
    procedure WriteUsers(users: IServiceQueryResultItem);
    procedure WriteDBAttachments(att: IServiceQueryResultItem);
    procedure WriteLimboTransactions(limbo: IServiceQueryResultItem);
  end;

implementation

function TFBTestBase.GetTestID: AnsiString;
var s: AnsiString;
    i,j: integer;
begin
  i := 1;
  s := TestTitle;
  while not (s[i] in ['0'..'9']) and (i <= Length(s)) do inc(i);
  j := i + 1;
  while (s[j] in ['0'..'9']) and (i <= Length(s)) do inc(j);
  Result := system.copy(s,i,j-i);
  if Length(Result) = 1 then
    Result := '0' + Result;
end;

function TFBTestBase.GetTestTitle: AnsiString;
begin
  Result := '';
end;

function TFBTestBase.WriteServiceQueryResult(QueryResult: IServiceQueryResults): boolean;
var i: integer;
    line: AnsiString;
begin
  Result := true;
  for i := 0 to QueryResult.GetCount - 1 do
  with QueryResult[i] do
  case getItemType of
  isc_info_svc_version:
    writeln(OutFile,'Service Manager Version = ',getAsInteger);
  isc_info_svc_server_version:
    writeln(OutFile,'Server Version = ',getAsString);
  isc_info_svc_implementation:
    writeln(OutFile,'Implementation = ',getAsString);
  isc_info_svc_get_license:
    writeLicence(QueryResult[i]);
  isc_info_svc_get_license_mask:
    writeln(OutFile,'Licence Mask = ',getAsInteger);
  isc_info_svc_capabilities:
    writeln(OutFile,'Capabilities = ',getAsInteger);
  isc_info_svc_get_config:
    WriteConfig(QueryResult[i]);
  isc_info_svc_get_env:
    writeln(OutFile,'Root Directory = ',getAsString);
  isc_info_svc_get_env_lock:
    writeln(OutFile,'Lock Directory = ',getAsString);
  isc_info_svc_get_env_msg:
    writeln(OutFile,'Message File = ',getAsString);
  isc_info_svc_user_dbpath:
    writeln(OutFile,'Security File = ',getAsString);
  isc_info_svc_get_licensed_users:
    writeln(OutFile,'Max Licenced Users = ',getAsInteger);
  isc_info_svc_get_users:
    WriteUsers(QueryResult[i]);
  isc_info_svc_svr_db_info:
    WriteDBAttachments(QueryResult[i]);
  isc_info_svc_line:
    begin
      line := getAsString;
      writeln(OutFile,line);
      Result := line <> '';
    end;
  isc_info_svc_running:
    writeln(OutFile,'Is Running = ',getAsInteger);
  isc_info_svc_limbo_trans:
    WriteLimboTransactions(QueryResult[i]);
  isc_info_svc_to_eof,
  isc_info_svc_timeout,
  isc_info_truncated,
  isc_info_data_not_ready,
  isc_info_svc_stdin:
    {ignore};
  else
    writeln(OutFile,'Unknown Service Response Item ', getItemType);
  end;
  writeln(OutFile);
end;

procedure TFBTestBase.writeLicence(Item: IServiceQueryResultItem);
var i: integer;
begin
  for i := 0 to Item.getCount - 1 do
  with Item[i] do
  case getItemType of
    isc_spb_lic_id:
      writeln(OutFile,'Licence ID = ',GetAsString);
    isc_spb_lic_key:
      writeln(OutFile,'Licence Key = ',GetAsString);
  end;
end;

procedure TFBTestBase.WriteConfig(config: IServiceQueryResultItem);
var i: integer;
begin
  writeln(OutFile,'Firebird Configuration File');
  for i := 0 to config.getCount - 1 do
    writeln(OutFile,'Key = ',config[i].getItemType,', Value = ',config[i].getAsInteger);
  writeln(OutFile);
end;

procedure TFBTestBase.WriteUsers(users: IServiceQueryResultItem);
var i: integer;
begin
  writeln(OutFile,'Sec. Database User');
  for i := 0 to users.getCount - 1 do
  with users[i] do
  case getItemType of
    isc_spb_sec_username:
      writeln(OutFile,'User Name = ',getAsString);
    isc_spb_sec_firstname:
      writeln(OutFile,'First Name = ',getAsString);
    isc_spb_sec_middlename:
      writeln(OutFile,'Middle Name = ',getAsString);
    isc_spb_sec_lastname:
      writeln(OutFile,'Last Name = ',getAsString);
    isc_spb_sec_userid:
      writeln(OutFile,'User ID = ',getAsInteger);
    isc_spb_sec_groupid:
      writeln(OutFile,'Group ID = ',getAsInteger);
    else
      writeln(OutFile,'Unknown user info ', getItemType);
  end;
  writeln(OutFile);
end;

procedure TFBTestBase.WriteDBAttachments(att: IServiceQueryResultItem);
var i: integer;
begin
  writeln(OutFile,'DB Attachments');
  for i := 0 to att.getCount - 1 do
  with att[i] do
  case getItemType of
  isc_spb_num_att:
    writeln(OutFile,'No. of Attachments = ',getAsInteger);
  isc_spb_num_db:
    writeln(OutFile,'Databases In Use = ',getAsInteger);
  isc_spb_dbname:
    writeln(OutFile,'DB Name = ',getAsString);
  end;
end;

procedure TFBTestBase.WriteLimboTransactions(limbo: IServiceQueryResultItem);
var i: integer;
begin
  writeln(OutFile,'Limbo Transactions');
  for i := 0 to limbo.getCount - 1 do
  with limbo[i] do
  case getItemType of
  isc_spb_single_tra_id:
    writeln(OutFile,'Single DB Transaction = ',getAsInteger);
  isc_spb_multi_tra_id:
    writeln(OutFile,'Multi DB Transaction = ',getAsInteger);
  isc_spb_tra_host_site:
    writeln(OutFile,'Host Name = ',getAsString);
  isc_spb_tra_advise:
    writeln(OutFile,'Resolution Advisory = ',getAsInteger);
  isc_spb_tra_remote_site:
    writeln(OutFile,'Server Name = ',getAsString);
  isc_spb_tra_db_path:
    writeln(OutFile,'DB Primary File Name = ',getAsString);
  isc_spb_tra_state:
    begin
      write(OutFile,'State = ');
      case getAsInteger of
        isc_spb_tra_state_limbo:
          writeln(OutFile,'limbo');
        isc_spb_tra_state_commit:
          writeln(OutFile,'commit');
        isc_spb_tra_state_rollback:
          writeln(OutFile,'rollback');
        isc_spb_tra_state_unknown:
          writeln(OutFile,'Unknown');
      end;
    end;
  end;
end;

end.

