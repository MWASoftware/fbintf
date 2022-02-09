(*
 *  Firebird UDR Support (fbudrtested). The fbudr components provide a set of
 *  Pascal language bindings for the Firebird API in support of server
 *  side User Defined Routines (UDRs). The fbudr package is an extension
 *  to the Firebird Pascal API.
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
 *  The Original Code is (C) 2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit udr_test05;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBUDRController, FBUDRIntf;

  {This unit tests the clone attachment and GetServiceManager services}

type
  {TShowMonAttachments implements a simple select procedure to return the
   selected fields from the MON$ATTACHMENTS table. If the user_name is not null then
   a new attachment is opened under that user name and that attachment is
   used to report back. Otherwise, the current attachment is cloned.

   create or alter procedure ShowAttachments (
     user_name VarChar(32))
    returns (
    MON$ATTACHMENT_ID bigint ,
    MON$ATTACHMENT_NAME varchar(255) ,
    MON$USER char(63) ,
    MON$ROLE char(63) ,
    MON$REMOTE_HOST varchar(255) ,
    )
    external name 'fbudrtests!show_att'
    engine udr;
   }

  TShowMonAttachments  = class(TFBUDRSelectProcedure)
  private
    FResults: IResultSet;
    FContext: IFBUDRExternalContext;
  public
    procedure open(context: IFBUDRExternalContext;
                     ProcMetadata: IFBUDRProcMetadata;
                     InputParams: IFBUDRInputParams); override;
    function fetch(OutputData: IFBUDROutputData): boolean; override;
    procedure close; override;
  end;

  {TServiceQuery is used to test the service manager interface and returns
   a list of service query results

   create or alter procedure GetServerInfo
   returns (ServerInfo VarChar(256))
   external name 'fbudrtests!server_info'
   engine udr;
  }

  TServiceQuery = class(TFBUDRSelectProcedure)
  private
    FResults: IServiceQueryResults;
    FIndex: integer;
    function DecodeQueryResult(i: integer): AnsiString;
    function DecodeDBAttachments(att: IServiceQueryResultItem): AnsiString;
    function WriteUsers(users: IServiceQueryResultItem): Ansistring;
  public
    procedure open(context: IFBUDRExternalContext;
                     ProcMetadata: IFBUDRProcMetadata;
                     InputParams: IFBUDRInputParams); override;
    function fetch(OutputData: IFBUDROutputData): boolean; override;
    procedure close; override;
  end;


implementation

const
  sqlSelect = 'Select * From MON$ATTACHMENTS order by MON$ATTACHMENT_ID desc';

{ TServiceQuery }

function TServiceQuery.DecodeQueryResult(i: integer): AnsiString;
begin
  with FResults[i] do
  case getItemType of
  isc_info_svc_version:
    Result := 'Service Manager Version = ' +getAsString;
  isc_info_svc_server_version:
    Result := 'Server Version = ' +getAsString;
  isc_info_svc_implementation:
    Result := 'Implementation = ' +getAsString;
  isc_info_svc_get_license_mask:
    Result := 'Licence Mask = ' +getAsString;
  isc_info_svc_capabilities:
    Result := 'Capabilities = ' +getAsString;
  isc_info_svc_get_env:
    Result := 'Root Directory = ' +getAsString;
  isc_info_svc_get_env_lock:
    Result := 'Lock Directory = ' +getAsString;
  isc_info_svc_get_env_msg:
    Result := 'Message File = ' +getAsString;
  isc_info_svc_user_dbpath:
    Result := 'Security File = ' +getAsString;
  isc_info_svc_get_licensed_users:
    Result := 'Max Licenced Users = ' +getAsString;
  isc_info_svc_get_users:
    Result := WriteUsers(FResults[i]);
  isc_info_svc_svr_db_info:
    Result := DecodeDBAttachments(FResults[i]);
  isc_info_svc_line:
    Result := getAsString;
  isc_info_svc_running:
    Result := 'Is Running = ' + getAsString;
  isc_info_svc_to_eof,
  isc_info_svc_timeout,
  isc_info_truncated,
  isc_info_data_not_ready,
  isc_info_svc_stdin:
    {ignore};
  else
    Result := 'Unknown Service Response Item ' + IntToStr(getItemType);
  end;
end;

function TServiceQuery.DecodeDBAttachments(att: IServiceQueryResultItem
  ): AnsiString;
var i: integer;
begin
  Result := 'DB Attachments: ';
  for i := 0 to att.getCount - 1 do
  with att[i] do
  case getItemType of
  isc_spb_num_att:
    Result := Result + '  No. of Attachments = ' + getAsString;
  isc_spb_num_db:
    Result := Result + 'Databases In Use = ' + getAsString;
  isc_spb_dbname:
    Result := Result + 'DB Name = ' + getAsString;
  end;
end;

function TServiceQuery.WriteUsers(users: IServiceQueryResultItem): Ansistring;
var i: integer;
begin
  Result := 'Sec. Database User';
  for i := 0 to users.getCount - 1 do
  with users[i] do
  case getItemType of
    isc_spb_sec_username:
      Result := Result + ' User Name = Result + '  + getAsString;
    isc_spb_sec_firstname:
      Result := Result + ' First Name = Result + '  + getAsString;
    isc_spb_sec_middlename:
      Result := Result + ' Middle Name = Result + '  + getAsString;
    isc_spb_sec_lastname:
      Result := Result + ' Last Name = Result + '  + getAsString;
    isc_spb_sec_userid:
      Result := Result + ' User ID = Result + '  + getAsString;
    isc_spb_sec_groupid:
      Result := Result + ' Group ID = Result + '  + getAsString;
    else
      Result := Result + ' Unknown user info ' +  IntToStr(getItemType);
  end;
end;

procedure TServiceQuery.open(context: IFBUDRExternalContext;
  ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams);
var Req: ISRB;
    ServiceManager: IServiceManager;
begin
  ServiceManager := context.GetServiceManager;
  Req := ServiceManager.AllocateSRB;
  Req.Add(isc_info_svc_version);
  Req.Add(isc_info_svc_server_version);
  Req.Add(isc_info_svc_implementation);
  Req.Add(isc_info_svc_get_env_lock);
  Req.Add(isc_info_svc_get_config);
  Req.Add(isc_info_svc_get_env_msg);
  Req.Add(isc_info_svc_user_dbpath);
  FResults := ServiceManager.Query(nil,Req);
  FIndex := 0;
end;

function TServiceQuery.fetch(OutputData: IFBUDROutputData): boolean;
begin
  Result := FIndex < FResults.Count;
  if Result then
  begin
    OutputData.ByName('ServerInfo').AsString := DecodeQueryResult(FIndex);
    Inc(FIndex);
  end;
end;

procedure TServiceQuery.close;
begin
  FResults := nil;
end;

{ TShowMonAttachments }

procedure TShowMonAttachments.open(context: IFBUDRExternalContext;
  ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams);
var att: IAttachment;
    DPB: IDPB;
    tr: ITransaction;
begin
  FContext := context;
  with context do
  begin
    if InputParams.ByName('user_name').IsNull then
    begin
      att := cloneAttachment;
      tr := att.StartTransaction([isc_tpb_read, isc_tpb_nowait, isc_tpb_read_committed],taCommit);
      FResults := att.OpenCursor(tr,sqlSelect)
    end
    else
    if InputParams.ByName('user_name').AsString = '' then
    begin
      tr := GetAttachment.StartTransaction([isc_tpb_read, isc_tpb_nowait, isc_tpb_read_committed],taCommit);
      FResults := GetAttachment.OpenCursor(tr,sqlSelect)
    end
    else
    begin
      DPB := FirebirdAPI.AllocateDPB;
      DPB.Add(isc_dpb_user_name).setAsString(InputParams.ByName('user_name').AsString);
      att := FirebirdAPI.OpenDatabase(GetDatabaseName,DPB);
      tr := att.StartTransaction([isc_tpb_read, isc_tpb_nowait, isc_tpb_read_committed],taCommit);
      FResults := att.OpenCursor(tr,sqlSelect)
    end;
  end;

end;

function TShowMonAttachments.fetch(OutputData: IFBUDROutputData): boolean;
var i: integer;
    fieldname: AnsiString;
begin
  Result := FResults.FetchNext;
  if Result then
  begin
    for i := 0 to FResults.getCount - 1 do
    begin
      fieldname := FResults[i].getName;
      if OutputData.FieldExists(fieldname) then
      begin
        if FResults[i].IsNull then
          OutputData.ByName(fieldname).clear
        else
        begin
          if fieldname = 'MON$ATTACHMENT_ID' then
            OutputData.ByName(fieldname).AsInt64 := FResults[i].AsInt64
          else
            OutputData.ByName(fieldname).AsString := Trim(FResults[i].AsString);
        end;
      end;
    end;
  end;
end;

procedure TShowMonAttachments.close;
begin
  FResults := nil;
end;

initialization
  FBRegisterUDRProcedure('show_att',TShowMonAttachments);
  FBRegisterUDRProcedure('server_info',TServiceQuery);

end.
