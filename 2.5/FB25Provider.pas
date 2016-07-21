unit FB25Provider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBExternals;

type

  { TFBProvider }

  TFBProvider = class(TInterfacedObject,IFirebird)
  private
    FClientAPI: TFBClientAPI;
    procedure GenerateDPB(sl: TStrings; var DPB: string; var DPBLength: Short);
  public
    constructor Create(aClientAPI: TFBClientAPI);

    {IFirebird}
    function GetStatus: IStatus; virtual;
    function OpenDatabase(DatabaseName: string; Params: TStrings): IAttachment;
    procedure CreateDatabase(DatabaseName: string; SQLDialect: integer; Params: TStrings);
    function GetServiceManager(Service: string; Params: TStrings): IService;
    function StartTransaction(Databases: array of IAttachment; Params: TStrings): ITransaction;
  end;

implementation

uses FB25Attachment, FB25Transaction;

const
  DPBPrefix = 'isc_dpb_';
  DPBConstantNames: array[1..isc_dpb_last_dpb_constant] of string = (
    'cdd_pathname',
    'allocation',
    'journal',
    'page_size',
    'num_buffers',
    'buffer_length',
    'debug',
    'garbage_collect',
    'verify',
    'sweep',
    'enable_journal',
    'disable_journal',
    'dbkey_scope',
    'number_of_users',
    'trace',
    'no_garbage_collect',
    'damaged',
    'license',
    'sys_user_name',
    'encrypt_key',
    'activate_shadow',
    'sweep_interval',
    'delete_shadow',
    'force_write',
    'begin_log',
    'quit_log',
    'no_reserve',
    'user_name',
    'password',
    'password_enc',
    'sys_user_name_enc',
    'interp',
    'online_dump',
    'old_file_size',
    'old_num_files',
    'old_file',
    'old_start_page',
    'old_start_seqno',
    'old_start_file',
    'drop_walfile',
    'old_dump_id',
    'wal_backup_dir',
    'wal_chkptlen',
    'wal_numbufs',
    'wal_bufsize',
    'wal_grp_cmt_wait',
    'lc_messages',
    'lc_ctype',
    'cache_manager',
    'shutdown',
    'online',
    'shutdown_delay',
    'reserved',
    'overwrite',
    'sec_attach',
    'disable_wal',
    'connect_timeout',
    'dummy_packet_interval',
    'gbak_attach',
    'sql_role_name',
    'set_page_buffers',
    'working_directory',
    'sql_dialect',
    'set_db_readonly',
    'set_db_sql_dialect',
    'gfix_attach',
    'gstat_attach'
  );

{ TFBProvider }

procedure TFBProvider.GenerateDPB(sl: TStrings; var DPB: string;
  var DPBLength: Short);
var
  i, j, pval: Integer;
  DPBVal: UShort;
  ParamName, ParamValue: string;
begin
  { The DPB is initially empty, with the exception that
    the DPB version must be the first byte of the string. }
  DPBLength := 1;
  DPB := Char(isc_dpb_version1);

  {Iterate through the textual database parameters, constructing
   a DPB on-the-fly }
  for i := 0 to sl.Count - 1 do
  with ClientAPI do
  begin
    { Get the parameter's name and value from the list,
      and make sure that the name is all lowercase with
      no leading 'isc_dpb_' prefix
    }
    if (Trim(sl.Names[i]) = '') then
      continue;
    ParamName := LowerCase(sl.Names[i]); {mbcs ok}
    ParamValue := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i])); {mbcs ok}
    if (Pos(DPBPrefix, ParamName) = 1) then {mbcs ok}
      Delete(ParamName, 1, Length(DPBPrefix));
     { We want to translate the parameter name to some Integer
       value. We do this by scanning through a list of known
       database parameter names (DPBConstantNames, defined above) }
    DPBVal := 0;
    { Find the parameter }
    for j := 1 to isc_dpb_last_dpb_constant do
      if (ParamName = DPBConstantNames[j]) then
      begin
        DPBVal := j;
        break;
      end;
     {  A database parameter either contains a string value (case 1)
       or an Integer value (case 2)
       or no value at all (case 3)
       or an error needs to be generated (case else)  }
    case DPBVal of
      isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
      isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
      isc_dpb_lc_messages, isc_dpb_lc_ctype,
      isc_dpb_sql_role_name, isc_dpb_sql_dialect:
      begin
        if DPBVal = isc_dpb_sql_dialect then
          ParamValue[1] := Char(Ord(ParamValue[1]) - 48);
        DPB := DPB +
               Char(DPBVal) +
               Char(Length(ParamValue)) +
               ParamValue;
        Inc(DPBLength, 2 + Length(ParamValue));
      end;
      isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
      isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify:
      begin
        DPB := DPB +
               Char(DPBVal) +
               #1 +
               Char(StrToInt(ParamValue));
        Inc(DPBLength, 3);
      end;
      isc_dpb_sweep:
      begin
        DPB := DPB +
               Char(DPBVal) +
               #1 +
               Char(isc_dpb_records);
        Inc(DPBLength, 3);
      end;
      isc_dpb_sweep_interval:
      begin
        pval := StrToInt(ParamValue);
        DPB := DPB +
               Char(DPBVal) +
               #4 +
               PChar(@pval)[0] +
               PChar(@pval)[1] +
               PChar(@pval)[2] +
               PChar(@pval)[3];
        Inc(DPBLength, 6);
      end;
      isc_dpb_activate_shadow, isc_dpb_delete_shadow, isc_dpb_begin_log,
      isc_dpb_quit_log:
      begin
        DPB := DPB +
               Char(DPBVal) +
               #1 + #0;
        Inc(DPBLength, 3);
      end;
      else
      begin
        if (DPBVal > 0) and
           (DPBVal <= isc_dpb_last_dpb_constant) then
          IBError(ibxeDPBConstantNotSupported, [DPBConstantNames[DPBVal]])
        else
          IBError(ibxeDPBConstantUnknownEx, [sl.Names[i]]);
      end;
    end;
  end;
end;

constructor TFBProvider.Create(aClientAPI: TFBClientAPI);
begin
  inherited Create;
  FClientAPI := aClientAPI;
end;

function TFBProvider.GetStatus: IStatus;
begin
  Result := FClientAPI.Status;
end;

function TFBProvider.OpenDatabase(DatabaseName: string; Params: TStrings
  ): IAttachment;
var DPB: string;
    DPBLength: short;
    aHandle: TISC_DB_HANDLE;
begin
  Result := nil;
  GenerateDPB(Params, DPB, DPBLength);
  with FClientAPI do
  if Call(isc_attach_database(StatusVector, Length(DatabaseName),
                         PChar(DatabaseName), @aHandle,
                         DPBLength, @DPB), False) = 0 then
    Result := TFBAttachment.Create(self,aHandle);

end;

procedure TFBProvider.CreateDatabase(DatabaseName: string; SQLDialect: integer;
  Params: TStrings);
var
  tr_handle: TISC_TR_HANDLE;
  db_Handle: TISC_DB_HANDLE;
begin
  tr_handle := nil;
  db_Handle := nil;
  with FClientAPI do
  Call(
    isc_dsql_execute_immediate(StatusVector, @db_Handle, @tr_handle, 0,
                               PChar('CREATE DATABASE ''' + DatabaseName + ''' ' + {do not localize}
                               Params.Text), SQLDialect, nil));
end;

function TFBProvider.GetServiceManager(Service: string; Params: TString
  ): IService;
begin

end;

function TFBProvider.StartTransaction(Databases: array of IAttachment;
  Params: TStrings): ITransaction;
begin
  Result := TFBTransaction.Create(FClientAPI,Databases,Params);
end;

end.

