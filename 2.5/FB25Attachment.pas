unit FB25Attachment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBLibrary, FB25ClientAPI, IBHeader, IBExternals;

type

  { TFBAttachment }

  TFBAttachment = class(TObjectOwner,IAttachment)
  private
    FClientAPI: TFBClientAPI;
    FHandle: TISC_DB_HANDLE;
    FUserNames   : TStringList;
    FBackoutCount: TStringList;
    FDeleteCount: TStringList;
    FExpungeCount: TStringList;
    FInsertCount: TStringList;
    FPurgeCount: TStringList;
    FReadIdxCount: TStringList;
    FReadSeqCount: TStringList;
    FUpdateCount: TStringList;
    function GetLongDatabaseInfo(DatabaseInfoCommand: Integer): Long;
    function GetOperationCounts(DBInfoCommand: Integer; FOperation: TStringList): TStringList;
    procedure GenerateDPB(sl: TStrings; var DPB: string; var DPBLength: Short);
    procedure InternalDisconnect(Force: boolean);
  public
    constructor Create(ClientAPI: TFBClientAPI; DatabaseName: string; Params: TStrings);
    destructor Destroy; override;
    property Handle: TISC_DB_HANDLE read FHandle;
    property ClientAPI: TFBClientAPI read FClientAPI;

  public
    {IAttachment}
    function GetStatus: IStatus;
    procedure Disconnect(Force: boolean);
    procedure DropDatabase;
    function StartTransaction(Params: TStrings): ITransaction;
    function CreateBlob(transaction: ITransaction): IBlob;
    function OpenBlob(transaction: ITransaction; BlobID: TISC_QUAD): IBlob;
    procedure ExecImmediate(transaction: ITransaction; sql: string; SQLDialect: integer);
    function Prepare(transaction: ITransaction; sql: string; SQLDialect: integer
      ): IStatement;
    function GetEventHandler(Events: TStrings): IEvents;

    {Database Information}
    function GetBlobCharSetID(transaction: ITransaction; tableName, columnName: string): short;
    function GetAllocation: Long;
    function GetBaseLevel: Long;
    function GetDBFileName: String;
    function GetDBSiteName: String;
    function GetDBImplementationNo: Long;
    function GetDBImplementationClass: Long;
    function GetNoReserve: Long;
    function GetODSMinorVersion: Long;
    function GetODSMajorVersion: Long;
    function GetPageSize: Long;
    function GetVersion: String;
    function GetCurrentMemory: Long;
    function GetForcedWrites: Long;
    function GetMaxMemory: Long;
    function GetNumBuffers: Long;
    function GetSweepInterval: Long;
    function GetUserNames: TStringList;
    function GetFetches: Long;
    function GetMarks: Long;
    function GetReads: Long;
    function GetWrites: Long;
    function GetBackoutCount: TStringList;
    function GetDeleteCount: TStringList;
    function GetExpungeCount: TStringList;
    function GetInsertCount: TStringList;
    function GetPurgeCount: TStringList;
    function GetReadIdxCount: TStringList;
    function GetReadSeqCount: TStringList;
    function GetUpdateCount: TStringList;
    function GetReadOnly: Long;
    function GetDBSQLDialect: Long;
  end;

implementation

uses FB25Events, FBStatus, FB25Transaction, FBErrorMessages, FB25Blob,
  FB25Statement;

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

  { TFBAttachment }

function TFBAttachment.GetLongDatabaseInfo(DatabaseInfoCommand: Integer): Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  length: Integer;
  _DatabaseInfoCommand: Char;
begin
  _DatabaseInfoCommand := Char(DatabaseInfoCommand);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @_DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
    length := isc_vax_integer(@local_buffer[1], 2);
    Result := isc_vax_integer(@local_buffer[3], length);
  end;
end;

function TFBAttachment.GetOperationCounts(DBInfoCommand: Integer;
  FOperation: TStringList): TStringList;
var
  local_buffer: array[0..IBHugeLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
  i, qtd_tables, id_table, qtd_operations: Integer;
begin
  if FOperation = nil then FOperation := TStringList.Create;
  result := FOperation;
  DatabaseInfoCommand := Char(DBInfoCommand);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                           IBHugeLocalBufferLength, local_buffer), True);
    FOperation.Clear;
    { 1. 1 byte specifying the item type requested (e.g., isc_info_insert_count).
      2. 2 bytes telling how many bytes compose the subsequent value pairs.
      3. A pair of values for each table in the database on wich the requested
        type of operation has occurred since the database was last attached.
      Each pair consists of:
      1. 2 bytes specifying the table ID.
      2. 4 bytes listing the number of operations (e.g., inserts) done on that table.
    }
    qtd_tables := trunc(isc_vax_integer(@local_buffer[1],2)/6);
    for i := 0 to qtd_tables - 1 do
    begin
      id_table := isc_vax_integer(@local_buffer[3+(i*6)],2);
      qtd_operations := isc_vax_integer(@local_buffer[5+(i*6)],4);
      FOperation.Add(IntToStr(id_table)+'='+IntToStr(qtd_operations));
    end;
  end;
end;

procedure TFBAttachment.GenerateDPB(sl: TStrings; var DPB: string;
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
  with FClientAPI do
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

procedure TFBAttachment.InternalDisconnect(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  with FClientAPI do
    Call(isc_detach_database(StatusVector, @FHandle),not Force);
  FHandle := nil;
end;

constructor TFBAttachment.Create(ClientAPI: TFBClientAPI; DatabaseName: string;
  Params: TStrings);
var DPB: string;
    DPBLength: short;
begin
  inherited Create;
  FClientAPI := ClientAPI;
  ClientAPI.RegisterObj(self);
  FUserNames := TStringList.Create;
  GenerateDPB(Params, DPB, DPBLength);
  with FClientAPI do
   Call(isc_attach_database(StatusVector, Length(DatabaseName),
                         PChar(DatabaseName), @FHandle,
                         DPBLength, @DPB));
end;

destructor TFBAttachment.Destroy;
begin
  InternalDisconnect(true);
  if assigned (FUserNames) then FUserNames.Free;
  if assigned (FBackoutCount) then FBackoutCount.Free;
  if assigned (FDeleteCount) then FDeleteCount.Free;
  if assigned (FExpungeCount) then FExpungeCount.Free;
  if assigned (FInsertCount) then FInsertCount.Free;
  if assigned (FPurgeCount) then FPurgeCount.Free;
  if assigned (FReadIdxCount) then FReadIdxCount.Free;
  if assigned (FReadSeqCount) then FReadSeqCount.Free;
  if assigned (FUpdateCount) then FUpdateCount.Free;
  FClientAPI.UnRegisterObj(self);
  inherited Destroy;
end;

function TFBAttachment.GetStatus: IStatus;
begin
  Result := FClientAPI.Status;
end;

procedure TFBAttachment.Disconnect(Force: boolean);
begin
  InternalDisconnect(Force);
  Free;
end;

procedure TFBAttachment.DropDatabase;
begin
  with FClientAPI do
    Call(isc_drop_database(StatusVector, @FHandle));
  Free;
end;

function TFBAttachment.StartTransaction(Params: TStrings): ITransaction;
begin
  Result := TFBTransaction.Create(FClientAPI,self,Params);
end;

function TFBAttachment.CreateBlob(transaction: ITransaction): IBlob;
begin
  Result := TFBBLob.Create(self,transaction);
end;

function TFBAttachment.OpenBlob(transaction: ITransaction; BlobID: TISC_QUAD
  ): IBlob;
begin
  Result := TFBBLob.Create(self,transaction,BlobID);
end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: string;
  SQLDialect: integer);
var TRHandle: TISC_TR_HANDLE;
begin
  TRHandle := (Transaction as TFBTransaction).Handle;
  with FClientAPI do
    Call(isc_dsql_execute_immediate(StatusVector, @fHandle, @TRHandle, 0,PChar(sql), SQLDialect, nil));
end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: string;
  SQLDialect: integer): IStatement;
begin
  Result := TFBStatement.Create(self,transaction as TFBTransaction,sql,SQLDialect);
end;

function TFBAttachment.GetEventHandler(Events: TStrings): IEvents;
begin
  Result := TFBEvents.Create(self,Events);
end;

function TFBAttachment.GetBlobCharSetID(transaction: ITransaction; tableName,
  columnName: string): short;
var desc: TISC_BLOB_DESC;
    uGlobal: array [0..31] of char;
    trHandle: TISC_TR_HANDLE;
begin
  trHandle := (transaction as TFBTransaction).Handle;
  with FClientAPI do
    Call(isc_blob_lookup_desc(StatusVector,@FHandle,@trHandle,
                PChar(tableName),PChar(columnName),@desc,@uGlobal));
  Result := desc.blob_desc_charset;
end;

function TFBAttachment.GetAllocation: Long;
begin
  result := GetLongDatabaseInfo(isc_info_allocation);
end;

function TFBAttachment.GetBaseLevel: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_base_level);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
    result := isc_vax_integer(@local_buffer[4], 1);
  end;
end;

function TFBAttachment.GetDBFileName: String;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_id);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                           IBLocalBufferLength, local_buffer), True);
    local_buffer[5 + Int(local_buffer[4])] := #0;
    result := String(PChar(@local_buffer[5]));
  end;
end;

function TFBAttachment.GetDBSiteName: String;
var
  local_buffer: array[0..IBBigLocalBufferLength - 1] of Char;
  p: PChar;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_id);
  with FClientAPI do
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                        IBLocalBufferLength, local_buffer), True);
  p := @local_buffer[5 + Int(local_buffer[4])]; { DBSiteName Length }
  p := p + Int(p^) + 1;                         { End of DBSiteName }
  p^ := #0;                                     { Null it }
  result := String(PChar(@local_buffer[6 + Int(local_buffer[4])]));
end;

function TFBAttachment.GetDBImplementationNo: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_implementation);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                        IBLocalBufferLength, local_buffer), True);
    result := isc_vax_integer(@local_buffer[3], 1);
  end;
end;

function TFBAttachment.GetDBImplementationClass: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_implementation);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
    result := isc_vax_integer(@local_buffer[4], 1);
  end;
end;

function TFBAttachment.GetNoReserve: Long;
begin
  result := GetLongDatabaseInfo(isc_info_no_reserve);
end;

function TFBAttachment.GetODSMinorVersion: Long;
begin
  result := GetLongDatabaseInfo(isc_info_ods_minor_version);
end;

function TFBAttachment.GetODSMajorVersion: Long;
begin
  result := GetLongDatabaseInfo(isc_info_ods_version);
end;

function TFBAttachment.GetPageSize: Long;
begin
  result := GetLongDatabaseInfo(isc_info_page_size);
end;

function TFBAttachment.GetVersion: String;
var
  local_buffer: array[0..IBBigLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_version);
  with FClientAPI do
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                        IBBigLocalBufferLength, local_buffer), True);
  local_buffer[5 + Int(local_buffer[4])] := #0;
  result := String(PChar(@local_buffer[5]));
end;

function TFBAttachment.GetCurrentMemory: Long;
begin
  result := GetLongDatabaseInfo(isc_info_current_memory);
end;

function TFBAttachment.GetForcedWrites: Long;
begin
  result := GetLongDatabaseInfo(isc_info_forced_writes);
end;

function TFBAttachment.GetMaxMemory: Long;
begin
  result := GetLongDatabaseInfo(isc_info_max_memory);
end;

function TFBAttachment.GetNumBuffers: Long;
begin
  result := GetLongDatabaseInfo(isc_info_num_buffers);
end;

function TFBAttachment.GetSweepInterval: Long;
begin
  result := GetLongDatabaseInfo(isc_info_sweep_interval);
end;

function TFBAttachment.GetUserNames: TStringList;
var
  local_buffer: array[0..IBHugeLocalBufferLength - 1] of Char;
  temp_buffer: array[0..IBLocalBufferLength - 2] of Char;
  DatabaseInfoCommand: Char;
  i, user_length: Integer;
begin
  result := FUserNames;
  DatabaseInfoCommand := Char(isc_info_user_names);
  with FClientAPI do
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                        IBHugeLocalBufferLength, local_buffer), True);
  FUserNames.Clear;
  i := 0;
  while local_buffer[i] = Char(isc_info_user_names) do
  begin
    Inc(i, 3); { skip "isc_info_user_names byte" & two unknown bytes of structure (see below) }
    user_length := Long(local_buffer[i]);
    Inc(i,1);
    Move(local_buffer[i], temp_buffer[0], user_length);
    Inc(i, user_length);
    temp_buffer[user_length] := #0;
    FUserNames.Add(String(temp_buffer));
  end;
end;

function TFBAttachment.GetFetches: Long;
begin
  result := GetLongDatabaseInfo(isc_info_fetches);
end;

function TFBAttachment.GetMarks: Long;
begin
  result := GetLongDatabaseInfo(isc_info_marks);
end;

function TFBAttachment.GetReads: Long;
begin
  result := GetLongDatabaseInfo(isc_info_reads);
end;

function TFBAttachment.GetWrites: Long;
begin
  result := GetLongDatabaseInfo(isc_info_writes);
end;

function TFBAttachment.GetBackoutCount: TStringList;
begin
  result := GetOperationCounts(isc_info_backout_count,FBackoutCount);
end;

function TFBAttachment.GetDeleteCount: TStringList;
begin
  result := GetOperationCounts(isc_info_delete_count,FDeleteCount);
end;

function TFBAttachment.GetExpungeCount: TStringList;
begin
  result := GetOperationCounts(isc_info_expunge_count,FExpungeCount);
end;

function TFBAttachment.GetInsertCount: TStringList;
begin
  result := GetOperationCounts(isc_info_insert_count,FInsertCount);
end;

function TFBAttachment.GetPurgeCount: TStringList;
begin
  result := GetOperationCounts(isc_info_purge_count,FPurgeCount);
end;

function TFBAttachment.GetReadIdxCount: TStringList;
begin
  result := GetOperationCounts(isc_info_read_idx_count,FReadIdxCount);
end;

function TFBAttachment.GetReadSeqCount: TStringList;
begin
  result := GetOperationCounts(isc_info_read_seq_count,FReadSeqCount);
end;

function TFBAttachment.GetUpdateCount: TStringList;
begin
  result := GetOperationCounts(isc_info_update_count,FUpdateCount);
end;

function TFBAttachment.GetReadOnly: Long;
begin
  result := GetLongDatabaseInfo(isc_info_db_read_only);
end;

function TFBAttachment.GetDBSQLDialect: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  length: Integer;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_SQL_Dialect);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
    if (local_buffer[0] <> Char(isc_info_db_SQL_dialect)) then
      result := 1
    else begin
      length := isc_vax_integer(@local_buffer[1], 2);
      result := isc_vax_integer(@local_buffer[3], length);
    end;
  end;
end;

end.

