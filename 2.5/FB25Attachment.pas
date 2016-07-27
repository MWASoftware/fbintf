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
    FDatabaseName: string;
    FDPB: string;
    FDPBLength: short;
    procedure GenerateDPB(sl: TStrings; var DPB: string; var DPBLength: Short);
  public
    constructor Create(ClientAPI: TFBClientAPI; DatabaseName: string; Params: TStrings);
    destructor Destroy; override;
    property Handle: TISC_DB_HANDLE read FHandle;
    property ClientAPI: TFBClientAPI read FClientAPI;

  public
    {IAttachment}
    function GetStatus: IStatus;
    procedure Connect;
    procedure Disconnect(Force: boolean);
    procedure DropDatabase;
    function StartTransaction(Params: TStrings): ITransaction;
    function CreateBlob(transaction: ITransaction): IBlob;
    function OpenBlob(transaction: ITransaction; BlobID: TISC_QUAD): IBlob;
    procedure ExecImmediate(transaction: ITransaction; sql: string; SQLDialect: integer);
    function Prepare(transaction: ITransaction; sql: string; SQLDialect: integer
      ): IStatement;
    function GetEventHandler(Events: TStrings): IEvents;
    procedure Release;

    {Database Information}
    function GetBlobCharSetID(transaction: ITransaction; tableName, columnName: string): short;
    function GetInfoValue(DBInfoCommand: char): integer;
    function GetInfoString(DBInfoCommand: char): string;
    function GetOperationCounts(DBInfoCommand: char; aOperation: TStringList): TStringList;
    function GetInfoBuffer(DBInfoCommand: char; var p: PChar): integer;
  end;

implementation

uses FB25Events, FBStatus, FB25Transaction, FBErrorMessages, FB25Blob,
  FB25Statement, FB25ResultBuffer;

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

function TFBAttachment.GetOperationCounts(DBInfoCommand: char;
  aOperation: TStringList): TStringList;
var
  i, qtd_tables, id_table, qtd_operations: Integer;
  p: PChar;
begin
  if aOperation = nil then aOperation := TStringList.Create;
  aOperation.Clear;
  Result := aOperation;

  with TDBInfoResultBuffer.Create(self,DBInfoCommand,IBHugeLocalBufferLength), FClientAPI do
  try
    p := buffer;
    { 1. 1 byte specifying the item type requested (e.g., isc_info_insert_count).
      2. 2 bytes telling how many bytes compose the subsequent value pairs.
      3. A pair of values for each table in the database on wich the requested
        type of operation has occurred since the database was last attached.
      Each pair consists of:
      1. 2 bytes specifying the table ID.
      2. 4 bytes listing the number of operations (e.g., inserts) done on that table.
    }
    qtd_tables := trunc(isc_vax_integer(p+1,2)/6);
    for i := 0 to qtd_tables - 1 do
    begin
      id_table := isc_vax_integer(p+3+(i*6),2);
      qtd_operations := isc_vax_integer(p+5+(i*6),4);
      aOperation.Add(IntToStr(id_table)+'='+IntToStr(qtd_operations));
    end;
  finally
    Free;
  end;
end;

function TFBAttachment.GetInfoBuffer(DBInfoCommand: char; var p: PChar
  ): integer;
begin
  with TDBInfoResultBuffer.Create(self,DBInfoCommand,IBHugeLocalBufferLength) do
  try
    Result := GetInfoBuffer(p);
  finally
    Free
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
               #4 + EncodeLsbf(pval,4);
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

constructor TFBAttachment.Create(ClientAPI: TFBClientAPI; DatabaseName: string;
  Params: TStrings);
begin
  inherited Create;
  FClientAPI := ClientAPI;
  ClientAPI.RegisterObj(self);
  FDatabaseName := DatabaseName;
  GenerateDPB(Params, FDPB, FDPBLength);
  Connect;
end;

destructor TFBAttachment.Destroy;
begin
  Disconnect(true);
  FClientAPI.UnRegisterObj(self);
  inherited Destroy;
end;

function TFBAttachment.GetStatus: IStatus;
begin
  Result := FClientAPI.Status;
end;

procedure TFBAttachment.Connect;
begin
  with FClientAPI do
   Call(isc_attach_database(StatusVector, Length(FDatabaseName),
                         PChar(FDatabaseName), @FHandle,
                         FDPBLength, @FDPB));
end;

procedure TFBAttachment.Disconnect(Force: boolean);
var i: integer;
begin
  if FHandle = nil then
    Exit;

  {Rollback or Cancel dependent objects}
  for i := 0 to OwnedObjects.Count - 1 do
    if (TObject(OwnedObjects[i]) is TFBTransaction) then
          TFBTransaction(OwnedObjects[i]).Rollback
    else
    if TObject(OwnedObjects[i]) is TFBEvents then
      TFBEvents(OwnedObjects[i]).Cancel;

  {Disconnect}
  with FClientAPI do
    Call(isc_detach_database(StatusVector, @FHandle),not Force);
  FHandle := nil;
end;

procedure TFBAttachment.DropDatabase;
begin
  with FClientAPI do
    Call(isc_drop_database(StatusVector, @FHandle));
  Free;
end;

function TFBAttachment.StartTransaction(Params: TStrings): ITransaction;
begin
  Result := TFBTransaction.Create(self,Params);
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

procedure TFBAttachment.Release;
begin
  if OwnedObjects.Count > 0 then
    IBError(ibxAttachementReleaseFails,[OwnedObjects.Count]);
  Free;
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

function TFBAttachment.GetInfoValue(DBInfoCommand: char): integer;
begin
  with TDBInfoResultBuffer.Create(self, DBInfoCommand) do
  try
    Result := GetValue;
  finally
    Free
  end;
end;

function TFBAttachment.GetInfoString(DBInfoCommand: char): string;
begin
  with TDBInfoResultBuffer.Create(self, DBInfoCommand) do
  try
    GetString(Result);
  finally
    Free
  end;
end;

end.

