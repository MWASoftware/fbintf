unit FB25ClientAPI;

{$mode delphi}

interface

uses
  Classes, SysUtils, FBLibrary, IBHeader, IBExternals,FB25Status, IB;

type

  { TFB25ClientAPI }

  TFB25ClientAPI = class(TFBLibrary,IFirebirdAPI)
  private
    FIBServiceAPIPresent: boolean;
    FStatus: TFBStatus;
    FStatusIntf: IStatus;   {Keep a reference to the interface - automatic destroy
                             when this class is freed and last reference to IStatus
                             goes out of scope.}
    procedure LoadInterface;
  public
    constructor Create;
    destructor Destroy; override;
    function StatusVector: PISC_STATUS;
    procedure IBDataBaseError;
    procedure EncodeLsbf(aValue: integer; len: integer; var buffer: PChar); overload;
    function EncodeLsbf(aValue: integer; len: integer): string; overload;
    property IBServiceAPIPresent: boolean read FIBServiceAPIPresent;
    property Status: TFBStatus read FStatus;

  public

    {fbclient API}
    BLOB_get: TBLOB_get;
    BLOB_put: TBLOB_put;
    isc_sqlcode: Tisc_sqlcode;
    isc_sql_interprete: Tisc_sql_interprete;
    isc_interprete: Tisc_interprete;
    isc_vax_integer: Tisc_vax_integer;
    isc_portable_integer: Tisc_portable_integer;
    isc_blob_info: Tisc_blob_info;
    isc_blob_lookup_desc: Tisc_blob_lookup_desc;
    isc_open_blob2: Tisc_open_blob2;
    isc_close_blob: Tisc_close_blob;
    isc_get_segment: Tisc_get_segment;
    isc_put_segment: Tisc_put_segment;
    isc_create_blob2: Tisc_create_blob2;
    isc_cancel_blob: Tisc_cancel_blob;
    isc_service_attach: Tisc_service_attach;
    isc_service_detach: Tisc_service_detach;
    isc_service_query: Tisc_service_query;
    isc_service_start: Tisc_service_start;
    isc_decode_date: Tisc_decode_date;
    isc_decode_sql_date: Tisc_decode_sql_date;
    isc_decode_sql_time: Tisc_decode_sql_time;
    isc_decode_timestamp: Tisc_decode_timestamp;
    isc_encode_date: Tisc_encode_date;
    isc_encode_sql_date: Tisc_encode_sql_date;
    isc_encode_sql_time: Tisc_encode_sql_time;
    isc_encode_timestamp: Tisc_encode_timestamp;
    isc_dsql_free_statement: Tisc_dsql_free_statement;
    isc_dsql_execute2: Tisc_dsql_execute2;
    isc_dsql_execute: Tisc_dsql_execute;
    isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
    isc_dsql_fetch: Tisc_dsql_fetch;
    isc_dsql_sql_info: Tisc_dsql_sql_info;
    isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
    isc_dsql_prepare: Tisc_dsql_prepare;
    isc_dsql_describe_bind: Tisc_dsql_describe_bind;
    isc_dsql_describe: Tisc_dsql_describe;
    isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
    isc_drop_database: Tisc_drop_database;
    isc_detach_database: Tisc_detach_database;
    isc_attach_database: Tisc_attach_database;
    isc_database_info: Tisc_database_info;
    isc_start_transaction: Tisc_start_transaction;
    isc_start_multiple: Tisc_start_multiple;
    isc_commit_transaction: Tisc_commit_transaction;
    isc_commit_retaining: Tisc_commit_retaining;
    isc_rollback_transaction: Tisc_rollback_transaction;
    isc_rollback_retaining: Tisc_rollback_retaining;
    isc_cancel_events: Tisc_cancel_events;
    isc_que_events: Tisc_que_events;
    isc_event_counts: Tisc_event_counts;
    isc_event_block: Tisc_event_block;
    isc_wait_for_event: Tisc_wait_for_event;
    isc_free: Tisc_free;
    isc_add_user   : Tisc_add_user;
    isc_delete_user: Tisc_delete_user;
    isc_modify_user: Tisc_modify_user;

  public
    {IFirebirdAPI}
    function GetStatus: IStatus;
    function OpenDatabase(DatabaseName: string; Params: TStrings): IAttachment;
    procedure CreateDatabase(DatabaseName: string; SQLDialect: integer;
                                          Params: TStrings);
    function GetServiceManager(ServerName: string; Protocol: TProtocol; Params: TStrings): IServiceManager;
    {Start Transaction against multiple databases}
    function StartTransaction(Attachments: array of IAttachment; Params: TStrings;
      DefaultCompletion: TTransactionCompletion): ITransaction;
    function IsEmbeddedServer: boolean;
    function GetLibraryName: string;
    function HasServiceAPI: boolean;
  end;

const
  Firebird25ClientAPI: TFB25ClientAPI = nil;

implementation

uses FBErrorMessages, dynlibs, FB25Attachment, FB25Transaction, FB25Services;

{ Stubs for 6.0 only functions }
function isc_rollback_retaining_stub(status_vector   : PISC_STATUS;
              tran_handle     : PISC_TR_HANDLE):
                                     ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := 0;
  IBError(ibxeIB60feature, ['isc_rollback_retaining']); {do not localize}
end;

function isc_service_attach_stub(status_vector      : PISC_STATUS;
                                 isc_arg2           : UShort;
                                 isc_arg3           : PChar;
                                 service_handle     : PISC_SVC_HANDLE;
                                 isc_arg5           : UShort;
                                 isc_arg6           : PChar):
                                 ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := 0;
  IBError(ibxeIB60feature, ['isc_service_attach']); {do not localize}
end;

function isc_service_detach_stub(status_vector      : PISC_STATUS;
                                 service_handle     : PISC_SVC_HANDLE):
                                 ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := 0;
  IBError(ibxeIB60feature, ['isc_service_detach']); {do not localize}
end;

function isc_service_query_stub(status_vector        : PISC_STATUS;
                                service_handle       : PISC_SVC_HANDLE;
                                recv_handle          : PISC_SVC_HANDLE;
                                isc_arg4             : UShort;
                                isc_arg5             : PChar;
                                isc_arg6             : UShort;
                                isc_arg7             : PChar;
                                isc_arg8             : UShort;
                                isc_arg9             : PChar):
                                ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := 0;
  IBError(ibxeIB60feature, ['isc_service_query']); {do not localize}
end;

function isc_service_start_stub(status_vector        : PISC_STATUS;
                                service_handle       : PISC_SVC_HANDLE;
                                recv_handle          : PISC_SVC_HANDLE;
                                isc_arg4             : UShort;
                                isc_arg5             : PChar):
                                ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := 0;
  IBError(ibxeIB60feature, ['isc_service_start']); {do not localize}
end;

procedure isc_encode_sql_date_stub(tm_date           : PCTimeStructure;
                 ib_date           : PISC_DATE);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_encode_sql_date']); {do not localize}
end;

procedure isc_encode_sql_time_stub(tm_date           : PCTimeStructure;
                   ib_time           : PISC_TIME);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_encode_sql_time']); {do not localize}
end;

procedure isc_encode_timestamp_stub(tm_date          : PCTimeStructure;
                  ib_timestamp     : PISC_TIMESTAMP);
                                    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_encode_sql_timestamp']); {do not localize}
end;

procedure isc_decode_sql_date_stub(ib_date           : PISC_DATE;
                                   tm_date           : PCTimeStructure);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_decode_sql_date']); {do not localize}
end;

procedure isc_decode_sql_time_stub(ib_time           : PISC_TIME;
                                   tm_date           : PCTimeStructure);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_decode_sql_time']); {do not localize}
end;

procedure isc_decode_timestamp_stub(ib_timestamp     : PISC_TIMESTAMP;
                                    tm_date          : PCTimeStructure);
                                    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_decode_timestamp']); {do not localize}
end;


{ TFB25ClientAPI }

procedure TFB25ClientAPI.LoadInterface;
begin
  BLOB_get := GetProcAddr('BLOB_get'); {do not localize}
  BLOB_put := GetProcAddr('BLOB_put'); {do not localize}
  isc_sqlcode := GetProcAddr('isc_sqlcode'); {do not localize}
  isc_sql_interprete := GetProcAddr('isc_sql_interprete'); {do not localize}
  isc_interprete := GetProcAddr('isc_interprete'); {do not localize}
  isc_vax_integer := GetProcAddr('isc_vax_integer'); {do not localize}
  isc_portable_integer := GetProcAddr('isc_portable_integer'); {do not localize}
  isc_blob_info := GetProcAddr('isc_blob_info'); {do not localize}
  isc_blob_lookup_desc := GetProcAddr('isc_blob_lookup_desc');  {do not localize}
  isc_open_blob2 := GetProcAddr('isc_open_blob2'); {do not localize}
  isc_close_blob := GetProcAddr('isc_close_blob'); {do not localize}
  isc_get_segment := GetProcAddr('isc_get_segment'); {do not localize}
  isc_put_segment := GetProcAddr('isc_put_segment'); {do not localize}
  isc_create_blob2 := GetProcAddr('isc_create_blob2'); {do not localize}
  isc_cancel_blob :=  GetProcAddr('isc_cancel_blob'); {do not localize}
  isc_decode_date := GetProcAddr('isc_decode_date'); {do not localize}
  isc_encode_date := GetProcAddr('isc_encode_date'); {do not localize}
  isc_dsql_free_statement := GetProcAddr('isc_dsql_free_statement'); {do not localize}
  isc_dsql_execute2 := GetProcAddr('isc_dsql_execute2'); {do not localize}
  isc_dsql_execute := GetProcAddr('isc_dsql_execute'); {do not localize}
  isc_dsql_set_cursor_name := GetProcAddr('isc_dsql_set_cursor_name'); {do not localize}
  isc_dsql_fetch := GetProcAddr('isc_dsql_fetch'); {do not localize}
  isc_dsql_sql_info := GetProcAddr('isc_dsql_sql_info'); {do not localize}
  isc_dsql_alloc_statement2 := GetProcAddr('isc_dsql_alloc_statement2'); {do not localize}
  isc_dsql_prepare := GetProcAddr('isc_dsql_prepare'); {do not localize}
  isc_dsql_describe_bind := GetProcAddr('isc_dsql_describe_bind'); {do not localize}
  isc_dsql_describe := GetProcAddr('isc_dsql_describe'); {do not localize}
  isc_dsql_execute_immediate := GetProcAddr('isc_dsql_execute_immediate'); {do not localize}
  isc_drop_database := GetProcAddr('isc_drop_database'); {do not localize}
  isc_detach_database := GetProcAddr('isc_detach_database'); {do not localize}
  isc_attach_database := GetProcAddr('isc_attach_database'); {do not localize}
  isc_database_info := GetProcAddr('isc_database_info'); {do not localize}
  isc_start_transaction := GetProcAddr('isc_start_transaction'); {do not localize}
  isc_start_multiple := GetProcAddr('isc_start_multiple'); {do not localize}
  isc_commit_transaction := GetProcAddr('isc_commit_transaction'); {do not localize}
  isc_commit_retaining := GetProcAddr('isc_commit_retaining'); {do not localize}
  isc_rollback_transaction := GetProcAddr('isc_rollback_transaction'); {do not localize}
  isc_cancel_events := GetProcAddr('isc_cancel_events'); {do not localize}
  isc_que_events := GetProcAddr('isc_que_events'); {do not localize}
  isc_event_counts := GetProcAddr('isc_event_counts'); {do not localize}
  isc_event_block := GetProcAddr('isc_event_block'); {do not localize}
  isc_wait_for_event := GetProcAddr('isc_wait_for_event'); {do not localize}
  isc_free := GetProcAddr('isc_free'); {do not localize}
  isc_add_user := GetProcAddr('isc_add_user'); {do not localize}
  isc_delete_user := GetProcAddr('isc_delete_user'); {do not localize}
  isc_modify_user := GetProcAddr('isc_modify_user'); {do not localize}

  FIBServiceAPIPresent := true;
  isc_rollback_retaining := GetProcAddress(IBLibrary, 'isc_rollback_retaining'); {do not localize}
  if Assigned(isc_rollback_retaining) then
  begin
    isc_service_attach := GetProcAddr('isc_service_attach'); {do not localize}
    isc_service_detach := GetProcAddr('isc_service_detach'); {do not localize}
    isc_service_query := GetProcAddr('isc_service_query'); {do not localize}
    isc_service_start := GetProcAddr('isc_service_start'); {do not localize}
    isc_decode_sql_date := GetProcAddr('isc_decode_sql_date'); {do not localize}
    isc_decode_sql_time := GetProcAddr('isc_decode_sql_time'); {do not localize}
    isc_decode_timestamp := GetProcAddr('isc_decode_timestamp'); {do not localize}
    isc_encode_sql_date := GetProcAddr('isc_encode_sql_date'); {do not localize}
    isc_encode_sql_time := GetProcAddr('isc_encode_sql_time'); {do not localize}
    isc_encode_timestamp := GetProcAddr('isc_encode_timestamp'); {do not localize}
  end else
  begin
    FIBServiceAPIPresent := false;
    isc_rollback_retaining := @isc_rollback_retaining_stub;
    isc_service_attach := @isc_service_attach_stub;
    isc_service_detach := @isc_service_detach_stub;
    isc_service_query := @isc_service_query_stub;
    isc_service_start := @isc_service_start_stub;
    isc_decode_sql_date := @isc_decode_sql_date_stub;
    isc_decode_sql_time := @isc_decode_sql_time_stub;
    isc_decode_timestamp := @isc_decode_timestamp_stub;
    isc_encode_sql_date := @isc_encode_sql_date_stub;
    isc_encode_sql_time := @isc_encode_sql_time_stub;
    isc_encode_timestamp := @isc_encode_timestamp_stub;
  end;
end;

constructor TFB25ClientAPI.Create;
begin
  inherited;
  FStatus := TFBStatus.Create;
  FStatusIntf := FStatus;
  if (IBLibrary <> NilHandle) then
    LoadInterface;
  Firebird25ClientAPI := self;
end;

destructor TFB25ClientAPI.Destroy;
var i: integer;
begin
  for i := 0 to OwnedObjects.Count - 1 do
    if TObject(OwnedObjects[i]) is TFBAttachment then
      TFBAttachment(OwnedObjects[i]).Disconnect(true);
  Firebird25ClientAPI := nil;
  inherited Destroy;
end;


function TFB25ClientAPI.StatusVector: PISC_STATUS;
begin
  Result := FStatus.StatusVector;
end;

procedure TFB25ClientAPI.IBDataBaseError;
begin
  raise EIBInterBaseError.Create(FStatus);
end;

procedure TFB25ClientAPI.EncodeLsbf(aValue: integer; len: integer;
  var buffer: PChar);
begin
  while len > 0 do
  begin
    buffer^ := char(aValue mod 256);
    Inc(buffer);
    Dec(len);
    aValue := aValue shr 8;
  end;
end;

function TFB25ClientAPI.EncodeLsbf(aValue: integer; len: integer): string;
begin
  Result := '';
  while len > 0 do
  begin
    Result += char(aValue mod 256);
    Dec(len);
    aValue := aValue shr 8;
  end;
end;

function TFB25ClientAPI.GetStatus: IStatus;
begin
  Result := FStatus;
end;

function TFB25ClientAPI.OpenDatabase(DatabaseName: string; Params: TStrings
  ): IAttachment;
begin
   Result := TFBAttachment.Create(DatabaseName,Params)
end;

procedure TFB25ClientAPI.CreateDatabase(DatabaseName: string;
  SQLDialect: integer; Params: TStrings);
var
  tr_handle: TISC_TR_HANDLE;
  db_Handle: TISC_DB_HANDLE;
begin
  tr_handle := nil;
  db_Handle := nil;
  if isc_dsql_execute_immediate(StatusVector, @db_Handle, @tr_handle, 0,
                                 PChar('CREATE DATABASE ''' + DatabaseName + ''' ' + {do not localize}
                                 Params.Text), SQLDialect, nil) > 0 then
    IBDataBaseError;
end;

function TFB25ClientAPI.GetServiceManager(ServerName: string;
  Protocol: TProtocol; Params: TStrings): IServiceManager;
begin
  Result := TFBServiceManager.Create(ServerName,Protocol,Params);
end;

function TFB25ClientAPI.StartTransaction(Attachments: array of IAttachment;
  Params: TStrings; DefaultCompletion: TTransactionCompletion): ITransaction;
begin
  Result := TFBTransaction.Create(Attachments,Params,DefaultCompletion);
end;

function TFB25ClientAPI.IsEmbeddedServer: boolean;
begin
  Result := IsEmbeddedServer;
end;

function TFB25ClientAPI.GetLibraryName: string;
begin
  Result := FBLibraryName;
end;

function TFB25ClientAPI.HasServiceAPI: boolean;
begin
  Result := IBServiceAPIPresent;
end;

end.

