unit IB;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, FBErrorMessages, IBExternals;

const
  (********************************)
  (** Database information items **)
  (********************************)

    isc_info_db_id                 =          4;
    isc_info_reads                 =          5;
    isc_info_writes                =          6;
    isc_info_fetches               =          7;
    isc_info_marks                 =          8;
    isc_info_implementation        =         11;
    isc_info_version               =         12;
    isc_info_base_level            =         13;
    isc_info_page_size             =         14;
    isc_info_num_buffers           =         15;
    isc_info_limbo                 =         16;
    isc_info_current_memory        =         17;
    isc_info_max_memory            =         18;
    isc_info_window_turns          =         19;
    isc_info_license               =         20;
    isc_info_allocation            =         21;
    isc_info_attachment_id         =         22;
    isc_info_read_seq_count        =         23;
    isc_info_read_idx_count        =         24;
    isc_info_insert_count          =         25;
    isc_info_update_count          =         26;
    isc_info_delete_count          =         27;
    isc_info_backout_count         =         28;
    isc_info_purge_count           =         29;
    isc_info_expunge_count         =         30;
    isc_info_sweep_interval        =         31;
    isc_info_ods_version           =         32;
    isc_info_ods_minor_version     =         33;
    isc_info_no_reserve            =         34;
    isc_info_logfile               =         35;
    isc_info_cur_logfile_name      =         36;
    isc_info_cur_log_part_offset   =         37;
    isc_info_num_wal_buffers       =         38;
    isc_info_wal_buffer_size       =         39;
    isc_info_wal_ckpt_length       =         40;
    isc_info_wal_cur_ckpt_interval =         41;
    isc_info_wal_prv_ckpt_fname    =         42;
    isc_info_wal_prv_ckpt_poffset  =         43;
    isc_info_wal_recv_ckpt_fname   =         44;
    isc_info_wal_recv_ckpt_poffset =         45;
    isc_info_wal_grpc_wait_usecs   =         47;
    isc_info_wal_num_io            =         48;
    isc_info_wal_avg_io_size       =         49;
    isc_info_wal_num_commits       =         50;
    isc_info_wal_avg_grpc_size     =         51;
    isc_info_forced_writes         =         52;
    isc_info_user_names            =         53;
    isc_info_page_errors           =         54;
    isc_info_record_errors         =         55;
    isc_info_bpage_errors          =         56;
    isc_info_dpage_errors          =         57;
    isc_info_ipage_errors          =         58;
    isc_info_ppage_errors          =         59;
    isc_info_tpage_errors          =         60;
    isc_info_set_page_buffers      =         61;
    isc_info_db_SQL_dialect        =         62;
    isc_info_db_read_only          =         63;
    isc_info_db_size_in_pages      =         64;

  (****************************************)
  (** Database information return values **)
  (****************************************)

    isc_info_db_impl_rdb_vms       =          1;
    isc_info_db_impl_rdb_eln       =          2;
    isc_info_db_impl_rdb_eln_dev   =          3;
    isc_info_db_impl_rdb_vms_y     =          4;
    isc_info_db_impl_rdb_eln_y     =          5;
    isc_info_db_impl_jri           =          6;
    isc_info_db_impl_jsv           =          7;
    isc_info_db_impl_isc_a         =         25;
    isc_info_db_impl_isc_u         =         26;
    isc_info_db_impl_isc_v         =         27;
    isc_info_db_impl_isc_s         =         28;
    isc_info_db_impl_isc_apl_68K   =         25;
    isc_info_db_impl_isc_vax_ultr  =         26;
    isc_info_db_impl_isc_vms       =         27;
    isc_info_db_impl_isc_sun_68k   =         28;
    isc_info_db_impl_isc_os2       =         29;
    isc_info_db_impl_isc_sun4      =         30;
    isc_info_db_impl_isc_hp_ux     =         31;
    isc_info_db_impl_isc_sun_386i  =         32;
    isc_info_db_impl_isc_vms_orcl  =         33;
    isc_info_db_impl_isc_mac_aux   =         34;
    isc_info_db_impl_isc_rt_aix    =         35;
    isc_info_db_impl_isc_mips_ult  =         36;
    isc_info_db_impl_isc_xenix     =         37;
    isc_info_db_impl_isc_dg        =         38;
    isc_info_db_impl_isc_hp_mpexl  =         39;
    isc_info_db_impl_isc_hp_ux68K  =         40;
    isc_info_db_impl_isc_sgi       =         41;
    isc_info_db_impl_isc_sco_unix  =         42;
    isc_info_db_impl_isc_cray      =         43;
    isc_info_db_impl_isc_imp       =         44;
    isc_info_db_impl_isc_delta     =         45;
    isc_info_db_impl_isc_next      =         46;
    isc_info_db_impl_isc_dos       =         47;
    isc_info_db_impl_isc_winnt     =         48;
    isc_info_db_impl_isc_epson     =         49;

    isc_info_db_class_access       =          1;
    isc_info_db_class_y_valve      =          2;
    isc_info_db_class_rem_int      =          3;
    isc_info_db_class_rem_srvr     =          4;
    isc_info_db_class_pipe_int     =          7;
    isc_info_db_class_pipe_srvr    =          8;
    isc_info_db_class_sam_int      =          9;
    isc_info_db_class_sam_srvr     =         10;
    isc_info_db_class_gateway      =         11;
    isc_info_db_class_cache        =         12;

type
   (*********************************************************************)
   (** Blob id structure                                               **)
   (*********************************************************************)
   TGDS_QUAD = record
     gds_quad_high      : ISC_LONG;
     gds_quad_low       : UISC_LONG;
   end;
   TGDS__QUAD           = TGDS_QUAD;
   TISC_QUAD            = TGDS_QUAD;
   PGDS_QUAD            = ^TGDS_QUAD;
   PGDS__QUAD           = ^TGDS__QUAD;
   PISC_QUAD            = ^TISC_QUAD;

   TFBStatusCode = cardinal;

  IStatus = interface
    function GetIBErrorCode: Long;
    function Getsqlcode: Long;
    function GetMessage: string;
    function CheckStatusVector(ErrorCodes: array of TFBStatusCode): Boolean;
    function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
  end;

  ITransaction = interface
    function GetStatus: IStatus;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;
  end;

  IBlob = interface
    function GetStatus: IStatus;
    procedure Cancel;
    procedure Close;
    function GetBlobID: TISC_QUAD;
    function GetInfo(var NumSegments: Int64; var MaxSegmentSize,
                      TotalSize: Int64; var BlobType: Short) :boolean;
    function Read(var Buffer; Count: Longint): Longint;
    function Write(const Buffer; Count: Longint): Longint;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(S: TStream);
 end;

  IFieldMetaData = interface
    function GetSQLType: cardinal;
    function getSubtype: integer;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if not alias}
    function getName: string;       {Disambiguated uppercase Field Name}
    function getScale: integer;
    function getCharSetID: cardinal;
    function getIsNullable: boolean;
    function GetSize: integer;
    property Name: string read GetName;
    property Size: Integer read GetSize;
    property CharSetID: cardinal read getCharSetID;
    property SQLType: cardinal read GetSQLType;
    property SQLSubtype: integer read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  end;

  IMetaData = interface
    function getCount: integer;
    function getFieldMetaData(index: integer): IFieldMetaData;
    function ByName(Idx: String): IFieldMetaData;
  end;

  ISQLData = interface(IFieldMetaData)
    function GetAsBoolean: boolean;
    function GetAsCurrency: Currency;
    function GetAsInt64: Int64;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsFloat: Float;
    function GetAsLong: Long;
    function GetAsPointer: Pointer;
    function GetAsQuad: TISC_QUAD;
    function GetAsShort: Short;
    function GetAsString: String;
    function GetIsNull: Boolean;
    function GetAsVariant: Variant;
    property AsDate: TDateTime read GetAsDateTime;
    property AsBoolean:boolean read GetAsBoolean;
    property AsTime: TDateTime read GetAsDateTime;
    property AsDateTime: TDateTime read GetAsDateTime ;
    property AsDouble: Double read GetAsDouble;
    property AsFloat: Float read GetAsFloat;
    property AsCurrency: Currency read GetAsCurrency;
    property AsInt64: Int64 read GetAsInt64 ;
    property AsInteger: Integer read GetAsLong;
    property AsLong: Long read GetAsLong;
    property AsPointer: Pointer read GetAsPointer;
    property AsQuad: TISC_QUAD read GetAsQuad;
    property AsShort: Short read GetAsShort;
    property AsString: String read GetAsString;
    property AsVariant: Variant read GetAsVariant ;
    property IsNull: Boolean read GetIsNull;
  end;

  IResults = interface
   function getCount: integer;
   function ByName(Idx: String): ISQLData;
   function getSQLData(index: integer): ISQLData;
  end;

  IResultSet = interface(IResults)
    function FetchNext: boolean;
    procedure Close;
  end;

  ISQLParam = interface
    function GetSQLType: cardinal;
    function getSubtype: integer;
    function getScale: integer;
    function getCharSetID: cardinal;
    function GetSize: integer;
    function GetAsBoolean: boolean;
    function GetAsCurrency: Currency;
    function GetAsInt64: Int64;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsFloat: Float;
    function GetAsLong: Long;
    function GetAsPointer: Pointer;
    function GetAsQuad: TISC_QUAD;
    function GetAsShort: Short;
    function GetAsString: String;
    function GetIsNull: Boolean;
    function GetAsVariant: Variant;
    function getName: string;
    procedure SetAsBoolean(AValue: boolean);
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsInt64(Value: Int64);
    procedure SetAsDate(Value: TDateTime);
    procedure SetAsLong(Value: Long);
    procedure SetAsTime(Value: TDateTime);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsDouble(Value: Double);
    procedure SetAsFloat(Value: Float);
    procedure SetAsPointer(Value: Pointer);
    procedure SetAsQuad(Value: TISC_QUAD);
    procedure SetAsShort(Value: Short);
    procedure SetAsString(Value: String);
    procedure SetAsVariant(Value: Variant);
    procedure SetIsNull(Value: Boolean);
    function getModified: boolean;
    procedure Clear;
    procedure SetName(Value: string);
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsBoolean:boolean read GetAsBoolean write SetAsBoolean;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsFloat: Float read GetAsFloat write SetAsFloat;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsInteger: Integer read GetAsLong write SetAsLong;
    property AsLong: Long read GetAsLong write SetAsLong;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property AsShort: Short read GetAsShort write SetAsShort;
    property AsString: String read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property Modified: Boolean read getModified;
    property Name: string read GetName write SetName;
 end;

  ISQLParams = interface
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ByName(Idx: String): ISQLParam ;
  end;


  TIBSQLTypes = (SQLUnknown, SQLSelect, SQLInsert,
                  SQLUpdate, SQLDelete, SQLDDL,
                  SQLGetSegment, SQLPutSegment,
                  SQLExecProcedure, SQLStartTransaction,
                  SQLCommit, SQLRollback,
                  SQLSelectForUpdate, SQLSetGenerator);

  IStatement = interface
    function GetStatus: IStatus;
    function GetSQLParams: ISQLParams;
    function GetOutMetaData: IMetaData;
    function GetPlan: String;
    function GetRowsAffected(var InsertCount, UpdateCount, DeleteCount: integer): boolean;
    function GetSQLType: TIBSQLTypes;
    function Execute: IResults; overload;
    function Execute(aTransaction: ITransaction): IResults; overload;
    function OpenCursor: IResultSet;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLType: TIBSQLTypes read GetSQLType;
  end;

type
  TEventInfo = record
    EventName: string;
    Count: integer;
  end;

  TEventCounts = array of TEventInfo;

  TEventHandler = procedure(EventCounts: TEventCounts) of object;

  IEvents = interface
    function GetStatus: IStatus;
    procedure Cancel;
    procedure WaitForEvent(var EventCounts: TEventCounts);
    procedure AsyncWaitForEvent(EventHandler: TEventHandler);
  end;

  IAttachment = interface
    function GetStatus: IStatus;
    procedure Disconnect(Force: boolean);
    procedure DropDatabase;
    function StartTransaction(Params: TStrings): ITransaction;
    function CreateBlob(transaction: ITransaction): IBlob;
    function OpenBlob(transaction: ITransaction; BlobID: TISC_QUAD): IBlob;
    procedure ExecImmediate(transaction: ITransaction; sql: string; SQLDialect: integer);
    function Prepare(transaction: ITransaction; sql: string; SQLDialect: integer): IStatement;

    {Events}
    function GetEventHandler(Events: TStrings): IEvents;

    {Database Information}
    function GetBlobCharSetID(transaction: ITransaction; tableName, columnName: string): short;
    function GetOperationCounts(DBInfoCommand: char; aOperation: TStringList): TStringList;
    function GetInfoValue(DBInfoCommand: char): integer;
    function GetInfoString(DBInfoCommand: char): string;
    function GetInfoBuffer(DBInfoCommand: char; var Buffer: PChar): integer;
  end;

  IService = interface
//    procedure Attach(ServerName: string; Protocol: TProtocol; Params: TStrings);
//    procedure Detach;
  end;

  IFirebirdAPI = interface
    function GetStatus: IStatus;
    function OpenDatabase(DatabaseName: string; Params: TStrings): IAttachment;
    procedure CreateDatabase(DatabaseName: string; SQLDialect: integer;
                                          Params: TStrings);
    function GetServiceManager(Service: string; Params: TStrings): IService;
    {Start Transaction against multiple databases}
    function StartTransaction(Databases: array of IAttachment; Params: TStrings): ITransaction;
    function GetIsEmbeddedServer: boolean;
    function GetLibraryName: string;
    function HasServiceAPI: boolean;
  end;

type
  TOnGetLibraryName = procedure(var libname: string);

const
  OnGetLibraryName: TOnGetLibraryName = nil;

type
   { EIBError }

   EIBError = class(EDatabaseError)
   private
     FSQLCode: Long;
   public
     constructor Create(ASQLCode: Long; Msg: string);
     property SQLCode: Long read FSQLCode;
   end;

   { EIBInterBaseError }

   EIBInterBaseError = class(EIBError)
   private
     FIBErrorCode: Long;
   public
     constructor Create(Status: IStatus); overload;
     constructor Create(ASQLCode: Long; AIBErrorCode: Long; Msg: string); overload;
     property IBErrorCode: Long read FIBErrorCode;
   end;

   EIBClientError = class(EIBError);

procedure IBError(ErrMess: TIBClientError; const Args: array of const);

var FirebirdAPI: IFirebirdAPI;

function TryIBLoad: Boolean;
procedure CheckIBLoaded;

implementation

uses FBLibrary, FB25ClientAPI;

const FBLibraryObj: TFBLibrary = nil;

function TryIBLoad: Boolean;
begin
  Result := false;
  if FBLibraryObj = nil then
  begin
    FBLibraryObj := TFBClientAPI.Create;
    FirebirdAPI := TFBClientAPI(FBLibraryObj);
    Result := true;
  end;
end;

procedure CheckIBLoaded;
begin
  if not TryIBLoad then
    IBError(ibxeInterBaseMissing, [nil]);
end;

{ EIBError }

constructor EIBError.Create(ASQLCode: Long; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode := ASQLCode;
end;

{ EIBInterBaseError }

constructor EIBInterBaseError.Create(Status: IStatus);
begin
  inherited Create(Status.Getsqlcode,Status.GetMessage);
  FIBErrorCode := Status.GetIBErrorCode;
end;

constructor EIBInterBaseError.Create(ASQLCode: Long; AIBErrorCode: Long;
  Msg: string);
begin
  inherited Create(ASQLCode,Msg);
  FIBErrorCode := AIBErrorCode;
end;

procedure IBError(ErrMess: TIBClientError; const Args: array of const);
begin
  raise EIBClientError.Create(Ord(ErrMess),
                              Format(GetErrorMessage(ErrMess), Args));
end;

initialization
  FirebirdAPI := nil;

finalization
  if FBLibraryObj <> nil then
    FBLibraryObj.Free;

end.

