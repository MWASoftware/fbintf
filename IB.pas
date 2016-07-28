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

    (*****************************************)
    (* Service action items                 **)
    (*****************************************)

      isc_action_svc_backup         = 1; (* Starts database backup process on the server *)
      isc_action_svc_restore        = 2; (* Starts database restore process on the server *)
      isc_action_svc_repair         = 3; (* Starts database repair process on the server *)
      isc_action_svc_add_user       = 4; (* Adds a new user to the security database *)
      isc_action_svc_delete_user    = 5; (* Deletes a user record from the security database *)
      isc_action_svc_modify_user    = 6; (* Modifies a user record in the security database *)
      isc_action_svc_display_user   = 7; (* Displays a user record from the security database *)
      isc_action_svc_properties     = 8; (* Sets database properties *)
      isc_action_svc_add_license    = 9; (* Adds a license to the license file *)
      isc_action_svc_remove_license = 10; (* Removes a license from the license file *)
      isc_action_svc_db_stats	= 11; (* Retrieves database statistics *)
      isc_action_svc_get_ib_log     = 12; (* Retrieves the InterBase log file from the server *)

    (*****************************************)
    (** Service information items           **)
    (*****************************************)

      isc_info_svc_svr_db_info      = 50; (* Retrieves the number of attachments and databases *)
      isc_info_svc_get_license      = 51; (* Retrieves all license keys and IDs from the license file *)
      isc_info_svc_get_license_mask = 52; (* Retrieves a bitmask representing licensed options on the server *)
      isc_info_svc_get_config       = 53; (* Retrieves the parameters and values for IB_CONFIG *)
      isc_info_svc_version          = 54; (* Retrieves the version of the services manager *)
      isc_info_svc_server_version   = 55;(* Retrieves the version of the InterBase server *)
      isc_info_svc_implementation   = 56; (* Retrieves the implementation of the InterBase server *)
      isc_info_svc_capabilities     = 57; (* Retrieves a bitmask representing the server's capabilities *)
      isc_info_svc_user_dbpath      = 58; (* Retrieves the path to the security database in use by the server *)
      isc_info_svc_get_env	        = 59; (* Retrieves the setting of $INTERBASE *)
      isc_info_svc_get_env_lock     = 60; (* Retrieves the setting of $INTERBASE_LCK *)
      isc_info_svc_get_env_msg      = 61; (* Retrieves the setting of $INTERBASE_MSG *)
      isc_info_svc_line             = 62; (* Retrieves 1 line of service output per call *)
      isc_info_svc_to_eof           = 63; (* Retrieves as much of the server output as will fit in the supplied buffer *)
      isc_info_svc_timeout          = 64; (* Sets / signifies a timeout value for reading service information *)
      isc_info_svc_get_licensed_users = 65; (* Retrieves the number of users licensed for accessing the server *)
      isc_info_svc_limbo_trans	= 66; (* Retrieve the limbo transactions *)
      isc_info_svc_running		= 67; (* Checks to see if a service is running on an attachment *)
      isc_info_svc_get_users	= 68; (* Returns the user information from isc_action_svc_display_users *)

    (*****************************************)
    (* Parameters for isc_action_{add|delete|modify)_user *)
    (*****************************************)

      isc_spb_sec_userid            = 5;
      isc_spb_sec_groupid           = 6;
      isc_spb_sec_username          = 7;
      isc_spb_sec_password          = 8;
      isc_spb_sec_groupname         = 9;
      isc_spb_sec_firstname         = 10;
      isc_spb_sec_middlename        = 11;
      isc_spb_sec_lastname          = 12;

    (*****************************************)
    (* Parameters for isc_action_svc_(add|remove)_license, *)
    (* isc_info_svc_get_license                            *)
    (*****************************************)

      isc_spb_lic_key               = 5;
      isc_spb_lic_id                = 6;
      isc_spb_lic_desc              = 7;


    (*****************************************)
    (* Parameters for isc_action_svc_backup  *)
    (*****************************************)

      isc_spb_bkp_file               = 5;
      isc_spb_bkp_factor             = 6;
      isc_spb_bkp_length             = 7;
      isc_spb_bkp_ignore_checksums   = $01;
      isc_spb_bkp_ignore_limbo       = $02;
      isc_spb_bkp_metadata_only      = $04;
      isc_spb_bkp_no_garbage_collect = $08;
      isc_spb_bkp_old_descriptions   = $10;
      isc_spb_bkp_non_transportable  = $20;
      isc_spb_bkp_convert            = $40;
      isc_spb_bkp_expand             = $80;

    (*****************************************)
    (* Parameters for isc_action_svc_properties *)
    (*****************************************)

      isc_spb_prp_page_buffers	      = 5;
      isc_spb_prp_sweep_interval	      = 6;
      isc_spb_prp_shutdown_db	      =	7;
      isc_spb_prp_deny_new_attachments    = 9;
      isc_spb_prp_deny_new_transactions   = 10;
      isc_spb_prp_reserve_space	      = 11;
      isc_spb_prp_write_mode	      =	12;
      isc_spb_prp_access_mode	      =	13;
      isc_spb_prp_set_sql_dialect	      = 14;
      isc_spb_prp_activate		      = $0100;
      isc_spb_prp_db_online		      = $0200;

    (*****************************************)
    (* Parameters for isc_spb_prp_reserve_space *)
    (*****************************************)

      isc_spb_prp_res_use_full	      = 35;
      isc_spb_prp_res		      =	36;

    (*****************************************)
    (* Parameters for isc_spb_prp_write_mode  *)
    (*****************************************)

      isc_spb_prp_wm_async		= 37;
      isc_spb_prp_wm_sync		= 38;

    (*****************************************)
    (* Parameters for isc_spb_prp_access_mode *)
    (*****************************************)

      isc_spb_prp_am_readonly	= 39;
      isc_spb_prp_am_readwrite	= 40;

    (*****************************************)
    (* Parameters for isc_action_svc_repair  *)
    (*****************************************)

      isc_spb_rpr_commit_trans	       = 15;
      isc_spb_rpr_rollback_trans	       = 34;
      isc_spb_rpr_recover_two_phase	       = 17;
      isc_spb_tra_id                       = 18;
      isc_spb_single_tra_id		       = 19;
      isc_spb_multi_tra_id		       = 20;
      isc_spb_tra_state		       = 21;
      isc_spb_tra_state_limbo	       = 22;
      isc_spb_tra_state_commit	       = 23;
      isc_spb_tra_state_rollback	       = 24;
      isc_spb_tra_state_unknown	       = 25;
      isc_spb_tra_host_site		       = 26;
      isc_spb_tra_remote_site	       = 27;
      isc_spb_tra_db_path		       = 28;
      isc_spb_tra_advise		       = 29;
      isc_spb_tra_advise_commit	       = 30;
      isc_spb_tra_advise_rollback	       = 31;
      isc_spb_tra_advise_unknown	       = 33;
      isc_spb_rpr_validate_db	       = $01;
      isc_spb_rpr_sweep_db		       = $02;
      isc_spb_rpr_mend_db		       = $04;
      isc_spb_rpr_list_limbo_trans	       = $08;
      isc_spb_rpr_check_db		       = $10;
      isc_spb_rpr_ignore_checksum	       = $20;
      isc_spb_rpr_kill_shadows	       = $40;
      isc_spb_rpr_full		       = $80;

    (*****************************************)
    (* Parameters for isc_action_svc_restore  *)
    (*****************************************)

      isc_spb_res_buffers		       = 9;
      isc_spb_res_page_size		       = 10;
      isc_spb_res_length		       = 11;
      isc_spb_res_access_mode	       = 12;
      isc_spb_res_deactivate_idx	       = $0100;
      isc_spb_res_no_shadow		       = $0200;
      isc_spb_res_no_validity	       = $0400;
      isc_spb_res_one_at_a_time	       = $0800;
      isc_spb_res_replace		       = $1000;
      isc_spb_res_create		       = $2000;
      isc_spb_res_use_all_space	       = $4000;

    (*****************************************)
    (* Parameters for isc_spb_res_access_mode  *)
    (*****************************************)

      isc_spb_res_am_readonly		= isc_spb_prp_am_readonly;
      isc_spb_res_am_readwrite		= isc_spb_prp_am_readwrite;

    (*****************************************)
    (* Parameters for isc_info_svc_svr_db_info *)
    (*****************************************)

      isc_spb_num_att               = 5;
      isc_spb_num_db                = 6;

    (*****************************************)
    (* Parameters for isc_info_svc_db_stats  *)
    (*****************************************)

      isc_spb_sts_data_pages	= $01;
      isc_spb_sts_db_log		= $02;
      isc_spb_sts_hdr_pages		= $04;
      isc_spb_sts_idx_pages		= $08;
      isc_spb_sts_sys_relations	= $10;

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
    function GetInTransaction: boolean;
    procedure Commit;
    procedure CommitRetaining;
    function HasActivity: boolean;
    procedure Start;
    procedure Release;
    procedure Rollback;
    procedure RollbackRetaining;
    property InTransaction: boolean read GetInTransaction;
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
   property Data[index: integer]: ISQLData read getSQLData; default;
  end;

  IResultSet = interface(IResults)
    function FetchNext: boolean;
    function GetCursorName: string;
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
    function OpenCursor: IResultSet; overload;
    function OpenCursor(aTransaction: ITransaction): IResultSet; overload;
    procedure Release;
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
    procedure Release;
  end;

  IAttachment = interface
    function GetStatus: IStatus;
    procedure Connect;
    procedure Disconnect(Force: boolean);
    procedure DropDatabase;
    function StartTransaction(Params: TStrings): ITransaction;
    function CreateBlob(transaction: ITransaction): IBlob;
    function OpenBlob(transaction: ITransaction; BlobID: TISC_QUAD): IBlob;
    procedure ExecImmediate(transaction: ITransaction; sql: string; SQLDialect: integer);
    function Prepare(transaction: ITransaction; sql: string; SQLDialect: integer): IStatement;
    procedure Release;

    {Events}
    function GetEventHandler(Events: TStrings): IEvents;

    {Database Information}
    function GetBlobCharSetID(transaction: ITransaction; tableName, columnName: string): short;
    function GetOperationCounts(DBInfoCommand: char; aOperation: TStringList): TStringList;
    function GetInfoValue(DBInfoCommand: char): integer;
    function GetInfoString(DBInfoCommand: char): string;
    function GetInfoBuffer(DBInfoCommand: char; var Buffer: PChar): integer;
    function HasActivity: boolean;
  end;

  TProtocol = (TCP, SPX, NamedPipe, Local);

  TServiceParam = record
    Identifier: char;
    case dt: (dtInteger,dtString,dtNone) of
    dtInteger: (IntValue: integer);
    dtString:  (StringValue: shortstring);
  end;

  TServiceResponse = record
    Identifier: char;
    dr: (drInteger,drString,drNone);
    IntValue: integer;
    StringValue: string;
  end;

  TServiceCommandParams = array of TServiceParam;
  TServiceQueryParams = array of TServiceParam;
  TServiceQueryResponse = array of TServiceResponse;

  { IServiceManager }

  IServiceManager = interface
    function GetStatus: IStatus;
    procedure Attach;
    procedure Detach;
    function IsAttached: boolean;
    procedure Start(Command: char; Params: TServiceCommandParams);
    function Query(Command: char; Params: TServiceQueryParams) :TServiceQueryResponse; overload;
    function Query(Commands: array of char):TServiceQueryResponse; overload;
    procedure Release;
  end;

  IFirebirdAPI = interface
    function GetStatus: IStatus;
    function OpenDatabase(DatabaseName: string; Params: TStrings): IAttachment;
    procedure CreateDatabase(DatabaseName: string; SQLDialect: integer;
                                          Params: TStrings);
    function GetServiceManager(ServerName: string; Protocol: TProtocol; Params: TStrings): IServiceManager;
    function StartTransaction(Attachments: array of IAttachment;
             Params: TStrings): ITransaction; {Start Transaction against multiple databases}
    function IsEmbeddedServer: boolean;
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

function TryIBLoad: Boolean;
var FBLibraryObj: TFBLibrary;
begin
  Result := FirebirdAPI <> nil;
  if not Result then
  begin
    FBLibraryObj := TFB25ClientAPI.Create;
    FirebirdAPI := TFB25ClientAPI(FBLibraryObj);
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


end.

