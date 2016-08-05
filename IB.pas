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
    isc_info_db_impl_isc_portable_ultr  =         26;
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

      (***************************************)
      (** Transaction parameter block stuff **)
      (***************************************)

        isc_tpb_version1               =          1;
        isc_tpb_version3               =          3;
        isc_tpb_consistency            =          1;
        isc_tpb_concurrency            =          2;
        isc_tpb_shared                 =          3;
        isc_tpb_protected              =          4;
        isc_tpb_exclusive              =          5;
        isc_tpb_wait                   =          6;
        isc_tpb_nowait                 =          7;
        isc_tpb_read                   =          8;
        isc_tpb_write                  =          9;
        isc_tpb_lock_read              =         10;
        isc_tpb_lock_write             =         11;
        isc_tpb_verb_time              =         12;
        isc_tpb_commit_time            =         13;
        isc_tpb_ignore_limbo           =         14;
        isc_tpb_read_committed         =         15;
        isc_tpb_autocommit             =         16;
        isc_tpb_rec_version            =         17;
        isc_tpb_no_rec_version         =         18;
        isc_tpb_restart_requests       =         19;
        isc_tpb_no_auto_undo           =         20;
        isc_tpb_last_tpb_constant      =         isc_tpb_no_auto_undo;

        (************************************)
        (** Database parameter block stuff **)
        (************************************)

          isc_dpb_version1               =          1;
          isc_dpb_cdd_pathname           =          1;
          isc_dpb_allocation             =          2;
          isc_dpb_journal                =          3;
          isc_dpb_page_size              =          4;
          isc_dpb_num_buffers            =          5;
          isc_dpb_buffer_length          =          6;
          isc_dpb_debug                  =          7;
          isc_dpb_garbage_collect        =          8;
          isc_dpb_verify                 =          9;
          isc_dpb_sweep                  =         10;
          isc_dpb_enable_journal         =         11;
          isc_dpb_disable_journal        =         12;
          isc_dpb_dbkey_scope            =         13;
          isc_dpb_number_of_users        =         14;
          isc_dpb_trace                  =         15;
          isc_dpb_no_garbage_collect     =         16;
          isc_dpb_damaged                =         17;
          isc_dpb_license                =         18;
          isc_dpb_sys_user_name          =         19;
          isc_dpb_encrypt_key            =         20;
          isc_dpb_activate_shadow        =         21;
          isc_dpb_sweep_interval         =         22;
          isc_dpb_delete_shadow          =         23;
          isc_dpb_force_write            =         24;
          isc_dpb_begin_log              =         25;
          isc_dpb_quit_log               =         26;
          isc_dpb_no_reserve             =         27;
          isc_dpb_user_name              =         28;
          isc_dpb_password               =         29;
          isc_dpb_password_enc           =         30;
          isc_dpb_sys_user_name_enc      =         31;
          isc_dpb_interp                 =         32;
          isc_dpb_online_dump            =         33;
          isc_dpb_old_file_size          =         34;
          isc_dpb_old_num_files          =         35;
          isc_dpb_old_file               =         36;
          isc_dpb_old_start_page         =         37;
          isc_dpb_old_start_seqno        =         38;
          isc_dpb_old_start_file         =         39;
          isc_dpb_drop_walfile           =         40;
          isc_dpb_old_dump_id            =         41;
          isc_dpb_wal_backup_dir         =         42;
          isc_dpb_wal_chkptlen           =         43;
          isc_dpb_wal_numbufs            =         44;
          isc_dpb_wal_bufsize            =         45;
          isc_dpb_wal_grp_cmt_wait       =         46;
          isc_dpb_lc_messages            =         47;
          isc_dpb_lc_ctype               =         48;
          isc_dpb_cache_manager          =         49;
          isc_dpb_shutdown               =         50;
          isc_dpb_online                 =         51;
          isc_dpb_shutdown_delay         =         52;
          isc_dpb_reserved               =         53;
          isc_dpb_overwrite              =         54;
          isc_dpb_sec_attach             =         55;
          isc_dpb_disable_wal            =         56;
          isc_dpb_connect_timeout        =         57;
          isc_dpb_dummy_packet_interval  =         58;
          isc_dpb_gbak_attach            =         59;
          isc_dpb_sql_role_name          =         60;
          isc_dpb_set_page_buffers       =         61;
          isc_dpb_working_directory      =         62;
          isc_dpb_SQL_dialect            =         63;
          isc_dpb_set_db_readonly        =         64;
          isc_dpb_set_db_SQL_dialect     =         65;
          isc_dpb_gfix_attach		 =         66;
          isc_dpb_gstat_attach		 =         67;
          isc_dpb_last_dpb_constant      =         isc_dpb_gstat_attach;


        (***********************************)
        (** isc_dpb_verify specific flags **)
        (***********************************)

          isc_dpb_pages                  =          1;
          isc_dpb_records                =          2;
          isc_dpb_indices                =          4;
          isc_dpb_transactions           =          8;
          isc_dpb_no_update              =         16;
          isc_dpb_repair                 =         32;
          isc_dpb_ignore                 =         64;

        (*************************************)
        (** isc_dpb_shutdown specific flags **)
        (*************************************)

          isc_dpb_shut_cache             =          1;
          isc_dpb_shut_attachment        =          2;
          isc_dpb_shut_transaction       =          4;
          isc_dpb_shut_force             =          8;

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

  {$IF FPC_FULLVERSION < 20700 }
  RawByteString = AnsiString; {Needed for backwards compatibility}
  {$ENDIF}

  TFBStatusCode = cardinal;
  TByteArray = array of byte;

  IAttachment = interface;
  ITransaction = interface;

  IStatus = interface
    function GetIBErrorCode: Long;
    function Getsqlcode: Long;
    function GetMessage: string;
    function CheckStatusVector(ErrorCodes: array of TFBStatusCode): Boolean;
    function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
  end;

  TArrayBound = record
    UpperBound: short;
    LowerBound: short;
  end;
  TArrayBounds = array of TArrayBound;
  TArrayCoords = array of integer;

  IArrayMetaData = interface
    function GetSQLType: short;
    function GetTableName: string;
    function GetColumnName: string;
    function GetDimensions: integer;
    function GetBounds: TArrayBounds;
  end;

  ISQLElement = interface;

  IArray = interface(IArrayMetaData)
    function GetAsInteger(index: array of integer): integer;
    function GetAsBoolean(index: array of integer): boolean;
    function GetAsCurrency(index: array of integer): Currency;
    function GetAsInt64(index: array of integer): Int64;
    function GetAsDateTime(index: array of integer): TDateTime;
    function GetAsDouble(index: array of integer): Double;
    function GetAsFloat(index: array of integer): Float;
    function GetAsLong(index: array of integer): Long;
    function GetAsShort(index: array of integer): Short;
    function GetAsString(index: array of integer): String;
    function GetAsVariant(index: array of integer): Variant;
    procedure SetAsInteger(index: array of integer; AValue: integer);
    procedure SetAsBoolean(index: array of integer; AValue: boolean);
    procedure SetAsCurrency(index: array of integer; Value: Currency);
    procedure SetAsInt64(index: array of integer; Value: Int64);
    procedure SetAsDate(index: array of integer; Value: TDateTime);
    procedure SetAsLong(index: array of integer; Value: Long);
    procedure SetAsTime(index: array of integer; Value: TDateTime);
    procedure SetAsDateTime(index: array of integer; Value: TDateTime);
    procedure SetAsDouble(index: array of integer; Value: Double);
    procedure SetAsFloat(index: array of integer; Value: Float);
    procedure SetAsShort(index: array of integer; Value: Short);
    procedure SetAsString(index: array of integer; Value: String);
    procedure SetAsVariant(index: array of integer; Value: Variant);
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
  end;

  IBlobMetaData = interface
    function GetSubType: short;
    function GetCharSetID: short;
    function GetSegmentSize: short;
    function GetTableName: string;
    function GetColumnName: string;
  end;

  TFBBlobMode = (fbmRead,fbmWrite);
  TBlobType = (btSegmented,btStream);

  IBlob = interface
    procedure Cancel;
    procedure Close;
    function GetBlobID: TISC_QUAD;
    function GetBlobMode: TFBBlobMode;
    function GetInfo(var NumSegments: Int64; var MaxSegmentSize,
                      TotalSize: Int64; var BlobType: TBlobType) :boolean;
    function Read(var Buffer; Count: Longint): Longint;
    function Write(const Buffer; Count: Longint): Longint;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(S: TStream);
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
 end;

  { IColumnMetaData }

  IColumnMetaData = interface
    function GetSQLType: short;
    function getSubtype: short;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if not alias}
    function getName: string;       {Disambiguated uppercase Field Name}
    function getScale: short;
    function getCharSetID: cardinal;
    function getIsNullable: boolean;
    function GetSize: integer;
    function GetArrayMetaData: IArrayMetaData; {Valid only for Array SQL Type}
    function GetBlobMetaData: IBlobMetaData; {Valid only for Blob SQL Type}
    property Name: string read GetName;
    property Size: Integer read GetSize;
    property CharSetID: cardinal read getCharSetID;
    property SQLType: short read GetSQLType;
    property SQLSubtype: short read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  end;

  { IMetaData }

  IMetaData = interface
    function getCount: integer;
    function getColumnMetaData(index: integer): IColumnMetaData;
    function GetUniqueRelationName: string;
    function ByName(Idx: String): IColumnMetaData;
    property ColMetaData[index: integer]: IColumnMetaData read getColumnMetaData; default;
  end;

  ISQLData = interface(IColumnMetaData)
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
    function GetAsBlob: IBlob;
    function GetAsArray: IArray;
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
    property AsBlob: IBlob read GetAsBlob;
    property AsArray: IArray read GetAsArray;
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

  ISQLElement = interface
    function getScale: short;
    function GetSize: integer;
    function GetAsBoolean: boolean;
    function GetAsCurrency: Currency;
    function GetAsInt64: Int64;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsFloat: Float;
    function GetAsLong: Long;
    function GetAsPointer: Pointer;
    function GetAsShort: Short;
    function GetAsString: String;
    function GetAsVariant: Variant;
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
    procedure SetAsShort(Value: Short);
    procedure SetAsString(Value: String);
    procedure SetAsVariant(Value: Variant);
    function GetModified: boolean;
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
    property AsShort: Short read GetAsShort write SetAsShort;
    property AsString: String read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;


  ISQLParam = interface(ISQLElement)
    procedure Clear;
    function getCharSetID: cardinal;
    function GetIsNull: Boolean;
    function GetAsBlob: IBlob;
    function GetAsArray: IArray;
    function GetName: string;
    function GetSQLType: short;
    function GetSubtype: short;
    function GetAsQuad: TISC_QUAD;
    procedure SetIsNull(Value: Boolean);
    procedure SetAsBlob(Value: IBlob);
    procedure SetAsArray(anArray: IArray);
    procedure SetAsQuad(Value: TISC_QUAD);
    property AsBlob: IBlob read GetAsBlob write SetAsBlob;
    property AsArray: IArray read GetAsArray write SetAsArray;
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property Modified: Boolean read getModified;
    property Name: string read GetName;
 end;

  ISQLParams = interface
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ByName(Idx: String): ISQLParam ;
    function GetModified: Boolean;
    property Modified: Boolean read GetModified;
    property Params[index: integer]: ISQLParam read getSQLParam; default;
  end;


  TIBSQLTypes = (SQLUnknown, SQLSelect, SQLInsert,
                  SQLUpdate, SQLDelete, SQLDDL,
                  SQLGetSegment, SQLPutSegment,
                  SQLExecProcedure, SQLStartTransaction,
                  SQLCommit, SQLRollback,
                  SQLSelectForUpdate, SQLSetGenerator);

  IDBInformation = interface;

  IStatement = interface
    function GetSQLParams: ISQLParams;
    function GetMetaData: IMetaData;
    function GetPlan: String;
    function GetRowsAffected(var InsertCount, UpdateCount, DeleteCount: integer): boolean;
    function GetSQLType: TIBSQLTypes;
    function GetSQLText: string;
    function IsPrepared: boolean;
    procedure Prepare(aTransaction: ITransaction=nil);
    function Execute(aTransaction: ITransaction=nil): IResults;
    function OpenCursor(aTransaction: ITransaction=nil): IResultSet;
    function CreateBlob: IBlob;
    function CreateArray(column: IColumnMetaData): IArray; overload;
    function CreateArray(columnName: string): IArray;  overload;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLType: TIBSQLTypes read GetSQLType;
  end;

  TTransactionCompletion = (tcCommit,tcRollback);

  ITransaction = interface
    procedure Start(DefaultCompletion: TTransactionCompletion);
    function GetInTransaction: boolean;
    procedure PrepareForCommit;
    procedure Commit;
    procedure CommitRetaining;
    function HasActivity: boolean;
    procedure Rollback;
    procedure RollbackRetaining;
    function GetAttachmentCount: integer;
    function GetAttachment(index: integer): IAttachment;
    property InTransaction: boolean read GetInTransaction;
  end;

  TEventInfo = record
    EventName: string;
    Count: integer;
  end;

  TEventCounts = array of TEventInfo;
  IEvents = interface;
  TEventHandler = procedure(Sender: IEvents) of object;

  IEvents = interface
    procedure GetEvents(EventNames: TStrings);
    procedure SetEvents(EventNames: TStrings);
    procedure Cancel;
    function ExtractEventCounts: TEventCounts;
    procedure WaitForEvent;
    procedure AsyncWaitForEvent(EventHandler: TEventHandler);
    function GetAttachment: IAttachment;
  end;

  TDBOperationCount = record
    TableID: UShort;
    Count: cardinal;
  end;

  TDBOperationCounts = array of TDBOperationCount;

  IDBInfoItem = interface
    function getItemType: byte;
    function getSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: string;
    function getAsInteger: integer;
    procedure DecodeIDCluster(var ConnectionType: integer; var DBFileName, DBSiteName: string);
    function getAsBytes: TByteArray;
    procedure DecodeVersionString(var Version: byte; var VersionString: string);
    procedure DecodeUserNames(var UserNames: TStrings);
    function getOperationCounts: TDBOperationCounts;
  end;

  { IDBInformation }

  IDBInformation = interface
    function getCount: integer;
    function getItem(index: integer): IDBInfoItem;
    function Find(ItemType: byte): IDBInfoItem;
    property Items[index: integer]: IDBInfoItem read getItem; default;
  end;

  IDPBItem = interface
    function getParamType: byte;
    function getAsString: string;
    function getAsByte: byte;
    procedure setAsString(aValue: string);
    procedure setAsByte(aValue: byte);
    property AsString: string read getAsString write setAsString;
    property AsByte: byte read getAsByte write setAsByte;
  end;

  IDPB = interface
    function getCount: integer;
    function Add(ParamType: byte): IDPBItem;
    function getItems(index: integer): IDPBItem;
    function Find(ParamType: byte): IDPBItem;
    procedure Remove(ParamType: byte);
    property Items[index: integer]: IDPBItem read getItems; default;
  end;

  { IAttachment }

  IAttachment = interface
    function getDPB: IDPB;
    procedure Connect;
    procedure Disconnect(Force: boolean=false);
    procedure DropDatabase;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction;
    function CreateBlob(transaction: ITransaction): IBlob;
    procedure ExecImmediate(transaction: ITransaction; sql: string; SQLDialect: integer); overload;
    procedure ExecImmediate(TPB: array of byte; sql: string; SQLDialect: integer); overload;
    procedure ExecImmediate(transaction: ITransaction; sql: string); overload;
    procedure ExecImmediate(TPB: array of byte; sql: string); overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string): IResultSet; overload;
    function Prepare(transaction: ITransaction; sql: string; aSQLDialect: integer): IStatement; overload;
    function Prepare(transaction: ITransaction; sql: string): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       aSQLDialect: integer; GenerateParamNames: boolean=false;
                       UniqueParamNames: boolean=false): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       GenerateParamNames: boolean=false;
                       UniqueParamNames: boolean=false): IStatement; overload;

    {Events}
    function GetEventHandler(Events: TStrings): IEvents;

    {Blob - may use to open existing Blobs. However, ISQLData.AsBlob is preferred}

    function OpenBlob(Transaction: ITransaction; BlobID: TISC_QUAD): IBlob;

    {Database Information}
    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: string): IBlobMetaData;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: string): IArrayMetaData;
    function GetDBInformation(DBInfoCommand: byte): IDBInformation;
    function HasActivity: boolean;
  end;

  TProtocol = (TCP, SPX, NamedPipe, Local);

  IServiceRequestItem = interface
    function getItemType: byte;
    function getAsString: string;
    function getAsInteger: integer;
    procedure setAsString(aValue: string);
    procedure setAsInteger(aValue: integer);
    property AsString: string read getAsString write setAsString;
    property AsInteger: integer read getAsInteger write setAsInteger;
  end;

  IServiceRequest = interface
    function getAction: byte;
    function Add(ItemType: byte): IServiceRequestItem;
    function getCount: integer;
    function getItem(index: integer): IServiceRequestItem;
    function find(ItemType: byte): IServiceRequestItem;
    property Items[index: integer]: IServiceRequestItem read getItem; default;
  end;

  IServiceQueryResultSubItem = interface
    function getItemType: byte;
    function getDataSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: string;
    function getAsInteger: integer;
    function getIsTruncated: boolean;
  end;

  IServiceQueryResultItem = interface(IServiceQueryResultSubItem)
    function getCount: integer;
    function getItem(index: integer): IServiceQueryResultSubItem;
    function find(ItemType: byte): IServiceQueryResultSubItem;
    property Items[index: integer]: IServiceQueryResultSubItem read getItem; default;
  end;

  IServiceQueryResults = interface
    function getCount: integer;
    function getItem(index: integer): IServiceQueryResultItem;
    function find(ItemType: byte): IServiceQueryResultItem;
    property Items[index: integer]: IServiceQueryResultItem read getItem; default;
  end;

  ISPBItem = interface
    function getParamType: byte;
    function getAsString: string;
    function getAsByte: byte;
    procedure setAsString(aValue: string);
    procedure setAsByte(aValue: byte);
    property AsString: string read getAsString write setAsString;
    property AsByte: byte read getAsByte write setAsByte;
  end;

  ISPB = interface
    function getCount: integer;
    function Add(ParamType: byte): ISPBItem;
    function getItems(index: integer): ISPBItem;
    function Find(ParamType: byte): ISPBItem;
    procedure Remove(ParamType: byte);
    property Items[index: integer]: ISPBItem read getItems; default;
  end;

  { IServiceManager }

  IServiceManager = interface
    function getSPB: ISPB;
    procedure Attach;
    procedure Detach(Force: boolean=false);
    function IsAttached: boolean;
    function AllocateRequestBuffer(action: byte): IServiceRequest;
    procedure Start(Request: IServiceRequest);
    procedure StartMultiple(Requests: array of IServiceRequest);
    function Query(Request: IServiceRequest) :IServiceQueryResults;
    function QueryMultiple(Requests: array of IServiceRequest) :IServiceQueryResults;
  end;

  IFirebirdAPI = interface
    function GetStatus: IStatus;
    function AllocateDPB: IDPB;

    {Database connections}
    function OpenDatabase(DatabaseName: string; DPB: IDPB): IAttachment;
    function CreateDatabase(DatabaseName: string; SQLDialect: integer;
      CreateParams: string; DPB: IDPB): IAttachment;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction; {Start Transaction against multiple databases}

    {Service Manager}
    function AllocateSPB: ISPB;
    function GetServiceManager(ServerName: string; Protocol: TProtocol; SPB: ISPB): IServiceManager;

    {Information}
    function IsEmbeddedServer: boolean;
    function GetLibraryName: string;
    function HasServiceAPI: boolean;
    function GetImplementationVersion: string;
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

function FirebirdAPI: IFirebirdAPI;

function TryIBLoad: Boolean;
procedure CheckIBLoaded;

implementation

uses FBLibrary, FB25ClientAPI;

var FFirebirdAPI: IFirebirdAPI;

function FirebirdAPI: IFirebirdAPI;
begin
  if FFirebirdAPI = nil then
    CheckIBLoaded;
  Result := FFirebirdAPI;
end;

function TryIBLoad: Boolean;
var FBLibraryObj: TFBLibrary;
begin
  Result := FFirebirdAPI <> nil;
  if not Result then
  begin
    FBLibraryObj := TFB25ClientAPI.Create;
    FFirebirdAPI := TFB25ClientAPI(FBLibraryObj);
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
  FFirebirdAPI := nil;


end.

