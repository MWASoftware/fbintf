{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

unit IBHeader;

{$mode objfpc}{$H+}

interface

uses
  IBExternals, IB;


const
  ISC_TRUE = 1;
  ISC_FALSE = 0;
  DSQL_close = 1;
  DSQL_drop = 2;

const
  SQLDA_VERSION1	       = 1; (* pre V6.0 SQLDA *)
  SQLDA_VERSION2	       = 2; (*     V6.0 SQLDA *)
  SQL_DIALECT_V5	       = 1; (* meaning is same as DIALECT_xsqlda *)
  SQL_DIALECT_V6_TRANSITION    = 2; (* flagging anything that is delimited
                                       by double quotes as an error and
                                       flagging keyword DATE as an error *)
  SQL_DIALECT_V6	       = 3; (* supports SQL delimited identifier,
                                       SQLDATE/DATE, TIME, TIMESTAMP,
                                       CURRENT_DATE, CURRENT_TIME,
                                       CURRENT_TIMESTAMP, and 64-bit exact
                                       numeric type *)
  SQL_DIALECT_CURRENT	       = SQL_DIALECT_V6; (* latest IB DIALECT *)


type
  (**********************************)
  (** InterBase Handle Definitions **)
  (**********************************)
  TISC_ATT_HANDLE               = FB_API_HANDLE;
  PISC_ATT_HANDLE               = ^TISC_ATT_HANDLE;
  TISC_BLOB_HANDLE              = FB_API_HANDLE;
  PISC_BLOB_HANDLE              = ^TISC_BLOB_HANDLE;
  TISC_DB_HANDLE                = FB_API_HANDLE;
  PISC_DB_HANDLE                = ^TISC_DB_HANDLE;
  TISC_FORM_HANDLE              = FB_API_HANDLE;
  PISC_FORM_HANDLE              = ^TISC_FORM_HANDLE;
  TISC_REQ_HANDLE               = FB_API_HANDLE;
  PISC_REQ_HANDLE               = ^TISC_REQ_HANDLE;
  TISC_STMT_HANDLE              = FB_API_HANDLE;
  PISC_STMT_HANDLE              = ^TISC_STMT_HANDLE;
  TISC_SVC_HANDLE               = FB_API_HANDLE;
  PISC_SVC_HANDLE               = ^TISC_SVC_HANDLE;
  TISC_TR_HANDLE                = FB_API_HANDLE;
  PISC_TR_HANDLE                = ^TISC_TR_HANDLE;
  TISC_WIN_HANDLE               = FB_API_HANDLE;
  PISC_WIN_HANDLE               = ^TISC_WIN_HANDLE;
  TISC_CALLBACK                 = procedure;
  ISC_SVC_HANDLE               = ISC_LONG;

  (*******************************************************************)
  (* Time & Date Support                                             *)
  (*******************************************************************)
const
  TIME_SECONDS_PRECISION       = 10000;
  TIME_SECONDS_PRECISION_SCALE = -4;

type
  ISC_DATE = Long;
  PISC_DATE = ^ISC_DATE;
  ISC_TIME = ULong;
  PISC_TIME = ^ISC_TIME;
  TISC_TIMESTAMP = record
    timestamp_date: ISC_DATE;
    timestamp_time: ISC_TIME;
  end;
  PISC_TIMESTAMP = ^TISC_TIMESTAMP;

  TISC_ARRAY_BOUND = record
    array_bound_lower  : short;
    array_bound_upper  : short;
  end;
  PISC_ARRAY_BOUND     = ^TISC_ARRAY_BOUND;
  TISC_ARRAY_DESC = record
    array_desc_dtype            : UChar;
    array_desc_scale            : Char;
    array_desc_length           : UShort;
    array_desc_field_name       : array[0..31] of Char;
    array_desc_relation_name    : array[0..31] of Char;
    array_desc_dimensions       : Short;
    array_desc_flags            : Short;
    array_desc_bounds           : array[0..15] of TISC_ARRAY_BOUND;
  end; // TISC_ARRAY_DESC
  PISC_ARRAY_DESC = ^TISC_ARRAY_DESC;

  TISC_BLOB_DESC = record
    blob_desc_subtype           : Short;
    blob_desc_charset           : Short;
    blob_desc_segment_size      : Short;
    blob_desc_field_name        : array[0..31] of UChar;
    blob_desc_relation_name     : array[0..31] of UChar;
  end; // TISC_BLOB_DESC
  PISC_BLOB_DESC = ^TISC_BLOB_DESC;

  (*****************************)
  (** Blob control structure  **)
  (*****************************)
  TISC_BLOB_CTL_SOURCE_FUNCTION = function: ISC_STATUS; // ISC_FAR
  PISC_BLOB_CTL                 = ^TISC_BLOB_CTL;        // ISC_FAR
  TISC_BLOB_CTL = record
    (** Source filter **)
    ctl_source                  : TISC_BLOB_CTL_SOURCE_FUNCTION;
    (** Argument to pass to source filter **)
    ctl_source_handle           : PISC_BLOB_CTL;
    ctl_to_sub_type             : Short;  	(** Target type **)
    ctl_from_sub_type           : Short;	(** Source type **)
    ctl_buffer_length           : UShort;	(** Length of buffer **)
    ctl_segment_length          : UShort;  	(** Length of current segment **)
    ctl_bpb_length              : UShort;	(** Length of blob parameter **)
					    	(** block **)
    ctl_bpb                     : PChar;	(** Address of blob parameter **)
						(** block **)
    ctl_buffer                  : PUChar;	(** Address of segment buffer **)
    ctl_max_segment             : ISC_LONG;	(** Length of longest segment **)
    ctl_number_segments 	: ISC_LONG;     (** Total number of segments **)
    ctl_total_length            : ISC_LONG;  	(** Total length of blob **)
    ctl_status                  : PISC_STATUS;	(** Address of status vector **)
    ctl_data                    : array[0..7] of long; (** Application specific data **)
  end;
  (*****************************)
  (** Blob stream definitions **)
  (*****************************)
  TBSTREAM = record
    bstr_blob                   : PVoid;  	(** Blob handle **)
    bstr_buffer                 : PChar;	(** Address of buffer **)
    bstr_ptr                    : PChar;	(** Next character **)
    bstr_length                 : Short;	(** Length of buffer **)
    bstr_cnt                    : Short;	(** Characters in buffer **)
    bstr_mode                   : Char;  	(** (mode) ? OUTPUT : INPUT **)
  end;
  PBSTREAM                      = ^TBSTREAM;

  (*****************************)
  (** Dynamic SQL definitions **)
  (*****************************)
{$IFDEF IB5_ONLY}
  TSQLVAR = record
    sqltype                     : Short;
    sqllen                      : Short;
    sqldata                     : PChar;
    sqlind                      : PShort;
    sqlname_length              : Short;
    sqlname                     : array[0..29] of Char;
  end;
  PSQLVAR                       = ^TSQLVAR;

  TSQLDA = record
    sqldaid                     : array[0..7] of Char;
    sqldabc                     : ISC_LONG;
    sqln                        : Short;
    sqld                        : Short;
    sqlvar                      : array[0..0] of TSQLVAR;
  end;
  PSQLDA                         = ^TSQLDA;
{$ENDIF}

  (********************************)
  (** Declare the extended SQLDA **)
  (********************************)
  TXSQLVAR = record
    sqltype                     : Short;     (** datatype of field **)
    sqlscale                    : Short;     (** scale factor **)
    sqlsubtype                  : Short;     (** datatype subtype - BLOBs **)
					     (** & text types only **)
    sqllen                      : Short;     (** length of data area **)
    sqldata                     : PChar;     (** address of data **)
    sqlind                      : PShort;    (** address of indicator **)
                                             (** variable **)
    sqlname_length              : Short;     (** length of sqlname field **)
    (** name of field, name length + space for NULL **)
    sqlname                     : array[0..31] of Char;
    relname_length              : Short;     (** length of relation name **)
    (** field's relation name + space for NULL **)
    relname                     : array[0..31] of Char;
    ownname_length              : Short;     (** length of owner name **)
    (** relation's owner name + space for NULL **)
    ownname                     : array[0..31] of Char;
    aliasname_length            : Short;     (** length of alias name **)
    (** relation's alias name + space for NULL **)
    aliasname                   : array[0..31] of Char;
  end;  // TXSQLVAR
  PXSQLVAR                      = ^TXSQLVAR;

  TXSQLDA = record
    version                     : Short;     (** version of this XSQLDA **)
    (** XSQLDA name field **)
    sqldaid                     : array[0..7] of Char;
    sqldabc                     : ISC_LONG;  (** length in bytes of SQLDA **)
    sqln                        : Short;     (** number of fields allocated **)
    sqld                        : Short;     (** actual number of fields **)
    (** first field address **)
    sqlvar                      : array[0..0] of TXSQLVAR;
  end; // TXSQLDA
  PXSQLDA                       = ^TXSQLDA;

(********************************************************)
(** This record type is for passing arguments to       **)
(** isc_start_transaction (See docs)                   **)
(********************************************************)
  TISC_START_TRANS = record
    db_handle      : PISC_DB_HANDLE;
    tpb_length     : UShort;
    tpb_address    : PChar;
  end;

(********************************************************)
(** This record type is for passing arguments to       **)
(** isc_start_multiple (see docs)                      **)
(********************************************************)
  TISC_TEB = record
    db_handle      : PISC_DB_HANDLE;
    tpb_length     : Long;
    tpb_address    : PChar;
  end;
  PISC_TEB = ^TISC_TEB;
  TISC_TEB_ARRAY = array[0..0] of TISC_TEB;
  PISC_TEB_ARRAY = ^TISC_TEB_ARRAY;

(*****************************)
(** OSRI database functions **)
(*****************************)

Tisc_attach_database = function (status_vector            : PISC_STATUS;
                                 db_name_length           : Short;
                                 db_name                  : PChar;
                                 db_handle                : PISC_DB_HANDLE;
			         parm_buffer_length	  : Short;
                                 parm_buffer              : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_array_gen_sdl = function   (status_vector            : PISC_STATUS;
                                 isc_array_desc           : PISC_ARRAY_DESC;
                                 isc_arg3                 : PShort;
                                 isc_arg4                 : PChar;
                                 isc_arg5                 : PShort): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_array_get_slice = function (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 trans_handle             : PISC_TR_HANDLE;
				 array_id                 : PISC_QUAD;
				 descriptor               : PISC_ARRAY_DESC;
				 dest_array               : PVoid;
				 slice_length             : PISC_LONG): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_array_lookup_bounds = function (status_vector        : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 trans_handle             : PISC_TR_HANDLE;
				 table_name,
				 column_name              : PChar;
				 descriptor               : PISC_ARRAY_DESC): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_array_lookup_desc = function (status_vector          : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 trans_handle             : PISC_TR_HANDLE;
				 table_name,
				 column_name              : PChar;
				 descriptor               : PISC_ARRAY_DESC): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_array_set_desc = function  (status_vector            : PISC_STATUS;
				 table_name               : PChar;
				 column_name              : PChar;
				 sql_dtype,
                                 sql_length,
                                 sql_dimensions           : PShort;
                                 descriptor               : PISC_ARRAY_DESC): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_array_put_slice = function (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 trans_handle             : PISC_TR_HANDLE;
                                 array_id                 : PISC_QUAD;
                                 descriptor               : PISC_ARRAY_DESC;
                                 source_array             : PVoid;
                                 slice_length             : PISC_LONG): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_blob_default_desc = procedure  (descriptor           : PISC_BLOB_DESC;
                                 table_name               : PUChar;
                                 column_name              : PUChar);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_blob_gen_bpb = function    (status_vector            : PISC_STATUS;
				 to_descriptor,
                                 from_descriptor          : PISC_BLOB_DESC;
                                 bpb_buffer_length        : UShort;
                                 bpb_buffer               : PUChar;
                                 bpb_length               : PUShort): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_blob_info = function       (status_vector            : PISC_STATUS;
				 blob_handle              : PISC_BLOB_HANDLE;
				 item_list_buffer_length  : Short;
 				 item_list_buffer         : PChar;
				 result_buffer_length     : Short;
				 result_buffer            : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_blob_lookup_desc = function (status_vector           : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 trans_handle             : PISC_TR_HANDLE;
                                 table_name,
                                 column_name              : PChar;
                                 descriptor               : PISC_BLOB_DESC;
                                 global                   : PUChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_blob_set_desc = function   (status_vector            : PISC_STATUS;
                                 table_name,
                                 column_name              : PChar;
                                 subtype,
                                 charset,
                                 segment_size             : Short;
                                 descriptor               : PISC_BLOB_DESC): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_cancel_blob = function     (status_vector            : PISC_STATUS;
				 blob_handle              : PISC_BLOB_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_cancel_events = function   (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
				 event_id                 : PISC_LONG): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_close_blob = function      (status_vector            : PISC_STATUS;
                                 blob_handle              : PISC_BLOB_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_commit_retaining = function (status_vector           : PISC_STATUS;
				 tran_handle              : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_commit_transaction = function  (status_vector        : PISC_STATUS;
				 tran_handle              : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_create_blob = function     (status_vector            : PISC_STATUS;
				 db_handle                : PISC_DB_HANDLE;
				 tran_handle              : PISC_TR_HANDLE;
                                 blob_handle              : PISC_BLOB_HANDLE;
				 blob_id                  : PISC_QUAD): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_create_blob2 = function    (status_vector            : PISC_STATUS;
				 db_handle                : PISC_DB_HANDLE;
				 tran_handle              : PISC_TR_HANDLE;
                                 blob_handle              : PISC_BLOB_HANDLE;
                                 blob_id                  : PISC_QUAD;
				 bpb_length               : Short;
				 bpb_address              : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_create_database = function (status_vector            : PISC_STATUS;
				 isc_arg2                 : Short;
				 isc_arg3                 : PChar;
				 db_handle                : PISC_DB_HANDLE;
				 isc_arg5	          : Short;
				 isc_arg6                 : PChar;
				 isc_arg7                 : Short): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_database_info = function   (status_vector            : PISC_STATUS;
				 db_handle                : PISC_DB_HANDLE;
                                 item_list_buffer_length  : Short;
				 item_list_buffer         : PChar;
                                 result_buffer_length     : Short;
                                 result_buffer            : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_decode_date = procedure    (ib_date: PISC_QUAD;
                                 tm_date: PCTimeStructure);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_decode_sql_date = procedure (ib_date: PISC_DATE;
                                 tm_date: PCTimeStructure);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_decode_sql_time = procedure  (ib_time: PISC_TIME;
                                 tm_date: PCTimeStructure);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_decode_timestamp = procedure  (ib_timestamp: PISC_TIMESTAMP;
                                 tm_date: PCTimeStructure);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_detach_database = function (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_drop_database = function   (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_allocate_statement = function (status_vector    : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
				 stmt_handle              : PISC_STMT_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_alloc_statement2 = function (status_vector      : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
				 stmt_handle              : PISC_STMT_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_describe = function   (status_vector            : PISC_STATUS;
				 stmt_handle              : PISC_STMT_HANDLE;
                                 dialect                  : UShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_describe_bind = function  (status_vector        : PISC_STATUS;
				 stmt_handle              : PISC_STMT_HANDLE;
                                 dialect                  : UShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_exec_immed2 = function (status_vector           : PISC_STATUS;
				 db_handle                : PISC_DB_HANDLE;
				 tran_handle              : PISC_TR_HANDLE;
				 length                   : UShort;
				 statement                : PChar;
				 dialect                  : UShort;
                                 in_xsqlda,
				 out_xsqlda               : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_execute = function    (status_vector            : PISC_STATUS;
				 tran_handle              : PISC_TR_HANDLE;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 dialect                  : UShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_execute2 = function   (status_vector            : PISC_STATUS;
				 tran_handle              : PISC_TR_HANDLE;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 dialect                  : UShort;
                                 in_xsqlda,
                                 out_xsqlda               : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_execute_immediate = function (status_vector     : PISC_STATUS;
				 db_handle                : PISC_DB_HANDLE;
				 tran_handle              : PISC_TR_HANDLE;
				 length                   : UShort;
				 statement                : PChar;
				 dialect                  : UShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_fetch = function      (status_vector            : PISC_STATUS;
                                 stmt_handle              : PISC_STMT_HANDLE;
				 dialect                  : UShort;
				 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(*$ifdef SCROLLABLE_CURSORS*)
Tisc_dsql_fetch2 = function     (status_vector            : PISC_STATUS;
                                 stmt_handle              : PISC_STMT_HANDLE;
				 dialect                  : UShort;
				 xsqlda                   : PXSQLDA;
				 isc_arg5                 : UShort;
				 isc_arg6                 : Long): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
(*$endif*)

Tisc_dsql_finish = function    (db_handle                : PISC_DB_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_free_statement = function (status_vector        : PISC_STATUS;
                                 stmt_handle              : PISC_STMT_HANDLE;
				 options                  : UShort): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_insert = function     (status_vector            : PISC_STATUS;
                                 stmt_handle              : PISC_STMT_HANDLE;
				 arg3                     : UShort;
				 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_prepare = function    (status_vector            : PISC_STATUS;
                                 tran_handle              : PISC_TR_HANDLE;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 length                   : UShort;
                                 statement                : PChar;
                                 dialect                  : UShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_set_cursor_name = function (status_vector        : PISC_STATUS;
				 stmt_handle               : PISC_STMT_HANDLE;
                                 cursor_name               : PChar;
                                 _type                     : UShort): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_sql_info = function   (status_vector             : PISC_STATUS;
                                 stmt_handle               : PISC_STMT_HANDLE;
				 item_length               : Short;
                                 items                     : PChar;
                                 buffer_length             : Short;
                                 buffer                    : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_encode_date = procedure    (tm_date                    : PCTimeStructure;
				 ib_date                    : PISC_QUAD);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_encode_sql_date = procedure (tm_date                   : PCTimeStructure;
				 ib_date                    : PISC_DATE);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_encode_sql_time = procedure (tm_date                   : PCTimeStructure;
				 ib_time                    : PISC_TIME);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_encode_timestamp = procedure (tm_date                  : PCTimeStructure;
				 ib_timestamp               : PISC_TIMESTAMP);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_event_block = function     (event_buffer               : PPChar;
				 result_buffer              : PPChar;
				 id_count                   : UShort):ISC_LONG;
                                 varargs; cdecl;

Tisc_event_counts = procedure   (status_vector             : PISC_LONG;
				 buffer_length             : Short;
				 event_buffer              : PChar;
				 result_buffer             : PChar);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_expand_dpb = procedure     (dpb                       : PPChar;
				 dpb_length                : PShort;
				 item_list                 : array of Pointer);
                                cdecl;

Tisc_modify_dpb = function      (dpb                       : PPChar;
				 isc_arg2,
                                 isc_arg3                  : PShort;
                                 isc_arg4                  : UShort;
				 isc_arg5                  : PChar;
                                 isc_arg6                  : Short): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_free = function           (isc_arg1                  : PChar): ISC_LONG;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_get_segment = function     (status_vector             : PISC_STATUS;
				 blob_handle               : PISC_BLOB_HANDLE;
                                 actual_seg_length         : PUShort;
                                 seg_buffer_length         : UShort;
				 seg_buffer                : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_get_slice = function       (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PISC_QUAD;
                                 isc_arg5                  : Short;
				 isc_arg6                  : PChar;
				 isc_arg7                  : Short;
				 isc_arg8                  : PISC_LONG;
				 isc_arg9                  : ISC_LONG;
				 isc_arg10                 : PVoid;
				 isc_arg11                 : PISC_LONG): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_interprete = function      (buffer                    : PChar;
				 status_vector             : PPISC_STATUS): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_open_blob = function       (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
				 blob_handle               : PISC_BLOB_HANDLE;
				 blob_id                   : PISC_QUAD): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_open_blob2 = function      (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
				 blob_handle               : PISC_BLOB_HANDLE;
				 blob_id                   : PISC_QUAD;
				 bpb_length                : Short;
				 bpb_buffer                : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_prepare_transaction2 = function (status_vector        : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 msg_length                : Short;
				 msg                       : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_print_sqlerror = procedure (sqlcode                   : Short;
				 status_vector             : PISC_STATUS);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_print_status = function   (status_vector              : PISC_STATUS): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_put_segment = function     (status_vector             : PISC_STATUS;
				 blob_handle               : PISC_BLOB_HANDLE;
				 seg_buffer_len            : UShort;
				 seg_buffer                : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_put_slice = function       (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
				 isc_arg4                  : PISC_QUAD;
				 isc_arg5                  : Short;
				 isc_arg6                  : PChar;
				 isc_arg7                  : Short;
				 isc_arg8                  : PISC_LONG;
				 isc_arg9                  : ISC_LONG;
				 isc_arg10                 : PVoid): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_que_events = function      (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
				 event_id                  : PISC_LONG;
				 length                    : Short;
				 event_buffer              : PChar;
                                 event_function            : TISC_CALLBACK;
				 event_function_arg        : PVoid): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_rollback_retaining = function (status_vector         : PISC_STATUS;
				 tran_handle              : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_rollback_transaction = function (status_vector        : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_start_multiple = function  (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 db_handle_count           : Short;
				 teb_vector_address        : PISC_TEB): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_start_transaction = function (status_vector           : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 db_handle_count           : Short;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tpb_length                : UShort;
                                 tpb_address               : PChar): ISC_STATUS;
                                cdecl;

Tisc_sqlcode = function        (status_vector             : PISC_STATUS): ISC_LONG;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}


Tisc_sql_interprete = procedure (sqlcode                   : Short;
				 buffer                    : PChar;
                                 buffer_length             : Short);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_transaction_info = function (status_vector            : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 item_list_buffer_length   : Short;
                                 item_list_buffer          : PChar;
                                 result_buffer_length      : Short;
                                 result_buffer             : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_transact_request = function (status_vector            : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
				 isc_arg4                  : UShort;
                                 isc_arg5                  : PChar;
				 isc_arg6	           : UShort;
				 isc_arg7                  : PChar;
                                 isc_arg8                  : UShort;
				 isc_arg9                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_vax_integer = function     (buffer                    : PChar;
				 length                    : Short): ISC_LONG;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_portable_integer = function (buffer                   : PChar;
				 length                    : Short): ISC_INT64;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(***************************************)
(** Security Functions and structures **)
(***************************************)

const
  sec_uid_spec		                = $01;
  sec_gid_spec		                = $02;
  sec_server_spec		        = $04;
  sec_password_spec	                = $08;
  sec_group_name_spec	                = $10;
  sec_first_name_spec	                = $20;
  sec_middle_name_spec                  = $40;
  sec_last_name_spec	                = $80;
  sec_dba_user_name_spec                = $100;
  sec_dba_password_spec                 = $200;

  sec_protocol_tcpip                    = 1;
  sec_protocol_netbeui                  = 2;
  sec_protocol_spx                      = 3;
  sec_protocol_local                    = 4;

type
  TUserSecData = record
    sec_flags: Short;		     (** which fields are specified **)
    uid: Int;			     (** the user's id **)
    gid: int;			     (** the user's group id **)
    protocol: Int;		     (** protocol to use for connection **)
    server: PChar;                   (** server to administer **)
    user_name: PChar;                (** the user's name **)
    password: PChar;                 (** the user's password **)
    group_name: PChar;               (** the group name **)
    first_name: PChar;	             (** the user's first name **)
    middle_name: PChar;              (** the user's middle name **)
    last_name: PChar;	             (** the user's last name **)
    dba_user_name: PChar;            (** the dba user name **)
    dba_password: PChar;             (** the dba password **)
  end;
  PUserSecData = ^TUserSecData;

Tisc_add_user = function        (status_vector             : PISC_STATUS;
                                 user_sec_data             : PUserSecData): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_delete_user = function     (status_vector             : PISC_STATUS;
                                 user_sec_data             : PUserSecData): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_modify_user = function     (status_vector             : PISC_STATUS;
                                 user_sec_data             : PUserSecData): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(************************************)
(**  Other OSRI functions          **)
(************************************)

Tisc_compile_request = function (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
				 isc_arg4                  : Short;
				 isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_compile_request2 = function (status_vector            : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
				 isc_arg4                  : Short;
				 isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_ddl = function             (status_vector             : PISC_STATUS;
			         db_handle                 : PISC_DB_HANDLE;
			         tran_handle               : PISC_TR_HANDLE;
			         isc_arg4                  : Short;
			         isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_prepare_transaction = function (status_vector         : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}


Tisc_receive = function         (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
				 isc_arg3,
                                 isc_arg4                  : Short;
				 isc_arg5                  : PVoid;
				 isc_arg6                  : Short): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_receive2 = function        (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
				 isc_arg3,
                                 isc_arg4                  : Short;
				 isc_arg5                  : PVoid;
				 isc_arg6,
                                 isc_arg7                  : Short;
                                 isc_arg8                  : Long): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_reconnect_transaction = function (status_vector       : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : Short;
                                 isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_release_request = function (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_request_info = function    (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg3                  : Short;
                                 isc_arg4                  : Short;
                                 isc_arg5                  : PChar;
                                 isc_arg6                  : Short;
                                 isc_arg7                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_seek_blob = function       (status_vector             : PISC_STATUS;
                                 blob_handle               : PISC_BLOB_HANDLE;
                                 isc_arg3                  : Short;
                                 isc_arg4                  : ISC_LONG;
                                 isc_arg5                  : PISC_LONG): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_send = function            (status_vector             : PISC_STATUS;
				 request_handle            : PISC_REQ_HANDLE;
				 isc_arg3,
                                 isc_arg4                  : Short;
				 isc_arg5                  : PVoid;
				 isc_arg6                  : Short): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_start_and_send = function  (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
				 tran_handle               : PISC_TR_HANDLE;
				 isc_arg4,
                                 isc_arg5                  : Short;
                                 isc_arg6                  : PVoid;
                                 isc_arg7                  : Short): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_start_request = function   (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : Short): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_unwind_request = function  (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : Short): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_wait_for_event = function  (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 length                    : Short;
                                 event_buffer,
                                 result_buffer             : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(*******************************)
(** Other Sql functions       **)
(*******************************)
{$IFDEF IB5_ONLY}
Tisc_close = function           (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_declare = function         (status_vector             : PISC_STATUS;
                                 isc_arg2,
                                 isc_arg3                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_describe = function        (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PChar;
                                 isc_arg3                  : PSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_describe_bind = function   (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PChar;
                                 isc_arg3                  : PSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_execute = function         (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PChar;
                                 isc_arg4                  : PSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_execute_immediate = function (status_vector           : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PShort;
                                 isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_fetch = function           (status_vector             : PISC_STATUS;
				 isc_arg2                  : PChar;
				 isc_arg3                  : PSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_open = function            (status_vector             : PISC_STATUS;
				 tran_handle               : PISC_TR_HANDLE;
				 isc_arg3                  : PChar;
				 isc_arg4                  : PSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_prepare = function         (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PChar;
                                 isc_arg5                  : PShort;
                                 isc_arg6                  : PChar;
                                 isc_arg7                  : PSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
{$ELSE}
Tisc_close = function           (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_declare = function         (status_vector             : PISC_STATUS;
                                 isc_arg2,
                                 isc_arg3                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_describe = function        (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PChar;
                                 isc_arg3                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_describe_bind = function   (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PChar;
                                 isc_arg3                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_execute = function         (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PChar;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_execute_immediate = function (status_vector           : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PShort;
                                 isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_fetch = function           (status_vector             : PISC_STATUS;
				 isc_arg2                  : PChar;
				 isc_arg3                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_open = function            (status_vector             : PISC_STATUS;
				 tran_handle               : PISC_TR_HANDLE;
				 isc_arg3                  : PChar;
				 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_prepare = function         (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PChar;
                                 isc_arg5                  : PShort;
                                 isc_arg6                  : PChar;
                                 isc_arg7                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
{$ENDIF}

(***************************************)
(** Other Dynamic sql functions       **)
(***************************************)

Tisc_dsql_execute_m = function  (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PChar;
                                 isc_arg6                  : UShort;
                                 isc_arg7                  : UShort;
                                 isc_arg8                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_execute2_m = function (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PChar;
                                 isc_arg6                  : UShort;
                                 isc_arg7                  : UShort;
                                 isc_arg8                  : PChar;
                                 isc_arg9                  : UShort;
                                 isc_arg10                 : PChar;
                                 isc_arg11                 : UShort;
                                 isc_arg12                 : UShort;
                                 isc_arg13                 : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_execute_immediate_m = function (status_vector    : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PChar;
                                 isc_arg6                  : UShort;
                                 isc_arg7                  : UShort;
                                 isc_arg8                  : PChar;
                                 isc_arg9                  : UShort;
                                 isc_arg10                 : UShort;
                                 isc_arg11                 : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_exec_immed3_m = function  (status_vector         : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PChar;
                                 isc_arg6                  : UShort;
                                 isc_arg7                  : UShort;
                                 isc_arg8                  : PChar;
                                 isc_arg9                  : UShort;
                                 isc_arg10                 : UShort;
                                 isc_arg11                 : PChar;
                                 isc_arg12                 : UShort;
                                 isc_arg13                 : PChar;
                                 isc_arg14                 : UShort;
                                 isc_arg15                 : UShort;
                                 isc_arg16                 : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_fetch_m = function    (status_vector             : PISC_STATUS;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg3                  : UShort;
                                 isc_arg4                  : PChar;
                                 isc_arg5                  : UShort;
                                 isc_arg6                  : UShort;
                                 isc_arg7                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(*$ifdef SCROLLABLE_CURSORS*)
Tisc_dsql_fetch2_m = function   (status_vector             : PISC_STATUS;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg3                  : UShort;
                                 isc_arg4                  : PChar;
                                 isc_arg5                  : UShort;
                                 isc_arg6                  : UShort;
                                 isc_arg7                  : PChar;
                                 isc_arg8                  : UShort;
                                 isc_arg9                  : Long): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
(*$endif*)

Tisc_dsql_insert_m = function   (status_vector             : PISC_STATUS;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg3                  : UShort;
                                 isc_arg4                  : PChar;
                                 isc_arg5                  : UShort;
                                 isc_arg6                  : UShort;
                                 isc_arg7                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_prepare_m = function  (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PChar;
                                 isc_arg6                  : UShort;
                                 isc_arg7                  : UShort;
                                 isc_arg8                  : PChar;
                                 isc_arg9                  : UShort;
                                 isc_arg10                 : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_dsql_release = function    (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_close = function(status_vector             : PISC_STATUS;
                                 isc_arg2                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_declare = function  (status_vector         : PISC_STATUS;
                                 isc_arg2                  : PChar;
                                 isc_arg3                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_describe = function (status_vector         : PISC_STATUS;
                                 isc_arg2                  : PChar;
                                 isc_arg3                  : UShort;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_describe_bind = function (status_vector    : PISC_STATUS;
				 isc_arg2                  : PChar;
                                 isc_arg3                  : UShort;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_execute = function  (status_vector         : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PChar;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_execute2 = function (status_vector         : PISC_STATUS;
				 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PChar;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PXSQLDA;
                                 isc_arg6                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_execute_immed = function (status_vector    : PISC_STATUS;
				 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PChar;
                                 isc_arg6                  : UShort;
                                 isc_arg7                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_fetch = function(status_vector             : PISC_STATUS;
                                 isc_arg2                  : PChar;
                                 isc_arg3                  : UShort;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(*$ifdef SCROLLABLE_CURSORS*)
Tisc_embed_dsql_fetch2 = function  (status_vector         : PISC_STATUS;
                                isc_arg2                  : PChar;
                                isc_arg3                  : UShort;
                                isc_arg4                  : PXSQLDA;
                                isc_arg5                  : UShort;
                                isc_arg6                  : Long): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
(*$endif*)

Tisc_embed_dsql_open = function (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PChar;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_open2 = function (status_vector            : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PChar;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PXSQLDA;
                                 isc_arg6                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_insert = function (status_vector           : PISC_STATUS;
                                 isc_arg2                  : PChar;
                                 isc_arg3                  : UShort;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_prepare = function  (status_vector         : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PChar;
                                 isc_arg5                  : UShort;
                                 isc_arg6                  : PChar;
                                 isc_arg7                  : UShort;
                                 isc_arg8                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_embed_dsql_release = function  (status_vector         : PISC_STATUS;
                                 isc_arg2                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(********************************)
(** Other Blob functions       **)
(********************************)

TBLOB_open = function           (blob_handle               : TISC_BLOB_HANDLE;
                                 isc_arg2                  : PChar;
                                 isc_arg3                  : int): PBSTREAM;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBLOB_put = function            (isc_arg1                  : char;
				 isc_arg2                  : PBSTREAM): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBLOB_close = function         (isc_arg1                  : PBSTREAM): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBLOB_get = function           (isc_arg1                  : PBSTREAM): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBLOB_display = function        (isc_arg1                  : PISC_QUAD;
                                 db_handle                 : TISC_DB_HANDLE;
                                 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PChar): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBLOB_dump = function           (isc_arg1                  : PISC_QUAD;
				 db_handle                 : TISC_DB_HANDLE;
				 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PChar): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBLOB_edit = function           (isc_arg1                  : PISC_QUAD;
				 db_handle                 : TISC_DB_HANDLE;
				 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PChar): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBLOB_load = function           (isc_arg1                  : PISC_QUAD;
				 db_handle                 : TISC_DB_HANDLE;
				 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PChar): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBLOB_text_dump = function      (isc_arg1                  : PISC_QUAD;
				 db_handle                 : TISC_DB_HANDLE;
				 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PChar): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBLOB_text_load = function      (isc_arg1                  : PISC_QUAD;
				 db_handle                 : TISC_DB_HANDLE;
				 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PChar): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBopen = function               (isc_arg1                  : PISC_QUAD;
				 db_handle                 : TISC_DB_HANDLE;
				 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PChar): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

TBopen2 = function              (isc_arg1                  : PISC_QUAD;
				 db_handle                 : TISC_DB_HANDLE;
				 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PChar;
                                 isc_arg5                  : UShort): PBSTREAM;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(********************************)
(** Other Misc functions       **)
(********************************)

Tisc_ftof = function            (isc_arg1                  : PChar;
				 isc_arg2                  : UShort;
				 isc_arg3                  : PChar;
				 isc_arg4                  : UShort): ISC_LONG;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_print_blr = function       (isc_arg1                  : PChar;
                                 isc_arg2                  : TISC_CALLBACK;
                                 isc_arg3                  : PVoid;
                                 isc_arg4                  : Short): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_set_debug = procedure     (isc_arg1                  : Int);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_qtoq = procedure           (isc_arg1                  : PISC_QUAD;
				 isc_arg2                  : PISC_QUAD);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_vtof = procedure           (isc_arg1                  : PChar;
				 isc_arg2                  : PChar;
				 isc_arg3                  : UShort);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_vtov = procedure           (isc_arg1                  : PChar;
				 isc_arg2                  : PChar;
				 isc_arg3                  : Short);
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_version = function         (db_handle                 : PISC_DB_HANDLE;
                                 isc_arg2                  : TISC_CALLBACK;
                                 isc_arg3                  : PVoid): Int;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_reset_fpe = function      (isc_arg1                  : UShort): ISC_LONG;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(*******************************************)
(** Service manager functions             **)
(*******************************************)

Tisc_service_attach = function  (status_vector             : PISC_STATUS;
                                 isc_arg2                  : UShort;
                                 isc_arg3                  : PChar;
                                 service_handle            : PISC_SVC_HANDLE;
                                 isc_arg5                  : UShort;
                                 isc_arg6                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_service_detach = function (status_vector             : PISC_STATUS;
                                service_handle            : PISC_SVC_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_service_query = function   (status_vector             : PISC_STATUS;
                                 service_handle            : PISC_SVC_HANDLE;
                                 recv_handle               : PISC_SVC_HANDLE;
                                 isc_arg4                  : UShort;
                                 isc_arg5                  : PChar;
                                 isc_arg6                  : UShort;
                                 isc_arg7                  : PChar;
                                 isc_arg8                  : UShort;
                                 isc_arg9                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_service_start = function  (status_vector             : PISC_STATUS;
                                service_handle            : PISC_SVC_HANDLE;
                                recv_handle               : PISC_SVC_HANDLE;
                                isc_arg4                  : UShort;
                                isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(*********************************)
(** Forms functions             **)
(*********************************)

Tisc_compile_map = function     (status_vector             : PISC_STATUS;
                                 form_handle               : PISC_FORM_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg4                  : PShort;
                                 isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_compile_menu = function    (status_vector             : PISC_STATUS;
                                 form_handle               : PISC_FORM_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg4                  : PShort;
                                 isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_compile_sub_map = function (status_vector             : PISC_STATUS;
                                 win_handle                : PISC_WIN_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg4                  : PShort;
                                 isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_create_window = function   (status_vector             : PISC_STATUS;
                                 win_handle                : PISC_WIN_HANDLE;
                                 isc_arg3                  : PShort;
                                 isc_arg4                  : PChar;
                                 isc_arg5                  : PShort;
                                 isc_arg6                  : PShort): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_delete_window = function   (status_vector             : PISC_STATUS;
                                 win_handle                : PISC_WIN_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_drive_form = function      (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 win_handle                : PISC_WIN_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg6                  : PUChar;
                                 isc_arg7                  : PUChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_drive_menu = function      (status_vector             : PISC_STATUS;
                                 win_handle                : PISC_WIN_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg4                  : PShort;
                                 isc_arg5                  : PChar;
                                 isc_arg6                  : PShort;
                                 isc_arg7                  : PChar;
                                 isc_arg8                  : PShort;
                                 isc_arg9                  : PShort;
                                 isc_arg10                 : PChar;
                                 isc_arg11                 : PISC_LONG): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_form_delete = function     (status_vector             : PISC_STATUS;
                                 form_handle               : PISC_FORM_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_form_fetch = function      (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg5                  : PUChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_form_insert = function     (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg5                  : PUChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_get_entree = function      (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg3                  : PShort;
                                 isc_arg4                  : PChar;
                                 isc_arg5                  : PISC_LONG;
                                 isc_arg6                  : PShort): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_initialize_menu = function (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_menu = function            (status_vector             : PISC_STATUS;
				 win_handle                : PISC_WIN_HANDLE;
				 request_handle            : PISC_REQ_HANDLE;
			 	 isc_arg4                  : PShort;
				 isc_arg5                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_load_form = function       (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 form_handle               : PISC_FORM_HANDLE;
                                 isc_arg5                  : PShort;
                                 isc_arg6                  : PChar): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_pop_window = function      (status_vector             : PISC_STATUS;
                                 win_handle                : PISC_WIN_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_put_entree = function      (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg3                  : PShort;
                                 isc_arg4                  : PChar;
                                 isc_arg5                  : PISC_LONG): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_reset_form = function      (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

Tisc_suspend_window = function  (status_vector             : PISC_STATUS;
                                 win_handle                : PISC_WIN_HANDLE): ISC_STATUS;
                                {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

(** Constants!!! **)
(*****************************************************)
(** Actions to pass to the blob filter (ctl_source) **)
(*****************************************************)

const
  isc_blob_filter_open           =          0;
  isc_blob_filter_get_segment    =          1;
  isc_blob_filter_close          =          2;
  isc_blob_filter_create         =          3;
  isc_blob_filter_put_segment    =          4;
  isc_blob_filter_alloc          =          5;
  isc_blob_filter_free           =          6;
  isc_blob_filter_seek           =          7;

(*********************)
(** Blr definitions **)
(*********************)

  // In pascal, how does one deal with the below "#define"?
  // blr_word(n) ((n) % 256), ((n) / 256)
  blr_text                       =         14;
  blr_text2                      =         15;
  blr_short                      =          7;
  blr_long                       =          8;
  blr_quad                       =          9;
  blr_float                      =         10;
  blr_double                     =         27;
  blr_d_float                    =         11;
  blr_timestamp                  =         35;
  blr_varying                    =         37;
  blr_varying2                   =         38;
  blr_blob                       =        261;
  blr_cstring                    =         40;
  blr_cstring2                   =         41;
  blr_blob_id                    =         45;
  blr_sql_date                   =         12;
  blr_sql_time                   =         13;
  blr_int64                      =         16;
  blr_date                       =         blr_timestamp;


  blr_inner                      =          0;
  blr_left                       =          1;
  blr_right                      =          2;
  blr_full                       =          3;

  blr_gds_code                   =          0;
  blr_sql_code                   =          1;
  blr_exception                  =          2;
  blr_trigger_code               =          3;
  blr_default_code               =          4;

  blr_version4                   =          4;
  blr_version5                   =          5;
  blr_eoc                        =         76;
  blr_end                        =         -1;

  blr_assignment                 =          1;
  blr_begin                      =          2;
  blr_dcl_variable               =          3;
  blr_message                    =          4;
  blr_erase                      =          5;
  blr_fetch                      =          6;
  blr_for                        =          7;
  blr_if                         =          8;
  blr_loop                       =          9;
  blr_modify                     =         10;
  blr_handler                    =         11;
  blr_receive                    =         12;
  blr_select                     =         13;
  blr_send                       =         14;
  blr_store                      =         15;
  blr_label                      =         17;
  blr_leave                      =         18;
  blr_store2                     =         19;
  blr_post                       =         20;

  blr_literal                    =         21;
  blr_dbkey                      =         22;
  blr_field                      =         23;
  blr_fid                        =         24;
  blr_parameter                  =         25;
  blr_variable                   =         26;
  blr_average                    =         27;
  blr_count                      =         28;
  blr_maximum                    =         29;
  blr_minimum                    =         30;
  blr_total                      =         31;
  blr_add                        =         34;
  blr_subtract                   =         35;
  blr_multiply                   =         36;
  blr_divide                     =         37;
  blr_negate                     =         38;
  blr_concatenate                =         39;
  blr_substring                  =         40;
  blr_parameter2                 =         41;
  blr_from                       =         42;
  blr_via                        =         43;
  blr_user_name                  =         44;
  blr_null                       =         45;

  blr_eql                        =         47;
  blr_neq                        =         48;
  blr_gtr                        =         49;
  blr_geq                        =         50;
  blr_lss                        =         51;
  blr_leq                        =         52;
  blr_containing                 =         53;
  blr_matching                   =         54;
  blr_starting                   =         55;
  blr_between                    =         56;
  blr_or                         =         57;
  blr_and                        =         58;
  blr_not                        =         59;
  blr_any                        =         60;
  blr_missing                    =         61;
  blr_unique                     =         62;
  blr_like                       =         63;

  blr_stream                     =         65;
  blr_set_index                  =         66;
  blr_rse                        =         67;
  blr_first                      =         68;
  blr_project                    =         69;
  blr_sort                       =         70;
  blr_boolean                    =         71;
  blr_ascending                  =         72;
  blr_descending                 =         73;
  blr_relation                   =         74;
  blr_rid                        =         75;
  blr_union                      =         76;
  blr_map                        =         77;
  blr_group_by                   =         78;
  blr_aggregate                  =         79;
  blr_join_type                  =         80;

  blr_agg_count                  =         83;
  blr_agg_max                    =         84;
  blr_agg_min                    =         85;
  blr_agg_total                  =         86;
  blr_agg_average                =         87;
  blr_parameter3                 =         88;
  blr_run_count                  =        118;
  blr_run_max                    =         89;
  blr_run_min                    =         90;
  blr_run_total                  =         91;
  blr_run_average                =         92;
  blr_agg_count2                 =         93;
  blr_agg_count_distinct         =         94;
  blr_agg_total_distinct         =         95;
  blr_agg_average_distinct       =         96;

  blr_function                   =        100;
  blr_gen_id                     =        101;
  blr_prot_mask                  =        102;
  blr_upcase                     =        103;
  blr_lock_state                 =        104;
  blr_value_if                   =        105;
  blr_matching2                  =        106;
  blr_index                      =        107;
  blr_ansi_like                  =        108;
  blr_bookmark                   =        109;
  blr_crack                      =        110;
  blr_force_crack                =        111;
  blr_seek                       =        112;
  blr_find                       =        113;

  blr_continue                   =          0;
  blr_forward                    =          1;
  blr_backward                   =          2;
  blr_bof_forward                =          3;
  blr_eof_backward               =          4;

  blr_lock_relation              =        114;
  blr_lock_record                =        115;
  blr_set_bookmark               =        116;
  blr_get_bookmark               =        117;
  blr_rs_stream                  =        119;
  blr_exec_proc                  =        120;
  blr_begin_range                =        121;
  blr_end_range                  =        122;
  blr_delete_range               =        123;
  blr_procedure                  =        124;
  blr_pid                        =        125;
  blr_exec_pid                   =        126;
  blr_singular                   =        127;
  blr_abort                      =        128;
  blr_block                      =        129;
  blr_error_handler              =        130;
  blr_cast                       =        131;
  blr_release_lock               =        132;
  blr_release_locks              =        133;
  blr_start_savepoint            =        134;
  blr_end_savepoint              =        135;
  blr_find_dbkey                 =        136;
  blr_range_relation             =        137;
  blr_delete_ranges              =        138;

  blr_plan                       =        139;
  blr_merge                      =        140;
  blr_join                       =        141;
  blr_sequential                 =        142;
  blr_navigational               =        143;
  blr_indices                    =        144;
  blr_retrieve                   =        145;

  blr_relation2                  =        146;
  blr_rid2                       =        147;
  blr_reset_stream               =        148;
  blr_release_bookmark           =        149;
  blr_set_generator              =        150;
  blr_ansi_any                   =        151;
  blr_exists                     =        152;
  blr_cardinality                =        153;

  blr_record_version             =        154;		(** get tid of record **)
  blr_stall                      =        155;		(** fake server stall **)
  blr_seek_no_warn               =        156;
  blr_find_dbkey_version         =        157;
  blr_ansi_all                   =        158;

  blr_extract                    = 159;

  (* sub parameters for blr_extract *)

  blr_extract_year               = 0;
  blr_extract_month              = 1;
  blr_extract_day	         = 2;
  blr_extract_hour               = 3;
  blr_extract_minute             = 4;
  blr_extract_second             = 5;
  blr_extract_weekday            = 6;
  blr_extract_yearday            = 7;

  blr_current_date               = 160;
  blr_current_timestamp          = 161;
  blr_current_time               = 162;

  (* These verbs were added in 6.0,
  primarily to support 64-bit integers *)

  blr_add2	            = 163;
  blr_subtract2	            = 164;
  blr_multiply2             = 165;
  blr_divide2	            = 166;
  blr_agg_total2            = 167;
  blr_agg_total_distinct2   = 168;
  blr_agg_average2          = 169;
  blr_agg_average_distinct2 = 170;
  blr_average2		    = 171;
  blr_gen_id2		    = 172;
  blr_set_generator2        = 173;

(****************************************)
(** Bit assignments in RDB$SYSTEM_FLAG **)
(****************************************)

  RDB_system                     =          1;
  RDB_id_assigned                =          2;



(**************************)
(** Blob Parameter Block **)
(**************************)

  isc_bpb_version1               =          1;
  isc_bpb_source_type            =          1;
  isc_bpb_target_type            =          2;
  isc_bpb_type                   =          3;
  isc_bpb_source_interp          =          4;
  isc_bpb_target_interp          =          5;
  isc_bpb_filter_parameter       =          6;

  isc_bpb_type_segmented         =          0;
  isc_bpb_type_stream            =          1;


(***********************************)
(** Service parameter block stuff **)
(***********************************)

  isc_spb_user_name              =          1;
  isc_spb_sys_user_name          =          2;
  isc_spb_sys_user_name_enc      =          3;
  isc_spb_password               =          4;
  isc_spb_password_enc           =          5;
  isc_spb_command_line           =          6;
  isc_spb_dbname                 =          7;
  isc_spb_verbose                =          8;
  isc_spb_options                =          9;
  isc_spb_connect_timeout        =          10;
  isc_spb_dummy_packet_interval  =          11;
  isc_spb_sql_role_name          =          12;
  isc_spb_last_spb_constant      =          isc_spb_sql_role_name;

  isc_spb_version1                                = 1;
  isc_spb_current_version                         = 2;
  isc_spb_version		                  = isc_spb_current_version;
  isc_spb_user_name_mapped_to_server              = isc_dpb_user_name;
  isc_spb_sys_user_name_mapped_to_server          = isc_dpb_sys_user_name;
  isc_spb_sys_user_name_enc_mapped_to_server      = isc_dpb_sys_user_name_enc;
  isc_spb_password_mapped_to_server               = isc_dpb_password;
  isc_spb_password_enc_mapped_to_server           = isc_dpb_password_enc;
  isc_spb_command_line_mapped_to_server           = 105;
  isc_spb_dbname_mapped_to_server                 = 106;
  isc_spb_verbose_mapped_to_server                = 107;
  isc_spb_options_mapped_to_server                = 108;
  isc_spb_connect_timeout_mapped_to_server        = isc_dpb_connect_timeout;
  isc_spb_dummy_packet_interval_mapped_to_server  = isc_dpb_dummy_packet_interval;
  isc_spb_sql_role_name_mapped_to_server          = isc_dpb_sql_role_name;

(***********************************)
(** Information call declarations **)
(***********************************)

(******************************)
(** Common, structural codes **)
(******************************)

  isc_info_end                   =          1;
  isc_info_truncated             =          2;
  isc_info_error                 =          3;
  isc_info_data_not_ready	 =          4;
  isc_info_flag_end		 =          127;


(*******************************)
(** Request information items **)
(*******************************)

  isc_info_number_messages       =          #4;
  isc_info_max_message           =          #5;
  isc_info_max_send              =          #6;
  isc_info_max_receive           =          #7;
  isc_info_state                 =          #8;
  isc_info_message_number        =          #9;
  isc_info_message_size          =         #10;
  isc_info_request_cost          =         #11;
  isc_info_access_path           =         #12;
  isc_info_req_select_count      =         #13;
  isc_info_req_insert_count      =         #14;
  isc_info_req_update_count      =         #15;
  isc_info_req_delete_count      =         #16;


(***********************)
(** Access path items **)
(***********************)

  isc_info_rsb_end               =          0;
  isc_info_rsb_begin             =          1;
  isc_info_rsb_type              =          2;
  isc_info_rsb_relation          =          3;
  isc_info_rsb_plan              =          4;

(***************)
(** Rsb types **)
(***************)

  isc_info_rsb_unknown           =          1;
  isc_info_rsb_indexed           =          2;
  isc_info_rsb_navigate          =          3;
  isc_info_rsb_sequential        =          4;
  isc_info_rsb_cross             =          5;
  isc_info_rsb_sort              =          6;
  isc_info_rsb_first             =          7;
  isc_info_rsb_boolean           =          8;
  isc_info_rsb_union             =          9;
  isc_info_rsb_aggregate         =         10;
  isc_info_rsb_merge             =         11;
  isc_info_rsb_ext_sequential    =         12;
  isc_info_rsb_ext_indexed       =         13;
  isc_info_rsb_ext_dbkey         =         14;
  isc_info_rsb_left_cross        =         15;
  isc_info_rsb_select            =         16;
  isc_info_rsb_sql_join          =         17;
  isc_info_rsb_simulate          =         18;
  isc_info_rsb_sim_cross         =         19;
  isc_info_rsb_once              =         20;
  isc_info_rsb_procedure         =         21;

(************************)
(** Bitmap expressions **)
(************************)

  isc_info_rsb_and               =          1;
  isc_info_rsb_or                =          2;
  isc_info_rsb_dbkey             =          3;
  isc_info_rsb_index             =          4;

  isc_info_req_active            =          2;
  isc_info_req_inactive          =          3;
  isc_info_req_send              =          4;
  isc_info_req_receive           =          5;
  isc_info_req_select            =          6;
  isc_info_req_sql_stall         =          7;

(****************************)
(** Blob information items **)
(****************************)

  isc_info_blob_num_segments     =          4;
  isc_info_blob_max_segment      =          5;
  isc_info_blob_total_length     =          6;
  isc_info_blob_type             =          7;

(***********************************)
(** Transaction information items **)
(***********************************)

  isc_info_tra_id                =          4;

(*******************************)
(** Service information items **)
(*******************************)

{$IFDEF IB5_ONLY }
(*  This defines are not there in version 6.0 *)
{  isc_info_svc_version           =          4;
  isc_info_svc_message           =          5;
  isc_info_svc_total_length      =          6;
  isc_info_svc_response          =          7;
  isc_info_svc_response_more     =          8;
  isc_info_svc_line              =          9;
  isc_info_svc_to_eof            =         10;
  isc_info_svc_timeout           =         11;
  isc_info_svc_server_version    =         12;
  isc_info_svc_implementation    =         13;
  isc_info_svc_capabilities      =         14;
  isc_info_svc_user_dbpath       =         15;
  isc_info_svc_svr_db_info       =         16;
  isc_info_svc_svr_online        =         17;
  isc_info_svc_svr_offline       =         18;
  isc_info_svc_get_config        =         19;
  isc_info_svc_set_config        =         20;
  isc_info_svc_default_config    =         21;
  isc_info_svc_get_env		 =         22;
  isc_info_svc_get_env_lock	 =         23;
  isc_info_svc_get_env_msg	 =         24; }
{$ENDIF}


(***************************)
(** SQL information items **)
(***************************)

  isc_info_sql_select            =          #4;
  isc_info_sql_bind              =          #5;
  isc_info_sql_num_variables     =          #6;
  isc_info_sql_describe_vars     =          #7;
  isc_info_sql_describe_end      =          #8;
  isc_info_sql_sqlda_seq         =          #9;
  isc_info_sql_message_seq       =         #10;
  isc_info_sql_type              =         #11;
  isc_info_sql_sub_type          =         #12;
  isc_info_sql_scale             =         #13;
  isc_info_sql_length            =         #14;
  isc_info_sql_null_ind          =         #15;
  isc_info_sql_field             =         #16;
  isc_info_sql_relation          =         #17;
  isc_info_sql_owner             =         #18;
  isc_info_sql_alias             =         #19;
  isc_info_sql_sqlda_start       =         #20;
  isc_info_sql_stmt_type         =         #21;
  isc_info_sql_get_plan          =         #22;
  isc_info_sql_records           =         #23;
  isc_info_sql_batch_fetch       =         #24;

(***********************************)
(** SQL information return values **)
(***********************************)

  isc_info_sql_stmt_select           =          #1;
  isc_info_sql_stmt_insert           =          #2;
  isc_info_sql_stmt_update           =          #3;
  isc_info_sql_stmt_delete           =          #4;
  isc_info_sql_stmt_ddl              =          #5;
  isc_info_sql_stmt_get_segment      =          #6;
  isc_info_sql_stmt_put_segment      =          #7;
  isc_info_sql_stmt_exec_procedure   =          #8;
  isc_info_sql_stmt_start_trans      =          #9;
  isc_info_sql_stmt_commit           =         #10;
  isc_info_sql_stmt_rollback         =         #11;
  isc_info_sql_stmt_select_for_upd   =         #12;
  isc_info_sql_stmt_set_generator    =         #13;


(*************************************)
(** Server configuration key values **)
(*************************************)

  ISCCFG_LOCKMEM_KEY             =          0;
  ISCCFG_LOCKSEM_KEY             =          1;
  ISCCFG_LOCKSIG_KEY             =          2;
  ISCCFG_EVNTMEM_KEY             =          3;
  ISCCFG_DBCACHE_KEY             =          4;
  ISCCFG_PRIORITY_KEY            =          5;
  ISCCFG_IPCMAP_KEY              =          6;
  ISCCFG_MEMMIN_KEY              =          7;
  ISCCFG_MEMMAX_KEY              =          8;
  ISCCFG_LOCKORDER_KEY           =          9;
  ISCCFG_ANYLOCKMEM_KEY          =         10;
  ISCCFG_ANYLOCKSEM_KEY          =         11;
  ISCCFG_ANYLOCKSIG_KEY          =         12;
  ISCCFG_ANYEVNTMEM_KEY          =         13;
  ISCCFG_LOCKHASH_KEY            =         14;
  ISCCFG_DEADLOCK_KEY            =         15;
  ISCCFG_LOCKSPIN_KEY            =         16;
  ISCCFG_CONN_TIMEOUT_KEY        =         17;
  ISCCFG_DUMMY_INTRVL_KEY        =         18;


(*****************)
(** Error codes **)
(*****************)

  isc_facility                   =         20;
  isc_err_base                   =  335544320;
  isc_err_factor                 =          1;
  isc_arg_end                    =          0;
  isc_arg_gds                    =          1;
  isc_arg_string                 =          2;
  isc_arg_cstring                =          3;
  isc_arg_number                 =          4;
  isc_arg_interpreted            =          5;
  isc_arg_vms                    =          6;
  isc_arg_unix                   =          7;
  isc_arg_domain                 =          8;
  isc_arg_dos                    =          9;
  isc_arg_mpexl                  =         10;
  isc_arg_mpexl_ipc              =         11;
  isc_arg_next_mach              =         15;
  isc_arg_netware                =         16;
  isc_arg_win32                  =         17;
  isc_arg_warning                =         18;

(************************************************)
(** Dynamic Data Definition Language operators **)
(************************************************)

(********************)
(** Version number **)
(********************)

  isc_dyn_version_1              =          1;
  isc_dyn_eoc                    =         -1;

(********************************)
(** Operations (may be nested) **)
(********************************)

  isc_dyn_begin                  =          2;
  isc_dyn_end                    =          3;
  isc_dyn_if                     =          4;
  isc_dyn_def_database           =          5;
  isc_dyn_def_global_fld         =          6;
  isc_dyn_def_local_fld          =          7;
  isc_dyn_def_idx                =          8;
  isc_dyn_def_rel                =          9;
  isc_dyn_def_sql_fld            =         10;
  isc_dyn_def_view               =         12;
  isc_dyn_def_trigger            =         15;
  isc_dyn_def_security_class     =        120;
  isc_dyn_def_dimension          =        140;
  isc_dyn_def_generator          =         24;
  isc_dyn_def_function           =         25;
  isc_dyn_def_filter             =         26;
  isc_dyn_def_function_arg       =         27;
  isc_dyn_def_shadow             =         34;
  isc_dyn_def_trigger_msg        =         17;
  isc_dyn_def_file               =         36;
  isc_dyn_mod_database           =         39;
  isc_dyn_mod_rel                =         11;
  isc_dyn_mod_global_fld         =         13;
  isc_dyn_mod_idx                =        102;
  isc_dyn_mod_local_fld          =         14;
  isc_dyn_mod_sql_fld          =          216;
  isc_dyn_mod_view               =         16;
  isc_dyn_mod_security_class     =        122;
  isc_dyn_mod_trigger            =        113;
  isc_dyn_mod_trigger_msg        =         28;
  isc_dyn_delete_database        =         18;
  isc_dyn_delete_rel             =         19;
  isc_dyn_delete_global_fld      =         20;
  isc_dyn_delete_local_fld       =         21;
  isc_dyn_delete_idx             =         22;
  isc_dyn_delete_security_class  =        123;
  isc_dyn_delete_dimensions      =        143;
  isc_dyn_delete_trigger         =         23;
  isc_dyn_delete_trigger_msg     =         29;
  isc_dyn_delete_filter          =         32;
  isc_dyn_delete_function        =         33;
  isc_dyn_delete_shadow          =         35;
  isc_dyn_grant                  =         30;
  isc_dyn_revoke                 =         31;
  isc_dyn_def_primary_key        =         37;
  isc_dyn_def_foreign_key        =         38;
  isc_dyn_def_unique             =         40;
  isc_dyn_def_procedure          =        164;
  isc_dyn_delete_procedure       =        165;
  isc_dyn_def_parameter          =        135;
  isc_dyn_delete_parameter       =        136;
  isc_dyn_mod_procedure          =        175;
  isc_dyn_def_log_file           =        176;
  isc_dyn_def_cache_file         =        180;
  isc_dyn_def_exception          =        181;
  isc_dyn_mod_exception          =        182;
  isc_dyn_del_exception          =        183;
  isc_dyn_drop_log               =        194;
  isc_dyn_drop_cache             =        195;
  isc_dyn_def_default_log        =        202;

(*************************)
(** View specific stuff **)
(*************************)

  isc_dyn_view_blr               =         43;
  isc_dyn_view_source            =         44;
  isc_dyn_view_relation          =         45;
  isc_dyn_view_context           =         46;
  isc_dyn_view_context_name      =         47;

(************************)
(** Generic attributes **)
(************************)

  isc_dyn_rel_name               =         50;
  isc_dyn_fld_name               =         51;
  isc_dyn_new_fld_name           =        215;
  isc_dyn_idx_name               =         52;
  isc_dyn_description            =         53;
  isc_dyn_security_class         =         54;
  isc_dyn_system_flag            =         55;
  isc_dyn_update_flag            =         56;
  isc_dyn_prc_name               =        166;
  isc_dyn_prm_name               =        137;
  isc_dyn_sql_object             =        196;
  isc_dyn_fld_character_set_name =        174;

(**********************************)
(** Relation specific attributes **)
(**********************************)

  isc_dyn_rel_dbkey_length       =         61;
  isc_dyn_rel_store_trig         =         62;
  isc_dyn_rel_modify_trig        =         63;
  isc_dyn_rel_erase_trig         =         64;
  isc_dyn_rel_store_trig_source  =         65;
  isc_dyn_rel_modify_trig_source =         66;
  isc_dyn_rel_erase_trig_source  =         67;
  isc_dyn_rel_ext_file           =         68;
  isc_dyn_rel_sql_protection     =         69;
  isc_dyn_rel_constraint         =        162;
  isc_dyn_delete_rel_constraint  =        163;

(**************************************)
(** Global field specific attributes **)
(**************************************)

  isc_dyn_fld_type               =         70;
  isc_dyn_fld_length             =         71;
  isc_dyn_fld_scale              =         72;
  isc_dyn_fld_sub_type           =         73;
  isc_dyn_fld_segment_length     =         74;
  isc_dyn_fld_query_header       =         75;
  isc_dyn_fld_edit_string        =         76;
  isc_dyn_fld_validation_blr     =         77;
  isc_dyn_fld_validation_source  =         78;
  isc_dyn_fld_computed_blr       =         79;
  isc_dyn_fld_computed_source    =         80;
  isc_dyn_fld_missing_value      =         81;
  isc_dyn_fld_default_value      =         82;
  isc_dyn_fld_query_name         =         83;
  isc_dyn_fld_dimensions         =         84;
  isc_dyn_fld_not_null           =         85;
  isc_dyn_fld_precision          =         86;
  isc_dyn_fld_char_length        =        172;
  isc_dyn_fld_collation          =        173;
  isc_dyn_fld_default_source     =        193;
  isc_dyn_del_default            =        197;
  isc_dyn_del_validation         =        198;
  isc_dyn_single_validation      =        199;
  isc_dyn_fld_character_set      =        203;

(*************************************)
(** Local field specific attributes **)
(*************************************)

  isc_dyn_fld_source             =         90;
  isc_dyn_fld_base_fld           =         91;
  isc_dyn_fld_position           =         92;
  isc_dyn_fld_update_flag        =         93;

(*******************************)
(** Index specific attributes **)
(*******************************)

  isc_dyn_idx_unique             =        100;
  isc_dyn_idx_inactive           =        101;
  isc_dyn_idx_type               =        103;
  isc_dyn_idx_foreign_key        =        104;
  isc_dyn_idx_ref_column         =        105;
  isc_dyn_idx_statistic          =        204;

(*********************************)
(** Trigger specific attributes **)
(*********************************)

  isc_dyn_trg_type               =        110;
  isc_dyn_trg_blr                =        111;
  isc_dyn_trg_source             =        112;
  isc_dyn_trg_name               =        114;
  isc_dyn_trg_sequence           =        115;
  isc_dyn_trg_inactive           =        116;
  isc_dyn_trg_msg_number         =        117;
  isc_dyn_trg_msg                =        118;

(****************************************)
(** Security Class specific attributes **)
(****************************************)

  isc_dyn_scl_acl                =        121;
  isc_dyn_grant_user             =        130;
  isc_dyn_grant_proc             =        186;
  isc_dyn_grant_trig             =        187;
  isc_dyn_grant_view             =        188;
  isc_dyn_grant_options          =        132;
  isc_dyn_grant_user_group       =        205;

(************************************)
(** Dimension specific information **)
(************************************)

  isc_dyn_dim_lower              =        141;
  isc_dyn_dim_upper              =        142;

(******************************)
(** File specific attributes **)
(******************************)

  isc_dyn_file_name              =        125;
  isc_dyn_file_start             =        126;
  isc_dyn_file_length            =        127;
  isc_dyn_shadow_number          =        128;
  isc_dyn_shadow_man_auto        =        129;
  isc_dyn_shadow_conditional     =        130;

(**********************************)
(** Log file specific attributes **)
(**********************************)

  isc_dyn_log_file_sequence      =        177;
  isc_dyn_log_file_partitions    =        178;
  isc_dyn_log_file_serial        =        179;
  isc_dyn_log_file_overflow      =        200;
  isc_dyn_log_file_raw           =        201;

(*****************************)
(** Log specific attributes **)
(*****************************)

  isc_dyn_log_group_commit_wait  =        189;
  isc_dyn_log_buffer_size        =        190;
  isc_dyn_log_check_point_length =        191;
  isc_dyn_log_num_of_buffers     =        192;

(**********************************)
(** Function specific attributes **)
(**********************************)

  isc_dyn_function_name          =        145;
  isc_dyn_function_type          =        146;
  isc_dyn_func_module_name       =        147;
  isc_dyn_func_entry_point       =        148;
  isc_dyn_func_return_argument   =        149;
  isc_dyn_func_arg_position      =        150;
  isc_dyn_func_mechanism         =        151;
  isc_dyn_filter_in_subtype      =        152;
  isc_dyn_filter_out_subtype     =        153;


  isc_dyn_description2           =        154;
  isc_dyn_fld_computed_source2   =        155;
  isc_dyn_fld_edit_string2       =        156;
  isc_dyn_fld_query_header2      =        157;
  isc_dyn_fld_validation_source2 =        158;
  isc_dyn_trg_msg2               =        159;
  isc_dyn_trg_source2            =        160;
  isc_dyn_view_source2           =        161;
  isc_dyn_xcp_msg2               =        184;

(***********************************)
(** Generator specific attributes **)
(***********************************)

  isc_dyn_generator_name         =         95;
  isc_dyn_generator_id           =         96;

(***********************************)
(** Procedure specific attributes **)
(***********************************)

  isc_dyn_prc_inputs             =        167;
  isc_dyn_prc_outputs            =        168;
  isc_dyn_prc_source             =        169;
  isc_dyn_prc_blr                =        170;
  isc_dyn_prc_source2            =        171;

(***********************************)
(** Parameter specific attributes **)
(***********************************)

  isc_dyn_prm_number             =        138;
  isc_dyn_prm_type               =        139;

(**********************************)
(** Relation specific attributes **)
(**********************************)

  isc_dyn_xcp_msg                =        185;

(************************************************)
(** Cascading referential integrity values     **)
(************************************************)
  isc_dyn_foreign_key_update     =        205;
  isc_dyn_foreign_key_delete     =        206;
  isc_dyn_foreign_key_cascade    =        207;
  isc_dyn_foreign_key_default    =        208;
  isc_dyn_foreign_key_null       =        209;
  isc_dyn_foreign_key_none       =        210;

(*************************)
(** SQL role values     **)
(*************************)
  isc_dyn_def_sql_role           =        211;
  isc_dyn_sql_role_name          =        212;
  isc_dyn_grant_admin_options    =        213;
  isc_dyn_del_sql_role           =        214;

(******************************)
(** Last $dyn value assigned **)
(******************************)

  isc_dyn_last_dyn_value         =        216;

(********************************************)
(** Array slice description language (SDL) **)
(********************************************)

  isc_sdl_version1               =          1;
  isc_sdl_eoc                    =         -1;
  isc_sdl_relation               =          2;
  isc_sdl_rid                    =          3;
  isc_sdl_field                  =          4;
  isc_sdl_fid                    =          5;
  isc_sdl_struct                 =          6;
  isc_sdl_variable               =          7;
  isc_sdl_scalar                 =          8;
  isc_sdl_tiny_integer           =          9;
  isc_sdl_short_integer          =         10;
  isc_sdl_long_integer           =         11;
  isc_sdl_literal                =         12;
  isc_sdl_add                    =         13;
  isc_sdl_subtract               =         14;
  isc_sdl_multiply               =         15;
  isc_sdl_divide                 =         16;
  isc_sdl_negate                 =         17;
  isc_sdl_eql                    =         18;
  isc_sdl_neq                    =         19;
  isc_sdl_gtr                    =         20;
  isc_sdl_geq                    =         21;
  isc_sdl_lss                    =         22;
  isc_sdl_leq                    =         23;
  isc_sdl_and                    =         24;
  isc_sdl_or                     =         25;
  isc_sdl_not                    =         26;
  isc_sdl_while                  =         27;
  isc_sdl_assignment             =         28;
  isc_sdl_label                  =         29;
  isc_sdl_leave                  =         30;
  isc_sdl_begin                  =         31;
  isc_sdl_end                    =         32;
  isc_sdl_do3                    =         33;
  isc_sdl_do2                    =         34;
  isc_sdl_do1                    =         35;
  isc_sdl_element                =         36;

(**********************************************)
(** International text interpretation values **)
(**********************************************)

  isc_interp_eng_ascii           =          0;
  isc_interp_jpn_sjis            =          5;
  isc_interp_jpn_euc             =          6;

(******************************************)
(** Scroll direction for isc_dsql_fetch2 **)
(******************************************)

  isc_fetch_next                 =          0;
  isc_fetch_prior                =          1;
  isc_fetch_first                =          2;
  isc_fetch_last                 =          3;
  isc_fetch_absolute             =          4;
  isc_fetch_relative             =          5;

(*********************)
(** SQL definitions **)
(*********************)
  SQL_VARYING                    =        448;
  SQL_TEXT                       =        452;
  SQL_DOUBLE                     =        480;
  SQL_FLOAT                      =        482;
  SQL_LONG                       =        496;
  SQL_SHORT                      =        500;
  SQL_TIMESTAMP                  =        510;
  SQL_BLOB                       =        520;
  SQL_D_FLOAT                    =        530;
  SQL_ARRAY                      =        540;
  SQL_QUAD                       =        550;
  SQL_TYPE_TIME                  =        560;
  SQL_TYPE_DATE                  =        570;
  SQL_INT64                      =        580;
  SQL_BOOLEAN                    =        32764;
  SQL_DATE                       =        SQL_TIMESTAMP;

(*******************)
(** Blob Subtypes **)
(*******************)

(** types less than zero are reserved for customer use **)

  isc_blob_untyped               =          0;

(** internal subtypes **)

  isc_blob_text                  =          1;
  isc_blob_blr                   =          2;
  isc_blob_acl                   =          3;
  isc_blob_ranges                =          4;
  isc_blob_summary               =          5;
  isc_blob_format                =          6;
  isc_blob_tra                   =          7;
  isc_blob_extfile               =          8;

(** the range 20-30 is reserved for dBASE and Paradox types **)

  isc_blob_formatted_memo        =         20;
  isc_blob_paradox_ole           =         21;
  isc_blob_graphic               =         22;
  isc_blob_dbase_ole             =         23;
  isc_blob_typed_binary          =         24;

{$IFDEF IB5_ONLY}
(** SQLDA_LENGTH is defined in C as a macro, but in Pascal we must defined it
   as a function... **)
function SQLDA_LENGTH(n: Long): Long;
{$ENDIF}

(** XSQLDA_LENGTH is defined in C as a macro, but in Pascal we must defined it
   as a function... **)
function XSQLDA_LENGTH(n: Long): Long;

(** getb, putb, putbx are all defined in C as macros.
   Use functions and procedures for the functionality **)
{function getb                   (p: PBSTREAM): Char;
function putb                   (x: Char; p: PBSTREAM): Int;
function putbx                  (x: Char; p: PBSTREAM): Int;}

(*
#define ADD_SPB_LENGTH(p, length)	{*(p)++ = (length); \
    					 *(p)++ = (length) >> 8;}

#define ADD_SPB_NUMERIC(p, data)	{*(p)++ = (data); \
    					 *(p)++ = (data) >> 8; \
					 *(p)++ = (data) >> 16; \
					 *(p)++ = (data) >> 24;}
*)
procedure add_spb_length(var p: PChar; length: integer);
procedure add_spb_numeric(var p: PChar; data: integer);


implementation


{$IFDEF IB5_ONLY}
function SQLDA_LENGTH(n: Long): Long;
(*  The C-macro reads like this:
   SQLDA_LENGTH(n)         (sizeof (SQLDA) + (n-1) * sizeof (SQLVAR)) *)
begin
  result := sizeof(TSQLDA) + ((n - 1) * sizeof(TSQLVAR));
end;
{$ENDIF}


function XSQLDA_LENGTH(n: Long): Long;
(*  The C-macro reads like this:
   XSQLDA_LENGTH(n)	(sizeof (XSQLDA) + (n-1) * sizeof (XSQLVAR)) *)
begin
  result := SizeOf(TXSQLDA) + ((n - 1) * SizeOf(TXSQLVAR));
end;

{function getb(p: PBSTREAM): Char;
(*  The C-macro reads like this:
   getb(p)	(--(p)->bstr_cnt >= 0 ? *(p)->bstr_ptr++ & 0377: BLOB_get (p)) *)
begin
  Dec(p^.bstr_cnt);
  if (p^.bstr_cnt >= 0) then begin
    result := Char(Int(p^.bstr_ptr^) and Int(0377));
    Inc(p^.bstr_ptr);
  end else
    result := Char(BLOB_get(p));
end;}

//function putb(x: Char; p: PBSTREAM): Int;
(*  The C-macro reads like this:
   putb(x,p) ((x == '\n' || (!(--(p)->bstr_cnt))) ?      // then
     BLOB_put (x,p) :                                    // else
     ((int) (*(p)->bstr_ptr++ = (unsigned) (x)))) *)
begin
  Dec(p^.bstr_cnt);
  if (x = Chr(Int('n') - Int('a'))) or (p^.bstr_cnt = 0) then
    result := BLOB_put(x, p)
  else begin
    p^.bstr_ptr^ := Char(x);
    result := UInt(x);
    Inc(p^.bstr_ptr^);
  end;
end;

function putbx(x: Char; p: PBSTREAM): Int;
(*  The C-macro reads like this:
   putbx(x,p) ((!(--(p)->bstr_cnt)) ?    // then
     BLOB_put (x,p) :                    // else
     ((int) (*(p)->bstr_ptr++ = (unsigned) (x)))) *)
begin
  Dec(p^.bstr_cnt);
  if (p^.bstr_cnt = 0) then
    result := BLOB_put(x, p)
  else begin
    p^.bstr_ptr^ := Char(x);
    Inc(p^.bstr_ptr^);
    result := UInt(x);
  end;
end;

(*******************************************)
(** Service manager functions             **)
(*******************************************)


procedure add_spb_length(var p: PChar; length: integer);
(*
#define ADD_SPB_LENGTH(p, length)	{*(p)++ = (length); \
    					 *(p)++ = (length) >> 8;}
*)
begin
  p^ := char(length);
  Inc (p);
  p^ := char(length shr 8);
  Inc (p);
end;

procedure add_spb_numeric(var p: PChar; data: integer);
(*
#define ADD_SPB_NUMERIC(p, data)	{*(p)++ = (data); \
    					 *(p)++ = (data) >> 8; \
					 *(p)++ = (data) >> 16; \
					 *(p)++ = (data) >> 24;}
*)
begin
  p^ := char(data);
  Inc (p);
  p^ := char(data shr 8);
  Inc (p);
  p^ := char(data shr 16);
  Inc (p);
  p^ := char(data shr 24);
  Inc (p);
end;

end.

