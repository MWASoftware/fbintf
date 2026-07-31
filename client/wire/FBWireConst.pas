(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  This file is part of the pure Pascal wire protocol implementation
 *  (no fbclient required) and is subject to the Initial Developer's
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
 *  The Initial Developer of the Original Code is MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBWireConst;

{ Remote protocol constants. The reference for these values is
  src/remote/protocol.h in the Firebird source tree. }

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

const
  {operation codes}
  op_void                 = 0;
  op_connect              = 1;
  op_exit                 = 2;
  op_accept               = 3;
  op_reject               = 4;
  op_disconnect           = 6;
  op_response             = 9;
  op_attach               = 19;
  op_create               = 20;
  op_detach               = 21;
  op_transaction          = 29;
  op_commit               = 30;
  op_rollback             = 31;
  op_prepare              = 32;   {two phase commit: prepare}
  op_reconnect            = 33;
  op_create_blob          = 34;
  op_open_blob            = 35;
  op_get_segment          = 36;
  op_put_segment          = 37;
  op_cancel_blob          = 38;
  op_close_blob           = 39;
  op_info_database        = 40;
  op_info_request         = 41;
  op_info_transaction     = 42;
  op_info_blob            = 43;
  op_batch_segments       = 44;
  op_que_events           = 48;
  op_cancel_events        = 49;
  op_commit_retaining     = 50;
  op_prepare2             = 51;
  op_event                = 52;
  op_connect_request      = 53;
  op_aux_connect          = 54;
  op_open_blob2           = 56;
  op_create_blob2         = 57;
  op_get_slice            = 58;
  op_put_slice            = 59;
  op_slice                = 60;
  op_seek_blob            = 61;
  op_allocate_statement   = 62;
  op_execute              = 63;
  op_exec_immediate       = 64;
  op_fetch                = 65;
  op_fetch_response       = 66;
  op_free_statement       = 67;
  op_prepare_statement    = 68;
  op_set_cursor           = 69;
  op_info_sql             = 70;
  op_dummy                = 71;
  op_response_piggyback   = 72;
  op_exec_immediate2      = 75;
  op_execute2             = 76;
  op_sql_response         = 78;
  op_drop_database        = 81;
  op_service_attach       = 82;
  op_service_detach       = 83;
  op_service_info         = 84;
  op_service_start        = 85;
  op_rollback_retaining   = 86;
  op_partial              = 89;
  op_trusted_auth         = 90;
  op_cancel               = 91;
  op_cont_auth            = 92;
  op_ping                 = 93;
  op_accept_data          = 94;
  op_abort_aux_connection = 95;
  op_crypt                = 96;
  op_crypt_key_callback   = 97;
  op_cond_accept          = 98;
  op_batch_create         = 99;
  op_batch_msg            = 100;
  op_batch_exec           = 101;
  op_batch_rls            = 102;
  op_batch_cs             = 103;
  op_batch_regblob        = 104;
  op_batch_blob_stream    = 105;
  op_batch_set_bpb        = 106;
  op_repl_data            = 107;
  op_repl_req             = 108;
  op_batch_cancel         = 109;
  op_batch_sync           = 110;
  op_info_batch           = 111;
  op_fetch_scroll         = 112;
  op_info_cursor          = 113;
  op_inline_blob          = 114;

  {connect operation versions}
  CONNECT_VERSION2        = 2;
  CONNECT_VERSION3        = 3;

  {architecture types - we are always generic}
  arch_generic            = 1;

  {protocol versions. Since protocol 11 the high bit (FB_PROTOCOL_FLAG) is
   set to distinguish Firebird from Borland InterBase}
  FB_PROTOCOL_FLAG        = $8000;
  FB_PROTOCOL_MASK        = $7FFF;
  PROTOCOL_VERSION10      = 10;
  PROTOCOL_VERSION11      = FB_PROTOCOL_FLAG or 11;
  PROTOCOL_VERSION12      = FB_PROTOCOL_FLAG or 12;
  {P13: authentication plugins (op_cont_auth), null bitmap in messages -
   Firebird 3.0}
  PROTOCOL_VERSION13      = FB_PROTOCOL_FLAG or 13;
  {P14: wire encryption}
  PROTOCOL_VERSION14      = FB_PROTOCOL_FLAG or 14;
  {P15: conditional accept with early wire encryption}
  PROTOCOL_VERSION15      = FB_PROTOCOL_FLAG or 15;
  {P16: statement timeouts, DECFLOAT, batch API - Firebird 4.0}
  PROTOCOL_VERSION16      = FB_PROTOCOL_FLAG or 16;
  {P17: continues Firebird 4.0 enhancements}
  PROTOCOL_VERSION17      = FB_PROTOCOL_FLAG or 17;
  {P18: scrollable cursor fetch - Firebird 5.0}
  PROTOCOL_VERSION18      = FB_PROTOCOL_FLAG or 18;
  {P19: inline blobs - Firebird 5.0.3}
  PROTOCOL_VERSION19      = FB_PROTOCOL_FLAG or 19;
  {P20: SQL schemas, named arguments - Firebird 6.0}
  PROTOCOL_VERSION20      = FB_PROTOCOL_FLAG or 20;

  {protocol types (accept types)}
  ptype_rpc               = 2;
  ptype_batch_send        = 3;
  ptype_out_of_band       = 4;
  ptype_lazy_send         = 5;
  ptype_mask              = $FF;
  pflag_compress          = $100;

  {user identification data tags (p_cnct_user_id)}
  CNCT_user               = 1;
  CNCT_passwd             = 2;
  CNCT_host               = 4;
  CNCT_group              = 5;
  CNCT_user_verification  = 6;
  CNCT_specific_data      = 7;
  CNCT_plugin_name        = 8;
  CNCT_login              = 9;
  CNCT_plugin_list        = 10;
  CNCT_client_crypt       = 11;

  {client crypt level in CNCT_client_crypt}
  WIRE_CRYPT_DISABLED     = 0;
  WIRE_CRYPT_ENABLED      = 1;
  WIRE_CRYPT_REQUIRED     = 2;

  {authentication plugin names}
  sLegacyAuthPluginName   = 'Legacy_Auth';
  sDefaultPluginList      = 'Srp256,Srp,Legacy_Auth';
  sLegacyAuthSalt         = '9z';

  {wire encryption plugin names}
  sArc4PluginName         = 'Arc4';
  sChaChaPluginName       = 'ChaCha';
  sChaCha64PluginName     = 'ChaCha64';
  sSymmetricKeyName       = 'Symmetric';

  {op_free_statement options}
  DSQL_close              = 1;
  DSQL_drop               = 2;
  DSQL_unprepare          = 4;

  {fetch response status}
  FETCH_status_more_rows  = 0;
  FETCH_status_eof        = 100;

  {op_cancel kinds}
  fb_cancel_disable       = 1;
  fb_cancel_enable        = 2;
  fb_cancel_raise         = 3;
  fb_cancel_abort         = 4;

  {op_connect_request types (for event aux connections)}
  P_REQ_async             = 1;

implementation

end.
