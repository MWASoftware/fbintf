(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API. Although predominantly
 *  a new development they include source code taken from IBX and may be
 *  considered a derived product. This software thus also includes the copyright
 *  notice and license conditions from IBX.
 *
 *  Except for those parts dervied from IBX, contents of this file are subject
 *  to the Initial Developer's Public License Version 1.0 (the "License"); you
 *  may not use this file except in compliance with the License. You may obtain a
 *  copy of the License here:
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
 *  The Original Code is (C) 2016 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
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
{    Associates Ltd 2011 - 2015                                                }
{                                                                        }
{************************************************************************}
unit FB25ClientAPI;

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, FBClientAPI, IBHeader, IBExternals, IB;

const
  FBClientInterfaceVersion = '2.5';

type

  { TFB25Status }

  TFB25Status = class(TFBStatus,IStatus)
  public
    function StatusVector: PStatusVector; override;
  end;

  { TFB25ClientAPI }

  TFB25ClientAPI = class(TFBClientAPI,IFirebirdAPI)
  private
    FIBServiceAPIPresent: boolean;
    FStatus: TFB25Status;
    FStatusIntf: IStatus;   {Keep a reference to the interface - automatic destroy
                             when this class is freed and last reference to IStatus
                             goes out of scope.}
  protected
    {$IFDEF UNIX}
    function GetFirebirdLibList: string; override;
    {$ENDIF}
    procedure LoadInterface; override;
  public
    constructor Create;
    destructor Destroy; override;
    function StatusVector: PISC_STATUS;
    property IBServiceAPIPresent: boolean read FIBServiceAPIPresent;
    property Status: TFB25Status read FStatus;

  public

    {fbclient API}
    BLOB_get: TBLOB_get;
    BLOB_put: TBLOB_put;
    isc_wait_for_event: Tisc_wait_for_event;
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
    isc_add_user   : Tisc_add_user;
    isc_delete_user: Tisc_delete_user;
    isc_modify_user: Tisc_modify_user;
    isc_array_lookup_bounds: Tisc_array_lookup_bounds;
    isc_array_get_slice: Tisc_array_get_slice;
    isc_array_put_slice: Tisc_array_put_slice;
    isc_prepare_transaction: Tisc_prepare_transaction;

  public
    {Helper Functions}
    function DecodeInteger(bufptr: PChar; len: short): integer; override;
    procedure SQLEncodeDate(aDate: TDateTime; bufptr: PChar); override;
    function SQLDecodeDate(bufptr: PChar): TDateTime; override;
    procedure SQLEncodeTime(aTime: TDateTime; bufptr: PChar); override;
    function SQLDecodeTime(bufptr: PChar): TDateTime;  override;
    procedure SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PChar); override;
    function SQLDecodeDateTime(bufptr: PChar): TDateTime; override;

  public
    {IFirebirdAPI}

    {Database connections}
    function AllocateDPB: IDPB;
    function OpenDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnConnectError: boolean=true): IAttachment;
    function CreateDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnError: boolean=true): IAttachment;  overload;
    function CreateDatabase(sql: string; aSQLDialect: integer; RaiseExceptionOnError: boolean=true): IAttachment; overload;

    {Start Transaction against multiple databases}
    function AllocateTPB: ITPB;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction; overload;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: ITPB; DefaultCompletion: TTransactionCompletion): ITransaction; overload;

    {Service Manager}
    function AllocateSPB: ISPB;
    function HasServiceAPI: boolean;
    function GetServiceManager(ServerName: string; Protocol: TProtocol; SPB: ISPB): IServiceManager;

    {Information}
    function GetStatus: IStatus; override;
    function HasRollbackRetaining: boolean;
    function IsEmbeddedServer: boolean; override;
    function GetImplementationVersion: string;

    {Firebird 3 API}
    function HasMasterIntf: boolean;
    function GetIMaster: TObject;

   end;

const
  Firebird25ClientAPI: TFB25ClientAPI = nil;

implementation

uses FBMessages, dynlibs, FB25Attachment, FB25Transaction, FB25Services, FBParamBlock,
  IBUtils;

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

{ TFB25Status }

threadvar
  FStatusVector: TStatusVector;

function TFB25Status.StatusVector: PStatusVector;
begin
  Result := @FStatusVector;
end;


{ TFB25ClientAPI }

{$IFDEF UNIX}
function TFB25ClientAPI.GetFirebirdLibList: string;
begin
  Result := 'libfbembed.so:libfbembed.so.2.5:libfbembed.so.2.1:libfbclient.so:libfbclient.so.2';
end;
{$ENDIF}

procedure TFB25ClientAPI.LoadInterface;
begin
  inherited LoadInterface;
  BLOB_get := GetProcAddr('BLOB_get'); {do not localize}
  BLOB_put := GetProcAddr('BLOB_put'); {do not localize}
  isc_wait_for_event := GetProcAddr('isc_wait_for_event'); {do not localize}
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
  isc_add_user := GetProcAddr('isc_add_user'); {do not localize}
  isc_delete_user := GetProcAddr('isc_delete_user'); {do not localize}
  isc_modify_user := GetProcAddr('isc_modify_user'); {do not localize}
  isc_array_lookup_bounds := GetProcAddr('isc_array_lookup_bounds'); {do not localize}
  isc_array_get_slice := GetProcAddr('isc_array_get_slice'); {do not localize}
  isc_array_put_slice := GetProcAddr('isc_array_put_slice'); {do not localize}
  isc_prepare_transaction  := GetProcAddr('isc_prepare_transaction'); {do not localize}

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
  FStatus := TFB25Status.Create(self);
  FStatusIntf := FStatus;
  Firebird25ClientAPI := self;
end;

destructor TFB25ClientAPI.Destroy;
begin
  FStatusIntf := nil;
  Firebird25ClientAPI := nil;
  inherited Destroy;
end;


function TFB25ClientAPI.StatusVector: PISC_STATUS;
begin
  Result := PISC_STATUS(FStatus.StatusVector);
end;

function TFB25ClientAPI.GetStatus: IStatus;
begin
  Result := FStatus;
end;

function TFB25ClientAPI.AllocateDPB: IDPB;
begin
  Result := TDPB.Create;
end;

function TFB25ClientAPI.OpenDatabase(DatabaseName: string; DPB: IDPB;
                                    RaiseExceptionOnConnectError: boolean): IAttachment;
begin
   Result := TFB25Attachment.Create(DatabaseName,DPB,RaiseExceptionOnConnectError);
   if not Result.IsConnected then
     Result := nil;
end;

function TFB25ClientAPI.CreateDatabase(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnError: boolean): IAttachment;
begin
  Result := TFB25Attachment.CreateDatabase(DatabaseName, DPB, RaiseExceptionOnError );
   if (Result <> nil) and not Result.IsConnected then
     Result := nil;
end;

function TFB25ClientAPI.CreateDatabase(sql: string; aSQLDialect: integer;
  RaiseExceptionOnError: boolean): IAttachment;
begin
  Result := TFB25Attachment.CreateDatabase(sql,aSQLDialect, RaiseExceptionOnError );
   if (Result <> nil) and not Result.IsConnected then
     Result := nil;
end;

function TFB25ClientAPI.AllocateSPB: ISPB;
begin
  Result := TSPB.Create;
end;

function TFB25ClientAPI.AllocateTPB: ITPB;
begin
  Result := TTPB.Create;
end;

function TFB25ClientAPI.GetServiceManager(ServerName: string;
  Protocol: TProtocol; SPB: ISPB): IServiceManager;
begin
  if HasServiceAPI then
    Result := TFB25ServiceManager.Create(ServerName,Protocol,SPB)
  else
    Result := nil;
end;

function TFB25ClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction;
begin
  Result := TFB25Transaction.Create(Attachments,TPB,DefaultCompletion);
end;

function TFB25ClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: ITPB; DefaultCompletion: TTransactionCompletion): ITransaction;
begin
  Result := TFB25Transaction.Create(Attachments,TPB,DefaultCompletion);
end;

function TFB25ClientAPI.HasServiceAPI: boolean;
begin
  Result := IBServiceAPIPresent;
end;

function TFB25ClientAPI.HasRollbackRetaining: boolean;
begin
  Result := assigned(isc_rollback_retaining);
end;

function TFB25ClientAPI.IsEmbeddedServer: boolean;
begin
  Result := false;
{$IFDEF UNIX}
  Result := Pos('libfbembed',FFBLibraryName) = 1;
{$ENDIF}
{$IFDEF WINDOWS}
  Result := CompareText(FFBLibraryName,FIREBIRD_EMBEDDED) = 0;
{$ENDIF}
end;

function TFB25ClientAPI.HasMasterIntf: boolean;
begin
  Result := false;
end;

function TFB25ClientAPI.GetIMaster: TObject;
begin
  Result := nil;
end;

function TFB25ClientAPI.GetImplementationVersion: string;
begin
  Result := FBClientInterfaceVersion;
end;

function TFB25ClientAPI.DecodeInteger(bufptr: PChar; len: short): integer;
begin
  Result := isc_portable_integer(bufptr,len);
end;

procedure TFB25ClientAPI.SQLEncodeDate(aDate: TDateTime; bufptr: PChar);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy: Word;
begin
  DecodeDate(aDate, Yr, Mn, Dy);
  with tm_date do begin
    tm_sec := 0;
    tm_min := 0;
    tm_hour := 0;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  isc_encode_sql_date(@tm_date, PISC_DATE(bufptr));
end;

function TFB25ClientAPI.SQLDecodeDate(bufptr: PChar): TDateTime;
var
  tm_date: TCTimeStructure;
begin
  isc_decode_sql_date(PISC_DATE(bufptr), @tm_date);
  try
    result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1),
                         Word(tm_date.tm_mday));
  except
    on E: EConvertError do begin
      IBError(ibxeInvalidDataConversion, [nil]);
    end;
  end;
end;

procedure TFB25ClientAPI.SQLEncodeTime(aTime: TDateTime; bufptr: PChar);
var
  tm_date: TCTimeStructure;
  Hr, Mt, S, Ms: Word;
begin
  DecodeTime(aTime, Hr, Mt, S, Ms);
  with tm_date do begin
    tm_sec := S;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := 0;
    tm_mon := 0;
    tm_year := 0;
  end;
  with Firebird25ClientAPI do
    isc_encode_sql_time(@tm_date, PISC_TIME(bufptr));
  if Ms > 0 then
    Inc(PISC_TIME(bufptr)^,Ms*10);
end;

function TFB25ClientAPI.SQLDecodeTime(bufptr: PChar): TDateTime;
var
  tm_date: TCTimeStructure;
  msecs: Word;
begin
  isc_decode_sql_time(PISC_TIME(bufptr), @tm_date);
  try
    msecs :=  (PISC_TIME(bufptr)^ mod 10000) div 10;
    result := EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                         Word(tm_date.tm_sec), msecs)
  except
    on E: EConvertError do begin
      IBError(ibxeInvalidDataConversion, [nil]);
    end;
  end;
end;

procedure TFB25ClientAPI.SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PChar);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy, Hr, Mt, S, Ms: Word;
begin
  DecodeDate(aDateTime, Yr, Mn, Dy);
  DecodeTime(aDateTime, Hr, Mt, S, Ms);
  with tm_date do begin
    tm_sec := S;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  isc_encode_date(@tm_date, PISC_QUAD(bufptr));
  if Ms > 0 then
    Inc(PISC_TIMESTAMP(bufptr)^.timestamp_time,Ms*10);
end;

function TFB25ClientAPI.SQLDecodeDateTime(bufptr: PChar): TDateTime;
var
  tm_date: TCTimeStructure;
  msecs: Word;
begin
  isc_decode_date(PISC_QUAD(bufptr), @tm_date);
  try
    result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1),
                        Word(tm_date.tm_mday));
    msecs := (PISC_TIMESTAMP(bufptr)^.timestamp_time mod 10000) div 10;
    if result >= 0 then
      result := result + EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                                    Word(tm_date.tm_sec), msecs)
    else
      result := result - EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                                    Word(tm_date.tm_sec), msecs)
  except
    on E: EConvertError do begin
      IBError(ibxeInvalidDataConversion, [nil]);
    end;
  end;
end;

end.

