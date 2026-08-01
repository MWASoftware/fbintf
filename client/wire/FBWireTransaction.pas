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
unit FBWireTransaction;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBTransaction, FBClientAPI, FBActivityMonitor,
  FBOutputBlock, FBWireClientAPI, FBWireProtocol, FBWireConst;

type
  { TFBWireTransaction }

  TFBWireTransaction = class(TFBTransaction,ITransaction,IActivityMonitor)
  private
    FWireAPI: TFBWireClientAPI;
    FHandle: integer;
    FInTransaction: boolean;
    function GetConnection: TFBWireConnection;
    {forgets this transaction's inline blob cache entries in every
     attachment - the ids die with the transaction}
    procedure DropInlineBlobs;
  protected
    function GetActivityIntf(att: IAttachment): IActivityMonitor; override;
    function GetTrInfo(ReqBuffer: PByte; ReqBufLen: integer): ITrInformation; override;
    procedure InternalStartSingle(attachment: IAttachment); override;
    procedure InternalStartMultiple; override;
    function InternalCommit(Force: boolean): TTrCompletionState; override;
    procedure InternalCommitRetaining; override;
    function InternalRollback(Force: boolean): TTrCompletionState; override;
    procedure InternalRollbackRetaining; override;
    procedure SetInterface(api: TFBClientAPI); override;
  public
    procedure PrepareForCommit; override;
    function GetInTransaction: boolean; override;
    {the server side transaction handle - used by statements and blobs}
    property Handle: integer read FHandle;
    property Connection: TFBWireConnection read GetConnection;
  end;

implementation

uses FBMessages, IBErrorCodes, FBWireAttachment;

const
  isc_info_end = 1;

{ TFBWireTransaction }

procedure TFBWireTransaction.SetInterface(api: TFBClientAPI);
begin
  inherited SetInterface(api);
  FWireAPI := api as TFBWireClientAPI;
end;

function TFBWireTransaction.GetConnection: TFBWireConnection;
begin
  if GetAttachmentCount = 0 then
    IBError(ibxeNotInTransaction,[nil]);
  Result := (GetAttachment(0) as IAttachment as TObject as TFBWireAttachment).Connection;
end;

procedure TFBWireTransaction.DropInlineBlobs;
var i: integer;
begin
  for i := 0 to GetAttachmentCount - 1 do
    (GetAttachment(i) as IAttachment as TObject as TFBWireAttachment).
      DropInlineBlobs(FHandle);
end;

function TFBWireTransaction.GetActivityIntf(att: IAttachment): IActivityMonitor;
begin
  Result := att as IActivityMonitor;
end;

function TFBWireTransaction.GetTrInfo(ReqBuffer: PByte; ReqBufLen: integer): ITrInformation;
var items, response: TBytes;
    i, len: integer;
    Buffer: TTrInformation;
begin
  CheckHandle;
  SetLength(items,ReqBufLen);
  for i := 0 to ReqBufLen - 1 do
    items[i] := ReqBuffer[i];
  SetLength(response,0);
  try
    response := Connection.GetInfo(op_info_transaction,FHandle,items,DefaultBufferSize);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  Buffer := TTrInformation.Create(FWireAPI);
  Result := Buffer;
  len := Length(response);
  if len > Buffer.getBufSize then
    len := Buffer.getBufSize;
  if len > 0 then
    Move(response[0],Buffer.Buffer^,len);
end;

procedure TFBWireTransaction.InternalStartSingle(attachment: IAttachment);
var tpb: TBytes;
begin
  if FInTransaction then Exit;
  tpb := ParamBlockToBytes(FTPB);
  try
    FHandle := (attachment as TObject as TFBWireAttachment).Connection.
                 StartTransaction((attachment as TObject as TFBWireAttachment).Handle,tpb);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  FInTransaction := true;
end;

procedure TFBWireTransaction.InternalStartMultiple;
begin
  {a transaction spanning several attachments requires the two phase commit
   coordinator which this provider does not implement}
  IBError(ibxeNotSupported,[nil]);
end;

function TFBWireTransaction.InternalCommit(Force: boolean): TTrCompletionState;
begin
  Result := trCommitted;
  if not FInTransaction then Exit;
  if not Connection.Connected then
  begin
    {the connection has gone: the server has already rolled the
     transaction back}
    FInTransaction := false;
    FHandle := 0;
    Exit;
  end;
  DropInlineBlobs;
  try
    Connection.Commit(FHandle);
  except
    on E: Exception do
    begin
      if not Force then
      begin
        Result := trCommitFailed;
        WireIBError(FWireAPI,E);
      end;
    end;
  end;
  FInTransaction := false;
  FHandle := 0;
end;

procedure TFBWireTransaction.InternalCommitRetaining;
begin
  CheckHandle;
  {retaining keeps the transaction handle but a commit still invalidates
   temporary blob ids}
  DropInlineBlobs;
  try
    Connection.CommitRetaining(FHandle);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
end;

function TFBWireTransaction.InternalRollback(Force: boolean): TTrCompletionState;
begin
  Result := trRolledback;
  if not FInTransaction then Exit;
  if not Connection.Connected then
  begin
    {the connection has gone: the server has already rolled the
     transaction back}
    FInTransaction := false;
    FHandle := 0;
    Exit;
  end;
  DropInlineBlobs;
  try
    Connection.Rollback(FHandle);
  except
    on E: Exception do
    begin
      if not Force then
      begin
        Result := trRollbackFailed;
        WireIBError(FWireAPI,E);
      end;
    end;
  end;
  FInTransaction := false;
  FHandle := 0;
end;

procedure TFBWireTransaction.InternalRollbackRetaining;
begin
  CheckHandle;
  DropInlineBlobs;
  try
    Connection.RollbackRetaining(FHandle);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
end;

procedure TFBWireTransaction.PrepareForCommit;
begin
  CheckHandle;
  try
    Connection.PrepareTransaction(FHandle);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
end;

function TFBWireTransaction.GetInTransaction: boolean;
begin
  Result := FInTransaction;
end;

end.
