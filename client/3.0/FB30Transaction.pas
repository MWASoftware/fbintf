(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  The contents of this file are subject to the Initial Developer's
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
unit FB30Transaction;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, Firebird, IB, FBClientAPI, FB30ClientAPI,
  FB30Attachment, FBParamBlock, FBActivityMonitor, FBTransaction;

type

  { TFB30Transaction }

  TFB30Transaction = class(TFBTransaction,ITransaction, IActivityMonitor)
  private
    FTransactionIntf: Firebird.ITransaction;
    FFirebird30ClientAPI: TFB30ClientAPI;
    procedure FreeHandle(Force: boolean);
  protected
    function GetActivityIntf(att: IAttachment): IActivityMonitor; override;
    procedure SetInterface(api: TFBClientAPI); override;
    function GetTrInfo(ReqBuffer: PByte; ReqBufLen: integer): ITrInformation; override;
    procedure InternalStartSingle(attachment: IAttachment); override;
    procedure InternalStartMultiple; override;
    procedure InternalCommit(Force: boolean); override;
    procedure InternalCommitRetaining; override;
    procedure InternalRollback(Force: boolean); override;
    procedure InternalRollbackRetaining; override;
  public
    constructor Create(api: TFBClientAPI; Attachment: IAttachment; aTransactionIntf: Firebird.ITransaction); overload;
    destructor Destroy; override;
    property TransactionIntf: Firebird.ITransaction read FTransactionIntf;
    {ITransaction}
    function GetInTransaction: boolean; override;
    procedure PrepareForCommit; override;
  end;


implementation

uses FBMessages;

{ TFB30Transaction }

procedure TFB30Transaction.FreeHandle(Force: boolean);
begin
  if assigned(FTransactionIntf) then
  try
    FTransactionIntf.release;
  except
    if not Force then raise;
    {else ignore if Force = true}
  end;
  FTransactionIntf := nil;
end;

function TFB30Transaction.GetActivityIntf(att: IAttachment): IActivityMonitor;
begin
  att.QueryInterface(IActivityMonitor,Result);
end;

procedure TFB30Transaction.SetInterface(api: TFBClientAPI);
begin
  inherited SetInterface(api);
  FFirebird30ClientAPI := api as TFB30ClientAPI;
end;

function TFB30Transaction.GetTrInfo(ReqBuffer: PByte; ReqBufLen: integer
  ): ITrInformation;
begin
  Result := TTrInformation.Create(FFirebird30ClientAPI);
  with FFirebird30ClientAPI, Result as TTrInformation do
  begin
    FTransactionIntf.getInfo(StatusIntf, ReqBufLen, BytePtr(ReqBuffer),
                               getBufSize, BytePtr(Buffer));
    Check4DataBaseError;
  end
end;

procedure TFB30Transaction.InternalStartSingle(attachment: IAttachment);
begin
  if FTransactionIntf = nil then
  with FFirebird30ClientAPI do
  begin
    FTransactionIntf  := (attachment as TFB30Attachment).AttachmentIntf.startTransaction(StatusIntf,
             (FTPB as TTPB).getDataLength,BytePtr((FTPB as TTPB).getBuffer));
    Check4DataBaseError;
  end;
  SignalActivity;
end;

procedure TFB30Transaction.InternalStartMultiple;
var Dtc: IDtc;
    DtcStart: IDtcStart;
    i: integer;
begin
  if FTransactionIntf = nil then
  with FFirebird30ClientAPI do
  begin
    Dtc := MasterIntf.getDtc;
    DtcStart := Dtc.startBuilder(StatusIntf);
    Check4DataBaseError;

    for i := 0 to Length(FAttachments) - 1 do
    if (FAttachments[i] <> nil)  then
    begin
      DTCStart.addWithTpb(StatusIntf,
                          (FAttachments[i] as TFB30Attachment).AttachmentIntf,
                          (FTPB as TTPB).getDataLength,
                          BytePtr((FTPB as TTPB).getBuffer));
      Check4DataBaseError;
    end;

    FTransactionIntf := DtcStart.start(StatusIntf);
    Check4DataBaseError;
    SignalActivity;
  end;
end;

procedure TFB30Transaction.InternalCommit(Force: boolean);
begin
  with FFirebird30ClientAPI do
  begin
    FTransactionIntf.commit(StatusIntf);
    if not Force and InErrorState then
       IBDataBaseError;
  end;
  SignalActivity;
  FreeHandle(Force);
end;

procedure TFB30Transaction.InternalCommitRetaining;
begin
  with FFirebird30ClientAPI do
  begin
    FTransactionIntf.commitRetaining(StatusIntf);
    Check4DataBaseError;
  end;
  SignalActivity;
end;

procedure TFB30Transaction.InternalRollback(Force: boolean);
begin
  with FFirebird30ClientAPI do
  begin
    FTransactionIntf.rollback(StatusIntf);
    if not Force and InErrorState then
       IBDataBaseError;
  end;
  SignalActivity;
  FreeHandle(Force);
end;

procedure TFB30Transaction.InternalRollbackRetaining;
begin
  with FFirebird30ClientAPI do
  begin
    FTransactionIntf.rollbackRetaining(StatusIntf);
    Check4DataBaseError;
  end;
  SignalActivity;
end;

constructor TFB30Transaction.Create(api: TFBClientAPI; Attachment: IAttachment;
  aTransactionIntf: Firebird.ITransaction);
begin
  FTransactionIntf := aTransactionIntf;
  FTransactionIntf.addRef();
  inherited Create(api,Attachment,nil,taCommit,'');
end;

destructor TFB30Transaction.Destroy;
begin
  inherited Destroy;
  FreeHandle(true);
end;

function TFB30Transaction.GetInTransaction: boolean;
begin
  Result := FTransactionIntf <> nil;
end;

procedure TFB30Transaction.PrepareForCommit;
begin
  if Length(FAttachments) < 2 then
    IBError(ibxeNotAMultiDatabaseTransaction,[nil]);
  if FTransactionIntf = nil then
    Exit;
  with FFirebird30ClientAPI do
  begin
    FTransactionIntf.prepare(StatusIntf,0,nil);
    Check4DataBaseError;
  end;
  SignalActivity;
end;


end.

