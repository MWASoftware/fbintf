unit FB30Transaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Firebird, IB, FBClientAPI, FB30ClientAPI,
  FB30Attachment, FBParamBlock, FBActivityMonitor, FBTransaction;

type

  { TFB30Transaction }

  TFB30Transaction = class(TFBTransaction,ITransaction, IActivityMonitor)
  private
    FTransactionIntf: Firebird.ITransaction;
    procedure StartMultiple;
    procedure FreeHandle;
  protected
    function GetActivityIntf(att: IAttachment): IActivityMonitor; override;
  public
    destructor Destroy; override;
    property TransactionIntf: Firebird.ITransaction read FTransactionIntf;
    {ITransaction}
    function GetInTransaction: boolean; override;
    procedure PrepareForCommit; override;
    procedure Commit(Force: boolean=false); override;
    procedure CommitRetaining; override;
    procedure Start(DefaultCompletion: TTransactionAction=taCommit); overload; override;
    procedure Rollback(Force: boolean=false); override;
    procedure RollbackRetaining; override;
  end;


implementation

uses FBMessages;

{ TFB30Transaction }

procedure TFB30Transaction.StartMultiple;
var Dtc: IDtc;
    DtcStart: IDtcStart;
    i: integer;
begin
  with Firebird30ClientAPI do
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
  end;
end;

procedure TFB30Transaction.FreeHandle;
begin
  if assigned(FTransactionIntf) then
    FTransactionIntf.release;
  FTransactionIntf := nil;
end;

function TFB30Transaction.GetActivityIntf(att: IAttachment): IActivityMonitor;
begin
  Result := att as TFB30Attachment;
end;

destructor TFB30Transaction.Destroy;
begin
  inherited Destroy;
  FreeHandle;
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
  with Firebird30ClientAPI do
  begin
    FTransactionIntf.prepare(StatusIntf,0,nil);
    Check4DataBaseError;
  end;
  SignalActivity;
end;

procedure TFB30Transaction.Commit(Force: boolean);
begin
  if FTransactionIntf = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FTransactionIntf.commit(StatusIntf);
    if not Force and InErrorState then
       IBDataBaseError;
  end;
  SignalActivity;
  FreeHandle;
end;

procedure TFB30Transaction.CommitRetaining;
begin
  if FTransactionIntf = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FTransactionIntf.commitRetaining(StatusIntf);
    Check4DataBaseError;
  end;
  SignalActivity;
end;

procedure TFB30Transaction.Start(DefaultCompletion: TTransactionAction);
begin
  if FTransactionIntf <> nil then
    Exit;

  FDefaultCompletion := DefaultCompletion;

  if Length(FAttachments) > 0 then
    StartMultiple
  else
    with Firebird30ClientAPI do
    begin
      FTransactionIntf  := (FAttachments[0] as TFB30Attachment).AttachmentIntf.startTransaction(StatusIntf,
               (FTPB as TTPB).getDataLength,BytePtr((FTPB as TTPB).getBuffer));
      Check4DataBaseError;
    end;
  SignalActivity;
  Inc(FSeqNo);
end;

procedure TFB30Transaction.Rollback(Force: boolean);
begin
  if FTransactionIntf = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FTransactionIntf.rollback(StatusIntf);
    if not Force and InErrorState then
       IBDataBaseError;
  end;
  SignalActivity;
  FreeHandle;
end;

procedure TFB30Transaction.RollbackRetaining;
begin
  if FTransactionIntf = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FTransactionIntf.rollbackRetaining(StatusIntf);
    Check4DataBaseError;
  end;
  SignalActivity;
end;


end.

