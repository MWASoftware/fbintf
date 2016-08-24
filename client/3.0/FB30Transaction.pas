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
    FTransaction: Firebird.ITransaction;
    procedure StartMultiple;
  protected
    function GetActivityIntf(att: IAttachment): IActivityMonitor; override;
  public
    property TransactionIntf: Firebird.ITransaction read FTransaction;
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
                          (FAttachments[i] as TFBAttachment).AttachmentIntf,
                          (FTPB as TTPB).getDataLength,
                          BytePtr((FTPB as TTPB).getBuffer));
      Check4DataBaseError;
    end;
    FTransaction := DtcStart.start(StatusIntf);
    Check4DataBaseError;
  end;
end;

function TFB30Transaction.GetActivityIntf(att: IAttachment): IActivityMonitor;
begin
  Result := att as TFBAttachment;
end;

function TFB30Transaction.GetInTransaction: boolean;
begin
  Result := FTransaction <> nil;
end;

procedure TFB30Transaction.PrepareForCommit;
begin
  if Length(FAttachments) < 2 then
    IBError(ibxeNotAMultiDatabaseTransaction,[nil]);
  if FTransaction = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FTransaction.prepare(StatusIntf,0,nil);
    Check4DataBaseError;
  end;
  SignalActivity;
end;

procedure TFB30Transaction.Commit(Force: boolean);
begin
  if FTransaction = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FTransaction.commit(StatusIntf);
    if not Force and InErrorState then
       IBDataBaseError;
  end;
  SignalActivity;
  FTransaction := nil;
end;

procedure TFB30Transaction.CommitRetaining;
begin
  if FTransaction = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FTransaction.commitRetaining(StatusIntf);
    Check4DataBaseError;
  end;
  SignalActivity;
end;

procedure TFB30Transaction.Start(DefaultCompletion: TTransactionAction);
begin
  if FTransaction <> nil then
    Exit;

  FDefaultCompletion := DefaultCompletion;

  if Length(FAttachments) > 0 then
    StartMultiple
  else
    with Firebird30ClientAPI do
    begin
      FTransaction  := (FAttachments[0] as TFBAttachment).AttachmentIntf.startTransaction(StatusIntf,
               (FTPB as TTPB).getDataLength,BytePtr((FTPB as TTPB).getBuffer));
      Check4DataBaseError;
    end;
  SignalActivity;
end;

procedure TFB30Transaction.Rollback(Force: boolean);
begin
  if FTransaction = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FTransaction.rollback(StatusIntf);
    if not Force and InErrorState then
       IBDataBaseError;
  end;
  SignalActivity;
  FTransaction := nil;
end;

procedure TFB30Transaction.RollbackRetaining;
begin
  if FTransaction = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FTransaction.rollbackRetaining(StatusIntf);
    Check4DataBaseError;
  end;
  SignalActivity;
end;


end.

