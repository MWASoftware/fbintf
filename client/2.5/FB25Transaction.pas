unit FB25Transaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBClientAPI, FB25ClientAPI, IBHeader,
  FB25Attachment, FBActivityMonitor, FBTransaction;

type
  { TFB25Transaction }

  TFB25Transaction = class(TFBTransaction,ITransaction, IActivityMonitor)
  private
    FHandle: TISC_TR_HANDLE;
  protected
    function GetActivityIntf(att: IAttachment): IActivityMonitor; override;
  public
    property Handle: TISC_TR_HANDLE read FHandle;

  public
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

uses FBMessages, FBParamBlock;

{ TFB25Transaction }

function TFB25Transaction.GetActivityIntf(att: IAttachment): IActivityMonitor;
begin
  Result := (att as TFB25Attachment);
end;

function TFB25Transaction.GetInTransaction: boolean;
begin
  Result := FHandle <> nil;
end;

procedure TFB25Transaction.PrepareForCommit;
begin
  if Length(FAttachments) < 2 then
    IBError(ibxeNotAMultiDatabaseTransaction,[nil]);
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_prepare_transaction(StatusVector, @FHandle));
end;

procedure TFB25Transaction.Commit(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_commit_transaction(StatusVector, @FHandle),not Force);
  FHandle := nil;
end;

procedure TFB25Transaction.CommitRetaining;
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_commit_retaining(StatusVector, @FHandle));
end;

procedure TFB25Transaction.Start(DefaultCompletion: TTransactionAction);
var pteb: PISC_TEB_ARRAY;
    i: integer;
    db_handle: TISC_DB_HANDLE;
begin
  if FHandle <> nil then
    Exit;
  pteb := nil;
  FDefaultCompletion := DefaultCompletion;
  with Firebird25ClientAPI do
  if (Length(FAttachments) = 1)  then
  try
    db_handle := (FAttachments[0] as TFB25Attachment).Handle;
    Call(isc_start_transaction(StatusVector, @FHandle,1,
              @db_handle,(FTPB as TTPB).getDataLength,(FTPB as TTPB).getBuffer));
  except
    FHandle := nil;
    raise;
  end
  else
  begin
    IBAlloc(pteb, 0, Length(FAttachments) * SizeOf(TISC_TEB));
     try
        for i := 0 to Length(FAttachments) - 1 do
        if (FAttachments[i] <> nil)  then
        begin
          pteb^[i].db_handle := @((FAttachments[i] as TFB25Attachment).Handle);
          pteb^[i].tpb_length := (FTPB as TTPB).getDataLength;
          pteb^[i].tpb_address := (FTPB as TTPB).getBuffer;
        end;
        try
          Call(isc_start_multiple(StatusVector, @FHandle,
                                   Length(FAttachments), PISC_TEB(pteb)));
        except
          FHandle := nil;
          raise;
        end;
     finally
        FreeMem(pteb);
     end;
  end;
  Inc(FSeqNo);
end;

procedure TFB25Transaction.Rollback(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_rollback_transaction(StatusVector, @FHandle),not Force);
  FHandle := nil;
end;

procedure TFB25Transaction.RollbackRetaining;
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_rollback_retaining(StatusVector, @FHandle));
end;

end.

