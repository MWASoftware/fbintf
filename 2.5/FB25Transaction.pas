unit FB25Transaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBLibrary, FB25ClientAPI, IBHeader, IBExternals,
  FB25Attachment, FB25APIObject;

type

  { TFBTransaction }

  TFBTransaction = class(TAPIObject,ITransaction)
  private
    FHandle: TISC_TR_HANDLE;
    FTPB: String;
    FTPBLength: short;
    FDefaultCompletion: TTransactionCompletion;
    FAttachments: array of IAttachment; {Keep reference to attachment - ensures
                                          attachment cannot be freed before transaction}
    procedure GenerateTPB(sl: array of byte; var TPB: string; var TPBLength: Short);
    procedure CloseAll(Force: boolean);
  public
    constructor Create(Attachments: array of IAttachment; Params: array of byte; DefaultCompletion: TTransactionCompletion); overload;
    constructor Create(Attachment: TFBAttachment; Params: array of byte; DefaultCompletion: TTransactionCompletion); overload;
    destructor Destroy; override;
    procedure DoDefaultTransactionEnd(Force: boolean);
    property Handle: TISC_TR_HANDLE read FHandle;

  public
    {ITransaction}
    function GetInTransaction: boolean;
    procedure PrepareForCommit;
    procedure Commit(Force: boolean=false);
    procedure CommitRetaining;
    function Start(DefaultCompletion: TTransactionCompletion=tcCommit): ITransaction;
    procedure Rollback(Force: boolean=false);
    procedure RollbackRetaining;
    function GetAttachmentCount: integer;
    function GetAttachment(index: integer): IAttachment;
    property InTransaction: boolean read GetInTransaction;
  end;

implementation

uses FBErrorMessages, FB25Blob, FB25Statement, FB25Array;


{ TFBTransaction }


procedure TFBTransaction.GenerateTPB(sl: array of byte; var TPB: string;
  var TPBLength: Short);
var
  i: Integer;
begin
  TPB := '';
  if (Length(sl) = 0) then
    TPBLength := 0
  else
  begin
    SetLength(TPB,Length(sl) + 1);
    TPB[1] :=  Char(isc_tpb_version3);
    for i := 0 to Length(sl) -1 do
       TPB[i+2] := char(sl[i]);
    TPBLength := Length(sl) +1;
  end;
end;

procedure TFBTransaction.CloseAll(Force: boolean);
var i: integer;
begin
  for i := 0 to OwnedObjects.Count - 1 do
    if TObject(OwnedObjects[i]) is TFBBlob then
      TFBBlob(OwnedObjects[i]).TransactionEnding(self,Force)
    else
    if TObject(OwnedObjects[i]) is TFBStatement then
      TFBStatement(OwnedObjects[i]).TransactionEnding(self,Force)
  else
  if TObject(OwnedObjects[i]) is TFBArray then
    TFBArray(OwnedObjects[i]).TransactionEnding(self,Force);
end;

constructor TFBTransaction.Create(Attachments: array of IAttachment;
  Params: array of byte; DefaultCompletion: TTransactionCompletion);
var
  i: Integer;
begin
  inherited Create;
  if Length(Attachments) = 0 then
    IBError(ibxeEmptyAttachmentsList,[nil]);

  SetLength(FAttachments,Length(Attachments));
  for i := 0 to Length(Attachments) - 1 do
  begin
    AddOwner(Attachments[i] as TFBAttachment);
    FAttachments[i] := Attachments[i];
  end;
  GenerateTPB(Params, FTPB, FTPBLength);
  Start(DefaultCompletion);
end;

constructor TFBTransaction.Create(Attachment: TFBAttachment; Params: array of byte;
   DefaultCompletion: TTransactionCompletion);
begin
  inherited Create;
  AddOwner(Attachment);
  SetLength(FAttachments,1);
  FAttachments[0] := Attachment;
  GenerateTPB(Params, FTPB, FTPBLength);
  Start(DefaultCompletion);
end;

destructor TFBTransaction.Destroy;
begin
  DoDefaultTransactionEnd(false);
  inherited Destroy;
end;

procedure TFBTransaction.DoDefaultTransactionEnd(Force: boolean);
begin
  if FHandle <> nil then
  case FDefaultCompletion of
  tcRollback:
    Rollback(Force);
  tcCommit:
    Commit(Force);
  end;
end;

function TFBTransaction.GetInTransaction: boolean;
begin
  Result := FHandle <> nil;
end;

procedure TFBTransaction.PrepareForCommit;
begin
  if Length(FAttachments) < 2 then
    IBError(ibxeNotAMultiDatabaseTransaction,[nil]);
  if FHandle = nil then
    Exit;
  CloseAll(false);
  with Firebird25ClientAPI do
    Call(isc_prepare_transaction(StatusVector, @FHandle));
end;

procedure TFBTransaction.Commit(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  CloseAll(Force);
  with Firebird25ClientAPI do
    Call(isc_commit_transaction(StatusVector, @FHandle),not Force);
  FHandle := nil;
end;

procedure TFBTransaction.CommitRetaining;
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_commit_retaining(StatusVector, @FHandle));
end;

function TFBTransaction.Start(DefaultCompletion: TTransactionCompletion
  ): ITransaction;
var pteb: PISC_TEB_ARRAY;
    i: integer;
    db_handle: TISC_DB_HANDLE;
begin
  Result := self;
  if FHandle <> nil then
    Exit;
  pteb := nil;
  FDefaultCompletion := DefaultCompletion;
  with Firebird25ClientAPI do
  if (Length(FAttachments) = 1)  then
  try
    db_handle := (FAttachments[0] as TFBAttachment).Handle;
    Call(isc_start_transaction(StatusVector, @FHandle,1,
              @db_handle,FTPBLength,PChar(FTPB)));
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
          pteb^[i].db_handle := @((FAttachments[i] as TFBAttachment).Handle);
          pteb^[i].tpb_length := FTPBLength;
          pteb^[i].tpb_address := PChar(FTPB);
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
end;

procedure TFBTransaction.Rollback(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  CloseAll(Force);
  with Firebird25ClientAPI do
    Call(isc_rollback_transaction(StatusVector, @FHandle),not Force);
  FHandle := nil;
end;

procedure TFBTransaction.RollbackRetaining;
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_rollback_retaining(StatusVector, @FHandle));
end;

function TFBTransaction.GetAttachmentCount: integer;
begin
  Result := Length(FAttachments);
end;

function TFBTransaction.GetAttachment(index: integer): IAttachment;
begin
  if (index >= 0) and (index < Length(FAttachments)) then
    Result := FAttachments[index]
  else
    IBError(ibxeAttachmentListIndexError,[index]);
end;

end.

