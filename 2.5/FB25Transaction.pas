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
    procedure GenerateTPB(sl: TStrings; var TPB: string; var TPBLength: Short);
    procedure CloseAll;
  public
    constructor Create(Attachments: array of IAttachment; Params: TStrings; DefaultCompletion: TTransactionCompletion); overload;
    constructor Create(Attachment: TFBAttachment; Params: TStrings; DefaultCompletion: TTransactionCompletion); overload;
    destructor Destroy; override;
    property Handle: TISC_TR_HANDLE read FHandle;

  public
    {ITransaction}
    function GetStatus: IStatus;
    function GetInTransaction: boolean;
    procedure Commit;
    procedure CommitRetaining;
    procedure Start(DefaultCompletion: TTransactionCompletion);
    procedure Rollback;
    procedure RollbackRetaining;
    property InTransaction: boolean read GetInTransaction;
  end;

implementation

uses FBErrorMessages, FB25Blob, FB25Statement;

const
  TPBPrefix = 'isc_tpb_';
  TPBConstantNames: array[1..isc_tpb_last_tpb_constant] of string = (
    'consistency',
    'concurrency',
    'shared',
    'protected',
    'exclusive',
    'wait',
    'nowait',
    'read',
    'write',
    'lock_read',
    'lock_write',
    'verb_time',
    'commit_time',
    'ignore_limbo',
    'read_committed',
    'autocommit',
    'rec_version',
    'no_rec_version',
    'restart_requests',
    'no_auto_undo'
  );

{ TFBTransaction }

{ GenerateTPB -
  Given a string containing a textual representation
  of the transaction parameters, generate a transaction
  parameter buffer, and return it and its length in
  TPB and TPBLength, respectively. }


procedure TFBTransaction.GenerateTPB(sl: TStrings; var TPB: string;
  var TPBLength: Short);
var
  i, j, TPBVal, ParamLength: Integer;
  ParamName, ParamValue: string;
begin
  TPB := '';
  if (sl.Count = 0) then
    TPBLength := 0
  else
  begin
    TPBLength := sl.Count + 1;
    TPB := TPB + Char(isc_tpb_version3);
  end;
  for i := 0 to sl.Count - 1 do
  begin
    if (Trim(sl[i]) =  '') then
    begin
      Dec(TPBLength);
      Continue;
    end;
    if (Pos('=', sl[i]) = 0) then {mbcs ok}
      ParamName := LowerCase(sl[i]) {mbcs ok}
    else
    begin
      ParamName := LowerCase(sl.Names[i]); {mbcs ok}
      ParamValue := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i])); {mbcs ok}
    end;
    if (Pos(TPBPrefix, ParamName) = 1) then {mbcs ok}
      Delete(ParamName, 1, Length(TPBPrefix));
    TPBVal := 0;
    { Find the parameter }
    for j := 1 to isc_tpb_last_tpb_constant do
      if (ParamName = TPBConstantNames[j]) then
      begin
        TPBVal := j;
        break;
      end;
    { Now act on it }
    case TPBVal of
      isc_tpb_consistency, isc_tpb_exclusive, isc_tpb_protected,
      isc_tpb_concurrency, isc_tpb_shared, isc_tpb_wait, isc_tpb_nowait,
      isc_tpb_read, isc_tpb_write, isc_tpb_ignore_limbo,
      isc_tpb_read_committed, isc_tpb_rec_version, isc_tpb_no_rec_version:
        TPB := TPB + Char(TPBVal);
      isc_tpb_lock_read, isc_tpb_lock_write:
      begin
        TPB := TPB + Char(TPBVal);
        { Now set the string parameter }
        ParamLength := Length(ParamValue);
        Inc(TPBLength, ParamLength + 1);
        TPB := TPB + Char(ParamLength) + ParamValue;
      end;
      else
      begin
        if (TPBVal > 0) and
           (TPBVal <= isc_tpb_last_tpb_constant) then
          IBError(ibxeTPBConstantNotSupported, [TPBConstantNames[TPBVal]])
        else
          IBError(ibxeTPBConstantUnknownEx, [sl.Names[i]]);
      end;
    end;
  end;
end;

procedure TFBTransaction.CloseAll;
var i: integer;
begin
  for i := 0 to OwnedObjects.Count - 1 do
    if TObject(OwnedObjects[i]) is TFBBlob then
      TFBBlob(OwnedObjects[i]).TransactionEnding(self)
    else
    if TObject(OwnedObjects[i]) is TFBStatement then
      TFBStatement(OwnedObjects[i]).TransactionEnding(self);
end;

constructor TFBTransaction.Create(Attachments: array of IAttachment;
  Params: TStrings; DefaultCompletion: TTransactionCompletion);
var
  TPB: String;
  TPBLength: short;
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

constructor TFBTransaction.Create(Attachment: TFBAttachment; Params: TStrings;
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
var i: integer;
begin
  case FDefaultCompletion of
  tcRollback:
    Rollback;
  tcCommit:
    Commit;
  end;
  inherited Destroy;
end;

function TFBTransaction.GetStatus: IStatus;
begin
  Result := Firebird25ClientAPI.Status;
end;

function TFBTransaction.GetInTransaction: boolean;
begin
  Result := FHandle <> nil;
end;

procedure TFBTransaction.Commit;
begin
  if FHandle = nil then
    Exit;
  CloseAll;
  with Firebird25ClientAPI do
    Call(isc_commit_transaction(StatusVector, @FHandle));
  FHandle := nil;
end;

procedure TFBTransaction.CommitRetaining;
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_commit_retaining(StatusVector, @FHandle));
end;

procedure TFBTransaction.Start(DefaultCompletion: TTransactionCompletion);
var pteb: PISC_TEB_ARRAY;
    i: integer;
begin
  pteb := nil;
  FDefaultCompletion := DefaultCompletion;
  with Firebird25ClientAPI do
  if (Owners.Count = 1) and (TObject(Owners[0]) is TFBAttachment) then
  try
    Call(isc_start_transaction(StatusVector, @FHandle,1,
              @(TFBAttachment(Owners[0]).Handle),FTPBLength,@FTPB));
  except
    FHandle := nil;
    raise;
  end
  else
  begin
    IBAlloc(pteb, 0, Owners.Count * SizeOf(TISC_TEB));
     try
        for i := 0 to Owners.Count - 1 do
        if (Owners[i] <> nil) and (TObject(Owners[i]) is TFBAttachment) then
        begin
          pteb^[i].db_handle := @(TFBAttachment(Owners[i]).Handle);
          pteb^[i].tpb_length := FTPBLength;
          pteb^[i].tpb_address := @FTPB;
        end;
        try
          Call(isc_start_multiple(StatusVector, @FHandle,
                                   Owners.Count, PISC_TEB(pteb)));
        except
          FHandle := nil;
          raise;
        end;
     finally
        FreeMem(pteb);
     end;
  end;
end;

procedure TFBTransaction.Rollback;
begin
  if FHandle = nil then
    Exit;
  CloseAll;
  with Firebird25ClientAPI do
    Call(isc_rollback_transaction(StatusVector, @FHandle));
  FHandle := nil;
end;

procedure TFBTransaction.RollbackRetaining;
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_rollback_retaining(StatusVector, @FHandle));
end;

end.

