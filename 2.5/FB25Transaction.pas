unit FB25Transaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBLibrary, FB25ClientAPI, IBHeader, IBExternals,
  FB25Attachment;

type

  { TFBTransaction }

  TFBTransaction = class(TObjectOwner,ITransaction)
  private
    FOwners: array of TFBAttachment;
    FHandle: TISC_TR_HANDLE;
    FTPB: String;
    FTPBLength: short;
    procedure GenerateTPB(sl: TStrings; var TPB: string; var TPBLength: Short);
    procedure CloseAll;
  public
    constructor Create(Attachments: array of TFBAttachment; Params: TStrings); overload;
    constructor Create(Attachment: TFBAttachment; Params: TStrings); overload;
    destructor Destroy; override;
    property Handle: TISC_TR_HANDLE read FHandle;

  public
    {ITransaction}
    function GetStatus: IStatus;
    function GetInTransaction: boolean;
    procedure Commit;
    procedure CommitRetaining;
    procedure Start;
    procedure Release;
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

constructor TFBTransaction.Create(Attachments: array of TFBAttachment;
  Params: TStrings);
var
  TPB: String;
  TPBLength: short;
  i: Integer;
begin
  inherited Create;
  if Length(Attachments) = 0 then
    IBError(ibxEmptyAttachmentsList,[nil]);

  setLength(FOwners,Length(Attachments));
  for i := 0 to Length(Attachments) - 1 do
  begin
    FOwners[i] := Attachments[i];
    FOwners[i].RegisterObj(self);
  end;
  GenerateTPB(Params, FTPB, FTPBLength);
  Start;
end;

constructor TFBTransaction.Create(Attachment: TFBAttachment; Params: TStrings);
begin
  inherited Create;
  setLength(FOwners,1);
  FOwners[0] := Attachment;
  FOwners[0].RegisterObj(self);
  GenerateTPB(Params, FTPB, FTPBLength);
  Start;
end;

destructor TFBTransaction.Destroy;
var i: integer;
begin
  Rollback;
  for i := 0 to Length(FOwners) - 1 do
    FOwners[i].UnRegisterObj(self);
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

procedure TFBTransaction.Start;
var pteb: PISC_TEB_ARRAY;
    i: integer;
begin
  pteb := nil;
  with Firebird25ClientAPI do
  if Length(FOwners) = 1 then
  try
    Call(isc_start_transaction(StatusVector, @FHandle,1,
              @(FOwners[0].Handle),FTPBLength,@FTPB));
  except
    FHandle := nil;
    raise;
  end
  else
  begin
    IBAlloc(pteb, 0, Length(FOwners) * SizeOf(TISC_TEB));
     try
        for i := 0 to Length(FOwners) - 1 do
        if FOwners[i] <> nil then
        begin
          pteb^[i].db_handle := @(FOwners[i].Handle);
          pteb^[i].tpb_length := FTPBLength;
          pteb^[i].tpb_address := @FTPB;
        end;
        try
          Call(isc_start_multiple(StatusVector, @FHandle,
                                   Length(FOwners), PISC_TEB(pteb)));
        except
          FHandle := nil;
          raise;
        end;
     finally
        FreeMem(pteb);
     end;
  end;
end;

procedure TFBTransaction.Release;
begin
  if OwnedObjects.Count > 0 then
    IBError(ibxTransactionReleaseFails,[OwnedObjects.Count]);
  Rollback;
  Free;
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

