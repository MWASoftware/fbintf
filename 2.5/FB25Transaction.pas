unit FB25Transaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, FBStatus;

type

  { TFBTransaction }

  TFBTransaction = class(TInterfacedObject,ITransaction)
  private
    FClientAPI: TFBClientAPI;
    FHandle: TISC_TR_HANDLE;
    procedure GenerateTPB(sl: TStrings; var TPB: string; var TPBLength: Short);
  public
    constructor Create(ClientAPI: TFBClientAPI;
               Databases: array of IAttachment; Params: TStrings); overload;
    constructor Create(ClientAPI: TFBClientAPI;
                Database: IAttachment; Params; TStrings); overload;

    {ITransaction}
    function GetStatus: IStatus; virtual;
    procedure Commit; virtual;
    procedure CommitRetaining; virtual;
    procedure Rollback; virtual;
    procedure RollbackRetaining; virtual;
    property Handle: TISC_TR_HANDLE read FHandle;
  end;

implementation

uses FBErrorMessages, IBHeader, FB25Attachment;

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

constructor TFBTransaction.Create(ClientAPI: TFBClientAPI;
  Databases: array of IAttachment; Params: TStrings);
var
  pteb: PISC_TEB_ARRAY;
  TPB: String;
  TPBLength: short;
  i: Integer;
begin
  inherited Create;
  FClientAPI := ClientAPI;
  GenerateTPB(Params, TPB, TPBLength);

  pteb := nil;
  with FClientAPI do
  begin
    IBAlloc(pteb, 0, Length(Databases) * SizeOf(TISC_TEB));
     try
        for i := 0 to Length(Databases) - 1 do
        if Databases[i] <> nil then
        begin
          pteb^[i].db_handle := @((Databases[i] as TFBAttachment).Handle);
          pteb^[i].tpb_length := TPBLength;
          pteb^[i].tpb_address := @TPB;
        end;
        try
          Call(isc_start_multiple(StatusVector, @FHandle,
                                   DatabaseCount, PISC_TEB(pteb)));
        except
          FHandle := nil;
          raise;
        end;
     finally
        FreeMem(pteb);
     end;
  end;
end;

constructor TFBTransaction.Create(ClientAPI: TFBClientAPI;
  Database: IAttachment; Params; TStrings);
var
  TPB: String;
  TPBLength: short;
begin
  inherited Create;
  FClientAPI := ClientAPI;
  GenerateTPB(Params, TPB, TPBLength);

  with FClientAPI do
  try
    Call(isc_start_transaction(StatusVector, @FHandle,1,
              @((Database as TFBAttachment).Handle),TPBLength,@TPB));
  except
    FHandle := nil;
    raise;
  end;
end;

function TFBTransaction.GetStatus: IStatus;
begin
  Result := FClientAPI.Status;
end;

procedure TFBTransaction.Commit;
begin
  with FClientAPI do
    Call(isc_commit_transaction(StatusVector, @FHandle));
  Free;
end;

procedure TFBTransaction.CommitRetaining;
begin
  with FClientAPI do
    Call(isc_commit_retaining(StatusVector, @FHandle));
end;

procedure TFBTransaction.Rollback;
begin
  with FClientAPI do
    Call(isc_rollback_transaction(StatusVector, @FHandle));
  Free;
end;

procedure TFBTransaction.RollbackRetaining;
begin
  with FClientAPI do
    Call(isc_rollback_retaining(StatusVector, @FHandle));
end;

end.

