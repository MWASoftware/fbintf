unit FBTransaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBParamBlock, FBActivityMonitor;

type
  { TFBTransaction }

  TFBTransaction = class(TActivityReporter, IActivityMonitor,ITransaction)
  private
    function GenerateTPB(sl: array of byte): ITPB;
  protected
    FTPB: ITPB;
    FDefaultCompletion: TTransactionAction;
    FAttachments: array of IAttachment; {Keep reference to attachment - ensures
                                          attachment cannot be freed before transaction}
    function GetActivityIntf(att: IAttachment): IActivityMonitor; virtual; abstract;
  public
    constructor Create(Attachments: array of IAttachment; Params: array of byte; DefaultCompletion: TTransactionAction); overload;
    constructor Create(Attachments: array of IAttachment; TPB: ITPB; DefaultCompletion: TTransactionAction); overload;
    constructor Create(Attachment: IAttachment; Params: array of byte; DefaultCompletion: TTransactionAction); overload;
    constructor Create(Attachment: IAttachment; TPB: ITPB; DefaultCompletion: TTransactionAction); overload;
    destructor Destroy; override;
    procedure DoDefaultTransactionEnd(Force: boolean);

  public
    {ITransaction}
    function getTPB: ITPB;
    procedure PrepareForCommit;virtual; abstract;
    procedure Commit(Force: boolean=false);  virtual; abstract;
    procedure CommitRetaining;  virtual; abstract;
    function GetInTransaction: boolean; virtual; abstract;
    function GetAttachmentCount: integer;
    function GetAttachment(index: integer): IAttachment;
    procedure Rollback(Force: boolean=false);  virtual; abstract;
    procedure RollbackRetaining;  virtual; abstract;
    procedure Start(DefaultCompletion: TTransactionAction=taCommit); overload; virtual; abstract;
    procedure Start(TPB: ITPB; DefaultCompletion: TTransactionAction=taCommit); overload;

    property InTransaction: boolean read GetInTransaction;
  end;

implementation

uses FBMessages;

{ TFBTransaction }

function TFBTransaction.GenerateTPB(sl: array of byte): ITPB;
var
  i: Integer;
begin
  Result := TTPB.Create;
  for i := 0 to Length(sl) - 1 do
    Result.Add(sl[i]);
end;

constructor TFBTransaction.Create(Attachments: array of IAttachment;
  Params: array of byte; DefaultCompletion: TTransactionAction);
begin
  Create(Attachments,GenerateTPB(Params), DefaultCompletion);
end;

constructor TFBTransaction.Create(Attachments: array of IAttachment; TPB: ITPB;
  DefaultCompletion: TTransactionAction);
var
  i: Integer;
begin
  inherited Create(nil);
  if Length(Attachments) = 0 then
    IBError(ibxeEmptyAttachmentsList,[nil]);

  SetLength(FAttachments,Length(Attachments));
  for i := 0 to Length(Attachments) - 1 do
  begin
    AddMonitor(GetActivityIntf(Attachments[i]));
    FAttachments[i] := Attachments[i];
  end;
  FTPB := TPB;
  Start(DefaultCompletion);
end;

constructor TFBTransaction.Create(Attachment: IAttachment;
  Params: array of byte; DefaultCompletion: TTransactionAction);
begin
  Create(Attachment,GenerateTPB(Params),DefaultCompletion);
end;

constructor TFBTransaction.Create(Attachment: IAttachment; TPB: ITPB;
  DefaultCompletion: TTransactionAction);
begin
  inherited Create(nil);
  AddMonitor(GetActivityIntf(Attachment));
  SetLength(FAttachments,1);
  FAttachments[0] := Attachment;
  FTPB := TPB;
  Start(DefaultCompletion);
end;

destructor TFBTransaction.Destroy;
begin
  DoDefaultTransactionEnd(false);
  inherited Destroy;
end;

procedure TFBTransaction.DoDefaultTransactionEnd(Force: boolean);
begin
  if InTransaction then
  case FDefaultCompletion of
  taRollback:
    Rollback(Force);
  taCommit:
    Commit(Force);
  end;
end;

function TFBTransaction.getTPB: ITPB;
begin
  Result := FTPB;
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

procedure TFBTransaction.Start(TPB: ITPB; DefaultCompletion: TTransactionAction
  );
begin
  FTPB := TPB;
  Start(DefaultCompletion);
end;

end.

