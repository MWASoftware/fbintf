(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API. Although predominantly
 *  a new development they include source code taken from IBX and may be
 *  considered a derived product. This software thus also includes the copyright
 *  notice and license conditions from IBX.
 *
 *  Except for those parts dervied from IBX, contents of this file are subject
 *  to the Initial Developer's Public License Version 1.0 (the "License"); you
 *  may not use this file except in compliance with the License. You may obtain a
 *  copy of the License here:
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
{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011 - 2015                                                }
{                                                                        }
{************************************************************************}
unit FBTransaction;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBParamBlock, FBActivityMonitor, FBClientAPI, FBOutputBlock;

type
  { TFBTransaction }

  TFBTransaction = class(TActivityReporter, IActivityMonitor,ITransaction)
  private
    FFirebirdAPI: TFBClientAPI;
    function GenerateTPB(sl: array of byte): ITPB;
  protected
    FTPB: ITPB;
    FSeqNo: integer;
    FDefaultCompletion: TTransactionCompletion;
    FAttachments: array of IAttachment; {Keep reference to attachment - ensures
                                          attachment cannot be freed before transaction}
    FTransactionName: AnsiString;
    procedure CheckHandle;
    function GetActivityIntf(att: IAttachment): IActivityMonitor; virtual; abstract;
    function GetJournalIntf(Attachment: IAttachment): IJournallingHook;
    procedure SetInterface(api: TFBClientAPI); virtual;
    function GetTrInfo(ReqBuffer: PByte; ReqBufLen: integer): ITrInformation; virtual; abstract;
    procedure InternalStartSingle(attachment: IAttachment); virtual; abstract;
    procedure InternalStartMultiple; virtual; abstract;
    procedure InternalCommit(Force: boolean); virtual; abstract;
    procedure InternalCommitRetaining; virtual; abstract;
    procedure InternalRollback(Force: boolean); virtual; abstract;
    procedure InternalRollbackRetaining; virtual; abstract;
  public
    constructor Create(api: TFBClientAPI; Attachments: array of IAttachment; Params: array of byte; DefaultCompletion: TTransactionAction; aName: AnsiString); overload;
    constructor Create(api: TFBClientAPI; Attachments: array of IAttachment; TPB: ITPB; DefaultCompletion: TTransactionAction; aName: AnsiString); overload;
    constructor Create(api: TFBClientAPI; Attachment: IAttachment; Params: array of byte; DefaultCompletion: TTransactionAction; aName: AnsiString); overload;
    constructor Create(api: TFBClientAPI; Attachment: IAttachment; TPB: ITPB; DefaultCompletion: TTransactionAction; aName: AnsiString); overload;
    destructor Destroy; override;
    procedure DoDefaultTransactionEnd(Force: boolean);
    property FirebirdAPI: TFBClientAPI read FFirebirdAPI;

  public
    {ITransaction}
    function getTPB: ITPB;
    procedure PrepareForCommit;virtual; abstract;
    procedure Commit(Force: boolean=false);
    procedure CommitRetaining;
    function GetInTransaction: boolean; virtual; abstract;
    function GetIsReadOnly: boolean;
    function GetTransactionID: integer;
    function GetAttachmentCount: integer;
    function GetAttachment(index: integer): IAttachment;
    function GetJournalingActive(attachment: IAttachment): boolean;
    function GetDefaultCompletion: TTransactionCompletion;
    procedure Rollback(Force: boolean=false);
    procedure RollbackRetaining;
    procedure Start(DefaultCompletion: TTransactionCompletion=taCommit); overload;
    procedure Start(TPB: ITPB; DefaultCompletion: TTransactionCompletion=taCommit); overload;
    function GetTrInformation(Requests: array of byte): ITrInformation; overload;
    function GetTrInformation(Request: byte): ITrInformation; overload;
    function GetTransactionName: AnsiString;
    procedure SetTransactionName(aValue: AnsiString);

    property InTransaction: boolean read GetInTransaction;
    property TransactionSeqNo: integer read FSeqNo;
  end;

  {The transaction user interface is used to force an action on the end of the
   transaction.}

  ITransactionUser = interface
    ['{156fcdc9-a326-44b3-a82d-f23c6fb9f97c}']
    procedure TransactionEnding(aTransaction: ITransaction; Force: boolean);
  end;

  { TTPBItem }

  TTPBItem = class(TParamBlockItem,ITPBItem)
  public
    function getParamTypeName: AnsiString; override;
  end;

  { TTPB }

  TTPB = class (TCustomParamBlock<TTPBItem,ITPBItem>, ITPB)
  protected
    function LookupItemType(ParamTypeName: AnsiString): byte; override;
  public
    constructor Create(api: TFBClientAPI);
    function GetParamTypeName(ParamType: byte): Ansistring;
    {$IFDEF FPC}
    function ITPB.GetDPBParamTypeName = GetParamTypeName;
    {$ELSE}
    function GetDPBParamTypeName(ParamType: byte): Ansistring;
    {$ENDIF}
    function AsText: AnsiString;
end;

  {$IFDEF FPC}
  TTrInfoItem = class;

  { TTrInfoItem }

  TTrInfoItem = class(TOutputBlockItemGroup<TTrInfoItem,ITrInfoItem>,ITrInfoItem)
  {$ELSE}
    TTrInfoItem = class(TOutputBlockItemGroup<TOutputBlockItem,ITrInfoItem>,ITrInfoItem)
  {$ENDIF}
    public
      procedure DecodeTraIsolation(var IsolationType, RecVersion: byte);
  end;

  { TTrInformation }

  TTrInformation = class(TCustomOutputBlock<TTrInfoItem,ITrInfoItem>, ITrInformation)
  protected
    procedure DoParseBuffer; override;
  public
    constructor Create(api: TFBClientAPI; aSize: integer = DefaultBufferSize);
    {$IFNDEF FPC}
    function Find(ItemType: byte): ITrInfoItem;
    {$ENDIF}
  end;


implementation

uses FBMessages;

const
  isc_tpb_last_tpb_constant = isc_tpb_at_snapshot_number;

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
    'no_auto_undo',
    'lock_timeout',
    'read_consistency',
    'at_snapshot_number'
  );

{ TFBTransaction }

function TFBTransaction.GenerateTPB(sl: array of byte): ITPB;
var
  i: Integer;
begin
  Result := TTPB.Create(FFirebirdAPI);
  for i := 0 to Length(sl) - 1 do
    Result.Add(sl[i]);
end;

procedure TFBTransaction.CheckHandle;
begin
  if not InTransaction then
    IBError(ibxeNotInTransaction,[]);
end;

function TFBTransaction.GetJournalIntf(Attachment: IAttachment): IJournallingHook;
begin
  Attachment.QueryInterface(IJournallingHook,Result)
end;

procedure TFBTransaction.SetInterface(api: TFBClientAPI);
begin
  FFirebirdAPI := api;
end;

constructor TFBTransaction.Create(api: TFBClientAPI; Attachments: array of IAttachment;
  Params: array of byte; DefaultCompletion: TTransactionAction; aName: AnsiString);
begin
  Create(api, Attachments,GenerateTPB(Params), DefaultCompletion, aName);
end;

constructor TFBTransaction.Create(api: TFBClientAPI; Attachments: array of IAttachment; TPB: ITPB;
  DefaultCompletion: TTransactionAction; aName: AnsiString);
var
  i: Integer;
begin
  inherited Create(nil);
  FTransactionName := aName;
  SetInterface(api);
  if Length(Attachments) = 0 then
    IBError(ibxeEmptyAttachmentsList,[nil]);

  {make sure all attachments use same Firebird API}
  for i := 0 to Length(Attachments) - 1 do
    if Attachments[i].getFirebirdAPI.GetFBLibrary.GetHandle <> FFirebirdAPI.GetFBLibrary.GetHandle then
      IBError(ibxeDifferentAPIs,[nil]);

  SetLength(FAttachments,Length(Attachments));
  for i := 0 to Length(Attachments) - 1 do
  begin
    AddMonitor(GetActivityIntf(Attachments[i]));
    FAttachments[i] := Attachments[i];
  end;
  FTPB := TPB;
  Start(DefaultCompletion);
end;

constructor TFBTransaction.Create(api: TFBClientAPI; Attachment: IAttachment;
  Params: array of byte; DefaultCompletion: TTransactionAction; aName: AnsiString);
begin
  Create(api,Attachment,GenerateTPB(Params),DefaultCompletion,aName);
end;

constructor TFBTransaction.Create(api: TFBClientAPI; Attachment: IAttachment; TPB: ITPB;
  DefaultCompletion: TTransactionAction; aName: AnsiString);
begin
  inherited Create(nil);
  SetInterface(api);
  AddMonitor(GetActivityIntf(Attachment));
  SetLength(FAttachments,1);
  FAttachments[0] := Attachment;
  FTPB := TPB;
  FTransactionName := aName;
  Start(DefaultCompletion);
end;

destructor TFBTransaction.Destroy;
begin
  DoDefaultTransactionEnd(false);
  inherited Destroy;
end;

procedure TFBTransaction.DoDefaultTransactionEnd(Force: boolean);
var i: integer;
    intf: IUnknown;
    user: ITransactionUser;
begin
  if InTransaction then
  begin
    for i := 0 to InterfaceCount - 1 do
    begin
      intf := GetInterface(i);
      if (intf <> nil) and  (intf.QueryInterface(ITransactionUser,user) = S_OK) then
        user.TransactionEnding(self,Force);
    end;
    case FDefaultCompletion of
    taRollback:
      Rollback(Force);
    taCommit:
      Commit(Force);
    end;
  end;
end;

function TFBTransaction.getTPB: ITPB;
begin
  Result := FTPB;
end;

procedure TFBTransaction.Commit(Force: boolean);
var i: integer;
    TransactionID: integer;
    TransactionEndNeeded: array of boolean;
begin
  if not GetInTransaction then Exit;

  SetLength(TransactionEndNeeded,Length(FAttachments));
  TransactionID := GetTransactionID;
  for i := 0 to Length(FAttachments) - 1 do
    if (FAttachments[i] <> nil)  then
      TransactionEndNeeded[i] := GetJournalingActive(FAttachments[i])
    else
      TransactionEndNeeded[i] := false;
  InternalCommit(Force);
  for i := 0 to Length(FAttachments) - 1 do
    if TransactionEndNeeded[i] then
       GetJournalIntf(FAttachments[i]).TransactionEnd(TransactionID, TACommit);
end;

procedure TFBTransaction.CommitRetaining;
var i: integer;
    TransactionID: integer;
begin
  if not GetInTransaction then Exit;

  TransactionID := GetTransactionID;
  InternalCommitRetaining;
  for i := 0 to Length(FAttachments) - 1 do
    if (FAttachments[i] <> nil) and GetJournalingActive(FAttachments[i]) then
       GetJournalIntf(FAttachments[i]).TransactionRetained(self,TransactionID, TACommitRetaining);
end;

function TFBTransaction.GetIsReadOnly: boolean;
var Info: ITrInformation;
begin
  Info := GetTrInformation(isc_info_tra_access);
  if (Info.Count > 0) and (Info[0].getItemType = isc_info_tra_access) then
    Result := Info[0].getAsInteger = isc_info_tra_readonly
  else
    Result := false;
end;

function TFBTransaction.GetTransactionID: integer;
var Info: ITrInformation;
begin
  Result := -1;
  Info := GetTrInformation(isc_info_tra_id);
  if (Info.Count > 0) and (Info[0].getItemType = isc_info_tra_id) then
    Result := Info[0].getAsInteger;
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

function TFBTransaction.GetJournalingActive(attachment: IAttachment): boolean;
begin
  Result := false;
  if (attachment = nil) and (length(FAttachments) > 0) then
    attachment := FAttachments[0];
  if attachment <> nil then
  with attachment do
  Result := self.GetInTransaction and JournalingActive and
     ((((joReadOnlyTransactions in GetJournalOptions) and self.GetIsReadOnly)) or
     ((joReadWriteTransactions in GetJournalOptions) and not self.GetIsReadOnly));
end;

function TFBTransaction.GetDefaultCompletion: TTransactionCompletion;
begin
  Result := FDefaultCompletion;
end;

procedure TFBTransaction.Rollback(Force: boolean);
var i: integer;
    TransactionID: integer;
    TransactionEndNeeded: array of boolean;
begin
  if not GetInTransaction then Exit;

  SetLength(TransactionEndNeeded,Length(FAttachments));
  TransactionID := GetTransactionID;
  for i := 0 to Length(FAttachments) - 1 do
    if (FAttachments[i] <> nil)  then
      TransactionEndNeeded[i] := GetJournalingActive(FAttachments[i])
    else
      TransactionEndNeeded[i] := false;
  InternalRollback(Force);
  for i := 0 to Length(FAttachments) - 1 do
    if TransactionEndNeeded[i] then
       GetJournalIntf(FAttachments[i]).TransactionEnd(TransactionID, TARollback);
end;

procedure TFBTransaction.RollbackRetaining;
var i: integer;
    TransactionID: integer;
begin
  if not GetInTransaction then Exit;

  TransactionID := GetTransactionID;
  InternalRollbackRetaining;
  for i := 0 to Length(FAttachments) - 1 do
    if (FAttachments[i] <> nil) and GetJournalingActive(FAttachments[i]) then
       GetJournalIntf(FAttachments[i]).TransactionRetained(self,TransactionID,TARollbackRetaining);
end;

procedure TFBTransaction.Start(DefaultCompletion: TTransactionCompletion);
var i: integer;
begin
  if GetInTransaction then
    Exit;

  FDefaultCompletion := DefaultCompletion;

  if Length(FAttachments) = 1 then
    InternalStartSingle(FAttachments[0])
  else
    InternalStartMultiple;
  for i := 0 to Length(FAttachments) - 1 do
    if (FAttachments[i] <> nil) and GetJournalingActive(FAttachments[i]) then
      GetJournalIntf(FAttachments[i]).TransactionStart(self);
  Inc(FSeqNo);
end;

procedure TFBTransaction.Start(TPB: ITPB; DefaultCompletion: TTransactionCompletion
  );
begin
  FTPB := TPB;
  Start(DefaultCompletion);
end;

function TFBTransaction.GetTrInformation(Requests: array of byte
  ): ITrInformation;
var ReqBuffer: PByte;
    i: integer;
begin
  CheckHandle;
  if Length(Requests) = 1 then
    Result := GetTrInformation(Requests[0])
  else
  begin
    GetMem(ReqBuffer,Length(Requests));
    try
      for i := 0 to Length(Requests) - 1 do
        ReqBuffer[i] := Requests[i];

      Result := GetTrInfo(ReqBuffer,Length(Requests));

    finally
      FreeMem(ReqBuffer);
    end;
  end;
end;

function TFBTransaction.GetTrInformation(Request: byte): ITrInformation;
begin
  CheckHandle;
  Result := GetTrInfo(@Request,1);
end;

function TFBTransaction.GetTransactionName: AnsiString;
begin
  Result := FTransactionName;
end;

procedure TFBTransaction.SetTransactionName(aValue: AnsiString);
begin
  FTransactionName := aValue;
end;

{ TTPBItem }

function TTPBItem.getParamTypeName: AnsiString;
begin
  Result :=  TPBPrefix + TPBConstantNames[getParamType];
end;


{TTPB}

constructor TTPB.Create(api: TFBClientAPI);
begin
  inherited Create(api);
  FDataLength := 1;
  FBuffer^ := isc_tpb_version3;
end;

function TTPB.GetParamTypeName(ParamType: byte): Ansistring;
begin
  if ParamType <= isc_tpb_last_tpb_constant then
    Result := TPBPrefix + TPBConstantNames[ParamType]
  else
    Result := '';
end;

function TTPB.AsText: AnsiString;
var i: integer;
begin
  Result := '[';
  for i := 0 to getCount - 1 do
  begin
    Result := Result + GetParamTypeName(getItems(i).getParamType);
    if i < getCount - 1 then
      Result := Result + ',';
  end;
  Result := Result + ']';
end;

{$IFNDEF FPC}
function TTPB.GetDPBParamTypeName(ParamType: byte): Ansistring;
begin
  Result := GetParamTypeName(ParamType);
end;
{$ENDIF}


function TTPB.LookupItemType(ParamTypeName: AnsiString): byte;
var i: byte;
begin
  Result := 0;
  ParamTypeName := LowerCase(ParamTypeName);
  if (Pos(TPBPrefix, ParamTypeName) = 1) then
    Delete(ParamTypeName, 1, Length(TPBPrefix));

  for i := 1 to isc_tpb_last_tpb_constant do
    if (ParamTypeName = TPBConstantNames[i]) then
    begin
      Result := i;
      break;
    end;
end;

{ TTrInfoItem }

procedure TTrInfoItem.DecodeTraIsolation(var IsolationType, RecVersion: byte);
begin
  with FFirebirdClientAPI, ItemData^ do
  if getItemType = isc_info_tra_isolation then
  begin
    if FDataLength = 1 then
    begin
      IsolationType := getAsInteger;
      RecVersion := 0;
    end
    else
    begin
      IsolationType := (FBufPtr + 3)^;
      RecVersion := (FBufPtr + 4)^;
    end
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FBufPtr^)]);
end;

{ TTrInformation }

procedure TTrInformation.DoParseBuffer;
var P: PByte;
    index: integer;
begin
  P := Buffer;
  index := 0;
  SetLength(FItems,0);
  while (P^ <> isc_info_end) and (P < Buffer + getBufSize) do
  begin
    SetLength(FItems,index+1);
    case byte(P^) of
    isc_info_tra_id,
    isc_info_tra_oldest_interesting,
    isc_info_tra_oldest_active,
    isc_info_tra_oldest_snapshot,
    fb_info_tra_snapshot_number,
    isc_info_tra_lock_timeout:
      FItems[index] := AddIntegerItem(P);

    isc_info_tra_isolation,
      {return transaction isolation mode of current transaction.
	format of returned clumplets is following:

	isc_info_tra_isolation,
		1, isc_info_tra_consistency | isc_info_tra_concurrency
	|
		2, isc_info_tra_read_committed,
			 isc_info_tra_no_rec_version | isc_info_tra_rec_version

	i.e. for read committed transactions returned 2 items while for
        other transactions returned 1 item}

    isc_info_tra_access:
      FItems[index] := AddIntegerItem(P);
    fb_info_tra_dbpath:
      FItems[index] := AddStringItem(P);
    else
      FItems[index] := AddItem(P);
    end;
    P := P + FItems[index]^.FSize;
    Inc(index);
  end;
end;

constructor TTrInformation.Create(api: TFBClientAPI; aSize: integer);
begin
  inherited Create(api,aSize);
  FIntegerType := dtInteger;
end;

{$IFNDEF FPC}
function TTrInformation.Find(ItemType: byte): ITrInfoItem;
begin
  Result := inherited Find(ItemType);
  if Result.GetSize = 0 then
    Result := nil;
end;
{$ENDIF}

end.

