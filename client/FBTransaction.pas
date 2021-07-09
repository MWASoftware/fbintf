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
  Classes, SysUtils, IB, FBParamBlock, FBActivityMonitor, FBClientAPI;

type
  { TFBTransaction }

  TFBTransaction = class(TActivityReporter, IActivityMonitor,ITransaction)
  private
    FFirebirdAPI: TFBClientAPI;
    function GenerateTPB(sl: array of byte): ITPB;
  protected
    FTPB: ITPB;
    FSeqNo: integer;
    FDefaultCompletion: TTransactionAction;
    FAttachments: array of IAttachment; {Keep reference to attachment - ensures
                                          attachment cannot be freed before transaction}
    function GetActivityIntf(att: IAttachment): IActivityMonitor; virtual; abstract;
    procedure SetInterface(api: TFBClientAPI); virtual;
  public
    constructor Create(api: TFBClientAPI; Attachments: array of IAttachment; Params: array of byte; DefaultCompletion: TTransactionAction); overload;
    constructor Create(api: TFBClientAPI; Attachments: array of IAttachment; TPB: ITPB; DefaultCompletion: TTransactionAction); overload;
    constructor Create(api: TFBClientAPI; Attachment: IAttachment; Params: array of byte; DefaultCompletion: TTransactionAction); overload;
    constructor Create(api: TFBClientAPI; Attachment: IAttachment; TPB: ITPB; DefaultCompletion: TTransactionAction); overload;
    destructor Destroy; override;
    procedure DoDefaultTransactionEnd(Force: boolean);
    property FirebirdAPI: TFBClientAPI read FFirebirdAPI;

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
    procedure Start(DefaultCompletion: TTransactionCompletion=taCommit); overload; virtual; abstract;
    procedure Start(TPB: ITPB; DefaultCompletion: TTransactionCompletion=taCommit); overload;

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
    function ITPB.GetDPBParamTypeName = GetParamTypeName;
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

procedure TFBTransaction.SetInterface(api: TFBClientAPI);
begin
  FFirebirdAPI := api;
end;

constructor TFBTransaction.Create(api: TFBClientAPI; Attachments: array of IAttachment;
  Params: array of byte; DefaultCompletion: TTransactionAction);
begin
  Create(api, Attachments,GenerateTPB(Params), DefaultCompletion);
end;

constructor TFBTransaction.Create(api: TFBClientAPI; Attachments: array of IAttachment; TPB: ITPB;
  DefaultCompletion: TTransactionAction);
var
  i: Integer;
begin
  inherited Create(nil);
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
  Params: array of byte; DefaultCompletion: TTransactionAction);
begin
  Create(api,Attachment,GenerateTPB(Params),DefaultCompletion);
end;

constructor TFBTransaction.Create(api: TFBClientAPI; Attachment: IAttachment; TPB: ITPB;
  DefaultCompletion: TTransactionAction);
begin
  inherited Create(nil);
  SetInterface(api);
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

procedure TFBTransaction.Start(TPB: ITPB; DefaultCompletion: TTransactionCompletion
  );
begin
  FTPB := TPB;
  Start(DefaultCompletion);
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
    Result := TPBConstantNames[ParamType]
  else
    Result := '';
end;

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

end.

