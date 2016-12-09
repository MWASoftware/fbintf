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

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$interfaces COM}
{$ENDIF}

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
    FSeqNo: integer;
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
    procedure Start(DefaultCompletion: TTransactionCompletion=taCommit); overload; virtual; abstract;
    procedure Start(TPB: ITPB; DefaultCompletion: TTransactionCompletion=taCommit); overload;

    property InTransaction: boolean read GetInTransaction;
    property TransactionSeqNo: integer read FSeqNo;
  end;

implementation

uses FBMessages, FBStatement;

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
var i: integer;
    intf: TInterfacedObject;
begin
  if InTransaction then
  begin
    for i := 0 to InterfaceCount - 1 do
    begin
      intf := GetInterface(i);
      if (intf <> nil) and  (intf is TFBStatement) then
        TFBStatement(intf).TransactionEnding(self,Force);
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

end.

