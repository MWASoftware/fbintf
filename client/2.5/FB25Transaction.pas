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
unit FB25Transaction;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}
{$R-}

interface

uses
  Classes, SysUtils, IB, FBClientAPI, FB25ClientAPI, IBHeader,
  FB25Attachment, FBActivityMonitor, FBTransaction;

type
  { TFB25Transaction }

  TFB25Transaction = class(TFBTransaction,ITransaction, IActivityMonitor)
  private
    FHandle: TISC_TR_HANDLE;
    FFirebird25ClientAPI: TFB25ClientAPI;
  protected
    function GetActivityIntf(att: IAttachment): IActivityMonitor; override;
    function GetTrInfo(ReqBuffer: PByte; ReqBufLen: integer): ITrInformation; override;
    procedure SetInterface(api: TFBClientAPI); override;
    procedure SetErrorHandler(Attachment: IAttachment); override;
    procedure InternalStartSingle(attachment: IAttachment); override;
    procedure InternalStartMultiple; override;
    function InternalCommit(Force: boolean): TTrCompletionState; override;
    function InternalRollback(Force: boolean): TTrCompletionState; override;
    procedure InternalCommitRetaining; override;
    procedure InternalRollbackRetaining; override;
  public
    property Handle: TISC_TR_HANDLE read FHandle;

  public
    {ITransaction}
    function GetInTransaction: boolean; override;
    procedure PrepareForCommit; override;
 end;

implementation

uses FBMessages, FBParamBlock;

{ TFB25Transaction }

function TFB25Transaction.GetActivityIntf(att: IAttachment): IActivityMonitor;
begin
  Result := (att as TFB25Attachment);
end;

function TFB25Transaction.GetTrInfo(ReqBuffer: PByte; ReqBufLen: integer
  ): ITrInformation;
begin
  Result := TTrInformation.Create(FFirebird25ClientAPI);
  with FFirebird25ClientAPI, Result as TTrInformation do
  begin
     if isc_transaction_info(StatusVector, @(FHandle), ReqBufLen, ReqBuffer,
                               getBufSize, Buffer) > 0 then
     raise EIBInterBaseError.Create(GetStatus,ConnectionCodePage);
  end;
end;

procedure TFB25Transaction.SetInterface(api: TFBClientAPI);
begin
  inherited SetInterface(api);
  FFirebird25ClientAPI := api as TFB25ClientAPI;
end;

procedure TFB25Transaction.SetErrorHandler(Attachment: IAttachment);
begin
  if Attachment <> nil then
       OnDatabaseError := (Attachment as TFB25Attachment).IBDataBaseError;
end;

procedure TFB25Transaction.InternalStartSingle(attachment: IAttachment);
var db_handle: TISC_DB_HANDLE;
begin
  with FFirebird25ClientAPI do
  try
    db_handle := (attachment as TFB25Attachment).Handle;
    Call(isc_start_transaction(StatusVector, @FHandle,1,
              @db_handle,(FTPB as TTPB).getDataLength,(FTPB as TTPB).getBuffer));
  except
    FHandle := nil;
    raise;
  end
end;

procedure TFB25Transaction.InternalStartMultiple;
var pteb: PISC_TEB_ARRAY;
    i: integer;
begin
  pteb := nil;
  with FFirebird25ClientAPI do
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
end;

function TFB25Transaction.InternalCommit(Force: boolean): TTrCompletionState;
begin
  Result := trCommitted;
  with FFirebird25ClientAPI do
    if Call(isc_commit_transaction(StatusVector, @FHandle),not Force) > 0 then
      Result := trCommitFailed;
  FHandle := nil;
end;

function TFB25Transaction.InternalRollback(Force: boolean): TTrCompletionState;
begin
  Result := trRolledback;
  with FFirebird25ClientAPI do
    if Call(isc_rollback_transaction(StatusVector, @FHandle),not Force) > 0 then
      Result := trRollbackFailed;
  FHandle := nil;
end;

procedure TFB25Transaction.InternalCommitRetaining;
begin
  with FFirebird25ClientAPI do
    Call(isc_commit_retaining(StatusVector, @FHandle));
end;

procedure TFB25Transaction.InternalRollbackRetaining;
begin
  with FFirebird25ClientAPI do
    Call(isc_rollback_retaining(StatusVector, @FHandle));
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
  with FFirebird25ClientAPI do
    Call(isc_prepare_transaction(StatusVector, @FHandle));
end;

end.

