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
unit FB25Blob;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB,  IBHeader,IBExternals, FBClientAPI, FB25ClientAPI, FB25Attachment,
  FB25Transaction, FBActivityMonitor, FBBlob, FBOutputBlock;

type

   { TFB25BlobMetaData }

   TFB25BlobMetaData  = class(TFBBlobMetaData, IBlobMetaData)
   private
     FHasFullMetaData: boolean;
     FAttachment: TFB25Attachment;
     FTransaction: TFB25Transaction;
     FFirebird25ClientAPI: TFB25ClientAPI;
   protected
     function Attachment: IAttachment; override;
     procedure NeedFullMetadata; override;
   public
     constructor Create(Attachment: TFB25Attachment; Transaction: TFB25Transaction;
       RelationName, ColumnName: AnsiString); overload;
     constructor Create(Attachment: TFB25Attachment; Transaction: TFB25Transaction;
       RelationName, ColumnName: AnsiString; SubType: integer); overload;
  end;


  { TFB25Blob }

  TFB25Blob = class(TFBBlob,IBlob)
  private
    FHandle: TISC_BLOB_HANDLE;
    FEOB: boolean;
    FFirebird25ClientAPI: TFB25ClientAPI;
  protected
    procedure CheckReadable; override;
    procedure CheckWritable; override;
    function GetIntf: IBlob; override;
    procedure GetInfo(Request: array of byte; Response: IBlobInfo); override;
    procedure InternalClose(Force: boolean); override;
    procedure InternalCancel(Force: boolean); override;
  public
    constructor Create(Attachment: TFB25Attachment; Transaction: TFB25Transaction;
                       MetaData: IBlobMetaData; BPB: IBPB); overload;
    constructor Create(Attachment: TFB25Attachment; Transaction: TFB25Transaction;
                       SubType: integer; CharSetID: cardinal; BPB: IBPB); overload;
    constructor Create(Attachment: TFB25Attachment; Transaction: TFB25Transaction;
                       MetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB); overload;
    property Handle: TISC_BLOB_HANDLE read FHandle;

  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

uses IBErrorCodes, FBMessages, FBParamBlock, IBUtils;

{ TFB25BlobMetaData }

function TFB25BlobMetaData.Attachment: IAttachment;
begin
  Result := FAttachment;
end;

procedure TFB25BlobMetaData.NeedFullMetadata;
var
  BlobDesc: TISC_BLOB_DESC;
  RelName: AnsiString;
  ColName: AnsiString;
begin
  if FHasFullMetaData then Exit;

  FSegmentSize := 80;
  RelName := SafeAnsiUpperCase(GetRelationName);
  ColName := SafeAnsiUpperCase(GetColumnName);
  if (ColName <> '') and (RelName <> '') then
  begin
    with FFirebird25ClientAPI do
      Call(isc_blob_lookup_desc(StatusVector,@(FAttachment.Handle),
                                            @(FTransaction.Handle),
                PAnsiChar(RelName),PAnsiChar(ColName),@BlobDesc,nil));
    if FUnconfirmedCharacterSet then
      FCharSetID := BlobDesc.blob_desc_charset;
    FSubType := BlobDesc.blob_desc_subtype;
    FSegmentSize := BlobDesc.blob_desc_segment_size ;
  end;

  if FUnconfirmedCharacterSet and (FCharSetID > 1) and FAttachment.HasDefaultCharSet then
  begin
    FCharSetID := FAttachment.CharSetID;
    FUnconfirmedCharacterSet := false;
  end;


  FHasFullMetaData := true;
  FHasSubType := true;
end;

constructor TFB25BlobMetaData.Create(Attachment: TFB25Attachment;
  Transaction: TFB25Transaction; RelationName, ColumnName: AnsiString);
begin
  inherited Create(Transaction,RelationName,ColumnName);
  FAttachment := Attachment;
  FTransaction := Transaction;
  FFirebird25ClientAPI := Attachment.Firebird25ClientAPI;
  OnDatabaseError := Attachment.IBDataBaseError;
end;

constructor TFB25BlobMetaData.Create(Attachment: TFB25Attachment;
  Transaction: TFB25Transaction; RelationName, ColumnName: AnsiString;
  SubType: integer);
begin
  Create(Attachment,Transaction,RelationName,ColumnName);
  FSubType := SubType;
  FHasSubType := true;
end;

{ TFB25Blob }

procedure TFB25Blob.CheckReadable;
begin
  if FCreating or (FHandle = nil) then
    IBError(ibxeBlobCannotBeRead, [nil]);
end;

procedure TFB25Blob.CheckWritable;
begin
  if not FCreating or (FHandle = nil) then
    IBError(ibxeBlobCannotBeWritten, [nil]);
end;

function TFB25Blob.GetIntf: IBlob;
begin
  Result := self;
end;

procedure TFB25Blob.GetInfo(Request: array of byte; Response: IBlobInfo);
begin
  if FHandle = nil then
    IBError(ibxeBlobNotOpen,[nil]);

  with FFirebird25ClientAPI, Response as TBlobInfo do
    Call(isc_blob_info(StatusVector, @FHandle, Length(Request),@Request,
                                               GetBufSize, Buffer));
end;

procedure TFB25Blob.InternalClose(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  with FFirebird25ClientAPI do
    Call(isc_close_blob(StatusVector, @FHandle), not Force);
  FHandle := nil;
end;

procedure TFB25Blob.InternalCancel(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  with FFirebird25ClientAPI do
    Call(isc_cancel_blob(StatusVector,@FHandle),not Force);
  FHandle := nil;
end;

constructor TFB25Blob.Create(Attachment: TFB25Attachment; Transaction: TFB25Transaction;
  MetaData: IBlobMetaData; BPB: IBPB);
var DBHandle: TISC_DB_HANDLE;
    TRHandle: TISC_TR_HANDLE;
begin
  inherited Create(Attachment,Transaction,MetaData,BPB);
  DBHandle := Attachment.Handle;
  TRHandle := Transaction.Handle;
  FFirebird25ClientAPI := Attachment.Firebird25ClientAPI;
  OnDatabaseError := Attachment.IBDataBaseError;
  with FFirebird25ClientAPI do
  if BPB = nil then
    Call(isc_create_blob2(StatusVector, @DBHandle, @TRHandle, @FHandle, @FBlobID,
                           0, nil))
  else
  with BPB as TBPB do
    Call(isc_create_blob2(StatusVector, @DBHandle, @TRHandle, @FHandle, @FBlobID,
                         getDataLength, getBuffer));
end;

constructor TFB25Blob.Create(Attachment: TFB25Attachment;
  Transaction: TFB25Transaction; SubType: integer; CharSetID: cardinal;
  BPB: IBPB);
var DBHandle: TISC_DB_HANDLE;
    TRHandle: TISC_TR_HANDLE;
    MetaData: TFB25BlobMetaData;
begin
  MetaData := TFB25BlobMetaData.Create(Attachment,Transaction,'','',SubType);
  MetaData.FCharSetID := CharSetID;
  MetaData.FHasFullMetaData := true;
  inherited Create(Attachment,Transaction,MetaData,BPB);
  DBHandle := Attachment.Handle;
  TRHandle := Transaction.Handle;
  FFirebird25ClientAPI := Attachment.Firebird25ClientAPI;
  OnDatabaseError := Attachment.IBDataBaseError;
  with FFirebird25ClientAPI do
  if BPB = nil then
    Call(isc_create_blob2(StatusVector, @DBHandle, @TRHandle, @FHandle, @FBlobID,
                           0, nil))
  else
  with BPB as TBPB do
    Call(isc_create_blob2(StatusVector, @DBHandle, @TRHandle, @FHandle, @FBlobID,
                         getDataLength, getBuffer));
end;

constructor TFB25Blob.Create(Attachment: TFB25Attachment;
  Transaction: TFB25Transaction; MetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB);
var DBHandle: TISC_DB_HANDLE;
    TRHandle: TISC_TR_HANDLE;
begin
  inherited Create(Attachment,Transaction,MetaData,BlobID,BPB);
  DBHandle := Attachment.Handle;
  TRHandle := Transaction.Handle;
  FFirebird25ClientAPI := Attachment.Firebird25ClientAPI;
  OnDatabaseError := Attachment.IBDataBaseError;
  if (BlobID.gds_quad_high = 0) and (BlobID.gds_quad_low = 0) then
    Exit;

  with FFirebird25ClientAPI do
  if BPB = nil then
    Call(isc_open_blob2(StatusVector,  @DBHandle, @TRHandle, @FHandle,
                     @FBlobID, 0, nil))
  else
  with BPB as TBPB do
    Call(isc_open_blob2(StatusVector,  @DBHandle, @TRHandle, @FHandle,
                   @FBlobID, getDataLength, getBuffer));
end;

function TFB25Blob.Read(var Buffer; Count: Longint): Longint;
var
  BytesRead : UShort;
  LocalBuffer: PByte;
  returnCode: long;
  localCount: uShort;
begin
  CheckReadable;
  Result := 0;
  if FEOB then
    Exit;

  LocalBuffer := PByte(@Buffer);
  repeat
    if Count > MaxuShort then
      localCount := MaxuShort
    else
      localCount := Count;
    with FFirebird25ClientAPI do
      returnCode := isc_get_segment(StatusVector, @FHandle, @BytesRead, localCount,
                           LocalBuffer);
    Inc(LocalBuffer,BytesRead);
    Inc(Result,BytesRead);
    Dec(Count,BytesRead);
  until ((returncode <> 0) and (returnCode <> isc_segment)) or (Count = 0);

  FEOB := returnCode = isc_segstr_eof;
  ClearStringCache;
  if (returnCode <> 0) and (returnCode <> isc_segment) and (returnCode <> isc_segstr_eof) then
    raise EIBInterBaseError.Create(FFirebird25ClientAPI.GetStatus,ConnectionCodePage);
end;

function TFB25Blob.Write(const Buffer; Count: Longint): Longint;
var
  LocalBuffer: PByte;
  localCount: uShort;
begin
  CheckWritable;
  LocalBuffer := PByte(@Buffer);
  Result := 0;
  if Count = 0 then Exit;

  repeat
    if Count > MaxuShort then
      localCount := MaxuShort
    else
      localCount := Count;
    with FFirebird25ClientAPI do
      Call(isc_put_segment(StatusVector,@FHandle,localCount,LocalBuffer));
    Dec(Count,localCount);
    Inc(LocalBuffer,localCount);
    Inc(Result,localCount);
  until Count = 0;
  ClearStringCache;
end;


end.

