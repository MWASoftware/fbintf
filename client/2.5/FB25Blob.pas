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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB,  IBHeader,IBExternals, FBClientAPI, FB25ClientAPI, FB25Attachment,
  FB25Transaction, FBActivityMonitor;

type

   { TFBBlobMetaData }

   TFBBlobMetaData  = class(TActivityReporter, IBlobMetaData)
   private
     FBlobDesc: TISC_BLOB_DESC;
     FGlobal: array [0..31] of char;
     FCharSetID: cardinal;
   public
     constructor Create(Attachment: TFB25Attachment; Transaction: TFB25Transaction;
       RelationName, ColumnName: string);

   public
     {IBlobMetaData}
    function GetSubType: integer;
    function GetCharSetID: cardinal;
    function GetSegmentSize: cardinal;
    function GetTableName: string;
    function GetColumnName: string;
  end;


  { TFBBlob }

  TFBBlob = class(TActivityReporter,IBlob)
  private
    FAttachment: TFB25Attachment;
    FHandle: TISC_BLOB_HANDLE;
    FBlobID: TISC_QUAD;
    FEOB: boolean;
    FCreating: boolean;
    FTransaction: ITransaction;
    procedure CheckReadable;
    procedure CheckWritable;
    procedure InternalClose(Force: boolean);
    procedure InternalCancel(Force: boolean);
  public
    constructor Create(Attachment: TFB25Attachment; Transaction: ITransaction); overload;
    constructor Create(Attachment: TFB25Attachment; Transaction: ITransaction;
                       BlobID: TISC_QUAD); overload;
    destructor Destroy; override;
    procedure TransactionEnding(aTransaction: TFB25Transaction; Force: boolean);
    property Handle: TISC_BLOB_HANDLE read FHandle;
    property Attachment: TFB25Attachment read FAttachment;

  {IBlob}
  public
    procedure Cancel;
    procedure Close;
    function GetBlobID: TISC_QUAD;
    function GetBlobMode: TFBBlobMode;
    procedure GetInfo(var NumSegments: Int64; var MaxSegmentSize, TotalSize: Int64;
      var BlobType: TBlobType);
    function Read(var Buffer; Count: Longint): Longint;
    function Write(const Buffer; Count: Longint): Longint;
    function LoadFromFile(Filename: string): IBlob;
    function LoadFromStream(S: TStream) : IBlob;
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(S: TStream);
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
  end;

implementation

uses IBErrorCodes, FBMessages;

{ TFBBlobMetaData }

constructor TFBBlobMetaData.Create(Attachment: TFB25Attachment;
  Transaction: TFB25Transaction; RelationName, ColumnName: string);
begin
  inherited Create(Transaction);
  with Firebird25ClientAPI do
    Call(isc_blob_lookup_desc(StatusVector,@(Attachment.Handle),
                                           @(Transaction.Handle),
                PChar(RelationName),PChar(ColumnName),@FBlobDesc,@FGlobal));
  FCharSetID := FBlobDesc.blob_desc_charset;
  if (FCharSetID > 1) and Attachment.HasDefaultCharSet then
    FCharSetID := Attachment.CharSetID
end;

function TFBBlobMetaData.GetSubType: integer;
begin
  Result := FBlobDesc.blob_desc_subtype;
end;

function TFBBlobMetaData.GetCharSetID: cardinal;
begin
  Result := FCharSetID;
end;

function TFBBlobMetaData.GetSegmentSize: cardinal;
begin
  Result := FBlobDesc.blob_desc_segment_size;
end;

function TFBBlobMetaData.GetTableName: string;
begin
  Result := strpas(FBlobDesc.blob_desc_relation_name);
end;

function TFBBlobMetaData.GetColumnName: string;
begin
  Result := strpas(FBlobDesc.blob_desc_field_name);
end;

{ TFBBlob }

procedure TFBBlob.CheckReadable;
begin
  if FCreating or (FHandle = nil) then
    IBError(ibxeBlobCannotBeRead, [nil]);
end;

procedure TFBBlob.CheckWritable;
begin
  if not FCreating or (FHandle = nil) then
    IBError(ibxeBlobCannotBeWritten, [nil]);
end;

procedure TFBBlob.InternalClose(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_close_blob(StatusVector, @FHandle), not Force);
  FHandle := nil;
end;

procedure TFBBlob.InternalCancel(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_cancel_blob(StatusVector,@FHandle),not Force);
  FHandle := nil;
end;

constructor TFBBlob.Create(Attachment: TFB25Attachment; Transaction: ITransaction
  );
var DBHandle: TISC_DB_HANDLE;
      TRHandle: TISC_TR_HANDLE;
begin
    inherited Create(Transaction as TFB25Transaction);
    FAttachment := Attachment;
    FTransaction := Transaction;
    AddMonitor(Transaction as TFB25Transaction);
    DBHandle := Attachment.Handle;
    TRHandle := (Transaction as TFB25Transaction).Handle;
    FCreating := true;
    with Firebird25ClientAPI do
      Call(isc_create_blob2(StatusVector, @DBHandle, @TRHandle, @FHandle, @FBlobID,
                           0, nil));
end;

constructor TFBBlob.Create(Attachment: TFB25Attachment;
  Transaction: ITransaction; BlobID: TISC_QUAD);
var DBHandle: TISC_DB_HANDLE;
    TRHandle: TISC_TR_HANDLE;
begin
  inherited Create(Transaction as TFB25Transaction);
  FAttachment := Attachment;
  FTransaction := Transaction;
  AddMonitor(Transaction as TFB25Transaction);
  DBHandle := Attachment.Handle;
  TRHandle := (Transaction as TFB25Transaction).Handle;
  FBlobID := BlobID;
  with Firebird25ClientAPI do
    Call(isc_open_blob2(StatusVector,  @DBHandle, @TRHandle, @FHandle,
                     @FBlobID, 0, nil));
end;

destructor TFBBlob.Destroy;
begin
  if FCreating then
    Cancel
  else
    Close;
  inherited Destroy;
end;

procedure TFBBlob.TransactionEnding(aTransaction: TFB25Transaction;
  Force: boolean);
begin
  if aTransaction <> FTransaction then
    Exit;
  if FCreating then
    InternalCancel(Force)
  else
    InternalClose(Force);
end;

procedure TFBBlob.Cancel;
begin
  InternalCancel(false);
end;

procedure TFBBlob.Close;
begin
  InternalClose(false);
end;

function TFBBlob.GetBlobID: TISC_QUAD;
begin
  Result := FBlobID;
end;

function TFBBlob.GetBlobMode: TFBBlobMode;
begin
  if FCreating then
    Result := fbmWrite
  else
    Result := fbmRead;
end;

procedure TFBBlob.GetInfo(var NumSegments: Int64; var MaxSegmentSize,
  TotalSize: Int64; var BlobType: TBlobType);
var
  items: array[0..3] of Char;
  results: array[0..99] of Char;
  i, item_length: Integer;
  item: Integer;
begin
  if FHandle = nil then
    IBError(ibxeBlobNotOpen,[nil]);

  items[0] := Char(isc_info_blob_num_segments);
  items[1] := Char(isc_info_blob_max_segment);
  items[2] := Char(isc_info_blob_total_length);
  items[3] := Char(isc_info_blob_type);

  with Firebird25ClientAPI do
  begin
    Call(isc_blob_info(StatusVector, @FHandle, 4, @items[0], SizeOf(results),
                    @results[0]));
    i := 0;
    while (i < SizeOf(results)) and (results[i] <> Char(isc_info_end)) do
    begin
      item := Integer(results[i]); Inc(i);
      item_length := isc_portable_integer(@results[i], 2); Inc(i, 2);
      case item of
        isc_info_blob_num_segments:
          NumSegments := isc_portable_integer(@results[i], item_length);
        isc_info_blob_max_segment:
          MaxSegmentSize := isc_portable_integer(@results[i], item_length);
        isc_info_blob_total_length:
          TotalSize := isc_portable_integer(@results[i], item_length);
        isc_info_blob_type:
          if isc_portable_integer(@results[i], item_length) = 0 then
            BlobType := btSegmented
          else
            BlobType := btStream;
      end;
      Inc(i, item_length);
    end;
  end;
end;

function TFBBlob.Read(var Buffer; Count: Longint): Longint;
var
  BytesRead : UShort;
  LocalBuffer: PChar;
  returnCode: long;
  localCount: uShort;
begin
  CheckReadable;
  Result := 0;
  if FEOB then
    Exit;

  LocalBuffer := PChar(@Buffer);
  repeat
    if Count > MaxuShort then
      localCount := MaxuShort
    else
      localCount := Count;
    with Firebird25ClientAPI do
      returnCode := isc_get_segment(StatusVector, @FHandle, @BytesRead, localCount,
                           LocalBuffer);
    Inc(LocalBuffer,BytesRead);
    Inc(Result,BytesRead);
    Dec(Count,BytesRead);
  until ((returncode <> 0) and (returnCode <> isc_segment)) or (Count = 0);

  FEOB := returnCode = isc_segstr_eof;
  if (returnCode <> 0) and (returnCode <> isc_segment) and (returnCode <> isc_segstr_eof) then
    Firebird25ClientAPI.IBDataBaseError
end;

function TFBBlob.Write(const Buffer; Count: Longint): Longint;
var
  LocalBuffer: PChar;
  localCount: uShort;
begin
  CheckWritable;
  LocalBuffer := PChar(@Buffer);
  Result := 0;
  if Count = 0 then Exit;

  repeat
    if Count > MaxuShort then
      localCount := MaxuShort
    else
      localCount := Count;
    with Firebird25ClientAPI do
      Call(isc_put_segment(StatusVector,@FHandle,localCount,LocalBuffer));
    Dec(Count,localCount);
    Inc(LocalBuffer,localCount);
    Inc(Result,localCount);
  until Count = 0;
end;

function TFBBlob.LoadFromFile(Filename: string): IBlob;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

const BufSize = 8 * 1024;

function TFBBlob.LoadFromStream(S: TStream): IBlob;
var Buffer: array [0..BufSize-1] of char;
    BytesRead: integer;
begin
  CheckWritable;
  S.Position := 0;
  repeat
    BytesRead := S.Read(Buffer,BufSize);
    Write(Buffer,BytesRead);
  until BytesRead = 0;
  Close;
  Result := self;
end;

procedure TFBBlob.SaveToFile(Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TFBBlob.SaveToStream(S: TStream);
var Buffer: array [0..BufSize-1] of char;
    BytesRead: integer;
begin
  CheckReadable;
  repeat
    BytesRead := Read(Buffer,BufSize);
    S.Write(Buffer,BytesRead);
  until BytesRead = 0;
  Close;
end;

function TFBBlob.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TFBBlob.GetTransaction: ITransaction;
begin
  Result := FTransaction;
end;

end.

