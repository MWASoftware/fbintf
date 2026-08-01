(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  This file is part of the pure Pascal wire protocol implementation
 *  (no fbclient required) and is subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBWireBlob;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBBlob, FBClientAPI, FBTransaction, FBActivityMonitor,
  FBOutputBlock, FBWireClientAPI, FBWireProtocol;

type
  { TFBWireBlobMetaData }

  TFBWireBlobMetaData = class(TFBBlobMetaData,IBlobMetaData)
  private
    FAttachmentIntf: IAttachment;
    FTransactionIntf: ITransaction;
  protected
    function Attachment: IAttachment; override;
    procedure NeedFullMetadata; override;
  public
    {aSubTypeKnown is true when the caller supplies the definitive subtype
     and character set (statement metadata); false makes NeedFullMetadata
     look the column up in the system tables}
    constructor Create(aAttachment: TObject; aTransaction: TObject;
                aRelationName, aColumnName: AnsiString;
                aSubType: integer; aCharSetID: cardinal;
                aSubTypeKnown: boolean = true);
  end;

  { TFBWireBlob }

  TFBWireBlob = class(TFBBlob,IBlob)
  private
    FWireAPI: TFBWireClientAPI;
    FWireAttachment: TObject;
    FHandle: integer;
    FHasHandle: boolean;
    FInline: boolean;           {served from the inline blob cache - no
                                 server side handle exists}
    FInlineInfo: TBytes;        {the pushed blob info response}
    FEOB: boolean;              {end of blob seen while reading}
    FReadBuffer: TBytes;        {segment data not yet consumed by Read}
    FReadPos: integer;
    function GetConnection: TFBWireConnection;
    function GetTransactionHandle: integer;
    procedure InternalOpen(const aBlobID: TISC_QUAD);
    procedure InternalCreate;
    function BPBBytes: TBytes;
  protected
    procedure CheckReadable; override;
    procedure CheckWritable; override;
    function GetIntf: IBlob; override;
    procedure GetInfo(Request: array of byte; Response: IBlobInfo); override;
    procedure InternalClose(Force: boolean); override;
    procedure InternalCancel(Force: boolean); override;
  public
    {open an existing blob}
    constructor Create(aAttachment: IAttachment; aTransaction: TFBTransaction;
                aMetaData: IBlobMetaData; aBlobID: TISC_QUAD; aBPB: IBPB); overload;
    {create a new blob}
    constructor Create(aAttachment: IAttachment; aTransaction: TFBTransaction;
                aMetaData: IBlobMetaData; aBPB: IBPB); overload;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

uses FBMessages, IBErrorCodes, IBHeader, FBWireAttachment, FBWireTransaction,
  FBWireConst;

const
  MaxSegmentSize = 32000;

  {the same system table lookup the 3.0 provider uses}
  sLookupBlobMetaData = 'Select F.RDB$FIELD_SUB_TYPE, F.RDB$SEGMENT_LENGTH, F.RDB$CHARACTER_SET_ID, F.RDB$FIELD_TYPE '+
    'From RDB$FIELDS F JOIN RDB$RELATION_FIELDS R On R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+
  'Where Trim(R.RDB$RELATION_NAME) = Upper(Trim(?)) and Trim(R.RDB$FIELD_NAME) = Upper(Trim(?)) ' +
  'UNION '+
  'Select F.RDB$FIELD_SUB_TYPE, F.RDB$SEGMENT_LENGTH, F.RDB$CHARACTER_SET_ID, F.RDB$FIELD_TYPE '+
    'From RDB$FIELDS F JOIN RDB$PROCEDURE_PARAMETERS P On P.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+
    'Where Trim(P.RDB$PROCEDURE_NAME) = Upper(Trim(?)) and Trim(P.RDB$PARAMETER_NAME) = Upper(Trim(?)) ';

{ TFBWireBlobMetaData }

constructor TFBWireBlobMetaData.Create(aAttachment: TObject;
  aTransaction: TObject; aRelationName, aColumnName: AnsiString;
  aSubType: integer; aCharSetID: cardinal; aSubTypeKnown: boolean);
begin
  inherited Create(aTransaction as TFBWireTransaction,aRelationName,aColumnName);
  FAttachmentIntf := (aAttachment as TFBWireAttachment) as IAttachment;
  FTransactionIntf := (aTransaction as TFBWireTransaction) as ITransaction;
  FSubType := aSubType;
  FCharSetID := aCharSetID;
  FSegmentSize := MaxSegmentSize;
  FHasSubType := aSubTypeKnown;
  FUnconfirmedCharacterSet := not aSubTypeKnown;
end;

function TFBWireBlobMetaData.Attachment: IAttachment;
begin
  Result := FAttachmentIntf;
end;

procedure TFBWireBlobMetaData.NeedFullMetadata;
var RS: IResultSet;
begin
  {When created from statement metadata the subtype and character set are
   already definitive. Otherwise the column is looked up in the system
   tables, exactly as the 3.0 provider does.}
  if FHasSubType then Exit;
  FSegmentSize := 80;
  if (GetColumnName <> '') and (GetRelationName <> '') then
  begin
    RS := FAttachmentIntf.OpenCursor(FTransactionIntf,sLookupBlobMetaData,
            [GetRelationName,GetColumnName,GetRelationName,GetColumnName]);
    if RS.FetchNext then
    begin
      if RS[3].AsInteger <> blr_blob then
        IBError(ibxeInvalidBlobMetaData,[nil]);
      FSubType := RS[0].AsInteger;
      FSegmentSize := RS[1].AsInteger;
      if FUnconfirmedCharacterSet then
        FCharSetID := RS[2].AsInteger;
    end
    else
      IBError(ibxeInvalidBlobMetaData,[nil]);
    RS.Close;
  end;
  if FUnconfirmedCharacterSet and (FCharSetID > 1) and
     FAttachmentIntf.HasDefaultCharSet then
  begin
    FCharSetID := FAttachmentIntf.GetDefaultCharSetID;
    FUnconfirmedCharacterSet := false;
  end;
  FHasSubType := true;
end;

{ TFBWireBlob }

constructor TFBWireBlob.Create(aAttachment: IAttachment;
  aTransaction: TFBTransaction; aMetaData: IBlobMetaData; aBlobID: TISC_QUAD;
  aBPB: IBPB);
begin
  inherited Create(aAttachment,aTransaction,aMetaData,aBlobID,aBPB);
  FWireAttachment := aAttachment as TObject;
  FWireAPI := (FWireAttachment as TFBWireAttachment).WireAPI;
  InternalOpen(aBlobID);
end;

constructor TFBWireBlob.Create(aAttachment: IAttachment;
  aTransaction: TFBTransaction; aMetaData: IBlobMetaData; aBPB: IBPB);
begin
  inherited Create(aAttachment,aTransaction,aMetaData,aBPB);
  FWireAttachment := aAttachment as TObject;
  FWireAPI := (FWireAttachment as TFBWireAttachment).WireAPI;
  InternalCreate;
end;

function TFBWireBlob.GetConnection: TFBWireConnection;
begin
  Result := (FWireAttachment as TFBWireAttachment).Connection;
end;

function TFBWireBlob.GetTransactionHandle: integer;
begin
  Result := (GetTransaction as TObject as TFBWireTransaction).Handle;
end;

function TFBWireBlob.BPBBytes: TBytes;
begin
  Result := ParamBlockToBytes(GetBPB);
end;

procedure TFBWireBlob.InternalOpen(const aBlobID: TISC_QUAD);
var id: Int64;
    data: TBytes;
    pos, segLen, outLen: integer;
begin
  id := (Int64(aBlobID.gds_quad_high) shl 32) or Int64(cardinal(aBlobID.gds_quad_low));

  {a protocol 19 server may have pushed this blob with the row that
   references it. On a hit the open is free of wire traffic: the
   segmented stream is unpacked here and Read serves it from the buffer.}
  if (FWireAttachment as TFBWireAttachment).TakeInlineBlob(
       GetTransactionHandle,id,FInlineInfo,data) then
  begin
    SetLength(FReadBuffer,Length(data));
    pos := 0;
    outLen := 0;
    while pos + 2 <= Length(data) do
    begin
      segLen := data[pos] or (data[pos+1] shl 8);
      Inc(pos,2);
      if pos + segLen > Length(data) then
        segLen := Length(data) - pos;
      if segLen > 0 then
      begin
        Move(data[pos],FReadBuffer[outLen],segLen);
        Inc(outLen,segLen);
        Inc(pos,segLen);
      end;
    end;
    SetLength(FReadBuffer,outLen);
    FReadPos := 0;
    FEOB := true;
    FInline := true;
    Exit;
  end;

  try
    FHandle := GetConnection.OpenBlob(GetTransactionHandle,BPBBytes,id);
    FHasHandle := true;
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  FEOB := false;
  SetLength(FReadBuffer,0);
  FReadPos := 0;
end;

procedure TFBWireBlob.InternalCreate;
var id: Int64;
begin
  try
    GetConnection.CreateBlob(GetTransactionHandle,BPBBytes,FHandle,id);
    FHasHandle := true;
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  FBlobID.gds_quad_high := Integer(id shr 32);
  FBlobID.gds_quad_low := Cardinal(id and $FFFFFFFF);
end;

procedure TFBWireBlob.CheckReadable;
begin
  if not (FHasHandle or FInline) then
    IBError(ibxeBlobCannotBeRead,[nil]);
end;

procedure TFBWireBlob.CheckWritable;
begin
  if not FHasHandle then
    IBError(ibxeBlobCannotBeWritten,[nil]);
end;

function TFBWireBlob.GetIntf: IBlob;
begin
  Result := self;
end;

procedure TFBWireBlob.GetInfo(Request: array of byte; Response: IBlobInfo);
var items, resp: TBytes;
    i, len: integer;
begin
  CheckReadable;
  if FInline then
  begin
    {the server pushed the info with the blob: num_segments, max_segment,
     total_length and type - the items every fbintf caller asks for}
    len := Length(FInlineInfo);
    if len > (Response as TBlobInfo).getBufSize then
      len := (Response as TBlobInfo).getBufSize;
    if len > 0 then
      Move(FInlineInfo[0],(Response as TBlobInfo).Buffer^,len);
    Exit;
  end;
  SetLength(items,Length(Request));
  for i := 0 to High(Request) do
    items[i] := Request[i];
  SetLength(resp,0);
  try
    resp := GetConnection.GetInfo(op_info_blob,FHandle,items,DefaultBufferSize);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  len := Length(resp);
  if len > (Response as TBlobInfo).getBufSize then
    len := (Response as TBlobInfo).getBufSize;
  if len > 0 then
    Move(resp[0],(Response as TBlobInfo).Buffer^,len);
end;

procedure TFBWireBlob.InternalClose(Force: boolean);
begin
  if FInline then
  begin
    {nothing exists server side}
    FInline := false;
    Exit;
  end;
  if not FHasHandle then Exit;
  if not GetConnection.Connected then
  begin
    FHasHandle := false;
    Exit;
  end;
  try
    GetConnection.CloseBlob(FHandle);
  except
    on E: Exception do
      if not Force then WireIBError(FWireAPI,E);
  end;
  FHasHandle := false;
end;

procedure TFBWireBlob.InternalCancel(Force: boolean);
begin
  if FInline then
  begin
    FInline := false;
    Exit;
  end;
  if not FHasHandle then Exit;
  if not GetConnection.Connected then
  begin
    FHasHandle := false;
    Exit;
  end;
  try
    GetConnection.CancelBlob(FHandle);
  except
    on E: Exception do
      if not Force then WireIBError(FWireAPI,E);
  end;
  FHasHandle := false;
end;

function TFBWireBlob.Read(var Buffer; Count: Longint): Longint;
var p: PByte;
    available, chunk: integer;
begin
  CheckReadable;
  p := @Buffer;
  Result := 0;
  while Count > 0 do
  begin
    available := Length(FReadBuffer) - FReadPos;
    if available = 0 then
    begin
      if FEOB then break;
      try
        FReadBuffer := GetConnection.GetSegment(FHandle,MaxSegmentSize,FEOB);
      except
        on E: Exception do WireIBError(FWireAPI,E);
      end;
      FReadPos := 0;
      available := Length(FReadBuffer);
      if available = 0 then break;
    end;
    chunk := available;
    if chunk > Count then chunk := Count;
    Move(FReadBuffer[FReadPos],p^,chunk);
    Inc(FReadPos,chunk);
    Inc(p,chunk);
    Dec(Count,chunk);
    Inc(Result,chunk);
  end;
end;

function TFBWireBlob.Write(const Buffer; Count: Longint): Longint;
var p: PByte;
    chunk: integer;
    seg: TBytes;
begin
  CheckWritable;
  p := @Buffer;
  Result := 0;
  while Count > 0 do
  begin
    chunk := Count;
    if chunk > MaxSegmentSize then
      chunk := MaxSegmentSize;
    SetLength(seg,chunk);
    Move(p^,seg[0],chunk);
    try
      GetConnection.PutSegment(FHandle,seg);
    except
      on E: Exception do WireIBError(FWireAPI,E);
    end;
    Inc(p,chunk);
    Dec(Count,chunk);
    Inc(Result,chunk);
  end;
end;

end.
