(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  The contents of this file are subject to the Initial Developer's
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
unit FB30Blob;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, Firebird, IB, IBHeader, IBExternals, FBClientAPI, FB30ClientAPI, FB30Attachment,
  FBTransaction, FB30Transaction,  FBBlob, FBOutputBlock;

type

   { TFB30BlobMetaData }

   TFB30BlobMetaData  = class(TFBBlobMetaData, IBlobMetaData)
   private
     FHasFullMetaData: boolean;
     FAttachment: TFB30Attachment;
     FTransaction: TFB30Transaction;
   protected
     function Attachment: IAttachment; override;
     procedure NeedFullMetadata; override;
   public
     constructor Create(Attachment: TFB30Attachment; Transaction: TFB30Transaction;
       RelationName, ColumnName: AnsiString); overload;
     constructor Create(Attachment: TFB30Attachment; Transaction: TFB30Transaction;
       RelationName, ColumnName: AnsiString; SubType: integer); overload;

  end;


  { TFB30Blob }

  TFB30Blob = class(TFBBlob,IBlob)
  private
    FBlobIntf: Firebird.IBlob;
    FEOB: boolean;
    FFirebird30ClientAPI: TFB30ClientAPI;
  protected
    procedure CheckReadable; override;
    procedure CheckWritable; override;
    function GetIntf: IBlob; override;
    procedure GetInfo(Request: array of byte; Response: IBlobInfo); override;
    procedure InternalClose(Force: boolean); override;
    procedure InternalCancel(Force: boolean); override;
  public
    constructor Create(Attachment: TFB30Attachment; Transaction: TFB30Transaction;
                       MetaData: IBlobMetaData; BPB: IBPB); overload;
    constructor Create(Attachment: TFB30Attachment; Transaction: TFB30Transaction;
                       SubType: integer; CharSetID: cardinal; BPB: IBPB); overload;
    constructor Create(Attachment: TFB30Attachment; Transaction: TFBTransaction;
                       MetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB); overload;
    property BlobIntf: Firebird.IBlob read FBlobIntf;

  {IBlob}
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

uses FBMessages, FB30Statement, FBParamBlock, Math;

const
  sLookupBlobMetaData = 'Select F.RDB$FIELD_SUB_TYPE, F.RDB$SEGMENT_LENGTH, F.RDB$CHARACTER_SET_ID, F.RDB$FIELD_TYPE '+
    'From RDB$FIELDS F JOIN RDB$RELATION_FIELDS R On R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+
  'Where Trim(R.RDB$RELATION_NAME) = Upper(Trim(?)) and Trim(R.RDB$FIELD_NAME) = Upper(Trim(?)) ' +
  'UNION '+
  'Select F.RDB$FIELD_SUB_TYPE, F.RDB$SEGMENT_LENGTH, F.RDB$CHARACTER_SET_ID, F.RDB$FIELD_TYPE '+
  'From RDB$FIELDS F JOIN RDB$PROCEDURE_PARAMETERS P On P.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+
    'Where Trim(P.RDB$PROCEDURE_NAME) = Upper(Trim(?)) and Trim(P.RDB$PARAMETER_NAME) = Upper(Trim(?)) ';

{ TFB30BlobMetaData }

function TFB30BlobMetaData.Attachment: IAttachment;
begin
  Result := FAttachment;
end;

procedure TFB30BlobMetaData.NeedFullMetadata;
var stmt: IStatement;
begin
  if FHasFullMetaData then Exit;

  FSegmentSize := 80;
  if (GetColumnName <> '') and (GetRelationName <> '') then
  begin
    stmt := TFB30Statement.Create(FAttachment,FTransaction, sLookupBlobMetaData ,FAttachment.SQLDialect);
    with stmt do
    begin
      SQLParams[0].AsString := GetRelationName;
      SQLParams[1].AsString := GetColumnName;
      SQLParams[2].AsString := GetRelationName;
      SQLParams[3].AsString := GetColumnName;
      with OpenCursor do
      if FetchNext then
      begin
        if Data[3].AsInteger <> blr_blob then
          IBError(ibxeInvalidBlobMetaData,[nil]);
        FSubType := Data[0].AsInteger;
        FSegmentSize := Data[1].AsInteger;
        if FUnconfirmedCharacterSet then
          FCharSetID := Data[2].AsInteger;
      end
      else
        IBError(ibxeInvalidBlobMetaData,[nil]);

    end;
  end;

    if FUnconfirmedCharacterSet and (FCharSetID > 1) and FAttachment.HasDefaultCharSet then
    begin
      FCharSetID := FAttachment.CharSetID;
      FUnconfirmedCharacterSet := false;
    end;

  FHasFullMetaData := true;
  FHasSubType := true;
end;

constructor TFB30BlobMetaData.Create(Attachment: TFB30Attachment;
  Transaction: TFB30Transaction; RelationName, ColumnName: AnsiString);
begin
  inherited Create(Transaction,RelationName,ColumnName);
  FAttachment := Attachment;
  FTransaction := Transaction;
end;

constructor TFB30BlobMetaData.Create(Attachment: TFB30Attachment;
  Transaction: TFB30Transaction; RelationName, ColumnName: AnsiString;
  SubType: integer);
begin
  Create(Attachment,Transaction,RelationName,ColumnName);
  FSubType := SubType;
  FHasSubType := true;
end;

{ TFB30Blob }

procedure TFB30Blob.CheckReadable;
begin
  if FCreating or (FBlobIntf = nil) then
    IBError(ibxeBlobCannotBeRead, [nil]);
end;

procedure TFB30Blob.CheckWritable;
begin
  if not FCreating or (FBlobIntf = nil) then
    IBError(ibxeBlobCannotBeWritten, [nil]);
end;

function TFB30Blob.GetIntf: IBlob;
begin
  Result := self;
end;

procedure TFB30Blob.GetInfo(Request: array of byte; Response: IBlobInfo);
begin
  if FBlobIntf = nil then
    IBError(ibxeBlobNotOpen,[nil]);

  with FFirebird30ClientAPI, Response as TBlobInfo do
  begin
    FBlobIntf.getInfo(StatusIntf,Length(Request),BytePtr(@Request),
                                               GetBufSize, BytePtr(Buffer));
    Check4DataBaseError;
    SignalActivity;
  end;
end;

procedure TFB30Blob.InternalClose(Force: boolean);
begin
  if FBlobIntf = nil then
    Exit;
  with FFirebird30ClientAPI do
  begin
    FBlobIntf.close(StatusIntf);
    if not Force then Check4DataBaseError;
  end;
  FBlobIntf.release;
  FBlobIntf := nil;
end;

procedure TFB30Blob.InternalCancel(Force: boolean);
begin
  if FBlobIntf = nil then
    Exit;
  with FFirebird30ClientAPI do
  begin
    FBlobIntf.cancel(StatusIntf);
    if not Force then Check4DataBaseError;
  end;
  FBlobIntf.release;
  FBlobIntf := nil;
end;

constructor TFB30Blob.Create(Attachment: TFB30Attachment; Transaction: TFB30Transaction;
  MetaData: IBlobMetaData; BPB: IBPB);
begin
    inherited Create(Attachment,Transaction,MetaData,BPB);
    FFirebird30ClientAPI := Attachment.Firebird30ClientAPI;
    with FFirebird30ClientAPI do
    begin
      if BPB = nil then
        FBlobIntf := Attachment.AttachmentIntf.createBlob(StatusIntf,Transaction.TransactionIntf,
                        @FBlobID,0,nil)
      else
      with BPB as TBPB do
        FBlobIntf := Attachment.AttachmentIntf.createBlob(StatusIntf,Transaction.TransactionIntf,
                      @FBlobID,getDataLength, BytePtr(getBuffer));
      Check4DataBaseError;
    end;
end;

constructor TFB30Blob.Create(Attachment: TFB30Attachment;
  Transaction: TFB30Transaction; SubType: integer; CharSetID: cardinal;
  BPB: IBPB);
var MetaData: TFB30BlobMetaData;
begin
  MetaData := TFB30BlobMetaData.Create(Attachment,Transaction,'','',SubType);
  MetaData.FCharSetID := CharSetID;
  MetaData.FHasFullMetaData := true;
  inherited Create(Attachment,Transaction,MetaData,BPB);
  FFirebird30ClientAPI := Attachment.Firebird30ClientAPI;
  with FFirebird30ClientAPI do
  begin
    if BPB = nil then
      FBlobIntf := Attachment.AttachmentIntf.createBlob(StatusIntf,Transaction.TransactionIntf,
                      @FBlobID,0,nil)
    else
    with BPB as TBPB do
      FBlobIntf := Attachment.AttachmentIntf.createBlob(StatusIntf,Transaction.TransactionIntf,
                    @FBlobID,getDataLength, BytePtr(getBuffer));
    Check4DataBaseError;
  end;
end;

constructor TFB30Blob.Create(Attachment: TFB30Attachment;
  Transaction: TFBTransaction; MetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB);
begin
  inherited Create(Attachment,Transaction,MetaData,BlobID,BPB);
  FFirebird30ClientAPI := Attachment.Firebird30ClientAPI;
  if (BlobID.gds_quad_high = 0) and (BlobID.gds_quad_low = 0) then
    Exit;

  with FFirebird30ClientAPI do
  begin
    if BPB = nil then
      FBlobIntf := Attachment.AttachmentIntf.openBlob(StatusIntf,(Transaction as TFB30Transaction).TransactionIntf,
                     @FBlobID, 0, nil)
    else
    with BPB as TBPB do
    FBlobIntf := Attachment.AttachmentIntf.openBlob(StatusIntf,(Transaction as TFB30Transaction).TransactionIntf,
                   @FBlobID, getDataLength, BytePtr(getBuffer));
    Check4DataBaseError;
  end;
end;

function TFB30Blob.Read(var Buffer; Count: Longint): Longint;
var
  BytesRead : cardinal;
  LocalBuffer: PAnsiChar;
  returnCode: integer;
  localCount: uShort;
begin
  CheckReadable;
  Result := 0;
  if FEOB then
    Exit;

  LocalBuffer := PAnsiChar(@Buffer);
  repeat
    localCount := Min(Count,MaxuShort);
    with FFirebird30ClientAPI do
      returnCode := FBlobIntf.getSegment(StatusIntf,localCount, LocalBuffer, @BytesRead);
    SignalActivity;
    Inc(LocalBuffer,BytesRead);
    Inc(Result,BytesRead);
    Dec(Count,BytesRead);
  until ((returncode <> Firebird.IStatus.Result_OK) and (returnCode <> Firebird.IStatus.Result_SEGMENT)) or (Count = 0);

  FEOB := returnCode = Firebird.IStatus.RESULT_NO_DATA;
  ClearStringCache;
  if (returnCode <> Firebird.IStatus.Result_OK) and
     (returnCode <> Firebird.IStatus.Result_SEGMENT) and
     (returnCode <> Firebird.IStatus.RESULT_NO_DATA) then
    FFirebird30ClientAPI.IBDataBaseError
end;

function TFB30Blob.Write(const Buffer; Count: Longint): Longint;
var
  LocalBuffer: PAnsiChar;
  localCount: uShort;
begin
  CheckWritable;
  Result := 0;
  if Count = 0 then Exit;

  LocalBuffer := PAnsiChar(@Buffer);
  repeat
    localCount := Min(Count,MaxuShort);
    with FFirebird30ClientAPI do
    begin
      FBlobIntf.putSegment(StatusIntf,localCount,LocalBuffer);
      Check4DataBaseError;
    end;
    Inc(LocalBuffer,localCount);
    Inc(Result,localCount);
    Dec(Count,localCount);
  until Count = 0;
  ClearStringCache;
  SignalActivity;
end;

end.

