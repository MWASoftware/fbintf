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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Firebird, IB, IBHeader, IBExternals, FBClientAPI, FB30ClientAPI, FB30Attachment,
  FB30Transaction, FBActivityMonitor;

type

   { TFBBlobMetaData }

   TFBBlobMetaData  = class(TActivityReporter, IBlobMetaData)
   private
     FSubType: cardinal;
     FCharSetID: cardinal;
     FSegmentSize: cardinal;
     FRelationName: string;
     FColumnName: string;
   public
     constructor Create(Attachment: TFB30Attachment; Transaction: TFB30Transaction;
       RelationName, ColumnName: string);

   public
     {IBlobMetaData}
    function GetSubType: cardinal;
    function GetCharSetID: cardinal;
    function GetSegmentSize: cardinal;
    function GetTableName: string;
    function GetColumnName: string;
  end;


  { TFBBlob }

  TFBBlob = class(TActivityReporter,IBlob)
  private
    FAttachment: TFB30Attachment;
    FBlobIntf: Firebird.IBlob;
    FBlobID: TISC_QUAD;
    FEOB: boolean;
    FCreating: boolean;
    FTransaction: ITransaction;
    procedure CheckReadable;
    procedure CheckWritable;
    procedure InternalClose(Force: boolean);
    procedure InternalCancel(Force: boolean);
  public
    constructor Create(Attachment: TFB30Attachment; Transaction: ITransaction); overload;
    constructor Create(Attachment: TFB30Attachment; Transaction: ITransaction;
                       BlobID: TISC_QUAD); overload;
    destructor Destroy; override;
    procedure TransactionEnding(aTransaction: TFB30Transaction; Force: boolean);
    property BlobIntf: Firebird.IBlob read FBlobIntf;
    property Attachment: TFB30Attachment read FAttachment;

  {IBlob}
  public
    procedure Cancel;
    procedure Close;
    function GetBlobID: TISC_QUAD;
    function GetBlobMode: TFBBlobMode;
    function GetInfo(var NumSegments: Int64; var MaxSegmentSize, TotalSize: Int64;
      var BlobType: TBlobType): boolean;
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

uses FBMessages, FB30Statement;

const
  sLookupBlobMetaData = 'Select F.RDB$FIELD_SUB_TYPE, F.RDB$SEGMENT_LENGTH, RDB$CHARACTER_SET_ID, F.RDB$FIELD_TYPE '+
    'From RDB$FIELDS F JOIN RDB$RELATION_FIELDS R On R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+
  'Where Trim(R.RDB$RELATION_NAME) = Upper(Trim(?)) and Trim(R.RDB$FIELD_NAME) = Upper(Trim(?))';

{ TFBBlobMetaData }

constructor TFBBlobMetaData.Create(Attachment: TFB30Attachment;
  Transaction: TFB30Transaction; RelationName, ColumnName: string);
var stmt: IStatement;
begin
  inherited Create(Transaction);
  FRelationName := RelationName;
  FColumnName := ColumnName;
  stmt := TFB30Statement.Create(Attachment,Transaction, sLookupBlobMetaData ,Attachment.SQLDialect);
  with stmt do
  begin
    SQLParams[0].AsString := RelationName;
    SQLParams[1].AsString := ColumnName;
    with OpenCursor do
    if FetchNext then
    begin
      if Data[3].AsInteger <> blr_blob then
        IBError(ibxeInvalidBlobMetaData,[nil]);
      FSubType := Data[0].AsInteger;
      FSegmentSize := Data[1].AsInteger;
      FCharSetID := Data[2].AsInteger;
    end
    else
      IBError(ibxeInvalidBlobMetaData,[nil]);

    if (FCharSetID <> 0) and Attachment.HasDefaultCharSet then
      FCharSetID := Attachment.CharSetID
  end;
end;

function TFBBlobMetaData.GetSubType: cardinal;
begin
  Result := FSubType;
end;

function TFBBlobMetaData.GetCharSetID: cardinal;
begin
  Result := FCharSetID;
end;

function TFBBlobMetaData.GetSegmentSize: cardinal;
begin
  Result := FSegmentSize;
end;

function TFBBlobMetaData.GetTableName: string;
begin
  Result := FRelationName;
end;

function TFBBlobMetaData.GetColumnName: string;
begin
  Result := FColumnName;
end;

{ TFBBlob }

procedure TFBBlob.CheckReadable;
begin
  if FCreating or (FBlobIntf = nil) then
    IBError(ibxeBlobCannotBeRead, [nil]);
end;

procedure TFBBlob.CheckWritable;
begin
  if not FCreating or (FBlobIntf = nil) then
    IBError(ibxeBlobCannotBeWritten, [nil]);
end;

procedure TFBBlob.InternalClose(Force: boolean);
begin
  if FBlobIntf = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FBlobIntf.close(StatusIntf);
    if not Force then Check4DataBaseError;
  end;
  FBlobIntf.release;
  FBlobIntf := nil;
end;

procedure TFBBlob.InternalCancel(Force: boolean);
begin
  if FBlobIntf = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FBlobIntf.cancel(StatusIntf);
    if not Force then Check4DataBaseError;
  end;
  FBlobIntf.release;
  FBlobIntf := nil;
end;

constructor TFBBlob.Create(Attachment: TFB30Attachment; Transaction: ITransaction
  );
begin
    inherited Create(Transaction as TFB30Transaction);
    FAttachment := Attachment;
    FTransaction := Transaction;
    AddMonitor(Transaction as TFB30Transaction);
    FCreating := true;
    with Firebird30ClientAPI do
    begin
      FBlobIntf := FAttachment.AttachmentIntf.createBlob(StatusIntf,(Transaction as TFB30Transaction).TransactionIntf,
                        @FBlobID,0,nil);
      Check4DataBaseError;
    end;
end;

constructor TFBBlob.Create(Attachment: TFB30Attachment;
  Transaction: ITransaction; BlobID: TISC_QUAD);
begin
  inherited Create(Transaction as TFB30Transaction);
  FAttachment := Attachment;
  FTransaction := Transaction;
  AddMonitor(Transaction as TFB30Transaction);
  FBlobID := BlobID;
  with Firebird30ClientAPI do
  begin
    FBlobIntf := FAttachment.AttachmentIntf.openBlob(StatusIntf,(Transaction as TFB30Transaction).TransactionIntf,
                     @FBlobID, 0, nil);
    Check4DataBaseError;
  end;
end;

destructor TFBBlob.Destroy;
begin
  if FCreating then
    Cancel
  else
    Close;
  inherited Destroy;
end;

procedure TFBBlob.TransactionEnding(aTransaction: TFB30Transaction;
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

function TFBBlob.GetInfo(var NumSegments: Int64; var MaxSegmentSize,
  TotalSize: Int64; var BlobType: TBlobType): boolean;
var
  items: array[0..3] of Char;
  results: array[0..99] of Char;
  i, item_length: Integer;
  item: Integer;
begin
  if FBlobIntf = nil then
    IBError(ibxeBlobNotOpen,[nil]);

  items[0] := Char(isc_info_blob_num_segments);
  items[1] := Char(isc_info_blob_max_segment);
  items[2] := Char(isc_info_blob_total_length);
  items[3] := Char(isc_info_blob_type);

  with Firebird30ClientAPI do
  begin
    FBlobIntf.getInfo(StatusIntf,4,@items[0],SizeOf(results),@results[0]);
    Check4DataBaseError;
    SignalActivity;
    i := 0;
    while (i < SizeOf(results)) and (results[i] <> Char(isc_info_end)) do
    begin
      item := Integer(results[i]); Inc(i);
      item_length := DecodeInteger(@results[i], 2); Inc(i, 2);
      case item of
        isc_info_blob_num_segments:
          NumSegments := DecodeInteger(@results[i], item_length);
        isc_info_blob_max_segment:
          MaxSegmentSize := DecodeInteger(@results[i], item_length);
        isc_info_blob_total_length:
          TotalSize := DecodeInteger(@results[i], item_length);
        isc_info_blob_type:
          if DecodeInteger(@results[i], item_length) = 0 then
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
  BytesRead : cardinal;
  LocalBuffer: PChar;
  returnCode: integer;
  localCount: uShort;
begin
  CheckReadable;
  Result := 0;
  if FEOB then
    Exit;

  LocalBuffer := PChar(@Buffer);
  repeat
    localCount := Count;
    with Firebird30ClientAPI do
      returnCode := FBlobIntf.getSegment(StatusIntf,localCount, LocalBuffer, @BytesRead);
    SignalActivity;
    Inc(LocalBuffer,BytesRead);
    Inc(Result,BytesRead);
    Dec(Count,BytesRead);
  until ((returncode <> Firebird.IStatus.Result_OK) and (returnCode <> Firebird.IStatus.Result_SEGMENT)) or (Count = 0);

  FEOB := returnCode = Firebird.IStatus.RESULT_NO_DATA;
  if (returnCode <> Firebird.IStatus.Result_OK) and
     (returnCode <> Firebird.IStatus.Result_SEGMENT) and
     (returnCode <> Firebird.IStatus.RESULT_NO_DATA) then
    Firebird30ClientAPI.IBDataBaseError
end;

function TFBBlob.Write(const Buffer; Count: Longint): Longint;
begin
  CheckWritable;
  Result := 0;
  if Count = 0 then Exit;

  with Firebird30ClientAPI do
  begin
    FBlobIntf.putSegment(StatusIntf,Count,@Buffer);
    Check4DataBaseError;
  end;
  SignalActivity;
  Result := Count;
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

