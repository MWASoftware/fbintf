unit FB25Blob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, IBHeader,IBExternals, FBLibrary, FB25ClientAPI, FB25Attachment,
  FB25Transaction, FB25APIObject;

type

  { TFBBlob }

  TFBBlob = class(TAPIObject,IBlob)
  private
    FAttachment: TFBAttachment;
    FHandle: TISC_BLOB_HANDLE;
    FBlobID: TISC_QUAD;
    FEOB: boolean;
    FCreating: boolean;
    procedure InternalCancel;
    procedure InternalClose;
    procedure CheckReadable;
    procedure CheckWritable;
  public
    constructor Create(Attachment: TFBAttachment; Transaction: ITransaction); overload;
    constructor Create(Attachment: TFBAttachment; Transaction: ITransaction;
                       BlobID: TISC_QUAD); overload;
    destructor Destroy; override;
    procedure TransactionEnding(aTransaction: TFBTransaction);

  {IBlob}
  public
    function GetStatus: IStatus;
    procedure Cancel;
    procedure Close;
    function GetBlobID: TISC_QUAD;
    function GetInfo(var NumSegments: Int64; var MaxSegmentSize,
                       TotalSize: Int64; var BlobType: Short) :boolean;
    function Read(var Buffer; Count: Longint): Longint;
    function Write(const Buffer; Count: Longint): Longint;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(S: TStream);
    property Handle: TISC_BLOB_HANDLE read FHandle;
    property Attachment: TFBAttachment read FAttachment;
  end;

implementation

uses IBErrorCodes, FBErrorMessages;

{ TFBBlob }

procedure TFBBlob.InternalCancel;
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_cancel_blob(StatusVector,@FHandle));
  FHandle := nil;
end;

procedure TFBBlob.InternalClose;
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
    Call(isc_close_blob(StatusVector, @FHandle), True);
  FHandle := nil;
end;

procedure TFBBlob.CheckReadable;
begin
  if FCreating then IBError(ibxeBlobCannotBeRead, [nil]);
end;

procedure TFBBlob.CheckWritable;
begin
  if not FCreating then IBError(ibxeBlobCannotBeWritten, [nil]);
end;

constructor TFBBlob.Create(Attachment: TFBAttachment; Transaction: ITransaction
  );
var DBHandle: TISC_DB_HANDLE;
      TRHandle: TISC_TR_HANDLE;
begin
    inherited Create;
    FAttachment := Attachment;
    AddOwner(TFBTransaction(Transaction));
    DBHandle := Attachment.Handle;
    TRHandle := TFBTransaction(Transaction).Handle;
    FCreating := true;
    with Firebird25ClientAPI do
      Call(isc_create_blob2(StatusVector, @DBHandle, @TRHandle, @FHandle, @FBlobID,
                           0, nil));
end;

constructor TFBBlob.Create(Attachment: TFBAttachment;
  Transaction: ITransaction; BlobID: TISC_QUAD);
var DBHandle: TISC_DB_HANDLE;
    TRHandle: TISC_TR_HANDLE;
begin
  inherited Create;
  FAttachment := Attachment;
  AddOwner(TFBTransaction(Transaction));
  DBHandle := Attachment.Handle;
  TRHandle := (Transaction as TFBTransaction).Handle;
  FBlobID := BlobID;
  with Firebird25ClientAPI do
    Call(isc_open_blob2(StatusVector,  @DBHandle, @TRHandle, @FHandle,
                     @FBlobID, 0, nil));
end;

destructor TFBBlob.Destroy;
begin
  if FCreating then
    InternalCancel
  else
    InternalClose;
  inherited Destroy;
end;

procedure TFBBlob.TransactionEnding(aTransaction: TFBTransaction);
begin
  if FCreating then
    InternalCancel
  else
    InternalClose;
end;

function TFBBlob.GetStatus: IStatus;
begin
  Result := Firebird25ClientAPI.Status;
end;

procedure TFBBlob.Cancel;
begin
  InternalCancel;
  Free;
end;

procedure TFBBlob.Close;
begin
  InternalClose;
  Free;
end;

function TFBBlob.GetBlobID: TISC_QUAD;
begin
  Result := FBlobID;
end;

function TFBBlob.GetInfo(var NumSegments: Int64; var MaxSegmentSize,
  TotalSize: Int64; var BlobType: Short): boolean;
var
  items: array[0..3] of Char;
  results: array[0..99] of Char;
  i, item_length: Integer;
  item: Integer;
begin
  items[0] := Char(isc_info_blob_num_segments);
  items[1] := Char(isc_info_blob_max_segment);
  items[2] := Char(isc_info_blob_total_length);
  items[3] := Char(isc_info_blob_type);

  with Firebird25ClientAPI do
  begin
    Result := Call(isc_blob_info(StatusVector, @FHandle, 4, @items[0], SizeOf(results),
                    @results[0]),false) = 0;
    i := 0;
    while (i < SizeOf(results)) and (results[i] <> Char(isc_info_end)) do
    begin
      item := Integer(results[i]); Inc(i);
      item_length := isc_vax_integer(@results[i], 2); Inc(i, 2);
      case item of
        isc_info_blob_num_segments:
          NumSegments := isc_portable_integer(@results[i], item_length);
        isc_info_blob_max_segment:
          MaxSegmentSize := isc_portable_integer(@results[i], item_length);
        isc_info_blob_total_length:
          TotalSize := isc_portable_integer(@results[i], item_length);
        isc_info_blob_type:
          BlobType := isc_portable_integer(@results[i], item_length);
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

  LocalBuffer := PChar(Buffer);
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
  until (returnCode <> isc_segment) or (Count = 0);

  FEOB := returnCode = isc_segstr_eof;
  if (returnCode <> isc_segment) and (returnCode <> isc_segstr_eof) then
    raise EIBInterBaseError.Create(GetStatus);
end;

function TFBBlob.Write(const Buffer; Count: Longint): Longint;
var
  LocalBuffer: PChar;
  localCount: uShort;
begin
  CheckWritable;
  LocalBuffer := PChar(Buffer);
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

procedure TFBBlob.LoadFromFile(Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

const BufSize = 8 * 1024;

procedure TFBBlob.LoadFromStream(S: TStream);
var Buffer: array [0..BufSize-1] of char;
    BytesRead: integer;
begin
  CheckWritable;
  S.Position := 0;
  repeat
    BytesRead := S.Read(Buffer,BufSize);
    Write(Buffer,BytesRead);
  until BytesRead = 0;
  InternalClose;
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
end;

end.

