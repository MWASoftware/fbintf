unit FB25Blob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, FB25Attachment, FB25Transaction;

type

  { TFBBlob }

  TFBBlob = class(TInterfacedObject,IBlob)
  private
    FClientAPI: TFBClientAPI;
    FAttachment: TFBAttachment;
    FTransaction: TFBTransaction;
    FHandle: TISC_BLOB_HANDLE;
    FBlobID: TISC_QUAD;
    FEOB: boolean;
  public
    constructor Create(ClientAPI: TFBClientAPI; Attachment: IAttachment; Transaction: ITransaction); overload;
    constructor Create(ClientAPI: TFBClientAPI; Attachment: IAttachment; Transaction: ITransaction;
                       BlobID: ISC_QUAD); overload;
  {IBlob}
  public
    function GetStatus: IStatus;
    procedure Cancel;
    procedure Close;
    function GetBlobID: ISC_QUAD;
    function GetInfo(var NumSegments: Int64; var MaxSegmentSize,
                       TotalSize: Int64; var BlobType: Short) :boolean;
    function Read(var Buffer; Count: uShort): Longint;
    function Write(const Buffer; Count: uShort): Longint;
    property Handle: TISC_BLOB_HANDLE read FHandle;
  end;

implementation

uses IBErrorCodes;

{ TFBBlob }

constructor TFBBlob.Create(ClientAPI: TFBClientAPI; Attachment: IAttachment;
  Transaction: ITransaction);
begin
  inherited Create;
  FClientAPI := ClientAPI;
  FAttachment := Attachment as TFBAttachment;
  FTransaction := Transaction as TFBTransaction;
  with ClientAPI do
    Call(isc_create_blob2(StatusVector, FAttachment.Handle, FTransaction.Handle, @FHandle, @FBlobID,
                         0, nil));
end;

constructor TFBBlob.Create(ClientAPI: TFBClientAPI; Attachment: IAttachment;
  Transaction: ITransaction; BlobID: ISC_QUAD);
begin
  inherited Create;
  FClientAPI := ClientAPI;
  FAttachment := Attachment as TFBAttachment;
  FTransaction := Transaction as TFBTransaction;
  FBlobID := BlobID;
  with ClientAPI do
    Call(isc_open_blob2(StatusVector, FAttachment.Handle, FTransaction.Handle, @FHandle,
                     @FBlobID, 0, nil));
end;

function TFBBlob.GetStatus: IStatus;
begin
  Result := FClientAPI.Status;
end;

procedure TFBBlob.Cancel;
begin
  with ClientAPI do
    Call(isc_cancel_blob(StatusVector,FHandle));
  Free;
end;

procedure TFBBlob.Close;
begin
  with FClientAPI do
    Call(isc_close_blob(StatusVector, @FHandle), True);
  Free;
end;

function TFBBlob.GetBlobID: ISC_QUAD;
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

  with FClientAPI do
  begin
    Call(isc_blob_info(StatusVector, hBlobHandle, 4, @items[0], SizeOf(results),
                    @results[0]));
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

function TFBBlob.Read(var Buffer; Count: uShort): Longint;
var
  BytesRead : UShort;
  LocalBuffer: PChar;
  returnCode: long;
begin
  Result := 0;
  if FEOB then
    Exit;

  LocalBuffer := Buffer;
  repeat
    with FClientAPI do
      returnCode := isc_get_segment(StatusVector, FHandle, @BytesRead, Count,
                           LocalBuffer);
    Inc(LocalBuffer,BytesRead);
    Inc(Result,BytesRead);
    Dec(Count,BytesRead);
  until (returnCode <> isc_segment) or (Count = 0):

  FEOB := returnCode = isc_segstr-eof;
  if not (returnCode in [isc_segment,isc_segstr-eof]) then
    raise EIBInterBaseError.Create(GetStatus);
end;

function TFBBlob.Write(const Buffer; Count: uShort): Longint;
begin
  with FClientAPI do
    Call(isc_put_segment(StatusVector,FHandle,Count,Buffer);
end;

end.

