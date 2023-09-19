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
unit FBBlob;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBActivityMonitor, FBTransaction, FBClientAPI,
  FBOutputBlock;

type

  { TFBBlobMetaData }

  TFBBlobMetaData  = class(TActivityReporter)
  private
    FRelationName: AnsiString;
    FColumnName: AnsiString;
  protected
    FUnconfirmedCharacterSet: boolean;
    FHasSubType: boolean;
    FSubType: integer;
    FCharSetID: cardinal;
    FSegmentSize: cardinal;
    function Attachment: IAttachment; virtual; abstract;
    function CanFetchMetaData: boolean;
    procedure NeedFullMetadata; virtual; abstract;
    procedure NeedSubType;
  public
    constructor Create(Transaction: TFBTransaction; RelationName, ColumnName: AnsiString
      );
    procedure SetCharSetID(aValue: integer);

  public
    {IBlobMetaData}
   function GetSubType: integer;
   function GetCharSetID: cardinal;
   function GetCodePage: TSystemCodePage;
   function GetSegmentSize: cardinal;
   function GetRelationName: AnsiString;
   function GetColumnName: AnsiString;
   function GetUnconfirmedCharacterSet: boolean;
  end;

  TFBBlob = class(TActivityReporter,ITransactionUser)
  private
    FConnectionCodePage: TSystemCodePage;
    FMetaData: IBlobMetaData;
    FAttachment: IAttachment;
    FTransaction: ITransaction;
    FBPB: IBPB;
    FStringData: rawbytestring;
    FStringCached: boolean;
  protected
    FCreating: boolean;
    FBlobID: TISC_QUAD;
    procedure CheckReadable; virtual; abstract;
    procedure CheckWritable; virtual; abstract;
    procedure ClearStringCache;
    function GetIntf: IBlob; virtual; abstract;
    procedure GetInfo(Request: array of byte; Response: IBlobInfo); overload; virtual; abstract;
    procedure InternalClose(Force: boolean); virtual; abstract;
    procedure InternalCancel(Force: boolean); virtual; abstract;
  public
    constructor Create(Attachment: IAttachment; Transaction: TFBTransaction;
                       MetaData: IBlobMetaData; BPB: IBPB); overload;
    constructor Create(Attachment: IAttachment; Transaction: TFBTransaction;
                       MetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB); overload;
    destructor Destroy; override;
    property ConnectionCodePage: TSystemCodePage read FConnectionCodePage;

  public
    {ITransactionUser}
    procedure TransactionEnding(aTransaction: ITransaction; Force: boolean);

  public
    {IBlobMetaData}
   function GetSubType: integer;
   function GetCharSetID: cardinal;
   function GetCodePage: TSystemCodePage;
   function GetSegmentSize: cardinal;
   function GetRelationName: AnsiString;
   function GetColumnName: AnsiString;
   function GetUnconfirmedCharacterSet: boolean;

   {IBlob}
    function GetBPB: IBPB;
    procedure Cancel;
    procedure Close;
    function GetBlobSize: Int64;
    procedure GetInfo(var NumSegments: Int64; var MaxSegmentSize, TotalSize: Int64;
      var BlobType: TBlobType); overload;
    function GetBlobID: TISC_QUAD;
    function GetBlobMode: TFBBlobMode;
    function Read(var Buffer; Count: Longint): Longint; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint;  virtual; abstract;
    function LoadFromFile(Filename: AnsiString): IBlob;
    function LoadFromStream(S: TStream) : IBlob;
    function SaveToFile(Filename: AnsiString): IBlob;
    function SaveToStream(S: TStream): IBlob;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    function GetAsString: rawbytestring;
    procedure SetAsString(aValue: rawbytestring);
    function SetString(aValue: rawbytestring): IBlob;
  end;



implementation

uses IBUtils;

{ TFBBlob }

procedure TFBBlob.ClearStringCache;
begin
  FStringData := '';
  FStringCached := false;
end;

constructor TFBBlob.Create(Attachment: IAttachment;
  Transaction: TFBTransaction; MetaData: IBlobMetaData; BPB: IBPB);
begin
  inherited Create(Transaction);
  FAttachment := Attachment;
  FTransaction := Transaction;
  FMetaData := MetaData;
  FBPB := BPB;
  FCreating := true;
  FConnectionCodePage := Attachment.GetCodePage;
end;

constructor TFBBlob.Create(Attachment: IAttachment;
  Transaction: TFBTransaction; MetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB);
begin
  Create(Attachment,Transaction,MetaData,BPB);
  FBlobID := BlobID;
  FCreating := false;
end;

destructor TFBBlob.Destroy;
begin
  if FCreating then
    Cancel
  else
    Close;
  inherited Destroy;
end;

procedure TFBBlob.TransactionEnding(aTransaction: ITransaction; Force: boolean);
begin
  if (aTransaction as TObject) <> (FTransaction as TObject) then
    Exit;
  if FCreating then
    InternalCancel(Force)
  else
    InternalClose(Force);
end;

function TFBBlob.GetSubType: integer;
begin
  Result := FMetaData.GetSubType;
end;

function TFBBlob.GetCharSetID: cardinal;
begin
  Result := FMetaData.GetCharSetID;
end;

function TFBBlob.GetCodePage: TSystemCodePage;
begin
  Result := FMetaData.GetCodePage;
end;

function TFBBlob.GetSegmentSize: cardinal;
begin
  Result := FMetaData.GetSegmentSize;
end;

function TFBBlob.GetRelationName: AnsiString;
begin
  Result := FMetaData.GetRelationName;
end;

function TFBBlob.GetColumnName: AnsiString;
begin
  Result := FMetaData.GetColumnName;
end;

function TFBBlob.GetUnconfirmedCharacterSet: boolean;
begin
  Result := (FMetadata as TFBBlobMetadata).GetUnconfirmedCharacterSet;
end;

function TFBBlob.GetBPB: IBPB;
begin
  Result := FBPB;
end;

procedure TFBBlob.Cancel;
begin
  InternalCancel(false);
end;

procedure TFBBlob.Close;
begin
  InternalClose(false);
end;

function TFBBlob.GetBlobSize: Int64;
var NumSegments: Int64;
    MaxSegmentSize: Int64;
    BlobType: TBlobType;
begin
  GetInfo(NumSegments,MaxSegmentSize,Result,BlobType);
end;

procedure TFBBlob.GetInfo(var NumSegments: Int64; var MaxSegmentSize,
  TotalSize: Int64; var BlobType: TBlobType);
var BlobInfo: IBlobInfo;
    i: integer;
begin
  NumSegments := 0;
  MaxSegmentSize := 0;
  TotalSize := 0;
  BlobType := btSegmented;

  BlobInfo := TBlobInfo.Create(FAttachment.getFirebirdAPI as TFBClientAPI);
  GetInfo([isc_info_blob_num_segments,
           isc_info_blob_max_segment,
           isc_info_blob_total_length,
           isc_info_blob_type],BlobInfo);

  for i := 0 to BlobInfo.Count - 1 do
  with BlobInfo[i] do
  case getItemType of
  isc_info_blob_num_segments:
    NumSegments := GetAsInteger;
  isc_info_blob_max_segment:
    MaxSegmentSize := GetAsInteger;
  isc_info_blob_total_length:
    TotalSize := GetAsInteger;
  isc_info_blob_type:
    if GetAsInteger = 0 then
      BlobType := btSegmented
    else
      BlobType := btStream;
  end;
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

function TFBBlob.LoadFromFile(Filename: AnsiString): IBlob;
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
  Result := GetIntf;
end;

function TFBBlob.SaveToFile(Filename: AnsiString): IBlob;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Result := SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TFBBlob.SaveToStream(S: TStream): IBlob;
var Buffer: array [0..BufSize-1] of char;
    BytesRead: integer;
begin
  CheckReadable;
  repeat
    BytesRead := Read(Buffer,BufSize);
    S.Write(Buffer,BytesRead);
  until BytesRead = 0;
  Close;
  Result := GetIntf;
end;

function TFBBlob.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TFBBlob.GetTransaction: ITransaction;
begin
   Result := FTransaction;
end;

function TFBBlob.GetAsString: rawbytestring;
var ss: TStringStream;
begin
  if FStringCached then
  begin
    Result := FStringData;
    Exit;
  end;

  ss := TStringStream.Create('');
  try
    SaveToStream(ss);
    Result :=  ss.DataString;
    if (GetSubType = 1) and (FBPB = nil) then
      SetCodePage(Result,GetCodePage,false);
  finally
    ss.Free;
  end;
  FStringData := Result;
  FStringCached := true;
end;

procedure TFBBlob.SetAsString(aValue: rawbytestring);
var
  ss: TStringStream;
begin
  {if GetUnconfirmedCharacterSet then
    IBError(ibxeNoDefaultCharacterSet,[nil]);}

  if (GetSubType = 1) and  (StringCodePage(aValue) <> GetCodePage) and
           (GetCodePage <> CP_NONE) and (FBPB = nil) then
    aValue := Transliterate(aValue,GetCodePage);
  ss := TStringStream.Create(aValue);
  try
    LoadFromStream(ss);
  finally
    ss.Free;
  end;
  FStringData := aValue;
  FStringCached := true;
end;

function TFBBlob.SetString(aValue: rawbytestring): IBlob;
begin
  SetAsString(aValue);
  Result := GetIntf;
end;

{TFBBlobMetaData}

function TFBBlobMetaData.CanFetchMetaData: boolean;
begin
  Result := (FRelationName <> '') and (FColumnName <> '');
end;

procedure TFBBlobMetaData.NeedSubType;
begin
  if not FHasSubType then
  begin
    NeedFullMetadata;
    FHasSubType := true;
  end;
end;

constructor TFBBlobMetaData.Create(Transaction: TFBTransaction; RelationName,
  ColumnName: AnsiString);
begin
  inherited Create(Transaction);
//  if (RelationName = '') or (ColumnName = '') then
 //   IBError(ibxeMissingColumnName,[]);
  FRelationName := RelationName;
  FColumnName := ColumnName;
  FSegmentSize := 80;
  FUnconfirmedCharacterSet := true;
  FCharSetID := 0;
end;

procedure TFBBlobMetaData.SetCharSetID(aValue: integer);
begin
  FCharSetID := aValue;
  FUnconfirmedCharacterSet := false;
end;

function TFBBlobMetaData.GetSubType: integer;
begin
  NeedSubType;
  Result := FSubType;
end;

function TFBBlobMetaData.GetCharSetID: cardinal;
begin
  if FUnconfirmedCharacterSet and CanFetchMetaData then
    NeedFullMetadata;
  Result := FCharSetID;
end;

function TFBBlobMetaData.GetCodePage: TSystemCodePage;
begin
  Attachment.CharSetID2CodePage(GetCharSetID,Result);
end;

function TFBBlobMetaData.GetSegmentSize: cardinal;
begin
  NeedFullMetadata;
  Result := FSegmentSize;
end;

function TFBBlobMetaData.GetRelationName: AnsiString;
begin
  Result := FRelationName;
end;

function TFBBlobMetaData.GetColumnName: AnsiString;
begin
  Result := FColumnName;
end;

function TFBBlobMetaData.GetUnconfirmedCharacterSet: boolean;
begin
  NeedFullMetadata;
  Result := FUnconfirmedCharacterSet;
end;


end.

