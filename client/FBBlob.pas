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

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBActivityMonitor, FBTransaction, FBClientAPI;

type

  { TFBBlobMetaData }

  TFBBlobMetaData  = class(TActivityReporter)
  private
    FRelationName: string;
    FColumnName: string;
  protected
    FHasSubType: boolean;
    FSubType: integer;
    FCharSetID: cardinal;
    FSegmentSize: cardinal;
    procedure NeedFullMetadata; virtual; abstract;
    procedure NeedSubType;
  public
    constructor Create(Transaction: TFBTransaction; RelationName, ColumnName: string
      );

  public
    {IBlobMetaData}
   function GetSubType: integer;
   function GetCharSetID: cardinal;
   function GetCodePage: TSystemCodePage;
   function GetSegmentSize: cardinal;
   function GetRelationName: string;
   function GetColumnName: string;
  end;

  TFBBlob = class(TActivityReporter)
  private
    FMetaData: IBlobMetaData;
    FAttachment: IAttachment;
    FTransaction: ITransaction;
    FBPB: IBPB;
  protected
    FCreating: boolean;
    FBlobID: TISC_QUAD;
    procedure CheckReadable; virtual; abstract;
    procedure CheckWritable; virtual; abstract;
    function GetIntf: IBlob; virtual; abstract;
    procedure InternalClose(Force: boolean); virtual; abstract;
    procedure InternalCancel(Force: boolean); virtual; abstract;
  public
    constructor Create(Attachment: IAttachment; Transaction: TFBTransaction;
                       MetaData: IBlobMetaData; BPB: IBPB); overload;
    constructor Create(Attachment: IAttachment; Transaction: TFBTransaction;
                       MetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB); overload;
    destructor Destroy; override;
    procedure TransactionEnding(aTransaction: TFBTransaction; Force: boolean);

  public
    {IBlobMetaData}
   function GetSubType: integer;
   function GetCharSetID: cardinal;
   function GetCodePage: TSystemCodePage;
   function GetSegmentSize: cardinal;
   function GetRelationName: string;
   function GetColumnName: string;

   {IBlob}
    function GetBPB: IBPB;
    procedure Cancel;
    procedure Close;
    function GetBlobID: TISC_QUAD;
    function GetBlobMode: TFBBlobMode;
    function Read(var Buffer; Count: Longint): Longint; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint;  virtual; abstract;
    function LoadFromFile(Filename: string): IBlob;
    function LoadFromStream(S: TStream) : IBlob;
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(S: TStream);
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    function GetAsString: string;
    procedure SetAsString(aValue: string);
    function SetString(aValue: string): IBlob;
  end;



implementation

uses FBMessages;

{ TFBBlob }

constructor TFBBlob.Create(Attachment: IAttachment;
  Transaction: TFBTransaction; MetaData: IBlobMetaData; BPB: IBPB);
begin
  inherited Create(Transaction);
  FAttachment := Attachment;
  FTransaction := Transaction;
  FMetaData := MetaData;
  FBPB := BPB;
  FCreating := true;
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

procedure TFBBlob.TransactionEnding(aTransaction: TFBTransaction;
  Force: boolean);
begin
  if aTransaction <> FTransaction then
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

function TFBBlob.GetRelationName: string;
begin
  Result := FMetaData.GetRelationName;
end;

function TFBBlob.GetColumnName: string;
begin
  Result := FMetaData.GetColumnName;
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
  Result := GetIntf;
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

function TFBBlob.GetAsString: string;
var ss: TStringStream;
    rs: RawByteString;
begin
  ss := TStringStream.Create('');
  try
    SaveToStream(ss);
    rs :=  ss.DataString;
    if GetSubType = 1 then
      SetCodePage(rs,GetCodePage,false);
    Result := rs;
  finally
    ss.Free;
  end;
end;

procedure TFBBlob.SetAsString(aValue: string);
var
  ss: TStringStream;
  rs: RawByteString;
begin
  rs := aValue;
  if (GetSubType = 1) and  (StringCodePage(aValue) <> GetCodePage) and
           (GetCodePage <> CP_NONE) then
    SetCodePage(rs,GetCodePage,true);
  ss := TStringStream.Create(rs);
  try
    LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;

function TFBBlob.SetString(aValue: string): IBlob;
begin
  SetAsString(aValue);
  Result := GetIntf;
end;

{TFBBlobMetaData}

procedure TFBBlobMetaData.NeedSubType;
begin
  if not FHasSubType then
  begin
    NeedFullMetadata;
    FHasSubType := true;
  end;
end;

constructor TFBBlobMetaData.Create(Transaction: TFBTransaction; RelationName,
  ColumnName: string);
begin
  inherited Create(Transaction);
//  if (RelationName = '') or (ColumnName = '') then
 //   IBError(ibxeMissingColumnName,[]);
  FRelationName := RelationName;
  FColumnName := ColumnName;
end;

function TFBBlobMetaData.GetSubType: integer;
begin
  NeedSubType;
  Result := FSubType;
end;

function TFBBlobMetaData.GetCharSetID: cardinal;
begin
  NeedFullMetadata;
  Result := FCharSetID;
end;

function TFBBlobMetaData.GetCodePage: TSystemCodePage;
begin
  FirebirdClientAPI.CharSetID2CodePage(GetCharSetID,Result);
end;

function TFBBlobMetaData.GetSegmentSize: cardinal;
begin
  NeedFullMetadata;
  Result := FSegmentSize;
end;

function TFBBlobMetaData.GetRelationName: string;
begin
  Result := FRelationName;
end;

function TFBBlobMetaData.GetColumnName: string;
begin
  Result := FColumnName;
end;


end.

