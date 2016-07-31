unit FB25DBInfo;

{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$DEFINE HAS_ANSISTRING_CODEPAGE}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBExternals, IBHeader, FB25Statement,
  FB25Attachment, FB25Status;

type
    { TIDBInfoItem }

  TIDBInfoItem = class(TInterfacedObject,IDBInfoItem)
  private
    FItemType: byte;
    FBufPtr: PChar;
    FItemlength: UShort;
    function GetString(var P: PChar): string;
  public
    constructor Create(ItemType: byte; BufPtr: PChar; Itemlength: UShort);

  public
    {IDBInfoItem}
    function getItemType: byte;
    function getSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: string;
    function getAsInteger: integer;
    procedure DecodeIDCluster(var ConnectionType: integer; var DBFileName, DBSiteName: string);
    function getAsBytes: TByteArray;
    procedure DecodeVersionString(var Version: byte; var VersionString: string);
    procedure DecodeUserNames(var UserNames: TStrings);
    function getOperationCounts: TDBOperationCounts;
 end;

  { TDBInformation }

  TDBInformation = class(TInterfacedObject,IDBInformation)
   private
    FBuffer: PChar;
    FBufSize: integer;
    FItems: array of IDBInfoItem;
    procedure ParseBuffer;
  public
    constructor Create(aAttachment: TFBAttachment; info_request: byte;
      aSize: integer=IBLocalBufferLength); overload;
    constructor Create(aAttachment: TFBAttachment; info_requests: array of byte;
      aSize: integer=IBLocalBufferLength); overload;
    constructor Create(aStatement: TFBStatement; info_request: byte;
      aSize: integer=IBLocalBufferLength
      ); overload;
    destructor Destroy; override;

  public
    {IDBInformation}
    function getCount: integer;
    function getItem(index: integer): IDBInfoItem;
    function Find(ItemType: byte): IDBInfoItem;
    property Items[index: integer]: IDBInfoItem read getItem; default;
  end;

implementation

uses FBErrorMessages;

{ TIDBInfoItem }

function TIDBInfoItem.GetString(var P: PChar): string;
var s: RawByteString;
    len: byte;
begin
  len := integer(P^);
  if P + len >= FBufPtr + FItemLength then
    IBError(ibxeInfoBufferOverflow,[nil]);
  SetString(s,P+1,len);
  {$IFDEF HAS_ANSISTRING_CODEPAGE}
  SetCodePage(s,CP_UTF8,false);
  {$ENDIF}
  Inc(P,len+1);

  Result := s;
end;

constructor TIDBInfoItem.Create(ItemType: byte; BufPtr: PChar;
  Itemlength: UShort);
begin
  inherited Create;
  FItemType := ItemType;
  FBufPtr := BufPtr;
  FItemlength := Itemlength;
end;

function TIDBInfoItem.getItemType: byte;
begin
  Result := FItemType;
end;

function TIDBInfoItem.getSize: integer;
begin
  Result := FItemlength;
end;

procedure TIDBInfoItem.getRawBytes(var Buffer);
begin
  Move(FBufPtr^,Buffer,FItemlength);
end;

function TIDBInfoItem.getAsString: string;
var s: RawByteString;
begin
  SetString(s,FBufPtr,FItemlength);
  {$IFDEF HAS_ANSISTRING_CODEPAGE}
  SetCodePage(s,CP_UTF8,false);
  {$ENDIF}
  Result := s;
end;

function TIDBInfoItem.getAsInteger: integer;
begin
  with Firebird25ClientAPI do
    Result := isc_vax_integer(FBufPtr, FItemLength);
end;

procedure TIDBInfoItem.DecodeIDCluster(var ConnectionType: integer;
  var DBFileName, DBSiteName: string);
var  P: PChar;
begin
  if FItemType = isc_info_db_id then
  begin
    P := FBufPtr;
    if FItemLength > 0 then
      ConnectionType := integer(P^);
    Inc(P);
    DBFileName := GetString(P);
    DBSiteName := GetString(P);
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FItemType)]);
end;

function TIDBInfoItem.getAsBytes: TByteArray;
var i: integer;
    P: PChar;
begin
  SetLength(Result,FItemLength);
  P := FBufPtr;
  for i := 0 to FItemLength - 1 do
  begin
    Result[i] := byte(P^);
    Inc(P);
  end
end;

procedure TIDBInfoItem.DecodeVersionString(var Version: byte;
  var VersionString: string);
var  P: PChar;
begin
  if FItemType = isc_info_version then
  begin
   P := FBufPtr;
   VersionString := '';
   Version := integer(P^);
   Inc(P);
   VersionString := GetString(P);
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FItemType)]);

end;

procedure TIDBInfoItem.DecodeUserNames(var UserNames: TStrings);
var P: PChar;
    s: string;
begin
  P := FBufPtr;
  while (P < FBufPtr + FItemLength) and (P^ = Char(isc_info_user_names)) do
  begin
    s := GetString(P);
    UserNames.Add(s);
  end;
end;

function TIDBInfoItem.getOperationCounts: TDBOperationCounts;
var tableCounts: integer;
    P: PChar;
    i: integer;
begin
  tableCounts := FItemLength div 6;
  SetLength(Result,TableCounts);
  P := FBufPtr;
  for i := 0 to TableCounts -1 do
  with Firebird25ClientAPI do
  begin
    Result[i].TableID := isc_vax_integer(P,2);
    Inc(P,2);
    Result[i].Count := isc_vax_integer(P,4);
    Inc(P,4);
  end;
end;

{ TDBInformation }

procedure TDBInformation.ParseBuffer;
var P: PChar;
    index: integer;
    len: integer;
begin
  P := FBuffer;
  index := 0;
  SetLength(FItems,0);
  while (P^ <> char(isc_info_end)) and (P < FBuffer + FBufSize) do
  with Firebird25ClientAPI do
  begin
    SetLength(FItems,index+1);
    len := isc_vax_integer(P+1,2);
    FItems[index] := TIDBInfoItem.Create(byte(P^),P+3,len);
    Inc(index);
    Inc(P,3+len);
  end;
end;


constructor TDBInformation.Create(aAttachment: TFBAttachment;
  info_request: byte; aSize: integer);
begin
  inherited Create;
  FBufSize := aSize;
  GetMem(FBuffer,FBufSize);
  if FBuffer = nil then
    OutOfMemoryError;
  FillChar(FBuffer^,FBufSize,255);
  with Firebird25ClientAPI do
    if isc_database_info(StatusVector, @(aAttachment.Handle), 1, @info_request,
                           FBufSize, FBuffer) > 0 then
      IBDataBaseError;
  ParseBuffer;
end;

constructor TDBInformation.Create(aAttachment: TFBAttachment;
  info_requests: array of byte; aSize: integer);
var ReqBuffer: string;
    i: integer;
begin
  inherited Create;
  FBufSize := aSize;
  GetMem(FBuffer,FBufSize);
  if FBuffer = nil then
    OutOfMemoryError;
  FillChar(FBuffer^,FBufSize,255);
  SetLength(ReqBuffer,Length(info_requests));
  for i := 0 to Length(info_requests) - 1 do
    ReqBuffer[i] := char(info_requests[i]);
  with Firebird25ClientAPI do
    if isc_database_info(StatusVector, @(aAttachment.Handle), Length(ReqBuffer), PChar(ReqBuffer),
                           FBufSize, FBuffer) > 0 then
      IBDataBaseError;
  ParseBuffer;
end;

constructor TDBInformation.Create(aStatement: TFBStatement; info_request: byte;
  aSize: integer);
begin
  inherited Create;
  FBufSize := aSize;
  GetMem(FBuffer,FBufSize);
  if FBuffer = nil then
    OutOfMemoryError;
  FillChar(FBuffer^,FBufSize,255);
  with Firebird25ClientAPI do
    if isc_dsql_sql_info(StatusVector, @(aStatement.Handle), 1, @info_request,
                           FBufSize, FBuffer) > 0 then
      IBDataBaseError;
  ParseBuffer;
end;

destructor TDBInformation.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TDBInformation.getCount: integer;
begin
  Result := Length(FItems);
end;

function TDBInformation.getItem(index: integer): IDBInfoItem;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[index]
  else
    IBError(ibxeInfoBufferIndexError,[index]);
end;

function TDBInformation.Find(ItemType: byte): IDBInfoItem;
var i: integer;
begin
  Result := nil;
  for i := 0 to getCount - 1 do
    if FItems[i].getItemType = ItemType then
    begin
      Result := FItems[i];
      Exit;
    end;
end;


end.

