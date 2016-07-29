unit FB25ResultBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBExternals, IBHeader, FB25Statement,
  FB25Attachment, FB25Status;

type
  {TInfoBuffer inspired by IBPP RB class}

  TInfoBuffer = class
  private
    mBuffer: PChar;
    mSize: short;
    function FindToken(token: char): PChar; overload;
    function FindToken(token: char; subtoken: char): PChar; overload;
  public
    constructor Create(aSize: integer);
    destructor Destroy; override;
    function Size: short;
    procedure Reset;
    function GetCountValue(token: char): integer;
    function GetBool(token: char): boolean;
    function GetString(token: char; var data: string): integer; overload;
    function GetString(var data: string): integer; overload; {gets first}
    function buffer: PChar;
  end;

  { TResultBuffer }

  TResultBuffer = class(TInfoBuffer)
  public
    function GetValue: integer; overload; {gets first value}
    function GetValue(token: char): integer; overload;
    function GetValue(token: char; subtoken: char): integer; overload;
  end;

  {Used for access to a isc_dsql_sql_info result buffer}

  { TSQLResultBuffer }

  TSQLResultBuffer = class(TResultBuffer)
  public
    constructor Create(aStatement: TFBStatement; info_request:char; aSize: integer = IBLocalBufferLength);
  end;

  { TDBInfoResultBuffer }

  TDBInfoResultBuffer = class(TResultBuffer)
  public
    constructor Create(aAttachment: TFBAttachment; info_request: char;
      aSize: integer=IBLocalBufferLength);
    function GetInfoBuffer(var p: PChar): integer;
  end;

  TIDBInfoItem = class(TInterfacedObject,IDBInformation)
  private
    FItemType: char;
    FBufPtr: PChar;
    FItemlength: UShort;
  public
    constructor Create(ItemType: char; BufPtr: PChar; Itemlength: UShort);

  public
    {IIDBInfoItem}
    function getItemType: char;
    function getSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: string;
    function getAsInteger: integer;
    procedure DecodeIDCluster(var ConnectionType: integer; var DBFileName, DBSiteName: string);
    function getAsBytes: array of byte;
    procedure DecodeVersionString(var Version: byte; var VersionString: string);
    procedure DecodeUserNames(var UserNames: TStrings);
    function getOperationCounts: array of TDBOperationCounts;
 end;

  TDBInformation = class(TInterfacedObject,IDBInformation)
   private
    FBuffer: PChar;
    FBufSize: integer;
    FItems: array of IDBInformation;
    procedure ParseBuffer;
  public
    constructor Create(aAttachment: TFBAttachment; info_request: char;
      aSize: integer=IBLocalBufferLength); overload;
    constructor Create(aAttachment: TFBAttachment; info_requests: array of char;
      aSize: integer=IBLocalBufferLength); overload;
    destructor Destroy; override;

  public
    {IDBInformation}
    function getCount: integer;
    function getItem(index: integer): IIDBInfoItem;
    property Items[index: integer]: IDBInfoItem read getItem; default;
  end;

  {Used for access to a isc_query_service result buffer}

  { TServiceQueryBuffer }

  TServiceQueryBuffer = class(TInfoBuffer)
  public
    function GetValue(token: char): integer;
  end;


implementation

uses FBErrorMessages;

{ TDBInfoResultBuffer }

constructor TDBInfoResultBuffer.Create(aAttachment: TFBAttachment;
  info_request: char; aSize: integer);
begin
  inherited Create(aSize);
  with Firebird25ClientAPI do
    if isc_database_info(StatusVector, @(aAttachment.Handle), 2, @info_request,
                           Size, Buffer) > 0 then
      IBDataBaseError;
end;

function TDBInfoResultBuffer.GetInfoBuffer(var p: PChar): integer;
begin
  p := buffer + 1;
  with Firebird25ClientAPI do
    Result := isc_vax_integer(p, 2);
  Inc(p,2);
end;

{ TSQLResultBuffer }

constructor TSQLResultBuffer.Create(aStatement: TFBStatement;
  info_request: char; aSize: integer);
begin
  inherited Create(aSize);
  if aStatement.Handle = nil then
    IBError(ibxeInvalidStatementHandle,[nil]);

  with Firebird25ClientAPI do
    if isc_dsql_sql_info(StatusVector, @(aStatement.Handle), 2, @info_request,
                           Size, Buffer) > 0 then
      IBDataBaseError;
end;

  { TServiceQueryBuffer }

function TServiceQueryBuffer.GetValue(token: char): integer;
var p: PChar;
begin
  Result := 0;
  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  with Firebird25ClientAPI do
    Result := isc_vax_integer(p+1, 4);
end;

  { TResultBuffer }

function TResultBuffer.GetValue: integer;
var len: integer;
    p: PChar;
begin
  p := buffer;
  with Firebird25ClientAPI do
  begin
    len := isc_vax_integer(p+1, 2);
    if (len <> 0) then
      Result := isc_vax_integer(p+3, len);
  end;
end;

function TResultBuffer.GetValue(token: char): integer;
var len: integer;
    p: PChar;
begin
  Result := 0;
  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  with Firebird25ClientAPI do
  begin
    len := isc_vax_integer(p+1, 2);
    if (len <> 0) then
      Result := isc_vax_integer(p+3, len);
  end;
end;

function TResultBuffer.GetValue(token: char; subtoken: char): integer;
var len: integer;
    p: PChar;
begin
  Result := 0;
  p := FindToken(token, subtoken);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  with Firebird25ClientAPI do
  begin
    len := isc_vax_integer(p+1, 2);
    if (len <> 0) then
      Result := isc_vax_integer(p+3, len);
  end;
end;

{ TInfoBuffer }

constructor TInfoBuffer.Create(aSize: integer);
begin
  inherited Create;
  mSize := aSize;
  GetMem(mBuffer,aSize);
  FillChar(mBuffer^,mSize,255);
end;

destructor TInfoBuffer.Destroy;
begin
  if mBuffer <> nil then FreeMem(mBuffer);
  inherited;
end;

function TInfoBuffer.buffer: PChar;
begin
  Result := mBuffer;
end;

function TInfoBuffer.FindToken(token: char): PChar;
var p: PChar;
    len: integer;
begin
  Result := nil;
  p := mBuffer;

  while p^ <> char(isc_info_end) do
  with Firebird25ClientAPI do
  begin
    if p^ = token then
    begin
      Result := p;
      Exit;
    end;
    len := isc_vax_integer(p+1,2);
    Inc(p,len+3);
  end;
end;

function TInfoBuffer.FindToken(token: char; subtoken: char): PChar;
var p: PChar;
    len, inlen: integer;
begin
  Result := nil;
  p := mBuffer;

  while p^ <> char(isc_info_end) do
  with Firebird25ClientAPI do
  begin
    if p^ = token then
    begin
      {Found token, now find subtoken}
      inlen := isc_vax_integer(p+1, 2);
      Inc(p,3);
      while inlen > 0 do
      begin
	if p^ = subtoken then
        begin
          Result := p;
          Exit;
        end;
  	len := isc_vax_integer(p+1, 2);
        Inc(p,len + 3);
        Dec(inlen,len + 3);
      end;
      Exit;
    end;
    len := isc_vax_integer(p+1, 2);
    inc(p,len+3);
  end;
end;

function TInfoBuffer.GetBool(token: char): boolean;
var aValue: integer;
    p: PChar;
begin
  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  with Firebird25ClientAPI do
    aValue := isc_vax_integer(p+1, 4);
  Result := aValue <> 0;
end;

function TInfoBuffer.GetCountValue(token: char): integer;
var len: integer;
    p: PChar;
begin
  {Specifically used on tokens like isc_info_insert_count and the like
   which return detailed counts per relation. We sum up the values.}

  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  {len is the number of bytes in the following array}

  with Firebird25ClientAPI do
    len := isc_vax_integer(p+1, 2);
  Inc(p,3);
  Result := 0;
  while len > 0 do
  begin
    {Each array item is 6 bytes : 2 bytes for the relation_id which
     we skip, and 4 bytes for the count value which we sum up across
     all tables.}

    with Firebird25ClientAPI do
       Inc(Result,isc_vax_integer(p+2, 4));
     Inc(p,6);
     Dec(len,6);
  end;
end;

function TInfoBuffer.GetString(token: char; var data: string): integer;
var p: PChar;
begin
  Result := 0;
  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  with Firebird25ClientAPI do
    Result := isc_vax_integer(p+1, 2);
  SetString(data,p+3,Result);
  data := Trim(data);
end;

function TInfoBuffer.GetString(var data: string): integer;
var p: PChar;
begin
  Result := 0;
  p := buffer;
  with Firebird25ClientAPI do
    Result := isc_vax_integer(p+1, 2);
  SetString(data,p+3,Result);
  data := Trim(data);
end;

function TInfoBuffer.Size: short;
begin
  Result := mSize;
end;

procedure TInfoBuffer.Reset;
begin
  if mBuffer <> nil then FreeMem(mBuffer);
  GetMem(mBuffer,mSize);
  FillChar(mBuffer^,mSize,255);
end;



end.

