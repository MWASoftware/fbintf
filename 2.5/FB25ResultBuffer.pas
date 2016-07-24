unit FB25ResultBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBExternals, IBHeader;

type
  {TInfoBuffer inspired by IBPP RB class}

  TInfoBuffer = class
  private
    mBuffer: PChar;
    mSize: short;
    FClientAPI: TFBClientAPI;
    function FindToken(token: char): PChar; overload;
    function FindToken(token: char; subtoken: char): PChar; overload;
  public
    constructor Create(ClientAPI: TFBClientAPI; aSize: integer = 1024);
    destructor Destroy; override;
    function Size: short;
    procedure Reset;
    function GetCountValue(token: char): integer;
    function GetBool(token: char): boolean;
    function GetString(token: char; var data: string): integer;
    function buffer: PChar;
  end;

  { TResultBuffer }

  {Used for access to a isc_dsql_sql_info result buffer}

  TResultBuffer = class(TInfoBuffer)
  public
    function GetValue(token: char): integer; overload;
    function GetValue(token: char; subtoken: char): integer; overload;
  end;

  {Used for access to a isc_query_service result buffer}

  { TServiceQueryBuffer }

  TServiceQueryBuffer = class(TInfoBuffer)
  public
    function GetValue(token: char): integer;
  end;


implementation

uses FBErrorMessages;

  { TServiceQueryBuffer }

function TServiceQueryBuffer.GetValue(token: char): integer;
var p: PChar;
begin
  Result := 0;
  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  with FClientAPI do
    Result := isc_vax_integer(p+1, 4);
end;

  { TResultBuffer }

function TResultBuffer.GetValue(token: char): integer;
var len: integer;
    p: PChar;
begin
  Result := 0;
  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  with FClientAPI do
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

  with FClientAPI do
  begin
    len := isc_vax_integer(p+1, 2);
    if (len <> 0) then
      Result := isc_vax_integer(p+3, len);
  end;
end;

{ TInfoBuffer }

constructor TInfoBuffer.Create(ClientAPI: TFBClientAPI; aSize: integer);
begin
  inherited Create;
  FClientAPI := ClientAPI;
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
  with FClientAPI do
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
  with FClientAPI do
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

  with FClientAPI do
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

  with FClientAPI do
    len := isc_vax_integer(p+1, 2);
  Inc(p,3);
  Result := 0;
  while len > 0 do
  begin
    {Each array item is 6 bytes : 2 bytes for the relation_id which
     we skip, and 4 bytes for the count value which we sum up across
     all tables.}

    with FClientAPI do
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

  with FClientAPI do
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

