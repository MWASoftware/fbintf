unit FB25OutputBlock;

{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$DEFINE HAS_ANSISTRING_CODEPAGE}
{$ENDIF}

interface

{Provides common handling for the DB Info results, SQL Info and Service Response Block}

uses
  Classes, SysUtils,  FB25ClientAPI, IB;

const
  DefaultBufferSize = 32000;

type
  TItemDataType = (dtString, dtString2, dtByte, dtBytes, dtInteger, dtIntegerFixed, dtnone,dtList,dtSpecial);

  POutputBlockItemData = ^TOutputBlockItemData;
  TOutputBlockItemData = record
     {Describes a Clumplet in the buffer. FBufPtr always points to the clumplet id
     the rest of the clumplet up to the FSize is data. The data format is
     given by FDataType, and the data length is given by FDataLength}
    FBufPtr: PChar;
    FDataLength: integer;
    FSize: integer;
    FDataType: TItemDataType;
    FSubItems: array of POutputBlockItemData;
  end;

  { TOutputBlock }

  TOutputBlock = class(TInterfacedObject)
  private
    FBuffer: PChar;
    FBufSize: integer;
    FBufferParsed: boolean;
    procedure ParseBuffer;
  protected
    FIntegerType: TItemDataType;
    FItems: array of POutputBlockItemData;
    procedure DoParseBuffer; virtual; abstract;
    function AddItem(BufPtr: PChar): POutputBlockItemData;
    function AddIntegerItem(BufPtr: PChar): POutputBlockItemData;
    function AddStringItem(BufPtr: PChar): POutputBlockItemData;
    function AddShortStringItem(BufPtr: PChar): POutputBlockItemData;
    function AddByteItem(BufPtr: PChar): POutputBlockItemData;
    function AddBytesItem(BufPtr: PChar): POutputBlockItemData;
    function AddListItem(BufPtr: PChar): POutputBlockItemData; virtual;
    function AddSpecialItem(BufPtr: PChar): POutputBlockItemData; virtual;
  public
    constructor Create(aSize: integer = DefaultBufferSize);
    destructor Destroy; override;
    function Buffer: PChar;
    function getBufSize: integer;

  public
    function GetCount: integer;
    function GetItem(index: integer): POutputBlockItemData;
    function Find(ItemType: byte): POutputBlockItemData;
    property Items[index: integer]: POutputBlockItemData read getItem; default;
  end;

  { TOutputBlockItem }

  TOutputBlockItem = class(TInterfacedObject)
  private
    FOwner: TOutputBlock;
    FOwnerIntf: IUnknown;
    FItemData: POutputBlockItemData;
  protected
    procedure SetString(out S: AnsiString; Buf: PAnsiChar; Len: SizeInt;
                                           CodePage: TSystemCodePage);
    property ItemData: POutputBlockItemData read FItemData;
    property Owner: TOutputBlock read FOwner;
  public
     constructor Create(AOwner: TOutputBlock; Data: POutputBlockItemData);
  public
    function getItemType: byte;
    function getSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsInteger: integer;
    function getParamType: byte;
    function getAsString: string;
    function getAsByte: byte;
    function getAsBytes: TByteArray;
  end;

  { TOutputBlockItemGroup }

  TOutputBlockItemGroup = class(TOutputBlockItem)
  public
    function GetCount: integer;
    function GetItem(index: integer): POutputBlockItemData;
    function Find(ItemType: byte): POutputBlockItemData;
    property Items[index: integer]: POutputBlockItemData read getItem; default;
  end;


implementation

uses FBErrorMessages;

{ TOutputBlockItemGroup }

function TOutputBlockItemGroup.GetCount: integer;
begin
  Result := Length(FItemData^.FSubItems);
end;

function TOutputBlockItemGroup.GetItem(index: integer): POutputBlockItemData;
begin
  if (index >= 0) and (index < Length(FItemData^.FSubItems)) then
    Result := FItemData^.FSubItems[index]
  else
  with Firebird25ClientAPI do
    IBError(ibxeOutputBlockIndexError,[index]);
end;

function TOutputBlockItemGroup.Find(ItemType: byte): POutputBlockItemData;
var i: integer;
begin
  Result := nil;
  for i := 0 to GetCount - 1 do
    if FItemData^.FSubItems[i]^.FBufPtr^ = char(ItemType) then
    begin
      Result := FItemData^.FSubItems[i];
      Exit;
    end;
end;

{ TOutputBlockItem }

procedure TOutputBlockItem.SetString(out S: AnsiString; Buf: PAnsiChar;
  Len: SizeInt; CodePage: TSystemCodePage);
var rs: RawByteString;
begin
  system.SetString(rs,Buf,len);
  {$IFDEF HAS_ANSISTRING_CODEPAGE}
  SetCodePage(rs,CodePage,false);
  {$ENDIF}
  S := rs;
end;

constructor TOutputBlockItem.Create(AOwner: TOutputBlock;
  Data: POutputBlockItemData);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerIntf := AOwner;
  FItemData := Data;
end;

function TOutputBlockItem.getItemType: byte;
begin
  Result := byte(FItemData^.FBufPtr^);
end;

function TOutputBlockItem.getSize: integer;
begin
  Result := FItemData^.FDataLength;
end;

procedure TOutputBlockItem.getRawBytes(var Buffer);
begin
  with FItemData^ do
    Move(FBufPtr^,Buffer,FDatalength);
end;

function TOutputBlockItem.getAsInteger: integer;
var len: integer;
begin
  with FItemData^ do
  case FDataType of
  dtIntegerFixed:
    with Firebird25ClientAPI do
      Result := isc_portable_integer(FBufPtr+1,4);

  dtByte,
  dtInteger:
    with Firebird25ClientAPI do
    begin
      len := isc_portable_integer(FBufPtr+1,2);
      Result := isc_portable_integer(FBufPtr+3,len);
    end;
  else
    IBError(ibxeOutputBlockTypeError,[nil]);
  end;
end;

function TOutputBlockItem.getParamType: byte;
begin
   Result := byte(FItemData^.FBufPtr^)
end;

function TOutputBlockItem.getAsString: string;
var len: byte;
begin
  Result := '';
  with FItemData^ do
  case FDataType of
  dtInteger:
    Result := IntToStr(getAsInteger);
  dtByte:
    Result := IntToStr(getAsByte);
  dtString:
    begin
      len := byte((FBufPtr+1)^);
      SetString(Result,FBufPtr+2,len,CP_ACP);
    end;
  dtString2:
    begin
      with Firebird25ClientAPI do
        len := isc_portable_integer(FBufPtr+1,2);
      SetString(Result,FBufPtr+3,len,CP_ACP);
    end;
  else
    IBError(ibxeOutputBlockTypeError,[nil]);
  end;
end;

function TOutputBlockItem.getAsByte: byte;
begin
  with FItemData^ do
  if FDataType = dtByte then
    Result := byte((FBufPtr+2)^)
  else
    IBError(ibxeOutputBlockTypeError,[nil]);
end;

function TOutputBlockItem.getAsBytes: TByteArray;
var i: integer;
    P: PChar;
begin
  with FItemData^ do
  if FDataType = dtBytes then
  begin
    SetLength(Result,FDataLength);
    P := FBufPtr;
    for i := 0 to FDataLength - 1 do
    begin
      Result[i] := byte(P^);
      Inc(P);
    end
  end
  else
    IBError(ibxeOutputBlockTypeError,[nil]);
end;

{ TOutputBlock }

procedure TOutputBlock.ParseBuffer;
begin
  if not FBufferParsed then
    DoParseBuffer;
  FBufferParsed := true;
end;

function TOutputBlock.AddItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtNone;
    FBufPtr := BufPtr;
    FDataLength := 0;
    FSize := 1;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddIntegerItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := FIntegerType;
    FBufPtr := BufPtr;
    if FDataType = dtIntegerFixed then
    begin
      FDataLength := 4;
      FSize := 5;
    end
    else
    begin
      with Firebird25ClientAPI do
        FDataLength := isc_portable_integer(FBufPtr+1, 2);
      FSize := FDataLength + 3;
    end;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddStringItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtString2;
    FBufPtr := BufPtr;
    with Firebird25ClientAPI do
      FDataLength := isc_portable_integer(FBufPtr+1, 2);
    FSize := FDataLength + 3;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddShortStringItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtString;
    FBufPtr := BufPtr;
    FDataLength := byte((FBufPtr+1)^);
    FSize := FDataLength + 2;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddByteItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtByte;
    FBufPtr := BufPtr;
    FDataLength := 1;
    FSize := 2;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddBytesItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtBytes;
    FBufPtr := BufPtr;
    with Firebird25ClientAPI do
      FDataLength := isc_portable_integer(FBufPtr+1, 2);
    FSize := FDataLength + 3;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddListItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtList;
    FBufPtr := BufPtr;
    FSize := FBuffer + FBufSize - FBufPtr;
    FDataLength := FSize - 1;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddSpecialItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtSpecial;
    FBufPtr := BufPtr;
    FSize := FBuffer + FBufSize - FBufPtr;
    FDataLength := FSize - 1;
    SetLength(FSubItems,0);
  end;
end;

constructor TOutputBlock.Create(aSize: integer);
begin
  FBufSize := aSize;
  GetMem(FBuffer,aSize);
  if FBuffer = nil then
    OutOfMemoryError;
  FillChar(FBuffer^,aSize,255);
  FBufferParsed := false;
  FIntegerType := dtIntegerFixed;
end;

destructor TOutputBlock.Destroy;
var i, j: integer;
begin
  for i := 0 to length(FItems) - 1 do
  begin
    for j := 0 to Length(FItems[i]^.FSubItems) -1 do
      dispose(FItems[i]^.FSubItems[j]);
    dispose(FItems[i]);
  end;
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TOutputBlock.Buffer: PChar;
begin
  Result := FBuffer;
end;

function TOutputBlock.getBufSize: integer;
begin
  Result := FBufSize;
end;

function TOutputBlock.GetCount: integer;
begin
  ParseBuffer;
  Result := length(FItems);
end;

function TOutputBlock.GetItem(index: integer): POutputBlockItemData;
begin
  ParseBuffer;
  if (index >= 0) and (index < Length(FItems)) then
    Result := FItems[index]
  else
    IBError(ibxeOutputBlockIndexError,[index]);
end;

function TOutputBlock.Find(ItemType: byte): POutputBlockItemData;
var i: integer;
begin
  Result := nil;
  for i := 0 to getCount - 1 do
    if FItems[i]^.FBufPtr^ = char(ItemType) then
    begin
      Result := FItems[i];
      Exit;
    end;
end;

end.

