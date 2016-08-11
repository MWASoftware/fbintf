unit FB25ParamBlock;

{$mode objfpc}{$H+}

interface

{Provides common handling for the DPB, SPB and Service Request Block}

uses
  Classes, SysUtils, IB, FB25ClientAPI;

type
  TParamDataType = (dtString, dtString2, dtByte,dtInteger,dtnone);

  PParamBlockItemData = ^TParamBlockItemData;
  TParamBlockItemData = record
    {Describes a Clumplet in the buffer. FBufPtr always points to the clumplet id
     the rest of the clumplet up to the FBufLength is data. The data format is
     given by FDataType}
    FBufPtr: PChar;
    FBuflength: integer;
    FDataType: TParamDataType;
  end;

  TParamBlockItem = class;

  { TParamBlock }

  TParamBlock = class(TInterfacedObject)
  private
    FItems: array of PParamBlockItemData;
    FBufferSize: integer;
    procedure AdjustBuffer;
    procedure MoveBy(Item: PParamBlockItemData; delta: integer);
    procedure UpdateRequestItemSize(Item: TParamBlockItem; NewSize: integer);
  protected
    FBuffer: PChar;
    FDataLength: integer;
    function Add(ParamType: byte): PParamBlockItemData;
    function Find(ParamType: byte): PParamBlockItemData;
    function GetItems(index: integer): PParamBlockItemData;
  public
    constructor Create;
    destructor Destroy; override;
    function getBuffer: PChar;
    function getDataLength: integer;

  public
    function getCount: integer;
  end;

  { TParamBlockItem }

  TParamBlockItem = class(TInterfacedObject)
  private
     FOwner: TParamBlock;
     FOwnerIntf: IUnknown;
     FParamData: PParamBlockItemData;
  protected
    property ParamData: PParamBlockItemData read FParamData;
  public
     constructor Create(AOwner: TParamBlock; Data: PParamBlockItemData);
  public
     function getAsInteger: integer;
     function getParamType: byte;
     function getAsString: string;
     function getAsByte: byte;
     procedure setAsByte(aValue: byte);
     procedure SetAsInteger(aValue: integer);
     procedure SetAsString(aValue: string);
     procedure SetAsString2(aValue: string);
  end;

implementation

uses FBErrorMessages;

{ TParamBlockItem }

constructor TParamBlockItem.Create(AOwner: TParamBlock;
  Data: PParamBlockItemData);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerIntf := AOwner;
  FParamData := Data;
end;

function TParamBlockItem.getAsInteger: integer;
begin
  with FParamData^ do
  if FDataType =  dtInteger then
  with Firebird25ClientAPI do
    Result := isc_portable_integer(FBufPtr+1,4)
  else
    IBError(ibxePBParamTypeError,[nil]);
end;

function TParamBlockItem.getParamType: byte;
begin
  Result := byte(FParamData^.FBufPtr^);
end;

function TParamBlockItem.getAsString: string;
var len: byte;
begin
  Result := '';
  with FParamData^ do
  case FDataType of
  dtInteger:
    Result := IntToStr(getAsInteger);
  dtByte:
    Result := IntToStr(getAsByte);
  dtString:
    begin
      len := byte((FBufPtr+1)^);
      SetString(Result,FBufPtr+2,len);
    end;
  dtString2:
    begin
      with Firebird25ClientAPI do
        len := isc_portable_integer(FBufPtr+1,2);
      SetString(Result,FBufPtr+3,len);
    end;
  end;
end;

function TParamBlockItem.getAsByte: byte;
begin
  with FParamData^ do
  if FDataType = dtByte then
    Result := byte((FBufPtr+2)^)
  else
    IBError(ibxePBParamTypeError,[nil]);
end;

procedure TParamBlockItem.setAsByte(aValue: byte);
begin
  with FParamData^ do
  begin
    if FBufLength <> 3 then
      FOwner.UpdateRequestItemSize(self,3);
    FDataType := dtByte;
    (FBufPtr+1)^ := #1;
    (FBufPtr+2)^ := char(aValue);
  end;
end;

procedure TParamBlockItem.SetAsInteger(aValue: integer);
begin
  with FParamData^ do
  begin
    if FBufLength <> 5 then
      FOwner.UpdateRequestItemSize(self,5);
    with Firebird25ClientAPI do
      EncodeLsbf(aValue,4,FBufPtr+1);
    FDataType := dtInteger;
  end;
end;

procedure TParamBlockItem.SetAsString(aValue: string);
var len: integer;
begin
  with FParamData^ do
  begin
    len := Length(aValue);
    FOwner.UpdateRequestItemSize(self,len+2);
    (FBufPtr+1)^ := char(len);
    Move(aValue[1],(FBufPtr+2)^,len);
    FDataType := dtString;
  end;
end;

procedure TParamBlockItem.SetAsString2(aValue: string);
var len: integer;
begin
  with FParamData^ do
  begin
    len := Length(aValue);
    FOwner.UpdateRequestItemSize(self,len + 3);
    with Firebird25ClientAPI do
      EncodeLsbf(len,2,FBufPtr+1);
    Move(aValue[1],(FBufPtr+3)^,len);
    FDataType := dtString2;
  end;
end;

{ TParamBlock }

procedure TParamBlock.AdjustBuffer;
begin
  if FDataLength > FBufferSize then
  begin
    FBufferSize := FDataLength;
    ReallocMem(FBuffer,FBufferSize);
  end;
end;

procedure TParamBlock.MoveBy(Item: PParamBlockItemData; delta: integer);
var src, dest: PChar;
  i: integer;
begin
  with Item^ do
  begin
    src := FBufptr;
    dest := FBufptr + delta ;
    if delta > 0 then
    begin
      for i := FBufLength - 1 downto 0 do
        (dest +i)^ := (src+i)^;
    end
    else
    begin
      for i := 0 to FBufLength - 1 do
      (dest +i)^ := (src+i)^;
    end;
    FBufPtr += delta;
  end;
end;

procedure TParamBlock.UpdateRequestItemSize(Item: TParamBlockItem;
  NewSize: integer);
var i, delta: integer;
begin
  delta := NewSize - Item.FParamData^.FBufLength;
  if delta > 0 then
  begin
    FDataLength += delta;
    AdjustBuffer;
    i := Length(FItems) - 1;
    while i >= 0  do
    begin
      if FItems[i]  = Item.FParamData then
        break; {we're done}
      Moveby(FItems[i],delta);
      Dec(i);
    end;
  end
  else
  begin
    i := 0;
    while i < Length(FItems) do
    begin
      if FItems[i] = Item.FParamData then
        break; {we're done}
      Inc(i);
    end;
    Inc(i);
    while i < Length(FItems) do
    begin
      Moveby(FItems[i],delta);
      Inc(i);
    end;
    FDataLength += delta;
  end;
  Item.FParamData^.FBufLength := NewSize;
end;

constructor TParamBlock.Create;
begin
  inherited Create;
  GetMem(FBuffer,128);
  if FBuffer = nil then
    OutOfMemoryError;
  FBufferSize := 128;
  FDataLength := 0;
end;

destructor TParamBlock.Destroy;
var i: integer;
begin
  for i := 0 to Length(FItems) -1 do
    dispose(FItems[i]);
  Freemem(FBuffer);
  inherited Destroy;
end;

function TParamBlock.getBuffer: PChar;
begin
  Result := FBuffer;
end;

function TParamBlock.getDataLength: integer;
begin
  Result :=  FDataLength
end;

function TParamBlock.Add(ParamType: byte): PParamBlockItemData;
begin
  new(Result);
  Result^.FBufPtr := FBuffer + FDataLength;
  Result^.FBufLength := 1;
  Result^.FBufPtr^ := char(ParamType);
  Result^.FDataType := dtnone; {default}
  Inc(FDataLength,1);
  AdjustBuffer;
  SetLength(FItems,Length(FItems)+1);
  FItems[Length(FItems) - 1 ] := Result;
end;

function TParamBlock.Find(ParamType: byte): PParamBlockItemData;
var i: integer;
begin
  Result := nil;
  for i := 0 to getCount - 1 do
    if FItems[i]^.FBufPtr^ = char(ParamType) then
    begin
      Result := FItems[i];
      Exit;
    end;
end;

function TParamBlock.GetItems(index: integer): PParamBlockItemData;
begin
  if (index >= 0 ) and (index < Length(FItems)) then
   Result := FItems[index]
 else
   IBError(ibxePBIndexError,[index]);
end;

function TParamBlock.getCount: integer;
begin
  Result := Length(FItems);
end;

end.

