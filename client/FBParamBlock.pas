unit FBParamBlock;

{$mode objfpc}{$H+}

interface

{Provides common handling for the DPB, TPB, SPB and Service Request Block (SRB)}

uses
  Classes, SysUtils, IB, FBClientAPI, FBActivityMonitor;

type
  TParamDataType = (dtString, dtString2, dtByte, dtInteger,
                    dtShortInteger,dtTinyInteger,dtnone);

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

  TParamBlock = class(TInterfaceParent)
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
    procedure Remove(ParamType: byte);
    procedure PrintBuf;
  end;

  { TParamBlockItem }

  TParamBlockItem = class(TInterfaceParent)
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
     procedure addByte(aValue: byte);
     procedure addShortInteger(aValue: integer);
     procedure setAsByte(aValue: byte);
     procedure SetAsInteger(aValue: integer);
     procedure SetAsShortInteger(aValue: integer);
     procedure SetAsTinyInteger(aValue: integer);
     procedure SetAsString(aValue: string);
     procedure SetAsString2(aValue: string);
  end;

  { TDPBItem }

  TDPBItem = class(TParamBlockItem,IDPBItem);

  { TDPB }

  TDPB = class(TParamBlock, IDPB)
  public
    constructor Create;

  public
    {IDPB}
    function Add(ParamType: byte): IDPBItem;
    function Find(ParamType: byte): IDPBItem;
    function GetItems(index: integer): IDPBItem;
  end;

  { TTPBItem }

  TTPBItem = class(TParamBlockItem,ITPBItem);

  { TTPB }

  TTPB = class(TParamBlock, ITPB)
    constructor Create;

  public
    {ITPB}
    function Add(ParamType: byte): ITPBItem;
    function Find(ParamType: byte): ITPBItem;
    function GetItems(index: integer): ITPBItem;
  end;

  { TSPB }

  TSPB = class(TParamBlock,ISPB)
  public
   constructor Create;

   function Add(ParamType: byte): ISPBItem;
   function Find(ParamType: byte): ISPBItem;
   function getItems(index: integer): ISPBItem;
  end;

  { TSRB }

  TSRB = class(TParamBlock,ISRB)
  public
   function Add(ParamType: byte): ISRBItem;
   function Find(ParamType: byte): ISRBItem;
   function getItems(index: integer): ISRBItem;
  end;

  { TSPBItem }

  TSPBItem = class(TParamBlockItem,ISPBItem);

  { TSRBItem }

  TSRBItem = class(TParamBlockItem,ISRBItem)
  public
    function ISRBItem.SetAsString = SetAsString2;
  end;

implementation

uses FBMessages;

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
  with FirebirdClientAPI, FParamData^ do
  case FDataType of
  dtInteger:
    Result := DecodeInteger(FBufPtr+1,4);
  dtShortInteger:
    Result := DecodeInteger(FBufPtr+1,2);
  dtTinyInteger:
    Result := DecodeInteger(FBufPtr+1,1);
  else
    IBError(ibxePBParamTypeError,[nil]);
  end;
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
      with FirebirdClientAPI do
        len := DecodeInteger(FBufPtr+1,2);
      SetString(Result,FBufPtr+3,len);
    end;
    else
      IBError(ibxeOutputBlockTypeError,[nil]);
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

procedure TParamBlockItem.addByte(aValue: byte);
var len: integer;
    P: PChar;
begin
  with FParamData^ do
  begin
    P := FBufPtr + FBufLength;
    len := FBufLength + 1;
    FOwner.UpdateRequestItemSize(self,len);
    P^ := char(aValue)
  end;
end;

procedure TParamBlockItem.addShortInteger(aValue: integer);
var len: integer;
    P: PChar;
begin
  with FParamData^ do
  begin
    P := FBufPtr + FBufLength;
    len := FBufLength + 2;
    FOwner.UpdateRequestItemSize(self,len);
    with FirebirdClientAPI do
      EncodeInteger(aValue,2,P);
  end;
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
    with FirebirdClientAPI do
      EncodeInteger(aValue,4,FBufPtr+1);
    FDataType := dtInteger;
  end;
end;

procedure TParamBlockItem.SetAsShortInteger(aValue: integer);
begin
  with FParamData^ do
  begin
    if FBufLength <> 3 then
      FOwner.UpdateRequestItemSize(self,3);
    with FirebirdClientAPI do
      EncodeInteger(aValue,2,FBufPtr+1);
    FDataType := dtShortInteger;
  end;
end;

procedure TParamBlockItem.SetAsTinyInteger(aValue: integer);
begin
  with FParamData^ do
  begin
    if FBufLength <> 2 then
      FOwner.UpdateRequestItemSize(self,2);
    with FirebirdClientAPI do
      EncodeInteger(aValue,1,FBufPtr+1);
    FDataType := dtTinyInteger;
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
    with FirebirdClientAPI do
      EncodeInteger(len,2,FBufPtr+1);
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

procedure TParamBlock.Remove(ParamType: byte);
var P: PParamBlockItemData;
    i, j: integer;
begin
  P := nil;
  for i := 0 to getCount - 1 do
    if FItems[i]^.FBufPtr^ = char(ParamType) then
    begin
      P := FItems[i];
      for j := i + 1 to getCount - 1 do
      begin
        MoveBy(FItems[j],-P^.FBufLength);
        FItems[j - 1] := FItems[j];
      end;
      FDataLength -= P^.FBufLength;
      dispose(P);
      SetLength(FItems,Length(FItems)-1);
      Exit;
    end;
end;

procedure TParamBlock.PrintBuf;
var i: integer;
begin
  write(ClassName,': ');
  for i := 0 to getDataLength - 1 do
    write(Format('%x ',[byte(FBuffer[i])]));
  writeln
end;

{ TDPB }

constructor TDPB.Create;
begin
  inherited Create;
  FDataLength := 1;
  FBuffer^ := char(isc_dpb_version1);
end;

function TDPB.Add(ParamType: byte): IDPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited Add(ParamType);
  Result := TDPBItem.Create(self,Item);
end;

function TDPB.Find(ParamType: byte): IDPBItem;
var Item: PParamBlockItemData;
begin
  Result := nil;
  Item := inherited Find(ParamType);
  if Item <> nil then
    Result := TDPBItem.Create(self,Item);
end;

function TDPB.getItems(index: integer): IDPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited getItems(index);
  Result := TDPBItem.Create(self,Item);
end;

{ TTPB }

constructor TTPB.Create;
begin
  inherited Create;
  FDataLength := 1;
  FBuffer^ := char(isc_tpb_version3);
end;

function TTPB.Add(ParamType: byte): ITPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited Add(ParamType);
  Result := TTPBItem.Create(self,Item);
end;

function TTPB.Find(ParamType: byte): ITPBItem;
var Item: PParamBlockItemData;
begin
  Result := nil;
  Item := inherited Find(ParamType);
  if Item <> nil then
    Result := TTPBItem.Create(self,Item);
end;

function TTPB.getItems(index: integer): ITPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited getItems(index);
  Result := TTPBItem.Create(self,Item);
end;

{ TSRB }

function TSRB.Add(ParamType: byte): ISRBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited Add(ParamType);
  Result := TSRBItem.Create(self,Item);
end;

function TSRB.Find(ParamType: byte): ISRBItem;
var Item: PParamBlockItemData;
begin
  Result := nil;
  Item := inherited Find(ParamType);
  if Item <> nil then
    Result := TSRBItem.Create(self,Item);
end;

function TSRB.getItems(index: integer): ISRBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited getItems(index);
  Result := TSRBItem.Create(self,Item);
end;

{ TSPB }

constructor TSPB.Create;
begin
  inherited Create;
  FDataLength := 2;
  FBuffer^ := char(isc_spb_version);
  (FBuffer+1)^ := char(isc_spb_current_version);
end;

function TSPB.Add(ParamType: byte): ISPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited Add(ParamType);
  Result := TSPBItem.Create(self,Item);
end;

function TSPB.Find(ParamType: byte): ISPBItem;
var Item: PParamBlockItemData;
begin
  Result := nil;
  Item := inherited Find(ParamType);
  if Item <> nil then
    Result := TSPBItem.Create(self,Item);
end;

function TSPB.getItems(index: integer): ISPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited getItems(index);
  Result := TSPBItem.Create(self,Item);
end;

end.

