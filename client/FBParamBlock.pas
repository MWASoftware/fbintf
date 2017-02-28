(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
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
unit FBParamBlock;

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

{Provides common handling for the DPB, TPB, SPB and Service Request Block (SRB)}

uses
  Classes, SysUtils, IB, FBClientAPI, FBActivityMonitor;

type
  TParamDataType = (dtString, dtString2, dtString0, dtByte, dtByte2, dtInteger,  dtInteger1,
                    dtInteger2,  dtShortInteger,dtTinyInteger,dtnone);

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

  TParamBlock = class(TFBInterfacedObject)
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
    function AvailableBufferSpace: integer;

  public
    function getCount: integer;
    procedure Remove(ParamType: byte);
    procedure PrintBuf;
  end;

  { TParamBlockItem }

  TParamBlockItem = class(TFBInterfacedObject)
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
     procedure setAsByte2(aValue: byte);
     procedure SetAsInteger(aValue: integer);
     procedure SetAsInteger1(aValue: integer);
     procedure SetAsInteger2(aValue: integer);
     procedure SetAsShortInteger(aValue: integer);
     procedure SetAsTinyInteger(aValue: integer);
     procedure SetAsString(aValue: string);
     procedure SetAsString2(aValue: string);
     procedure SetAsString0(aValue: string);
  end;

 {Delphi generic's don't really work as well as FPC. It's difficult enought to
   call a constructor with parameters, but just about impossible to extract
   an interface from a class when both are parameters to the generic class. So
   we just do this explicitly. To be honest, we could use the Delphi approach
   for FPC - but why lower yourself to this level just because of Delphi. Better
   to leave it as an example of where FPC is better than Delphi.
   }

{$IFDEF FPC}
  { TCustomParamBlock }

  TCustomParamBlock<_TItem, _IItem> = class(TParamBlock)
  public
    function Add(ParamType: byte): _IItem;
    function Find(ParamType: byte): _IItem;
    function GetItems(index: integer): _IItem;
  end;

  { TDPB }

  TDPB = class (TCustomParamBlock<TDPBItem,IDPBItem>, IDPB)
  public
    constructor Create;
  end;

  { TTPB }

  TTPB = class (TCustomParamBlock<TTPBItem,ITPBItem>, ITPB)
  public
    constructor Create;
  end;

  { TSPB }

  TSPB = class (TCustomParamBlock<TSPBItem,ISPBItem>, ISPB)
  public
   constructor Create;
  end;

  { TSRB }

  TSRB = class (TCustomParamBlock<TSRBItem,ISRBItem>, ISRB);

  { TSQPB }

  TSQPB = class (TCustomParamBlock<TSQPBItem,ISQPBItem>, ISQPB);

  { TBPB }

  TBPB = class (TCustomParamBlock<TBPBItem,IBPBItem>, IBPB)
  public
   constructor Create;
  end;
{$ELSE}

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

  { TTPB }

  TTPB = class(TParamBlock, ITPB)
  public
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

  { TSQPB }

  TSQPB = class(TParamBlock,ISQPB)
  public
   function Add(ParamType: byte): ISQPBItem;
   function Find(ParamType: byte): ISQPBItem;
   function getItems(index: integer): ISQPBItem;
  end;

  { TBPB }

  TBPB = class(TParamBlock,IBPB)
  public
   constructor Create;
  public
   {IBPB}
   function Add(ParamType: byte): IBPBItem;
   function Find(ParamType: byte): IBPBItem;
   function getItems(index: integer): IBPBItem;
  end;
{$ENDIF}


  { TDPBItem }

  TDPBItem = class(TParamBlockItem,IDPBItem);

  { TTPBItem }

  TTPBItem = class(TParamBlockItem,ITPBItem);

  { TSPBItem }

  TSPBItem = class(TParamBlockItem,ISPBItem);

  { TSRBItem }

  TSRBItem = class(TParamBlockItem,ISRBItem)
  public
    {$IFDEF FPC}
    procedure ISRBItem.SetAsString = SetAsString2;
    procedure ISRBItem.SetAsByte = SetAsByte2;
    {$ELSE}
    procedure SetAsString(aValue: string) ;
    procedure SetAsByte(aValue: byte);
    {$ENDIF}
  end;

  { TSQPBItem }

  TSQPBItem = class(TParamBlockItem,ISQPBItem)
  public
   function CopyFrom(source: TStream; count: integer): integer;
   {$IFDEF FPC}
   procedure ISQPBItem.SetAsInteger = SetAsInteger2;
   procedure ISQPBItem.SetAsString = SetAsString2;
   {$ELSE}
   procedure SetAsString(aValue: string) ;
   procedure SetAsInteger(aValue: integer);
   {$ENDIF}
  end;

  { TBPBItem }

  TBPBItem =  class(TParamBlockItem,IBPBItem)
  public
   {$IFDEF FPC}
    procedure IBPBItem.SetAsInteger = SetAsInteger1;
   {$ELSE}
    procedure SetAsInteger(aValue: integer);
   {$ENDIF}
  end;

implementation

uses FBMessages;

const
  MaxBufferSize = 65535;

{ TBPBItem }
{$IFNDEF FPC}
procedure TBPBItem.SetAsInteger(aValue: integer);
begin
  SetAsInteger1(aValue);
end;
{$ENDIF}

{ TSRBItem }

{$IFNDEF FPC}
procedure TSRBItem.SetAsString(aValue: string);
begin
  SetAsString2(aValue);
end;

procedure TSRBItem.SetAsByte(aValue: byte);
begin
  SetAsByte2(aValue);
end;
{$ENDIF}

{ TSQPBItem }

function TSQPBItem.CopyFrom(source: TStream; count: integer): integer;
begin
  if count > (FOwner.AvailableBufferSpace - 4) then
    count := FOwner.AvailableBufferSpace - 4;
    with FParamData^ do
    begin
      FOwner.UpdateRequestItemSize(self,count + 4);
      Result := source.Read((FBufPtr+3)^,count);
      with FirebirdClientAPI do
        EncodeInteger(Result,2,FBufPtr+1);
      (FBufPtr+Result + 3)^ := chr(isc_info_end);
      if Result <> count then
        FOwner.UpdateRequestItemSize(self,Result + 4);
      FDataType := dtString2;
    end;
end;

{$IFNDEF FPC}
procedure TSQPBItem.SetAsString(aValue: string);
begin
  SetAsString2(aValue);
end;

procedure TSQPBItem.SetAsInteger(aValue: integer);
begin
  SetAsInteger2(aValue);
end;
{$ENDIF}

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
  dtInteger1:
    Result := DecodeInteger(FBufPtr+2,2);
  dtInteger2:
    Result := DecodeInteger(FBufPtr+3,4);
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
  dtInteger,
  dtInteger1,
  dtInteger2,
  dtShortInteger,
  dtTinyInteger:
    Result := IntToStr(getAsInteger);
  dtByte,
  dtByte2:
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
  dtString0:
      Result := strpas(FBufPtr+1);
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
  if FDataType = dtByte2 then
    Result := byte((FBufPtr+1)^)
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
    (FBufPtr+2)^ := chr(aValue);
  end;
end;

procedure TParamBlockItem.setAsByte2(aValue: byte);
begin
  with FParamData^ do
  begin
    if FBufLength <> 2 then
      FOwner.UpdateRequestItemSize(self,2);
    FDataType := dtByte2;
    (FBufPtr+1)^ := chr(aValue);
  end;
end;

{Four byte integer - no length}

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

{Four byte integer - length byte}

procedure TParamBlockItem.SetAsInteger1(aValue: integer);
begin
  with FParamData^ do
  begin
    if FBufLength <> 6 then
      FOwner.UpdateRequestItemSize(self,6);
    (FBufPtr+1)^ := chr(4);
    with FirebirdClientAPI do
      EncodeInteger(aValue,4,FBufPtr+2);
    FDataType := dtInteger1;
  end;
end;

{Four byte integer - 2 byte length}

procedure TParamBlockItem.SetAsInteger2(aValue: integer);
begin
  with FParamData^ do
  begin
    if FBufLength <> 7 then
      FOwner.UpdateRequestItemSize(self,7);
    with FirebirdClientAPI do
    begin
      EncodeInteger(4,2,FBufPtr+1); {Encode length as two bytes}
      EncodeInteger(aValue,4,FBufPtr+3);
    end;
    FDataType := dtInteger2
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

{Short string encoding}

procedure TParamBlockItem.SetAsString(aValue: string);
var len: integer;
begin
  with FParamData^ do
  begin
    len := Length(aValue);
    if len > 255 then
      IBError(ibxStringTooLong,[aValue,255]);
    FOwner.UpdateRequestItemSize(self,len+2);
    (FBufPtr+1)^ := char(len);
    if len > 0 then
      Move(aValue[1],(FBufPtr+2)^,len);
    FDataType := dtString;
  end;
end;

{Long string up to 65535 encoding}

procedure TParamBlockItem.SetAsString2(aValue: string);
var len: integer;
begin
  with FParamData^ do
  begin
    len := Length(aValue);
    if len > 65535 then
      IBError(ibxStringTooLong,[aValue,65535]);
    FOwner.UpdateRequestItemSize(self,len + 3);
    with FirebirdClientAPI do
      EncodeInteger(len,2,FBufPtr+1);
    if len > 0 then
      Move(aValue[1],(FBufPtr+3)^,len);
    FDataType := dtString2;
  end;
end;

{Zero byte terminated string encoding}

procedure TParamBlockItem.SetAsString0(aValue: string);
var len: integer;
begin
  with FParamData^ do
  begin
    len := Length(aValue);
    FOwner.UpdateRequestItemSize(self,len+2);
    if len > 0 then
      Move(aValue[1],(FBufPtr+1)^,len);
    (FBufPtr+len+1)^ := #0;
    FDataType := dtString0;
  end;
end;

{ TParamBlock }

procedure TParamBlock.AdjustBuffer;
var P: PChar;
    i: integer;
    headerLen: integer;
begin
  if FDataLength > FBufferSize then
  begin
    if Length(FItems) > 0 then
      headerLen := FItems[0]^.FBufPtr - FBuffer
    else
      headerLen := 0;
    FBufferSize := 2*FDataLength;
    ReallocMem(FBuffer,FBufferSize);
    P := FBuffer + headerLen;
    for i := 0 to Length(FItems) - 1 do
    begin
      FItems[i]^.FBufPtr := P;
      Inc(P,FItems[i]^.FBuflength);
    end;
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
    FBufPtr := FBufPtr + delta;
  end;
end;

procedure TParamBlock.UpdateRequestItemSize(Item: TParamBlockItem;
  NewSize: integer);
var i, delta: integer;
begin
  delta := NewSize - Item.FParamData^.FBufLength;
  Item.FParamData^.FBufLength := NewSize;
  if delta > 0 then
  begin
    if FDataLength + delta > MaxBufferSize then
      IBError(ibxeParamBufferOverflow,[nil]);
    FDataLength := FDataLength + delta;
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
    FDataLength := FDataLength + delta;
  end;
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
  if FDataLength = 0 then
    Result := nil
  else
    Result := FBuffer;
end;

function TParamBlock.getDataLength: integer;
begin
  Result :=  FDataLength
end;

function TParamBlock.AvailableBufferSpace: integer;
begin
  Result := MaxBufferSize - FDataLength;
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
      FDataLength := FDataLength - P^.FBufLength;
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

{$IFDEF FPC}
{ TCustomParamBlock }

function TCustomParamBlock<_TItem, _IItem>.Add(ParamType: byte): _IItem;
var Item: PParamBlockItemData;
begin
  Item := inherited Add(ParamType);
  Result := _TItem.Create(self,Item);
end;

function TCustomParamBlock<_TItem, _IItem>.Find(ParamType: byte): _IItem;
var Item: PParamBlockItemData;
begin
  Result := nil;
  Item := inherited Find(ParamType);
  if Item <> nil then
    Result := _TItem.Create(self,Item);
end;

function TCustomParamBlock<_TItem, _IItem>.GetItems(index: integer): _IItem;
var Item: PParamBlockItemData;
begin
  Item := inherited getItems(index);
  Result := _TItem.Create(self,Item);
end;

{ TDPB }

constructor TDPB.Create;
begin
  inherited Create;
  FDataLength := 1;
  FBuffer^ := char(isc_dpb_version1);
end;

{ TTPB }

constructor TTPB.Create;
begin
  inherited Create;
  FDataLength := 1;
  FBuffer^ := char(isc_tpb_version3);
end;

{ TSPB }

constructor TSPB.Create;
begin
  inherited Create;
  FDataLength := 2;
  FBuffer^ := char(isc_spb_version);
  (FBuffer+1)^ := char(isc_spb_current_version);
end;

{ TBPB }

constructor TBPB.Create;
begin
  inherited Create;
  FDataLength := 1;
  FBuffer^ := char(isc_bpb_version1);
end;


{$ELSE}
{and now the long winded Delphi way}

{ TSQPB }

function TSQPB.Add(ParamType: byte): ISQPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited Add(ParamType);
  Result := TSQPBItem.Create(self,Item);
end;

function TSQPB.Find(ParamType: byte): ISQPBItem;
var Item: PParamBlockItemData;
begin
  Result := nil;
  Item := inherited Find(ParamType);
  if Item <> nil then
    Result := TSQPBItem.Create(self,Item);
end;

function TSQPB.getItems(index: integer): ISQPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited getItems(index);
  Result := TSQPBItem.Create(self,Item);
end;

{ TBPB }

constructor TBPB.Create;
begin
  inherited Create;
  FDataLength := 1;
  FBuffer^ := char(isc_bpb_version1);
end;

function TBPB.Add(ParamType: byte): IBPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited Add(ParamType);
  Result := TBPBItem.Create(self,Item);
end;

function TBPB.Find(ParamType: byte): IBPBItem;
var Item: PParamBlockItemData;
begin
  Result := nil;
  Item := inherited Find(ParamType);
  if Item <> nil then
    Result := TBPBItem.Create(self,Item);
end;

function TBPB.getItems(index: integer): IBPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited getItems(index);
  Result := TBPBItem.Create(self,Item);
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
{$ENDIF}

end.

