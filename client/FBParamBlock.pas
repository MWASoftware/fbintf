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
{$mode objfpc}{$H+}
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

  { TCustomParamBlock }

  generic TCustomParamBlock<_TItem; _IItem> = class(TParamBlock)
  public
    function Add(ParamType: byte): _IItem;
    function Find(ParamType: byte): _IItem;
    function GetItems(index: integer): _IItem;
  end;

  { TDPBItem }

  TDPBItem = class(TParamBlockItem,IDPBItem);

  { TDPB }

  TDPB = class (specialize TCustomParamBlock<TDPBItem,IDPBItem>, IDPB)
  public
    constructor Create;
  end;

  { TTPBItem }

  TTPBItem = class(TParamBlockItem,ITPBItem);

  { TTPB }

  TTPB = class (specialize TCustomParamBlock<TTPBItem,ITPBItem>, ITPB)
  public
    constructor Create;
  end;

  { TSPBItem }

  TSPBItem = class(TParamBlockItem,ISPBItem);

  { TSPB }

  TSPB = class (specialize TCustomParamBlock<TSPBItem,ISPBItem>, ISPB)
  public
   constructor Create;
  end;

  { TSRBItem }

  TSRBItem = class(TParamBlockItem,ISRBItem)
  public
    function ISRBItem.SetAsString = SetAsString2;
    function ISRBItem.SetAsByte = SetAsByte2;
  end;

  { TSRB }

  TSRB = class (specialize TCustomParamBlock<TSRBItem,ISRBItem>, ISRB);

  { TSQPBItem }

  TSQPBItem = class(TParamBlockItem,ISQPBItem)
  public
   function CopyFrom(source: TStream; count: integer): integer;
   procedure ISQPBItem.SetAsInteger = SetAsInteger2;
   procedure ISQPBItem.SetAsString = SetAsString2;
  end;

  { TSQPB }

  TSQPB = class (specialize TCustomParamBlock<TSQPBItem,ISQPBItem>, ISQPB);

  { TBPBItem }

  TBPBItem =  class(TParamBlockItem,IBPBItem)
  public
    procedure IBPBItem.SetAsInteger = SetAsInteger1;
  end;

  { TBPB }

  TBPB = class (specialize TCustomParamBlock<TBPBItem,IBPBItem>, IBPB)
  public
   constructor Create;
  end;

implementation

uses FBMessages;

const
  MaxBufferSize = 65535;

{ TCustomParamBlock }

function TCustomParamBlock.Add(ParamType: byte): _IItem;
var Item: PParamBlockItemData;
begin
  Item := inherited Add(ParamType);
  Result := _TItem.Create(self,Item);
end;

function TCustomParamBlock.Find(ParamType: byte): _IItem;
var Item: PParamBlockItemData;
begin
  Result := nil;
  Item := inherited Find(ParamType);
  if Item <> nil then
    Result := _TItem.Create(self,Item);
end;

function TCustomParamBlock.GetItems(index: integer): _IItem;
var Item: PParamBlockItemData;
begin
  Item := inherited getItems(index);
  Result := _TItem.Create(self,Item);
end;

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

{ TBPB }

constructor TBPB.Create;
begin
  inherited Create;
  FDataLength := 1;
  FBuffer^ := char(isc_bpb_version1);
end;

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
    FBufPtr += delta;
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

end.

