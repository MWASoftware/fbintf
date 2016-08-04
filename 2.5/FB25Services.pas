unit FB25Services;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBExternals, IBHeader;

const
  DefaultBufferSize = 32000;

type
  TServiceRequest = class;

  { TServiceRequestItem }

  TServiceRequestItem = class(TInterfacedObject,IServiceRequestItem)
  private
    FOwner: TServiceRequest;
    FBufPtr: PChar;
    FBuflength: integer;
    FIsInteger: boolean;
    procedure MoveBy(delta: integer);
  public
    constructor Create(AOwner: TServiceRequest; Param: byte; BufPtr: PChar;
      Buflength: integer);

  public
    {IServiceRequestItem}
    function getItemType: byte;
    function getAsString: string;
    function getAsInteger: integer;
    procedure setAsString(aValue: string);
    procedure setAsInteger(aValue: integer);
    property AsString: string read getAsString write setAsString;
    property AsInteger: integer read getAsInteger write setAsInteger;
  end;

  { TServiceRequest }

  TServiceRequest = class(TInterfacedObject,IServiceRequest)
  private
    FItems: array of IServiceRequestItem;
    FBuffer: PChar;
    FDataLength: integer;
    FBufferSize: integer;
    procedure AdjustBuffer;
    procedure UpdateRequestItemSize(Item: TServiceRequestItem; NewSize: integer);
  public
    constructor Create(action: byte);
    destructor Destroy; override;
    function getBuffer: PChar;
    function getBufferLength: integer;

  public
    {IServiceRequest}
    function getAction: byte;
    function Add(ItemType: byte): IServiceRequestItem;
    function getCount: integer;
    function getItem(index: integer): IServiceRequestItem;
    function find(ItemType: byte): IServiceRequestItem;
    property Items[index: integer]: IServiceRequestItem read getItem; default;
  end;

  { TServiceQueryResultSubItem }

  TServiceQueryResultSubItem = class(TInterfacedObject,IServiceQueryResultSubItem)
  private
    FBufPtr: PChar;
    FDataLength: integer;
    FSize: integer;
    FIsInteger: boolean;
    FTruncated: boolean;
  public
    constructor CreateAsInteger(BufPtr: PChar);
    constructor CreateAsString(BufPtr: PChar);
    constructor CreateAsByte(BufPtr: PChar);
    function getSize: integer;

  public
    {IServiceQueryResultSubItem}
    function getItemType: byte;
    function getDataSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: string;
    function getAsInteger: integer;
    function getIsTruncated: boolean;
 end;

  { TServiceQueryResultItem }

  TServiceQueryResultItem = class(TServiceQueryResultSubItem,IServiceQueryResultItem)
  private
    FSubItems: array of IServiceQueryResultSubItem;
    procedure ParseBuffer;
    procedure ParseConfigItems;
  public
    constructor CreateAsList(BufPtr: PChar);
    constructor CreateAsConfigItems(BufPtr: PChar);

  public
    {IServiceQueryResultItem}
    function getCount: integer;
    function getItem(index: integer): IServiceQueryResultSubItem;
    function find(ItemType: byte): IServiceQueryResultSubItem;
    property Items[index: integer]: IServiceQueryResultSubItem read getItem; default;
  end;

  { TServiceQueryResults }

  TServiceQueryResults = class(TInterfacedObject,IServiceQueryResults)
  private
    FBuffer: PChar;
    FSize: integer;
    FBufferParsed: boolean;
    FItems: array of IServiceQueryResultItem;
    procedure ParseBuffer;
  public
    constructor Create(aSize: integer = DefaultBufferSize);
    destructor Destroy; override;
    function Buffer: PChar;
    function getBufSize: integer;

  public
    {IServiceQueryResults}
    function getCount: integer;
    function getItem(index: integer): IServiceQueryResultItem;
    function find(ItemType: byte): IServiceQueryResultItem;
    property Items[index: integer]: IServiceQueryResultItem read getItem; default;
 end;

  TSPB = class;

  { TSPBItem }

  TSPBItem = class(TInterfacedObject,ISPBItem)
  private
    FOwner: TSPB;
    FBufPtr: PChar;
    FBuflength: integer;
    FIsByte: boolean;
    procedure MoveBy(delta: integer);
  public
    constructor Create(AOwner: TSPB; Param: byte; BufPtr: PChar;
       Buflength: integer);

  public
    {ISPBItem}
    function getParamType: byte;
    function getAsString: string;
    function getAsByte: byte;
    procedure setAsString(aValue: string);
    procedure setAsByte(aValue: byte);
  end;

  { TSPB }

  TSPB = class(TInterfacedObject, ISPB)
  private
    FItems: array of ISPBItem;
    FBuffer: PChar;
    FDataLength: integer;
    FBufferSize: integer;
    procedure AdjustBuffer;
    procedure UpdateRequestItemSize(Item: TSPBItem; NewSize: integer);
  public
    constructor Create;
    destructor Destroy; override;
    function getBuffer: PChar;
    function getBufferLength: integer;

  public
    {ISPB}
    function getCount: integer;
    function Add(ParamType: byte): ISPBItem;
    function Find(ParamType: byte): ISPBItem;
    procedure Remove(ParamType: byte);
    function getItems(index: integer): ISPBItem;
  end;

  { TFBServiceManager }

  TFBServiceManager = class(TInterfacedObject,IServiceManager)
  private
    FServerName: string;
    FSPB: ISPB;
    FSPBLength: Short;
    FHandle: TISC_SVC_HANDLE;
    FProtocol: TProtocol;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckServerName;
    procedure MergeBuffers(Requests: array of IServiceRequest; var buffer: PChar; var size: integer);
  public
    constructor Create(ServerName: string; Protocol: TProtocol; SPB: ISPB);
    destructor Destroy; override;
    property Handle: TISC_SVC_HANDLE read FHandle;

  public
    {IServiceManager}
    function getSPB: ISPB;
    procedure Attach;
    procedure Detach(Force: boolean=false);
    function IsAttached: boolean;
    function AllocateRequestBuffer(action: byte): IServiceRequest;
    procedure Start(Request: IServiceRequest);
    procedure StartMultiple(Requests: array of IServiceRequest);
    function Query(Request: IServiceRequest) :IServiceQueryResults;
    function QueryMultiple(Requests: array of IServiceRequest) :IServiceQueryResults;
  end;

implementation

uses FBErrorMessages;

  { TSPBItem }

  procedure TSPBItem.MoveBy(delta: integer);
  var src, dest: PChar;
      i: integer;
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

  constructor TSPBItem.Create(AOwner: TSPB; Param: byte; BufPtr: PChar;
    Buflength: integer);
  begin
    inherited Create;
    FOwner := AOwner;
    FBufPtr := BufPtr;
    FBufLength := BufLength;
    FBufPtr^ := char(Param);
    (FBufPtr+1)^ := #1;
    (FBufPtr+2)^ := #0;
    FIsByte := true; {default}
  end;

  function TSPBItem.getParamType: byte;
  begin
    Result := byte(FBufPtr^);
  end;

  function TSPBItem.getAsString: string;
  var len: byte;
  begin
    if FIsByte then
      Result := IntToStr(getAsByte)
    else
    begin
      len := byte((FBufPtr+1)^);
      SetString(Result,FBufPtr+2,len);
    end;
  end;

  function TSPBItem.getAsByte: byte;
  begin
    if FIsByte then
      Result := byte((FBufPtr+2)^)
    else
      IBError(ibxeSPBParamTypeError,[nil]);
  end;

  procedure TSPBItem.setAsString(aValue: string);
  var len: integer;
  begin
    len := Length(aValue);
    if len <> FBufLength - 2 then
      FOwner.UpdateRequestItemSize(self,len+2);
    (FBufPtr+1)^ := char(len);
    Move(aValue[1],(FBufPtr+2)^,len);
    FIsByte := false;
  end;

  procedure TSPBItem.setAsByte(aValue: byte);
  begin
    if FBufLength <> 3 then
    FOwner.UpdateRequestItemSize(self,3);
    FIsByte := true;
    (FBufPtr+1)^ := #1;
    (FBufPtr+2)^ := char(aValue);
  end;

  { TSPB }

  procedure TSPB.AdjustBuffer;
  begin
    if FDataLength > FBufferSize then
    begin
      FBufferSize := FDataLength;
      ReallocMem(FBuffer,FBufferSize);
    end;
  end;

  procedure TSPB.UpdateRequestItemSize(Item: TSPBItem; NewSize: integer
    );
  var i, delta: integer;
  begin
    delta := NewSize - Item.FBufLength;
    if delta > 0 then
    begin
      FDataLength += delta;
      AdjustBuffer;
      i := Length(FItems) - 1;
      while i >= 0  do
      begin
        if (FItems[i] as TSPBItem) = Item then
          break; {we're done}
        (FItems[i] as TSPBItem).Moveby(delta);
        Dec(i);
      end;
    end
    else
    begin
      i := 0;
      while i < Length(FItems) do
      begin
        if (FItems[i] as TSPBItem) = Item then
          break; {we're done}
        Inc(i);
      end;
      Inc(i);
      while i < Length(FItems) do
      begin
        (FItems[i] as TSPBItem).Moveby(delta);
        Inc(i);
      end;
      FDataLength += delta;
    end;
    Item.FBufLength := NewSize;
  end;

  constructor TSPB.Create;
  begin
    inherited Create;
    GetMem(FBuffer,128);
    if FBuffer = nil then
      OutOfMemoryError;
    FBufferSize := 128;
    FDataLength := 2;
    FBuffer^ := char(isc_spb_version);
    (FBuffer+1)^ := char(isc_spb_current_version);
  end;

  destructor TSPB.Destroy;
  begin
    Freemem(FBuffer);
    inherited Destroy;
  end;

  function TSPB.getBuffer: PChar;
  begin
    Result := FBuffer;
  end;

  function TSPB.getBufferLength: integer;
  begin
    Result :=  FDataLength
  end;

  function TSPB.getCount: integer;
  begin
    Result := Length(FItems);
  end;

  function TSPB.Add(ParamType: byte): ISPBItem;
  var P: PChar;
  begin
    P := FBuffer + FDataLength;
    Inc(FDataLength,3); {assume byte}
    AdjustBuffer;
    Result := TSPBItem.Create(self,ParamType,P,3);
    SetLength(FItems,Length(FItems)+1);
    FItems[Length(FItems) - 1 ] := Result;
  end;

  function TSPB.Find(ParamType: byte): ISPBItem;
  var i: integer;
  begin
    Result := nil;
    for i := 0 to getCount - 1 do
      if FItems[i].getParamType = ParamType then
      begin
        Result := FItems[i];
        Exit;
      end;
  end;

  procedure TSPB.Remove(ParamType: byte);
  var i: integer;
      len: integer;
  begin
    i := 0;
    while (i < Length(FItems)) and  (FItems[i].getParamType <> ParamType) do
      inc(i);

    if i < Length(FItems) then
    begin
      len := (FItems[i] as TSPBItem).FBuflength;
      inc(i);
      while i < Length(FItems) do
      begin
         (FItems[i] as TSPBItem).MoveBy(-len);
         Inc(i);
      end;
      Dec(FDataLength,len);
    end;
  end;

  function TSPB.getItems(index: integer): ISPBItem;
  begin
     if (index >= 0 ) and (index < Length(FItems)) then
      Result := FItems[index]
    else
      IBError(ibxeSPBIndexError,[index]);
  end;

  { TServiceQueryResultItem }

procedure TServiceQueryResultItem.ParseBuffer;
var P: PChar;
    i: integer;
    group: byte;
begin
  P := FBufPtr + 1;
  i := 0;
  group := byte(P^);
  while (P < FBufPtr + FDataLength) and (P^ <> char(isc_info_flag_end)) do
  begin
    case group of
    isc_info_svc_svr_db_info:
        case integer(P^) of
        isc_spb_num_att,
        isc_spb_num_db:
          FSubItems[i] := TServiceQueryResultSubItem.CreateAsInteger(P);

        isc_spb_dbname:
          FSubItems[i] := TServiceQueryResultSubItem.CreateAsString(P);

        else
          IBError(ibxeOutputParsingError, [nil]);
        end;

    isc_info_svc_get_license:
      case integer(P^) of
      isc_spb_lic_id,
      isc_spb_lic_key:
        FSubItems[i] := TServiceQueryResultSubItem.CreateAsInteger(P);
      else
        IBError(ibxeOutputParsingError, [nil]);
      end;

    isc_info_svc_limbo_trans:
     case integer(P^) of
     isc_spb_single_tra_id,
     isc_spb_multi_tra_id:
       FSubItems[i] := TServiceQueryResultSubItem.CreateAsInteger(P);

     isc_spb_tra_host_site,
     isc_spb_tra_remote_site,
     isc_spb_tra_db_path:
       FSubItems[i] := TServiceQueryResultSubItem.CreateAsString(P);

     isc_spb_tra_advise,
     isc_spb_tra_state:
       FSubItems[i] := TServiceQueryResultSubItem.CreateAsByte(P);
     else
       IBError(ibxeOutputParsingError, [nil]);
     end;

    isc_info_svc_get_users:
      case integer(P^) of
      isc_spb_sec_userid,
      isc_spb_sec_groupid:
        FSubItems[i] := TServiceQueryResultSubItem.CreateAsInteger(P);

      isc_spb_sec_username,
      isc_spb_sec_password,
      isc_spb_sec_firstname,
      isc_spb_sec_middlename,
      isc_spb_sec_lastname:
        FSubItems[i] := TServiceQueryResultSubItem.CreateAsString(P);

      else
        IBError(ibxeOutputParsingError, [nil]);
      end;

    end;
    P +=  (FSubItems[i] as TServiceQueryResultSubItem).getSize;
    Inc(i);
  end;
  FDataLength := 0;
  for i := 0 to Length(FSubItems) - 1 do
    FDataLength += (FSubItems[i] as TServiceQueryResultSubItem).getSize;
end;

procedure TServiceQueryResultItem.ParseConfigItems;
var P: PChar;
    i: integer;
    group: char;
begin
  P := FBufPtr + 3; {skip length bytes}
  i := 0;
  while P < FBufPtr + FDataLength do
  begin
    FSubItems[i] := TServiceQueryResultSubItem.CreateAsInteger(P);
    P +=  (FSubItems[i] as TServiceQueryResultSubItem).getSize;
    Inc(i);
  end;
end;

constructor TServiceQueryResultItem.CreateAsList(BufPtr: PChar);
begin
  inherited Create;
  FIsInteger := false;
  FBufPtr := BufPtr;
  ParseBuffer;
end;

constructor TServiceQueryResultItem.CreateAsConfigItems(BufPtr: PChar);
begin
  inherited Create;
  FIsInteger := false;
  FBufPtr := BufPtr;
  with Firebird25ClientAPI do
    FDataLength := isc_portable_integer(FBufPtr+1, 2);
  ParseConfigItems;
end;

function TServiceQueryResultItem.getCount: integer;
begin
  Result := Length(FSubItems);
end;

function TServiceQueryResultItem.getItem(index: integer
  ): IServiceQueryResultSubItem;
begin
  if (index >= 0) and (index < Length(FSubItems)) then
    Result := FSubItems[index]
  else
    IBError(ibxeServiceResponseIndexError,[index]);
end;

function TServiceQueryResultItem.find(ItemType: byte
  ): IServiceQueryResultSubItem;
var i: integer;
begin
  Result := nil;
  for i := 0 to getCount - 1 do
    if FSubItems[i].getItemType = ItemType then
    begin
      Result := FSubItems[i];
      Exit;
    end;
end;

{ TServiceQueryResultSubItem }

constructor TServiceQueryResultSubItem.CreateAsInteger(BufPtr: PChar);
begin
  inherited Create;
  FIsInteger := true;
  FBufPtr := BufPtr;
  FDataLength := 4;
  FSize := 5;
end;

constructor TServiceQueryResultSubItem.CreateAsString(BufPtr: PChar);
begin
  inherited Create;
  FIsInteger := false;
  FBufPtr := BufPtr;
  with Firebird25ClientAPI do
    FDataLength := 3 + isc_portable_integer(FBufPtr+1, 2);
  FSize := FDataLength + 1;
end;

constructor TServiceQueryResultSubItem.CreateAsByte(BufPtr: PChar);
begin
  inherited Create;
  FIsInteger := true;
  FBufPtr := BufPtr;
  FDataLength := 1;
  FSize := 2;
end;

function TServiceQueryResultSubItem.getSize: integer;
begin
  Result := FSize;
end;

function TServiceQueryResultSubItem.getItemType: byte;
begin
  Result := byte(FBufPtr^);
end;

function TServiceQueryResultSubItem.getDataSize: integer;
begin
  Result := FDataLength;
end;

procedure TServiceQueryResultSubItem.getRawBytes(var Buffer);
begin
  Move((FBufPtr+1)^,Buffer,FDataLength);
end;

function TServiceQueryResultSubItem.getAsString: string;
begin
  if FIsInteger then
    Result := IntToStr(getAsInteger)
  else
    SetString(Result,FBufPtr+3,FDataLength-3);
end;

function TServiceQueryResultSubItem.getAsInteger: integer;
begin
  if FIsInteger then
  begin
    if FDataLength = 1 then
      Result := byte((FBufPtr+1)^)
    else
    with Firebird25ClientAPI do
      Result := isc_portable_integer(FBufPtr+1, 4)
  end
  else
    IBError(ibxeServiceResponseTypeError,[nil]);
end;

function TServiceQueryResultSubItem.getIsTruncated: boolean;
begin
  Result := FTruncated;
end;

{ TServiceQueryResults }

procedure TServiceQueryResults.ParseBuffer;
var P: PChar;
    i: integer;
begin
  if FBufferParsed then Exit;
  P := FBuffer;
  i := 0;
  while  (P < FBuffer + FSize) and (P^ <> char(isc_info_end)) do
  begin
    case integer(P^) of
      isc_info_svc_line,
      isc_info_svc_get_env,
      isc_info_svc_get_env_lock,
      isc_info_svc_get_env_msg,
      isc_info_svc_user_dbpath,
      isc_info_svc_server_version,
      isc_info_svc_implementation:
        FItems[i] := TServiceQueryResultItem.CreateAsString(P);

      isc_info_svc_get_license_mask,
      isc_info_svc_capabilities,
      isc_info_svc_version,
      isc_info_svc_running:
        FItems[i] := TServiceQueryResultItem.CreateAsInteger(P);

      isc_info_svc_to_eof:
      begin
        FItems[i] := TServiceQueryResultItem.CreateAsString(P);
        with FItems[i] as TServiceQueryResultItem do
          FTruncated := (P + FSize < FBuffer + self.FSize) and ((P + FSize)^ <> char(isc_info_truncated));
      end;

    isc_info_svc_get_config:
      FItems[i] := TServiceQueryResultItem.CreateAsConfigItems(P);

    isc_info_svc_svr_db_info,
    isc_info_svc_get_license,
    isc_info_svc_limbo_trans,
    isc_info_svc_get_users:
      FItems[i] := TServiceQueryResultItem.CreateAsList(P);
    else
       IBError(ibxeOutputParsingError, [nil]);
    end;
    P += (FItems[i] as TServiceQueryResultItem).getSize;
    Inc(i);
  end;
  FBufferParsed := true;
end;

constructor TServiceQueryResults.Create(aSize: integer);
begin
  inherited Create;
  FSize := aSize;
  GetMem(FBuffer,aSize);
  if FBuffer = nil then
    OutOfMemoryError;
  FillChar(FBuffer^,aSize,255);
  FBufferParsed := false;
end;

destructor TServiceQueryResults.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TServiceQueryResults.Buffer: PChar;
begin
  Result := FBuffer;
end;

function TServiceQueryResults.getBufSize: integer;
begin
  Result := FSize;
end;

function TServiceQueryResults.getCount: integer;
begin
  ParseBuffer;
  Result := length(FItems);
end;

function TServiceQueryResults.getItem(index: integer): IServiceQueryResultItem;
begin
  ParseBuffer;
  if (index >= 0) and (index < Length(FItems)) then
    Result := FItems[index]
  else
    IBError(ibxeServiceResponseIndexError,[index]);
end;

function TServiceQueryResults.find(ItemType: byte): IServiceQueryResultItem;
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

{ TServiceRequestItem }

procedure TServiceRequestItem.MoveBy(delta: integer);
var src, dest: PChar;
    i: integer;
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

constructor TServiceRequestItem.Create(AOwner: TServiceRequest; Param: byte;
  BufPtr: PChar; Buflength: integer);
begin
  inherited Create;
  FOwner := AOwner;
  FBufPtr := BufPtr;
  FBufLength := BufLength;
  FBufPtr^ := char(Param);
  FIsInteger := true; {default}
end;

function TServiceRequestItem.getItemType: byte;
begin
  Result := byte(FBufPtr^);
end;

function TServiceRequestItem.getAsString: string;
var len: integer;
begin
  if FIsInteger then
    Result := IntToStr(getAsInteger)
  else
  begin
    with Firebird25ClientAPI do
      len := isc_portable_integer(FBufPtr+1, 2);
    SetString(Result,FBufPtr+3,len);
  end;
end;

function TServiceRequestItem.getAsInteger: integer;
begin
  if FIsInteger then
    with Firebird25ClientAPI do
      Result := isc_portable_integer(FBufPtr+1, 4)
  else
    IBError(ibxServiceParamTypeError,[nil]);
end;

procedure TServiceRequestItem.setAsString(aValue: string);
var len: integer;
begin
  len := Length(aValue);
  if len <> FBufLength - 3 then
    FOwner.UpdateRequestItemSize(self,len+3);
  with Firebird25ClientAPI do
    EncodeLsbf(len,2,FBufPtr+1);
  Move(aValue[1],(FBufPtr+3)^,len);
  FIsInteger := false;
end;

procedure TServiceRequestItem.setAsInteger(aValue: integer);
begin
  if FBufLength <> 5 then
  FOwner.UpdateRequestItemSize(self,5);
  with Firebird25ClientAPI do
    EncodeLsbf(aValue,4,FBufPtr+1);
  FIsInteger := true;
end;

{ TServiceRequest }

procedure TServiceRequest.AdjustBuffer;
begin
  if FDataLength > FBufferSize then
  begin
    FBufferSize := FDataLength;
    ReallocMem(FBuffer,FBufferSize);
  end;
end;

procedure TServiceRequest.UpdateRequestItemSize(Item: TServiceRequestItem;
  NewSize: integer);
var i, delta: integer;
begin
  delta := NewSize - Item.FBufLength;
  if delta > 0 then
  begin
    FDataLength += delta;
    AdjustBuffer;
    i := Length(FItems) - 1;
    while i >= 0  do
    begin
      if (FItems[i] as TServiceRequestItem) = Item then
        break; {we're done}
      (FItems[i] as TServiceRequestItem).Moveby(delta);
      Dec(i);
    end;
  end
  else
  begin
    i := 0;
    while i < Length(FItems) do
    begin
      if (FItems[i] as TServiceRequestItem) = Item then
        break; {we're done}
      Inc(i);
    end;
    Inc(i);
    while i < Length(FItems) do
    begin
      (FItems[i] as TServiceRequestItem).Moveby(delta);
      Inc(i);
    end;
    FDataLength += delta;
  end;
  Item.FBufLength := NewSize;
end;

constructor TServiceRequest.Create(action: byte);
begin
  inherited Create;
  GetMem(FBuffer,128);
  if FBuffer = nil then
    OutOfMemoryError;
  FBufferSize := 128;
  FDataLength := 1;
  FBuffer^ := char(action);
end;

destructor TServiceRequest.Destroy;
begin
  Freemem(FBuffer);
  inherited Destroy;
end;

function TServiceRequest.getBuffer: PChar;
begin
  Result := FBuffer;
end;

function TServiceRequest.getBufferLength: integer;
begin
  Result :=  FDataLength
end;

function TServiceRequest.getAction: byte;
begin
  Result := byte(FBuffer^);
end;

function TServiceRequest.Add(ItemType: byte): IServiceRequestItem;
var P: PChar;
begin
  P := FBuffer + FDataLength;
  Inc(FDataLength,5); {assume integer}
  AdjustBuffer;
  Result := TServiceRequestItem.Create(self,ItemType,P,5);
  SetLength(FItems,Length(FItems)+1);
  FItems[Length(FItems) - 1 ] := Result;
end;

function TServiceRequest.getCount: integer;
begin
  Result := Length(FItems);
end;

function TServiceRequest.getItem(index: integer): IServiceRequestItem;
begin
  if (index >= 0 ) and (index < Length(FItems)) then
    Result := FItems[index]
  else
    IBError(ibxServiceRequestIndexError,[index]);
end;

function TServiceRequest.find(ItemType: byte): IServiceRequestItem;
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



{ TFBServiceManager }

{
 * GenerateSPB -
 *  Given a string containing a textual representation
 *  of the Service parameters, generate a service
 *  parameter buffer, and return it and its length
 *  in SPB and SPBLength, respectively.
}

procedure TFBServiceManager.CheckActive;
begin
  if FHandle = nil then
    IBError(ibxeServiceActive, [nil]);
end;

procedure TFBServiceManager.CheckInactive;
begin
  if FHandle <> nil then
    IBError(ibxeServiceInActive, [nil]);
end;

procedure TFBServiceManager.CheckServerName;
begin
  if (FServerName = '') and (FProtocol <> Local) then
    IBError(ibxeServerNameMissing, [nil]);
end;

procedure TFBServiceManager.MergeBuffers(Requests: array of IServiceRequest;
  var buffer: PChar; var size: integer);
var i: integer;
    P: PChar;
begin
  size := 0;
  for i := 0 to length(Requests) - 1 do
    size += (Requests[i] as TServiceRequest).getBufferLength;

  GetMem(buffer,Size);
  if buffer = nil then
    OutOfMemoryError;

  P := buffer;
  for i := 0 to length(Requests) - 1 do
  begin
    Move((Requests[i] as TServiceRequest).getBuffer^,
              P^,
              (Requests[i] as TServiceRequest).getBufferLength);
    P += (Requests[i] as TServiceRequest).getBufferLength;
  end;
end;

constructor TFBServiceManager.Create(ServerName: string; Protocol: TProtocol;
  SPB: ISPB);
begin
  inherited Create;
  FProtocol := Protocol;
  Firebird25ClientAPI.RegisterObj(self);
  FSPB := SPB;
  FServerName := ServerName;
  Attach;
end;

destructor TFBServiceManager.Destroy;
begin
  Detach(true);
  Firebird25ClientAPI.UnRegisterObj(self);
  inherited Destroy;
end;

function TFBServiceManager.getSPB: ISPB;
begin
  Result := FSPB;
end;

procedure TFBServiceManager.Attach;
var ConnectString: String;
begin
  case FProtocol of
    TCP: ConnectString := FServerName + ':service_mgr'; {do not localize}
    SPX: ConnectString := FServerName + '@service_mgr'; {do not localize}
    NamedPipe: ConnectString := '\\' + FServerName + '\service_mgr'; {do not localize}
    Local: ConnectString := 'service_mgr'; {do not localize}
  end;
  with Firebird25ClientAPI do
  if FSPB = nil then
  begin
    if isc_service_attach(StatusVector, Length(ConnectString),
                         PChar(ConnectString), @FHandle, 0, nil) > 0 then
      IBDataBaseError;
  end
  else
  begin
    if isc_service_attach(StatusVector, Length(ConnectString),
                           PChar(ConnectString), @FHandle,
                           (FSPB as TSPB).getBufferLength,
                           (FSPB as TSPB).getBuffer) > 0 then
      IBDataBaseError;
  end;
end;

procedure TFBServiceManager.Detach(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  with Firebird25ClientAPI do
  if isc_service_detach(StatusVector, @FHandle) > 0 then
  begin
    FHandle := nil;
    if not Force then
     IBDataBaseError;
  end
  else
    FHandle := nil;
end;

function TFBServiceManager.IsAttached: boolean;
begin
  Result := FHandle <> nil;
end;

function TFBServiceManager.AllocateRequestBuffer(action: byte): IServiceRequest;
begin
  Result := TServiceRequest.Create(action);
end;

procedure TFBServiceManager.Start(Request: IServiceRequest);
begin
    with Firebird25ClientAPI do
      if isc_service_start(StatusVector, @FHandle, nil,
                           (Request as TServiceRequest).getBufferLength,
                           (Request as TServiceRequest).getBuffer) > 0 then
        IBDataBaseError;
end;

procedure TFBServiceManager.StartMultiple(Requests: array of IServiceRequest);
var Buffer: PChar;
    BufLength: integer;
begin
  if Length(Requests) = 0 then
    Exit;
  if Length(Requests) = 1 then
    Start(Requests[0]);

  MergeBuffers(Requests,Buffer,BufLength);
  try
    with Firebird25ClientAPI do
      if isc_service_start(StatusVector, @FHandle, nil,
                           BufLength,
                           Buffer) > 0 then
        IBDataBaseError;

  finally
    FreeMem(Buffer);
  end;
end;

function TFBServiceManager.Query(Request: IServiceRequest
  ): IServiceQueryResults;
var QueryResults: TServiceQueryResults;
begin
  QueryResults := TServiceQueryResults.Create;
  Result := QueryResults;
  with Firebird25ClientAPI do
    if isc_service_query(StatusVector, @FHandle, nil, 0, nil,
                       (Request as TServiceRequest).getBufferLength,
                       (Request as TServiceRequest).getBuffer,
                       QueryResults.getBufSize,
                       QueryResults.Buffer) > 0 then
      IBDataBaseError;

end;

function TFBServiceManager.QueryMultiple(Requests: array of IServiceRequest
  ): IServiceQueryResults;
var Buffer: PChar;
    BufLength: integer;
    QueryResults: TServiceQueryResults;
begin
  if Length(Requests) = 0 then
    Exit;
  if Length(Requests) = 1 then
    Result := Query(Requests[0]);

  MergeBuffers(Requests,Buffer,BufLength);
  try
    QueryResults := TServiceQueryResults.Create;
    Result := QueryResults;
    with Firebird25ClientAPI do
      if isc_service_query(StatusVector, @FHandle, nil, 0, nil,
                         BufLength,
                         Buffer,
                         QueryResults.getBufSize,
                         QueryResults.Buffer) > 0 then
        IBDataBaseError;
  finally
    FreeMem(Buffer);
  end;
end;

end.

