unit FB25Services;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBExternals, IBHeader, FB25ParamBlock;

const
  DefaultBufferSize = 32000;

type
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
    constructor CreateAsList(BufPtr: PChar; aSize: integer);
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

  TSPBItem = class(TParamBlockItem,ISPBItem)
  private
    FSPB: ISPB;
  public
    constructor Create(AOwner: TSPB; Data: PParamBlockItemData);
  end;

  { TSRBItem }

  TSRBItem = class(TParamBlockItem,ISRBItem)
  private
    FSRB: ISRB;
  public
    constructor Create(AOwner: TSRB; Data: PParamBlockItemData);
    function ISRBItem.SetAsString = SetAsString2;
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
    function AllocateRequestBuffer: ISRB;
    procedure Start(Request: ISRB);
    function Query(Request: ISRB) :IServiceQueryResults;
  end;

implementation

uses FBErrorMessages;

{ TSRBItem }

constructor TSRBItem.Create(AOwner: TSRB; Data: PParamBlockItemData);
begin
  inherited Create(AOwner,Data);
  FSRB := AOwner;
end;

{ TSPBItem }

constructor TSPBItem.Create(AOwner: TSPB; Data: PParamBlockItemData);
begin
  inherited Create(AOwner,Data);
  FSPB := AOwner;
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

  { TServiceQueryResultItem }

procedure TServiceQueryResultItem.ParseBuffer;
var P: PChar;
    i: integer;
    group: byte;
begin
  P := FBufPtr + 1;
  i := 0;
  group := byte(FBufPtr^);
  if group in [isc_info_svc_get_users,isc_info_svc_limbo_trans] then
  begin
    with Firebird25ClientAPI do
       FSize := isc_portable_integer(P,2) + 3;
    Inc(P,2);
  end;
  while (P < FBufPtr + FSize) and (P^ <> char(isc_info_flag_end)) do
  begin
    SetLength(FSubItems,i+1);
    case group of
    isc_info_svc_svr_db_info:
      case integer(P^) of
        isc_spb_num_att,
        isc_spb_num_db:
          FSubItems[i] := TServiceQueryResultSubItem.CreateAsInteger(P);

        isc_spb_dbname:
          FSubItems[i] := TServiceQueryResultSubItem.CreateAsString(P);

        else
          IBError(ibxeOutputParsingError, [integer(P^)]);
        end;

    isc_info_svc_get_license:
      case integer(P^) of
      isc_spb_lic_id,
      isc_spb_lic_key:
        FSubItems[i] := TServiceQueryResultSubItem.CreateAsInteger(P);
      else
        IBError(ibxeOutputParsingError, [integer(P^)]);
      end;

    isc_info_svc_limbo_trans:
     case integer(P^) of
     isc_spb_tra_id,
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
       IBError(ibxeOutputParsingError, [integer(P^)]);
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
        IBError(ibxeOutputParsingError, [integer(P^)]);
      end;

    end;
    P +=  (FSubItems[i] as TServiceQueryResultSubItem).getSize;
    Inc(i);
  end;
  FDataLength := 0;
  for i := 0 to Length(FSubItems) - 1 do
    FDataLength += (FSubItems[i] as TServiceQueryResultSubItem).getSize;
  if group in [isc_info_svc_get_users,isc_info_svc_limbo_trans] then
    Exit;

  if (P < FBufPtr + FSize) and (P^ = char(isc_info_flag_end)) then
    FSize := FDataLength + 2 {include start and end flag}
  else
    FSize := FDataLength + 1; {start flag only}
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

constructor TServiceQueryResultItem.CreateAsList(BufPtr: PChar; aSize: integer);
begin
  inherited Create;
  FIsInteger := false;
  FBufPtr := BufPtr;
  FSize := aSize;
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
    FDataLength := isc_portable_integer(FBufPtr+1, 2);
  FSize := FDataLength + 3;
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
    SetString(Result,FBufPtr+3,FDataLength);
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
    SetLength(FItems,i+1);
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
      isc_info_svc_running,
      isc_info_svc_stdin:
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
      FItems[i] := TServiceQueryResultItem.CreateAsList(P, FSize - (P - FBuffer));
    else
       IBError(ibxeOutputParsingError, [integer(P^)]);
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
                           (FSPB as TSPB).getDataLength,
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

function TFBServiceManager.AllocateRequestBuffer: ISRB;
begin
  Result := TSRB.Create;
end;

procedure TFBServiceManager.Start(Request: ISRB);
begin
    with Firebird25ClientAPI do
      if isc_service_start(StatusVector, @FHandle, nil,
                           (Request as TSRB).getDataLength,
                           (Request as TSRB).getBuffer) > 0 then
        IBDataBaseError;
end;

function TFBServiceManager.Query(Request: ISRB): IServiceQueryResults;
var QueryResults: TServiceQueryResults;
begin
  QueryResults := TServiceQueryResults.Create;
  Result := QueryResults;
  with Firebird25ClientAPI do
    if isc_service_query(StatusVector, @FHandle, nil, 0, nil,
                       (Request as TSRB).getDataLength,
                       (Request as TSRB).getBuffer,
                       QueryResults.getBufSize,
                       QueryResults.Buffer) > 0 then
      IBDataBaseError;

end;

end.

