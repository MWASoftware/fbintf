unit FB25Services;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBHeader, FB25ParamBlock, FB25OutputBlock;

type
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

  TServiceQueryResultSubItem = class(TOutputBlockItem,IServiceQueryResultSubItem);

  { TServiceQueryResultItem }

  TServiceQueryResultItem = class(TOutputBlockItemGroup,IServiceQueryResultItem)
  public
    function getItem(index: integer): IServiceQueryResultSubItem;
    function find(ItemType: byte): IServiceQueryResultSubItem;
  end;

  { TServiceQueryResults }

  TServiceQueryResults = class(TOutputBlock,IServiceQueryResults)
  protected
    function AddListItem(BufPtr: PChar): POutputBlockItemData; override;
    function AddSpecialItem(BufPtr: PChar): POutputBlockItemData; override;
    procedure DoParseBuffer; override;
  public
    {IServiceQueryResults}
    function getItem(index: integer): IServiceQueryResultItem;
    function find(ItemType: byte): IServiceQueryResultItem;
    property Items[index: integer]: IServiceQueryResultItem read getItem; default;
  end;

  { TFBServiceManager }

  TFBServiceManager = class(TInterfacedObject,IServiceManager)
  private
    FServerName: string;
    FSPB: ISPB;
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

function TServiceQueryResultItem.getItem(index: integer
  ): IServiceQueryResultSubItem;
var P: POutputBlockItemData;
begin
  P := inherited getItem(index);
  Result := TServiceQueryResultSubItem.Create(self.Owner,P);
end;

function TServiceQueryResultItem.find(ItemType: byte
  ): IServiceQueryResultSubItem;
var P: POutputBlockItemData;
begin
  P := inherited Find(ItemType);
  Result := TServiceQueryResultSubItem.Create(self.Owner,P);
end;

{ TServiceQueryResults }

function TServiceQueryResults.AddListItem(BufPtr: PChar): POutputBlockItemData;
var P: PChar;
    i: integer;
    group: byte;
begin
  Result := inherited AddListItem(BufPtr);
  P := BufPtr + 1;
  i := 0;
  group := byte(BufPtr^);
  if group in [isc_info_svc_get_users,isc_info_svc_limbo_trans] then
  begin
    with Firebird25ClientAPI do
       Result^.FSize := isc_portable_integer(P,2) + 3;
    Inc(P,2);
  end;
  with Result^ do
  begin
    while (P < FBufPtr + FSize) and (P^ <> char(isc_info_flag_end)) do
    begin
      SetLength(FSubItems,i+1);
      case group of
      isc_info_svc_svr_db_info:
        case integer(P^) of
          isc_spb_num_att,
          isc_spb_num_db:
            FSubItems[i] := AddIntegerItem(P);

          isc_spb_dbname:
            FSubItems[i] := AddStringItem(P);

          else
            IBError(ibxeOutputParsingError, [integer(P^)]);
          end;

      isc_info_svc_get_license:
        case integer(P^) of
        isc_spb_lic_id,
        isc_spb_lic_key:
          FSubItems[i] := AddIntegerItem(P);
        else
          IBError(ibxeOutputParsingError, [integer(P^)]);
        end;

      isc_info_svc_limbo_trans:
       case integer(P^) of
       isc_spb_tra_id,
       isc_spb_single_tra_id,
       isc_spb_multi_tra_id:
         FSubItems[i] := AddIntegerItem(P);

       isc_spb_tra_host_site,
       isc_spb_tra_remote_site,
       isc_spb_tra_db_path:
         FSubItems[i] := AddStringItem(P);

       isc_spb_tra_advise,
       isc_spb_tra_state:
         FSubItems[i] := AddByteItem(P);
       else
         IBError(ibxeOutputParsingError, [integer(P^)]);
       end;

      isc_info_svc_get_users:
        case integer(P^) of
        isc_spb_sec_userid,
        isc_spb_sec_groupid:
          FSubItems[i] := AddIntegerItem(P);

        isc_spb_sec_username,
        isc_spb_sec_password,
        isc_spb_sec_firstname,
        isc_spb_sec_middlename,
        isc_spb_sec_lastname:
          FSubItems[i] := AddStringItem(P);

        else
          IBError(ibxeOutputParsingError, [integer(P^)]);
        end;

      end;
      P +=  FSubItems[i]^.FSize;
      Inc(i);
    end;
    FDataLength := 0;
    for i := 0 to Length(FSubItems) - 1 do
      FDataLength += FSubItems[i]^.FSize;
    if group in [isc_info_svc_get_users,isc_info_svc_limbo_trans] then
      Exit;

    if (P < FBufPtr + FSize) and (P^ = char(isc_info_flag_end)) then
      FSize := FDataLength + 2 {include start and end flag}
    else
      FSize := FDataLength + 1; {start flag only}
  end;
end;

function TServiceQueryResults.AddSpecialItem(BufPtr: PChar
  ): POutputBlockItemData;
var P: PChar;
    i: integer;
begin
  Result := inherited AddSpecialItem(BufPtr);
  with Result^ do
  begin
    with Firebird25ClientAPI do
      FDataLength := isc_portable_integer(FBufPtr+1, 2);

    P := FBufPtr + 3; {skip length bytes}
    i := 0;
    while P < FBufPtr + FDataLength do
    begin
      FSubItems[i] := AddIntegerItem(P);
      P +=  FSubItems[i]^.FSize;
      Inc(i);
    end;
  end;
end;

procedure TServiceQueryResults.DoParseBuffer;
var P: PChar;
    i: integer;
begin
  P := Buffer;
  i := 0;
  while  (P < Buffer + getBufSize) and (P^ <> char(isc_info_end)) do
  begin
    SetLength(FItems,i+1);
    case integer(P^) of
    isc_info_svc_line,
    isc_info_svc_get_env,
    isc_info_svc_get_env_lock,
    isc_info_svc_get_env_msg,
    isc_info_svc_user_dbpath,
    isc_info_svc_server_version,
    isc_info_svc_implementation,
    isc_info_svc_to_eof:
      FItems[i] := AddStringItem(P);

    isc_info_svc_get_license_mask,
    isc_info_svc_capabilities,
    isc_info_svc_version,
    isc_info_svc_running,
    isc_info_svc_stdin:
      FItems[i] := AddIntegerItem(P);

    isc_info_svc_timeout,
    isc_info_data_not_ready,
    isc_info_truncated:
      FItems[i] := AddItem(P);

    isc_info_svc_svr_db_info,
    isc_info_svc_get_license,
    isc_info_svc_limbo_trans,
    isc_info_svc_get_users:
      FItems[i] := AddListItem(P);

    isc_info_svc_get_config:
      FItems[i] := AddSpecialItem(P);


    else
       IBError(ibxeOutputParsingError, [integer(P^)]);
    end;
    P += FItems[i]^.FSize;
    Inc(i);
  end;
end;

function TServiceQueryResults.getItem(index: integer): IServiceQueryResultItem;
var P: POutputBlockItemData;
begin
  P := inherited getItem(index);
  Result := TServiceQueryResultItem.Create(self,P)
end;

function TServiceQueryResults.find(ItemType: byte): IServiceQueryResultItem;
var P: POutputBlockItemData;
begin
  P := inherited find(ItemType);
  if P = nil then
    Result := nil
  else
    Result := TServiceQueryResultItem.Create(self,P);
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

