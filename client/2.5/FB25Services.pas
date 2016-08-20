unit FB25Services;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBHeader, FBParamBlock, FBOutputBlock,
    FBActivityMonitor;

type
  { TFBServiceManager }

  TFBServiceManager = class(TInterfaceParent,IServiceManager)
  private
    FFirebirdAPI: IFirebirdAPI;
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

uses FBMessages;

{ TFBServiceManager }

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
  FFirebirdAPI := Firebird25ClientAPI; {Keep reference to interface}
  FProtocol := Protocol;
  FSPB := SPB;
  FServerName := ServerName;
  Attach;
end;

destructor TFBServiceManager.Destroy;
begin
  Detach(true);
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

