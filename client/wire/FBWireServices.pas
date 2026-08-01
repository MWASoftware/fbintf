(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  This file is part of the pure Pascal wire protocol implementation
 *  (no fbclient required) and is subject to the Initial Developer's
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
 *  The Initial Developer of the Original Code is MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBWireServices;

{ The IServiceManager implementation for the pure Pascal wire protocol
  client.

  A service session is its own connection: TFBWireConnection.ConnectTo
  performs the SRP authentication and, when negotiated, starts wire
  encryption exactly as it does for a database attach, and then
  op_service_attach names service_mgr on that connection. The password is
  consumed by the SRP exchange and never placed in the SPB that travels to
  the server; when the server asked for the proof to be delivered with the
  attach (the op_accept_data flow) it is added to the SPB as
  isc_spb_specific_auth_data, mirroring what TFBWireAttachment does with
  the DPB.

  The SPB, SRB and SQPB parameter blocks and the response parser all come
  from the generic fbintf machinery (FBServices, FBParamBlock,
  FBOutputBlock): the buffers those classes build are exactly what the
  wire carries. }

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBServices, FBOutputBlock, FBWireClientAPI,
  FBWireProtocol;

type

  { TFBWireServiceManager }

  TFBWireServiceManager = class(TFBServiceManager,IServiceManager)
  private
    FWireAPI: TFBWireClientAPI;
    FConnection: TFBWireConnection;
    FHandle: integer;
    FIsAttached: boolean;
    procedure CheckActive;
    {rebuilds the SPB without the password, adding the authentication
     proof when the server asked for it in the attach}
    function PrepareSPB: TBytes;
  protected
    procedure InternalAttach(ConnectString: AnsiString); override;
  public
    constructor Create(api: TFBWireClientAPI; ServerName: AnsiString;
                Protocol: TProtocol; SPB: ISPB; Port: AnsiString = '');
    destructor Destroy; override;

    property Connection: TFBWireConnection read FConnection;
    property Handle: integer read FHandle;

  public
    {IServiceManager}
    procedure Detach(Force: boolean=false); override;
    function IsAttached: boolean;
    function Start(Request: ISRB; RaiseExceptionOnError: boolean=true): boolean;
    function Query(SQPB: ISQPB; Request: ISRB;
                RaiseExceptionOnError: boolean=true): IServiceQueryResults; override;
  end;

implementation

uses FBMessages, IBUtils;

{ TFBWireServiceManager }

procedure TFBWireServiceManager.CheckActive;
begin
  if not FIsAttached then
    IBError(ibxeServiceActive,[nil]);
end;

function TFBWireServiceManager.PrepareSPB: TBytes;
var NewSPB: ISPB;
    Item, NewItem: ISPBItem;
    i: integer;
begin
  NewSPB := TSPB.Create(FWireAPI);
  if FSPB <> nil then
    for i := 0 to FSPB.Count - 1 do
    begin
      Item := FSPB.Items[i];
      {SRP has already proved knowledge of the password - it must not
       travel to the server in any form}
      if Item.getParamType in [isc_spb_password,isc_spb_password_enc] then
        continue;
      NewItem := NewSPB.Add(Item.getParamType);
      if Item.getParamType in [isc_spb_options,isc_spb_connect_timeout,
                               isc_spb_dummy_packet_interval] then
        NewItem.SetAsInteger(Item.AsInteger)
      else
        NewItem.SetAsString(Item.AsString);
    end;
  if FConnection.AuthData <> '' then
  begin
    NewSPB.Add(isc_spb_specific_auth_data).AsString := FConnection.AuthData;
    NewSPB.Add(isc_spb_auth_plugin_name).AsString := FConnection.AuthPluginName;
  end;
  Result := ParamBlockToBytes(NewSPB);
end;

procedure TFBWireServiceManager.InternalAttach(ConnectString: AnsiString);
var aHost, aServiceName, aPortNo: AnsiString;
    aPort: integer;
    aProtocol: TProtocolAll;
    aUser, aPassword: AnsiString;
    Item: ISPBItem;
    spbBytes: TBytes;
begin
  aHost := '';
  aPort := 3050;
  aServiceName := ConnectString;
  if ParseConnectString(ConnectString,aHost,aServiceName,aProtocol,aPortNo) then
  begin
    if aPortNo <> '' then
      aPort := StrToIntDef(aPortNo,3050);
    case aProtocol of
    TCP, inet, inet4, inet6:
      {a remote connection - this is what the wire protocol provides};
    Local:
      {a local connection cannot be made without the client library, but
       most servers also listen on the loopback interface}
      if aHost = '' then
        aHost := 'localhost';
    else
      IBError(ibxeNotSupported,[nil]);
    end;
  end;
  if aHost = '' then
    aHost := 'localhost';

  aUser := '';
  aPassword := '';
  if FSPB <> nil then
  begin
    Item := FSPB.Find(isc_spb_user_name);
    if Item <> nil then
      aUser := Item.AsString;
    Item := FSPB.Find(isc_spb_password);
    if Item <> nil then
      aPassword := Item.AsString;
  end;

  try
    FConnection.ConnectTo(aHost,aPort,aServiceName,aUser,aPassword);
    spbBytes := PrepareSPB;
    FHandle := FConnection.ServiceAttach(aServiceName,spbBytes);
    FIsAttached := true;
  except
    on E: Exception do
    begin
      FConnection.Disconnect;
      FIsAttached := false;
      WireIBError(FWireAPI,E);
    end;
  end;
end;

constructor TFBWireServiceManager.Create(api: TFBWireClientAPI;
  ServerName: AnsiString; Protocol: TProtocol; SPB: ISPB; Port: AnsiString);
begin
  FWireAPI := api;
  FConnection := TFBWireConnection.Create;
  {the inherited constructor attaches}
  inherited Create(api,ServerName,Protocol,SPB,Port);
end;

destructor TFBWireServiceManager.Destroy;
begin
  inherited Destroy;
  if FConnection <> nil then
    FConnection.Free;
end;

procedure TFBWireServiceManager.Detach(Force: boolean);
begin
  if not FIsAttached then Exit;
  try
    FConnection.ServiceDetach(FHandle);
  except
    on E: Exception do
      if not Force then
      begin
        FIsAttached := false;
        FConnection.Disconnect;
        WireIBError(FWireAPI,E);
      end;
  end;
  FIsAttached := false;
  FHandle := 0;
  FConnection.Disconnect;
end;

function TFBWireServiceManager.IsAttached: boolean;
begin
  Result := FIsAttached;
end;

function TFBWireServiceManager.Start(Request: ISRB;
  RaiseExceptionOnError: boolean): boolean;
begin
  CheckActive;
  Result := true;
  try
    FConnection.ServiceStart(FHandle,ParamBlockToBytes(Request));
  except
    on E: Exception do
    begin
      Result := false;
      if RaiseExceptionOnError then
        WireIBError(FWireAPI,E);
    end;
  end;
end;

function TFBWireServiceManager.Query(SQPB: ISQPB; Request: ISRB;
  RaiseExceptionOnError: boolean): IServiceQueryResults;
var QueryResults: TServiceQueryResults;
    response: TBytes;
    len: integer;
begin
  CheckActive;
  QueryResults := TServiceQueryResults.Create(FWireAPI);
  Result := QueryResults;
  try
    response := FConnection.ServiceQuery(FHandle,ParamBlockToBytes(SQPB),
                    ParamBlockToBytes(Request),QueryResults.getBufSize);
    len := Length(response);
    if len > QueryResults.getBufSize then
      len := QueryResults.getBufSize;
    if len > 0 then
      Move(response[0],QueryResults.Buffer^,len);
  except
    on E: Exception do
    begin
      Result := nil;
      if RaiseExceptionOnError then
        WireIBError(FWireAPI,E);
    end;
  end;
end;

end.
