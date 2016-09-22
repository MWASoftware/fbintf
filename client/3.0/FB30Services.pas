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
unit FB30Services;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Firebird, IB, FB30ClientAPI, FBParamBlock, FBOutputBlock,
    FBActivityMonitor;

type
  { TFBServiceManager }

  TFBServiceManager = class(TInterfaceParent,IServiceManager)
  private
    FFirebirdAPI: IFirebirdAPI;
    FServerName: string;
    FSPB: ISPB;
    FProtocol: TProtocol;
    FServiceIntf: Firebird.IService;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckServerName;
  public
    constructor Create(ServerName: string; Protocol: TProtocol; SPB: ISPB);
    destructor Destroy; override;
    property ServiceIntf: Firebird.IService read FServiceIntf;

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
  if FServiceIntf = nil then
    IBError(ibxeServiceActive, [nil]);
end;

procedure TFBServiceManager.CheckInactive;
begin
  if FServiceIntf <> nil then
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
  FFirebirdAPI := Firebird30ClientAPI; {Keep reference to interface}
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
  with Firebird30ClientAPI do
  if FSPB = nil then
  begin
    FServiceIntf := ProviderIntf.attachServiceManager(StatusIntf, PChar(ConnectString), 0, nil);
    Check4DataBaseError;
  end
  else
  begin
    FServiceIntf := ProviderIntf.attachServiceManager(StatusIntf,
                                               PChar(ConnectString),
                                               (FSPB as TSPB).getDataLength,
                                               BytePtr((FSPB as TSPB).getBuffer));
    Check4DataBaseError;
  end;
end;

procedure TFBServiceManager.Detach(Force: boolean);
begin
  if FServiceIntf = nil then
    Exit;
  with Firebird30ClientAPI do
  begin
    FServiceIntf.detach(StatusIntf);
    if not Force and InErrorState then
      IBDataBaseError;
    FServiceIntf := nil;
  end;
end;

function TFBServiceManager.IsAttached: boolean;
begin
  Result := FServiceIntf <> nil;
end;

function TFBServiceManager.AllocateRequestBuffer: ISRB;
begin
  Result := TSRB.Create;
end;

procedure TFBServiceManager.Start(Request: ISRB);
begin
  CheckActive;
  with Firebird30ClientAPI do
    begin
      FServiceIntf.Start(StatusIntf,
                           (Request as TSRB).getDataLength,
                           BytePtr((Request as TSRB).getBuffer));
      Check4DataBaseError;
    end;
end;

function TFBServiceManager.Query(Request: ISRB): IServiceQueryResults;
var QueryResults: TServiceQueryResults;
begin
  CheckActive;
  QueryResults := TServiceQueryResults.Create;
  Result := QueryResults;
  with Firebird30ClientAPI do
  begin
    FServiceIntf.query(StatusIntf, 0, nil,
                       (Request as TSRB).getDataLength,
                       BytePtr((Request as TSRB).getBuffer),
                       QueryResults.getBufSize,
                       BytePtr(QueryResults.Buffer));
      Check4DataBaseError;
  end;
end;

end.

