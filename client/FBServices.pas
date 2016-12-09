(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API. Although predominantly
 *  a new development they include source code taken from IBX and may be
 *  considered a derived product. This software thus also includes the copyright
 *  notice and license conditions from IBX.
 *
 *  Except for those parts dervied from IBX, contents of this file are subject
 *  to the Initial Developer's Public License Version 1.0 (the "License"); you
 *  may not use this file except in compliance with the License. You may obtain a
 *  copy of the License here:
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
unit FBServices;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB,  FBParamBlock, FBActivityMonitor;

type

  { TFBServiceManager }

  TFBServiceManager = class(TFBInterfacedObject)
  private
    FFirebirdAPI: IFirebirdAPI;
    FProtocol: TProtocol;
    FServerName: string;
    procedure CheckServerName;
  protected
    FSPB: ISPB;
    procedure InternalAttach(ConnectString: string); virtual; abstract;
  public
    constructor Create(ServerName: string; Protocol: TProtocol; SPB: ISPB);
    destructor Destroy; override;
  public
    {IServiceManager}
    function getSPB: ISPB;
    function getServerName: string;
    procedure Attach;
    procedure Detach(Force: boolean=false); virtual; abstract;
    function AllocateSRB: ISRB;
    function AllocateSQPB: ISQPB;
    function Query(SQPB: ISQPB; Request: ISRB): IServiceQueryResults; virtual; abstract; overload;
    function Query(Request: ISRB): IServiceQueryResults;  overload;
  end;

implementation

uses FBMessages, FBClientAPI;

{ TFBServiceManager }

procedure TFBServiceManager.CheckServerName;
begin
  if (FServerName = '') and (FProtocol <> Local) then
    IBError(ibxeServerNameMissing, [nil]);
end;

constructor TFBServiceManager.Create(ServerName: string; Protocol: TProtocol;
  SPB: ISPB);
begin
  inherited Create;
  FFirebirdAPI := FirebirdAPI; {Keep reference to interface}
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

function TFBServiceManager.getServerName: string;
begin
  Result := FServerName;
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
  InternalAttach(ConnectString);
end;

function TFBServiceManager.AllocateSRB: ISRB;
begin
  Result := TSRB.Create;
end;

function TFBServiceManager.AllocateSQPB: ISQPB;
begin
   Result := TSQPB.Create;
end;

function TFBServiceManager.Query(Request: ISRB): IServiceQueryResults;
begin
   Result := Query(nil,Request);
end;

end.

