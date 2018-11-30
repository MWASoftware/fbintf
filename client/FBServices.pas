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
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB,  FBParamBlock, FBActivityMonitor, FBClientAPI;

type

  { TFBServiceManager }

  TFBServiceManager = class(TFBInterfacedObject)
  private
    FFirebirdAPI: IFirebirdAPI;
    FProtocol: TProtocol;
    FServerName: AnsiString;
    FPort: AnsiString;
    procedure CheckServerName;
  protected
    FSPB: ISPB;
    procedure InternalAttach(ConnectString: AnsiString); virtual; abstract;
  public
    constructor Create(api: TFBClientAPI; ServerName: AnsiString; Protocol: TProtocol; SPB: ISPB; Port: AnsiString = '');
    destructor Destroy; override;
  public
    {IServiceManager}
    function getFirebirdAPI: IFirebirdAPI;
    function getSPB: ISPB;
    function getServerName: AnsiString;
    function getProtocol: TProtocol;
    function getPortNo: AnsiString;
    procedure Attach;
    procedure Detach(Force: boolean=false); virtual; abstract;
    function AllocateSRB: ISRB;
    function AllocateSQPB: ISQPB;
    function Query(SQPB: ISQPB; Request: ISRB; RaiseExceptionOnError: boolean=true): IServiceQueryResults; overload; virtual; abstract;
    function Query(Request: ISRB; RaiseExceptionOnError: boolean=true): IServiceQueryResults;  overload;
  end;

implementation

uses FBMessages, IBUtils;

{ TFBServiceManager }

procedure TFBServiceManager.CheckServerName;
begin
  if (FServerName = '') and (FProtocol <> Local) then
    IBError(ibxeServerNameMissing, [nil]);
end;

constructor TFBServiceManager.Create(api: TFBClientAPI; ServerName: AnsiString;
  Protocol: TProtocol; SPB: ISPB; Port: AnsiString);
begin
  inherited Create;
  FFirebirdAPI := api.getAPI; {Keep reference to interface}
  FProtocol := Protocol;
  FSPB := SPB;
  FServerName := ServerName;
  FPort := Port;
  Attach;
end;

destructor TFBServiceManager.Destroy;
begin
  Detach(true);
  inherited Destroy;
end;

function TFBServiceManager.getFirebirdAPI: IFirebirdAPI;
begin
  Result := FFirebirdAPI;
end;

function TFBServiceManager.getSPB: ISPB;
begin
  Result := FSPB;
end;

function TFBServiceManager.getServerName: AnsiString;
begin
  Result := FServerName;
end;

function TFBServiceManager.getProtocol: TProtocol;
begin
  Result := FProtocol;
end;

function TFBServiceManager.getPortNo: AnsiString;
begin
  Result := FPort;
end;

procedure TFBServiceManager.Attach;
var ConnectString: AnsiString;
begin
  ConnectString := MakeConnectString(FServerName,'service_mgr',FProtocol,FPort);
  InternalAttach(ConnectString);
end;

function TFBServiceManager.AllocateSRB: ISRB;
begin
  Result := TSRB.Create(FFirebirdAPI as TFBClientAPI);
end;

function TFBServiceManager.AllocateSQPB: ISQPB;
begin
   Result := TSQPB.Create(FFirebirdAPI as TFBClientAPI);
end;

function TFBServiceManager.Query(Request: ISRB; RaiseExceptionOnError: boolean
  ): IServiceQueryResults;
begin
   Result := Query(nil,Request,RaiseExceptionOnError);
end;

end.

