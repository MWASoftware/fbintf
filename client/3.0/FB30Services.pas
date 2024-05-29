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
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, FirebirdOOAPI, IB, FB30ClientAPI, FBParamBlock, FBOutputBlock,
    FBServices;

type
  { TFBServiceManager }

  { TFB30ServiceManager }

  TFB30ServiceManager = class(TFBServiceManager,IServiceManager)
  private
    FServiceIntf: FirebirdOOAPI.IService;
    FFirebird30ClientAPI: TFB30ClientAPI;
    procedure CheckActive;
    procedure CheckInactive;
  protected
    procedure InternalAttach(ConnectString: AnsiString); override;
  public
    constructor Create(api: TFB30ClientAPI; ServerName: AnsiString; Protocol: TProtocol; SPB: ISPB; Port: AnsiString = '');
    property ServiceIntf: FirebirdOOAPI.IService read FServiceIntf;

  public
    {IServiceManager}
    procedure Detach(Force: boolean=false); override;
    function IsAttached: boolean;
    function Start(Request: ISRB; RaiseExceptionOnError: boolean=true): boolean;
    function Query(SQPB: ISQPB; Request: ISRB; RaiseExceptionOnError: boolean=true): IServiceQueryResults; override;
  end;

implementation

uses FBMessages;

{ TFBServiceManager }

procedure TFB30ServiceManager.CheckActive;
begin
  if FServiceIntf = nil then
    IBError(ibxeServiceActive, [nil]);
end;

procedure TFB30ServiceManager.CheckInactive;
begin
  if FServiceIntf <> nil then
    IBError(ibxeServiceInActive, [nil]);
end;

procedure TFB30ServiceManager.InternalAttach(ConnectString: AnsiString);
begin
  with FFirebird30ClientAPI do
  if FSPB = nil then
  begin
    FServiceIntf := ProviderIntf.attachServiceManager(StatusIntf, PAnsiChar(ConnectString), 0, nil);
    Check4DataBaseError;
  end
  else
  begin
    FServiceIntf := ProviderIntf.attachServiceManager(StatusIntf,
                                               PAnsiChar(ConnectString),
                                               (FSPB as TSPB).getDataLength,
                                               BytePtr((FSPB as TSPB).getBuffer));
    Check4DataBaseError;
  end;
end;

constructor TFB30ServiceManager.Create(api: TFB30ClientAPI;
  ServerName: AnsiString; Protocol: TProtocol; SPB: ISPB; Port: AnsiString);
begin
  FFirebird30ClientAPI := api;
  inherited Create(api,ServerName, Protocol, SPB, Port);
end;

procedure TFB30ServiceManager.Detach(Force: boolean);
begin
  if FServiceIntf = nil then
    Exit;
  with FFirebird30ClientAPI do
  begin
    FServiceIntf.detach(StatusIntf);
    if not Force and InErrorState then
      IBDataBaseError;
    FServiceIntf := nil;
  end;
end;

function TFB30ServiceManager.IsAttached: boolean;
begin
  Result := FServiceIntf <> nil;
end;

function TFB30ServiceManager.Start(Request: ISRB; RaiseExceptionOnError: boolean
  ): boolean;
begin
  Result := true;
  CheckActive;
  with FFirebird30ClientAPI do
    begin
      FServiceIntf.Start(StatusIntf,
                           (Request as TSRB).getDataLength,
                           BytePtr((Request as TSRB).getBuffer));
      if RaiseExceptionOnError then
        Check4DataBaseError
      else
        Result := not InErrorState;
    end;
end;

function TFB30ServiceManager.Query(SQPB: ISQPB; Request: ISRB;
  RaiseExceptionOnError: boolean): IServiceQueryResults;
var QueryResults: TServiceQueryResults;
begin
  CheckActive;
  QueryResults := TServiceQueryResults.Create(FFirebird30ClientAPI);
  Result := QueryResults;
  with FFirebird30ClientAPI do
  begin
    if SQPB <> nil then
    begin
      FServiceIntf.query(StatusIntf,
                         (SQPB as TSQPB).getDataLength,
                         BytePtr((SQPB as TSQPB).getBuffer),
                         (Request as TSRB).getDataLength,
                         BytePtr((Request as TSRB).getBuffer),
                         QueryResults.getBufSize,
                         BytePtr(QueryResults.Buffer));
      if RaiseExceptionOnError then
        Check4DataBaseError
      else
      if InErrorState then
        Result := nil;
    end
    else
    begin
      FServiceIntf.query(StatusIntf, 0, nil,
                       (Request as TSRB).getDataLength,
                       BytePtr((Request as TSRB).getBuffer),
                       QueryResults.getBufSize,
                       BytePtr(QueryResults.Buffer));
      if RaiseExceptionOnError then
        Check4DataBaseError
      else
      if InErrorState then
        Result := nil;
    end;
  end;
end;

end.

