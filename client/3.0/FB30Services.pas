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

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, Firebird, IB, FB30ClientAPI, FBParamBlock, FBOutputBlock,
    FBServices;

type
  { TFBServiceManager }

  TFB30ServiceManager = class(TFBServiceManager,IServiceManager)
  private
    FServiceIntf: Firebird.IService;
    procedure CheckActive;
    procedure CheckInactive;
  protected
    procedure InternalAttach(ConnectString: string); override;
  public
    property ServiceIntf: Firebird.IService read FServiceIntf;

  public
    {IServiceManager}
    procedure Detach(Force: boolean=false); override;
    function IsAttached: boolean;
    procedure Start(Request: ISRB);
    function Query(SQPB: ISQPB; Request: ISRB): IServiceQueryResults; override;
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

procedure TFB30ServiceManager.InternalAttach(ConnectString: String);
begin
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

procedure TFB30ServiceManager.Detach(Force: boolean);
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

function TFB30ServiceManager.IsAttached: boolean;
begin
  Result := FServiceIntf <> nil;
end;

procedure TFB30ServiceManager.Start(Request: ISRB);
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

function TFB30ServiceManager.Query(SQPB: ISQPB; Request: ISRB
  ): IServiceQueryResults;
var QueryResults: TServiceQueryResults;
begin
  CheckActive;
  QueryResults := TServiceQueryResults.Create;
  Result := QueryResults;
  with Firebird30ClientAPI do
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
        Check4DataBaseError;
    end
    else
     FServiceIntf.query(StatusIntf, 0, nil,
                       (Request as TSRB).getDataLength,
                       BytePtr((Request as TSRB).getBuffer),
                       QueryResults.getBufSize,
                       BytePtr(QueryResults.Buffer));
      Check4DataBaseError;
  end;
end;

end.

