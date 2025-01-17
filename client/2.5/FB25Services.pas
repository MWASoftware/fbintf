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
{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011 - 2015                                                }
{                                                                        }
{************************************************************************}
unit FB25Services;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBHeader, FBParamBlock, FBOutputBlock,
    FBServices;

type
  { TFBServiceManager }

  { TFB25ServiceManager }

  TFB25ServiceManager = class(TFBServiceManager,IServiceManager)
  private
    FHandle: TISC_SVC_HANDLE;
    FFirebird25ClientAPI: TFB25ClientAPI;
    procedure CheckActive;
    procedure CheckInactive;
  protected
    procedure InternalAttach(ConnectString: AnsiString); override;
  public
    constructor Create(api: TFB25ClientAPI; ServerName: AnsiString; Protocol: TProtocol; SPB: ISPB; Port: AnsiString = '');
    property Handle: TISC_SVC_HANDLE read FHandle;

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

procedure TFB25ServiceManager.CheckActive;
begin
  if FHandle = nil then
    IBError(ibxeServiceActive, [nil]);
end;

procedure TFB25ServiceManager.CheckInactive;
begin
  if FHandle <> nil then
    IBError(ibxeServiceInActive, [nil]);
end;

procedure TFB25ServiceManager.InternalAttach(ConnectString: AnsiString);
begin
  with FFirebird25ClientAPI do
  if FSPB = nil then
  begin
    if isc_service_attach(StatusVector, Length(ConnectString),
                         PAnsiChar(ConnectString), @FHandle, 0, nil) > 0 then
      IBDataBaseError;
  end
  else
  begin
    if isc_service_attach(StatusVector, Length(ConnectString),
                           PAnsiChar(ConnectString), @FHandle,
                           (FSPB as TSPB).getDataLength,
                           (FSPB as TSPB).getBuffer) > 0 then
      IBDataBaseError;
  end;
end;

constructor TFB25ServiceManager.Create(api: TFB25ClientAPI;
  ServerName: AnsiString; Protocol: TProtocol; SPB: ISPB; Port: AnsiString);
begin
  FFirebird25ClientAPI := api;
  inherited Create(api,ServerName, Protocol, SPB, Port);
end;

procedure TFB25ServiceManager.Detach(Force: boolean);
begin
  if FHandle = nil then
    Exit;
  with FFirebird25ClientAPI do
  if isc_service_detach(StatusVector, @FHandle) > 0 then
  begin
    FHandle := nil;
    if not Force then
     IBDataBaseError;
  end
  else
    FHandle := nil;
end;

function TFB25ServiceManager.IsAttached: boolean;
begin
  Result := FHandle <> nil;
end;

function TFB25ServiceManager.Start(Request: ISRB; RaiseExceptionOnError: boolean
  ): boolean;
begin
  Result := true;
  CheckActive;
  with FFirebird25ClientAPI do
  begin
    Result := isc_service_start(StatusVector, @FHandle, nil,
                           (Request as TSRB).getDataLength,
                           (Request as TSRB).getBuffer) = 0;
    if not Result and RaiseExceptionOnError then
      IBDataBaseError;
  end;
end;

function TFB25ServiceManager.Query(SQPB: ISQPB; Request: ISRB;
  RaiseExceptionOnError: boolean): IServiceQueryResults;
var QueryResults: TServiceQueryResults;
begin
  CheckActive;
  QueryResults := TServiceQueryResults.Create(FFirebird25ClientAPI);
  Result := QueryResults;
  with FFirebird25ClientAPI do
    if SQPB = nil then
    begin
      if isc_service_query(StatusVector, @FHandle, nil,0,nil,
                         (Request as TSRB).getDataLength,
                         (Request as TSRB).getBuffer,
                         QueryResults.getBufSize,
                         QueryResults.Buffer) > 0 then
      begin
        if RaiseExceptionOnError then
          IBDataBaseError
        else
          Result := nil;
      end;
    end
    else
    if isc_service_query(StatusVector, @FHandle, nil,
                       (SQPB as TSQPB).getDataLength,
                       (SQPB as TSQPB).getBuffer,
                       (Request as TSRB).getDataLength,
                       (Request as TSRB).getBuffer,
                       QueryResults.getBufSize,
                       QueryResults.Buffer) > 0 then
    begin
      if RaiseExceptionOnError then
        IBDataBaseError
      else
        Result := nil;
    end;
end;

end.

