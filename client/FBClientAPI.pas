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
unit FBClientAPI;
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
  Classes,
    {$IFDEF WINDOWS}Windows, {$ENDIF}
    {$IFDEF FPC} Dynlibs, {$ENDIF}
   IB, IBHeader, FBActivityMonitor, FBMessages, IBExternals;

{For Linux see result of GetFirebirdLibList method}
{$IFDEF DARWIN}
const
FIREBIRD_SO2 = 'libfbclient.dylib';
{$ENDIF}
{$IFDEF WINDOWS}
const
IBASE_DLL = 'gds32.dll';
FIREBIRD_CLIENT = 'fbclient.dll'; {do not localize}
FIREBIRD_EMBEDDED = 'fbembed.dll';
{$ENDIF}

type
  TStatusVector              = array[0..19] of NativeInt;
  PStatusVector              = ^TStatusVector;

  TFBClientAPI = class;

  { TFBStatus }

  TFBStatus = class(TFBInterfacedObject)
  private
    FIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
  protected
    FOwner: TFBClientAPI;
  public
    constructor Create(aOwner: TFBClientAPI);
    function StatusVector: PStatusVector; virtual; abstract;

    {IStatus}
    function GetIBErrorCode: Long;
    function Getsqlcode: Long;
    function GetMessage: AnsiString;
    function CheckStatusVector(ErrorCodes: array of TFBStatusCode): Boolean;
    function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
  end;

  { TFBLibrary }

  TFBLibrary = class(TFBInterfacedObject,IFirebirdLibrary)
  private
    class var FEnvSetupDone: boolean;
    class var FLibraryList: array of IFirebirdLibrary;
    FFirebirdAPI: IFirebirdAPI;
    FRequestedLibName: string;
    function LoadIBLibrary: boolean;
  protected
    FFBLibraryName: string;
    FIBLibrary: TLibHandle;
    procedure FreeFBLibrary;
    function GetOverrideLibName: string;
    class procedure SetupEnvironment;
  protected
    function GetFirebird3API: IFirebirdAPI; virtual; abstract;
    function GetLegacyFirebirdAPI: IFirebirdAPI; virtual; abstract;
  public
    constructor Create(aLibPathName: string='');
    destructor Destroy; override;
    class function GetFBLibrary(aLibPathName: string): IFirebirdLibrary;
    class procedure FreeLibraries;

    {IFirebirdLibrary}
    function GetHandle: TLibHandle;
    function GetLibraryName: string;
    function GetLibraryFilePath: string;
    function GetFirebirdAPI: IFirebirdAPI;
    property IBLibrary: TLibHandle read FIBLibrary;
  end;

  { TFBClientAPI }

  TFBClientAPI = class(TFBInterfacedObject)
  private
    class var FIBCS: TRTLCriticalSection;
  protected
    FFBLibrary: TFBLibrary;
    function GetProcAddr(ProcName: PAnsiChar): Pointer;
    function SupportsTimeZone: boolean; virtual;
  public
    {Taken from legacy API}
    isc_sqlcode: Tisc_sqlcode;
    isc_sql_interprete: Tisc_sql_interprete;
    isc_interprete: Tisc_interprete;
    isc_event_counts: Tisc_event_counts;
    isc_event_block: Tisc_event_block;
    isc_free: Tisc_free;

    constructor Create(aFBLibrary: TFBLibrary);
    procedure IBAlloc(var P; OldSize, NewSize: Integer);
    procedure IBDataBaseError;
    function LoadInterface: boolean; virtual;
    function GetAPI: IFirebirdAPI; virtual; abstract;
    {$IFDEF UNIX}
    function GetFirebirdLibList: string; virtual; abstract;
    {$ENDIF}

    {Encode/Decode}
    procedure EncodeInteger(aValue: integer; len: integer; buffer: PByte);
    function DecodeInteger(bufptr: PByte; len: short): integer; virtual; abstract;
    procedure SQLEncodeDate(aDate: TDateTime; bufptr: PByte); virtual; abstract;
    function SQLDecodeDate(byfptr: PByte): TDateTime; virtual; abstract;
    procedure SQLEncodeTime(aTime: TDateTime; bufptr: PByte); virtual; abstract;
    function SQLDecodeTime(bufptr: PByte): TDateTime;  virtual; abstract;
    procedure SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PByte); virtual; abstract;
    function SQLDecodeDateTime(bufptr: PByte): TDateTime; virtual; abstract;
    {Firebird 4 Extensions}
    procedure SQLEncodeTimeTZ(aTime: TDateTime; aTimeZone: AnsiString; bufptr: PByte); virtual;
    procedure SQLDecodeTimeTZ(var aTime: TDateTime; var aTimeZone: AnsiString; bufptr: PByte); virtual;
    procedure SQLEncodeTimeStampTZ(aTimeStamp: TDateTime; aTimeZone: AnsiString;
      bufptr: PByte); virtual;
    procedure SQLDecodeTimeStampTZ(var aTimeStamp: TDateTime;
      var aTimeZone: AnsiString; bufptr: PByte); virtual;

    {utility}
    function FBStrToDate(S: AnsiString): TFBTZDateTime;
    function GetTimeZoneName(timeZoneID: word): AnsiString; virtual;

    {IFirebirdAPI}
    function GetStatus: IStatus; virtual; abstract;
    function IsLibraryLoaded: boolean;
    function IsEmbeddedServer: boolean; virtual; abstract;
    function GetFBLibrary: IFirebirdLibrary;
end;

implementation

uses IBUtils, Registry,
  {$IFDEF Unix} initc, dl, {$ENDIF}
{$IFDEF FPC}
{$IFDEF WINDOWS }
WinDirs,
{$ENDIF}
{$ELSE}
 ShlObj,
{$ENDIF}
SysUtils;

const
  IBLocalBufferLength = 512;
  IBBigLocalBufferLength = IBLocalBufferLength * 2;
  IBHugeLocalBufferLength = IBBigLocalBufferLength * 20;

{$IFDEF UNIX}
{$I 'include/uloadlibrary.inc'}
{$ELSE}
{$I 'include/wloadlibrary.inc'}
{$ENDIF}


{ TFBLibrary }

function TFBLibrary.GetOverrideLibName: string;
begin
  Result := FFBLibraryName;
  if (Result = '') and AllowUseOfFBLIB then
    Result := GetEnvironmentVariable('FBLIB');
  if Result = '' then
  begin
    if assigned(OnGetLibraryName) then
      OnGetLibraryName(Result)
  end;
end;

procedure TFBLibrary.FreeFBLibrary;
begin
  if FIBLibrary <> NilHandle then
    FreeLibrary(FIBLibrary);
  FIBLibrary := NilHandle;
end;

function TFBLibrary.GetLibraryName: string;
begin
  Result := ExtractFileName(FFBLibraryName);
end;

function TFBLibrary.GetFirebirdAPI: IFirebirdAPI;
begin
  Result := FFirebirdAPI;
end;

constructor TFBLibrary.Create(aLibPathName: string);
begin
  inherited Create;
  SetupEnvironment;
  FFBLibraryName := aLibPathName;
  FIBLibrary := NilHandle;
  FFirebirdAPI := GetFirebird3API;
  FRequestedLibName := aLibPathName;
  if aLibPathName <> '' then
  begin
    SetLength(FLibraryList,Length(FLibraryList)+1);
    FLibraryList[Length(FLibraryList)-1] := self;
  end;
  if FFirebirdAPI <> nil then
  begin
    {First try Firebird 3}
    if not LoadIBLibrary or not (FFirebirdAPI as TFBClientAPI).LoadInterface then
      FFirebirdAPI := nil;
  end;

  if FFirebirdAPI = nil then
  begin
    {now try Firebird 2.5. Under Unix we need to reload the library in case we
     are to use the embedded library}
    FFirebirdAPI := GetLegacyFirebirdAPI;
    if FFirebirdAPI <> nil then
    begin
      {$IFDEF UNIX}
      FreeFBLibrary;
      {$ENDIF}
      if not LoadIBLibrary or not (FFirebirdAPI as TFBClientAPI).LoadInterface then
        FFirebirdAPI := nil;
    end;
  end;
  {Note: FFirebirdAPI will be set to nil if the Firebird API fails to load}
end;

destructor TFBLibrary.Destroy;
begin
  FFirebirdAPI := nil;
  FreeFBLibrary;
  inherited Destroy;
end;

class function TFBLibrary.GetFBLibrary(aLibPathName: string): IFirebirdLibrary;
var i: integer;
begin
  Result := nil;
  if aLibPathName <> '' then
  begin
    for i := 0 to Length(FLibraryList) - 1 do
      if (FLibraryList[i] as TFBLibrary).FRequestedLibName = aLibPathName then
      begin
        Result := FLibraryList[i];
        Exit;
      end;
    Result := Create(aLibPathName);
  end;

end;

class procedure TFBLibrary.FreeLibraries;
var i: integer;
begin
  for i := 0 to Length(FLibraryList) - 1 do
    FLibraryList[i] := nil;
  SetLength(FLibraryList,0);
end;

function TFBLibrary.GetHandle: TLibHandle;
begin
  Result := FIBLibrary;
end;

{ TFBClientAPI }

constructor TFBClientAPI.Create(aFBLibrary: TFBLibrary);
begin
  inherited Create;
  FFBLibrary := aFBLibrary;
end;

procedure TFBClientAPI.IBAlloc(var P; OldSize, NewSize: Integer);
var
  i: Integer;
begin
  ReallocMem(Pointer(P), NewSize);
  for i := OldSize to NewSize - 1 do PAnsiChar(P)[i] := #0;
end;

procedure TFBClientAPI.IBDataBaseError;
begin
  raise EIBInterBaseError.Create(GetStatus);
end;

{Under Unixes, if using an embedded server then set up local TMP and LOCK Directories}

procedure TFBClientAPI.EncodeInteger(aValue: integer; len: integer; buffer: PByte);
begin
  while len > 0 do
  begin
    buffer^ := aValue and $FF;
    Inc(buffer);
    Dec(len);
    aValue := aValue shr 8;
  end;
end;

procedure TFBClientAPI.SQLEncodeTimeTZ(aTime: TDateTime; aTimeZone: AnsiString;
  bufptr: PByte);
begin
  IBError(ibxeNotSupported,[]);
end;

procedure TFBClientAPI.SQLDecodeTimeTZ(var aTime: TDateTime;
  var aTimeZone: AnsiString; bufptr: PByte);
begin
  IBError(ibxeNotSupported,[]);
end;

procedure TFBClientAPI.SQLEncodeTimeStampTZ(aTimeStamp: TDateTime;
  aTimeZone: AnsiString; bufptr: PByte);
begin
  IBError(ibxeNotSupported,[]);
end;

procedure TFBClientAPI.SQLDecodeTimeStampTZ(var aTimeStamp: TDateTime;
  var aTimeZone: AnsiString; bufptr: PByte);
begin
  IBError(ibxeNotSupported,[]);
end;

{This function is a generic string to date decoder that also allows for a
 time zone component. A Time Zone is ignored if FBClient is less than Firebird 4}

function TFBClientAPI.FBStrToDate(S: AnsiString): TFBTZDateTime;
var Parts: TStringList;
    i: integer;
    TimeStamp: ISC_TIMESTAMP_TZ;

    function LTrim(S: AnsiString): AnsiString;
    begin
      while (S <> '') and (S[1] = ' ') do
        system.Delete(S,1,Length(S)-1);
    end;

begin
  with result do
  begin
    timestamp := 0;
    timeZone := '';
    timeZoneID := 0;
  end;

  Parts := TStringList.Create;
  try
    i := 0;
    repeat
      S := LTrim(S);
      i := AnsiPos(' ',S);
      if i > 0 then
      begin
        Parts.Add(system.copy(S,1,i-1));
        system.Delete(S,1,i);
      end;
    until i = 0;
    Parts.Add(S);

    case Parts.Count of
    1:
      {date or time part only}
      if not TryStrToDate(S,result.timestamp) then
        result.timestamp := StrToTime(Parts[0]);

    2:
      {date and time or time and TZ}
      if TryStrToDate(Parts[0],result.timestamp) then
        result.timestamp := result.timestamp + StrToTime(Parts[1])
      else
      begin
        result.timestamp := StrToTime(Parts[0]);
        if SupportsTimeZone then
        begin
          {Let Firebird validate the time zone part}
          SQLEncodeTimeTZ(result.timestamp,Parts[1],@TimeStamp);
          result.timeZone := Parts[1];
          result.timeZoneID := TimeStamp.time_zone;
        end;
      end;

    3:
      {date, time and TZ}
      begin
        result.timestamp := StrToDate(Parts[0]) + StrToTime(Parts[1]) ;
        if SupportsTimeZone then
        begin
          {Let Firebird validate the time zone part}
          SQLEncodeTimeTZ(result.timestamp,Parts[2],@TimeStamp);
          result.timeZone := Parts[2];
          result.timeZoneID := TimeStamp.time_zone;
        end;
      end;

    else
      IBError(ibxeInvalidDateTimeStr, [S]);
    end;
  finally
    Parts.Free;
  end;
end;

function TFBClientAPI.GetTimeZoneName(timeZoneID: word): AnsiString;
begin
  IBError(ibxeNotSupported,[]);
end;

function TFBClientAPI.IsLibraryLoaded: boolean;
begin
  Result := FFBLibrary.IBLibrary <> NilHandle;
end;

function TFBClientAPI.GetFBLibrary: IFirebirdLibrary;
begin
  Result := FFBLibrary;
end;

function TFBClientAPI.GetProcAddr(ProcName: PAnsiChar): Pointer;
begin
  Result := GetProcAddress(FFBLibrary.IBLibrary, ProcName);
  if not Assigned(Result) then
    raise Exception.CreateFmt(SFirebirdAPIFuncNotFound,[ProcName]);
end;

function TFBClientAPI.SupportsTimeZone: boolean;
begin
  Result := false;
end;

function TFBClientAPI.LoadInterface: boolean;
begin
  isc_sqlcode := GetProcAddr('isc_sqlcode'); {do not localize}
  isc_sql_interprete := GetProcAddr('isc_sql_interprete'); {do not localize}
  isc_interprete := GetProcAddr('isc_interprete'); {do not localize}
  isc_event_counts := GetProcAddr('isc_event_counts'); {do not localize}
  isc_event_block := GetProcAddr('isc_event_block'); {do not localize}
  isc_free := GetProcAddr('isc_free'); {do not localize}
  Result := assigned(isc_free);
end;

{ TFBStatus }

constructor TFBStatus.Create(aOwner: TFBClientAPI);
begin
  inherited Create;
  FOwner := aOwner;
  FIBDataBaseErrorMessages := [ShowSQLMessage, ShowIBMessage];
end;

function TFBStatus.GetIBErrorCode: Long;
begin
  Result := StatusVector^[1];
end;

function TFBStatus.Getsqlcode: Long;
begin
  with FOwner do
    Result := isc_sqlcode(PISC_STATUS(StatusVector));
end;

function TFBStatus.GetMessage: AnsiString;
var local_buffer: array[0..IBHugeLocalBufferLength - 1] of AnsiChar;
    IBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    sqlcode: Long;
    psb: PStatusVector;
begin
  Result := '';
  IBDataBaseErrorMessages := FIBDataBaseErrorMessages;
  sqlcode := Getsqlcode;
  if (ShowSQLCode in IBDataBaseErrorMessages) then
    Result := Result + 'SQLCODE: ' + IntToStr(sqlcode); {do not localize}

  Exclude(IBDataBaseErrorMessages, ShowSQLMessage);
  if (ShowSQLMessage in IBDataBaseErrorMessages) then
  begin
    with FOwner do
      isc_sql_interprete(sqlcode, local_buffer, IBLocalBufferLength);
    if (ShowSQLCode in FIBDataBaseErrorMessages) then
      Result := Result + CRLF;
    Result := Result + strpas(local_buffer);
  end;

  if (ShowIBMessage in IBDataBaseErrorMessages) then
  begin
    if (ShowSQLCode in IBDataBaseErrorMessages) or
       (ShowSQLMessage in IBDataBaseErrorMessages) then
      Result := Result + CRLF;
    psb := StatusVector;
    with FOwner do
    while (isc_interprete(@local_buffer, @psb) > 0) do
    begin
      if (Result <> '') and (Result[Length(Result)] <> LF) then
        Result := Result + CRLF;
      Result := Result + strpas(local_buffer);
    end;
  end;
  if (Result <> '') and (Result[Length(Result)] = '.') then
    Delete(Result, Length(Result), 1);
end;

function TFBStatus.CheckStatusVector(ErrorCodes: array of TFBStatusCode
  ): Boolean;
var
  p: PISC_STATUS;
  i: Integer;
  procedure NextP(i: Integer);
  begin
    p := PISC_STATUS(PAnsiChar(p) + (i * SizeOf(ISC_STATUS)));
  end;
begin
  p := PISC_STATUS(StatusVector);
  result := False;
  while (p^ <> 0) and (not result) do
    case p^ of
      3: NextP(3);
      1, 4:
      begin
        NextP(1);
        i := 0;
        while (i <= High(ErrorCodes)) and (not result) do
        begin
          result := p^ = ErrorCodes[i];
          Inc(i);
        end;
        NextP(1);
      end;
      else
        NextP(2);
    end;
end;

function TFBStatus.GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
begin
  EnterCriticalSection(TFBClientAPI.FIBCS);
  try
    result := FIBDataBaseErrorMessages;
  finally
    LeaveCriticalSection(TFBClientAPI.FIBCS);
  end;
end;

procedure TFBStatus.SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
begin
  EnterCriticalSection(TFBClientAPI.FIBCS);
  try
    FIBDataBaseErrorMessages := Value;
  finally
    LeaveCriticalSection(TFBClientAPI.FIBCS);
  end;
end;

initialization
  TFBLibrary.FEnvSetupDone := false;
  {$IFNDEF FPC}
  InitializeCriticalSection(TFBClientAPI.FIBCS);
  {$ELSE}
  InitCriticalSection(TFBClientAPI.FIBCS);
  {$ENDIF}

finalization
  TFBLibrary.FreeLibraries;
  {$IFNDEF FPC}
  DeleteCriticalSection(TFBClientAPI.FIBCS);
  {$ELSE}
  DoneCriticalSection(TFBClientAPI.FIBCS);
  {$ENDIF}
end.

