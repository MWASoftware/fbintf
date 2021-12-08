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
   IB, IBHeader, FBActivityMonitor, FBMessages, IBExternals, FmtBCD;

{For Linux see result of GetFirebirdLibListruntime/nongui/winipc.inc method}
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

const
  {fb_shutdown reasons}
  fb_shutrsn_svc_stopped          = -1;
  fb_shutrsn_no_connection	  = -2;
  fb_shutrsn_app_stopped	  = -3;
  fb_shutrsn_signal		  = -5;
  fb_shutrsn_services		  = -6;
  fb_shutrsn_exit_called	  = -7;

const
    DefaultTimeZoneFile = '/etc/timezone';

const
  IBLocalBufferLength = 512;
  IBBigLocalBufferLength = IBLocalBufferLength * 2;
  IBHugeLocalBufferLength = IBBigLocalBufferLength * 20;

type
  TStatusVector              = array[0..19] of NativeInt;
  PStatusVector              = ^TStatusVector;

  TFBClientAPI = class;

  { TFBStatus }

  TFBStatus = class(TFBInterfacedObject)
  private
    FIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    FPrefix: AnsiString;
  protected
    FOwner: TFBClientAPI;
    function GetIBMessage: Ansistring;
    function GetSQLMessage: Ansistring; virtual; abstract;
  public
    constructor Create(aOwner: TFBClientAPI; prefix: AnsiString='');
    function StatusVector: PStatusVector; virtual; abstract;

    {IStatus}
    function GetIBErrorCode: TStatusCode;
    function Getsqlcode: TStatusCode;
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
  private
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
    function SameLibrary(aLibName: string): boolean;

  public
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
    FLocalTimeZoneName: AnsiString; {Informal Time Zone Name from tzname e.g. GMT or BST}
    FTZDataTimeZoneID: AnsiString; {TZData DB ID e.g. Europe/London}
    FLocalTimeOffset: integer;
    FIsDaylightSavingsTime: boolean;
    class var FIBCS: TRTLCriticalSection;
    function FBTimeStampToDateTime(aDate, aTime: longint): TDateTime;
    procedure GetTZDataSettings;
  protected
    FFBLibrary: TFBLibrary;
    function GetProcAddr(ProcName: PAnsiChar): Pointer;

  protected type
    Tfb_shutdown = function (timeout: uint;
                                 const reason: int): int;
                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  protected
    {FB Shutdown API}
    fb_shutdown: Tfb_shutdown;

  public
    {Taken from legacy API}
    isc_interprete: Tisc_interprete;
    isc_sqlcode: Tisc_sqlcode;

    constructor Create(aFBLibrary: TFBLibrary);
    procedure IBAlloc(var P; OldSize, NewSize: Integer);
    procedure IBDataBaseError;
    function LoadInterface: boolean; virtual;
    procedure FBShutdown; virtual;
    function GetAPI: IFirebirdAPI; virtual; abstract;
    {$IFDEF UNIX}
    function GetFirebirdLibList: string; virtual; abstract;
    {$ENDIF}
    function HasDecFloatSupport: boolean;
    function HasInt128Support: boolean; virtual;
    function HasLocalTZDB: boolean; virtual;
    function HasExtendedTZSupport: boolean; virtual;
    function HasTimeZoneSupport: boolean; virtual;

  public
    property LocalTimeZoneName: AnsiString read FLocalTimeZoneName;
    property TZDataTimeZoneID: AnsiString read FTZDataTimeZoneID;
    property LocalTimeOffset: integer read FLocalTimeOffset;
  public
    {Encode/Decode}
    procedure EncodeInteger(aValue: int64; len: integer; buffer: PByte);
    function DecodeInteger(bufptr: PByte; len: short): int64;
    procedure SQLEncodeDate(aDate: TDateTime; bufptr: PByte);  virtual; abstract;
    function SQLDecodeDate(byfptr: PByte): TDateTime;  virtual; abstract;
    procedure SQLEncodeTime(aTime: TDateTime; bufptr: PByte);  virtual; abstract;
    function SQLDecodeTime(bufptr: PByte): TDateTime;  virtual; abstract;
    procedure SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PByte); virtual; abstract;
    function  SQLDecodeDateTime(bufptr: PByte): TDateTime; virtual; abstract;
    function Int128ToStr(bufptr: PByte; scale: integer): AnsiString; virtual;
    procedure StrToInt128(scale: integer; aValue: AnsiString; bufptr: PByte);
      virtual;
    procedure SQLDecFloatEncode(aValue: tBCD; SQLType: cardinal; bufptr: PByte); virtual;
    function SQLDecFloatDecode(SQLType: cardinal;  bufptr: PByte): tBCD; virtual;

    {IFirebirdAPI}
    function GetStatus: IStatus; virtual; abstract;
    function IsLibraryLoaded: boolean;
    function IsEmbeddedServer: boolean; virtual; abstract;
    function GetFBLibrary: IFirebirdLibrary;
    function GetImplementationVersion: AnsiString;
    function GetClientMajor: integer;  virtual; abstract;
    function GetClientMinor: integer;  virtual; abstract;
end;

implementation

uses IBUtils, Registry,
  {$IFDEF Unix} unix, initc, dl, {$ENDIF}
{$IFDEF FPC}
{$IFDEF WINDOWS }
WinDirs,
{$ENDIF}
{$ELSE}
 ShlObj,
{$ENDIF}
SysUtils;

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
  (FFirebirdAPI as TFBClientAPI).FBShutdown;
  if FIBLibrary <> NilHandle then
    FreeLibrary(FIBLibrary);
  FIBLibrary := NilHandle;
  FFBLibraryName := '';
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
  FreeFBLibrary;
  FFirebirdAPI := nil;
  inherited Destroy;
end;

class function TFBLibrary.GetFBLibrary(aLibPathName: string): IFirebirdLibrary;
var i: integer;
begin
  Result := nil;
  if aLibPathName <> '' then
  begin
    for i := 0 to Length(FLibraryList) - 1 do
    begin
      if (FLibraryList[i] as TFBLibrary).SameLibrary(aLibPathName) then
      begin
        Result := FLibraryList[i];
        Exit;
      end;
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

function TFBLibrary.SameLibrary(aLibName: string): boolean;
begin
  Result := FRequestedLibName = aLibName;
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
  GetTZDataSettings;
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

procedure TFBClientAPI.EncodeInteger(aValue: int64; len: integer; buffer: PByte);
begin
  while len > 0 do
  begin
    buffer^ := aValue and $FF;
    Inc(buffer);
    Dec(len);
    aValue := aValue shr 8;
  end;
end;

(*
  DecodeInteger is Translated from

SINT64 API_ROUTINE isc_portable_integer(const UCHAR* ptr, SSHORT length)
if (!ptr || length <= 0 || length > 8)
	return 0;

SINT64 value = 0;
int shift = 0;

while (--length > 0)
{
	value += ((SINT64) *ptr++) << shift;
	shift += 8;
}

value += ((SINT64)(SCHAR) *ptr) << shift;

return value;
*)

function TFBClientAPI.DecodeInteger(bufptr: PByte; len: short): int64;
var shift: integer;
begin
  Result := 0;
  if (BufPtr = nil) or (len <= 0) or (len > 8) then
    Exit;

  shift := 0;
  dec(len);
  while len > 0 do
  begin
    Result := Result + (int64(bufptr^) shl shift);
    Inc(bufptr);
    shift := shift + 8;
    dec(len);
  end;
  Result := Result + (int64(bufptr^) shl shift);
end;

function TFBClientAPI.Int128ToStr(bufptr: PByte; scale: integer): AnsiString;
begin
  if not HasInt128Support then
    IBError(ibxeNotSupported,[]);
end;

procedure TFBClientAPI.StrToInt128(scale: integer; aValue: AnsiString; bufptr: PByte);
begin
  if not HasInt128Support then
    IBError(ibxeNotSupported,[]);
end;

procedure TFBClientAPI.SQLDecFloatEncode(aValue: tBCD; SQLType: cardinal;
  bufptr: PByte);
begin
  if not HasDecFloatSupport then
    IBError(ibxeNotSupported,[]);
end;

function TFBClientAPI.SQLDecFloatDecode(SQLType: cardinal; bufptr: PByte): tBCD;
begin
  if not HasDecFloatSupport then
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

function TFBClientAPI.FBTimeStampToDateTime(aDate, aTime: longint): TDateTime;
begin
  {aDate/aTime are in TTimestamp format but aTime is decimilliseconds}
  aDate := aDate - DateDelta;
  if aDate < 0 then
    Result := trunc(aDate) - abs(frac(aTime / (MSecsPerDay*10)))
  else
    Result := trunc(aDate) + abs(frac(aTime / (MSecsPerDay*10)));
end;

{$IFDEF UNIX}

procedure TFBClientAPI.GetTZDataSettings;
var S: TStringList;
begin
  FLocalTimeOffset := GetLocalTimeOffset;
  {$if declared(Gettzname)}
  FLocalTimeZoneName := Gettzname(tzdaylight);
  {$else}
  FLocalTimeZoneName := tzname[tzdaylight];
  {$ifend}
  FIsDaylightSavingsTime := tzdaylight;
  if FileExists(DefaultTimeZoneFile) then
  begin
    S := TStringList.Create;
    try
      S.LoadFromFile(DefaultTimeZoneFile);
      if S.Count > 0 then
        FTZDataTimeZoneID := S[0];
    finally
      S.Free;
    end;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
procedure TFBClientAPI.GetTZDataSettings;
var TZInfo: TTimeZoneInformation;
begin
  FIsDaylightSavingsTime := false;
  {is there any way of working out the default TZData DB time zone ID under Windows?}
  case GetTimeZoneInformation(TZInfo) of
    TIME_ZONE_ID_UNKNOWN:
      begin
        FLocalTimeZoneName := '';
        FLocalTimeOffset := 0;
      end;
    TIME_ZONE_ID_STANDARD:
      begin
        FLocalTimeZoneName := strpas(PWideChar(@TZInfo.StandardName));
        FLocalTimeOffset := TZInfo.Bias;
      end;
    TIME_ZONE_ID_DAYLIGHT:
      begin
        FLocalTimeZoneName := strpas(PWideChar(@TZInfo.DaylightName));
        FLocalTimeOffset := TZInfo.DayLightBias;
        FIsDaylightSavingsTime := true;
      end;
  end;
end;
{$ENDIF}

function TFBClientAPI.GetProcAddr(ProcName: PAnsiChar): Pointer;
begin
  Result := nil;
  if assigned(FFBLibrary) and (FFBLibrary.IBLibrary <> NilHandle) then
    Result := GetProcAddress(FFBLibrary.IBLibrary, ProcName);
  if not Assigned(Result) then
    raise Exception.CreateFmt(SFirebirdAPIFuncNotFound,[ProcName]);
end;

function TFBClientAPI.HasDecFloatSupport: boolean;
begin
  Result := GetClientMajor >= 4;
end;

function TFBClientAPI.HasInt128Support: boolean;
begin
  Result := false;
end;

function TFBClientAPI.HasLocalTZDB: boolean;
begin
  Result := false;
end;

function TFBClientAPI.HasExtendedTZSupport: boolean;
begin
  Result := false;
end;

function TFBClientAPI.HasTimeZoneSupport: boolean;
begin
  Result := false;
end;

function TFBClientAPI.GetImplementationVersion: AnsiString;
begin
  Result := Format('%d.%d',[GetClientMajor,GetClientMinor]);
end;

function TFBClientAPI.LoadInterface: boolean;
begin
  isc_interprete := GetProcAddr('isc_interprete'); {do not localize}
  isc_sqlcode := GetProcAddr('isc_sqlcode'); {do not localize}
  fb_shutdown := GetProcAddr('fb_shutdown'); {do not localize}
  Result := assigned(isc_sqlcode);
end;

procedure TFBClientAPI.FBShutdown;
begin
  if assigned(fb_shutdown) then
    fb_shutdown(0,fb_shutrsn_exit_called);
end;

{ TFBStatus }

function TFBStatus.GetIBMessage: Ansistring;
var psb: PStatusVector;
    local_buffer: array[0..IBHugeLocalBufferLength - 1] of AnsiChar;
begin
  psb := StatusVector;
  Result := '';
  if assigned(FOwner.isc_interprete) then
  while FOwner.isc_interprete(@local_buffer,@psb) > 0 do
  begin
    if (Result <> '') and (Result[Length(Result)] <> LF) then
      Result := Result + LineEnding + '-';
    Result := Result + strpas(local_buffer);
  end;
end;

constructor TFBStatus.Create(aOwner: TFBClientAPI; prefix: AnsiString);
begin
  inherited Create;
  FOwner := aOwner;
  FPrefix := prefix;
  FIBDataBaseErrorMessages := [ShowSQLCode, ShowSQLMessage, ShowIBMessage];
end;

function TFBStatus.GetIBErrorCode: TStatusCode;
begin
  Result := StatusVector^[1];
end;

function TFBStatus.Getsqlcode: TStatusCode;
begin
  if assigned(FOwner.isc_sqlcode) then
    Result := FOwner.isc_sqlcode(PISC_STATUS(StatusVector))
  else
    Result := -999; {generic SQL Code}
end;

function TFBStatus.GetMessage: AnsiString;
var IBDataBaseErrorMessages: TIBDataBaseErrorMessages;
begin
  Result := FPrefix;
  IBDataBaseErrorMessages := FIBDataBaseErrorMessages;
  if (ShowSQLCode in IBDataBaseErrorMessages) then
    Result := Result + 'SQLCODE: ' + IntToStr(Getsqlcode); {do not localize}

  if [ShowSQLMessage, ShowIBMessage]*IBDataBaseErrorMessages <> [] then
  begin
    if (ShowSQLCode in FIBDataBaseErrorMessages) then
      Result := Result + LineEnding;
    Result := Result + 'Engine Code: ' + IntToStr(GetIBErrorCode) + ' ';
  end;

  if (ShowSQLMessage in IBDataBaseErrorMessages) then
    Result := Result + GetSQLMessage;

  if (ShowIBMessage in IBDataBaseErrorMessages) then
  begin
    if ShowSQLMessage in IBDataBaseErrorMessages then
      Result := Result + LineEnding;
    Result := Result + GetIBMessage;
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

