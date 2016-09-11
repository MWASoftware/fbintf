unit FBClientAPI;

{$mode delphi}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$ELSE}
RawByteString = AnsiString; {Needed for backwards compatibility}
{$ENDIF}

interface

uses
  Classes, SysUtils, Dynlibs,  Firebird, IB, IBHeader, FBActivityMonitor,
  FBMessages, IBExternals;

type
  TStatusVector              = array[0..19] of NativeInt;
  PStatusVector              = ^TStatusVector;

  TFBClientAPI = class;

  { TFBStatus }

  TFBStatus = class(TInterfaceParent)
  private
    FIBCS: TRTLCriticalSection; static;
    FIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
  protected
    FOwner: TFBClientAPI;
  public
    constructor Create(aOwner: TFBClientAPI);
    function StatusVector: PStatusVector; virtual; abstract;

    {IStatus}
    function GetIBErrorCode: Long;
    function Getsqlcode: Long;
    function GetMessage: string;
    function CheckStatusVector(ErrorCodes: array of TFBStatusCode): Boolean;
    function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
  end;

  { TFBClientAPI }

  TFBClientAPI = class(TInterfaceParent)
  private
    FFBLibraryName: string; static;
    procedure LoadIBLibrary;
  protected
    IBLibrary: TLibHandle; static;
    function GetProcAddr(ProcName: PChar): Pointer;
    procedure LoadInterface; virtual;
  public
    {Taken from legacy API}
    isc_sqlcode: Tisc_sqlcode;
    isc_sql_interprete: Tisc_sql_interprete;
    isc_interprete: Tisc_interprete;
    isc_event_counts: Tisc_event_counts;
    isc_event_block: Tisc_event_block;
    isc_wait_for_event: Tisc_wait_for_event;
    isc_free: Tisc_free;

    constructor Create;
    destructor Destroy; override;
    procedure IBAlloc(var P; OldSize, NewSize: Integer);
    procedure IBDataBaseError;

    {Encode/Decode}
    procedure EncodeInteger(aValue: integer; len: integer; buffer: PChar);
    function DecodeInteger(bufptr: PChar; len: short): integer; virtual; abstract;
    procedure SQLEncodeDate(aDate: TDateTime; bufptr: PChar); virtual; abstract;
    function SQLDecodeDate(byfptr: PChar): TDateTime; virtual; abstract;
    procedure SQLEncodeTime(aTime: TDateTime; bufptr: PChar); virtual; abstract;
    function SQLDecodeTime(bufptr: PChar): TDateTime;  virtual; abstract;
    procedure SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PChar); virtual; abstract;
    function SQLDecodeDateTime(bufptr: PChar): TDateTime; virtual; abstract;


    {IFirebirdAPI}
    function GetStatus: IStatus; virtual; abstract;
    function IsEmbeddedServer: boolean;
    function GetLibraryName: string;
    function GetCharsetName(CharSetID: integer): string;
    function CharSetID2CodePage(CharSetID: integer; var CodePage: TSystemCodePage): boolean;
    function CodePage2CharSetID(CodePage: TSystemCodePage; var CharSetID: integer): boolean;
    function CharSetName2CharSetID(CharSetName: string; var CharSetID: integer): boolean;
    function CharSetWidth(CharSetID: integer; var Width: integer): boolean;
  end;

const FirebirdClientAPI: TFBClientAPI = nil;

implementation

uses IBUtils
{$IFDEF WINDOWS }
,Windows,Registry, WinDirs;
{$ENDIF};

const
{$IFDEF LINUX}
FIREBIRD_SO2 = 'libfbembed.so.2.5:libfbembed.so.2.1:libfbclient.so.2';

{$ENDIF}
{$IFDEF DARWIN}
FIREBIRD_SO2 = 'libfbclient.dylib';
{$ENDIF}
{$IFDEF WINDOWS}
IBASE_DLL = 'gds32.dll';
FIREBIRD_CLIENT = 'fbclient.dll'; {do not localize}
FIREBIRD_EMBEDDED = 'fbembed.dll';
{$ENDIF}


{$IFDEF UNIX}
{$I uloadlibrary.inc}
{$ELSE}
{$I wloadlibrary.inc}
{$ENDIF}

type
  TCharsetMap = record
    CharsetID: integer;
    CharSetName: string;
    CharSetWidth: integer;
    CodePage: TSystemCodePage;
  end;

const
  CharSetMap: array [0..69] of TCharsetMap = (
  (CharsetID: 0; CharSetName: 'NONE'; CharSetWidth: 1; CodePage: CP_NONE),
  (CharsetID: 1; CharSetName: 'OCTETS'; CharSetWidth: 1; CodePage: CP_NONE),
  (CharsetID: 2; CharSetName: 'ASCII'; CharSetWidth: 1; CodePage: CP_ASCII),
  (CharsetID: 3; CharSetName: 'UNICODE_FSS'; CharSetWidth: 3; CodePage: CP_UTF16),
  (CharsetID: 4; CharSetName: 'UTF8'; CharSetWidth: 4; CodePage: CP_UTF8),
  (CharsetID: 5; CharSetName: 'SJIS_0208'; CharSetWidth: 2; CodePage: 20932),
  (CharsetID: 6; CharSetName: 'EUCJ_0208'; CharSetWidth: 2; CodePage: 20932),
  (CharsetID: 7; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 8; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 9; CharSetName: 'DOS737'; CharSetWidth: 1; CodePage: 737),
  (CharsetID: 10; CharSetName: 'DOS437'; CharSetWidth: 1; CodePage: 437),
  (CharsetID: 11; CharSetName: 'DOS850'; CharSetWidth: 1; CodePage: 850),
  (CharsetID: 12; CharSetName: 'DOS865'; CharSetWidth: 1; CodePage: 865),
  (CharsetID: 13; CharSetName: 'DOS860'; CharSetWidth: 1; CodePage: 860),
  (CharsetID: 14; CharSetName: 'DOS863'; CharSetWidth: 1; CodePage: 863),
  (CharsetID: 15; CharSetName: 'DOS775'; CharSetWidth: 1; CodePage: 775),
  (CharsetID: 16; CharSetName: 'DOS858'; CharSetWidth: 1; CodePage: 858),
  (CharsetID: 17; CharSetName: 'DOS862'; CharSetWidth: 1; CodePage: 862),
  (CharsetID: 18; CharSetName: 'DOS864'; CharSetWidth: 1; CodePage: 864),
  (CharsetID: 19; CharSetName: 'NEXT'; CharSetWidth: 1; CodePage: CP_NONE),
  (CharsetID: 20; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 21; CharSetName: 'ISO8859_1'; CharSetWidth: 1; CodePage: 28591),
  (CharsetID: 22; CharSetName: 'ISO8859_2'; CharSetWidth: 1; CodePage: 28592),
  (CharsetID: 23; CharSetName: 'ISO8859_3'; CharSetWidth: 1; CodePage: 28593),
  (CharsetID: 24; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 25; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 26; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 27; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 28; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 29; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 30; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 31; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 32; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 33; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 34; CharSetName: 'ISO8859_4'; CharSetWidth: 1; CodePage: 28594),
  (CharsetID: 35; CharSetName: 'ISO8859_5'; CharSetWidth: 1; CodePage: 28595),
  (CharsetID: 36; CharSetName: 'ISO8859_6'; CharSetWidth: 1; CodePage: 28596),
  (CharsetID: 37; CharSetName: 'ISO8859_7'; CharSetWidth: 1; CodePage: 28597),
  (CharsetID: 38; CharSetName: 'ISO8859_8'; CharSetWidth: 1; CodePage: 28598),
  (CharsetID: 39; CharSetName: 'ISO8859_9'; CharSetWidth: 1; CodePage: 28599),
  (CharsetID: 40; CharSetName: 'ISO8859_13'; CharSetWidth: 1; CodePage: 28603),
  (CharsetID: 41; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 42; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 43; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 44; CharSetName: 'KSC_5601'; CharSetWidth: 2; CodePage: 949),
  (CharsetID: 45; CharSetName: 'DOS852'; CharSetWidth: 1; CodePage: 852),
  (CharsetID: 46; CharSetName: 'DOS857'; CharSetWidth: 1; CodePage: 857),
  (CharsetID: 47; CharSetName: 'DOS861'; CharSetWidth: 1; CodePage: 861),
  (CharsetID: 48; CharSetName: 'DOS866'; CharSetWidth: 1; CodePage: 866),
  (CharsetID: 49; CharSetName: 'DOS869'; CharSetWidth: 1; CodePage: 869),
  (CharsetID: 50; CharSetName: 'CYRL'; CharSetWidth: 1; CodePage: 1251),
  (CharsetID: 51; CharSetName: 'WIN1250'; CharSetWidth: 1; CodePage: 1250),
  (CharsetID: 52; CharSetName: 'WIN1251'; CharSetWidth: 1; CodePage: 1251),
  (CharsetID: 53; CharSetName: 'WIN1252'; CharSetWidth: 1; CodePage: 1252),
  (CharsetID: 54; CharSetName: 'WIN1253'; CharSetWidth: 1; CodePage: 1253),
  (CharsetID: 55; CharSetName: 'WIN1254'; CharSetWidth: 1; CodePage: 1254),
  (CharsetID: 56; CharSetName: 'BIG_5'; CharSetWidth: 2; CodePage: 950),
  (CharsetID: 57; CharSetName: 'GB_2312'; CharSetWidth: 2; CodePage: 936),
  (CharsetID: 58; CharSetName: 'WIN1255'; CharSetWidth: 1; CodePage: 1255),
  (CharsetID: 59; CharSetName: 'WIN1256'; CharSetWidth: 1; CodePage: 1256),
  (CharsetID: 60; CharSetName: 'WIN1257'; CharSetWidth: 1; CodePage: 1257),
  (CharsetID: 61; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 62; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
  (CharsetID: 63; CharSetName: 'KOI8R'; CharSetWidth: 1; CodePage: 20866),
  (CharsetID: 64; CharSetName: 'KOI8U'; CharSetWidth: 1; CodePage: 21866),
  (CharsetID: 65; CharSetName: 'WIN1258'; CharSetWidth: 1; CodePage: 1258),
  (CharsetID: 66; CharSetName: 'TIS620'; CharSetWidth: 1; CodePage: 874),
  (CharsetID: 67; CharSetName: 'GBK'; CharSetWidth: 2; CodePage: 936),
  (CharsetID: 68; CharSetName: 'CP943C'; CharSetWidth: 2; CodePage: 943),
  (CharsetID: 69; CharSetName: 'GB18030'; CharSetWidth: 4; CodePage: 54936)
);

{ TFBClientAPI }

constructor TFBClientAPI.Create;
begin
  inherited Create;
  if IBLibrary = NilHandle then
    LoadIBLibrary;
  if (IBLibrary <> NilHandle) then
    LoadInterface;
  FirebirdClientAPI := self;
end;

destructor TFBClientAPI.Destroy;
begin
  FirebirdClientAPI := nil;
  inherited Destroy;
end;

procedure TFBClientAPI.IBAlloc(var P; OldSize, NewSize: Integer);
var
  i: Integer;
begin
  ReallocMem(Pointer(P), NewSize);
  for i := OldSize to NewSize - 1 do PChar(P)[i] := #0;
end;

procedure TFBClientAPI.IBDataBaseError;
begin
  raise EIBInterBaseError.Create(GetStatus);
end;

procedure TFBClientAPI.EncodeInteger(aValue: integer; len: integer; buffer: PChar);
begin
  while len > 0 do
  begin
    buffer^ := char(aValue and $FF);
    Inc(buffer);
    Dec(len);
    aValue := aValue shr 8;
  end;
end;

function TFBClientAPI.GetProcAddr(ProcName: PChar): Pointer;
begin
  Result := GetProcAddress(IBLibrary, ProcName);
  if not Assigned(Result) then
    raise Exception.CreateFmt(SFirebirdAPIFuncNotFound,[ProcName]);
end;

procedure TFBClientAPI.LoadInterface;
begin
  isc_sqlcode := GetProcAddr('isc_sqlcode'); {do not localize}
  isc_sql_interprete := GetProcAddr('isc_sql_interprete'); {do not localize}
  isc_interprete := GetProcAddr('isc_interprete'); {do not localize}
  isc_event_counts := GetProcAddr('isc_event_counts'); {do not localize}
  isc_event_block := GetProcAddr('isc_event_block'); {do not localize}
  isc_wait_for_event := GetProcAddr('isc_wait_for_event'); {do not localize}
  isc_free := GetProcAddr('isc_free'); {do not localize}
end;

function TFBClientAPI.GetLibraryName: string;
begin
  Result := FFBLibraryName;
end;

function TFBClientAPI.GetCharsetName(CharSetID: integer): string;
begin
  Result := '';
  if (CharSetID >= Low(CharSetMap)) and (CharSetID <= High(CharSetMap)) and
                                  (CharSetMap[CharSetID].CharSetID = CharSetID) then
    begin
      Result := CharSetMap[CharSetID].CharSetName;
      Exit;
    end;
end;

function TFBClientAPI.CharSetID2CodePage(CharSetID: integer;
  var CodePage: TSystemCodePage): boolean;
begin
  Result := (CharSetID >= Low(CharSetMap)) and (CharSetID <= High(CharSetMap))
               and (CharSetMap[CharSetID].CharSetID = CharSetID);
  if Result then
    begin
      CodePage := CharSetMap[CharSetID].CodePage;
      Result := true;
      Exit;
    end;
end;

function TFBClientAPI.CodePage2CharSetID(CodePage: TSystemCodePage;
  var CharSetID: integer): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(CharSetMap) to High(CharSetMap) do
    if CharSetMap[i].CodePage = CodePage then
    begin
      CharSetID := CharSetMap[i].CharSetID;
      Result := true;
      Exit;
    end;
end;

function TFBClientAPI.CharSetName2CharSetID(CharSetName: string;
  var CharSetID: integer): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(CharSetMap) to High(CharSetMap) do
    if CompareStr(CharSetMap[i].CharSetName, CharSetName) = 0 then
    begin
      CharSetID := CharSetMap[i].CharSetID;
      Result := true;
      Exit;
    end;
end;

function TFBClientAPI.CharSetWidth(CharSetID: integer; var Width: integer
  ): boolean;
begin
  Result := (CharSetID >= Low(CharSetMap)) and (CharSetID <= High(CharSetMap))
               and (CharSetMap[CharSetID].CharSetID = CharSetID);
  if Result then
    begin
      Width := CharSetMap[CharSetID].CharSetWidth;
      Result := true;
      Exit;
    end;
end;

const
  IBLocalBufferLength = 512;
  IBBigLocalBufferLength = IBLocalBufferLength * 2;
  IBHugeLocalBufferLength = IBBigLocalBufferLength * 20;

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

function TFBStatus.GetMessage: string;
var local_buffer: array[0..IBHugeLocalBufferLength - 1] of char;
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
    p := PISC_STATUS(PChar(p) + (i * SizeOf(ISC_STATUS)));
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
  EnterCriticalSection(FIBCS);
  try
    result := FIBDataBaseErrorMessages;
  finally
    LeaveCriticalSection(FIBCS);
  end;
end;

procedure TFBStatus.SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
begin
  EnterCriticalSection(FIBCS);
  try
    FIBDataBaseErrorMessages := Value;
  finally
    LeaveCriticalSection(FIBCS);
  end;
end;
initialization
  TFBClientAPI.IBLibrary := NilHandle;
  InitCriticalSection(TFBStatus.FIBCS);

finalization
  DoneCriticalSection(TFBStatus.FIBCS);
  if TFBClientAPI.IBLibrary <> NilHandle then
  begin
    FreeLibrary(TFBClientAPI.IBLibrary);
    TFBClientAPI.IBLibrary := NilHandle;
    TFBClientAPI.FFBLibraryName := '';
  end;

end.

