unit FBClientAPI;

{$mode delphi}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$ENDIF}

interface

uses
  Classes, SysUtils, Dynlibs,  Firebird, IB, FBActivityMonitor;

type
  Tfb_get_master_interface = function: IMaster;
                              {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  { TFBClientAPI }

  TFBClientAPI = class(TInterfaceParent)
  private
    FFBLibraryName: string;
    fb_get_master_interface: Tfb_get_master_interface;
    FMaster: IMaster;
    procedure LoadIBLibrary;
  protected
    IBLibrary: TLibHandle; static;
    function GetProcAddr(ProcName: PChar): Pointer;
    procedure LoadInterface; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IBAlloc(var P; OldSize, NewSize: Integer);
    property MasterIntf: IMaster read FMaster;

    {IFirebirdAPI}
    function IsEmbeddedServer: boolean;
    function GetLibraryName: string;
  end;

implementation

uses FBMessages

{$IFDEF WINDOWS }
, Windows,Registry, WinDirs
{$ENDIF}
;

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

{ TFBClientAPI }

constructor TFBClientAPI.Create;
begin
  inherited Create;
  if IBLibrary <> NilHandle then
    IBError(ibxeFirebirdLibraryLoaded,[nil]);
  LoadIBLibrary;
  if (IBLibrary <> NilHandle) then
    LoadInterface;
end;

destructor TFBClientAPI.Destroy;
begin
  if IBLibrary <> NilHandle then
  begin
    FreeLibrary(IBLibrary);
    IBLibrary := NilHandle;
  end;
  inherited Destroy;
end;

procedure TFBClientAPI.IBAlloc(var P; OldSize, NewSize: Integer);
var
  i: Integer;
begin
  ReallocMem(Pointer(P), NewSize);
  for i := OldSize to NewSize - 1 do PChar(P)[i] := #0;
end;

function TFBClientAPI.GetProcAddr(ProcName: PChar): Pointer;
begin
  Result := GetProcAddress(IBLibrary, ProcName);
  if not Assigned(Result) then
    raise Exception.CreateFmt(SFirebirdAPIFuncNotFound,[ProcName]);
end;

procedure TFBClientAPI.LoadInterface;
begin
  fb_get_master_interface := GetProcAddress(IBLibrary, 'fb_get_master_interface'); {do not localize}
  if assigned(fb_get_master_interface) then
    FMaster := fb_get_master_interface;
end;

function TFBClientAPI.GetLibraryName: string;
begin
  Result := FFBLibraryName;
end;

initialization
   TFBClientAPI.IBLibrary := NilHandle;
end.

