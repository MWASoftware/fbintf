unit FBLibrary;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$ENDIF}

interface

uses
  Classes, SysUtils, Dynlibs, DB, IB;

type
  { TFBLibrary }

  TFBLibrary = class
  private
    FFBLibraryName: string;
    procedure LoadIBLibrary;
  protected
    IBLibrary: TLibHandle; static;
    FProvider: IFirebird;
    function GetProcAddr(ProcName: PChar): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IBAlloc(var P; OldSize, NewSize: Integer);
    function IsEmbeddedServer: boolean; virtual;
    property FBLibraryName: string read FFBLibraryName;
    property Provider: IFirebird read FProvider;
  end;

implementation

uses
{$IFDEF WINDOWS }
  Windows;
{$ELSE}
  unix;
{$ENDIF}

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

{ TFBLibrary }

constructor TFBLibrary.Create;
begin
  IBLibrary := NilHandle;
  LoadIBLibrary;
end;

destructor TFBLibrary.Destroy;
begin
  if IBLibrary <> NilHandle then
  begin
    FreeLibrary(IBLibrary);
    IBLibrary := NilHandle;
  end;
  inherited Destroy;
end;

procedure TFBLibrary.IBAlloc(var P; OldSize, NewSize: Integer);
var
  i: Integer;
begin
  ReallocMem(Pointer(P), NewSize);
  for i := OldSize to NewSize - 1 do PChar(P)[i] := #0;
end;

function TFBLibrary.GetProcAddr(ProcName: PChar): Pointer;
begin
  Result := GetProcAddress(IBLibrary, ProcName);
  if not Assigned(Result) then
    raise Exception.Create('Unable to load Firebird Client Library');
end;

initialization
   TFBLibrary.IBLibrary := NilHandle;
end.

