unit FBLibrary;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$ENDIF}

interface

uses
  Classes, SysUtils, Dynlibs,  IB;

type

  { TObjectOwner }

  TObjectOwner = class(TInterfacedObject)
  private
    FOwnedObjects: TList;
  protected
    property OwnedObjects: TList read FOwnedObjects;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterObj(AObj: TObject);
    procedure UnRegisterObj(AObj: TObject);
  end;

  { TFBLibrary }

  TFBLibrary = class(TObjectOwner)
  private
    FFBLibraryName: string;
    procedure LoadIBLibrary;
  protected
    IBLibrary: TLibHandle; static;
    function GetProcAddr(ProcName: PChar): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IBAlloc(var P; OldSize, NewSize: Integer);
    function IsEmbeddedServer: boolean; virtual;
    property FBLibraryName: string read FFBLibraryName;
  end;

implementation

uses FBErrorMessages

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

{ TObjectOwner }

constructor TObjectOwner.Create;
begin
  inherited Create;
//  writeln('Creating ' + ClassName);
  FOwnedObjects := TList.Create;
end;

destructor TObjectOwner.Destroy;
begin
//  writeln('Destroying ' + ClassName);
  inherited Destroy;
end;

procedure TObjectOwner.RegisterObj(AObj: TObject);
var index: integer;
begin
  index := FOwnedObjects.IndexOf(AObj);
  if index = -1 then
    FOwnedObjects.Add(AObj);
end;

procedure TObjectOwner.UnRegisterObj(AObj: TObject);
begin
  FOwnedObjects.Remove(AObj);
end;
{ TFBLibrary }

constructor TFBLibrary.Create;
begin
  inherited Create;
  if IBLibrary <> NilHandle then
    IBError(ibxeFirebirdLibraryLoaded,[nil]);
  LoadIBLibrary;
end;

destructor TFBLibrary.Destroy;
begin
  inherited Destroy;  {Free owned objects first}
  if IBLibrary <> NilHandle then
  begin
    FreeLibrary(IBLibrary);
    IBLibrary := NilHandle;
  end;
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
    raise Exception.CreateFmt('Unable to load Firebird Client Library Function "%s"',[ProcName]);
end;

initialization
   TFBLibrary.IBLibrary := NilHandle;
end.

