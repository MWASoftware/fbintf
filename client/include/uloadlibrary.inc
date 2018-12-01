function TFBLibrary.LoadIBLibrary: boolean;

  function FindLibrary(LibNameList: string): TLibHandle;
  var LibNames: TStringList;
      i: integer;
  begin
    Result := NilHandle;
    LibNames := TStringList.Create;
    try
      LibNames.Delimiter := ':';
      LibNames.StrictDelimiter := true;
      LibNames.DelimitedText := LibNameList; {Split list on semi-colon}
      for i := 0 to LibNames.Count - 1 do
      begin
        Result := LoadLibrary(LibNames[i]);
        if Result <> NilHandle then
        begin
          FFBLibraryName := LibNames[i];
          Exit;
        end;
      end;
    finally
      LibNames.Free;
    end;
  end;

var LibName: string;
begin
  Result := FIBLibrary <> NilHandle;
  if Result then Exit;

  LibName := GetOverrideLibName;
  if LibName = '' then
    LibName := (FFirebirdAPI as TFBClientAPI).GetFirebirdLibList;
  FIBLibrary := FindLibrary(LibName);
  {$IFDEF DARWIN}
  if FIBLibrary = NilHandle then
  begin
    {See http://paulbeachsblog.blogspot.co.uk/2008/03/where-is-libfbclientdylib-on-macosx.html
     Try loading direct from Firebird Framework}

    LibName := '/Library/Frameworks/Firebird.framework/Firebird';
    FIBLibrary := LoadLibrary(LibName);
    if FIBLibrary = NilHandle then
    begin
      LibName := '/Library/Frameworks/Firebird.framework/Libraries/libfbclient.dylib';
      FIBLibrary := LoadLibrary(LibName);
    end;
    if FIBLibrary <> NilHandle then
       FFBLibraryName := ExtractFileName(LibName);
  end;
  {$ENDIF}
  Result := FIBLibrary <> NilHandle;
end;

{SetEnvironmentVariable doesn't exist so we have to use C Library}
function setenv(name:Pchar; value:Pchar; replace:integer):integer;cdecl;external clib name 'setenv';
function unsetenv(name:Pchar):integer;cdecl;external clib name 'unsetenv';

function SetEnvironmentVariable(name:PAnsiChar; value:PAnsiChar):boolean;
// Set environment variable; if empty string given, remove it.
begin
  result:=false; //assume failure
  if value = '' then
  begin
    // Assume user wants to remove variable.
    if unsetenv(name)=0 then result:=true;
  end
  else
  begin
    // Non empty so set the variable
    if setenv(name, value, 1)=0 then result:=true;
  end;
end;

class procedure TFBLibrary.SetupEnvironment;
var TmpDir: AnsiString;
begin
  if FEnvSetupDone then Exit;
    TmpDir := GetTempDir +
        DirectorySeparator + 'firebird_' + sysutils.GetEnvironmentVariable('USER');
    if sysutils.GetEnvironmentVariable('FIREBIRD_TMP') = '' then
    begin
      if not DirectoryExists(tmpDir) then
        mkdir(tmpDir);
      SetEnvironmentVariable('FIREBIRD_TMP',PAnsiChar(TmpDir));
    end;
    if sysutils.GetEnvironmentVariable('FIREBIRD_LOCK') = '' then
    begin
      if not DirectoryExists(tmpDir) then
        mkdir(tmpDir);
      SetEnvironmentVariable('FIREBIRD_LOCK',PAnsiChar(TmpDir));
    end;
  FEnvSetupDone := true;
end;

{Type and function definition for dlinfo imported from dlfcn.h}
const
  RTLD_DI_ORIGIN = 6;

type
  Tdlinfo = function (Lib : Pointer; Request: longint; info: pointer) : Longint; cdecl;

function TFBLibrary.GetLibraryFilePath: string;
var path: array [0..IBLocalBufferLength] of char; {hopefully this is big enough}
    dlinfo: Tdlinfo;
begin
  Result := FFBLibraryName;
  dlinfo := dlsym(RTLD_DEFAULT,'dlinfo');
  if assigned(dlinfo) then
  begin
    if dlinfo(Pointer(FIBLibrary),RTLD_DI_ORIGIN,@path) = -1 then
      IBError(ibxeDLInfoError,[strpas(dlerror)]);
    Result := strpas(@path) + DirectorySeparator + Result;
  end;

end;


