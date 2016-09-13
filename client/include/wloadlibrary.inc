procedure TFBClientAPI.LoadIBLibrary;

  function DoLoadLibrary(LibName: string): TLibHandle;
  begin
    Result := LoadLibrary(PChar(LibName));
    if Result <> NilHandle then
      FFBLibraryName := ExtractFileName(LibName);
  end;

var InstallDir: string;
    dllPathName: string;
begin
  IBLibrary := NilHandle;
  {If OnGetLibraryName given then use this}
  if assigned(OnGetLibraryName) then
  begin
    OnGetLibraryName(dllPathName);
    IBLibrary := DoLoadLibrary(dllPathName);
    Exit
  end;

  {Then look in application installation directory}
  InstallDir := ExtractFilePath(Paramstr(0)); {Using ParamStr(0) assumes windows conventions}

  //First look for Firebird Embedded Server in installation dir
  if FileExists(InstallDir + FIREBIRD_EMBEDDED) then
  begin
       dllPathName := InstallDir + FIREBIRD_EMBEDDED;
       IBLibrary := DoLoadLibrary(dllPathName)
  end
  else
  //Otherwise look for Firebird Client in installation dir
  if FileExists(InstallDir + FIREBIRD_CLIENT) then
  begin
    //assume firebird.conf and firebird.msg in same dir
    SetEnvironmentVariable('FIREBIRD',PChar(InstallDir));
    dllPathName := InstallDir +FIREBIRD_CLIENT;
    IBLibrary := DoLoadLibrary(dllPathName)
  end;

  {If FIREBIRD environment variable available then try this}
  if IBLibrary = NilHandle then
  begin
    InstallDir := GetEnvironmentVariable('FIREBIRD');
    if (InstallDir <> '') and FileExists(InstallDir + FIREBIRD_CLIENT) then
    begin
      //assume firebird.conf and firebird.msg in same dir
      dllPathName := InstallDir + FIREBIRD_CLIENT;
      IBLibrary := DoLoadLibrary(dllPathName)
    end
    else
    if (InstallDir <> '') and FileExists(InstallDir + 'bin' + DirectorySeparator + FIREBIRD_CLIENT) then
    begin
      dllPathName := InstallDir + FIREBIRD_CLIENT;
      IBLibrary := DoLoadLibrary(dllPathName)
    end
  end;

  if IBLibrary = NilHandle then
  {Use Registry key if it exists to locate library}
  begin
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\Firebird Project\Firebird Server\Instances',false) then
      begin
        if ValueExists('DefaultInstance') then
        begin
          InstallDir := ReadString('DefaultInstance')  + 'bin' + DirectorySeparator ;
          dllPathName := InstallDir + FIREBIRD_CLIENT;
          IBLibrary := DoLoadLibrary(dllPathName)
        end
      end
    finally
      Free
    end;

    {Now try default install dir}
    if IBLibrary = NilHandle then
    begin
      InstallDir := GetWindowsSpecialDir(CSIDL_PROGRAM_FILES) +
        DirectorySeparator + 'Firebird' +
        DirectorySeparator + 'Firebird_2_5' +
        DirectorySeparator + 'bin' + DirectorySeparator;
      dllPathName := InstallDir + FIREBIRD_CLIENT;
      IBLibrary := DoLoadLibrary(dllPathName)
    end;

    //Otherwise see if Firebird client is in path
    //and rely on registry for location of firebird.conf and firebird.msg
    if IBLibrary = NilHandle then
    begin
      IBLibrary := DoLoadLibrary(FIREBIRD_CLIENT);
      if IBLibrary <= HINSTANCE_ERROR then
       //well maybe InterBase is present...
       IBLibrary := DoLoadLibrary(IBASE_DLL);
    end;
  end
end;

function TFBClientAPI.IsEmbeddedServer: boolean;
begin
 Result := CompareText(FFBLibraryName,FIREBIRD_EMBEDDED) = 0;
end;

