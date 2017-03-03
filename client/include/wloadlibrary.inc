procedure TFBClientAPI.LoadIBLibrary;

  function IsValidHandle(aHandle: TLibHandle): boolean;
  {$IFDEF FPC}
  begin
    Result := aHandle <> nil;
  end;
  {$ELSE}
  begin
    Result := aHandle > HINSTANCE_ERROR;
  end;
  {$ENDIF}

  function DoLoadLibrary(LibName: string): TLibHandle;
  {$IFDEF MSWINDOWS}
  var w: widestring;
  begin
    w := LibName;
    Result := LoadLibrary(PChar(w));
  {$ELSE}}
  begin
    Result := LoadLibrary(PChar(LibName));
  {$ENDIF}
    if IsValidHandle(Result) then
    begin
      FFBLibraryName := ExtractFileName(LibName);
      FFBLibraryPath := ExtractFileDir(LibName);
    end;
  end;

  function GetSpecialFolder(const CSIDL: integer) : string;
  {$IFDEF FPC}
  begin
    Result := GetWindowsSpecialDir(CSIDL);
  end;
  {$ELSE}
  var
    RecPath : PWideChar;
  begin
    RecPath := StrAlloc(MAX_PATH);
    try
    FillChar(RecPath^, MAX_PATH, 0);
    if SHGetSpecialFolderPath(0, RecPath, CSIDL, false)
      then result := RecPath
      else result := '';
    finally
      StrDispose(RecPath);
    end;
  end;
  {$ENDIF}

var InstallDir: string;
    dllPathName: string;
    oldFirebirdEV: string;
begin
  if IsValidHandle(IBLibrary) then Exit;

  {First try any user override}
  dllPathName := GetOverrideLibName;
  if dllPathName <> '' then
  begin
    IBLibrary := DoLoadLibrary(dllPathName);
    FOwnsIBLibrary := IsValidHandle(IBLibrary);
    Exit;
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
    oldFirebirdEV := GetEnvironmentVariable('FIREBIRD');
    SetEnvironmentVariable('FIREBIRD',PChar(InstallDir));
    dllPathName := InstallDir + FIREBIRD_CLIENT;
    try
      IBLibrary := DoLoadLibrary(dllPathName)
    finally
      if not IsValidHandle(IBLibrary) then
        SetEnvironmentVariable('FIREBIRD',PChar(oldFirebirdEV)); {restore}
    end;
  end;

//  writeln('Dir = ',InstallDir);
  {If FIREBIRD environment variable available then try this}
  if not IsValidHandle(IBLibrary) then
  begin
    InstallDir := GetEnvironmentVariable('FIREBIRD');
    if (length(InstallDir) > 0) and (InstallDir[length(InstallDir)] <> DirectorySeparator) then
      InstallDir := InstallDir + DirectorySeparator;
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

  if not IsValidHandle(IBLibrary) then
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
    if not IsValidHandle(IBLibrary) then
    begin
      InstallDir := GetSpecialFolder(CSIDL_PROGRAM_FILES) +
        DirectorySeparator + 'Firebird' +
        DirectorySeparator + 'Firebird_3_0' +
        DirectorySeparator + 'bin' + DirectorySeparator;
      dllPathName := InstallDir + FIREBIRD_CLIENT;
      IBLibrary := DoLoadLibrary(dllPathName)
    end;

    if not IsValidHandle(IBLibrary) then
    begin
      InstallDir := GetSpecialFolder(CSIDL_PROGRAM_FILES) +
        DirectorySeparator + 'Firebird' +
        DirectorySeparator + 'Firebird_2_5' +
        DirectorySeparator + 'bin' + DirectorySeparator;
      dllPathName := InstallDir + FIREBIRD_CLIENT;
      IBLibrary := DoLoadLibrary(dllPathName)
    end;

    if not IsValidHandle(IBLibrary) then
    begin
      InstallDir := GetSpecialFolder(CSIDL_PROGRAM_FILES) +
        DirectorySeparator + 'Firebird' +
        DirectorySeparator + 'Firebird_2_1' +
        DirectorySeparator + 'bin' + DirectorySeparator;
      dllPathName := InstallDir + FIREBIRD_CLIENT;
      IBLibrary := DoLoadLibrary(dllPathName)
    end;

    //Otherwise see if Firebird client is in path
    //and rely on registry for location of firebird.conf and firebird.msg
    if not IsValidHandle(IBLibrary) then
    begin
      IBLibrary := DoLoadLibrary(FIREBIRD_CLIENT);
      if IBLibrary <= HINSTANCE_ERROR then
       //well maybe InterBase is present...
       IBLibrary := DoLoadLibrary(IBASE_DLL);
    end;
  end;
  FOwnsIBLibrary := IsValidHandle(IBLibrary);
end;


