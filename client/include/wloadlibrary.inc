function TFBLibrary.LoadIBLibrary: boolean;

  function IsValidHandle(aHandle: TLibHandle): boolean;
  begin
    Result := aHandle > HINSTANCE_ERROR;
  end;

  function DoLoadLibrary(LibName: string): TLibHandle;
  begin
    Result := LoadLibrary(PChar(LibName));
    if IsValidHandle(Result) then
      FFBLibraryName := ExtractFileName(LibName);
  end;

  function GetSpecialFolder(const CSIDL: integer) : string;
  {$IFDEF FPC}
  begin
    Result := GetWindowsSpecialDir(CSIDL);
  end;
  {$ELSE}
  var
    RecPath : PChar;
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

var curPath: string;

  procedure Add2Path(dirname: string);
  var newPath: string;
  begin
    newPath := dirname + ';' + curPath;
    SetEnvironmentVariable('PATH',PChar(newPath));
  end;

var InstallDir: string;
    dllPathName: string;
    oldFirebirdEV: string;
begin
  Result := IsValidHandle(FIBLibrary);
  if Result then Exit;

  curPath := GetEnvironmentVariable('PATH');

  {First try any user override}
  dllPathName := GetOverrideLibName;
  if dllPathName <> '' then
  begin
    FIBLibrary := DoLoadLibrary(dllPathName);
    Result := IsValidHandle(FIBLibrary);
    Exit;
  end;

  {Then look in application installation directory}
  InstallDir := ExtractFilePath(Paramstr(0)); {Using ParamStr(0) assumes windows conventions}

  //First look for Firebird Embedded Server in installation dir
  if FileExists(InstallDir + FIREBIRD_EMBEDDED) then
  begin
       dllPathName := InstallDir + FIREBIRD_EMBEDDED;
       FIBLibrary := DoLoadLibrary(dllPathName)
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
      FIBLibrary := DoLoadLibrary(dllPathName)
    finally
      if not IsValidHandle(FIBLibrary) then
        SetEnvironmentVariable('FIREBIRD',PChar(oldFirebirdEV)); {restore}
    end;
  end;

//  writeln('Dir = ',InstallDir);
  {If FIREBIRD environment variable available then try this}
  if not IsValidHandle(FIBLibrary) and (oldFirebirdEV <> '') then
  begin
    InstallDir := IncludeTrailingPathDelimiter(oldFirebirdEV);
    if (InstallDir <> '') and FileExists(InstallDir + FIREBIRD_CLIENT) then
    begin
      //assume firebird.conf and firebird.msg in same dir
      dllPathName := InstallDir + FIREBIRD_CLIENT;
      Add2Path(InstallDir);
      FIBLibrary := DoLoadLibrary(dllPathName)
    end
    else
    if (InstallDir <> '') then
    begin
      InstallDir := InstallDir + 'bin' + DirectorySeparator;
      if FileExists(InstallDir + FIREBIRD_CLIENT) then
      begin
        dllPathName := InstallDir +  FIREBIRD_CLIENT;
        Add2Path(InstallDir);
        FIBLibrary := DoLoadLibrary(dllPathName)
      end
    end
  end;

  if not IsValidHandle(FIBLibrary) then
  {Use Registry key if it exists to locate library Firebird 2 only}
  begin
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('SOFTWARE\Firebird Project\Firebird Server\Instances') then
      begin
        if ValueExists('DefaultInstance') then
        begin
          InstallDir := IncludeTrailingPathDelimiter(ReadString('DefaultInstance')) ;
          dllPathName := InstallDir + FIREBIRD_CLIENT;
          Add2Path(InstallDir);
          FIBLibrary := DoLoadLibrary(dllPathName);
          if not IsValidHandle(FIBLibrary) then
          begin
            InstallDir := InstallDir  + 'bin' + DirectorySeparator ;
            dllPathName := InstallDir + FIREBIRD_CLIENT;
            Add2Path(InstallDir);
            FIBLibrary := DoLoadLibrary(dllPathName)
          end;
        end
      end
    finally
      Free
    end;

    {Now try default install dir}
    if not IsValidHandle(FIBLibrary) then
    begin
      InstallDir := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_PROGRAM_FILES)) +
        'Firebird' +
        DirectorySeparator + 'Firebird_4_0' + DirectorySeparator;
      dllPathName := InstallDir  + FIREBIRD_CLIENT;
      Add2Path(InstallDir);
      FIBLibrary := DoLoadLibrary(dllPathName)
    end;

    if not IsValidHandle(FIBLibrary) then
    begin
      InstallDir := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_PROGRAM_FILES)) +
        'Firebird' +
        DirectorySeparator + 'Firebird_3_0' + DirectorySeparator;
      dllPathName := InstallDir  + FIREBIRD_CLIENT;
      Add2Path(InstallDir);
      FIBLibrary := DoLoadLibrary(dllPathName)
    end;

    if not IsValidHandle(FIBLibrary) then
    begin
      InstallDir := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_PROGRAM_FILES)) +
        'Firebird' +
        DirectorySeparator + 'Firebird_2_5' +
        DirectorySeparator + 'bin' + DirectorySeparator;
      dllPathName := InstallDir + FIREBIRD_CLIENT;
      Add2Path(InstallDir);
      FIBLibrary := DoLoadLibrary(dllPathName)
    end;

    if not IsValidHandle(FIBLibrary) then
    begin
      InstallDir := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_PROGRAM_FILES)) +
        'Firebird' +
        DirectorySeparator + 'Firebird_2_1' +
        DirectorySeparator + 'bin' + DirectorySeparator;
      dllPathName := InstallDir + FIREBIRD_CLIENT;
      Add2Path(InstallDir);
      FIBLibrary := DoLoadLibrary(dllPathName)
    end;

    //Otherwise see if Firebird client is in path
    //and rely on registry for location of firebird.conf and firebird.msg
    if not IsValidHandle(FIBLibrary) then
    begin
      SetEnvironmentVariable('PATH',PChar(curPath));
      FIBLibrary := DoLoadLibrary(FIREBIRD_CLIENT);
      if FIBLibrary <= HINSTANCE_ERROR then
       //well maybe InterBase is present...
       FIBLibrary := DoLoadLibrary(IBASE_DLL);
    end;
  end;
  Result := IsValidHandle(FIBLibrary);
end;

class procedure TFBLibrary.SetupEnvironment;
begin
end;

function TFBLibrary.GetLibraryFilePath: string;
var buffer: array [0..IBBigLocalBufferLength] of char;
    nSize: DWORD;
begin
  nSize := GetModuleFileNameA(FIBLibrary,@buffer,IBBigLocalBufferLength+1);
  if nSize > IBBigLocalBufferLength then
    buffer[IBBigLocalBufferLength] :=#0;
  Result := strpas(PAnsiChar(@buffer));
end;
