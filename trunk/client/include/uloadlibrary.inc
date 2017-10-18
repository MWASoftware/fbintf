procedure TFBClientAPI.LoadIBLibrary;

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
  if IBLibrary <> NilHandle then Exit;

  LibName := GetOverrideLibName;
  if LibName = '' then
    LibName := GetFirebirdLibList;
  IBLibrary := FindLibrary(LibName);
  {$IFDEF DARWIN}
  if Result = NilHandle then
  begin
    {See http://paulbeachsblog.blogspot.co.uk/2008/03/where-is-libfbclientdylib-on-macosx.html
     Try loading direct from Firebird Framework}

    LibName := '/Library/Frameworks/Firebird.framework/Firebird';
    IBLibrary := LoadLibrary(LibName);
    if Result = NilHandle then
    begin
      LibName := '/Library/Frameworks/Firebird.framework/Libraries/libfbclient.dylib';
      IBLibrary := LoadLibrary(LibName);
    end;
    if IBLibrary <> NilHandle then
       FBLibraryName := ExtractFileName(LibName);
  end
  {$ENDIF}
  FOwnsIBLibrary := IBLibrary <> NilHandle;
end;




