program testsuite;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  IB,
  Test1 in 'Test1.pas',
  test2 in 'test2.pas',
  Test3 in 'Test3.pas',
  Test4 in 'Test4.pas',
  Test5 in 'Test5.pas',
  Test6 in 'Test6.pas',
  Test7 in 'Test7.pas',
  Test8 in 'Test8.pas',
  Test9 in 'Test9.pas',
  Test10 in 'Test10.pas',
  Test11 in 'Test11.pas',
  Test12 in 'Test12.pas',
  Test13 in 'Test13.pas',
  Test14 in 'Test14.pas',
  Test15 in 'Test15.pas',
  Test16 in 'Test16.pas',
  Test17 in 'Test17.pas',
  TestManager in 'TestManager.pas',
  FBOutputBlock in '..\client\FBOutputBlock.pas';

procedure WriteHelp;
begin
  { add your help code here }
  writeln(OutFile,'Usage: ', ParamStr(0), ' -h');
end;

function GetCmdLineValue(const Switch: string; var aValue: string): boolean;
var i: integer;
begin
  Result := FindCmdLineSwitch(Switch,false);
  if Result then
  begin
    for i := 0 to ParamCount do
      if (ParamStr(i) = '-' + Switch) and (i <= ParamCount) then
      begin
        aValue := ParamStr(i+1);
        exit;
      end;
    Result := false;
  end;
end;


var
  FTestID: integer;
  aValue: string;
begin
  try

  FTestID := 0;
  AssignFile(OutFile,'');
  ReWrite(outFile);

  
  // parse parameters
  if FindCmdLineSwitch('h') or FindCmdLineSwitch('help') then
  begin
    WriteHelp;
    Exit;
  end;

  if GetCmdLineValue('t',aValue) then
    FTestID := StrToInt(aValue);

  if TestMgr <> nil then
  begin
    if GetCmdLineValue('u',aValue) or GetCmdLineValue('user',aValue) then
      TestMgr.SetUserName(aValue);

    if GetCmdLineValue('p',aValue) or GetCmdLineValue('passwd',aValue) then
      TestMgr.SetPassword(aValue);

    if GetCmdLineValue('e',aValue) or GetCmdLineValue('employeedb',aValue) then
      TestMgr.SetEmployeeDatabaseName(aValue);

    if GetCmdLineValue('n',aValue) or GetCmdLineValue('newdbname',aValue) then
      TestMgr.SetNewDatabaseName(aValue);

    if GetCmdLineValue('s',aValue) or GetCmdLineValue('secondnewdbname',aValue) then
      TestMgr.SetSecondNewDatabaseName(aValue);

    if GetCmdLineValue('b',aValue) or GetCmdLineValue('backupfile',aValue) then
      TestMgr.SetBackupFileName(aValue);

    if GetCmdLineValue('o',aValue) or GetCmdLineValue('outfile',aValue) then
    begin
      system.Assign(outFile,aValue);
      ReWrite(outFile);
    end;

    TestMgr.ShowStatistics := FindCmdLineSwitch('S',false) or FindCmdLineSwitch('stats');

    {Ensure consistent date reporting across platforms}
    {$IF declared(FormatSettings)}
    FormatSettings.ShortDateFormat := 'd/m/yyyy';
    FormatSettings.LongTimeFormat := 'HH:MM:SS';
    FormatSettings.DateSeparator := '/';
    {$ELSE}
    ShortDateFormat := 'd/m/yyyy';
    LongTimeFormat := 'HH:MM:SS';
    DateSeparator := '/';
    {$IFEND}

    writeln(OutFile,'Firebird Client API Test Suite');
    writeln(OutFile,'Copyright MWA Software 2016');
    writeln(OutFile);
    writeln(OutFile,'Starting Tests');
    writeln(OutFile,'Client API Version = ',FirebirdAPI.GetImplementationVersion);

    if FTestID = 0 then
      TestMgr.RunAll
    else
      TestMgr.Run(FTestID);
    TestMgr.Free;
  end;

  writeln(OutFile,'Test Suite Ends');
  Flush(OutFile);
  //readln; {uncomment if running from IDE and console window closes before you can view results}

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
