program testsuite;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, IB,
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
  TestManager in 'TestManager.pas';

procedure WriteHelp;
begin
  { add your help code here }
  writeln(OutFile,'Usage: ', ParamStr(0), ' -h');
end;


var
  ErrorMsg: AnsiString;
  FTestID: integer;
  aValue: string;
begin
  try

  AssignFile(OutFile,'');
  ReWrite(outFile);

  
  // parse parameters
  if FindCmdLineSwitch('h') or FindCmdLineSwitch('help') then
  begin
    WriteHelp;
    Exit;
  end;

  if FindCmdLineSwitch('t',aValue) then
    FTestID := StrToInt(aValue);

  if TestMgr <> nil then
  begin
    if FindCmdLineSwitch('u',aValue) or FindCmdLineSwitch('user',aValue) then
      TestMgr.SetUserName(aValue);

    if FindCmdLineSwitch('p',aValue) or FindCmdLineSwitch('passwd',aValue) then
      TestMgr.SetPassword(aValue);

    if FindCmdLineSwitch('e',aValue) or FindCmdLineSwitch('employeedb',aValue) then
      TestMgr.SetEmployeeDatabaseName(aValue);

    if FindCmdLineSwitch('n',aValue) or FindCmdLineSwitch('newdbname',aValue) then
      TestMgr.SetNewDatabaseName(aValue);

    if FindCmdLineSwitch('s',aValue) or FindCmdLineSwitch('secondnewdbname',aValue) then
      TestMgr.SetSecondNewDatabaseName(aValue);

    if FindCmdLineSwitch('b',aValue) or FindCmdLineSwitch('backupfile',aValue) then
      TestMgr.SetBackupFileName(aValue);

    if FindCmdLineSwitch('o',aValue) or FindCmdLineSwitch('outfile',aValue) then
    begin
      system.Assign(outFile,aValue);
      ReWrite(outFile);
    end;

    TestMgr.ShowStatistics := FindCmdLineSwitch('S') or FindCmdLineSwitch('stats');

    {Ensure consistent date reporting across platforms}
    FormatSettings.ShortDateFormat := 'd/m/yyyy';
    FormatSettings.DateSeparator := '/';

    writeln(OutFile,'Firebird Client API Test Suite');
    writeln(OutFile,'Copyright MWA Software 2017');
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
  readln; {uncomment if running from IDE and console window closes before you can view results}

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
