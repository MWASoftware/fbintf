program testsuite;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
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
  writeln(OutFile,'Usage: ', ExeName, ' -h');
end;


var
  ErrorMsg: AnsiString;
  FTestID: integer;
begin
  try

  OutFile := stdout;
  // quick check parameters
  ErrorMsg := CheckOptions('htupensboS', 'help test user passwd employeedb newdbname secondnewdbname backupfile outfile stats');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('t') then
    FTestID := StrToInt(GetOptionValue('t'));

  if TestMgr <> nil then
  begin
    if HasOption('u','user') then
      TestMgr.SetUserName(GetOptionValue('u'));

    if HasOption('p','passwd') then
      TestMgr.SetPassword(GetOptionValue('p'));

    if HasOption('e','employeedb') then
      TestMgr.SetEmployeeDatabaseName(GetOptionValue('e'));

    if HasOption('n','newdbname') then
      TestMgr.SetNewDatabaseName(GetOptionValue('n'));

    if HasOption('s','secondnewdbname') then
      TestMgr.SetSecondNewDatabaseName(GetOptionValue('s'));

    if HasOption('b','backupfile') then
      TestMgr.SetBackupFileName(GetOptionValue('b'));

    if HasOption('o','outfile') then
    begin
      system.Assign(outFile,GetOptionValue('o'));
      ReWrite(outFile);
    end;

    TestMgr.ShowStatistics := HasOption('S','stats');

    {Ensure consistent date reporting across platforms}
    FormatSettings.ShortDateFormat := 'd/m/yyyy';
    FormatSettings.DateSeparator := '/';

    writeln(OutFile,Title);
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
  //readln; {uncomment if running from IDE and console window closes before you can view results}

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
