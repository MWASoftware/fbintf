program testsuite;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Test1 in 'Test1.pas',
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
  Test18 in 'Test18.pas',
  Test2 in 'Test2.pas',
  TestApplication in 'testApp\TestApplication.pas',
  FBTestApp in 'FBTestApp.pas',
  Test19 in 'Test19.pas',
  Test20 in 'Test20.pas';

var
  Application: TTestApplication;
begin
  Application := TTestApplication.Create(nil);
  Application.Title:='Firebird API Test Suite';
  Application.Run;
  Application.Free;
end.
