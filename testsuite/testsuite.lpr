program testsuite;

{$mode objfpc}{$H+}
{$codepage utf8}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, TestManager, Test1, test2, Test3, Test4, Test5,
  Test6, Test7, Test8, Test9, Test10, Test11, Test12, Test13, Test14, Test15,
  Test16, IB;

type

  { TFBIntTestbed }

  TFBIntTestbed = class(TCustomApplication)
  private
    FTestID: integer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TFBIntTestbed }

procedure TFBIntTestbed.DoRun;
var
  ErrorMsg: String;
begin
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
    DefaultFormatSettings.ShortDateFormat := 'd/m/yyyy';
    DefaultFormatSettings.DateSeparator := '/';

    writeln(OutFile,Title);
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
  {$IFDEF WINDOWS}
  //readln; {uncomment if running from IDE and console window closes before you can view results}
  {$ENDIF}

  // stop program loop
  Terminate;
end;

constructor TFBIntTestbed.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TFBIntTestbed.Destroy;
begin
  inherited Destroy;
end;

procedure TFBIntTestbed.WriteHelp;
begin
  { add your help code here }
  writeln(OutFile,'Usage: ', ExeName, ' -h');
end;

var
  Application: TFBIntTestbed;
begin
  Application := TFBIntTestbed.Create(nil);
  Application.Title := 'Firebird API Test Suite';
  Application.Run;
  Application.Free;
end.

