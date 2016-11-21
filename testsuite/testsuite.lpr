program testsuite;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, TestManager, Test1, test2, Test3, Test4, Test5,
  Test6, Test7, Test8, Test9, Test10, Test11, Test12, Test13, Test14, Test15,
  Test16;

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
  writeln(Title);
  writeln('Copyright MWA Software 2016');
  writeln;
  // quick check parameters
  ErrorMsg := CheckOptions('htupensb', 'help test user passwd employeedb newdbname secondnewdbname backupfile');
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

    writeln('Starting Tests');

    if FTestID = 0 then
      TestMgr.RunAll
    else
      TestMgr.Run(FTestID);
    TestMgr.Free;
  end;

  writeln('Test Suite Ends');
  Flush(stdout);
  {$IFDEF WINDOWS}
  //readln;
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
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TFBIntTestbed;
begin
  Application := TFBIntTestbed.Create(nil);
  Application.Title := 'Firebird API Test Suite';
  Application.Run;
  Application.Free;
end.

