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
  ErrorMsg := CheckOptions('ht', 'help');
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

  { add your program here }

  writeln('Starting Tests');

  if TestMgr <> nil then
  begin
    if FTestID = 0 then
      TestMgr.RunAll
    else
      TestMgr.Run(FTestID);
    TestMgr.Free;
  end;

  writeln('Test Suite Ends');
  {$IFDEF WINDOWS}
  readln;
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

