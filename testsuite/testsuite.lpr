program testsuite;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, TestManager, Test1, test2
  { you can add units after this };

type

  { TFBIntTestbed }

  TFBIntTestbed = class(TCustomApplication)
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
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
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

  { add your program here }

  writeln('Starting Tests');

  if TestMgr <> nil then
  begin
    TestMgr.RunAll;
    TestMgr.Free;
  end;

  writeln('Test Suite Ends');

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

