unit TestManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTestBase }

  TTestBase = class
  public
    constructor Create;  virtual;
    function TestTitle: string; virtual; abstract;
    procedure RunTest; virtual; abstract;
  end;

  TTest = class of TTestBase;

  { TTestManager }

  TTestManager = class
  private
    FTests: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RunAll;
  end;

const
  TestMgr: TTestManager = nil;

procedure RegisterTest(aTest: TTest);

implementation

uses IB;

procedure RegisterTest(aTest: TTest);
begin
  if TestMgr = nil then
    TestMgr := TTestManager.Create;
  TestMgr.FTests.Add(aTest.Create);
end;

{ TTestBase }

constructor TTestBase.Create;
begin
  inherited Create;
end;

{ TTestManager }

constructor TTestManager.Create;
begin
  inherited Create;
  FTests := TList.Create;
end;

destructor TTestManager.Destroy;
var i: integer;
begin
  if assigned(FTests) then
  begin
    for i := 0 to FTests.Count - 1 do
      TObject(FTests[i]).Free;
    FTests.Free;
  end;
  inherited Destroy;
end;

procedure TTestManager.RunAll;
var i: integer;
begin
  if not TryIBLoad then
  begin
    writeln('Firebird Client Library not found');
    Exit;
  end;

  for i := 0 to FTests.Count - 1 do
    with TTestBase(FTests[i]) do
  begin
    writeln('Running ' + TestTitle);
    try
      RunTest;
    except on E:Exception do
      begin
        writeln('Test Completed with Error: ' + E.Message);
        Exit;
      end;
    end;
  end;
end;

end.

