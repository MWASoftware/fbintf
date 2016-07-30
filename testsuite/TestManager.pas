unit TestManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTestManager = class;

  { TTestBase }

  TTestBase = class
  private
    FOwner: TTestManager;
  public
    constructor Create(aOwner: TTestManager);  virtual;
    function TestTitle: string; virtual; abstract;
    procedure RunTest(CharSet: string; SQLDialect: integer); virtual; abstract;
    property Owner: TTestManager read FOwner;
  end;

  TTest = class of TTestBase;

  { TTestManager }

  TTestManager = class
  private
    FTests: TList;
    FEmployeeDatabaseName: string;
    FNewDatabaseName: string;
    FUserName: string;
    FPassword: string;
 public
    constructor Create;
    destructor Destroy; override;
    function GetUserName: string;
    function GetPassword: string;
    function GetEmployeeDatabaseName: string;
    function GetNewDatabaseName: string;
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
  TestMgr.FTests.Add(aTest.Create(TestMgr));
end;

{ TTestBase }

constructor TTestBase.Create(aOwner: TTestManager);
begin
  inherited Create;
  FOwner := aOwner;
end;

{ TTestManager }

constructor TTestManager.Create;
begin
  inherited Create;
  FTests := TList.Create;
  FNewDatabaseName := 'localhost:/tmp/test1.fdb';
  FUserName := 'SYSDBA';
  FPassword := 'Pears';
  FEmployeeDatabaseName := 'localhost:employee';
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

function TTestManager.GetUserName: string;
begin
  Result := FUserName;
end;

function TTestManager.GetPassword: string;
begin
  Result := FPassword;
end;

function TTestManager.GetEmployeeDatabaseName: string;
begin
  Result := FEmployeeDatabaseName;
end;

function TTestManager.GetNewDatabaseName: string;
begin
  Result := FNewDatabaseName;
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
      RunTest('UTF8',3);
    except on E:Exception do
      begin
        writeln('Test Completed with Error: ' + E.Message);
        Exit;
      end;
    end;
  end;
end;

end.

