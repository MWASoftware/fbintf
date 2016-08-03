unit TestManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, IBExternals, IBHeader;

type
  TTestManager = class;

  { TTestBase }

  TTestBase = class
  private
    FOwner: TTestManager;
  protected
    procedure ReportResults(Statement: IStatement);
    procedure PrintDPB(DPB: IDPB);
    procedure PrintMetaData(meta: IMetaData);
    function SQLType2Text(sqltype: short): string;
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

procedure TTestBase.ReportResults(Statement: IStatement);
var ResultSet: IResultSet;
    i: integer;
begin
  ResultSet := Statement.OpenCursor;
  try
    while ResultSet.FetchNext do
    begin
      for i := 0 to ResultSet.getCount - 1 do
      begin
        if ResultSet[i].GetScale <> 0 then
          writeln( ResultSet[i].Name,' = ',FormatFloat('#,##0.00',ResultSet[i].AsFloat))
        else
          writeln(ResultSet[i].Name,' = ',ResultSet[i].AsString);
      end;
    end;
  finally
    ResultSet.Close;
  end;
end;

procedure TTestBase.PrintDPB(DPB: IDPB);
var i: integer;
begin
  writeln('DPB');
  writeln('Count = ', DPB.getCount);
  for i := 0 to DPB.getCount - 1 do
    writeln(DPB.Items[i].getParamType,' = ', DPB.Items[i].AsString);
end;

procedure TTestBase.PrintMetaData(meta: IMetaData);
var i: integer;
begin
  writeln('Metadata');
  for i := 0 to meta.GetCount - 1 do
  with meta[i] do
  begin
    writeln('SQLType =',SQLType2Text(getSQLType));
    writeln('sub type = ',getSubType);
    writeln('Table = ',getRelationName);
    writeln('Owner = ',getOwnerName);
    writeln('Column Name = ',getSQLName);
    writeln('Alias Name = ',getAliasName);
    writeln('Field Name = ',getName);
    writeln('Scale = ',getScale);
    writeln('Charset id = ',getCharSetID);
    if getIsNullable then writeln('Nullable') else writeln('Not Null');
    writeln('Size = ',GetSize);
    writeln;
  end;
end;

function TTestBase.SQLType2Text(sqltype: short): string;
begin
  Result := '';
  case sqltype of
  SQL_VARYING:	Result := 'SQL_VARYING';
  SQL_TEXT:		Result := 'SQL_TEXT';
  SQL_DOUBLE:		Result := 'SQL_DOUBLE';
  SQL_FLOAT:		Result := 'SQL_FLOAT';
  SQL_LONG:		Result := 'SQL_LONG';
  SQL_SHORT:		Result := 'SQL_SHORT';
  SQL_TIMESTAMP:	Result := 'SQL_TIMESTAMP';
  SQL_BLOB:		Result := 'SQL_BLOB';
  SQL_D_FLOAT:	Result := 'SQL_D_FLOAT';
  SQL_ARRAY:		Result := 'SQL_ARRAY';
  SQL_QUAD:		Result := 'SQL_QUAD';
  SQL_TYPE_TIME:	Result := 'SQL_TYPE_TIME';
  SQL_TYPE_DATE:	Result := 'SQL_TYPE_DATE';
  SQL_INT64:		Result := 'SQL_INT64';
  end;
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
    writeln;
    writeln;
  end;
end;

end.

