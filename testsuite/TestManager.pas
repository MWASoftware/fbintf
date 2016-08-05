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
    procedure WriteArray(ar: IArray);
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
        case ResultSet[i].SQLType of
        SQL_ARRAY:
          begin
            if not ResultSet[i].IsNull then
              WriteArray(ResultSet[i].AsArray);
          end;
        SQL_FLOAT,SQL_DOUBLE,
        SQL_D_FLOAT:
          writeln( ResultSet[i].Name,' = ',FormatFloat('#,##0.00',ResultSet[i].AsFloat))
        else
          writeln(ResultSet[i].Name,' = ',ResultSet[i].AsString);

        end;
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
var i, j: integer;
    b: IBlobMetaData;
    ar: IArrayMetaData;
    Bounds: TArrayBounds;
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
    case getSQLType of
      SQL_BLOB:
        begin
          writeln('Blob Meta Data:');
          b := GetBlobMetaData;
          writeln('Sub Type = ',b.GetSubType);
          writeln('CharSetID = ',b.GetCharSetID);
          writeln('Segment Size = ',b.GetSegmentSize);
          writeln('Table = ',b.GetTableName);
          writeln('Column = ',b.GetColumnName);
        end;
      SQL_ARRAY:
        begin
          writeln('Array Meta Data:');
          ar := GetArrayMetaData;
          writeln('SQLType =',SQLType2Text(ar.getSQLType));
          writeln('Table = ',ar.GetTableName);
          writeln('Column = ',ar.GetColumnName);
          writeln('Dimensions = ',ar.GetDimensions);
          write('Bounds: ');
          Bounds := ar.GetBounds;
          for j := 0 to Length(Bounds) - 1 do
            write('(',Bounds[j].LowerBound,':',Bounds[j].UpperBound,') ');
          writeln;
        end;
    end;
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

procedure TTestBase.WriteArray(ar: IArray);
var Bounds: TArrayBounds;
    i,j: integer;
begin
  write('Array: ');
  Bounds := ar.GetBounds;
  case ar.GetDimensions of
  1:
    begin
      for i := Bounds[0].LowerBound to Bounds[0].UpperBound do
        write('(',i,': ',ar[i].AsVariant,') ');
    end;

  2:
    begin
      for i := Bounds[0].LowerBound to Bounds[0].UpperBound do
        for j := Bounds[1].LowerBound to Bounds[1].UpperBound do
          write('(',i,',',j,': ',ar.GetElement(i,j).AsVariant,') ');
    end;
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

