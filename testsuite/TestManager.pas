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
    function ReportResults(Statement: IStatement): IResultSet;
    procedure PrintDPB(DPB: IDPB);
    procedure PrintMetaData(meta: IMetaData);
    function SQLType2Text(sqltype: short): string;
    procedure WriteArray(ar: IArray);
    procedure WriteAffectedRows(Statement: IStatement);
    procedure WriteServiceQueryResult(QueryResult: IServiceQueryResults);
    procedure writeLicence(Item: IServiceQueryResultItem);
    procedure WriteConfig(config: IServiceQueryResultItem);
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

function TTestBase.ReportResults(Statement: IStatement): IResultSet;
var i: integer;
begin
  Result := Statement.OpenCursor;
  try
    while Result.FetchNext do
    begin
      for i := 0 to Result.getCount - 1 do
      begin
        case Result[i].SQLType of
        SQL_ARRAY:
          begin
            if not Result[i].IsNull then
              WriteArray(Result[i].AsArray);
          end;
        SQL_FLOAT,SQL_DOUBLE,
        SQL_D_FLOAT:
          writeln( Result[i].Name,' = ',FormatFloat('#,##0.00',Result[i].AsFloat))
        else
          writeln(Result[i].Name,' = ',Result[i].AsString);

        end;
      end;
    end;
  finally
    Result.Close;
  end;
  writeln;
end;

procedure TTestBase.PrintDPB(DPB: IDPB);
var i: integer;
begin
  writeln('DPB');
  writeln('Count = ', DPB.getCount);
  for i := 0 to DPB.getCount - 1 do
    writeln(DPB.Items[i].getParamType,' = ', DPB.Items[i].AsString);
  writeln;
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
        write('(',i,': ',ar.GetAsVariant([i]),') ');
    end;

  2:
    begin
      for i := Bounds[0].LowerBound to Bounds[0].UpperBound do
        for j := Bounds[1].LowerBound to Bounds[1].UpperBound do
          write('(',i,',',j,': ',ar.GetAsVariant([i,j]),') ');
    end;
  end;
  writeln;
end;

procedure TTestBase.WriteAffectedRows(Statement: IStatement);
var  InsertCount, UpdateCount, DeleteCount: integer;
begin
  Statement.GetRowsAffected(InsertCount, UpdateCount, DeleteCount);
  writeln('InsertCount = ',InsertCount,' UpdateCount = ', UpdateCount, ' DeleteCount = ',DeleteCount);
end;

procedure TTestBase.WriteServiceQueryResult(QueryResult: IServiceQueryResults);
var i: integer;
begin
  for i := 0 to QueryResult.GetCount - 1 do
  with QueryResult[i] do
  case getItemType of
  isc_info_svc_version:
    writeln('Service Manager Version = ',getAsInteger);
  isc_info_svc_server_version:
    writeln('Server Version = ',getAsString);
  isc_info_svc_implementation:
    writeln('Implementation = ',getAsString);
  isc_info_svc_get_license:
    writeLicence(QueryResult[i]);
  isc_info_svc_get_license_mask:
    writeln('Licence Mask = ',getAsInteger);
  isc_info_svc_capabilities:
    writeln('Capabilities = ',getAsInteger);
  isc_info_svc_get_config:
    WriteConfig(QueryResult[i]);
  isc_info_svc_get_env:
    writeln('Root Directory = ',getAsString);
  isc_info_svc_get_env_lock:
    writeln('Lock Directory = ',getAsString);
  isc_info_svc_get_env_msg:
    writeln('Message File = ',getAsString);
  isc_info_svc_user_dbpath:
    writeln('Security File = ',getAsString);
  else
    writeln('Unknown');
  end
end;

procedure TTestBase.writeLicence(Item: IServiceQueryResultItem);
var i: integer;
begin
  for i := 0 to Item.getCount - 1 do
  with Item[i] do
  case getItemType of
    isc_spb_lic_id:
      writeln('Licence ID = ',GetAsString);
    isc_spb_lic_key:
      writeln('Licence Key = ',GetAsString);
  end;
end;

procedure TTestBase.WriteConfig(config: IServiceQueryResultItem);
begin

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

