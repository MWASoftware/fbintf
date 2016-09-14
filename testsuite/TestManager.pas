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
    FHexStrings: boolean;
    function ReportResults(Statement: IStatement): IResultSet;
    procedure PrintHexString(s: string);
    procedure PrintDPB(DPB: IDPB);
    procedure PrintMetaData(meta: IMetaData);
    procedure WriteArray(ar: IArray);
    procedure WriteAffectedRows(Statement: IStatement);
    function WriteServiceQueryResult(QueryResult: IServiceQueryResults): boolean;
    procedure writeLicence(Item: IServiceQueryResultItem);
    procedure WriteConfig(config: IServiceQueryResultItem);
    procedure WriteUsers(users: IServiceQueryResultItem);
    procedure WriteDBAttachments(att: IServiceQueryResultItem);
    procedure WriteLimboTransactions(limbo: IServiceQueryResultItem);
    procedure WriteDBInfo(DBInfo: IDBInformation);
    procedure WriteBytes(Bytes: TByteArray);
    procedure WriteOperationCounts(Category: string; ops: TDBOperationCounts);
    procedure CheckActivity(Attachment: IAttachment); overload;
    procedure CheckActivity(Transaction: ITransaction); overload;
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
    procedure Run(TestID: integer);
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
    s: string;
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
          writeln( Result[i].Name,' = ',FormatFloat('#,##0.00',Result[i].AsFloat));

        SQL_INT64:
          if Result[i].Scale <> 0 then
            writeln( Result[i].Name,' = ',FormatFloat('#,##0.00',Result[i].AsFloat))
          else
            writeln(Result[i].Name,' = ',Result[i].AsString);

        SQL_BLOB:
          if Result[i].IsNull then
            writeln(Result[i].Name,' = (null blob)')
          else
          if Result[i].SQLSubType = 1 then
          begin
            s := Result[i].AsString;
            writeln(Result[i].Name,' (Charset Id = ',Result[i].GetCharSetID, ' Codepage = ',StringCodePage(s),')');
            writeln;
            writeln(s);
          end
          else
            writeln(Result[i].Name,' = (blob)');

        SQL_TEXT,SQL_VARYING:
        begin
          s := Result[i].AsString;
          if FHexStrings then
            PrintHexString(s);
          if Result[i].GetCharSetID > 0 then
            writeln(Result[i].Name,' = ',s,' (Charset Id = ',Result[i].GetCharSetID, ' Codepage = ',StringCodePage(s),')')
          else
            writeln(Result[i].Name,' = ',s);
        end;

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

procedure TTestBase.PrintHexString(s: string);
var i: integer;
begin
  for i := 1 to length(s) do
    write(Format('%x ',[byte(s[i])]));
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
    ar: IArrayMetaData;
    Bounds: TArrayBounds;
begin
  writeln('Metadata');
  for i := 0 to meta.GetCount - 1 do
  with meta[i] do
  begin
    writeln('SQLType =',GetSQLTypeName);
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
      SQL_ARRAY:
        begin
          writeln('Array Meta Data:');
          ar := GetArrayMetaData;
          writeln('SQLType =',ar.GetSQLTypeName);
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

procedure TTestBase.WriteArray(ar: IArray);
var Bounds: TArrayBounds;
    i,j: integer;
begin
  write('Array: ');
  Bounds := ar.GetMetaData.GetBounds;
  case ar.GetMetaData.GetDimensions of
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
var  SelectCount, InsertCount, UpdateCount, DeleteCount: integer;
begin
  Statement.GetRowsAffected(SelectCount, InsertCount, UpdateCount, DeleteCount);
  writeln('Select Count = ', SelectCount,' InsertCount = ',InsertCount,' UpdateCount = ', UpdateCount, ' DeleteCount = ',DeleteCount);
end;

function TTestBase.WriteServiceQueryResult(QueryResult: IServiceQueryResults): boolean;
var i: integer;
    line: string;
begin
  Result := true;
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
  isc_info_svc_get_licensed_users:
    writeln('Max Licenced Users = ',getAsInteger);
  isc_info_svc_get_users:
    WriteUsers(QueryResult[i]);
  isc_info_svc_svr_db_info:
    WriteDBAttachments(QueryResult[i]);
  isc_info_svc_line,
  isc_info_svc_to_eof:
    begin
      line := getAsString;
      writeln(line);
      Result := line <> '';
    end;
  isc_info_svc_running:
    writeln('Is Running = ',getAsInteger);
  isc_info_svc_limbo_trans:
    WriteLimboTransactions(QueryResult[i]);
  else
    writeln('Unknown Service Response Item', getItemType);
  end;
  writeln;
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
var i: integer;
begin
  writeln('Firebird Configuration File');
  for i := 0 to config.getCount - 1 do
    writeln('Key = ',config.getItemType,', Value = ',config.getAsInteger);
  writeln;
end;

procedure TTestBase.WriteUsers(users: IServiceQueryResultItem);
var i: integer;
begin
  writeln('Sec. Database User');
  for i := 0 to users.getCount - 1 do
  with users[i] do
  case getItemType of
    isc_spb_sec_username:
      writeln('User Name = ',getAsString);
    isc_spb_sec_firstname:
      writeln('First Name = ',getAsString);
    isc_spb_sec_middlename:
      writeln('Middle Name = ',getAsString);
    isc_spb_sec_lastname:
      writeln('Last Name = ',getAsString);
    isc_spb_sec_userid:
      writeln('User ID = ',getAsInteger);
    isc_spb_sec_groupid:
      writeln('Group ID = ',getAsInteger);
    else
      writeln('Unknown user info ', getItemType);
  end;
  writeln;
end;

procedure TTestBase.WriteDBAttachments(att: IServiceQueryResultItem);
var i: integer;
begin
  writeln('DB Attachments');
  for i := 0 to att.getCount - 1 do
  with att[i] do
  case getItemType of
  isc_spb_num_att:
    writeln('No. of Attachments = ',getAsInteger);
  isc_spb_num_db:
    writeln('Databases In Use = ',getAsInteger);
  isc_spb_dbname:
    writeln('DB Name = ',getAsString);
  end;
end;

procedure TTestBase.WriteLimboTransactions(limbo: IServiceQueryResultItem);
var i: integer;
begin
  writeln('Limbo Transactions');
  for i := 0 to limbo.getCount - 1 do
  with limbo[i] do
  case getItemType of
  isc_spb_single_tra_id:
    writeln('Single DB Transaction = ',getAsInteger);
  isc_spb_multi_tra_id:
    writeln('Multi DB Transaction = ',getAsInteger);
  isc_spb_tra_host_site:
    writeln('Host Name = ',getAsString);
  isc_spb_tra_advise:
    writeln('Resolution Advisory = ',getAsInteger);
  isc_spb_tra_remote_site:
    writeln('Server Name = ',getAsString);
  isc_spb_tra_db_path:
    writeln('DB Primary File Name = ',getAsString);
  isc_spb_tra_state:
    begin
      write('State = ');
      case getAsInteger of
        isc_spb_tra_state_limbo:
          writeln('limbo');
        isc_spb_tra_state_commit:
          writeln('commit');
        isc_spb_tra_state_rollback:
          writeln('rollback');
        isc_spb_tra_state_unknown:
          writeln('Unknown');
      end;
    end;
  end;
end;

procedure TTestBase.WriteDBInfo(DBInfo: IDBInformation);
var i, j: integer;
    bytes: TByteArray;
    ConType: integer;
    DBFileName: string;
    DBSiteName: string;
    Version: byte;
    VersionString: string;
    Users: TStringList;
begin
  for i := 0 to DBInfo.GetCount - 1 do
  with DBInfo[i] do
  case getItemType of
  isc_info_allocation:
    writeln('Pages =',getAsInteger);
  isc_info_base_level:
    begin
      bytes := getAsBytes;
      write('Base Level = ');
      WriteBytes(Bytes);
    end;
   isc_info_db_id:
     begin
       DecodeIDCluster(ConType,DBFileName,DBSiteName);
       writeln('Database ID = ', ConType,' FB = ', DBFileName, ' SN = ',DBSiteName);
     end;
   isc_info_implementation:
     begin
       bytes := getAsBytes;
       write('Implementation = ');
       WriteBytes(Bytes);
     end;
   isc_info_no_reserve:
     writeln('Reserved = ',getAsInteger);
   isc_info_ods_minor_version:
     writeln('ODS minor = ',getAsInteger);
   isc_info_ods_version:
     writeln('ODS major = ',getAsInteger);
   isc_info_page_size:
     writeln('Page Size = ',getAsInteger);
   isc_info_version:
     begin
       DecodeVersionString(Version,VersionString);
       writeln('Version = ',Version,': ',VersionString);
     end;
   isc_info_current_memory:
     writeln('Server Memory = ',getAsInteger);
   isc_info_forced_writes:
     writeln('Forced Writes  = ',getAsInteger);
   isc_info_max_memory:
     writeln('Max Memory  = ',getAsInteger);
   isc_info_num_buffers:
     writeln('Num Buffers  = ',getAsInteger);
   isc_info_sweep_interval:
     writeln('Sweep Interval  = ',getAsInteger);
   isc_info_user_names:
     begin
       Users := TStringList.Create;
       try
        write('Logged in Users: ');
        DecodeUserNames(Users);
        for j := 0 to Users.Count - 1 do
          write(Users[j],',');

       finally
         Users.Free;
       end;
       writeln;
     end;
   isc_info_fetches:
     writeln('Fetches  = ',getAsInteger);
   isc_info_marks:
     writeln('Writes  = ',getAsInteger);
   isc_info_reads:
     writeln('Reads  = ',getAsInteger);
   isc_info_writes:
     writeln('Page Writes  = ',getAsInteger);
   isc_info_backout_count:
     WriteOperationCounts('Record Version Removals',getOperationCounts);
   isc_info_delete_count:
     WriteOperationCounts('Deletes',getOperationCounts);
   isc_info_expunge_count:
     WriteOperationCounts('Expunge Count',getOperationCounts);
   isc_info_insert_count:
     WriteOperationCounts('Insert Count',getOperationCounts);
   isc_info_purge_count:
     WriteOperationCounts('Purge Count Countites',getOperationCounts);
   isc_info_read_idx_count:
     WriteOperationCounts('Indexed Reads Count',getOperationCounts);
   isc_info_read_seq_count:
     WriteOperationCounts('Sequential Table Scans',getOperationCounts);
   isc_info_update_count:
     WriteOperationCounts('Update Count',getOperationCounts);
   else
     writeln('Unknown Response ',getItemType);
  end;
end;

procedure TTestBase.WriteBytes(Bytes: TByteArray);
var i: integer;
begin
  for i := 0 to length(Bytes) - 1 do
    write(Bytes[i],',');
  writeln;
end;

procedure TTestBase.WriteOperationCounts(Category: string;
  ops: TDBOperationCounts);
var i: integer;
begin
  writeln(Category,' Operation Counts');
  for i := 0 to Length(ops) - 1 do
  begin
    writeln('Table ID = ',ops[i].TableID);
    writeln('Count = ',ops[i].Count);
  end;
  writeln;
end;

procedure TTestBase.CheckActivity(Attachment: IAttachment);
begin
    writeln('Database Activity = ',Attachment.HasActivity)
end;

procedure TTestBase.CheckActivity(Transaction: ITransaction);
begin
  writeln('Transaction Activity = ',Transaction.HasActivity)
end;

{ TTestManager }

constructor TTestManager.Create;
begin
  inherited Create;
  FTests := TList.Create;
  {$IFDEF WINDOWS}
  FNewDatabaseName := 'localhost:C:\test1.fdb';
  {$ELSE}
  FNewDatabaseName := 'localhost:/tmp/test1.fdb';
  {$ENDIF}
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

procedure TTestManager.Run(TestID: integer);
begin
  with TTestBase(FTests[TestID-1]) do
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

