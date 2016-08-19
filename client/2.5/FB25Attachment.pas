unit FB25Attachment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB,  FBLibrary, FB25ClientAPI, IBHeader,
  FB25ParamBlock, FB25OutputBlock, FB25Status, FBActivityMonitor;

type
  TDPB = class;

  { TDPBItem }

  TDPBItem = class(TParamBlockItem,IDPBItem);

  { TDPB }

  TDPB = class(TParamBlock, IDPB)
  public
    constructor Create;

  public
    {IDPB}
    function Add(ParamType: byte): IDPBItem;
    function Find(ParamType: byte): IDPBItem;
    function GetItems(index: integer): IDPBItem;
  end;

  { TDBInfoItem }

  TDBInfoItem = class(TOutputBlockItemGroup,IDBInfoItem)
  public
    procedure DecodeIDCluster(var ConnectionType: integer; var DBFileName, DBSiteName: string);
    procedure DecodeVersionString(var Version: byte; var VersionString: string);
    procedure DecodeUserNames(UserNames: TStrings);
    function getOperationCounts: TDBOperationCounts;
    function GetItem(index: integer): IDBInfoItem;
    function Find(ItemType: byte): IDBInfoItem;
 end;

  TFBAttachment = class;

  { TDBInformation }

  TDBInformation = class(TOutputBlock,IDBInformation)
  protected
    function AddSpecialItem(BufPtr: PChar): POutputBlockItemData; override;
    procedure DoParseBuffer; override;
  public
    constructor Create(aAttachment: TFBAttachment; info_request: byte;
      aSize: integer=IBLocalBufferLength); overload;
    constructor Create(aAttachment: TFBAttachment; info_requests: array of byte;
      aSize: integer=IBLocalBufferLength); overload;

  public
    {IDBInformation}
    function GetItem(index: integer): IDBInfoItem;
    function Find(ItemType: byte): IDBInfoItem;
  end;

  { TFBAttachment }

  TFBAttachment = class(TInterfaceParent, IAttachment, IActivityMonitor)
  private
    FHandle: TISC_DB_HANDLE;
    FDatabaseName: string;
    FDPB: IDPB;
    FSQLDialect: integer;
    FFirebirdAPI: IFirebirdAPI;
    FRaiseExceptionOnConnectError: boolean;
    FActivity: boolean;
  public
    constructor Create(DatabaseName: string; DPB: IDPB;
      RaiseExceptionOnConnectError: boolean);
    constructor CreateDatabase(DatabaseName: string; SQLDialect: integer;
      CreateParams: string; DPB: IDPB);
    destructor Destroy; override;
    property Handle: TISC_DB_HANDLE read FHandle;
    property SQLDialect: integer read FSQLDialect;

  public
    {IAttachment}
    function getDPB: IDPB;
    procedure Connect;
    procedure Disconnect(Force: boolean=false);
    function IsConnected: boolean;
    procedure DropDatabase;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionAction): ITransaction; overload;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionAction): ITransaction; overload;
    procedure ExecImmediate(transaction: ITransaction; sql: string; aSQLDialect: integer); overload;
    procedure ExecImmediate(TPB: array of byte; sql: string; aSQLDialect: integer); overload;
    procedure ExecImmediate(transaction: ITransaction; sql: string); overload;
    procedure ExecImmediate(TPB: array of byte; sql: string); overload;
    function OpenCursor(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: string): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string): IResultSet; overload;
    function Prepare(transaction: ITransaction; sql: string; aSQLDialect: integer): IStatement; overload;
    function Prepare(transaction: ITransaction; sql: string): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       aSQLDialect: integer; GenerateParamNames: boolean=false;
                       UniqueParamNames: boolean=false): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       GenerateParamNames: boolean=false;
                       UniqueParamNames: boolean=false): IStatement; overload;
    function GetEventHandler(Events: TStrings): IEvents; overload;
    function GetEventHandler(Event: string): IEvents; overload;
    function CreateBlob(transaction: ITransaction): IBlob;
    function OpenBlob(Transaction: ITransaction; BlobID: TISC_QUAD): IBlob;

    function OpenArray(transaction: ITransaction; RelationName, ColumnName: string;
      ArrayID: TISC_QUAD): IArray;
    function CreateArray(transaction: ITransaction; RelationName, ColumnName: string
      ): IArray;

    {Database Information}

    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: string): IBlobMetaData;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: string): IArrayMetaData;
    function GetDBInformation(Requests: array of byte): IDBInformation;
    function HasActivity: boolean; {one shot - reset after call}

    {IActivityMonitor}
    procedure SignalActivity;
  end;

implementation

uses FB25Events,FB25Transaction, FBMessages, FB25Blob,
  FB25Statement, FB25Array;

{ TDBInfoItem }

procedure TDBInfoItem.DecodeIDCluster(var ConnectionType: integer;
  var DBFileName, DBSiteName: string);
var  P: PChar;
begin
  with ItemData^ do
  if FBufPtr^ = char(isc_info_db_id) then
  begin
    P := FBufPtr + 3;
    if FDataLength > 0 then
      ConnectionType := integer(P^);
    Inc(P);
    SetString(DBFileName,P+1,byte(P^),CP_ACP);
    P += Length(DBFileName) + 1;
    SetString(DBSiteName,P+1,byte(P^),CP_ACP);
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FBufPtr^)]);
end;

procedure TDBInfoItem.DecodeVersionString(var Version: byte;
  var VersionString: string);
var  P: PChar;
begin
  with ItemData^ do
  if FBufPtr^ = char(isc_info_version) then
  begin
   P := FBufPtr+3;
   VersionString := '';
   Version := byte(P^);
   Inc(P);
   SetString(VersionString,P+1,byte(P^),CP_ACP);
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FBufPtr^)]);
end;

procedure TDBInfoItem.DecodeUserNames(UserNames: TStrings);
var P: PChar;
    s: string;
begin
  with ItemData^ do
  if FBufPtr^ = char(isc_info_user_names) then
  begin
    P := FBufPtr+3;
    while (P < FBufPtr + FSize) do
    begin
      SetString(s,P+1,byte(P^),CP_ACP);
      UserNames.Add(s);
      P += Length(s) + 1;
    end;
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FBufPtr^)]);
end;

function TDBInfoItem.getOperationCounts: TDBOperationCounts;
var tableCounts: integer;
    P: PChar;
    i: integer;
begin
  with ItemData^ do
  if byte(FBufPtr^) in [isc_info_backout_count, isc_info_delete_count,
                              isc_info_expunge_count,isc_info_insert_count, isc_info_purge_count,
                              isc_info_read_idx_count, isc_info_read_seq_count, isc_info_update_count] then
  begin
    tableCounts := FDataLength div 6;
    SetLength(Result,TableCounts);
    P := FBufPtr + 3;
    for i := 0 to TableCounts -1 do
    with Firebird25ClientAPI do
    begin
      Result[i].TableID := isc_portable_integer(P,2);
      Inc(P,2);
      Result[i].Count := isc_portable_integer(P,4);
      Inc(P,4);
    end;
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FBufPtr^)]);
end;

function TDBInfoItem.GetItem(index: integer): IDBInfoItem;
var P: POutputBlockItemData;
begin
  P := inherited getItem(index);
  Result := TDBInfoItem.Create(self.Owner,P);
end;

function TDBInfoItem.Find(ItemType: byte): IDBInfoItem;
var P: POutputBlockItemData;
begin
  P := inherited Find(ItemType);
  Result := TDBInfoItem.Create(self.Owner,P);
end;

{ TDBInformation }

function TDBInformation.AddSpecialItem(BufPtr: PChar): POutputBlockItemData;
begin
  Result := inherited AddSpecialItem(BufPtr);
  with Result^ do
  begin
    with Firebird25ClientAPI do
      FDataLength := isc_portable_integer(FBufPtr+1,2);
    FSize := FDataLength + 3;
  end;
end;

procedure TDBInformation.DoParseBuffer;
var P: PChar;
    index: integer;
    len: integer;
begin
  P := Buffer;
  index := 0;
  SetLength(FItems,0);
  while (P^ <> char(isc_info_end)) and (P < Buffer + getBufSize) do
  begin
    SetLength(FItems,index+1);
    case byte(P^) of
    isc_info_no_reserve,
    isc_info_allocation,
    isc_info_ods_minor_version,
    isc_info_ods_version,
    isc_info_db_SQL_dialect,
    isc_info_page_size,
    isc_info_current_memory,
    isc_info_forced_writes,
    isc_info_max_memory,
    isc_info_num_buffers,
    isc_info_sweep_interval,
    isc_info_fetches,
    isc_info_marks,
    isc_info_reads,
    isc_info_writes:
      FItems[index] := AddIntegerItem(P);

    isc_info_implementation,
    isc_info_base_level:
      FItems[index] := AddBytesItem(P);

    isc_info_db_id,
    isc_info_version,
    isc_info_backout_count,
    isc_info_delete_count,
    isc_info_expunge_count,
    isc_info_insert_count,
    isc_info_purge_count,
    isc_info_read_idx_count,
    isc_info_read_seq_count,
    isc_info_update_count,
    isc_info_user_names:
      FItems[index] := AddSpecialItem(P);

    else
      FItems[index] := AddSpecialItem(P);
     end;
    P += FItems[index]^.FSize;
    Inc(index);
  end;
end;

constructor TDBInformation.Create(aAttachment: TFBAttachment;
  info_request: byte; aSize: integer);
begin
  inherited Create(aSize);
  FIntegerType := dtInteger;
  if aAttachment.Handle = nil then
    IBError(ibxeInvalidStatementHandle,[nil]);
  with Firebird25ClientAPI do
    if isc_database_info(StatusVector, @(aAttachment.Handle), 1, @info_request,
                           getBufSize, Buffer) > 0 then
      IBDataBaseError;
end;

constructor TDBInformation.Create(aAttachment: TFBAttachment;
  info_requests: array of byte; aSize: integer);
var ReqBuffer: PByte;
    i: integer;
begin
  if Length(info_requests) = 1 then
    Create(aAttachment,info_requests[0],aSize)
  else
  begin
    inherited Create(aSize);
    if aAttachment.Handle = nil then
      IBError(ibxeInvalidStatementHandle,[nil]);
    GetMem(ReqBuffer,Length(info_requests));
    try
      for i := 0 to Length(info_requests) - 1 do
        ReqBuffer[i] := info_requests[i];

      with Firebird25ClientAPI do
          if isc_database_info(StatusVector, @(aAttachment.Handle), Length(info_requests), PChar(ReqBuffer),
                                 getBufSize, Buffer) > 0 then
            IBDataBaseError;

    finally
      FreeMem(ReqBuffer);
    end;
  end;
  FIntegerType := dtInteger;
end;

function TDBInformation.GetItem(index: integer): IDBInfoItem;
var P: POutputBlockItemData;
begin
  P := inherited getItem(index);
  Result := TDBInfoItem.Create(self,P)
end;

function TDBInformation.Find(ItemType: byte): IDBInfoItem;
var P: POutputBlockItemData;
begin
  P := inherited Find(ItemType);
  Result := TDBInfoItem.Create(self,P)
end;

{ TDPBItem }


{ TDPB }

constructor TDPB.Create;
begin
  inherited Create;
  FDataLength := 1;
  FBuffer^ := char(isc_dpb_version1);
end;

function TDPB.Add(ParamType: byte): IDPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited Add(ParamType);
  Result := TDPBItem.Create(self,Item);
end;

function TDPB.Find(ParamType: byte): IDPBItem;
var Item: PParamBlockItemData;
begin
  Result := nil;
  Item := inherited Find(ParamType);
  if Item <> nil then
    Result := TDPBItem.Create(self,Item);
end;

function TDPB.getItems(index: integer): IDPBItem;
var Item: PParamBlockItemData;
begin
  Item := inherited getItems(index);
  Result := TDPBItem.Create(self,Item);
end;

  { TFBAttachment }

constructor TFBAttachment.Create(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnConnectError: boolean);
begin
  inherited Create;
  FFirebirdAPI := Firebird25ClientAPI; {Keep reference to interface}
  FSQLDialect := 3;
  FDatabaseName := DatabaseName;
  FDPB := DPB;
  Connect;
end;

constructor TFBAttachment.CreateDatabase(DatabaseName: string;
  SQLDialect: integer; CreateParams: string; DPB: IDPB);
var sql: string;
    tr_handle: TISC_TR_HANDLE;
begin
  inherited Create;
  FFirebirdAPI := Firebird25ClientAPI; {Keep reference to interface}
  FSQLDialect := 3;
  FDatabaseName := DatabaseName;
  FDPB := DPB;
  tr_handle := nil;
  sql := 'CREATE DATABASE ''' + DatabaseName + ''' ' + CreateParams; {do not localize}
  with Firebird25ClientAPI do
  if isc_dsql_execute_immediate(StatusVector, @FHandle, @tr_handle, 0, PChar(sql),
                                  SQLDialect, nil) > 0 then
    IBDataBaseError;

  if assigned(FDPB) and (FDPB.getCount > 0) then
  begin
    {If connect params specified then detach and connect properly}
    with Firebird25ClientAPI do
      if isc_detach_database(StatusVector, @FHandle) > 0 then
        IBDatabaseError;
    Connect;
  end
end;

destructor TFBAttachment.Destroy;
begin
  Disconnect(true);
  inherited Destroy;
end;

function TFBAttachment.getDPB: IDPB;
begin
  Result := FDPB;
end;

procedure TFBAttachment.Connect;
var Param: IDPBItem;
begin
  FSQLDialect := 3;

  with Firebird25ClientAPI do
  if FDPB = nil then
  begin
    if (isc_attach_database(StatusVector, Length(FDatabaseName),
                        PChar(FDatabaseName), @FHandle, 0, nil) > 0) and FRaiseExceptionOnConnectError then
      IBDatabaseError;
  end
  else
  begin
    if (isc_attach_database(StatusVector, Length(FDatabaseName),
                         PChar(FDatabaseName), @FHandle,
                         (FDPB as TDPB).getDataLength,
                         (FDPB as TDPB).getBuffer) > 0 ) and FRaiseExceptionOnConnectError then
      IBDatabaseError;

     Param := FDPB.Find(isc_dpb_set_db_SQL_dialect);
     if Param <> nil then
       FSQLDialect := Param.AsByte;
  end;
end;

procedure TFBAttachment.Disconnect(Force: boolean);
begin
  if FHandle = nil then
    Exit;

  {Disconnect}
  with Firebird25ClientAPI do
    if (isc_detach_database(StatusVector, @FHandle) > 0) and not Force then
      IBDatabaseError;
  FHandle := nil;
end;

function TFBAttachment.HasActivity: boolean;
begin
  Result := FActivity;
  FActivity := false;
end;

procedure TFBAttachment.SignalActivity;
begin
  FActivity := true;
end;

function TFBAttachment.IsConnected: boolean;
begin
  Result := FHandle <> nil;
end;

procedure TFBAttachment.DropDatabase;
begin
  with Firebird25ClientAPI do
    if isc_drop_database(StatusVector, @FHandle) > 0 then
      IBDatabaseError;
  FHandle := nil;
end;

function TFBAttachment.StartTransaction(TPB: array of byte;
  DefaultCompletion: TTransactionAction): ITransaction;
begin
  Result := TFBTransaction.Create(self,TPB,DefaultCompletion);
end;

function TFBAttachment.StartTransaction(TPB: ITPB;
  DefaultCompletion: TTransactionAction): ITransaction;
begin
  Result := TFBTransaction.Create(self,TPB,DefaultCompletion);
end;

function TFBAttachment.CreateBlob(transaction: ITransaction): IBlob;
begin
  Result := TFBBLob.Create(self,transaction);
end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: string;
  aSQLDialect: integer);
var TRHandle: TISC_TR_HANDLE;
begin
  TRHandle := (Transaction as TFBTransaction).Handle;
  with Firebird25ClientAPI do
    if isc_dsql_execute_immediate(StatusVector, @fHandle, @TRHandle, 0,PChar(sql), aSQLDialect, nil) > 0 then
      IBDatabaseError;
  SignalActivity;
end;

procedure TFBAttachment.ExecImmediate(TPB: array of byte; sql: string;
  aSQLDialect: integer);
begin
  ExecImmediate(StartTransaction(TPB,taCommit),sql,aSQLDialect);
end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: string);
begin
  ExecImmediate(transaction,sql,FSQLDialect);
end;

procedure TFBAttachment.ExecImmediate(TPB: array of byte; sql: string);
begin
  ExecImmediate(StartTransaction(TPB,taCommit),sql,FSQLDialect);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: string;
  aSQLDialect: integer): IResultSet;
var Statement: IStatement;
begin
  Statement := Prepare(transaction,sql,aSQLDialect);
  Result := Statement.OpenCursor;
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: string
  ): IResultSet;
begin
  Result := OpenCursor(transaction,sql,FSQLDialect);
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: string; aSQLDialect: integer): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect);
  Result.FetchNext;
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction; sql: string
  ): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect);
end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: string;
  aSQLDialect: integer): IStatement;
begin
  Result := TFBStatement.Create(self,transaction,sql,aSQLDialect);
end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: string
  ): IStatement;
begin
  Result := Prepare(transaction,sql,FSQLDialect);
end;

function TFBAttachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; aSQLDialect: integer; GenerateParamNames: boolean;
  UniqueParamNames: boolean): IStatement;
begin
  Result := TFBStatement.CreateWithParameterNames(self,transaction,sql,aSQLDialect,
         GenerateParamNames,UniqueParamNames);
end;

function TFBAttachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; GenerateParamNames: boolean; UniqueParamNames: boolean
  ): IStatement;
begin
  Result := TFBStatement.CreateWithParameterNames(self,transaction,sql,FSQLDialect,
         GenerateParamNames,UniqueParamNames);
end;

function TFBAttachment.GetEventHandler(Events: TStrings): IEvents;
begin
  Result := TFBEvents.Create(self,Events);
end;

function TFBAttachment.GetEventHandler(Event: string): IEvents;
var S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Add(Event);
    Result := GetEventHandler(S);
  finally
    S.Free;
  end;
end;

function TFBAttachment.OpenBlob(Transaction: ITransaction; BlobID: TISC_QUAD
  ): IBlob;
begin
  Result := TFBBlob.Create(self,Transaction,BlobID);
end;

function TFBAttachment.OpenArray(transaction: ITransaction; RelationName, ColumnName: string;
  ArrayID: TISC_QUAD): IArray;
begin
  Result := TFBArray.Create(self,transaction as TFBTransaction,RelationName,ColumnName,ArrayID);
end;

function TFBAttachment.CreateArray(transaction: ITransaction; RelationName, ColumnName: string): IArray;
begin
  Result := TFBArray.Create(self,transaction as TFBTransaction,RelationName,ColumnName);
end;

function TFBAttachment.GetBlobMetaData(Transaction: ITransaction; tableName,
  columnName: string): IBlobMetaData;
begin
  Result := TFBBlobMetaData.Create(self,Transaction as TFBTransaction,tableName,columnName);
end;

function TFBAttachment.GetArrayMetaData(Transaction: ITransaction; tableName,
  columnName: string): IArrayMetaData;
begin
  Result := TFBArrayMetaData.Create(self,Transaction as TFBTransaction,tableName,columnName);
end;

function TFBAttachment.GetDBInformation(Requests: array of byte
  ): IDBInformation;
begin
  Result := TDBInformation.Create(self,Requests);
end;

end.

