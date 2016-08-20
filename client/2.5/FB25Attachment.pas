unit FB25Attachment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB,  FBClientAPI, FB25ClientAPI, IBHeader,
  FBParamBlock, FBOutputBlock, FB25Status, FBActivityMonitor;

type
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
    constructor CreateDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnError: boolean);
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
    function GetDBInformation(Requests: array of byte): IDBInformation; overload;
    function GetDBInformation(Request: byte): IDBInformation; overload;
    function HasActivity: boolean; {one shot - reset after call}

    {IActivityMonitor}
    procedure SignalActivity;
  end;

implementation

uses FB25Events,FB25Transaction, FBMessages, FB25Blob,
  FB25Statement, FB25Array;

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

constructor TFBAttachment.CreateDatabase(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnError: boolean);
var sql: string;
    tr_handle: TISC_TR_HANDLE;
    CreateParams: string;
    DPBItem: IDPBItem;
begin
  inherited Create;
  FFirebirdAPI := Firebird25ClientAPI; {Keep reference to interface}
  CreateParams := '';
  FSQLDialect := 3;

  if DPB <> nil then
  begin
    DPBItem :=  DPB.Find(isc_dpb_user_name);
    if DPBItem <> nil then
      CreateParams += ' USER ''' + DPBItem.AsString + '''';

    DPBItem :=  DPB.Find(isc_dpb_password);
    if DPBItem <> nil then
      CreateParams += ' Password ''' + DPBItem.AsString + '''';

    DPBItem :=  DPB.Find(isc_dpb_page_size);
    if DPBItem <> nil then
      CreateParams += ' PAGE_SIZE ' + DPBItem.AsString;

    DPBItem :=  DPB.Find(isc_dpb_lc_ctype);
    if DPBItem <> nil then
      CreateParams += ' DEFAULT CHARACTER SET ' + DPBItem.AsString;

    DPBItem :=  DPB.Find(isc_dpb_sql_dialect);
    if DPBItem <> nil then
      FSQLDialect := DPBItem.AsInteger;
  end;

  FDatabaseName := DatabaseName;
  FDPB := DPB;
  tr_handle := nil;
  sql := 'CREATE DATABASE ''' + DatabaseName + ''' ' + CreateParams; {do not localize}
  with Firebird25ClientAPI do
  if (isc_dsql_execute_immediate(StatusVector, @FHandle, @tr_handle, 0, PChar(sql),
                                  SQLDialect, nil) > 0) and RaiseExceptionOnError then
    IBDataBaseError;
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
var ReqBuffer: PByte;
    i: integer;
begin
  if Length(Requests) = 1 then
    Result := GetDBInformation(Requests[0])
  else
  begin
    Result := TDBInformation.Create;
    GetMem(ReqBuffer,Length(Requests));
    try
      for i := 0 to Length(Requests) - 1 do
        ReqBuffer[i] := Requests[i];

      with Firebird25ClientAPI, Result as TDBInformation do
          if isc_database_info(StatusVector, @(FHandle), Length(Requests), PChar(ReqBuffer),
                                 getBufSize, Buffer) > 0 then
            IBDataBaseError;

    finally
      FreeMem(ReqBuffer);
    end;
  end;
end;

function TFBAttachment.GetDBInformation(Request: byte): IDBInformation;
begin
  Result := TDBInformation.Create;
  with Firebird25ClientAPI, Result as TDBInformation do
    if isc_database_info(StatusVector, @(FHandle), 1, @Request,
                           getBufSize, Buffer) > 0 then
      IBDataBaseError;
end;

end.

