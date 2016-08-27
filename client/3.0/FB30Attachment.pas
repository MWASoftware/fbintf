unit FB30Attachment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FB30ClientAPI, Firebird, IB, FBActivityMonitor, FBParamBlock;

type

  { TFBAttachment }

  TFBAttachment = class(TActivityHandler,IAttachment, IActivityMonitor)
  private
    FAttachmentIntf: Firebird.IAttachment;
    FSQLDialect: integer;
    FFirebirdAPI: IFirebirdAPI;
    FDatabaseName: string;
    FDPB: IDPB;
    FRaiseExceptionOnConnectError: boolean;
    procedure CheckHandle;
  public
    constructor Create(DatabaseName: string; DPB: IDPB;
          RaiseExceptionOnConnectError: boolean);
    constructor CreateDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnError: boolean);
    destructor Destroy; override;
    property SQLDialect: integer read FSQLDialect;
    property AttachmentIntf: Firebird.IAttachment read FAttachmentIntf;

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

    {Events}
    function GetEventHandler(Events: TStrings): IEvents; overload;
    function GetEventHandler(Event: string): IEvents; overload;

    {Blob - may use to open existing Blobs. However, ISQLData.AsBlob is preferred}

    function CreateBlob(transaction: ITransaction): IBlob;
    function OpenBlob(Transaction: ITransaction; BlobID: TISC_QUAD): IBlob;

    {Array}
    function OpenArray(transaction: ITransaction; RelationName, ColumnName: string; ArrayID: TISC_QUAD): IArray;
    function CreateArray(transaction: ITransaction; RelationName, ColumnName: string): IArray;

    {Database Information}
    function GetSQLDialect: integer;
    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: string): IBlobMetaData;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: string): IArrayMetaData;
    function GetDBInformation(Requests: array of byte): IDBInformation; overload;
    function GetDBInformation(Request: byte): IDBInformation; overload;
  end;

implementation

uses FB30Transaction, FB30Statement, FB30Array, FB30Blob, FBMessages, FBOutputBlock;

{ TFBAttachment }

procedure TFBAttachment.CheckHandle;
begin
  if FAttachmentIntf = nil then
    IBError(ibxeDatabaseClosed,[nil]);
end;

constructor TFBAttachment.Create(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnConnectError: boolean);
begin
  inherited Create;
  FFirebirdAPI := Firebird30ClientAPI; {Keep reference to interface}
  FSQLDialect := 3;
  FDatabaseName := DatabaseName;
  FDPB := DPB;
  FRaiseExceptionOnConnectError := RaiseExceptionOnConnectError;
  Connect;
end;

constructor TFBAttachment.CreateDatabase(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnError: boolean);
begin
  FFirebirdAPI := Firebird30ClientAPI; {Keep reference to interface}
  FSQLDialect := 3;
  FDatabaseName := DatabaseName;
  FDPB := DPB;
  FRaiseExceptionOnConnectError := RaiseExceptionOnError;
  with Firebird30ClientAPI do
  begin
    FAttachmentIntf := ProviderIntf.createDatabase(StatusIntf,PAnsiChar(DatabaseName),
                                         (FDPB as TDPB).getDataLength,
                                         BytePtr((FDPB as TDPB).getBuffer));
    if FRaiseExceptionOnConnectError then Check4DataBaseError;
    if InErrorState then
      FAttachmentIntf := nil;
  end;
end;

destructor TFBAttachment.Destroy;
begin
  Disconnect(true);
  if assigned(FAttachmentIntf) then
    FAttachmentIntf.release;
  inherited Destroy;
end;

function TFBAttachment.getDPB: IDPB;
begin
  Result := FDPB;
end;

procedure TFBAttachment.Connect;
begin
  with Firebird30ClientAPI do
  begin
    FAttachmentIntf := ProviderIntf.attachDatabase(StatusIntf,PAnsiChar(FDatabaseName),
                         (FDPB as TDPB).getDataLength,
                         BytePtr((FDPB as TDPB).getBuffer));
    if FRaiseExceptionOnConnectError then Check4DataBaseError;
    if InErrorState then
      FAttachmentIntf := nil;
  end;
end;

procedure TFBAttachment.Disconnect(Force: boolean);
begin
  if IsConnected then
    with Firebird30ClientAPI do
    begin
      FAttachmentIntf.Detach(StatusIntf);
      if not Force and InErrorState then
        IBDataBaseError;
      FAttachmentIntf := nil;
    end;
end;

function TFBAttachment.IsConnected: boolean;
begin
  Result := FAttachmentIntf <> nil;
end;

procedure TFBAttachment.DropDatabase;
begin
  if IsConnected then
    with Firebird30ClientAPI do
    begin
      FAttachmentIntf.dropDatabase(StatusIntf);
      Check4DataBaseError;
      FAttachmentIntf := nil;
    end;
end;

function TFBAttachment.StartTransaction(TPB: array of byte;
  DefaultCompletion: TTransactionAction): ITransaction;
begin
  CheckHandle;
  Result := TFB30Transaction.Create(self,TPB,DefaultCompletion);
end;

function TFBAttachment.StartTransaction(TPB: ITPB;
  DefaultCompletion: TTransactionAction): ITransaction;
begin
  CheckHandle;
  Result := TFB30Transaction.Create(self,TPB,DefaultCompletion);
end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: string;
  aSQLDialect: integer);
begin
  CheckHandle;
  with Firebird30ClientAPI do
  begin
    FAttachmentIntf.executeDyn(StatusIntf,(transaction as TFB30Transaction).TransactionIntf,Length(sql),BytePtr(PChar(sql)));
    Check4DataBaseError;
  end;
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
  CheckHandle;
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
  CheckHandle;
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
  CheckHandle;
  Result := TFBStatement.CreateWithParameterNames(self,transaction,sql,aSQLDialect,
         GenerateParamNames,UniqueParamNames);
end;

function TFBAttachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; GenerateParamNames: boolean; UniqueParamNames: boolean
  ): IStatement;
begin
  CheckHandle;
  Result := TFBStatement.CreateWithParameterNames(self,transaction,sql,FSQLDialect,
         GenerateParamNames,UniqueParamNames);
end;

function TFBAttachment.GetEventHandler(Events: TStrings): IEvents;
begin

end;

function TFBAttachment.GetEventHandler(Event: string): IEvents;
begin

end;

function TFBAttachment.CreateBlob(transaction: ITransaction): IBlob;
begin
  CheckHandle;
  Result := TFBBLob.Create(self,transaction);
end;

function TFBAttachment.OpenBlob(Transaction: ITransaction; BlobID: TISC_QUAD
  ): IBlob;
begin
  CheckHandle;
  Result := TFBBlob.Create(self,Transaction,BlobID);
end;

function TFBAttachment.OpenArray(transaction: ITransaction; RelationName,
  ColumnName: string; ArrayID: TISC_QUAD): IArray;
begin
  CheckHandle;
  Result := TFB30Array.Create(self,transaction as TFB30Transaction,
                    GetArrayMetaData(transaction,RelationName,ColumnName),ArrayID);
end;

function TFBAttachment.CreateArray(transaction: ITransaction; RelationName,
  ColumnName: string): IArray;
begin
  CheckHandle;
  Result := TFB30Array.Create(self,transaction as TFB30Transaction,
                    GetArrayMetaData(transaction,RelationName,ColumnName));
end;

function TFBAttachment.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

function TFBAttachment.GetBlobMetaData(Transaction: ITransaction; tableName,
  columnName: string): IBlobMetaData;
begin
  CheckHandle;
  Result := TFBBlobMetaData.Create(self,Transaction as TFB30Transaction,tableName,columnName);
end;

function TFBAttachment.GetArrayMetaData(Transaction: ITransaction; tableName,
  columnName: string): IArrayMetaData;
begin
  CheckHandle;
  Result := TFB30ArrayMetaData.Create(self,Transaction as TFB30Transaction,tableName,columnName);
end;

function TFBAttachment.GetDBInformation(Requests: array of byte
  ): IDBInformation;
var ReqBuffer: PByte;
    i: integer;
begin
  CheckHandle;
  if Length(Requests) = 1 then
    Result := GetDBInformation(Requests[0])
  else
  begin
    Result := TDBInformation.Create;
    GetMem(ReqBuffer,Length(Requests));
    try
      for i := 0 to Length(Requests) - 1 do
        ReqBuffer[i] := Requests[i];

      with Firebird30ClientAPI, Result as TDBInformation do
      begin
        FAttachmentIntf.getInfo(StatusIntf, Length(Requests), BytePtr(ReqBuffer),
                                 getBufSize, BytePtr(Buffer));
          Check4DataBaseError;
      end

    finally
      FreeMem(ReqBuffer);
    end;
  end;
end;

function TFBAttachment.GetDBInformation(Request: byte): IDBInformation;
begin
  CheckHandle;
  Result := TDBInformation.Create;
  with Firebird30ClientAPI, Result as TDBInformation do
  begin
    FAttachmentIntf.getInfo(StatusIntf, 1, BytePtr(@Request),
                           getBufSize, BytePtr(Buffer));
      Check4DataBaseError;
  end;
end;

end.

