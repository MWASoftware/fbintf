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

{ TFBAttachment }

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
    FAttachmentIntf := MasterIntf.getDispatcher.attachDatabase(StatusIntf,PAnsiChar(FDatabaseName),
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

end;

function TFBAttachment.StartTransaction(TPB: ITPB;
  DefaultCompletion: TTransactionAction): ITransaction;
begin

end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: string;
  aSQLDialect: integer);
begin

end;

procedure TFBAttachment.ExecImmediate(TPB: array of byte; sql: string;
  aSQLDialect: integer);
begin

end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: string);
begin

end;

procedure TFBAttachment.ExecImmediate(TPB: array of byte; sql: string);
begin

end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: string;
  aSQLDialect: integer): IResultSet;
begin

end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: string
  ): IResultSet;
begin

end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: string; aSQLDialect: integer): IResultSet;
begin

end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction; sql: string
  ): IResultSet;
begin

end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: string;
  aSQLDialect: integer): IStatement;
begin

end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: string
  ): IStatement;
begin

end;

function TFBAttachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; aSQLDialect: integer; GenerateParamNames: boolean;
  UniqueParamNames: boolean): IStatement;
begin

end;

function TFBAttachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; GenerateParamNames: boolean; UniqueParamNames: boolean
  ): IStatement;
begin

end;

function TFBAttachment.GetEventHandler(Events: TStrings): IEvents;
begin

end;

function TFBAttachment.GetEventHandler(Event: string): IEvents;
begin

end;

function TFBAttachment.CreateBlob(transaction: ITransaction): IBlob;
begin

end;

function TFBAttachment.OpenBlob(Transaction: ITransaction; BlobID: TISC_QUAD
  ): IBlob;
begin

end;

function TFBAttachment.OpenArray(transaction: ITransaction; RelationName,
  ColumnName: string; ArrayID: TISC_QUAD): IArray;
begin

end;

function TFBAttachment.CreateArray(transaction: ITransaction; RelationName,
  ColumnName: string): IArray;
begin

end;

function TFBAttachment.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

function TFBAttachment.GetBlobMetaData(Transaction: ITransaction; tableName,
  columnName: string): IBlobMetaData;
begin

end;

function TFBAttachment.GetArrayMetaData(Transaction: ITransaction; tableName,
  columnName: string): IArrayMetaData;
begin

end;

function TFBAttachment.GetDBInformation(Requests: array of byte
  ): IDBInformation;
begin

end;

function TFBAttachment.GetDBInformation(Request: byte): IDBInformation;
begin

end;

end.

