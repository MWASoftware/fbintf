unit FB25Attachment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBTypes, FBLibrary, FB25ClientAPI, IBHeader,
  FB25ParamBlock;

type
  TDPB = class;

  { TDPBItem }

  TDPBItem = class(TParamBlockItem,IDPBItem)
  private
    FDPB: IDPB;
  public
    constructor Create(AOwner: TDPB; Data: PParamBlockItemData);
  end;

  { TDPB }

  TDPB = class(TParamBlock, IDPB)
    constructor Create;

  public
    {IDPB}
    function Add(ParamType: byte): IDPBItem;
    function Find(ParamType: byte): IDPBItem;
    function GetItems(index: integer): IDPBItem;
  end;

  { TFBAttachment }

  TFBAttachment = class(TAPIObject,IAttachment)
  private
    FHandle: TISC_DB_HANDLE;
    FDatabaseName: string;
    FDPB: IDPB;
    FSQLDialect: integer;
  public
    constructor Create(DatabaseName: string; DPB: IDPB);
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
    function CreateBlob(transaction: ITransaction): IBlob;
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
    function OpenBlob(Transaction: ITransaction; BlobID: TISC_QUAD): IBlob;

    {Database Information}
    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: string): IBlobMetaData;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: string): IArrayMetaData;
    function GetDBInformation(Requests: array of byte): IDBInformation;
  end;

implementation

uses FB25Events,FB25Transaction, FBErrorMessages, FB25Blob,
  FB25Statement, FB25DBInfo, FB25Array;

{ TDPBItem }

constructor TDPBItem.Create(AOwner: TDPB; Data: PParamBlockItemData);
begin
  inherited Create(AOwner,Data);
  FDPB := AOwner;
end;


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

constructor TFBAttachment.Create(DatabaseName: string; DPB: IDPB);
begin
  inherited Create;
  FSQLDialect := 3;
  Firebird25ClientAPI.RegisterObj(self);
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
  FSQLDialect := 3;
  Firebird25ClientAPI.RegisterObj(self);
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
      Call(isc_detach_database(StatusVector, @FHandle));
    Connect;
  end
end;

destructor TFBAttachment.Destroy;
begin
  Disconnect(true);
  Firebird25ClientAPI.UnRegisterObj(self);
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
  Call(isc_attach_database(StatusVector, Length(FDatabaseName),
                        PChar(FDatabaseName), @FHandle, 0, nil))
  else
  begin
   Call(isc_attach_database(StatusVector, Length(FDatabaseName),
                         PChar(FDatabaseName), @FHandle,
                         (FDPB as TDPB).getDataLength,
                         (FDPB as TDPB).getBuffer));

     Param := FDPB.Find(isc_dpb_set_db_SQL_dialect);
     if Param <> nil then
       FSQLDialect := Param.AsByte;
  end;
end;

procedure TFBAttachment.Disconnect(Force: boolean);
var i: integer;
begin
  if FHandle = nil then
    Exit;

  {Rollback or Cancel dependent objects}
  for i := 0 to OwnedObjects.Count - 1 do
    if (TObject(OwnedObjects[i]) is TFBTransaction) then
          TFBTransaction(OwnedObjects[i]).DoDefaultTransactionEnd(Force)
    else
    if TObject(OwnedObjects[i]) is TFBEvents then
      TFBEvents(OwnedObjects[i]).EndAttachment(self,Force);

  {Disconnect}
  with Firebird25ClientAPI do
    Call(isc_detach_database(StatusVector, @FHandle),not Force);
  FHandle := nil;
end;

function TFBAttachment.IsConnected: boolean;
begin
  Result := FHandle <> nil;
end;

procedure TFBAttachment.DropDatabase;
begin
  with Firebird25ClientAPI do
    Call(isc_drop_database(StatusVector, @FHandle));
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
    Call(isc_dsql_execute_immediate(StatusVector, @fHandle, @TRHandle, 0,PChar(sql), aSQLDialect, nil));
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

