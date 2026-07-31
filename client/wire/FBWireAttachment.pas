(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  This file is part of the pure Pascal wire protocol implementation
 *  (no fbclient required) and is subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBWireAttachment;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBAttachment, FBClientAPI, FBActivityMonitor,
  FBOutputBlock, FBParamBlock, FBWireClientAPI, FBWireProtocol, FBWireConst;

type
  { TFBWireAttachment }

  TFBWireAttachment = class(TFBAttachment,IAttachment,IActivityMonitor)
  private
    FWireAPI: TFBWireClientAPI;
    FConnection: TFBWireConnection;
    FHandle: integer;
    FIsConnected: boolean;
    FHost: AnsiString;
    FPort: integer;
    FRemoteDatabaseName: AnsiString;
    procedure ParseDatabaseName(const aDatabaseName: AnsiString);
    {opens the TCP connection, authenticates and returns the DPB to send
     with op_attach/op_create (the authentication proof is added to it)}
    function ConnectAndPrepareDPB: TBytes;
    procedure CreateDatabaseFromDPB(RaiseExceptionOnError: boolean);
  protected
    procedure CheckHandle; override;
    function GetAttachment: IAttachment; override;
  public
    constructor Create(api: TFBWireClientAPI; DatabaseName: AnsiString;
                aDPB: IDPB; RaiseExceptionOnConnectError: boolean); overload;
    constructor CreateDatabase(api: TFBWireClientAPI; DatabaseName: AnsiString;
                aDPB: IDPB; RaiseExceptionOnError: boolean); overload;
    constructor CreateDatabase(api: TFBWireClientAPI; sql: AnsiString;
                aSQLDialect: integer; RaiseExceptionOnError: boolean); overload;
    destructor Destroy; override;
    function GetDBInfo(ReqBuffer: PByte; ReqBufLen: integer): IDBInformation; override;

    property Connection: TFBWireConnection read FConnection;
    property Handle: integer read FHandle;
    property WireAPI: TFBWireClientAPI read FWireAPI;

  public
    {IAttachment}
    procedure Connect;
    procedure Disconnect(Force: boolean = false); override;
    function IsConnected: boolean; override;
    procedure DropDatabase; override;
    function StartTransaction(TPB: array of byte;
                DefaultCompletion: TTransactionCompletion;
                aName: AnsiString = ''): ITransaction; override;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionCompletion;
                aName: AnsiString = ''): ITransaction; override;
    procedure ExecImmediate(transaction: ITransaction; sql: AnsiString;
                aSQLDialect: integer); override;
    function Prepare(transaction: ITransaction; sql: AnsiString;
                aSQLDialect: integer; CursorName: AnsiString = ''): IStatement; override;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: AnsiString;
                aSQLDialect: integer; GenerateParamNames: boolean = false;
                CaseSensitiveParams: boolean = false;
                CursorName: AnsiString = ''): IStatement; override;

    {Events - not implemented by this provider: they need the auxiliary
     connection established with op_connect_request}
    function GetEventHandler(Events: TStrings): IEvents; override;

    {Blobs}
    function CreateBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData;
                BPB: IBPB = nil): IBlob; overload; override;
    function CreateBlob(transaction: ITransaction; SubType: integer;
                aCharSetID: cardinal = 0; BPB: IBPB = nil): IBlob; overload;
    function OpenBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData;
                BlobID: TISC_QUAD; BPB: IBPB = nil): IBlob; overload; override;

    {Arrays - not implemented by this provider}
    function OpenArray(transaction: ITransaction; ArrayMetaData: IArrayMetaData;
                ArrayID: TISC_QUAD): IArray; overload; override;
    function CreateArray(transaction: ITransaction;
                ArrayMetaData: IArrayMetaData): IArray; overload; override;
    function CreateArrayMetaData(SQLType: cardinal; tableName: AnsiString;
                columnName: AnsiString; Scale: integer; size: cardinal;
                aCharSetID: cardinal; dimensions: cardinal;
                bounds: TArrayBounds): IArrayMetaData;

    {Metadata}
    function GetBlobMetaData(Transaction: ITransaction;
                tableName, columnName: AnsiString): IBlobMetaData; override;
    function GetArrayMetaData(Transaction: ITransaction;
                tableName, columnName: AnsiString): IArrayMetaData; override;
    function HasDecFloatSupport: boolean; override;
    function HasTimeZoneSupport: boolean; override;
    function HasBatchMode: boolean; override;
    function HasScollableCursors: boolean;
    procedure getFBVersion(version: TStrings);
  end;

implementation

uses FBMessages, IBErrorCodes, IBUtils, FBWireTransaction, FBWireStatement,
  FBWireBlob, FBWireStream;

{ TFBWireAttachment }

procedure TFBWireAttachment.ParseDatabaseName(const aDatabaseName: AnsiString);
var aProtocol: TProtocolAll;
    aPortNo: AnsiString;
begin
  FHost := '';
  FPort := 3050;
  FRemoteDatabaseName := aDatabaseName;
  if ParseConnectString(aDatabaseName,FHost,FRemoteDatabaseName,aProtocol,aPortNo) then
  begin
    if aPortNo <> '' then
      FPort := StrToIntDef(aPortNo,3050);
    case aProtocol of
    TCP, inet, inet4, inet6:
      {a remote connection - this is what the wire protocol provides};
    Local:
      {a local connection cannot be made without the client library, but
       most servers also listen on the loopback interface}
      if FHost = '' then
        FHost := 'localhost';
    else
      IBError(ibxeNotSupported,[nil]);
    end;
  end;
  if FHost = '' then
    FHost := 'localhost';
end;

function TFBWireAttachment.ConnectAndPrepareDPB: TBytes;
var aUser, aPassword: AnsiString;
    UserItem, PasswordItem, Item, NewItem: IDPBItem;
    NewDPB: IDPB;
    i: integer;
begin
  aUser := '';
  aPassword := '';
  UserItem := DPB.Find(isc_dpb_user_name);
  if UserItem <> nil then
    aUser := UserItem.AsString;
  PasswordItem := DPB.Find(isc_dpb_password);
  if PasswordItem <> nil then
    aPassword := PasswordItem.AsString;

  FConnection.ConnectTo(FHost,FPort,FRemoteDatabaseName,aUser,aPassword);

  {The plain text password must not travel to the server: SRP has already
   proved knowledge of it. Rebuild the DPB without the password, adding the
   authentication proof where the server asked for it in the attach (the
   op_accept_data flow).}
  NewDPB := TDPB.Create(FWireAPI);
  for i := 0 to DPB.Count - 1 do
  begin
    Item := DPB.Items[i];
    if Item.getParamType in [isc_dpb_password,isc_dpb_password_enc] then
      continue;
    NewItem := NewDPB.Add(Item.getParamType);
    if Item.getParamType in [isc_dpb_sql_dialect,isc_dpb_num_buffers,
                             isc_dpb_dbkey_scope,isc_dpb_force_write,
                             isc_dpb_no_reserve,isc_dpb_page_size] then
      NewItem.SetAsInteger(Item.AsInteger)
    else
      NewItem.SetAsString(Item.AsString);
  end;
  if FConnection.AuthData <> '' then
  begin
    NewDPB.Add(isc_dpb_specific_auth_data).AsString := FConnection.AuthData;
    NewDPB.Add(isc_dpb_auth_plugin_name).AsString := FConnection.AuthPluginName;
  end;

  Result := ParamBlockToBytes(NewDPB);
end;

constructor TFBWireAttachment.Create(api: TFBWireClientAPI;
  DatabaseName: AnsiString; aDPB: IDPB; RaiseExceptionOnConnectError: boolean);
begin
  FWireAPI := api;
  inherited Create(api,DatabaseName,aDPB,RaiseExceptionOnConnectError);
  FConnection := TFBWireConnection.Create;
  ParseDatabaseName(DatabaseName);
  Connect;
end;

constructor TFBWireAttachment.CreateDatabase(api: TFBWireClientAPI;
  DatabaseName: AnsiString; aDPB: IDPB; RaiseExceptionOnError: boolean);
begin
  FWireAPI := api;
  inherited Create(api,DatabaseName,aDPB,RaiseExceptionOnError);
  FConnection := TFBWireConnection.Create;
  ParseDatabaseName(DatabaseName);
  CreateDatabaseFromDPB(RaiseExceptionOnError);
end;

procedure TFBWireAttachment.CreateDatabaseFromDPB(RaiseExceptionOnError: boolean);
var dpbBytes: TBytes;
begin
  try
    dpbBytes := ConnectAndPrepareDPB;
    FHandle := FConnection.CreateDatabase(FRemoteDatabaseName,dpbBytes);
    FIsConnected := true;
  except
    on E: Exception do
    begin
      FConnection.Disconnect;
      FIsConnected := false;
      if RaiseExceptionOnError then
        WireIBError(FWireAPI,E);
    end;
  end;
end;

constructor TFBWireAttachment.CreateDatabase(api: TFBWireClientAPI;
  sql: AnsiString; aSQLDialect: integer; RaiseExceptionOnError: boolean);
var aDPB: IDPB;
    DatabaseName: AnsiString;
begin
  {the DPB and the database name are derived from the create statement}
  aDPB := TDPB.Create(api);
  DatabaseName := '';
  {the base class parses the create statement into FDPB and FDatabaseName}
  inherited Create(api,'',aDPB,RaiseExceptionOnError);
  DPBFromCreateSQL(sql);
  DatabaseName := FDatabaseName;
  FWireAPI := api;
  FConnection := TFBWireConnection.Create;
  ParseDatabaseName(DatabaseName);
  CreateDatabaseFromDPB(RaiseExceptionOnError);
end;

destructor TFBWireAttachment.Destroy;
begin
  inherited Destroy;
  if FConnection <> nil then
    FConnection.Free;
end;

procedure TFBWireAttachment.CheckHandle;
begin
  if not FIsConnected then
    IBError(ibxeDatabaseClosed,[nil]);
end;

function TFBWireAttachment.GetAttachment: IAttachment;
begin
  Result := self;
end;

procedure TFBWireAttachment.Connect;
var dpbBytes: TBytes;
begin
  if FIsConnected then Exit;
  try
    dpbBytes := ConnectAndPrepareDPB;
    FHandle := FConnection.AttachDatabase(FRemoteDatabaseName,dpbBytes);
    FIsConnected := true;
  except
    on E: Exception do
    begin
      FConnection.Disconnect;
      FIsConnected := false;
      if FRaiseExceptionOnConnectError then
        WireIBError(FWireAPI,E);
    end;
  end;
  if FIsConnected then
    ClearCachedInfo;
end;

procedure TFBWireAttachment.Disconnect(Force: boolean);
begin
  if not FIsConnected then Exit;
  EndAllTransactions;
  try
    FConnection.DetachDatabase(FHandle);
  except
    on E: Exception do
      if not Force then
      begin
        FIsConnected := false;
        FConnection.Disconnect;
        WireIBError(FWireAPI,E);
      end;
  end;
  FIsConnected := false;
  FHandle := 0;
  FConnection.Disconnect;
  ClearCachedInfo;
end;

function TFBWireAttachment.IsConnected: boolean;
begin
  Result := FIsConnected;
end;

procedure TFBWireAttachment.DropDatabase;
begin
  CheckHandle;
  EndAllTransactions;
  try
    FConnection.DropDatabase(FHandle);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  FIsConnected := false;
  FHandle := 0;
  FConnection.Disconnect;
  ClearCachedInfo;
end;

function TFBWireAttachment.StartTransaction(TPB: array of byte;
  DefaultCompletion: TTransactionCompletion; aName: AnsiString): ITransaction;
begin
  CheckHandle;
  Result := TFBWireTransaction.Create(FWireAPI,self,TPB,DefaultCompletion,aName);
end;

function TFBWireAttachment.StartTransaction(TPB: ITPB;
  DefaultCompletion: TTransactionCompletion; aName: AnsiString): ITransaction;
begin
  CheckHandle;
  Result := TFBWireTransaction.Create(FWireAPI,self,TPB,DefaultCompletion,aName);
end;

procedure TFBWireAttachment.ExecImmediate(transaction: ITransaction;
  sql: AnsiString; aSQLDialect: integer);
begin
  CheckHandle;
  try
    FConnection.ExecImmediate((transaction as TObject as TFBWireTransaction).Handle,
                              FHandle,aSQLDialect,sql);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
end;

function TFBWireAttachment.Prepare(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer; CursorName: AnsiString): IStatement;
begin
  CheckHandle;
  Result := TFBWireStatement.Create(self,transaction,sql,aSQLDialect,CursorName);
end;

function TFBWireAttachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: AnsiString; aSQLDialect: integer; GenerateParamNames: boolean;
  CaseSensitiveParams: boolean; CursorName: AnsiString): IStatement;
begin
  CheckHandle;
  Result := TFBWireStatement.CreateWithNamedParameters(self,transaction,sql,
              aSQLDialect,GenerateParamNames,CaseSensitiveParams,CursorName);
end;

function TFBWireAttachment.GetEventHandler(Events: TStrings): IEvents;
begin
  {events require a second TCP connection negotiated with
   op_connect_request - not yet implemented}
  IBError(ibxeNotSupported,[nil]);
  Result := nil;
end;

function TFBWireAttachment.CreateBlob(transaction: ITransaction;
  BlobMetaData: IBlobMetaData; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result := TFBWireBlob.Create(self,transaction as TFBWireTransaction,
                               BlobMetaData,BPB);
end;

function TFBWireAttachment.CreateBlob(transaction: ITransaction;
  SubType: integer; aCharSetID: cardinal; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result := TFBWireBlob.Create(self,transaction as TFBWireTransaction,
              TFBWireBlobMetaData.Create(self,transaction as TFBWireTransaction,
                                         '','',SubType,aCharSetID),BPB);
end;

function TFBWireAttachment.OpenBlob(transaction: ITransaction;
  BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result := TFBWireBlob.Create(self,transaction as TFBWireTransaction,
                               BlobMetaData,BlobID,BPB);
end;

function TFBWireAttachment.OpenArray(transaction: ITransaction;
  ArrayMetaData: IArrayMetaData; ArrayID: TISC_QUAD): IArray;
begin
  {array slices need op_get_slice and an SDL description - not yet
   implemented by this provider}
  IBError(ibxeNotSupported,[nil]);
  Result := nil;
end;

function TFBWireAttachment.CreateArray(transaction: ITransaction;
  ArrayMetaData: IArrayMetaData): IArray;
begin
  IBError(ibxeNotSupported,[nil]);
  Result := nil;
end;

function TFBWireAttachment.CreateArrayMetaData(SQLType: cardinal;
  tableName: AnsiString; columnName: AnsiString; Scale: integer; size: cardinal;
  aCharSetID: cardinal; dimensions: cardinal; bounds: TArrayBounds): IArrayMetaData;
begin
  IBError(ibxeNotSupported,[nil]);
  Result := nil;
end;

function TFBWireAttachment.GetBlobMetaData(Transaction: ITransaction;
  tableName, columnName: AnsiString): IBlobMetaData;
begin
  CheckHandle;
  Result := TFBWireBlobMetaData.Create(self,Transaction as TFBWireTransaction,
                                       tableName,columnName,0,0);

end;

function TFBWireAttachment.GetArrayMetaData(Transaction: ITransaction;
  tableName, columnName: AnsiString): IArrayMetaData;
begin
  IBError(ibxeNotSupported,[nil]);
  Result := nil;
end;

function TFBWireAttachment.GetDBInfo(ReqBuffer: PByte; ReqBufLen: integer): IDBInformation;
var items, response: TBytes;
    i, len: integer;
    Buffer: TDBInformation;
begin
  CheckHandle;
  SetLength(items,ReqBufLen);
  for i := 0 to ReqBufLen - 1 do
    items[i] := ReqBuffer[i];
  SetLength(response,0);
  try
    response := FConnection.GetInfo(op_info_database,FHandle,items,
                                    DBInfoDefaultBufferSize);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  Buffer := TDBInformation.Create(FWireAPI);
  Result := Buffer;
  len := Length(response);
  if len > Buffer.getBufSize then
    len := Buffer.getBufSize;
  if len > 0 then
    Move(response[0],Buffer.Buffer^,len);
end;

function TFBWireAttachment.HasDecFloatSupport: boolean;
begin
  Result := (FConnection <> nil) and
            (FConnection.ProtocolVersion >= (PROTOCOL_VERSION16 and FB_PROTOCOL_MASK));
end;

function TFBWireAttachment.HasTimeZoneSupport: boolean;
begin
  Result := HasDecFloatSupport;
end;

function TFBWireAttachment.HasBatchMode: boolean;
begin
  {the batch operations of protocol 16 are not implemented yet}
  Result := false;
end;

function TFBWireAttachment.HasScollableCursors: boolean;
begin
  Result := false;
end;

procedure TFBWireAttachment.getFBVersion(version: TStrings);
var CryptDescription: AnsiString;
begin
  version.Clear;
  if (FConnection <> nil) and (FConnection.CryptPlugin = '') then
    CryptDescription := 'unencrypted'
  else
  if FConnection <> nil then
    CryptDescription := FConnection.CryptPlugin + ' wire encryption';
  if FConnection <> nil then
    version.Add(Format('Firebird wire protocol version %d, %s authentication, %s',
      [FConnection.ProtocolVersion,FConnection.AuthPluginName,
       CryptDescription]));
end;

end.
