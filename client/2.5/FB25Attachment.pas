(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  The contents of this file are subject to the Initial Developer's
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
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2016 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FB25Attachment;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB,  FBAttachment, FB25ClientAPI, IBHeader,
  FBParamBlock, FBOutputBlock, FBActivityMonitor;

type
  { TFB25Attachment }

  TFB25Attachment = class(TFBAttachment, IAttachment, IActivityMonitor)
  private
    FHandle: TISC_DB_HANDLE;
    FFirebird25ClientAPI: TFB25ClientAPI;
  protected
    procedure CheckHandle; override;
  public
    constructor Create(api: TFB25ClientAPI; DatabaseName: AnsiString; aDPB: IDPB;
      RaiseExceptionOnConnectError: boolean);
    constructor CreateDatabase(api: TFB25ClientAPI; DatabaseName: AnsiString; aDPB: IDPB; RaiseExceptionOnError: boolean); overload;
    constructor CreateDatabase(api: TFB25ClientAPI; sql: AnsiString; aSQLDialect: integer;
      RaiseExceptionOnError: boolean); overload;
    function GetDBInfo(ReqBuffer: PByte; ReqBufLen: integer): IDBInformation; override;
    property Handle: TISC_DB_HANDLE read FHandle;
    property Firebird25ClientAPI: TFB25ClientAPI read FFirebird25ClientAPI;

  public
    {IAttachment}
    procedure Connect;
    procedure Disconnect(Force: boolean=false); override;
    function IsConnected: boolean; override;
    procedure DropDatabase;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction; override;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionCompletion): ITransaction; override;
    procedure ExecImmediate(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer); override;
    function Prepare(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer): IStatement; override;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: AnsiString;
                       aSQLDialect: integer; GenerateParamNames: boolean=false): IStatement; override;
    function GetEventHandler(Events: TStrings): IEvents; override;
    function CreateBlob(transaction: ITransaction; RelationName, ColumnName: AnsiString; BPB: IBPB=nil): IBlob; overload;
    function CreateBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BPB: IBPB=nil): IBlob; overload;
    function CreateBlob(transaction: ITransaction; SubType: integer; aCharSetID: cardinal=0; BPB: IBPB=nil): IBlob; overload;
    function OpenBlob(transaction: ITransaction; RelationName, ColumnName: AnsiString; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob; overload;
    function OpenBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob; overload; override;

    function OpenArray(transaction: ITransaction; RelationName, ColumnName: AnsiString;
      ArrayID: TISC_QUAD): IArray;
    function CreateArray(transaction: ITransaction; RelationName, ColumnName: AnsiString
      ): IArray; overload;
    function CreateArray(transaction: ITransaction; ArrayMetaData: IArrayMetaData): IArray; overload;
    function CreateArrayMetaData(SQLType: cardinal; tableName: AnsiString; columnName: AnsiString;
      Scale: integer; size: cardinal;
      acharSetID: cardinal; dimensions: cardinal; bounds: TArrayBounds
  ): IArrayMetaData;

    {Database Information}

    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: AnsiString): IBlobMetaData;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: AnsiString): IArrayMetaData;
    procedure getFBVersion(version: TStrings);
  end;

implementation

uses FB25Events,FB25Transaction, FBMessages, FB25Blob,
  FB25Statement, FB25Array, IBUtils, IBExternals;

  { TFB25Attachment }

procedure TFB25Attachment.CheckHandle;
begin
  if FHandle = nil then
    IBError(ibxeDatabaseClosed,[nil]);
end;

constructor TFB25Attachment.Create(api: TFB25ClientAPI; DatabaseName: AnsiString; aDPB: IDPB;
  RaiseExceptionOnConnectError: boolean);
begin
  FFirebird25ClientAPI := api;
  if aDPB = nil then
  begin
    if RaiseExceptionOnConnectError then
       IBError(ibxeNoDPB,[nil]);
    Exit;
  end;
  inherited Create(api,DatabaseName,aDPB,RaiseExceptionOnConnectError);
  Connect;
end;

constructor TFB25Attachment.CreateDatabase(api: TFB25ClientAPI; DatabaseName: AnsiString; aDPB: IDPB;
  RaiseExceptionOnError: boolean);
var sql: AnsiString;
    tr_handle: TISC_TR_HANDLE;
begin
  inherited Create(api,DatabaseName,aDPB,RaiseExceptionOnError);
  FFirebird25ClientAPI := api;
  sql := GenerateCreateDatabaseSQL(DatabaseName,aDPB);
  tr_handle := nil;
  with FFirebird25ClientAPI do
  if (isc_dsql_execute_immediate(StatusVector, @FHandle, @tr_handle, 0, PAnsiChar(sql),
                                  SQLDialect, nil) > 0) and RaiseExceptionOnError then
    IBDataBaseError;
  if DPB <> nil then
  {Connect using known parameters}
  begin
    Disconnect;
    Connect;
  end
  else
    GetODSAndConnectionInfo;
end;

constructor TFB25Attachment.CreateDatabase(api: TFB25ClientAPI; sql: AnsiString; aSQLDialect: integer;
    RaiseExceptionOnError: boolean);
var tr_handle: TISC_TR_HANDLE;
begin
  inherited Create(api,'',nil,RaiseExceptionOnError);
  FFirebird25ClientAPI := api;
  FSQLDialect := aSQLDialect;
  tr_handle := nil;
  with FFirebird25ClientAPI do
  begin
    if (isc_dsql_execute_immediate(StatusVector, @FHandle, @tr_handle, 0, PAnsiChar(sql),
                                  aSQLDialect, nil) > 0) and RaiseExceptionOnError then
      IBDataBaseError;

  end;
  GetODSAndConnectionInfo;
  ExtractConnectString(sql,FDatabaseName);
  DPBFromCreateSQL(sql);
end;

function TFB25Attachment.GetDBInfo(ReqBuffer: PByte; ReqBufLen: integer
  ): IDBInformation;
begin
  Result := TDBInformation.Create(FFirebird25ClientAPI);
  with FFirebird25ClientAPI, Result as TDBInformation do
     if isc_database_info(StatusVector, @(FHandle), ReqBufLen, ReqBuffer,
                               getBufSize, Buffer) > 0 then
          IBDataBaseError;
end;

procedure TFB25Attachment.Connect;
begin
  FSQLDialect := 3;

  with FFirebird25ClientAPI do
  if DPB = nil then
  begin
    if (isc_attach_database(StatusVector, Length(FDatabaseName),
                        PAnsiChar(FDatabaseName), @FHandle, 0, nil) > 0) and FRaiseExceptionOnConnectError then
      IBDatabaseError;
  end
  else
  begin
    if (isc_attach_database(StatusVector, Length(FDatabaseName),
                         PAnsiChar(FDatabaseName), @FHandle,
                         (DPB as TDPB).getDataLength,
                         (DPB as TDPB).getBuffer) > 0 ) and FRaiseExceptionOnConnectError then
      IBDatabaseError;

  end;
  GetODSAndConnectionInfo;
end;

procedure TFB25Attachment.Disconnect(Force: boolean);
begin
  if FHandle = nil then
    Exit;

  EndAllTransactions;
  {Disconnect}
  with FFirebird25ClientAPI do
    if (isc_detach_database(StatusVector, @FHandle) > 0) and not Force then
      IBDatabaseError;
  FHandle := nil;
  FHasDefaultCharSet := false;
  FCodePage := CP_NONE;
  FCharSetID := 0;
end;

function TFB25Attachment.IsConnected: boolean;
begin
  Result := FHandle <> nil;
end;

procedure TFB25Attachment.DropDatabase;
begin
  CheckHandle;
  EndAllTransactions;
  with FFirebird25ClientAPI do
    if isc_drop_database(StatusVector, @FHandle) > 0 then
      IBDatabaseError;
  FHandle := nil;
end;

function TFB25Attachment.StartTransaction(TPB: array of byte;
  DefaultCompletion: TTransactionCompletion): ITransaction;
begin
  CheckHandle;
  Result := TFB25Transaction.Create(FFirebird25ClientAPI,self,TPB,DefaultCompletion);
end;

function TFB25Attachment.StartTransaction(TPB: ITPB;
  DefaultCompletion: TTransactionCompletion): ITransaction;
begin
  CheckHandle;
  Result := TFB25Transaction.Create(FFirebird25ClientAPI,self,TPB,DefaultCompletion);
end;

function TFB25Attachment.CreateBlob(transaction: ITransaction; RelationName,
  ColumnName: AnsiString; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result := TFB25Blob.Create(self,transaction as TFB25transaction,
                TFB25BlobMetaData.Create(self,Transaction as TFB25Transaction,RelationName,ColumnName),BPB);
end;

function TFB25Attachment.CreateBlob(transaction: ITransaction;
  BlobMetaData: IBlobMetaData; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result := TFB25Blob.Create(self,transaction as TFB25transaction,BlobMetaData,BPB);
end;

function TFB25Attachment.CreateBlob(transaction: ITransaction;
  SubType: integer; aCharSetID: cardinal; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result := TFB25Blob.Create(self,transaction as TFB25transaction,SubType,aCharSetID,BPB);
end;

function TFB25Attachment.OpenBlob(transaction: ITransaction; RelationName,
  ColumnName: AnsiString; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob;
begin
  CheckHandle;
  Result := TFB25Blob.Create(self,transaction as TFB25transaction,
                TFB25BlobMetaData.Create(self,Transaction as TFB25Transaction,RelationName,ColumnName),
                BlobID,BPB);
end;

function TFB25Attachment.OpenBlob(transaction: ITransaction;
  BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob;
begin
  CheckHandle;
  Result :=  TFB25Blob.Create(self,transaction as TFB25transaction,BlobMetaData,BlobID,BPB);
end;

procedure TFB25Attachment.ExecImmediate(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer);
var TRHandle: TISC_TR_HANDLE;
begin
  CheckHandle;
  TRHandle := (Transaction as TFB25Transaction).Handle;
  with FFirebird25ClientAPI do
    if isc_dsql_execute_immediate(StatusVector, @fHandle, @TRHandle, 0,PAnsiChar(sql), aSQLDialect, nil) > 0 then
      IBDatabaseError;
  SignalActivity;
end;

function TFB25Attachment.Prepare(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer): IStatement;
begin
  CheckHandle;
  Result := TFB25Statement.Create(self,transaction,sql,aSQLDialect);
end;

function TFB25Attachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: AnsiString; aSQLDialect: integer; GenerateParamNames: boolean): IStatement;
begin
  CheckHandle;
  Result := TFB25Statement.CreateWithParameterNames(self,transaction,sql,aSQLDialect,
         GenerateParamNames);
end;

function TFB25Attachment.GetEventHandler(Events: TStrings): IEvents;
begin
  CheckHandle;
  Result := TFB25Events.Create(self,Events);
end;

function TFB25Attachment.OpenArray(transaction: ITransaction; RelationName, ColumnName: AnsiString;
  ArrayID: TISC_QUAD): IArray;
begin
  CheckHandle;
  Result := TFB25Array.Create(self,transaction as TFB25Transaction,
                    GetArrayMetaData(transaction,RelationName,ColumnName),ArrayID);
end;

function TFB25Attachment.CreateArray(transaction: ITransaction; RelationName, ColumnName: AnsiString): IArray;
begin
  CheckHandle;
  Result := TFB25Array.Create(self,transaction as TFB25Transaction,
                    GetArrayMetaData(transaction,RelationName,ColumnName));
end;

function TFB25Attachment.CreateArray(transaction: ITransaction;
  ArrayMetaData: IArrayMetaData): IArray;
begin
  CheckHandle;
  Result := TFB25Array.Create(self,transaction as TFB25Transaction,ArrayMetaData);
end;

function TFB25Attachment.CreateArrayMetaData(SQLType: cardinal;
  tableName: AnsiString; columnName: AnsiString; Scale: integer; size: cardinal;
  acharSetID: cardinal; dimensions: cardinal; bounds: TArrayBounds
  ): IArrayMetaData;
begin
  Result := TFB25ArrayMetaData.Create(self,SQLType,tableName,ColumnName,Scale,size,acharSetID,dimensions,bounds);
end;

function TFB25Attachment.GetBlobMetaData(Transaction: ITransaction; tableName,
  columnName: AnsiString): IBlobMetaData;
begin
  CheckHandle;
  Result := TFB25BlobMetaData.Create(self,Transaction as TFB25Transaction,tableName,columnName);
end;

function TFB25Attachment.GetArrayMetaData(Transaction: ITransaction; tableName,
  columnName: AnsiString): IArrayMetaData;
begin
  CheckHandle;
  Result := TFB25ArrayMetaData.Create(self,Transaction as TFB25Transaction,tableName,columnName);
end;

procedure ISCVersionCallback(userArg: pointer; text: PAnsiChar); cdecl;
begin
  TStrings(userArg).Add(text);
end;

procedure TFB25Attachment.getFBVersion(version: TStrings);
var callback: pointer;
begin
  callback := @ISCVersionCallback;
  version.Clear;
  with FFirebird25ClientAPI do
    if isc_version(@FHandle,TISC_CALLBACK(callback),PVoid(version)) > 0 then
       IBDataBaseError;
end;

end.

