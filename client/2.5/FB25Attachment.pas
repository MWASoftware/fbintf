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

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB,  FBClientAPI, FB25ClientAPI, IBHeader,
  FBParamBlock, FBOutputBlock, FBActivityMonitor;

type
  { TFB25Attachment }

  TFB25Attachment = class(TActivityHandler, IAttachment, IActivityMonitor)
  private
    FHasDefaultCharSet: boolean;
    FCharSetID: integer;
    FCodePage: TSystemCodePage;
    FHandle: TISC_DB_HANDLE;
    FDatabaseName: string;
    FDPB: IDPB;
    FSQLDialect: integer;
    FFirebirdAPI: IFirebirdAPI;
    FRaiseExceptionOnConnectError: boolean;
    procedure CheckHandle;
  public
    constructor Create(DatabaseName: string; DPB: IDPB;
      RaiseExceptionOnConnectError: boolean);
    constructor CreateDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnError: boolean);
    destructor Destroy; override;
    property Handle: TISC_DB_HANDLE read FHandle;
    property SQLDialect: integer read FSQLDialect;
    property HasDefaultCharSet: boolean read FHasDefaultCharSet;
    property CharSetID: integer read FCharSetID;
    property CodePage: TSystemCodePage read FCodePage;

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
    function CreateBlob(transaction: ITransaction; RelationName, ColumnName: string): IBlob; overload;
    function CreateBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData): IBlob; overload;
    function OpenBlob(transaction: ITransaction; RelationName, ColumnName: string; BlobID: TISC_QUAD): IBlob; overload;
    function OpenBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD): IBlob; overload;

    function OpenArray(transaction: ITransaction; RelationName, ColumnName: string;
      ArrayID: TISC_QUAD): IArray;
    function CreateArray(transaction: ITransaction; RelationName, ColumnName: string
      ): IArray;

    {Database Information}

    function GetSQLDialect: integer;
    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: string): IBlobMetaData;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: string): IArrayMetaData;
    function GetDBInformation(Requests: array of byte): IDBInformation; overload;
    function GetDBInformation(Request: byte): IDBInformation; overload;
  end;

implementation

uses FB25Events,FB25Transaction, FBMessages, FB25Blob,
  FB25Statement, FB25Array;

  { TFB25Attachment }

procedure TFB25Attachment.CheckHandle;
begin
  if FHandle = nil then
    IBError(ibxeDatabaseClosed,[nil]);
end;

constructor TFB25Attachment.Create(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnConnectError: boolean);
begin
  inherited Create;
  FFirebirdAPI := Firebird25ClientAPI; {Keep reference to interface}
  FSQLDialect := 3;
  FDatabaseName := DatabaseName;
  if DPB = nil then
  begin
    if RaiseExceptionOnConnectError then
       IBError(ibxeNoDPB,[nil]);
    Exit;
  end;
  FDPB := DPB;
  FRaiseExceptionOnConnectError := RaiseExceptionOnConnectError;
  Connect;
end;

constructor TFB25Attachment.CreateDatabase(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnError: boolean);
var sql: string;
    tr_handle: TISC_TR_HANDLE;
    CreateParams: string;
    DPBItem: IDPBItem;
begin
  inherited Create;
  FFirebirdAPI := Firebird25ClientAPI; {Keep reference to interface}
  CreateParams := '';
  FRaiseExceptionOnConnectError := RaiseExceptionOnError;
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
    with FirebirdClientAPI do
    begin
      CreateParams += ' DEFAULT CHARACTER SET ' + DPBItem.AsString;
      FHasDefaultCharSet :=   CharSetName2CharSetID(DPBItem.AsString,FCharSetID) and
                              CharSetID2CodePage(FCharSetID,FCodePage) and
                              (FCharSetID > 1);
    end;

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
  if DPB <> nil then
  {Connect using known parameters}
  begin
    Disconnect;
    Connect;
  end;
end;

destructor TFB25Attachment.Destroy;
begin
  Disconnect(true);
  inherited Destroy;
end;

function TFB25Attachment.getDPB: IDPB;
begin
  Result := FDPB;
end;

procedure TFB25Attachment.Connect;
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

    if IsConnected then
    begin
     Param := FDPB.Find(isc_dpb_set_db_SQL_dialect);
     if Param <> nil then
       FSQLDialect := Param.AsByte;
     Param :=  FDPB.Find(isc_dpb_lc_ctype);
     FHasDefaultCharSet :=  (Param <> nil) and
                             CharSetName2CharSetID(Param.AsString,FCharSetID) and
                             CharSetID2CodePage(FCharSetID,FCodePage) and
                             (FCharSetID > 1);
    end;
  end;
end;

procedure TFB25Attachment.Disconnect(Force: boolean);
begin
  if FHandle = nil then
    Exit;

  {Disconnect}
  with Firebird25ClientAPI do
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
  with Firebird25ClientAPI do
    if isc_drop_database(StatusVector, @FHandle) > 0 then
      IBDatabaseError;
  FHandle := nil;
end;

function TFB25Attachment.StartTransaction(TPB: array of byte;
  DefaultCompletion: TTransactionAction): ITransaction;
begin
  CheckHandle;
  Result := TFB25Transaction.Create(self,TPB,DefaultCompletion);
end;

function TFB25Attachment.StartTransaction(TPB: ITPB;
  DefaultCompletion: TTransactionAction): ITransaction;
begin
  CheckHandle;
  Result := TFB25Transaction.Create(self,TPB,DefaultCompletion);
end;

function TFB25Attachment.CreateBlob(transaction: ITransaction; RelationName,
  ColumnName: string): IBlob;
begin
  CheckHandle;
  Result := TFB25Blob.Create(self,transaction as TFB25transaction,
                TFB25BlobMetaData.Create(self,Transaction as TFB25Transaction,RelationName,ColumnName));
end;

function TFB25Attachment.CreateBlob(transaction: ITransaction;
  BlobMetaData: IBlobMetaData): IBlob;
begin
  CheckHandle;
  Result := TFB25Blob.Create(self,transaction as TFB25transaction,BlobMetaData);
end;

function TFB25Attachment.OpenBlob(transaction: ITransaction; RelationName,
  ColumnName: string; BlobID: TISC_QUAD): IBlob;
begin
  CheckHandle;
  Result := TFB25Blob.Create(self,transaction as TFB25transaction,
                TFB25BlobMetaData.Create(self,Transaction as TFB25Transaction,RelationName,ColumnName),
                BlobID);
end;

function TFB25Attachment.OpenBlob(transaction: ITransaction;
  BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD): IBlob;
begin
  CheckHandle;
  Result :=  TFB25Blob.Create(self,transaction as TFB25transaction,BlobMetaData,BlobID);
end;

procedure TFB25Attachment.ExecImmediate(transaction: ITransaction; sql: string;
  aSQLDialect: integer);
var TRHandle: TISC_TR_HANDLE;
begin
  CheckHandle;
  TRHandle := (Transaction as TFB25Transaction).Handle;
  with Firebird25ClientAPI do
    if isc_dsql_execute_immediate(StatusVector, @fHandle, @TRHandle, 0,PChar(sql), aSQLDialect, nil) > 0 then
      IBDatabaseError;
  SignalActivity;
end;

procedure TFB25Attachment.ExecImmediate(TPB: array of byte; sql: string;
  aSQLDialect: integer);
begin
  ExecImmediate(StartTransaction(TPB,taCommit),sql,aSQLDialect);
end;

procedure TFB25Attachment.ExecImmediate(transaction: ITransaction; sql: string);
begin
  ExecImmediate(transaction,sql,FSQLDialect);
end;

procedure TFB25Attachment.ExecImmediate(TPB: array of byte; sql: string);
begin
  ExecImmediate(StartTransaction(TPB,taCommit),sql,FSQLDialect);
end;

function TFB25Attachment.OpenCursor(transaction: ITransaction; sql: string;
  aSQLDialect: integer): IResultSet;
var Statement: IStatement;
begin
  CheckHandle;
  Statement := Prepare(transaction,sql,aSQLDialect);
  Result := Statement.OpenCursor;
end;

function TFB25Attachment.OpenCursor(transaction: ITransaction; sql: string
  ): IResultSet;
begin
  Result := OpenCursor(transaction,sql,FSQLDialect);
end;

function TFB25Attachment.OpenCursorAtStart(transaction: ITransaction;
  sql: string; aSQLDialect: integer): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect);
  Result.FetchNext;
end;

function TFB25Attachment.OpenCursorAtStart(transaction: ITransaction; sql: string
  ): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect);
end;

function TFB25Attachment.Prepare(transaction: ITransaction; sql: string;
  aSQLDialect: integer): IStatement;
begin
  CheckHandle;
  Result := TFB25Statement.Create(self,transaction,sql,aSQLDialect);
end;

function TFB25Attachment.Prepare(transaction: ITransaction; sql: string
  ): IStatement;
begin
  Result := Prepare(transaction,sql,FSQLDialect);
end;

function TFB25Attachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; aSQLDialect: integer; GenerateParamNames: boolean;
  UniqueParamNames: boolean): IStatement;
begin
  CheckHandle;
  Result := TFB25Statement.CreateWithParameterNames(self,transaction,sql,aSQLDialect,
         GenerateParamNames,UniqueParamNames);
end;

function TFB25Attachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; GenerateParamNames: boolean; UniqueParamNames: boolean
  ): IStatement;
begin
  CheckHandle;
  Result := TFB25Statement.CreateWithParameterNames(self,transaction,sql,FSQLDialect,
         GenerateParamNames,UniqueParamNames);
end;

function TFB25Attachment.GetEventHandler(Events: TStrings): IEvents;
begin
  CheckHandle;
  Result := TFB25Events.Create(self,Events);
end;

function TFB25Attachment.GetEventHandler(Event: string): IEvents;
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

function TFB25Attachment.OpenArray(transaction: ITransaction; RelationName, ColumnName: string;
  ArrayID: TISC_QUAD): IArray;
begin
  CheckHandle;
  Result := TFB25Array.Create(self,transaction as TFB25Transaction,
                    GetArrayMetaData(transaction,RelationName,ColumnName),ArrayID);
end;

function TFB25Attachment.CreateArray(transaction: ITransaction; RelationName, ColumnName: string): IArray;
begin
  CheckHandle;
  Result := TFB25Array.Create(self,transaction as TFB25Transaction,
                    GetArrayMetaData(transaction,RelationName,ColumnName));
end;

function TFB25Attachment.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

function TFB25Attachment.GetBlobMetaData(Transaction: ITransaction; tableName,
  columnName: string): IBlobMetaData;
begin
  CheckHandle;
  Result := TFB25BlobMetaData.Create(self,Transaction as TFB25Transaction,tableName,columnName);
end;

function TFB25Attachment.GetArrayMetaData(Transaction: ITransaction; tableName,
  columnName: string): IArrayMetaData;
begin
  CheckHandle;
  Result := TFB25ArrayMetaData.Create(self,Transaction as TFB25Transaction,tableName,columnName);
end;

function TFB25Attachment.GetDBInformation(Requests: array of byte
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

      with Firebird25ClientAPI, Result as TDBInformation do
          if isc_database_info(StatusVector, @(FHandle), Length(Requests), PChar(ReqBuffer),
                                 getBufSize, Buffer) > 0 then
            IBDataBaseError;

    finally
      FreeMem(ReqBuffer);
    end;
  end;
end;

function TFB25Attachment.GetDBInformation(Request: byte): IDBInformation;
begin
  CheckHandle;
  Result := TDBInformation.Create;
  with Firebird25ClientAPI, Result as TDBInformation do
    if isc_database_info(StatusVector, @(FHandle), 1, @Request,
                           getBufSize, Buffer) > 0 then
      IBDataBaseError;
end;

end.

