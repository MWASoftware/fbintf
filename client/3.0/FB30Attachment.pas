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
unit FB30Attachment;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, FB30ClientAPI, Firebird, IB, FBActivityMonitor, FBParamBlock;

type

  { TFB30Attachment }

  TFB30Attachment = class(TActivityHandler,IAttachment, IActivityMonitor)
  private
    FHasDefaultCharSet: boolean;
    FCharSetID: integer;
    FCodePage: TSystemCodePage;
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
    property HasDefaultCharSet: boolean read FHasDefaultCharSet;
    property CharSetID: integer read FCharSetID;
    property CodePage: TSystemCodePage read FCodePage;

  public
    {IAttachment}
    function getDPB: IDPB;
    function AllocateBPB: IBPB;
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

    function CreateBlob(transaction: ITransaction; RelationName, ColumnName: string; BPB: IBPB=nil): IBlob; overload;
    function CreateBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BPB: IBPB=nil): IBlob; overload;
    function CreateBlob(transaction: ITransaction; SubType: integer; aCharSetID: cardinal=0; BPB: IBPB=nil): IBlob; overload;
    function OpenBlob(transaction: ITransaction; RelationName, ColumnName: string; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob; overload;
    function OpenBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob; overload;

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

uses FB30Transaction, FB30Statement, FB30Array, FB30Blob, FBMessages,
  FBOutputBlock, FB30Events;

{ TFB30Attachment }

procedure TFB30Attachment.CheckHandle;
begin
  if FAttachmentIntf = nil then
    IBError(ibxeDatabaseClosed,[nil]);
end;

constructor TFB30Attachment.Create(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnConnectError: boolean);
begin
  inherited Create;
  FFirebirdAPI := Firebird30ClientAPI; {Keep reference to interface}
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

constructor TFB30Attachment.CreateDatabase(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnError: boolean);
var Param: IDPBItem;
begin
  FFirebirdAPI := Firebird30ClientAPI; {Keep reference to interface}
  FSQLDialect := 3;
  FDatabaseName := DatabaseName;
  if DPB = nil then
  begin
    if RaiseExceptionOnError then
       IBError(ibxeNoDPB,[nil]);
    Exit;
  end;
  FDPB := DPB;
  FRaiseExceptionOnConnectError := RaiseExceptionOnError;
  with Firebird30ClientAPI do
  begin
    FAttachmentIntf := ProviderIntf.createDatabase(StatusIntf,PAnsiChar(DatabaseName),
                                         (FDPB as TDPB).getDataLength,
                                         BytePtr((FDPB as TDPB).getBuffer));
    if FRaiseExceptionOnConnectError then Check4DataBaseError;
    if InErrorState then
      FAttachmentIntf := nil
    else
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

destructor TFB30Attachment.Destroy;
begin
  Disconnect(true);
  if assigned(FAttachmentIntf) then
    FAttachmentIntf.release;
  inherited Destroy;
end;

function TFB30Attachment.getDPB: IDPB;
begin
  Result := FDPB;
end;

function TFB30Attachment.AllocateBPB: IBPB;
begin
  Result := TBPB.Create;
end;

procedure TFB30Attachment.Connect;
var Param: IDPBItem;
begin
  with Firebird30ClientAPI do
  begin
    FAttachmentIntf := ProviderIntf.attachDatabase(StatusIntf,PAnsiChar(FDatabaseName),
                         (FDPB as TDPB).getDataLength,
                         BytePtr((FDPB as TDPB).getBuffer));
    if FRaiseExceptionOnConnectError then Check4DataBaseError;
    if InErrorState then
      FAttachmentIntf := nil
    else
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

procedure TFB30Attachment.Disconnect(Force: boolean);
begin
  if IsConnected then
    with Firebird30ClientAPI do
    begin
      FAttachmentIntf.Detach(StatusIntf);
      if not Force and InErrorState then
        IBDataBaseError;
      FAttachmentIntf := nil;
      FHasDefaultCharSet := false;
      FCodePage := CP_NONE;
      FCharSetID := 0;
    end;
end;

function TFB30Attachment.IsConnected: boolean;
begin
  Result := FAttachmentIntf <> nil;
end;

procedure TFB30Attachment.DropDatabase;
begin
  if IsConnected then
    with Firebird30ClientAPI do
    begin
      FAttachmentIntf.dropDatabase(StatusIntf);
      Check4DataBaseError;
      FAttachmentIntf := nil;
    end;
end;

function TFB30Attachment.StartTransaction(TPB: array of byte;
  DefaultCompletion: TTransactionAction): ITransaction;
begin
  CheckHandle;
  Result := TFB30Transaction.Create(self,TPB,DefaultCompletion);
end;

function TFB30Attachment.StartTransaction(TPB: ITPB;
  DefaultCompletion: TTransactionAction): ITransaction;
begin
  CheckHandle;
  Result := TFB30Transaction.Create(self,TPB,DefaultCompletion);
end;

procedure TFB30Attachment.ExecImmediate(transaction: ITransaction; sql: string;
  aSQLDialect: integer);
begin
  CheckHandle;
  with Firebird30ClientAPI do
  begin
    FAttachmentIntf.execute(StatusIntf,(transaction as TFB30Transaction).TransactionIntf,
                    Length(sql),PChar(sql),aSQLDialect,nil,nil,nil,nil);
    Check4DataBaseError;
  end;
end;

procedure TFB30Attachment.ExecImmediate(TPB: array of byte; sql: string;
  aSQLDialect: integer);
begin
  ExecImmediate(StartTransaction(TPB,taCommit),sql,aSQLDialect);
end;

procedure TFB30Attachment.ExecImmediate(transaction: ITransaction; sql: string);
begin
  ExecImmediate(transaction,sql,FSQLDialect);
end;

procedure TFB30Attachment.ExecImmediate(TPB: array of byte; sql: string);
begin
   ExecImmediate(StartTransaction(TPB,taCommit),sql,FSQLDialect);
end;

function TFB30Attachment.OpenCursor(transaction: ITransaction; sql: string;
  aSQLDialect: integer): IResultSet;
var Statement: IStatement;
begin
  CheckHandle;
  Statement := Prepare(transaction,sql,aSQLDialect);
  Result := Statement.OpenCursor;
end;

function TFB30Attachment.OpenCursor(transaction: ITransaction; sql: string
  ): IResultSet;
begin
    Result := OpenCursor(transaction,sql,FSQLDialect);
end;

function TFB30Attachment.OpenCursorAtStart(transaction: ITransaction;
  sql: string; aSQLDialect: integer): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect);
  Result.FetchNext;
end;

function TFB30Attachment.OpenCursorAtStart(transaction: ITransaction; sql: string
  ): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect);
end;

function TFB30Attachment.Prepare(transaction: ITransaction; sql: string;
  aSQLDialect: integer): IStatement;
begin
  CheckHandle;
  Result := TFB30Statement.Create(self,transaction,sql,aSQLDialect);
end;

function TFB30Attachment.Prepare(transaction: ITransaction; sql: string
  ): IStatement;
begin
  Result := Prepare(transaction,sql,FSQLDialect);
end;

function TFB30Attachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; aSQLDialect: integer; GenerateParamNames: boolean;
  UniqueParamNames: boolean): IStatement;
begin
  CheckHandle;
  Result := TFB30Statement.CreateWithParameterNames(self,transaction,sql,aSQLDialect,
         GenerateParamNames,UniqueParamNames);
end;

function TFB30Attachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; GenerateParamNames: boolean; UniqueParamNames: boolean
  ): IStatement;
begin
  CheckHandle;
  Result := TFB30Statement.CreateWithParameterNames(self,transaction,sql,FSQLDialect,
         GenerateParamNames,UniqueParamNames);
end;

function TFB30Attachment.GetEventHandler(Events: TStrings): IEvents;
begin
  CheckHandle;
  Result := TFB30Events.Create(self,Events);
end;

function TFB30Attachment.GetEventHandler(Event: string): IEvents;
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

function TFB30Attachment.CreateBlob(transaction: ITransaction; RelationName,
  ColumnName: string; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result := TFB30Blob.Create(self,transaction as TFB30Transaction,
              TFB30BlobMetaData.Create(self,Transaction as TFB30Transaction,RelationName,ColumnName),BPB);
end;

function TFB30Attachment.CreateBlob(transaction: ITransaction;
  BlobMetaData: IBlobMetaData; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result := TFB30Blob.Create(self,transaction as TFB30Transaction, BlobMetaData,BPB);
end;

function TFB30Attachment.CreateBlob(transaction: ITransaction;
  SubType: integer; aCharSetID: cardinal; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result := TFB30Blob.Create(self,transaction as TFB30Transaction, SubType,aCharSetID,BPB);
end;

function TFB30Attachment.OpenBlob(transaction: ITransaction; RelationName,
  ColumnName: string; BlobID: TISC_QUAD; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result := TFB30Blob.Create(self,transaction as TFB30transaction,
                TFB30BlobMetaData.Create(self,Transaction as TFB30Transaction,RelationName,ColumnName),
                BlobID,BPB);
end;

function TFB30Attachment.OpenBlob(transaction: ITransaction;
  BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result :=  TFB30Blob.Create(self,transaction as TFB30transaction,BlobMetaData,BlobID,BPB);
end;

function TFB30Attachment.OpenArray(transaction: ITransaction; RelationName,
  ColumnName: string; ArrayID: TISC_QUAD): IArray;
begin
  CheckHandle;
  Result := TFB30Array.Create(self,transaction as TFB30Transaction,
                    GetArrayMetaData(transaction,RelationName,ColumnName),ArrayID);
end;

function TFB30Attachment.CreateArray(transaction: ITransaction; RelationName,
  ColumnName: string): IArray;
begin
  CheckHandle;
  Result := TFB30Array.Create(self,transaction as TFB30Transaction,
                    GetArrayMetaData(transaction,RelationName,ColumnName));
end;

function TFB30Attachment.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

function TFB30Attachment.GetBlobMetaData(Transaction: ITransaction; tableName,
  columnName: string): IBlobMetaData;
begin
  CheckHandle;
  Result := TFB30BlobMetaData.Create(self,Transaction as TFB30Transaction,tableName,columnName);
end;

function TFB30Attachment.GetArrayMetaData(Transaction: ITransaction; tableName,
  columnName: string): IArrayMetaData;
begin
  CheckHandle;
  Result := TFB30ArrayMetaData.Create(self,Transaction as TFB30Transaction,tableName,columnName);
end;

function TFB30Attachment.GetDBInformation(Requests: array of byte
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

function TFB30Attachment.GetDBInformation(Request: byte): IDBInformation;
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

