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
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, FBAttachment, FBClientAPI, FB30ClientAPI, Firebird, IB,
  FBActivityMonitor, FBParamBlock;

type

  { TFB30Attachment }

  TFB30Attachment = class(TFBAttachment,IAttachment, IActivityMonitor)
  private
    FAttachmentIntf: Firebird.IAttachment;
    FFirebird30ClientAPI: TFB30ClientAPI;
    FTimeZoneServices: ITimeZoneServices;
    FUsingRemoteICU: boolean;
    procedure SetUseRemoteICU(aValue: boolean);
  protected
    procedure CheckHandle; override;
  public
    constructor Create(api: TFB30ClientAPI; DatabaseName: AnsiString; aDPB: IDPB;
          RaiseExceptionOnConnectError: boolean);
    constructor CreateDatabase(api: TFB30ClientAPI; DatabaseName: AnsiString; aDPB: IDPB; RaiseExceptionOnError: boolean);  overload;
    constructor CreateDatabase(api: TFB30ClientAPI; sql: AnsiString; aSQLDialect: integer;
      RaiseExceptionOnError: boolean); overload;
    destructor Destroy; override;
    function GetDBInfo(ReqBuffer: PByte; ReqBufLen: integer): IDBInformation;
      override;
    property AttachmentIntf: Firebird.IAttachment read FAttachmentIntf;
    property Firebird30ClientAPI: TFB30ClientAPI read FFirebird30ClientAPI;

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
                       aSQLDialect: integer; GenerateParamNames: boolean=false;
                       CaseSensitiveParams: boolean=false): IStatement; override;

    {Events}
    function GetEventHandler(Events: TStrings): IEvents; override;

    {Blob - may use to open existing Blobs. However, ISQLData.AsBlob is preferred}

    function CreateBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BPB: IBPB=nil): IBlob; overload; override;
    function CreateBlob(transaction: ITransaction; SubType: integer; aCharSetID: cardinal=0; BPB: IBPB=nil): IBlob; overload;
    function OpenBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob;  overload; override;

    {Array}
    function OpenArray(transaction: ITransaction; ArrayMetaData: IArrayMetaData; ArrayID: TISC_QUAD): IArray; overload; override;
    function CreateArray(transaction: ITransaction; ArrayMetaData: IArrayMetaData): IArray; overload; override;
    function CreateArrayMetaData(SQLType: cardinal; tableName: AnsiString;
      columnName: AnsiString; Scale: integer; size: cardinal; aCharSetID: cardinal;
      dimensions: cardinal; bounds: TArrayBounds): IArrayMetaData;


    {Database Information}
    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: AnsiString): IBlobMetaData; override;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: AnsiString): IArrayMetaData; override;
    procedure getFBVersion(version: TStrings);
    function HasDecFloatSupport: boolean; override;

    {Time Zone Support}
    function GetTimeZoneServices: ITimeZoneServices; override;
    function HasTimeZoneSupport: boolean; override;
  end;

implementation

uses FB30Transaction, FB30Statement, FB30Array, FB30Blob, FBMessages,
  FBOutputBlock, FB30Events, IBUtils, FB30TimeZoneServices;

type
  { TVersionCallback }

  TVersionCallback = class(Firebird.IVersionCallbackImpl)
  private
    FOutput: TStrings;
    FFirebirdClientAPI: TFBClientAPI;
  public
    constructor Create(FirebirdClientAPI: TFBClientAPI; output: TStrings);
    procedure callback(status: Firebird.IStatus; text: PAnsiChar); override;
  end;

{ TVersionCallback }

constructor TVersionCallback.Create(FirebirdClientAPI: TFBClientAPI;
  output: TStrings);
begin
  inherited Create;
  FFirebirdClientAPI := FirebirdClientAPI;
  FOutput := output;
end;

procedure TVersionCallback.callback(status: Firebird.IStatus; text: PAnsiChar);
var StatusObj: TFB30StatusObject;
begin
  if ((status.getState and status.STATE_ERRORS) <> 0) then
  begin
    StatusObj := TFB30StatusObject.Create(FFirebirdClientAPI,status);
    try
      raise EIBInterBaseError.Create(StatusObj);
    finally
      StatusObj.Free;
    end;
  end;
  FOutput.Add(text);
end;


{ TFB30Attachment }

procedure TFB30Attachment.SetUseRemoteICU(aValue: boolean);
begin
  if (FUsingRemoteICU <> aValue) and (GetODSMajorVersion >= 13) then
  begin
    if aValue then
      ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_concurrency],'SET BIND OF TIME ZONE TO EXTENDED')
    else
      ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_concurrency],'SET BIND OF TIME ZONE TO NATIVE');
    FUsingRemoteICU := aValue;
  end;
end;

procedure TFB30Attachment.CheckHandle;
begin
  if FAttachmentIntf = nil then
    IBError(ibxeDatabaseClosed,[nil]);
end;

constructor TFB30Attachment.Create(api: TFB30ClientAPI; DatabaseName: AnsiString; aDPB: IDPB;
  RaiseExceptionOnConnectError: boolean);
begin
  FFirebird30ClientAPI := api;
  if aDPB = nil then
  begin
    if RaiseExceptionOnConnectError then
       IBError(ibxeNoDPB,[nil]);
    Exit;
  end;
  inherited Create(api,DatabaseName,aDPB,RaiseExceptionOnConnectError);
  Connect;
end;

constructor TFB30Attachment.CreateDatabase(api: TFB30ClientAPI; DatabaseName: AnsiString; aDPB: IDPB;
  RaiseExceptionOnError: boolean);
var Param: IDPBItem;
    sql: AnsiString;
    IsCreateDB: boolean;
begin
  inherited Create(api,DatabaseName,aDPB,RaiseExceptionOnError);
  FFirebird30ClientAPI := api;
  IsCreateDB := true;
  if aDPB <> nil then
  begin
    Param := aDPB.Find(isc_dpb_set_db_SQL_dialect);
    if Param <> nil then
      FSQLDialect := Param.AsByte;
  end;
  sql := GenerateCreateDatabaseSQL(DatabaseName,aDPB);
  with FFirebird30ClientAPI do
  begin
    FAttachmentIntf := UtilIntf.executeCreateDatabase(StatusIntf,Length(sql),
                                       PAnsiChar(sql),FSQLDialect,@IsCreateDB);
    if FRaiseExceptionOnConnectError then Check4DataBaseError;
    if InErrorState then
      FAttachmentIntf := nil
    else
    if aDPB <> nil then
    {Connect using known parameters}
    begin
      Disconnect;
      Connect;
    end
    else
      GetODSAndConnectionInfo;
  end;
end;

constructor TFB30Attachment.CreateDatabase(api: TFB30ClientAPI; sql: AnsiString; aSQLDialect: integer;
  RaiseExceptionOnError: boolean);
var IsCreateDB: boolean;
begin
  inherited Create(api,'',nil,RaiseExceptionOnError);
  FFirebird30ClientAPI := api;
  FSQLDialect := aSQLDialect;
  with FFirebird30ClientAPI do
  begin
    FAttachmentIntf := UtilIntf.executeCreateDatabase(StatusIntf,Length(sql),
                                       PAnsiChar(sql),aSQLDialect,@IsCreateDB);
    if FRaiseExceptionOnConnectError then Check4DataBaseError;
    if InErrorState then
      FAttachmentIntf := nil;
  end;
  GetODSAndConnectionInfo;
  ExtractConnectString(sql,FDatabaseName);
  DPBFromCreateSQL(sql);
end;

destructor TFB30Attachment.Destroy;
begin
  inherited Destroy;
  if assigned(FAttachmentIntf) then
    FAttachmentIntf.release;
end;

function TFB30Attachment.GetDBInfo(ReqBuffer: PByte; ReqBufLen: integer): IDBInformation;
begin
  Result := TDBInformation.Create(Firebird30ClientAPI);
  with FFirebird30ClientAPI, Result as TDBInformation do
  begin
    FAttachmentIntf.getInfo(StatusIntf, ReqBufLen, BytePtr(ReqBuffer),
                               getBufSize, BytePtr(Buffer));
      Check4DataBaseError;
  end
end;

procedure TFB30Attachment.Connect;
begin
  with FFirebird30ClientAPI do
  begin
    FAttachmentIntf := ProviderIntf.attachDatabase(StatusIntf,PAnsiChar(FDatabaseName),
                         (DPB as TDPB).getDataLength,
                         BytePtr((DPB as TDPB).getBuffer));
    if FRaiseExceptionOnConnectError then Check4DataBaseError;
    if InErrorState then
      FAttachmentIntf := nil
    else
      GetODSAndConnectionInfo;

  end;
end;

procedure TFB30Attachment.Disconnect(Force: boolean);
begin
  if IsConnected then
    with FFirebird30ClientAPI do
    begin
      EndAllTransactions;
      FAttachmentIntf.Detach(StatusIntf);
      if not Force and InErrorState then
        IBDataBaseError;
      FAttachmentIntf := nil;
      FHasDefaultCharSet := false;
      FCodePage := CP_NONE;
      FCharSetID := 0;
      FTimeZoneServices := nil;
    end;
end;

function TFB30Attachment.IsConnected: boolean;
begin
  Result := FAttachmentIntf <> nil;
end;

procedure TFB30Attachment.DropDatabase;
begin
  if IsConnected then
    with FFirebird30ClientAPI do
    begin
      EndAllTransactions;
      FAttachmentIntf.dropDatabase(StatusIntf);
      Check4DataBaseError;
      FAttachmentIntf := nil;
    end;
end;

function TFB30Attachment.StartTransaction(TPB: array of byte;
  DefaultCompletion: TTransactionCompletion): ITransaction;
begin
  CheckHandle;
  Result := TFB30Transaction.Create(FFirebird30ClientAPI,self,TPB,DefaultCompletion);
end;

function TFB30Attachment.StartTransaction(TPB: ITPB;
  DefaultCompletion: TTransactionCompletion): ITransaction;
begin
  CheckHandle;
  Result := TFB30Transaction.Create(FFirebird30ClientAPI,self,TPB,DefaultCompletion);
end;

procedure TFB30Attachment.ExecImmediate(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer);
begin
  CheckHandle;
  with FFirebird30ClientAPI do
  begin
    FAttachmentIntf.execute(StatusIntf,(transaction as TFB30Transaction).TransactionIntf,
                    Length(sql),PAnsiChar(sql),aSQLDialect,nil,nil,nil,nil);
    Check4DataBaseError;
  end;
end;

function TFB30Attachment.Prepare(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer): IStatement;
begin
  CheckHandle;
  Result := TFB30Statement.Create(self,transaction,sql,aSQLDialect);
end;

function TFB30Attachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: AnsiString; aSQLDialect: integer; GenerateParamNames: boolean;
  CaseSensitiveParams: boolean): IStatement;
begin
  CheckHandle;
  Result := TFB30Statement.CreateWithParameterNames(self,transaction,sql,aSQLDialect,
         GenerateParamNames,CaseSensitiveParams);
end;

function TFB30Attachment.GetEventHandler(Events: TStrings): IEvents;
begin
  CheckHandle;
  Result := TFB30Events.Create(self,Events);
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

function TFB30Attachment.OpenBlob(transaction: ITransaction;
  BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB): IBlob;
begin
  CheckHandle;
  Result :=  TFB30Blob.Create(self,transaction as TFB30transaction,BlobMetaData,BlobID,BPB);
end;

function TFB30Attachment.OpenArray(transaction: ITransaction;
  ArrayMetaData: IArrayMetaData; ArrayID: TISC_QUAD): IArray;
begin
  CheckHandle;
  Result := TFB30Array.Create(self,transaction as TFB30Transaction,
                    ArrayMetaData,ArrayID);
end;

function TFB30Attachment.CreateArray(transaction: ITransaction;
  ArrayMetaData: IArrayMetaData): IArray;
begin
  CheckHandle;
  Result := TFB30Array.Create(self,transaction as TFB30Transaction,ArrayMetaData);
end;

function TFB30Attachment.CreateArrayMetaData(SQLType: cardinal; tableName: AnsiString; columnName: AnsiString;
  Scale: integer; size: cardinal; aCharSetID: cardinal; dimensions: cardinal;
  bounds: TArrayBounds): IArrayMetaData;
begin
  Result := TFB30ArrayMetaData.Create(self,SQLType,tableName,ColumnName,Scale,size,aCharSetID, dimensions,bounds);
end;

function TFB30Attachment.GetBlobMetaData(Transaction: ITransaction; tableName,
  columnName: AnsiString): IBlobMetaData;
begin
  CheckHandle;
  Result := TFB30BlobMetaData.Create(self,Transaction as TFB30Transaction,tableName,columnName);
end;

function TFB30Attachment.GetArrayMetaData(Transaction: ITransaction; tableName,
  columnName: AnsiString): IArrayMetaData;
begin
  CheckHandle;
  Result := TFB30ArrayMetaData.Create(self,Transaction as TFB30Transaction,tableName,columnName);
end;

procedure TFB30Attachment.getFBVersion(version: TStrings);
var bufferObj: TVersionCallback;
begin
  version.Clear;
  bufferObj := TVersionCallback.Create(Firebird30ClientAPI,version);
  try
    with FFirebird30ClientAPI do
    begin
       UtilIntf.getFbVersion(StatusIntf,FAttachmentIntf,bufferObj);
       Check4DataBaseError;
    end;
  finally
    bufferObj.Free;
  end;
end;

function TFB30Attachment.HasDecFloatSupport: boolean;
begin
  Result := (FFirebird30ClientAPI.GetClientMajor >= 4) and
   (GetODSMajorVersion >= 13);
end;

function TFB30Attachment.GetTimeZoneServices: ITimeZoneServices;
begin
  if not HasTimeZoneSupport then
    IBError(ibxeNotSupported,[]);

  if FTimeZoneServices = nil then
    FTimeZoneServices := TFB30TimeZoneServices.Create(self);
  Result := FTimeZoneServices;
end;

function TFB30Attachment.HasTimeZoneSupport: boolean;
begin
  Result := (FFirebird30ClientAPI.GetClientMajor >= 4) and
   (GetODSMajorVersion >= 13);
end;

end.

