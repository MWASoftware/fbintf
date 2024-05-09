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
    FOwnsAttachmentHandle: boolean;
    procedure SetAttachmentIntf(AValue: Firebird.IAttachment);
    procedure SetUseRemoteICU(aValue: boolean);
  protected
    procedure CheckHandle; override;
    function GetAttachment: IAttachment; override;
  public
    constructor Create(api: TFB30ClientAPI; DatabaseName: AnsiString; aDPB: IDPB;
          RaiseExceptionOnConnectError: boolean); overload;
    constructor Create(api: TFB30ClientAPI;
      attachment: Firebird.IAttachment; aDatabaseName: AnsiString); overload;
    constructor CreateDatabase(api: TFB30ClientAPI; DatabaseName: AnsiString; aDPB: IDPB; RaiseExceptionOnError: boolean);  overload;
    constructor CreateDatabase(api: TFB30ClientAPI; sql: AnsiString; aSQLDialect: integer;
      RaiseExceptionOnError: boolean); overload;
    function GetDBInfo(ReqBuffer: PByte; ReqBufLen: integer): IDBInformation;
      override;
    property AttachmentIntf: Firebird.IAttachment read FAttachmentIntf write SetAttachmentIntf;
    property Firebird30ClientAPI: TFB30ClientAPI read FFirebird30ClientAPI;

  public
    {IAttachment}
    procedure Connect;
    procedure Disconnect(Force: boolean=false); override;
    function IsConnected: boolean; override;
    procedure DropDatabase;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionCompletion; aName: AnsiString=''): ITransaction; override;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionCompletion; aName: AnsiString=''): ITransaction; override;
    procedure ExecImmediate(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer); override;
    function Prepare(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer; CursorName: AnsiString=''): IStatement; override;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: AnsiString;
                       aSQLDialect: integer; GenerateParamNames: boolean=false;
                       CaseSensitiveParams: boolean=false; CursorName: AnsiString=''): IStatement; override;

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
    function HasBatchMode: boolean; override;
    function HasScollableCursors: boolean;

    {Time Zone Support}
    function GetTimeZoneServices: ITimeZoneServices; override;
    function HasTimeZoneSupport: boolean; override;
  end;

implementation

uses FB30Transaction, FB30Statement, FB30Array, FB30Blob, FBMessages,
  FBOutputBlock, FB30Events, IBUtils, FB30TimeZoneServices
  {$ifdef WINDOWS}, Windows{$endif};

type
  { TVersionCallback }

  TVersionCallback = class(Firebird.IVersionCallbackImpl)
  private
    FConnectionCodePage: TSystemCodePage;
    FOutput: TStrings;
    FFirebirdClientAPI: TFBClientAPI;
  public
    constructor Create(FirebirdClientAPI: TFBClientAPI; output: TStrings;
      aConnectionCodePage: TSystemCodePage);
    procedure callback(status: Firebird.IStatus; text: PAnsiChar); override;
    property ConnectionCodePage: TSystemCodePage read FConnectionCodePage;
  end;

{ TVersionCallback }

constructor TVersionCallback.Create(FirebirdClientAPI: TFBClientAPI;
  output: TStrings; aConnectionCodePage: TSystemCodePage);
begin
  inherited Create;
  FFirebirdClientAPI := FirebirdClientAPI;
  FOutput := output;
  FConnectionCodePage := aConnectionCodePage;
end;

procedure TVersionCallback.callback(status : Firebird.IStatus; text : PAnsiChar
  );
var aStatus: IStatus;
begin
  aStatus := TFB30Status.Create(FFirebirdClientAPI,status);
  if aStatus.InErrorState then
      raise EIBInterBaseError.Create(aStatus,ConnectionCodePage);
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

procedure TFB30Attachment.SetAttachmentIntf(AValue: Firebird.IAttachment);
begin
  if FAttachmentIntf = AValue then Exit;
  if FAttachmentIntf <> nil then
  try
    FAttachmentIntf.release;
  except {ignore - forced release}
  end;
  FOwnsAttachmentHandle := false;
  ClearCachedInfo;
  FTimeZoneServices := nil;
  FAttachmentIntf := AValue;
  if FAttachmentIntf <> nil then
    FAttachmentIntf.AddRef;
end;

procedure TFB30Attachment.CheckHandle;
begin
  if FAttachmentIntf = nil then
    IBError(ibxeDatabaseClosed,[nil]);
end;

function TFB30Attachment.GetAttachment: IAttachment;
begin
  Result := self;
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
    Intf: Firebird.IAttachment;
begin
  inherited Create(api,DatabaseName,aDPB,RaiseExceptionOnError);
  FFirebird30ClientAPI := api;
  IsCreateDB := true;
  if aDPB <> nil then
  begin
    Param := aDPB.Find(isc_dpb_set_db_SQL_dialect);
    if Param <> nil then
      SetSQLDialect(Param.AsByte);
  end;
  sql := GenerateCreateDatabaseSQL(DatabaseName,aDPB);
  with FFirebird30ClientAPI do
  begin
    Intf := UtilIntf.executeCreateDatabase(StatusIntf,Length(sql),
                                       PAnsiChar(sql),SQLDialect,@IsCreateDB);
    if FRaiseExceptionOnConnectError then Check4DataBaseError;
    if not InErrorState then
    begin
      if aDPB <> nil then
      {Connect using known parameters}
      begin
        Intf.Detach(StatusIntf); {releases interface}
        Check4DataBaseError;
        Connect;
      end
      else
        AttachmentIntf := Intf;
      FOwnsAttachmentHandle:= true;
    end;
  end;
end;

constructor TFB30Attachment.CreateDatabase(api: TFB30ClientAPI; sql: AnsiString; aSQLDialect: integer;
  RaiseExceptionOnError: boolean);
var IsCreateDB: boolean;
    Intf: Firebird.IAttachment;
begin
  inherited Create(api,'',nil,RaiseExceptionOnError);
  FFirebird30ClientAPI := api;
  SetSQLDialect(aSQLDialect);
  with FFirebird30ClientAPI do
  begin
    Intf := UtilIntf.executeCreateDatabase(StatusIntf,Length(sql),
                                       PAnsiChar(sql),aSQLDialect,@IsCreateDB);
    if FRaiseExceptionOnConnectError then Check4DataBaseError;
    if InErrorState then
      Exit;
  end;
  AttachmentIntf := Intf;
  FOwnsAttachmentHandle:= true;
  ExtractConnectString(sql,FDatabaseName);
  DPBFromCreateSQL(sql);
end;

constructor TFB30Attachment.Create(api: TFB30ClientAPI;
  attachment: Firebird.IAttachment; aDatabaseName: AnsiString);
begin
  inherited Create(api,aDatabaseName,nil,false);
  FFirebird30ClientAPI := api;
  AttachmentIntf := attachment;
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
var Intf: Firebird.IAttachment;
begin
  with FFirebird30ClientAPI do
  begin
    Intf := ProviderIntf.attachDatabase(StatusIntf,PAnsiChar(FDatabaseName),
                         (DPB as TDPB).getDataLength,
                         BytePtr((DPB as TDPB).getBuffer));
    if FRaiseExceptionOnConnectError then Check4DataBaseError;
    if not InErrorState then
    begin
      AttachmentIntf := Intf;
      FOwnsAttachmentHandle := true;
    end;
  end;
end;

procedure TFB30Attachment.Disconnect(Force: boolean);
begin
  if IsConnected then
  begin
    EndAllTransactions;
    if FOwnsAttachmentHandle then
    with FFirebird30ClientAPI do
    begin
      FAttachmentIntf.Detach(StatusIntf);
      if not Force and InErrorState then
        IBDataBaseError;
    end;
    AttachmentIntf := nil;
  end;
  inherited Disconnect(Force);
end;

function TFB30Attachment.IsConnected: boolean;
begin
  Result := FAttachmentIntf <> nil;
end;

procedure TFB30Attachment.DropDatabase;
begin
  if IsConnected then
  begin
    if not FOwnsAttachmentHandle then
      IBError(ibxeCantDropAcquiredDB,[nil]);
    with FFirebird30ClientAPI do
    begin
      EndAllTransactions;
      EndSession(false);
      FAttachmentIntf.dropDatabase(StatusIntf);
      Check4DataBaseError;
    end;
    AttachmentIntf := nil;
  end;
end;

function TFB30Attachment.StartTransaction(TPB: array of byte;
  DefaultCompletion: TTransactionCompletion; aName: AnsiString): ITransaction;
begin
  CheckHandle;
  Result := TFB30Transaction.Create(FFirebird30ClientAPI,self,TPB,DefaultCompletion, aName);
end;

function TFB30Attachment.StartTransaction(TPB: ITPB;
  DefaultCompletion: TTransactionCompletion; aName: AnsiString): ITransaction;
begin
  CheckHandle;
  Result := TFB30Transaction.Create(FFirebird30ClientAPI,self,TPB,DefaultCompletion,aName);
end;

procedure TFB30Attachment.ExecImmediate(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer);
begin
  CheckHandle;
  if StringCodePage(sql) <> CP_NONE then
    sql := TransliterateToCodePage(sql,CodePage);
  with FFirebird30ClientAPI do
  try
    FAttachmentIntf.execute(StatusIntf,(transaction as TFB30Transaction).TransactionIntf,
                    Length(sql),PAnsiChar(sql),aSQLDialect,nil,nil,nil,nil);
    Check4DataBaseError;
  finally
    if JournalingActive and
      ((joReadOnlyQueries in GetJournalOptions)  or
      (joModifyQueries in GetJournalOptions)) then
      ExecImmediateJnl(sql,transaction);
  end;
end;

function TFB30Attachment.Prepare(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer; CursorName: AnsiString): IStatement;
begin
  CheckHandle;
  Result := TFB30Statement.Create(self,transaction,sql,aSQLDialect,CursorName);
end;

function TFB30Attachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: AnsiString; aSQLDialect: integer; GenerateParamNames: boolean;
  CaseSensitiveParams: boolean; CursorName: AnsiString): IStatement;
begin
  CheckHandle;
  Result := TFB30Statement.CreateWithParameterNames(self,transaction,sql,aSQLDialect,
         GenerateParamNames,CaseSensitiveParams,CursorName);
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
  bufferObj := TVersionCallback.Create(Firebird30ClientAPI,version,CodePage);
  try
    with FFirebird30ClientAPI do
    begin
       UtilIntf.getFbVersion(StatusIntf,FAttachmentIntf,bufferObj);
       Check4DataBaseError(CodePage);
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

function TFB30Attachment.HasBatchMode: boolean;
begin
  Result := FFirebird30ClientAPI.Firebird4orLater and
     (GetODSMajorVersion >= 13);
end;

function TFB30Attachment.HasScollableCursors: boolean;
begin
  Result := (GetODSMajorVersion >= 12);
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

