(*
 *  Firebird UDR Support (fbudrtested). The fbudr components provide a set of
 *  Pascal language bindings for the Firebird API in support of server
 *  side User Defined Routines (UDRs). The fbudr package is an extension
 *  to the Firebird Pascal API.
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
 *  The Original Code is (C) 2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBUdrPlugin;

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
  Classes, SysUtils, Firebird, IB, FBUDRIntf, FBUDRController, FB30Statement;

type
  TFBUdrPluginEmulator = class;

  { TEmulatedExternalContext }

  TEmulatedExternalContext = class(Firebird.IExternalContextImpl)
  private
    FAttachmentIntf: Firebird.IAttachment;
    FStatement: IStatement;
    FTransaction: ITransaction;
    FUserNameBuffer: Ansistring;
    FClientCharSet: AnsiString;
    FDatabaseName: AnsiString;
  public
    constructor Create(aStatement: IStatement);
    destructor Destroy; override;
    property Transaction: ITransaction read FTransaction write FTransaction;
  public
    {IExternalContext}
    function getMaster(): Firebird.IMaster; override;
    function getEngine(status: Firebird.IStatus): Firebird.IExternalEngine; override;
    function getAttachment(status: Firebird.IStatus): Firebird.IAttachment; override;
    function getTransaction(status: Firebird.IStatus): Firebird.ITransaction; override;
    function getUserName(): PAnsiChar; override;
    function getDatabaseName(): PAnsiChar; override;
    function getClientCharSet(): PAnsiChar; override;
    function obtainInfoCode(): Integer; override;
    function getInfo(code: Integer): Pointer; override;
    function setInfo(code: Integer; value: Pointer): Pointer; override;
  end;

  { TEmulatedRoutineMetadata }

  TEmulatedRoutineMetadata = class(IRoutineMetadataImpl)
  private
    FManager: TFBUdrPluginEmulator;
    FName: AnsiString;
    FPackageName: AnsiString;
    FStatement: IStatement;
    FEntryPoint: AnsiString;
    FTableName: AnsiString;
    FTriggerType: cardinal;
    FInputMetadata: firebird.IMessageMetadata;
    FOutputMetadata: firebird.IMessageMetadata;
    FTriggerMetadata: firebird.IMessageMetadata;
  public
    constructor Create(aManager: TFBUdrPluginEmulator; aName, aPackageName, aEntryPoint: AnsiString; aStatement: IStatement);
    destructor Destroy; override;
    procedure SetTriggerInfo(aTableName: AnsiString; aTriggerType: cardinal);
  public
    {IRoutineMetadata}
    function getPackage(status: Firebird.IStatus): PAnsiChar; override;
    function getName(status: Firebird.IStatus): PAnsiChar; override;
    function getEntryPoint(status: Firebird.IStatus): PAnsiChar; override;
    function getBody(status: Firebird.IStatus): PAnsiChar; override;
    function getInputMetadata(status: Firebird.IStatus): IMessageMetadata; override;
    function getOutputMetadata(status: Firebird.IStatus): IMessageMetadata; override;
    function getTriggerMetadata(status: Firebird.IStatus): IMessageMetadata; override;
    function getTriggerTable(status: Firebird.IStatus): PAnsiChar; override;
    function getTriggerType(status: Firebird.IStatus): Cardinal; override;
  end;

  { TExternalWrapper }

  TExternalWrapper = class
  protected
    FManager: TFBUdrPluginEmulator;
    FName: AnsiString;
    FPreparedStatement: IStatement;
    FContext: TEmulatedExternalContext;
    FRoutineMetadata: TEmulatedRoutineMetadata;
    FStatus: Firebird.IStatus;
    FInputParams: ISQLParams;
    procedure CheckStatus;
    procedure ChangeResultsCharset(FromID, toID: integer);
    procedure Setup;
    procedure DoSetup(status: Firebird.IStatus;
                      context: Firebird.IExternalContext;
                      metadata: Firebird.IRoutineMetadata;
                      inBuilder: Firebird.IMetadataBuilder;
                      outBuilder: Firebird.IMetadataBuilder); virtual; abstract;
  public
    constructor Create(aManager: TFBUdrPluginEmulator; aName, aPackageName, aEntryPoint: AnsiString;
                                 preparedStmt: IStatement);
    destructor Destroy; override;
  end;

  { TExternalFunctionWrapper }

  TExternalFunctionWrapper = class(TExternalWrapper)
  private
    FFunctionFactory: TFBUDRFunctionFactory;
  protected
    procedure DoSetup(status: Firebird.IStatus;
                      context: Firebird.IExternalContext;
                      metadata: Firebird.IRoutineMetadata;
                      inBuilder: Firebird.IMetadataBuilder;
                      outBuilder: Firebird.IMetadataBuilder); override;
  public
    constructor Create(aManager: TFBUdrPluginEmulator;aName, aPackageName, aEntryPoint: AnsiString;
       aFunctionFactory: TFBUDRFunctionFactory;
       preparedStmt: IStatement);
    function Execute(aTransaction: ITransaction): ISQLData;
    property InputParams: ISQLParams read FInputParams;
  end;

  {IProcedureResults is a cut down version of IResultsSet}

  IProcedureResults = interface
    ['{1b851373-a7c2-493e-b457-6a19980e0f5f}']
    function getCount: integer;
    function ByName(Idx: AnsiString): ISQLData;
    function getSQLData(index: integer): ISQLData;
    function FetchNext: boolean; {fetch next record}
    function IsEof: boolean;
    property Data[index: integer]: ISQLData read getSQLData; default;
    property Count: integer read getCount;
  end;

  { TExternalProcedureWrapper }

  TExternalProcedureWrapper = class(TExternalWrapper)
  private
    FProcedureFactory: TFBUDRProcedureFactory;
  protected
    procedure DoSetup(status: Firebird.IStatus;
                      context: Firebird.IExternalContext;
                      metadata: Firebird.IRoutineMetadata;
                      inBuilder: Firebird.IMetadataBuilder;
                      outBuilder: Firebird.IMetadataBuilder); override;
  public
    constructor Create(aManager: TFBUdrPluginEmulator; aName, aPackageName, aEntryPoint: AnsiString;
       aProcedureFactory: TFBUDRProcedureFactory;
       preparedStmt: IStatement);
    function Execute(aTransaction: ITransaction): IProcedureResults;
    property InputParams: ISQLParams read FInputParams;
  end;

  { TFBTriggerSQLDA }

  TFBTriggerSQLDA = class(TIBXINPUTSQLDA)
  private
    FAttachment: IAttachment;
  protected
    function GetAttachment: IAttachment; override;
    function CanChangeMetaData: boolean; override;
  public
    {created with the input messge metadata and a pointer to the inMsg buffer}
    constructor Create(att: IAttachment; aMetadata: Firebird.IMessageMetaData);
    procedure Finalise;
  end;

  { TExternalTriggerWrapper }

  TExternalTriggerWrapper = class(TExternalWrapper)
  private
    FTriggerFactory: TFBUDRTriggerFactory;
    FTriggerOldSQLDA: TFBTriggerSQLDA;
    FTriggerNewSQLDA: TFBTriggerSQLDA;
    FOldValues: IFBUDROutputData;
    FNewValues: IFBUDROutputData;
  protected
    procedure DoSetup(status: Firebird.IStatus;
                      context: Firebird.IExternalContext;
                      metadata: Firebird.IRoutineMetadata;
                      inBuilder: Firebird.IMetadataBuilder;
                      outBuilder: Firebird.IMetadataBuilder); override;
  public
    constructor Create(aManager: TFBUdrPluginEmulator; aName,  aTableName, aEntryPoint: AnsiString;
       aTriggerType: cardinal;
       aTriggerFactory: TFBUDRTriggerFactory;
       preparedStmt: IStatement);
    destructor Destroy; override;
    procedure Execute(aTransaction: ITransaction; action: cardinal);
    property OldValues: IFBUDROutputData read FOldValues;
    property NewValues: IFBUDROutputData read FNewValues;
  end;

  { TFBUdrPluginEmulator }

  TFBUdrPluginEmulator = class(Firebird.IUdrPluginImpl)
  private
    FModuleName: AnsiString;
    FTheirUnloadFlag: booleanPtr;
    FMyUnloadFlag: boolean;
    FStatus: Firebird.IStatus;
    FAttachment: IAttachment;
    FFunctionFactories: TStringList;
    FProcedureFactories: TStringList;
    FTriggerFactories: TStringList;
    procedure CheckStatus;
    procedure FreeList(var list: TStringList);
    function CreateSelectFunctionSQL(aFunctionName: AnsiString): AnsiString;
    function CreateExecProcedureSQL(aProcName: AnsiString): AnsiString;
    procedure SetAttachment(AValue: IAttachment);
  public
    {IUdrPluginImpl}
    function getMaster(): IMaster; override;
    procedure registerFunction(status: Firebird.IStatus; name: PAnsiChar; factory: Firebird.IUdrFunctionFactory); override;
    procedure registerProcedure(status: Firebird.IStatus; name: PAnsiChar; factory: Firebird.IUdrProcedureFactory); override;
    procedure registerTrigger(status: Firebird.IStatus; name: PAnsiChar; factory: Firebird.IUdrTriggerFactory); override;
  public
    constructor Create(aModuleName: AnsiString);
    destructor Destroy; override;
    function makeFunction(aFunctionName, aPackageName,
      aEntryPoint: AnsiString): TExternalFunctionWrapper;
    function makeProcedure(aProcName, aPackageName, aEntryPoint: AnsiString): TExternalProcedureWrapper;
    function makeTrigger(aName, aEntryPoint, datasetName: AnsiString; aTriggerType: cardinal
      ): TExternalTriggerWrapper;
    property Attachment: IAttachment read FAttachment write SetAttachment;
    property ModuleName: AnsiString read FModuleName;
  end;

implementation

uses FBClientLib, IBUtils, FB30Attachment, FB30Transaction,
     FBSQLData, FBUDRUtils;

resourcestring
  SNoMasterInterface = 'A Master Interface is required - legacy API not supported';
  SNoAttachment = 'An attachment must be provided before a statement can be prepared';

type
  { TProcedureResults }

  TProcedureResults = class(TInterfacedObject,IProcedureResults)
  private
    FExternalResultSet: Firebird.IExternalResultSet;
    FResults: IResults;
    FIsEof: boolean;
    FManager: TFBUdrPluginEmulator;
  public
    constructor Create(aManager: TFBUdrPluginEmulator;
      aExternalResultSet: Firebird.IExternalResultSet;
  aSQLRecord: TIBXOUTPUTSQLDA);
    destructor Destroy; override;
  public
    {IProcedureResults}
    function getCount: integer;
    function ByName(Idx: AnsiString): ISQLData;
    function getSQLData(index: integer): ISQLData;
    function FetchNext: boolean; {fetch next record}
    function IsEof: boolean;
  end;

{ TFBTriggerSQLDA }

function TFBTriggerSQLDA.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TFBTriggerSQLDA.CanChangeMetaData: boolean;
begin
  Result := false;
end;

constructor TFBTriggerSQLDA.Create(att: IAttachment;
  aMetadata: Firebird.IMessageMetaData);
begin
  inherited Create(FirebirdAPI);
  FAttachment := att;
  Bind(aMetaData);
end;

procedure TFBTriggerSQLDA.Finalise;
begin
  PackBuffer;
end;

{ TExternalTriggerWrapper }

procedure TExternalTriggerWrapper.DoSetup(status: Firebird.IStatus;
  context: Firebird.IExternalContext; metadata: Firebird.IRoutineMetadata;
  inBuilder: Firebird.IMetadataBuilder; outBuilder: Firebird.IMetadataBuilder);
begin
  FTriggerFactory.setup(status,context,metadata,outBuilder);
end;

constructor TExternalTriggerWrapper.Create(aManager: TFBUdrPluginEmulator; aName,
  aTableName, aEntryPoint: AnsiString; aTriggerType: cardinal;
  aTriggerFactory: TFBUDRTriggerFactory; preparedStmt: IStatement);
begin
  inherited Create(aManager,aName,'',aEntryPoint, preparedStmt);
  FTriggerFactory := aTriggerFactory;
  FRoutineMetadata.SetTriggerInfo(aTableName,aTriggerType);
  FTriggerOldSQLDA := TFBTriggerSQLDA.Create(FManager.Attachment,(preparedStmt as TFB30Statement).SQLRecord.GetMetaData);
  FTriggerNewSQLDA := TFBTriggerSQLDA.Create(FManager.Attachment,(preparedStmt as TFB30Statement).SQLRecord.GetMetaData);
  FOldValues := TFBUDROutputParams.Create(FTriggerOldSQLDA);
  FOldValues.Clear;
  FNewValues := TFBUDROutputParams.Create(FTriggerNewSQLDA);
  FNewValues.Clear;
end;

destructor TExternalTriggerWrapper.Destroy;
begin
  FOldValues := nil;
  FNewValues := nil;
  if FTriggerOldSQLDA <> nil then
    FTriggerOldSQLDA.Free;
  if FTriggerNewSQLDA <> nil then
    FTriggerNewSQLDA.Free;
  inherited Destroy;
end;

procedure TExternalTriggerWrapper.Execute(aTransaction: ITransaction;
  action: cardinal);
var aTriggerInstance: Firebird.IExternalTrigger;
    Buffer: array [0..512] of AnsiChar;
begin
  (FContext as TEmulatedExternalContext).Transaction := aTransaction;
  try
    Setup;
    aTriggerInstance := FTriggerFactory.newItem(FStatus,FContext,FRoutineMetadata);
    try
    Buffer[0] := #0;
    aTriggerInstance.getCharSet(FStatus,FContext,@Buffer,sizeof(Buffer)); {The UDR engine does this thus so do we}
    CheckStatus;
    FTriggerOldSQLDA.Finalise;
    FTriggerNewSQLDA.Finalise;
    aTriggerInstance.execute(FStatus,FContext,action,
                             FTriggerOldSQLDA.MessageBuffer,
                             FTriggerNewSQLDA.MessageBuffer
                             );
    finally
      aTriggerInstance.dispose;
    end;
  finally
    (FContext as TEmulatedExternalContext).Transaction := nil;
  end;
end;

{ TExternalProcedureWrapper }

procedure TExternalProcedureWrapper.DoSetup(status: Firebird.IStatus;
  context: Firebird.IExternalContext; metadata: Firebird.IRoutineMetadata;
  inBuilder: Firebird.IMetadataBuilder; outBuilder: Firebird.IMetadataBuilder);
begin
  FProcedureFactory.setup(status,context,metadata,inBuilder,outBuilder);
end;

constructor TExternalProcedureWrapper.Create(aManager: TFBUdrPluginEmulator; aName,
  aPackageName, aEntryPoint: AnsiString;
  aProcedureFactory: TFBUDRProcedureFactory; preparedStmt: IStatement);
begin
  inherited Create(aManager,aName, aPackageName, aEntryPoint, preparedStmt);
  FProcedureFactory := aProcedureFactory;
end;

function TExternalProcedureWrapper.Execute(aTransaction: ITransaction
  ): IProcedureResults;
var aProcedureInstance: Firebird.IExternalProcedure;
    Buffer: array [0..512] of AnsiChar;
    ResultsSet: IExternalResultSet;
    OutputData: IResults;
begin
  Result := nil;
  (FContext as TEmulatedExternalContext).Transaction := aTransaction;
  try
    Setup;
    aProcedureInstance := FProcedureFactory.newItem(FStatus,FContext,FRoutineMetadata);
    try
      Buffer[0] := #0;
      aProcedureInstance.getCharSet(FStatus,FContext,@Buffer,sizeof(Buffer));
      CheckStatus;
      ResultsSet := aProcedureInstance.open(FStatus,FContext,
                        (FPreparedStatement as TFB30Statement).SQLParams.MessageBuffer,
                        (FPreparedStatement as TFB30Statement).SQLRecord.MessageBuffer);
      CheckStatus;
      if ResultsSet <> nil then
        Result := TProcedureResults.Create(FManager,ResultsSet,(FPreparedStatement as TFB30Statement).SQLRecord);
    finally
      aProcedureInstance.dispose;
    end;
  finally
    (FContext as TEmulatedExternalContext).Transaction := nil;
  end;
end;

{ TProcedureResults }

constructor TProcedureResults.Create(aManager: TFBUdrPluginEmulator;
  aExternalResultSet: Firebird.IExternalResultSet; aSQLRecord: TIBXOUTPUTSQLDA);
begin
  inherited Create;
  FManager := aManager;
  FExternalResultSet := aExternalResultSet;
  FResults := TResults.Create(aSQLRecord);
end;

destructor TProcedureResults.Destroy;
begin
  if FExternalResultSet <> nil then
    FExternalResultSet.dispose;
  inherited Destroy;
end;

function TProcedureResults.getCount: integer;
begin
  Result := FResults.Count;
end;

function TProcedureResults.ByName(Idx: AnsiString): ISQLData;
begin
  Result := FResults.ByName(Idx);
end;

function TProcedureResults.getSQLData(index: integer): ISQLData;
begin
  Result := FResults.getSQLData(index);
end;

function TProcedureResults.FetchNext: boolean;
begin
  Result := FExternalResultSet.fetch(FManager.FStatus);
  FManager.CheckStatus;
  FIsEof := not Result;
end;

function TProcedureResults.IsEof: boolean;
begin
  Result := FIsEOF;
end;

{ TExternalWrapper }

procedure TExternalWrapper.CheckStatus;
var buffer: array [0..4096] of AnsiChar;
begin
  with FStatus do
    if (getState and STATE_ERRORS) <> 0 then
    begin
      FManager.getMaster.getUtilInterface.formatStatus(@buffer,sizeof(buffer),FStatus);
      raise Exception.Create(strpas(PAnsiChar(@buffer)));
    end;
end;

procedure TExternalWrapper.ChangeResultsCharset(FromID, toID: integer);
var i: integer;
begin
  with (FPreparedStatement as TFB30Statement) do
   for i := 0 to SQLRecord.Count - 1 do
     if SQLRecord.Column[i].CharSetID = FromID then
        SQLRecord.Column[i].CharSetID := ToID;
end;

procedure TExternalWrapper.Setup;
var inBuilder: Firebird.IMetadataBuilder;
    outBuilder: Firebird.IMetadataBuilder;
    inMetadata: Firebird.IMessageMetadata;
    outMetadata: Firebird.IMessageMetadata;
begin
  inMetadata := FRoutineMetadata.getInputMetadata(FStatus);
  CheckStatus;
  if inMetadata <> nil then
  try
    inBuilder := inMetadata.getBuilder(FStatus);
    CheckStatus;
  finally
    inMetadata.release;
  end
  else
    inBuilder := nil;

  outMetadata := FRoutineMetadata.getOutputMetadata(FStatus);
  CheckStatus;
  if outMetadata <> nil then
  try
    outBuilder := outMetadata.getBuilder(FStatus);
    CheckStatus;
  finally
    outMetadata.release;
  end
  else
    outBuilder := nil;
  DoSetup(FStatus,FContext,FRoutineMetadata,inBuilder,outBuilder);
  CheckStatus;
end;

constructor TExternalWrapper.Create(aManager: TFBUdrPluginEmulator; aName,
  aPackageName, aEntryPoint: AnsiString; preparedStmt: IStatement);
begin
  inherited Create;
  FManager := aManager;
  FName := aName;
  FPreparedStatement := preparedStmt;
  FContext := TEmulatedExternalContext.Create(FPreparedStatement);
  FRoutineMetadata := TEmulatedRoutineMetadata.Create(aManager,aName,aPackageName,aEntryPoint,FPreparedStatement);
  FStatus := FContext.getMaster.getStatus;
  FInputParams := FPreparedStatement.SQLParams;
end;

destructor TExternalWrapper.Destroy;
begin
  if FContext <> nil then
    FContext.Free;
  if FRoutineMetadata <> nil then
    FRoutineMetadata.Free;
  if FStatus <> nil then
    FStatus.dispose;
  inherited Destroy;
end;

{ TEmulatedRoutineMetadata }

constructor TEmulatedRoutineMetadata.Create(aManager: TFBUdrPluginEmulator; aName,
  aPackageName, aEntryPoint: AnsiString; aStatement: IStatement);
begin
  inherited Create;
  FManager := aManager;
  FName := aName;
  FPackageName := aPackageName;
  FEntryPoint := aEntryPoint;
  FStatement := aStatement;
end;

destructor TEmulatedRoutineMetadata.Destroy;
begin
  if FInputMetadata <> nil then
    FInputMetadata.release;
  if FOutputMetadata <> nil then
    FOutputMetadata.release;
  if FTriggerMetadata <> nil then
    FTriggerMetadata.release;
  inherited Destroy;
end;

procedure TEmulatedRoutineMetadata.SetTriggerInfo(aTableName: AnsiString;
  aTriggerType: cardinal);
begin
  FTableName := aTableName;
  FTriggerType := aTriggerType;
end;

function TEmulatedRoutineMetadata.getPackage(status: Firebird.IStatus
  ): PAnsiChar;
begin
  Result := PAnsiChar(FPackageName);
end;

function TEmulatedRoutineMetadata.getName(status: Firebird.IStatus): PAnsiChar;
begin
  Result := PAnsiChar(FName);
end;

function TEmulatedRoutineMetadata.getEntryPoint(status: Firebird.IStatus
  ): PAnsiChar;
begin
  Result := PAnsiChar(FEntryPoint);
end;

function TEmulatedRoutineMetadata.getBody(status: Firebird.IStatus): PAnsiChar;
begin
  Result := nil;
end;

function TEmulatedRoutineMetadata.getInputMetadata(status: Firebird.IStatus
  ): IMessageMetadata;
begin
  if (FTriggerType = 0) and (FInputMetadata = nil) then
    FInputMetadata := (FStatement as TFB30Statement).SQLParams.GetMetaData ;
  Result := FInputMetadata;
  if Result <> nil then
    Result.addRef();
end;

function TEmulatedRoutineMetadata.getOutputMetadata(status: Firebird.IStatus
  ): IMessageMetadata;
begin
  if (FTriggerType = 0) and (FOutputMetadata = nil) then
    FOutputMetadata := (FStatement as TFB30Statement).SQLRecord.GetMetaData;
  Result := FOutputMetadata;
  if Result <> nil then
    Result.addRef();
end;

function TEmulatedRoutineMetadata.getTriggerMetadata(status: Firebird.IStatus
  ): IMessageMetadata;
begin
  if (FTriggerType <> 0) and (FTriggerMetadata = nil) then
    FTriggerMetadata := (FStatement as TFB30Statement).SQLRecord.GetMetaData;
  Result := FTriggerMetadata;
  if Result <> nil then
    Result.addRef();
end;

function TEmulatedRoutineMetadata.getTriggerTable(status: Firebird.IStatus
  ): PAnsiChar;
begin
  Result := PAnsiChar(FTableName);
end;

function TEmulatedRoutineMetadata.getTriggerType(status: Firebird.IStatus
  ): Cardinal;
begin
  Result := FTriggerType;
end;

{ TExternalFunctionWrapper }

constructor TExternalFunctionWrapper.Create(aManager: TFBUdrPluginEmulator; aName,
  aPackageName, aEntryPoint: AnsiString;
  aFunctionFactory: TFBUDRFunctionFactory; preparedStmt: IStatement);
begin
  inherited Create(aManager,aName, aPackageName, aEntryPoint, preparedStmt);
  FFunctionFactory := aFunctionFactory;
end;

function TExternalFunctionWrapper.Execute(aTransaction: ITransaction): ISQLData;
var aFunctionInstance: Firebird.IExternalFunction;
    Buffer: array [0..512] of AnsiChar;
    CodePage: TSystemCodePage;
    OutputData: IResults;
begin
  (FContext as TEmulatedExternalContext).Transaction := aTransaction;
  try
    Setup;
    aFunctionInstance := FFunctionFactory.newItem(FStatus,FContext,FRoutineMetadata);
    try
      Buffer[0] := #0;
      aFunctionInstance.getCharSet(FStatus,FContext,@Buffer,sizeof(Buffer));
      CheckStatus;
      aFunctionInstance.execute(FStatus,FContext,
                        (FPreparedStatement as TFB30Statement).SQLParams.MessageBuffer,
                        (FPreparedStatement as TFB30Statement).SQLRecord.MessageBuffer);
      CheckStatus;
      OutputData := TResults.Create( (FPreparedStatement as TFB30Statement).SQLRecord);
      Result := OutputData[0];
    finally
      aFunctionInstance.dispose;
    end;
  finally
    (FContext as TEmulatedExternalContext).Transaction := nil;
  end;
end;

procedure TExternalFunctionWrapper.DoSetup(status: Firebird.IStatus;
  context: Firebird.IExternalContext; metadata: Firebird.IRoutineMetadata;
  inBuilder: Firebird.IMetadataBuilder; outBuilder: Firebird.IMetadataBuilder);
begin
  FFunctionFactory.setup(status,context,metadata,inBuilder,outBuilder);
end;

{ TEmulatedExternalContext }

constructor TEmulatedExternalContext.Create(aStatement: IStatement);
begin
  inherited Create;
  FStatement := aStatement;
  FAttachmentIntf := (FStatement.GetAttachment as TFB30Attachment).AttachmentIntf;
  FAttachmentIntf.addRef;
end;

destructor TEmulatedExternalContext.Destroy;
begin
  if FAttachmentIntf <> nil then
    FAttachmentIntf.release;
  inherited Destroy;
end;

function TEmulatedExternalContext.getMaster(): Firebird.IMaster;
var MasterProvider: IFBIMasterProvider;
begin
  if FirebirdAPI.HasMasterIntf and (FirebirdAPI.QueryInterface(IFBIMasterProvider,MasterProvider) = S_OK) then
    Result := MasterProvider.GetIMasterIntf
  else
    Result := nil;
end;

function TEmulatedExternalContext.getEngine(status: Firebird.IStatus
  ): Firebird.IExternalEngine;
begin
  Result := nil;
end;

function TEmulatedExternalContext.getAttachment(status: Firebird.IStatus
  ): Firebird.IAttachment;
begin
  Result := FAttachmentIntf;
end;

function TEmulatedExternalContext.getTransaction(status: Firebird.IStatus
  ): Firebird.ITransaction;
begin
  Result := (FTransaction as TFB30Transaction).TransactionIntf;
end;

function TEmulatedExternalContext.getUserName(): PAnsiChar;
var DPB: IDPB;
    DPBItem: IDPBItem;
begin
  Result := '';
  DPB := FStatement.GetAttachment.getDPB;
  DPBItem := DPB.Find(isc_dpb_user_name);
  if DPBItem <> nil then
  begin
    FUserNameBuffer := DPBItem.AsString;
    Result := PAnsiChar(FUserNameBuffer);
  end;
end;

function TEmulatedExternalContext.getDatabaseName(): PAnsiChar;
var ServerName: AnsiString;
    Protocol: TProtocolAll;
    PortNo: AnsiString;
begin
  Result := '';
  if ParseConnectString(FStatement.getAttachment.GetConnectString,
      ServerName,FDatabaseName,Protocol,PortNo) then
        Result := PAnsiChar(FDatabaseName);
end;

function TEmulatedExternalContext.getClientCharSet(): PAnsiChar;
var DPB: IDPB;
    DPBItem: IDPBItem;
begin
  Result := '';
  DPB := FStatement.GetAttachment.getDPB;
  DPBItem := DPB.Find(isc_dpb_lc_ctype);
  if DPBItem <> nil then
  begin
    FClientCharSet := DPBItem.AsString;
    Result := PAnsiChar(FClientCharSet);
  end;
end;

function TEmulatedExternalContext.obtainInfoCode(): Integer;
begin
  Result := 0;
end;

function TEmulatedExternalContext.getInfo(code: Integer): Pointer;
begin
  Result := nil;
end;

function TEmulatedExternalContext.setInfo(code: Integer; value: Pointer
  ): Pointer;
begin
  Result := nil;
end;

{ TFBUdrPluginEmulator }

function TFBUdrPluginEmulator.getMaster(): IMaster;
var MasterProvider: IFBIMasterProvider;
begin
  if FirebirdAPI.HasMasterIntf and (FirebirdAPI.QueryInterface(IFBIMasterProvider,MasterProvider) = S_OK) then
    Result := MasterProvider.GetIMasterIntf
  else
    Result := nil;
end;

procedure TFBUdrPluginEmulator.registerFunction(status: Firebird.IStatus;
  name: PAnsiChar; factory: Firebird.IUdrFunctionFactory);
var factoryImpl: Firebird.IUdrFunctionFactoryImpl;
begin
  factoryImpl := factory;
  FFunctionFactories.AddObject(strpas(name),factoryImpl);
end;

procedure TFBUdrPluginEmulator.registerProcedure(status: Firebird.IStatus;
  name: PAnsiChar; factory: Firebird.IUdrProcedureFactory);
var factoryImpl: Firebird.IUdrProcedureFactoryImpl;
begin
  factoryImpl := factory;
  FProcedureFactories.AddObject(strpas(name),factoryImpl);
end;

procedure TFBUdrPluginEmulator.registerTrigger(status: Firebird.IStatus;
  name: PAnsiChar; factory: Firebird.IUdrTriggerFactory);
var factoryImpl: Firebird.IUdrTriggerFactoryImpl;
begin
  factoryImpl := factory;
  FTriggerFactories.AddObject(strpas(name),factoryImpl);
end;

constructor TFBUdrPluginEmulator.Create(aModuleName: AnsiString);
begin
  inherited Create;
  FModuleName := aModuleName;
  FStatus := GetMaster.getStatus;
  FFunctionFactories := TStringList.Create;
  FProcedureFactories := TStringList.Create;
  FTriggerFactories := TStringList.Create;
  FTheirUnloadFlag := firebird_udr_plugin(FStatus,@FMyUnloadFlag,self);
  CheckStatus;
end;

destructor TFBUdrPluginEmulator.Destroy;
begin
  FreeList(FFunctionFactories);
  FreeList(FProcedureFactories);
  FreeList(FTriggerFactories);
  if FStatus <> nil then
    FStatus.dispose();
  inherited Destroy;
end;

procedure TFBUdrPluginEmulator.CheckStatus;
var buffer: array [0..4096] of AnsiChar;
begin
  with FStatus do
    if (getState and STATE_ERRORS) <> 0 then
    begin
      getMaster.getUtilInterface.formatStatus(@buffer,sizeof(buffer),FStatus);
      raise Exception.Create(strpas(PAnsiChar(@buffer)));
    end;
end;

procedure TFBUdrPluginEmulator.FreeList(var list: TStringList);
var i: integer;
    obj: TObject;
begin
  if List = nil then Exit;

  for i := 0 to List.Count - 1 do
  begin
    obj := List.Objects[i];
    if obj <> nil then
    begin
      if obj is TFBUDRFunctionFactory then
       TFBUDRFunctionFactory(obj).dispose
      else
      if obj is TFBUDRProcedureFactory then
        TFBUDRProcedureFactory(obj).dispose
      else
      if obj is TFBUDRTriggerFactory then
        TFBUDRTriggerFactory(obj).dispose;
    end;
  end;
  FreeAndNil(List);
end;

function TFBUdrPluginEmulator.CreateSelectFunctionSQL(aFunctionName: AnsiString
  ): AnsiString;
const
  FunctionArgsSQL =
    'SELECT * FROM RDB$FUNCTION_ARGUMENTS RFA JOIN RDB$FIELDS FLD ' +
    'ON RFA.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME '+
    'WHERE RDB$FUNCTION_NAME = ? ' +
    'ORDER BY RDB$ARGUMENT_POSITION';
var args: IResultset;
    arglist: AnsiString;
    separator: AnsiString;
begin
  if not FAttachment.HasFunction(aFunctionName) then
    aFunctionName := SafeAnsiUpperCase(aFunctionName);
  args := FAttachment.OpenCursorAtStart(FunctionArgsSQL,[aFunctionName]);
  arglist := '';
  separator := ':';
  while not args.IsEof do
  begin
    if args.ByName('RDB$ARGUMENT_POSITION').AsInteger > 0 then
    begin
      arglist := arglist + separator + Trim(args.ByName('RDB$ARGUMENT_NAME').AsString);
      separator := ', :';
    end;
    args.FetchNext;
  end;
  Result := 'Select ' + QuoteIdentifierIfNeeded(FAttachment.GetSQLDialect,aFunctionName) + '(' + arglist + ') From RDB$DATABASE';
end;

function TFBUdrPluginEmulator.CreateExecProcedureSQL(aProcName: AnsiString): AnsiString;
const
  sGetProcArgsSQL =
    'SELECT * ' +
    ' FROM RDB$PROCEDURE_PARAMETERS PRM JOIN RDB$FIELDS FLD ON ' +
    ' PRM.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME ' +
    'WHERE ' +
    '    PRM.RDB$PROCEDURE_NAME = ? AND ' +
    '    PRM.RDB$PARAMETER_TYPE = 0 ' +
    'ORDER BY PRM.RDB$PARAMETER_NUMBER';

  sGetProcType = 'Select RDB$PROCEDURE_TYPE FROM RDB$PROCEDURES ' +
                 'Where Trim(RDB$PROCEDURE_NAME) = ?';

var args: IResultset;
    arglist: AnsiString;
    separator: AnsiString;
    ProcType: integer;
begin
  if not FAttachment.HasProcedure(aProcName) then
    aProcName := SafeAnsiUpperCase(aProcName);
  args := FAttachment.OpenCursorAtStart(sGetProcArgsSQL,[aProcName]);
  arglist := '';
  separator := ':';
  while not args.IsEof do
  begin
    arglist := arglist + separator + Trim(args.ByName('RDB$PARAMETER_NAME').AsString);
    separator := ', :';
    args.FetchNext;
  end;
  ProcType := FAttachment.OpenCursorAtStart(sGetProcType,[aProcName])[0].AsInteger;
  case ProcType of
  1:
    if arglist <> '' then
      Result := 'Select * From ' + QuoteIdentifierIfNeeded(FAttachment.GetSQLDialect,aProcName) + '(' + arglist + ')'
    else
      Result := 'Select * From ' + QuoteIdentifierIfNeeded(FAttachment.GetSQLDialect,aProcName);
  2:
    if arglist <> '' then
      Result := 'Execute Procedure ' + QuoteIdentifierIfNeeded(FAttachment.GetSQLDialect,aProcName) + '(' + arglist + ')'
    else
      Result := 'Execute Procedure ' + QuoteIdentifierIfNeeded(FAttachment.GetSQLDialect,aProcName);
  else
    raise Exception.CreateFmt('Unknown Procedure Type %d for %s',[ProcType,aProcName]);

  end;
end;

procedure TFBUdrPluginEmulator.SetAttachment(AValue: IAttachment);
begin
  if FAttachment = AValue then Exit;
  if (AValue = nil) or (AValue.getFirebirdAPI = nil) or not AValue.getFirebirdAPI.HasMasterIntf then
    raise Exception.Create(SNoMasterInterface);
  FAttachment := AValue;
end;

function TFBUdrPluginEmulator.makeFunction(aFunctionName, aPackageName,
  aEntryPoint: AnsiString): TExternalFunctionWrapper;
var index: integer;
    aTransaction: ITransaction;
    aModuleName,aRoutineName,aInfo: AnsiString;
begin
  Result := nil;
  if FAttachment = nil then
    raise Exception.Create(SNoAttachment);
  TFBUDRRoutineMetadata.ParseEntryPoint(aEntryPoint,aModuleName,aRoutineName,aInfo);
  index := FFunctionFactories.IndexOf(aRoutineName);
  if (index <> -1) and (FFunctionFactories.Objects[index] <> nil) then
  begin
    aTransaction := FAttachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
    Result := TExternalFunctionWrapper.Create(self,aFunctionName, aPackageName, aEntryPoint,
                   FFunctionFactories.Objects[index] as TFBUDRFunctionFactory,
                   FAttachment.PrepareWithNamedParameters(aTransaction,
                   CreateSelectFunctionSQL(aFunctionName),
                   true));
  end;
end;

function TFBUdrPluginEmulator.makeProcedure(aProcName, aPackageName,
  aEntryPoint: AnsiString): TExternalProcedureWrapper;
var index: integer;
    aTransaction: ITransaction;
    aModuleName,aRoutineName,aInfo: AnsiString;
begin
  Result := nil;
  if FAttachment = nil then
    raise Exception.Create(SNoAttachment);
  TFBUDRRoutineMetadata.ParseEntryPoint(aEntryPoint,aModuleName,aRoutineName,aInfo);
  index := FProcedureFactories.IndexOf(aRoutineName);
  if (index <> -1) and (FProcedureFactories.Objects[index] <> nil) then
  begin
    aTransaction := FAttachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
    Result := TExternalProcedureWrapper.Create(self,aProcName, aPackageName, aEntryPoint,
                   FProcedureFactories.Objects[index] as TFBUDRProcedureFactory,
                   FAttachment.PrepareWithNamedParameters(aTransaction,
                   CreateExecProcedureSQL(aProcName),true));
  end;
end;

function TFBUdrPluginEmulator.makeTrigger(aName, aEntryPoint,
  datasetName: AnsiString; aTriggerType: cardinal): TExternalTriggerWrapper;
var index: integer;
    aTransaction: ITransaction;
    sql: AnsiString;
    aModuleName,aRoutineName,aInfo: AnsiString;
begin
  Result := nil;
  sql := 'Select * from ' + datasetName;
  if FAttachment = nil then
    raise Exception.Create(SNoAttachment);
  TFBUDRRoutineMetadata.ParseEntryPoint(aEntryPoint,aModuleName,aRoutineName,aInfo);
  index := FTriggerFactories.IndexOf(aRoutineName);
  if (index <> -1) and (FTriggerFactories.Objects[index] <> nil) then
  begin
    aTransaction := FAttachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
    Result := TExternalTriggerWrapper.Create(self,aName, datasetName, aEntryPoint, aTriggerType,
                   FTriggerFactories.Objects[index] as TFBUDRTriggerFactory,
                   FAttachment.PrepareWithNamedParameters(aTransaction,sql,true));
  end;
end;

end.

