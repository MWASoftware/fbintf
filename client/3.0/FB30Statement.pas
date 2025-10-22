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
unit FB30Statement;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$CodePage UTF8}
{$interfaces COM}
{$ENDIF}

{This unit is hacked from IBSQL and contains the code for managing an XSQLDA and
 SQLVars, along with statement preparation, execution and cursor management.
 Most of the SQLVar code has been moved to unit FBSQLData. Client access is
 provided through interface rather than direct access to the XSQLDA and XSQLVar
 objects.}

{
  Note on reference counted interfaces.
  ------------------------------------

  TFB30Statement manages both an input and an output SQLDA through the TIBXINPUTSQLDA
  and TIBXOUTPUTSQLDA objects. As pure objects, these are explicitly destroyed
  when the statement is destroyed.

  However, IResultSet is an interface and is returned when a cursor is opened and
  has a reference for the TIBXOUTPUTSQLDA. The   user may discard their reference
  to the IStatement while still using the   IResultSet. This would be a problem if t
  he underlying TFB30Statement object and its TIBXOUTPUTSQLDA is destroyed while
  still leaving the TIBXResultSet object in place. Calls to (e.g.)   FetchNext would fail.

  To avoid this problem, TResultsSet objects  have a reference to the IStatement
  interface of the TFB30Statement object. Thus, as long as these "copies" exist,
  the owning statement is not destroyed even if the user discards their reference
  to the statement. Note: the TFB30Statement does not have a reference to the TIBXResultSet
  interface. This way circular references are avoided.

  To avoid and IResultSet interface being kept to long and no longer synchronised
  with the query, each statement includes a prepare sequence number, incremented
  each time the query is prepared. When the IResultSet interface is created, it
  noted the current prepare sequence number. Whe an IResult interface is accessed
  it checks this number against the statement's current prepare sequence number.
  If not the same, an error is raised.

  A similar strategy is used for the IMetaData, IResults and ISQLParams interfaces.
}

interface

uses
  Classes, SysUtils, FirebirdOOAPI, IB,  FBStatement, FB30ClientAPI, FB30Transaction,
  FB30Attachment{$ifdef WINDOWS}, Windows{$endif},IBExternals, FBSQLData, FBOutputBlock, FBActivityMonitor;

type
  TFB30Statement = class;
  TIBXSQLDA = class;

  { TIBXSQLVAR }

  TIBXSQLVAR = class(TSQLVarData)
  private
    FStatement: TFB30Statement;
    FFirebird30ClientAPI: TFB30ClientAPI;
    FBlob: IBlob;             {Cache references}
    FNullIndicator: short;
    FOwnsSQLData: boolean;
    FBlobMetaData: IBlobMetaData;
    FArrayMetaData: IArrayMetaData;

    {SQL Var Type Data}
    FSQLType: cardinal;
    FSQLSubType: integer;
    FSQLData: PByte; {Address of SQL Data in Message Buffer}
    FSQLNullIndicator: PShort; {Address of null indicator}
    FDataLength: cardinal;
    FMetadataSize: cardinal;
    FNullable: boolean;
    FScale: integer;
    FCharSetID: cardinal;
    FRelationName: AnsiString;
    FFieldName: AnsiString;
    function GetConnectionCodePage: TSystemCodePage;

    protected
     function CanChangeSQLType: boolean;
     function GetSQLType: cardinal; override;
     function GetSubtype: integer; override;
     function GetAliasName: AnsiString;  override;
     function GetFieldName: AnsiString; override;
     function GetOwnerName: AnsiString;  override;
     function GetRelationName: AnsiString;  override;
     function GetScale: integer; override;
     function GetCharSetID: cardinal; override;
     function GetIsNull: Boolean;   override;
     function GetIsNullable: boolean; override;
     function GetSQLData: PByte;  override;
     function GetDataLength: cardinal; override;
     function GetSize: cardinal; override;
     function GetDefaultTextSQLType: cardinal; override;
     procedure SetIsNull(Value: Boolean); override;
     procedure SetIsNullable(Value: Boolean);  override;
     procedure InternalSetScale(aValue: integer); override;
     procedure InternalSetDataLength(len: cardinal); override;
     procedure InternalSetSQLType(aValue: cardinal; aSubType: integer); override;
     procedure SetCharSetID(aValue: cardinal); override;
     procedure SetMetaSize(aValue: cardinal); override;
  public
    constructor Create(aParent: TIBXSQLDA; aIndex: integer);
    procedure Changed; override;
    procedure InitColumnMetaData(aMetaData: FirebirdOOAPI.IMessageMetadata);
    procedure ColumnSQLDataInit;
    procedure RowChange; override;
    procedure FreeSQLData;
    function GetAsArray: IArray; override;
    function GetAsBlob(Blob_ID: TISC_QUAD; BPB: IBPB): IBlob; override;
    function GetArrayMetaData: IArrayMetaData; override;
    function GetBlobMetaData: IBlobMetaData; override;
    function CreateBlob: IBlob; override;
    procedure SetSQLData(AValue: PByte; len: cardinal); override;
    property ConnectionCodePage: TSystemCodePage read GetConnectionCodePage;
  end;

  { TIBXSQLDA }

  TIBXSQLDA = class(TSQLDataArea)
  private
    FCount: Integer; {Columns in use - may be less than inherited columns}
    FSize: Integer;  {Number of TIBXSQLVARs in column list}
    FMetaData: FirebirdOOAPI.IMessageMetadata;
    FTransactionSeqNo: integer;
    function GetConnectionCodePage: TSystemCodePage;
 protected
    FStatement: TFB30Statement;
    FFirebird30ClientAPI: TFB30ClientAPI;
    FMessageBuffer: PByte; {Message Buffer}
    FMsgLength: integer; {Message Buffer length}
    function GetTransactionSeqNo: integer; override;
    procedure FreeXSQLDA; virtual;
    function GetStatement: IStatement; override;
    function GetPrepareSeqNo: integer; override;
    procedure SetCount(Value: Integer); override;
    procedure AllocMessageBuffer(len: integer); virtual;
    procedure FreeMessageBuffer; virtual;
  public
    constructor Create(aStatement: TFB30Statement); overload;
    constructor Create(api: IFirebirdAPI); overload;
    destructor Destroy; override;
    procedure Changed; virtual;
    function CheckStatementStatus(Request: TStatementStatus): boolean; override;
    function ColumnsInUseCount: integer; override;
    function GetMetaData: FirebirdOOAPI.IMessageMetadata; virtual;
    procedure Initialize; override;
    function StateChanged(var ChangeSeqNo: integer): boolean; override;
    function CanChangeMetaData: boolean; override;
    property Count: Integer read FCount write SetCount;
    property Statement: TFB30Statement read FStatement;
    property ConnectionCodePage: TSystemCodePage read GetConnectionCodePage;
  end;

  { TIBXINPUTSQLDA }

  TIBXINPUTSQLDA = class(TIBXSQLDA)
  private
    FCurMetaData: FirebirdOOAPI.IMessageMetadata;
    procedure FreeCurMetaData;
    function GetMessageBuffer: PByte;
    function GetModified: Boolean;
    function GetMsgLength: integer;
    procedure BuildMetadata;
  protected
    procedure PackBuffer;
    procedure FreeXSQLDA; override;
  public
    constructor Create(aStatement: TFB30Statement); overload;
    constructor Create(api: IFirebirdAPI); overload;
    destructor Destroy; override;
    procedure Bind(aMetaData: FirebirdOOAPI.IMessageMetadata);
    procedure Changed; override;
    function GetMetaData: FirebirdOOAPI.IMessageMetadata; override;
    procedure ReInitialise;
    function IsInputDataArea: boolean; override;
    property MessageBuffer: PByte read GetMessageBuffer;
    property MsgLength: integer read GetMsgLength;
  end;

  { TIBXOUTPUTSQLDA }

  TIBXOUTPUTSQLDA = class(TIBXSQLDA)
  private
    FTransaction: TFB30Transaction; {transaction used to execute the statement}
  protected
    function GetTransaction: ITransaction; override;
  public
    procedure Bind(aMetaData: FirebirdOOAPI.IMessageMetadata);
    procedure GetData(index: integer; var aIsNull: boolean; var len: short;
      var data: PByte); override;
    function IsInputDataArea: boolean; override;
    property MessageBuffer: PByte read FMessageBuffer;
    property MsgLength: integer read FMsgLength;
  end;

  { TResultSet }

  TResultSet = class(TResults,IResultSet)
  private
    FResults: TIBXOUTPUTSQLDA;
    FCursorSeqNo: integer;
    procedure RowChange;
  public
    constructor Create(aResults: TIBXOUTPUTSQLDA);
    destructor Destroy; override;
    {IResultSet}
    function FetchNext: boolean; {fetch next record}
    function FetchPrior: boolean; {fetch previous record}
    function FetchFirst:boolean; {fetch first record}
    function FetchLast: boolean; {fetch last record}
    function FetchAbsolute(position: Integer): boolean; {fetch record by its absolute position in result set}
    function FetchRelative(offset: Integer): boolean; {fetch record by position relative to current}
    function GetCursorName: AnsiString;
    function IsBof: boolean;
    function IsEof: boolean;
    procedure Close;
  end;

  { TBatchCompletion }

  TBatchCompletion = class(TInterfaceOwner,IBatchCompletion)
  private
    FCompletionState: FirebirdOOAPI.IBatchCompletionState;
    FConnectionCodePage: TSystemCodePage;
    FFirebird30ClientAPI: TFB30ClientAPI;
    FStatus: IStatus;
  public
    constructor Create(api: TFB30ClientAPI; cs: IBatchCompletionState;
                aConnectionCodePage: TSystemCodePage);
    destructor Destroy; override;
    property ConnectionCodePage: TSystemCodePage read FConnectionCodePage;
    {IBatchCompletion}
    function getErrorStatus(var RowNo: integer; var status: IStatus): boolean;
    function getTotalProcessed: cardinal;
    function getState(updateNo: cardinal): TBatchCompletionState;
    function getStatusMessage(updateNo: cardinal): AnsiString;
    function getUpdated: integer;
  end;

  TFetchType = (ftNext,ftPrior,ftFirst,ftLast,ftAbsolute,ftRelative);

  { TFB30Statement }

  TFB30Statement = class(TFBStatement,IStatement)
  private
    FStatementIntf: FirebirdOOAPI.IStatement;
    FFirebird30ClientAPI: TFB30ClientAPI;
    FSQLParams: TIBXINPUTSQLDA;
    FSQLRecord: TIBXOUTPUTSQLDA;
    FResultSet: FirebirdOOAPI.IResultSet;
    FCursorSeqNo: integer;
    FCursor: AnsiString;
    FBatch: FirebirdOOAPI.IBatch;
    FBatchCompletion: IBatchCompletion;
    FBatchRowCount: integer;
    FBatchBufferSize: integer;
    FBatchBufferUsed: integer;
  protected
    procedure CheckChangeBatchRowLimit; override;
    procedure CheckHandle; override;
    procedure CheckBatchModeAvailable;
    procedure GetDSQLInfo(info_request: byte; buffer: ISQLInfoResults); override;
    function GetStatementIntf: IStatement; override;
    procedure InternalPrepare(CursorName: AnsiString=''); override;
    function InternalExecute(aTransaction: ITransaction): IResults; override;
    function InternalOpenCursor(aTransaction: ITransaction; Scrollable: boolean
      ): IResultSet; override;
    procedure ProcessSQL(sql: AnsiString; GenerateParamNames: boolean; var processedSQL: AnsiString); override;
    procedure FreeHandle; override;
    procedure InternalClose(Force: boolean); override;
    function SavePerfStats(var Stats: TPerfStatistics): boolean;
  public
    constructor Create(Attachment: TFB30Attachment; Transaction: ITransaction;
      sql: AnsiString; aSQLDialect: integer; CursorName: AnsiString='');
    constructor CreateWithParameterNames(Attachment: TFB30Attachment; Transaction: ITransaction;
      sql: AnsiString;  aSQLDialect: integer; GenerateParamNames: boolean =false;
      CaseSensitiveParams: boolean=false; CursorName: AnsiString='');
    destructor Destroy; override;
    function Fetch(FetchType: TFetchType; PosOrOffset: integer=0): boolean;
    property StatementIntf: FirebirdOOAPI.IStatement read FStatementIntf;
    property SQLParams: TIBXINPUTSQLDA read FSQLParams;
    property SQLRecord: TIBXOUTPUTSQLDA read FSQLRecord;

  public
    {IStatement}
    function GetSQLParams: ISQLParams; override;
    function GetMetaData: IMetaData; override;
    function GetPlan: AnsiString;
    function IsPrepared: boolean;
    function GetFlags: TStatementFlags; override;
    function CreateBlob(column: TColumnMetaData): IBlob; override;
    function CreateArray(column: TColumnMetaData): IArray; override;
    procedure SetRetainInterfaces(aValue: boolean); override;
    function IsInBatchMode: boolean; override;
    function HasBatchMode: boolean; override;
    procedure AddToBatch; override;
    function ExecuteBatch(aTransaction: ITransaction
      ): IBatchCompletion; override;
    procedure CancelBatch; override;
    function GetBatchCompletion: IBatchCompletion; override;
end;

implementation

uses IBUtils, FBMessages, FBBlob, FB30Blob, variants,  FBArray, FB30Array;

const
  ISQL_COUNTERS = 'CurrentMemory, MaxMemory, RealTime, UserTime, Buffers, Reads, Writes, Fetches';

{ EIBBatchCompletionError }

{ TBatchCompletion }

constructor TBatchCompletion.Create(api: TFB30ClientAPI;
  cs: IBatchCompletionState; aConnectionCodePage: TSystemCodePage);
begin
  inherited Create;
  FFirebird30ClientAPI := api;
  FCompletionState := cs;
  FStatus := api.GetStatus.clone;
  FConnectionCodePage := aConnectionCodePage;
end;

destructor TBatchCompletion.Destroy;
begin
  if FCompletionState <> nil then
  begin
    FCompletionState.dispose;
    FCompletionState := nil;
  end;
  inherited Destroy;
end;

function TBatchCompletion.getErrorStatus(var RowNo: integer; var status: IStatus
  ): boolean;
var i: integer;
  upcount: cardinal;
  state: integer;
begin
  Result := false;
  RowNo := -1;
  with FFirebird30ClientAPI do
  begin
    upcount := FCompletionState.getSize(StatusIntf);
    Check4DataBaseError(ConnectionCodePage);
    for i := 0 to upcount - 1 do
    begin
      state := FCompletionState.getState(StatusIntf,i);
      if state = FirebirdOOAPI.IBatchCompletionStateImpl.EXECUTE_FAILED then
      begin
        RowNo := i+1;
        FCompletionState.getStatus(StatusIntf,(FStatus as TFB30Status).GetStatus,i);
        Check4DataBaseError(ConnectionCodePage);
        status := FStatus;
        Result := true;
        break;
      end;
    end;
  end;
end;

function TBatchCompletion.getTotalProcessed: cardinal;
begin
  with FFirebird30ClientAPI do
  begin
    Result := FCompletionState.getsize(StatusIntf);
    Check4DataBaseError(ConnectionCodePage);
  end;
end;

function TBatchCompletion.getState(updateNo: cardinal): TBatchCompletionState;
var state: integer;
begin
  with FFirebird30ClientAPI do
  begin
    state := FCompletionState.getState(StatusIntf,updateNo);
    Check4DataBaseError(ConnectionCodePage);
    case state of
      FirebirdOOAPI.IBatchCompletionStateImpl.EXECUTE_FAILED:
        Result := bcExecuteFailed;

      FirebirdOOAPI.IBatchCompletionStateImpl.SUCCESS_NO_INFO:
        Result := bcSuccessNoInfo;

     else
        Result := bcNoMoreErrors;
    end;
  end;
end;

function TBatchCompletion.getStatusMessage(updateNo: cardinal): AnsiString;
var status: FirebirdOOAPI.IStatus;
begin
  with FFirebird30ClientAPI do
  begin
    status := MasterIntf.getStatus;
    FCompletionState.getStatus(StatusIntf,status,updateNo);
    Check4DataBaseError(ConnectionCodePage);
    Result := FormatStatus(status,ConnectionCodePage);
  end;
end;

function TBatchCompletion.getUpdated: integer;
var i: integer;
    upcount: cardinal;
    state: integer;
begin
  Result := 0;
  with FFirebird30ClientAPI do
  begin
    upcount := FCompletionState.getSize(StatusIntf);
    Check4DataBaseError(ConnectionCodePage);
    for i := 0 to upcount -1  do
    begin
      state := FCompletionState.getState(StatusIntf,i);
      if state = FirebirdOOAPI.IBatchCompletionStateImpl.EXECUTE_FAILED then
          break;
      Inc(Result);
    end;
  end;
end;

{ TIBXSQLVAR }

procedure TIBXSQLVAR.Changed;
begin
  inherited Changed;
  TIBXSQLDA(Parent).Changed;
end;

procedure TIBXSQLVAR.InitColumnMetaData(aMetaData: FirebirdOOAPI.IMessageMetadata);
begin
  with FFirebird30ClientAPI do
  begin
    FSQLType := aMetaData.getType(StatusIntf,Index);
    Check4DataBaseError(ConnectionCodePage);
    if FSQLType = SQL_BLOB then
    begin
      FSQLSubType := aMetaData.getSubType(StatusIntf,Index);
      Check4DataBaseError(ConnectionCodePage);
    end
    else
      FSQLSubType := 0;
    FDataLength := aMetaData.getLength(StatusIntf,Index);
    Check4DataBaseError(ConnectionCodePage);
    FMetadataSize := FDataLength;
    FRelationName := PCharToAnsiString(aMetaData.getRelation(StatusIntf,Index),ConnectionCodePage);
    Check4DataBaseError(ConnectionCodePage);
    FFieldName := PCharToAnsiString(aMetaData.getField(StatusIntf,Index),ConnectionCodePage);
    Check4DataBaseError(ConnectionCodePage);
    FNullable := aMetaData.isNullable(StatusIntf,Index);
    Check4DataBaseError(ConnectionCodePage);
    FScale := aMetaData.getScale(StatusIntf,Index);
    Check4DataBaseError(ConnectionCodePage);
    FCharSetID :=  aMetaData.getCharSet(StatusIntf,Index) and $FF;
    Check4DataBaseError(ConnectionCodePage);
  end;
  if Name = '' then
    Name := FFieldName;
end;

procedure TIBXSQLVAR.ColumnSQLDataInit;
begin
  FreeSQLData;
  with FFirebird30ClientAPI do
  begin
    case SQLType of
      SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP,
      SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT, SQL_BOOLEAN,
      SQL_LONG, SQL_INT64, SQL_INT128, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT,
      SQL_TIMESTAMP_TZ, SQL_TIME_TZ, SQL_DEC_FIXED, SQL_DEC16, SQL_DEC34,
      SQL_TIMESTAMP_TZ_EX, SQL_TIME_TZ_EX:
      begin
        if (FDataLength = 0) then
          { Make sure you get a valid pointer anyway
           select '' from foo }
          IBAlloc(FSQLData, 0, 1)
        else
          IBAlloc(FSQLData, 0, FDataLength)
      end;
      SQL_VARYING:
        IBAlloc(FSQLData, 0, FDataLength + 2);
     else
        IBError(ibxeUnknownSQLDataType, [SQLType and (not 1)])
    end;
    FOwnsSQLData := true;
    FNullIndicator := -1;
  end;
end;

function TIBXSQLVAR.GetConnectionCodePage: TSystemCodePage;
begin
  if FStatement = nil then
    Result := CP_NONE
  else
    Result := FStatement.ConnectionCodePage;
end;

function TIBXSQLVAR.CanChangeSQLType: boolean;
begin
  Result := Parent.CanChangeMetaData;
end;

function TIBXSQLVAR.GetSQLType: cardinal;
begin
  Result := FSQLType;
end;

function TIBXSQLVAR.GetSubtype: integer;
begin
  Result := FSQLSubType;
end;

function TIBXSQLVAR.GetAliasName: AnsiString;
var metadata: FirebirdOOAPI.IMessageMetadata;
begin
  metadata := TIBXSQLDA(Parent).GetMetaData;
  try
    with FFirebird30ClientAPI do
    begin
      Result := PCharToAnsiString(metaData.getAlias(StatusIntf,Index),ConnectionCodePage);
      Check4DataBaseError(ConnectionCodePage);
    end;
  finally
    metadata.release;
  end;
end;

function TIBXSQLVAR.GetFieldName: AnsiString;
begin
  Result := FFieldName;
end;

function TIBXSQLVAR.GetOwnerName: AnsiString;
var metadata: FirebirdOOAPI.IMessageMetadata;
begin
  metadata := TIBXSQLDA(Parent).GetMetaData;
  try
    with FFirebird30ClientAPI do
    begin
      result := PCharToAnsiString(metaData.getOwner(StatusIntf,Index),ConnectionCodePage);
      Check4DataBaseError(ConnectionCodePage);
    end;
  finally
    metadata.release;
  end;
end;

function TIBXSQLVAR.GetRelationName: AnsiString;
begin
  Result := FRelationName;
end;

function TIBXSQLVAR.GetScale: integer;
begin
  Result := FScale;
end;

function TIBXSQLVAR.GetCharSetID: cardinal;
begin
  result := 0; {NONE}
  case SQLType of
  SQL_VARYING, SQL_TEXT:
      result := FCharSetID;

  SQL_BLOB:
    if (SQLSubType = 1) then
      result := FCharSetID
    else
      result := 1; {OCTETS}

  SQL_ARRAY:
    if (FRelationName <> '') and (FFieldName <> '') then
      result := GetArrayMetaData.GetCharSetID
    else
      result := FCharSetID;
  end;
end;

function TIBXSQLVAR.GetIsNull: Boolean;
begin
  Result := IsNullable and (FSQLNullIndicator^ = -1);
end;

function TIBXSQLVAR.GetIsNullable: boolean;
begin
  Result := FSQLNullIndicator <> nil;
end;

function TIBXSQLVAR.GetSQLData: PByte;
begin
  Result := FSQLData;
end;

function TIBXSQLVAR.GetDataLength: cardinal;
begin
  Result := FDataLength;
end;

function TIBXSQLVAR.GetSize: cardinal;
begin
  Result := FMetadataSize;
end;

function TIBXSQLVAR.GetArrayMetaData: IArrayMetaData;
begin
  if GetSQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if FArrayMetaData = nil then
    FArrayMetaData := TFB30ArrayMetaData.Create(GetAttachment as TFB30Attachment,
                GetTransaction as TFB30Transaction,
                GetRelationName,GetFieldName);
  Result := FArrayMetaData;
end;

function TIBXSQLVAR.GetBlobMetaData: IBlobMetaData;
begin
  if GetSQLType <> SQL_BLOB then
    IBError(ibxeInvalidDataConversion,[nil]);

  if FBlobMetaData = nil then
    FBlobMetaData := TFB30BlobMetaData.Create(GetAttachment as TFB30Attachment,
              GetTransaction as TFB30Transaction,
              GetRelationName,GetFieldName,
              GetSubType);
  (FBlobMetaData as TFBBlobMetaData).SetCharSetID(GetCharSetID);
  Result := FBlobMetaData;
end;

procedure TIBXSQLVAR.SetIsNull(Value: Boolean);
begin
  if Value then
  begin
    IsNullable := true;
    FNullIndicator := -1;
  end
  else
  if IsNullable then
    FNullIndicator := 0;
  Changed;
end;

procedure TIBXSQLVAR.SetIsNullable(Value: Boolean);
begin
  if Value = IsNullable then Exit;
  if Value then
  begin
    FSQLNullIndicator := @FNullIndicator;
    FNullIndicator := 0;
  end
  else
    FSQLNullIndicator := nil;
  Changed;
end;

procedure TIBXSQLVAR.SetSQLData(AValue: PByte; len: cardinal);
begin
  if FOwnsSQLData then
    FreeMem(FSQLData);
  FSQLData := AValue;
  FDataLength := len;
  FOwnsSQLData := false;
  Changed;
end;

procedure TIBXSQLVAR.InternalSetScale(aValue: integer);
begin
  FScale := aValue;
  Changed;
end;

procedure TIBXSQLVAR.InternalSetDataLength(len: cardinal);
begin
  if not FOwnsSQLData then
    FSQLData := nil;
  FDataLength := len;
  with FFirebird30ClientAPI do
    IBAlloc(FSQLData, 0, FDataLength);
  FOwnsSQLData := true;
  Changed;
end;

procedure TIBXSQLVAR.InternalSetSQLType(aValue: cardinal; aSubType: integer);
begin
  FSQLType := aValue;
  if aValue = SQL_BLOB then
    FSQLSubType := aSubType;
  Changed;
end;

procedure TIBXSQLVAR.SetCharSetID(aValue: cardinal);
begin
  FCharSetID := aValue;
  Changed;
end;

procedure TIBXSQLVAR.SetMetaSize(aValue: cardinal);
begin
  if (aValue > FMetaDataSize) and not CanChangeSQLType then
    IBError(ibxeCannotIncreaseMetadatasize,[FMetaDataSize,aValue]);
  FMetaDataSize := aValue;
end;

function TIBXSQLVAR.GetDefaultTextSQLType: cardinal;
begin
  Result := SQL_VARYING;
end;

constructor TIBXSQLVAR.Create(aParent: TIBXSQLDA; aIndex: integer);
begin
  inherited Create(aParent,aIndex);
  FStatement := aParent.Statement;
  FFirebird30ClientAPI := aParent.FFirebird30ClientAPI;
end;

procedure TIBXSQLVAR.RowChange;
begin
  inherited;
  FBlob := nil;
end;

procedure TIBXSQLVAR.FreeSQLData;
begin
  if FOwnsSQLData then
    FreeMem(FSQLData);
  FSQLData := nil;
  FOwnsSQLData := true;
end;

function TIBXSQLVAR.GetAsArray: IArray;
begin
  if SQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if IsNull then
    Result := nil
  else
  begin
    if FArrayIntf = nil then
      FArrayIntf := TFB30Array.Create(GetAttachment as TFB30Attachment,
                                GetTransaction as TFB30Transaction,
                                GetArrayMetaData,PISC_QUAD(SQLData)^);
    Result := FArrayIntf;
  end;
end;

function TIBXSQLVAR.GetAsBlob(Blob_ID: TISC_QUAD; BPB: IBPB): IBlob;
begin
  if FBlob <> nil then
    Result := FBlob
  else
  begin
    if SQLType <>  SQL_BLOB then
        IBError(ibxeInvalidDataConversion, [nil]);
    if IsNull then
      Result := nil
    else
      Result := TFB30Blob.Create(GetAttachment as TFB30Attachment,
                               GetTransaction as TFB30Transaction,
                               GetBlobMetaData,
                               Blob_ID,BPB);
    FBlob := Result;
  end;
end;

function TIBXSQLVAR.CreateBlob: IBlob;
begin
  Result := TFB30Blob.Create(GetAttachment as TFB30Attachment,
                             GetTransaction as TFB30Transaction,
                             GetSubType,GetCharSetID,nil);
end;

{ TResultSet }

procedure TResultSet.RowChange;
var i: integer;
begin
  for i := 0 to getCount - 1 do
    FResults.Column[i].RowChange;
end;

constructor TResultSet.Create(aResults: TIBXOUTPUTSQLDA);
begin
  inherited Create(aResults);
  FResults := aResults;
  FCursorSeqNo := aResults.FStatement.FCursorSeqNo;
end;

destructor TResultSet.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TResultSet.FetchNext: boolean;
begin
  CheckActive;
  Result := FResults.FStatement.Fetch(ftNext);
  if Result then
    RowChange;
end;

function TResultSet.FetchPrior: boolean;
begin
  CheckActive;
  Result := FResults.FStatement.Fetch(ftPrior);
  if Result then
    RowChange;
end;

function TResultSet.FetchFirst: boolean;
begin
  CheckActive;
  Result := FResults.FStatement.Fetch(ftFirst);
  if Result then
    RowChange;
end;

function TResultSet.FetchLast: boolean;
begin
  CheckActive;
  Result := FResults.FStatement.Fetch(ftLast);
  if Result then
    RowChange;
end;

function TResultSet.FetchAbsolute(position: Integer): boolean;
begin
  CheckActive;
  Result := FResults.FStatement.Fetch(ftAbsolute,position);
  if Result then
    RowChange;
end;

function TResultSet.FetchRelative(offset: Integer): boolean;
begin
  CheckActive;
  Result := FResults.FStatement.Fetch(ftRelative,offset);
  if Result then
    RowChange;
end;

function TResultSet.GetCursorName: AnsiString;
begin
  Result := FResults.FStatement.FCursor;
end;

function TResultSet.IsBof: boolean;
begin
  Result := FResults.FStatement.FBof;
end;

function TResultSet.IsEof: boolean;
begin
  Result := FResults.FStatement.FEof;
end;

procedure TResultSet.Close;
begin
  if FCursorSeqNo = FResults.FStatement.FCursorSeqNo then
    FResults.FStatement.Close;
end;

{ TIBXINPUTSQLDA }

function TIBXINPUTSQLDA.GetModified: Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to FCount - 1 do
    if Column[i].Modified then
    begin
      result := True;
      exit;
    end;
end;

procedure TIBXINPUTSQLDA.FreeCurMetaData;
begin
  if FCurMetaData <> nil then
  begin
    FCurMetaData.release;
    FCurMetaData := nil;
  end;
end;

function TIBXINPUTSQLDA.GetMessageBuffer: PByte;
begin
  PackBuffer;
  Result := FMessageBuffer;
end;

function TIBXINPUTSQLDA.GetMetaData: FirebirdOOAPI.IMessageMetadata;
begin
  BuildMetadata;
  Result := FCurMetaData;
  if Result <> nil then
    Result.addRef;
end;

function TIBXINPUTSQLDA.GetMsgLength: integer;
begin
  PackBuffer;
  Result := FMsgLength;
end;

procedure TIBXINPUTSQLDA.BuildMetadata;
var Builder: FirebirdOOAPI.IMetadataBuilder;
    i: integer;
    version: NativeInt;
begin
  if (FCurMetaData = nil) and (Count > 0) then
  with FFirebird30ClientAPI do
  begin
    Builder := FFirebird30ClientAPI.MasterIntf.getMetadataBuilder(StatusIntf,Count);
    Check4DataBaseError(ConnectionCodePage);
    try
      for i := 0 to Count - 1 do
      with TIBXSQLVar(Column[i]) do
      begin
        version := Builder.vTable.version;
        if version >= 4 then
        {Firebird 4 or later}
        begin
          Builder.setField(StatusIntf,i,PAnsiChar(Name));
          Check4DataBaseError(ConnectionCodePage);
          Builder.setAlias(StatusIntf,i,PAnsiChar(Name));
          Check4DataBaseError(ConnectionCodePage);
        end;
        Builder.setType(StatusIntf,i,FSQLType);
        Check4DataBaseError(ConnectionCodePage);
        Builder.setSubType(StatusIntf,i,FSQLSubType);
        Check4DataBaseError(ConnectionCodePage);
//        writeln('Column ',Name,' Type = ',TSQLDataItem.GetSQLTypeName(FSQLType),' Size = ',GetSize,' DataLength = ',GetDataLength);
        if FSQLType = SQL_VARYING then
        begin
          {The datalength can be greater than the metadata size when SQLType has been overridden to text}
          if (GetDataLength > GetSize) and CanChangeMetaData then
            Builder.setLength(StatusIntf,i,GetDataLength)
          else
            Builder.setLength(StatusIntf,i,GetSize)
        end
        else
          Builder.setLength(StatusIntf,i,GetDataLength);
        Check4DataBaseError(ConnectionCodePage);
        Builder.setCharSet(StatusIntf,i,GetCharSetID);
        Check4DataBaseError(ConnectionCodePage);
        Builder.setScale(StatusIntf,i,FScale);
        Check4DataBaseError(ConnectionCodePage);
      end;
      FCurMetaData := Builder.getMetadata(StatusIntf);
      Check4DataBaseError(ConnectionCodePage);
    finally
      Builder.release;
    end;
  end;
end;

procedure TIBXINPUTSQLDA.PackBuffer;
var i: integer;
    P: PByte;
    MsgLen: cardinal;
    aNullIndicator: short;
begin
  BuildMetadata;

  if (FMsgLength = 0) and (FCurMetaData <> nil) then
  with FFirebird30ClientAPI do
  begin
    MsgLen := FCurMetaData.getMessageLength(StatusIntf);
    Check4DataBaseError(ConnectionCodePage);

    AllocMessageBuffer(MsgLen);

    for i := 0 to Count - 1 do
    with TIBXSQLVar(Column[i]) do
    begin
      P := FMessageBuffer + FCurMetaData.getOffset(StatusIntf,i);
 //     writeln('Packbuffer: Column ',Name,' Type = ',TSQLDataItem.GetSQLTypeName(FSQLType),' Size = ',GetSize,' DataLength = ',GetDataLength);
      if not Modified then
        IBError(ibxeUninitializedInputParameter,[i,Name]);
      if IsNull then
        FillChar(P^,FDataLength,0)
      else
      if FSQLData <> nil then
      begin
        if SQLType = SQL_VARYING then
        begin
            EncodeInteger(FDataLength,2,P);
            Inc(P,2);
        end
        else
        if (SQLType = SQL_BLOB) and (FStatement.FBatch <> nil) then
        begin
          FStatement.FBatch.registerBlob(Statusintf,ISC_QUADPtr(FSQLData),ISC_QUADPtr(FSQLData));
          Check4DataBaseError(ConnectionCodePage);
        end;
        Move(FSQLData^,P^,FDataLength);
      end;
      if IsNullable then
      begin
        Move(FNullIndicator,(FMessageBuffer + FCurMetaData.getNullOffset(StatusIntf,i))^,sizeof(FNullIndicator));
        Check4DataBaseError(ConnectionCodePage);
      end
      else
      begin
        aNullIndicator := 0;
        Move(aNullIndicator,(FMessageBuffer + FCurMetaData.getNullOffset(StatusIntf,i))^,sizeof(aNullIndicator));
      end;
    end;
  end;
end;

procedure TIBXINPUTSQLDA.FreeXSQLDA;
begin
  inherited FreeXSQLDA;
  FreeCurMetaData;
end;

constructor TIBXINPUTSQLDA.Create(aStatement: TFB30Statement);
begin
  inherited Create(aStatement);
  FMessageBuffer := nil;
end;

constructor TIBXINPUTSQLDA.Create(api: IFirebirdAPI);
begin
  inherited Create(api);
  FMessageBuffer := nil;
end;

destructor TIBXINPUTSQLDA.Destroy;
begin
  FreeXSQLDA;
  inherited Destroy;
end;

procedure TIBXINPUTSQLDA.Bind(aMetaData: FirebirdOOAPI.IMessageMetadata);
var i: integer;
begin
  FMetaData := aMetaData;
  FMetaData.AddRef;
  with FFirebird30ClientAPI do
  begin
    Count := aMetadata.getCount(StatusIntf);
    Check4DataBaseError(ConnectionCodePage);
    Initialize;

    for i := 0 to Count - 1 do
    with TIBXSQLVar(Column[i]) do
    begin
      InitColumnMetaData(aMetaData);
      SaveMetaData;
      if FNullable then
        FSQLNullIndicator := @FNullIndicator
      else
        FSQLNullIndicator := nil;
      ColumnSQLDataInit;
    end;
  end;
end;

procedure TIBXINPUTSQLDA.Changed;
begin
  inherited Changed;
  FreeCurMetaData;
  FreeMessageBuffer;
end;

procedure TIBXINPUTSQLDA.ReInitialise;
var i: integer;
begin
  FreeMessageBuffer;
  for i := 0 to Count - 1 do
    TIBXSQLVar(Column[i]).ColumnSQLDataInit;
end;

function TIBXINPUTSQLDA.IsInputDataArea: boolean;
begin
  Result := true;
end;

{ TIBXOUTPUTSQLDA }

function TIBXOUTPUTSQLDA.GetTransaction: ITransaction;
begin
  if FTransaction <> nil then
    Result := FTransaction
  else
    Result := inherited GetTransaction;
end;

procedure TIBXOUTPUTSQLDA.Bind(aMetaData: FirebirdOOAPI.IMessageMetadata);
var i: integer;
    MsgLen: cardinal;
begin
  FMetaData := aMetaData;
  FMetaData.AddRef;
  with FFirebird30ClientAPI do
  begin
    Count := aMetaData.getCount(StatusIntf);
    Check4DataBaseError(ConnectionCodePage);
    Initialize;

    MsgLen := aMetaData.getMessageLength(StatusIntf);
    Check4DataBaseError(ConnectionCodePage);
    AllocMessageBuffer(MsgLen);

    for i := 0 to Count - 1 do
    with TIBXSQLVar(Column[i]) do
    begin
      InitColumnMetaData(aMetaData);
      FSQLData := FMessageBuffer + aMetaData.getOffset(StatusIntf,i);
      Check4DataBaseError(ConnectionCodePage);
      if FNullable then
      begin
        FSQLNullIndicator := PShort(FMessageBuffer + aMetaData.getNullOffset(StatusIntf,i));
        Check4DataBaseError(ConnectionCodePage);
      end
      else
        FSQLNullIndicator := nil;
      FBlob := nil;
      FArrayIntf := nil;
    end;
  end;
  SetUniqueRelationName;
end;

procedure TIBXOUTPUTSQLDA.GetData(index: integer; var aIsNull: boolean;
  var len: short; var data: PByte);
begin
  with TIBXSQLVAR(Column[index]) do
  begin
    aIsNull := FNullable and (FSQLNullIndicator^ = -1);
    data := FSQLData;
    len := FDataLength;
    if not IsNull and (FSQLType = SQL_VARYING) then
    begin
      with FFirebird30ClientAPI do
        len := DecodeInteger(data,2);
      Inc(Data,2);
    end;
  end;
end;

function TIBXOUTPUTSQLDA.IsInputDataArea: boolean;
begin
  Result := false;
end;

{ TIBXSQLDA }
constructor TIBXSQLDA.Create(aStatement: TFB30Statement);
begin
  inherited Create;
  FStatement := aStatement;
  FFirebird30ClientAPI := aStatement.FFirebird30ClientAPI;
  FSize := 0;
//  writeln('Creating ',ClassName);
end;

constructor TIBXSQLDA.Create(api: IFirebirdAPI);
begin
  inherited Create;
  FStatement := nil;
  FSize := 0;
  FFirebird30ClientAPI := api as TFB30ClientAPI;
end;

destructor TIBXSQLDA.Destroy;
begin
  FreeXSQLDA;
//  writeln('Destroying ',ClassName);
  inherited Destroy;
end;

procedure TIBXSQLDA.Changed;
begin

end;

function TIBXSQLDA.CheckStatementStatus(Request: TStatementStatus): boolean;
begin
  Result := false;
  if FStatement <> nil then
  case Request of
  ssPrepared:
    Result := FStatement.IsPrepared;

  ssExecuteResults:
    Result := FStatement.FSingleResults;

  ssCursorOpen:
    Result := FStatement.FOpen;

  ssBOF:
    Result := FStatement.FBOF;

  ssEOF:
    Result := FStatement.FEOF;
  end;
end;

function TIBXSQLDA.ColumnsInUseCount: integer;
begin
  Result := FCount;
end;

procedure TIBXSQLDA.Initialize;
begin
  if FMetaData <> nil then
    inherited Initialize;
end;

function TIBXSQLDA.StateChanged(var ChangeSeqNo: integer): boolean;
begin
  Result := (FStatement <> nil) and (FStatement.ChangeSeqNo <> ChangeSeqNo);
  if Result then
    ChangeSeqNo := FStatement.ChangeSeqNo;
end;

function TIBXSQLDA.CanChangeMetaData: boolean;
begin
  Result := FStatement.FBatch = nil;
end;

procedure TIBXSQLDA.SetCount(Value: Integer);
var
  i: Integer;
begin
  FCount := Value;
  if FCount = 0 then
    FUniqueRelationName := ''
  else
  begin
    SetLength(FColumnList, FCount);
    for i := FSize to FCount - 1 do
      FColumnList[i] := TIBXSQLVAR.Create(self,i);
    FSize := FCount;
  end;
end;

procedure TIBXSQLDA.AllocMessageBuffer(len: integer);
begin
  with FFirebird30ClientAPI do
    IBAlloc(FMessageBuffer,0,len);
  FMsgLength := len;
end;

procedure TIBXSQLDA.FreeMessageBuffer;
begin
  if FMessageBuffer <> nil then
  begin
    FreeMem(FMessageBuffer);
    FMessageBuffer := nil;
  end;
  FMsgLength := 0;
end;

function TIBXSQLDA.GetMetaData: FirebirdOOAPI.IMessageMetadata;
begin
  Result := FMetadata;
  if Result <> nil then
    Result.addRef;
end;

function TIBXSQLDA.GetConnectionCodePage: TSystemCodePage;
begin
  if Statement = nil then
    Result := CP_NONE
  else
    Result := Statement.ConnectionCodePage;
end;

function TIBXSQLDA.GetTransactionSeqNo: integer;
begin
  Result := FTransactionSeqNo;
end;

procedure TIBXSQLDA.FreeXSQLDA;
var i: integer;
begin
  if FMetaData <> nil then
    FMetaData.release;
  FMetaData := nil;
  for i := 0 to Count - 1 do
    TIBXSQLVAR(Column[i]).FreeSQLData;
  for i := 0 to FSize - 1  do
    TIBXSQLVAR(Column[i]).Free;
  FCount := 0;
  SetLength(FColumnList,0);
  FSize := 0;
  FreeMessageBuffer;
end;

function TIBXSQLDA.GetStatement: IStatement;
begin
  Result := FStatement;
end;

function TIBXSQLDA.GetPrepareSeqNo: integer;
begin
  if FStatement = nil then
    Result := 0
  else
    Result := FStatement.FPrepareSeqNo;
end;

{ TFB30Statement }

procedure TFB30Statement.CheckChangeBatchRowLimit;
begin
  if IsInBatchMode then
    IBError(ibxeInBatchMode,[nil]);
end;

procedure TFB30Statement.CheckHandle;
begin
  if FStatementIntf = nil then
    IBError(ibxeInvalidStatementHandle,[nil]);
end;

procedure TFB30Statement.CheckBatchModeAvailable;
begin
  if not HasBatchMode then
    IBError(ibxeBatchModeNotSupported,[nil]);
  case SQLStatementType of
  SQLInsert,
  SQLUpdate: {OK};
  else
     IBError(ibxeInvalidBatchQuery,[GetSQLStatementTypeName]);
  end;
end;

procedure TFB30Statement.GetDSQLInfo(info_request: byte; buffer: ISQLInfoResults
  );
begin
  with FFirebird30ClientAPI, buffer as TSQLInfoResultsBuffer do
  begin
    StatementIntf.getInfo(StatusIntf,1,BytePtr(@info_request),
                     GetBufSize, BytePtr(Buffer));
    Check4DataBaseError(ConnectionCodePage);
  end;
end;

function TFB30Statement.GetStatementIntf: IStatement;
begin
  Result := self;
end;

procedure TFB30Statement.InternalPrepare(CursorName: AnsiString);
var GUID : TGUID;
    metadata: FirebirdOOAPI.IMessageMetadata;
    sql: AnsiString;
begin
  if FPrepared then
    Exit;

  FCursor := CursorName;
  if (FSQL = '') then
    IBError(ibxeEmptyQuery, [nil]);
  try
    CheckTransaction(FTransactionIntf);
    with FFirebird30ClientAPI do
    begin
      if FCursor = '' then
      begin
        CreateGuid(GUID);
        FCursor := GUIDToString(GUID);
      end;

      if FHasParamNames then
      begin
        if FProcessedSQL = '' then
          ProcessSQL(FSQL,FGenerateParamNames,FProcessedSQL);
        sql := FProcessedSQL;
      end
      else
        sql := FSQL;

      if StringCodePage(sql) <> CP_NONE then
        sql := TransliterateToCodePage(sql,ConnectionCodePage);
      FStatementIntf := (GetAttachment as TFB30Attachment).AttachmentIntf.prepare(StatusIntf,
                          (FTransactionIntf as TFB30Transaction).TransactionIntf,
                          Length(sql),
                          PAnsiChar(sql),
                          FSQLDialect,
                          FirebirdOOAPI.IStatementImpl.PREPARE_PREFETCH_METADATA);
      Check4DataBaseError(ConnectionCodePage);
      FSQLStatementType := TIBSQLStatementTypes(FStatementIntf.getType(StatusIntf));
      Check4DataBaseError(ConnectionCodePage);

      if FSQLStatementType = SQLSelect then
      begin
        FStatementIntf.setCursorName(StatusIntf,PAnsiChar(FCursor));
        Check4DataBaseError(ConnectionCodePage);
      end;
      { Done getting the type }
      case FSQLStatementType of
        SQLGetSegment,
        SQLPutSegment,
        SQLStartTransaction:
        begin
          FreeHandle;
          IBError(ibxeNotPermitted, [nil]);
        end;
        SQLCommit,
        SQLRollback,
        SQLDDL, SQLSetGenerator,
        SQLInsert, SQLUpdate, SQLDelete, SQLSelect, SQLSelectForUpdate,
        SQLExecProcedure:
        begin
          {set up input sqlda}
          metadata := FStatementIntf.getInputMetadata(StatusIntf);
          Check4DataBaseError(ConnectionCodePage);
          try
            FSQLParams.Bind(metadata);
          finally
            metadata.release;
          end;

          {setup output sqlda}
          if FSQLStatementType in [SQLSelect, SQLSelectForUpdate,
                          SQLExecProcedure] then
          begin
            metadata := FStatementIntf.getOutputMetadata(StatusIntf);
            Check4DataBaseError(ConnectionCodePage);
            try
              FSQLRecord.Bind(metadata);
            finally
              metadata.release;
            end;
          end;
        end;
      end;
    end;
  except
    on E: Exception do begin
      if (FStatementIntf <> nil) then
        FreeHandle;
      if E is EIBInterBaseError then
        E.Message := E.Message + sSQLErrorSeparator + FSQL;
      raise;
    end;
  end;
  FPrepared := true;

  FSingleResults := false;
  if RetainInterfaces then
  begin
    SetRetainInterfaces(false);
    SetRetainInterfaces(true);
  end;
  Inc(FPrepareSeqNo);
  with GetTransaction as TFB30Transaction do
  begin
    FSQLParams.FTransactionSeqNo := TransactionSeqNo;
    FSQLRecord.FTransactionSeqNo := TransactionSeqNo;
  end;
  SignalActivity;
  Inc(FChangeSeqNo);
end;

function TFB30Statement.InternalExecute(aTransaction: ITransaction): IResults;

  procedure ExecuteQuery(outMetaData: FirebirdOOAPI.IMessageMetaData; outBuffer: pointer=nil);
  var inMetadata: FirebirdOOAPI.IMessageMetaData;
  begin
    with FFirebird30ClientAPI do
    begin
      SavePerfStats(FBeforeStats);
      inMetadata := FSQLParams.GetMetaData;
      try
        FStatementIntf.execute(StatusIntf,
                               (aTransaction as TFB30Transaction).TransactionIntf,
                               inMetaData,
                               FSQLParams.MessageBuffer,
                               outMetaData,
                               outBuffer);
        Check4DataBaseError(ConnectionCodePage);
      finally
        if inMetadata <> nil then
          inMetadata.release;
      end;
      FStatisticsAvailable := SavePerfStats(FAfterStats);
    end;
  end;

var Cursor: IResultSet;
    outMetadata: FirebirdOOAPI.IMessageMetaData;

begin
  Result := nil;
  FBatchCompletion := nil;
  FBOF := false;
  FEOF := false;
  FSingleResults := false;
  FStatisticsAvailable := false;
  outMetadata := nil;
  if IsInBatchMode then
    IBerror(ibxeInBatchMode,[]);
  CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  if aTransaction <> FTransactionIntf then
    AddMonitor(aTransaction as TFB30Transaction);
  if FStaleReferenceChecks and (FSQLParams.FTransactionSeqNo < (FTransactionIntf as TFB30transaction).TransactionSeqNo) then
    IBError(ibxeInterfaceOutofDate,[nil]);


  try
    with FFirebird30ClientAPI do
    begin
      case FSQLStatementType of
      SQLSelect:
       {e.g. Update...returning with a single row in Firebird 5 and later}
      begin
        Cursor := InternalOpenCursor(aTransaction,false);
        if not Cursor.IsEof then
          Cursor.FetchNext;
        Result := Cursor; {note only first row}
        FSingleResults := true;
      end;

      SQLExecProcedure:
      begin
        outMetadata := FSQLRecord.GetMetaData;
        try
          ExecuteQuery(outMetadata,FSQLRecord.MessageBuffer);
          Result := TResults.Create(FSQLRecord);
          FSingleResults := true;
        finally
          if outMetadata <> nil then
            outMetadata.release;
        end;
      end;

      else
        ExecuteQuery(outMetaData);
      end;
    end;
  finally
    if aTransaction <> FTransactionIntf then
       RemoveMonitor(aTransaction as TFB30Transaction);
  end;
  FExecTransactionIntf := aTransaction;
  FSQLRecord.FTransaction := (aTransaction as TFB30Transaction);
  FSQLRecord.FTransactionSeqNo := FSQLRecord.FTransaction.TransactionSeqNo;
  SignalActivity;
  Inc(FChangeSeqNo);
end;

function TFB30Statement.InternalOpenCursor(aTransaction: ITransaction;
  Scrollable: boolean): IResultSet;
var flags: cardinal;
    inMetadata,
    outMetadata: FirebirdOOAPI.IMessageMetadata;
begin
  flags := 0;
  if (FSQLStatementType <> SQLSelect) and not (stHasCursor in getFlags) then
   IBError(ibxeIsASelectStatement,[]);

  FBatchCompletion := nil;
  CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  if aTransaction <> FTransactionIntf then
    AddMonitor(aTransaction as TFB30Transaction);
  if FStaleReferenceChecks and (FSQLParams.FTransactionSeqNo < (FTransactionIntf as TFB30transaction).TransactionSeqNo) then
    IBError(ibxeInterfaceOutofDate,[nil]);

 if Scrollable then
   flags := FirebirdOOAPI.IStatementImpl.CURSOR_TYPE_SCROLLABLE;

 with FFirebird30ClientAPI do
 begin
   inMetadata := FSQLParams.GetMetaData;
   outMetadata := FSQLRecord.GetMetaData;
   try
     if FCollectStatistics then
       SavePerfStats(FBeforeStats);
     FResultSet := FStatementIntf.openCursor(StatusIntf,
                          (aTransaction as TFB30Transaction).TransactionIntf,
                          inMetaData,
                          FSQLParams.MessageBuffer,
                          outMetaData,
                          flags);
     Check4DataBaseError(ConnectionCodePage);
   finally
     if inMetadata <> nil then
       inMetadata.release;
     if outMetadata <> nil then
       outMetadata.release;
   end;

   if FCollectStatistics then
     FStatisticsAvailable := SavePerfStats(FAfterStats);
 end;
 Inc(FCursorSeqNo);
 FSingleResults := false;
 FOpen := True;
 FExecTransactionIntf := aTransaction;
 FBOF := true;
 FEOF := false;
 FSQLRecord.FTransaction := (aTransaction as TFB30Transaction);
 FSQLRecord.FTransactionSeqNo := FSQLRecord.FTransaction.TransactionSeqNo;
 Result := TResultSet.Create(FSQLRecord);
 SignalActivity;
 Inc(FChangeSeqNo);
end;

procedure TFB30Statement.ProcessSQL(sql: AnsiString; GenerateParamNames: boolean;
  var processedSQL: AnsiString);
begin
  FSQLParams.PreprocessSQL(sql,GenerateParamNames,processedSQL);
end;

procedure TFB30Statement.FreeHandle;
begin
  Close;
  ReleaseInterfaces;
  if FBatch <> nil then
  begin
    FBatch.release;
    FBatch := nil;
  end;
  if FStatementIntf <> nil then
  begin
    FStatementIntf.release;
    FStatementIntf := nil;
    FPrepared := false;
  end;
  FCursor := '';
  FProcessedSQL := '';
  if FSQLParams <> nil then
    FSQLParams.FreeXSQLDA;
  if FSQLRecord <> nil then
    FSQLRecord.FreeXSQLDA;
end;

procedure TFB30Statement.InternalClose(Force: boolean);
begin
  if (FStatementIntf <> nil) and (SQLStatementType = SQLSelect) and FOpen then
  try
    with FFirebird30ClientAPI do
    begin
      if FResultSet <> nil then
      begin
        if FSQLRecord.FTransaction.InTransaction and
          (FSQLRecord.FTransactionSeqNo = FSQLRecord.FTransaction.TransactionSeqNo) then
          FResultSet.close(StatusIntf)
        else
          FResultSet.release;
      end;
      FResultSet := nil;
      if not Force then Check4DataBaseError(ConnectionCodePage);
    end;
  finally
    if (FSQLRecord.FTransaction <> nil) and (FSQLRecord.FTransaction <> (FTransactionIntf as TFB30Transaction)) then
      RemoveMonitor(FSQLRecord.FTransaction);
    FOpen := False;
    FExecTransactionIntf := nil;
    FSQLRecord.FTransaction := nil;
  end;
  SignalActivity;
  Inc(FChangeSeqNo);
end;

function TFB30Statement.SavePerfStats(var Stats: TPerfStatistics): boolean;
begin
  Result := false;
  if FCollectStatistics then
  with FFirebird30ClientAPI do
  begin
    UtilIntf.getPerfCounters(StatusIntf,
              (GetAttachment as TFB30Attachment).AttachmentIntf,
              ISQL_COUNTERS, @Stats);
    Check4DataBaseError(ConnectionCodePage);
    Result := true;
  end;
end;

constructor TFB30Statement.Create(Attachment: TFB30Attachment;
  Transaction: ITransaction; sql: AnsiString; aSQLDialect: integer;
  CursorName: AnsiString);
begin
  inherited Create(Attachment,Transaction,sql,aSQLDialect);
  FFirebird30ClientAPI := Attachment.Firebird30ClientAPI;
  FSQLParams := TIBXINPUTSQLDA.Create(self);
  FSQLRecord := TIBXOUTPUTSQLDA.Create(self);
  InternalPrepare(CursorName);
end;

constructor TFB30Statement.CreateWithParameterNames(
  Attachment: TFB30Attachment; Transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer; GenerateParamNames: boolean;
  CaseSensitiveParams: boolean; CursorName: AnsiString);
begin
  inherited CreateWithParameterNames(Attachment,Transaction,sql,aSQLDialect,GenerateParamNames);
  FFirebird30ClientAPI := Attachment.Firebird30ClientAPI;
  FSQLParams := TIBXINPUTSQLDA.Create(self);
  FSQLParams.CaseSensitiveParams := CaseSensitiveParams;
  FSQLRecord := TIBXOUTPUTSQLDA.Create(self);
  InternalPrepare(CursorName);
end;

destructor TFB30Statement.Destroy;
begin
  inherited Destroy;
  if assigned(FSQLParams) then FSQLParams.Free;
  if assigned(FSQLRecord) then FSQLRecord.Free;
end;

function TFB30Statement.Fetch(FetchType: TFetchType; PosOrOffset: integer
  ): boolean;
var fetchResult: integer;
begin
    result := false;
  if not FOpen then
    IBError(ibxeSQLClosed, [nil]);

  with FFirebird30ClientAPI, FirebirdOOAPI.IStatusImpl do
  begin
    case FetchType of
    ftNext:
      begin
        if FEOF then
          IBError(ibxeEOF,[nil]);
        { Go to the next record... }
        fetchResult := FResultSet.fetchNext(StatusIntf,FSQLRecord.MessageBuffer);
        Check4DataBaseError(ConnectionCodePage);
        if fetchResult = RESULT_NO_DATA then
        begin
          FBOF := false;
          FEOF := true;
          Exit; {End of File}
        end
      end;

    ftPrior:
      begin
        if FBOF then
          IBError(ibxeBOF,[nil]);
        { Go to the next record... }
        fetchResult := FResultSet.fetchPrior(StatusIntf,FSQLRecord.MessageBuffer);
        Check4DataBaseError(ConnectionCodePage);
        if fetchResult = RESULT_NO_DATA then
        begin
          FBOF := true;
          FEOF := false;
          Exit; {Top of File}
        end
      end;

    ftFirst:
      begin
        fetchResult := FResultSet.fetchFirst(StatusIntf,FSQLRecord.MessageBuffer);
        Check4DataBaseError(ConnectionCodePage);
      end;

    ftLast:
      begin
        fetchResult := FResultSet.fetchLast(StatusIntf,FSQLRecord.MessageBuffer);
        Check4DataBaseError(ConnectionCodePage);
      end;

    ftAbsolute:
      begin
        fetchResult := FResultSet.fetchAbsolute(StatusIntf,PosOrOffset,FSQLRecord.MessageBuffer);
        Check4DataBaseError(ConnectionCodePage);
      end;

    ftRelative:
      begin
        fetchResult := FResultSet.fetchRelative(StatusIntf,PosOrOffset,FSQLRecord.MessageBuffer);
        Check4DataBaseError(ConnectionCodePage);
      end;
    end;

    if fetchResult <> RESULT_OK then
      exit; {result = false}

    {Result OK}
    FBOF := false;
    FEOF := false;
    result := true;

    if FCollectStatistics then
    begin
      UtilIntf.getPerfCounters(StatusIntf,
                              (GetAttachment as TFB30Attachment).AttachmentIntf,
                              ISQL_COUNTERS,@FAfterStats);
      Check4DataBaseError(ConnectionCodePage);
      FStatisticsAvailable := true;
    end;
  end;
  FSQLRecord.RowChange;
  SignalActivity;
  if FEOF then
    Inc(FChangeSeqNo);
end;

function TFB30Statement.GetSQLParams: ISQLParams;
begin
  CheckHandle;
  if not HasInterface(0) then
    AddInterface(0,TSQLParams.Create(FSQLParams));
  Result := TSQLParams(GetInterface(0));
end;

function TFB30Statement.GetMetaData: IMetaData;
begin
  CheckHandle;
  if not HasInterface(1) then
    AddInterface(1, TMetaData.Create(FSQLRecord));
  Result := TMetaData(GetInterface(1));
end;

function TFB30Statement.GetPlan: AnsiString;
begin
  CheckHandle;
  if (not (FSQLStatementType in [SQLSelect, SQLSelectForUpdate,
       {TODO: SQLExecProcedure, }
       SQLUpdate, SQLDelete])) then
    result := ''
  else
  with FFirebird30ClientAPI do
  begin
    Result := FStatementIntf.getPlan(StatusIntf,true);
    Check4DataBaseError(ConnectionCodePage);
  end;
end;

function TFB30Statement.CreateBlob(column: TColumnMetaData): IBlob;
begin
  if assigned(column) and (column.SQLType <> SQL_Blob) then
    IBError(ibxeNotABlob,[nil]);
  Result := TFB30Blob.Create(GetAttachment as TFB30Attachment,
                             GetTransaction as TFB30Transaction,
                             column.GetBlobMetaData,nil);
end;

function TFB30Statement.CreateArray(column: TColumnMetaData): IArray;
begin
  if assigned(column) and (column.SQLType <> SQL_ARRAY) then
    IBError(ibxeNotAnArray,[nil]);
  Result := TFB30Array.Create(GetAttachment as TFB30Attachment,
                             GetTransaction as TFB30Transaction,
                             column.GetArrayMetaData);
end;

procedure TFB30Statement.SetRetainInterfaces(aValue: boolean);
begin
  inherited SetRetainInterfaces(aValue);
  if HasInterface(1) then
    TMetaData(GetInterface(1)).RetainInterfaces := aValue;
  if HasInterface(0) then
    TSQLParams(GetInterface(0)).RetainInterfaces := aValue;
end;

function TFB30Statement.IsInBatchMode: boolean;
begin
  Result := FBatch <> nil;
end;

function TFB30Statement.HasBatchMode: boolean;
begin
  Result := GetAttachment.HasBatchMode;
end;

procedure TFB30Statement.AddToBatch;
var BatchPB: TXPBParameterBlock;
    inMetadata: FirebirdOOAPI.IMessageMetadata;

const SixteenMB = 16 * 1024 * 1024;
      MB256 = 256* 1024 *1024;
begin
  FBatchCompletion := nil;
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  CheckBatchModeAvailable;
  inMetadata := FSQLParams.GetMetaData;
  try
    with FFirebird30ClientAPI do
    begin
      if FBatch = nil then
      begin
        {Start Batch}
        BatchPB := TXPBParameterBlock.Create(FFirebird30ClientAPI,FirebirdOOAPI.IXpbBuilderImpl.BATCH);
        with FFirebird30ClientAPI do
        try
          if FBatchRowLimit = maxint then
            FBatchBufferSize := MB256
          else
          begin
            FBatchBufferSize := FBatchRowLimit * inMetadata.getAlignedLength(StatusIntf);
            Check4DataBaseError(ConnectionCodePage);
            if FBatchBufferSize < SixteenMB then
              FBatchBufferSize := SixteenMB;
            if FBatchBufferSize > MB256 {assumed limit} then
              IBError(ibxeBatchBufferSizeTooBig,[FBatchBufferSize]);
          end;
          BatchPB.insertInt(FirebirdOOAPI.IBatchImpl.TAG_RECORD_COUNTS,1);
          BatchPB.insertInt(FirebirdOOAPI.IBatchImpl.TAG_BUFFER_BYTES_SIZE,FBatchBufferSize);
          FBatch := FStatementIntf.createBatch(StatusIntf,
                                               inMetadata,
                                               BatchPB.getDataLength,
                                               BatchPB.getBuffer);
          Check4DataBaseError(ConnectionCodePage);

        finally
          BatchPB.Free;
        end;
        FBatchRowCount := 0;
        FBatchBufferUsed := 0;
      end;

      Inc(FBatchRowCount);
      Inc(FBatchBufferUsed,inMetadata.getAlignedLength(StatusIntf));
      Check4DataBaseError(ConnectionCodePage);
      if FBatchBufferUsed > FBatchBufferSize then
        raise EIBBatchBufferOverflow.Create(Ord(ibxeBatchRowBufferOverflow),
                                Format(GetErrorMessage(ibxeBatchRowBufferOverflow),
                                [FBatchRowCount,FBatchBufferSize]));

      FBatch.Add(StatusIntf,1,FSQLParams.GetMessageBuffer);
        Check4DataBaseError(ConnectionCodePage)
    end;
  finally
    if inMetadata <> nil then
      inMetadata.release;
  end;
end;

function TFB30Statement.ExecuteBatch(aTransaction: ITransaction
  ): IBatchCompletion;

procedure Check4BatchCompletionError(bc: IBatchCompletion);
var status: IStatus;
    RowNo: integer;
begin
  status := nil;
  {Raise an exception if there was an error reported in the BatchCompletion}
  if (bc <> nil) and bc.getErrorStatus(RowNo,status) then
    raise EIBInterbaseError.Create(status,ConnectionCodePage);
end;

var cs: FirebirdOOAPI.IBatchCompletionState;

begin
  Result := nil;
  if FBatch = nil then
    IBError(ibxeNotInBatchMode,[]);

  with FFirebird30ClientAPI do
  begin
    SavePerfStats(FBeforeStats);
    if aTransaction = nil then
      cs := FBatch.execute(StatusIntf,(FTransactionIntf as TFB30Transaction).TransactionIntf)
    else
      cs := FBatch.execute(StatusIntf,(aTransaction as TFB30Transaction).TransactionIntf);
    Check4DataBaseError(ConnectionCodePage);
    FBatchCompletion := TBatchCompletion.Create(FFirebird30ClientAPI,cs,ConnectionCodePage);
    FStatisticsAvailable := SavePerfStats(FAfterStats);
    FBatch.release;
    FBatch := nil;
    Check4BatchCompletionError(FBatchCompletion);
    Result := FBatchCompletion;
  end;
end;

procedure TFB30Statement.CancelBatch;
begin
  if FBatch = nil then
    IBError(ibxeNotInBatchMode,[]);
  FBatch.release;
  FBatch := nil;
end;

function TFB30Statement.GetBatchCompletion: IBatchCompletion;
begin
  Result := FBatchCompletion;
end;

function TFB30Statement.IsPrepared: boolean;
begin
  Result := FStatementIntf <> nil;
end;

function TFB30Statement.GetFlags: TStatementFlags;
var flags: cardinal;
begin
  CheckHandle;
  Result := [];
  with FFirebird30ClientAPI do
  begin
    flags := FStatementIntf.getFlags(StatusIntf);
    Check4DataBaseError(ConnectionCodePage);
  end;
  if flags and FirebirdOOAPI.IStatementImpl.FLAG_HAS_CURSOR <> 0 then
    Result := Result + [stHasCursor];
  if flags and FirebirdOOAPI.IStatementImpl.FLAG_REPEAT_EXECUTE <> 0 then
    Result := Result + [stRepeatExecute];
  if flags and FirebirdOOAPI.IStatementImpl.CURSOR_TYPE_SCROLLABLE <> 0 then
    Result := Result + [stScrollable];
end;

end.

