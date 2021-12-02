(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API. Although predominantly
 *  a new development they include source code taken from IBX and may be
 *  considered a derived product. This software thus also includes the copyright
 *  notice and license conditions from IBX.
 *
 *  Except for those parts dervied from IBX, contents of this file are subject
 *  to the Initial Developer's Public License Version 1.0 (the "License"); you
 *  may not use this file except in compliance with the License. You may obtain a
 *  copy of the License here:
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
unit FBStatement;
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
  Classes, SysUtils,  IB,  FBClientAPI, FBSQLData, FBOutputBlock, FBActivityMonitor,
  FBTransaction;

const
  DefaultBatchRowLimit = 1000;

type
  TPerfStatistics = array[psCurrentMemory..psFetches] of Int64;

  { TFBStatement }

  TFBStatement = class(TActivityReporter,ITransactionUser)
  private
    FAttachmentIntf: IAttachment;
    FFirebirdClientAPI: TFBClientAPI;
  protected
    FTransactionIntf: ITransaction;
    FExecTransactionIntf: ITransaction;
    FStaleReferenceChecks: boolean;
    FSQLStatementType: TIBSQLStatementTypes;         { Select, update, delete, insert, create, alter, etc...}
    FSQLDialect: integer;
    FOpen: boolean;
    FPrepared: boolean;
    FPrepareSeqNo: integer; {used to check for out of date references from interfaces}
    FSQL: AnsiString;
    FProcessedSQL: AnsiString;
    FHasParamNames: boolean;
    FBOF: boolean;
    FEOF: boolean;
    FSingleResults: boolean;
    FGenerateParamNames: boolean;
    FChangeSeqNo: integer;
    FCollectStatistics: boolean;
    FStatisticsAvailable: boolean;
    FBeforeStats: TPerfStatistics;
    FAfterStats: TPerfStatistics;
    FCaseSensitiveParams: boolean;
    FBatchRowLimit: integer;
    procedure CheckChangeBatchRowLimit; virtual;
    procedure CheckHandle; virtual; abstract;
    procedure CheckTransaction(aTransaction: ITransaction);
    function GetJournalIntf: IJournallingHook;
    function GetStatementIntf: IStatement; virtual; abstract;
    procedure GetDsqlInfo(info_request: byte; buffer: ISQLInfoResults); overload; virtual; abstract;
    procedure InternalPrepare(CursorName: AnsiString='');  virtual; abstract;
    function InternalExecute(Transaction: ITransaction): IResults;  virtual; abstract;
    function InternalOpenCursor(aTransaction: ITransaction; Scrollable: boolean): IResultSet;   virtual; abstract;
    procedure ProcessSQL(sql: AnsiString; GenerateParamNames: boolean; var processedSQL: AnsiString); virtual; abstract;
    procedure FreeHandle;  virtual; abstract;
    procedure InternalClose(Force: boolean); virtual; abstract;
    function TimeStampToMSecs(const TimeStamp: TTimeStamp): Int64;
  public
    constructor Create(Attachment: IAttachment; Transaction: ITransaction;
      sql: AnsiString; SQLDialect: integer);
    constructor CreateWithParameterNames(Attachment: IAttachment; Transaction: ITransaction;
      sql: AnsiString;  SQLDialect: integer; GenerateParamNames: boolean =false;
      CaseSensitiveParams: boolean = false);
    destructor Destroy; override;
    procedure Close;
    procedure TransactionEnding(aTransaction: ITransaction; Force: boolean);
    property SQLDialect: integer read FSQLDialect;
    property FirebirdClientAPI: TFBClientAPI read FFirebirdClientAPI;

  public
    function GetSQLParams: ISQLParams; virtual; abstract;
    function GetMetaData: IMetaData;  virtual; abstract;
    function GetRowsAffected(var SelectCount, InsertCount, UpdateCount,
      DeleteCount: integer): boolean;
    function GetSQLStatementType: TIBSQLStatementTypes;
    function GetSQLStatementTypeName: AnsiString;
    function GetSQLText: AnsiString;
    function GetProcessedSQLText: AnsiString;
    function GetSQLDialect: integer;
    function GetFlags: TStatementFlags; virtual;

    {GetDSQLInfo only supports isc_info_sql_stmt_type, isc_info_sql_get_plan, isc_info_sql_records}
    procedure Prepare(aTransaction: ITransaction=nil);  overload;
    procedure Prepare(CursorName: AnsiString; aTransaction: ITransaction=nil); overload; virtual;
    function Execute(aTransaction: ITransaction=nil): IResults;
    function OpenCursor(aTransaction: ITransaction=nil): IResultSet; overload;
    function OpenCursor(Scrollable: boolean; aTransaction: ITransaction=nil): IResultSet; overload;
    function CreateBlob(paramName: AnsiString): IBlob; overload;
    function CreateBlob(index: integer): IBlob; overload;
    function CreateBlob(column: TColumnMetaData): IBlob; overload; virtual; abstract;
    function CreateArray(paramName: AnsiString): IArray; overload;
    function CreateArray(index: integer): IArray;  overload;
    function CreateArray(column: TColumnMetaData): IArray; overload; virtual; abstract;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    function GetDSQLInfo(Request: byte): ISQLInfoResults; overload;
    procedure SetRetainInterfaces(aValue: boolean); virtual;
    procedure EnableStatistics(aValue: boolean);
    function GetPerfStatistics(var stats: TPerfCounters): boolean;
    function IsInBatchMode: boolean; virtual;
    function HasBatchMode: boolean; virtual;
  public
    {IBatch support}
    procedure AddToBatch; virtual;
    function ExecuteBatch(aTransaction: ITransaction): IBatchCompletion; virtual;
    procedure CancelBatch; virtual;
    function GetBatchCompletion: IBatchCompletion; virtual;
    function GetBatchRowLimit: integer;
    procedure SetBatchRowLimit(aLimit: integer);
    {Stale Reference Check}
    procedure SetStaleReferenceChecks(Enable:boolean); {default true}
    function GetStaleReferenceChecks: boolean;
  public
    property ChangeSeqNo: integer read FChangeSeqNo;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLStatementType: TIBSQLStatementTypes read GetSQLStatementType;
end;

implementation

uses FBMessages;

{ TFBStatement }

procedure TFBStatement.CheckChangeBatchRowLimit;
begin
  //Do Nothing
end;

procedure TFBStatement.CheckTransaction(aTransaction: ITransaction);
begin
  if (aTransaction = nil) then
    IBError(ibxeTransactionNotAssigned,[]);

  if not aTransaction.InTransaction then
    IBError(ibxeNotInTransaction,[]);
end;

function TFBStatement.GetJournalIntf: IJournallingHook;
begin
  GetAttachment.QueryInterface(IJournallingHook,Result)
end;

function TFBStatement.TimeStampToMSecs(const TimeStamp: TTimeStamp): Int64;
begin
  Result := TimeStamp.Time + Int64(timestamp.date)*msecsperday;
end;

constructor TFBStatement.Create(Attachment: IAttachment;
  Transaction: ITransaction; sql: AnsiString; SQLDialect: integer);
begin
  inherited Create(Transaction as TFBTransaction,2);
  FAttachmentIntf := Attachment;
  FTransactionIntf := Transaction;
  FFirebirdClientAPI := Attachment.getFirebirdAPI as TFBClientAPI;
  FSQLDialect := SQLDialect;
  FSQL := sql;
  FBatchRowLimit := DefaultBatchRowLimit;
  FStaleReferenceChecks := true;
end;

constructor TFBStatement.CreateWithParameterNames(Attachment: IAttachment;
  Transaction: ITransaction; sql: AnsiString; SQLDialect: integer;
  GenerateParamNames: boolean; CaseSensitiveParams: boolean);
begin
  FHasParamNames := true;
  FGenerateParamNames := GenerateParamNames;
  FCaseSensitiveParams := CaseSensitiveParams;
  Create(Attachment,Transaction,sql,SQLDialect);
end;

destructor TFBStatement.Destroy;
begin
  Close;
  FreeHandle;
  inherited Destroy;
end;

procedure TFBStatement.Close;
begin
  InternalClose(false);
end;

procedure TFBStatement.TransactionEnding(aTransaction: ITransaction;
  Force: boolean);
begin
  if FOpen and ((FExecTransactionIntf as TObject) = (aTransaction as TObject)) then
    InternalClose(Force);

  if FTransactionIntf = aTransaction then
  begin
    FreeHandle;
    FPrepared := false;
  end;
end;

function TFBStatement.GetRowsAffected(var SelectCount, InsertCount,
  UpdateCount, DeleteCount: integer): boolean;
var
  RB: ISQLInfoResults;
  i, j: integer;
begin
  InsertCount := 0;
  UpdateCount := 0;
  DeleteCount := 0;
  Result := FPrepared;
  if not Result then Exit;

  RB := GetDsqlInfo(isc_info_sql_records);

  for i := 0 to RB.Count - 1 do
  with RB[i] do
  case getItemType of
  isc_info_sql_records:
    for j := 0 to Count -1 do
    with Items[j] do
    case getItemType of
    isc_info_req_select_count:
      SelectCount := GetAsInteger;
    isc_info_req_insert_count:
      InsertCount := GetAsInteger;
    isc_info_req_update_count:
      UpdateCount := GetAsInteger;
    isc_info_req_delete_count:
      DeleteCount := GetAsInteger;
    end;
  end;
end;

function TFBStatement.GetSQLStatementType: TIBSQLStatementTypes;
begin
  Result := FSQLStatementType;
end;

function TFBStatement.GetSQLStatementTypeName: AnsiString;
begin
  case FSQLStatementType of
  SQLUnknown: Result := 'SQL_Unknown';
  SQLSelect: Result := 'SQL_Select';
  SQLInsert: Result := 'SQL_Insert';
  SQLUpdate: Result := 'SQL_Update';
  SQLDelete: Result := 'SQL_Delete';
  SQLDDL: Result := 'SQL_DDL';
  SQLGetSegment: Result := 'SQL_GetSegment';
  SQLPutSegment: Result := 'SQL_PutSegment';
  SQLExecProcedure: Result := 'SQL_ExecProcedure';
  SQLStartTransaction: Result := 'SQL_StartTransaction';
  SQLCommit: Result := 'SQL_Commit';
  SQLRollback: Result := 'SQL_Rollback';
  SQLSelectForUpdate: Result := 'SQL_SelectForUpdate';
  SQLSetGenerator: Result := 'SQL_SetGenerator';
  SQLSavePoint: Result := 'SQL_SavePoint';
  end;
end;

function TFBStatement.GetSQLText: AnsiString;
begin
  Result := FSQL;
end;

function TFBStatement.GetProcessedSQLText: AnsiString;
begin
  if FProcessedSQL = '' then
    ProcessSQL(FSQL,FGenerateParamNames,FProcessedSQL);
  Result := FProcessedSQL
end;

function TFBStatement.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

function TFBStatement.GetFlags: TStatementFlags;
begin
  Result := [];
end;

procedure TFBStatement.Prepare(aTransaction: ITransaction);
begin
  Prepare('',aTransaction);
end;

procedure TFBStatement.Prepare(CursorName: AnsiString;
  aTransaction: ITransaction);
begin
  if FPrepared then FreeHandle;
  if aTransaction <> nil then
  begin
    RemoveMonitor(FTransactionIntf as TFBTransaction);
    FTransactionIntf := aTransaction;
    AddMonitor(FTransactionIntf as TFBTransaction);
  end;
  InternalPrepare(CursorName);
end;

function TFBStatement.Execute(aTransaction: ITransaction): IResults;
begin
  if aTransaction = nil then
    Result :=  InternalExecute(FTransactionIntf)
  else
    Result := InternalExecute(aTransaction);
  GetJournalIntf.ExecQuery(GetStatementIntf);
end;

procedure TFBStatement.AddToBatch;
begin
  IBError(ibxeBatchModeNotSupported,[]);
end;

function TFBStatement.ExecuteBatch(aTransaction: ITransaction
  ): IBatchCompletion;
begin
  IBError(ibxeBatchModeNotSupported,[]);
end;

procedure TFBStatement.CancelBatch;
begin
  IBError(ibxeBatchModeNotSupported,[]);
end;

function TFBStatement.GetBatchCompletion: IBatchCompletion;
begin
  IBError(ibxeBatchModeNotSupported,[]);
end;

function TFBStatement.GetBatchRowLimit: integer;
begin
  Result := FBatchRowLimit;
end;

procedure TFBStatement.SetBatchRowLimit(aLimit: integer);
begin
  CheckChangeBatchRowLimit;
  FBatchRowLimit := aLimit;
end;

procedure TFBStatement.SetStaleReferenceChecks(Enable: boolean);
begin
  FStaleReferenceChecks := Enable;
end;

function TFBStatement.GetStaleReferenceChecks: boolean;
begin
  Result := FStaleReferenceChecks;
end;

function TFBStatement.OpenCursor(aTransaction: ITransaction): IResultSet;
begin
  Result := OpenCursor(false,aTransaction);
end;

function TFBStatement.OpenCursor(Scrollable: boolean; aTransaction: ITransaction
  ): IResultSet;
begin
  Close;
  if aTransaction = nil then
    Result := InternalOpenCursor(FTransactionIntf,Scrollable)
  else
    Result := InternalOpenCursor(aTransaction,Scrollable);
  GetJournalIntf.ExecQuery(GetStatementIntf);
end;

function TFBStatement.CreateBlob(paramName: AnsiString): IBlob;
var column: TColumnMetaData;
begin
  InternalPrepare;
  column := SQLParams.ByName(paramName) as TSQLParam;
  if column = nil then
    IBError(ibxeFieldNotFound,[paramName]);
  Result := CreateBlob(column);
end;

function TFBStatement.CreateBlob(index: integer): IBlob;
begin
  InternalPrepare;
  Result := CreateBlob(SQLParams[index] as TSQLParam);
end;

function TFBStatement.CreateArray(paramName: AnsiString): IArray;
var column: TColumnMetaData;
begin
  InternalPrepare;
  column := SQLParams.ByName(paramName) as TSQLParam;
  if column = nil then
    IBError(ibxeFieldNotFound,[paramName]);
  Result := CreateArray(column);
end;

function TFBStatement.CreateArray(index: integer): IArray;
begin
  InternalPrepare;
  Result := CreateArray(SQLParams[index] as TSQLParam);
end;

function TFBStatement.GetAttachment: IAttachment;
begin
  Result := FAttachmentIntf;
end;

function TFBStatement.GetTransaction: ITransaction;
begin
  Result := FTransactionIntf
end;

function TFBStatement.GetDSQLInfo(Request: byte): ISQLInfoResults;
begin
  Result := TSQLInfoResultsBuffer.Create(FFirebirdClientAPI);
  GetDsqlInfo(Request,Result);
end;

procedure TFBStatement.SetRetainInterfaces(aValue: boolean);
begin
  RetainInterfaces := aValue;
end;

procedure TFBStatement.EnableStatistics(aValue: boolean);
begin
  if FCollectStatistics <> aValue then
  begin
    FCollectStatistics := aValue;
    FStatisticsAvailable := false;
  end;
end;

function TFBStatement.GetPerfStatistics(var stats: TPerfCounters): boolean;
begin
  Result := FStatisticsAvailable;
  if Result then
  begin
    stats[psCurrentMemory] := FAfterStats[psCurrentMemory];
    stats[psDeltaMemory] := FAfterStats[psCurrentMemory] - FBeforeStats[psCurrentMemory];
    stats[psMaxMemory] := FAfterStats[psMaxMemory];
    stats[psRealTime] :=  FAfterStats[psRealTime] - FBeforeStats[psRealTime];
    stats[psUserTime] :=  FAfterStats[psUserTime] - FBeforeStats[psUserTime];
    stats[psReads] := FAfterStats[psReads] - FBeforeStats[psReads];
    stats[psWrites] := FAfterStats[psWrites] - FBeforeStats[psWrites];
    stats[psFetches] := FAfterStats[psFetches] - FBeforeStats[psFetches];
    stats[psBuffers] :=  FAfterStats[psBuffers];
  end;
end;

function TFBStatement.IsInBatchMode: boolean;
begin
  Result := false;
end;

function TFBStatement.HasBatchMode: boolean;
begin
  Result := false;
end;

end.

