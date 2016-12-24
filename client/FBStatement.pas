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

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils,  IB,  FBClientAPI, FBSQLData, FBOutputBlock, FBActivityMonitor,
  FBTransaction;

type
  TPerfStatistics = array[psCurrentMemory..psFetches] of Int64;

  { TFBStatement }

  TFBStatement = class(TActivityReporter)
  private
    FAttachmentIntf: IAttachment;
  protected
    FTransactionIntf: ITransaction;
    FExecTransactionIntf: ITransaction;
    FSQLStatementType: TIBSQLStatementTypes;         { Select, update, delete, insert, create, alter, etc...}
    FSQLDialect: integer;
    FOpen: boolean;
    FPrepared: boolean;
    FPrepareSeqNo: integer; {used to check for out of date references from interfaces}
    FSQL: string;
    FProcessedSQL: string;
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
    procedure CheckHandle; virtual; abstract;
    procedure CheckTransaction(aTransaction: ITransaction);
    procedure GetDsqlInfo(info_request: byte; buffer: ISQLInfoResults); overload; virtual; abstract;
    procedure InternalPrepare;  virtual; abstract;
    function InternalExecute(aTransaction: ITransaction): IResults;  virtual; abstract;
    function InternalOpenCursor(aTransaction: ITransaction): IResultSet;   virtual; abstract;
    procedure FreeHandle;  virtual; abstract;
    procedure InternalClose(Force: boolean); virtual; abstract;
  public
    constructor Create(Attachment: IAttachment; Transaction: ITransaction;
      sql: string; SQLDialect: integer);
    constructor CreateWithParameterNames(Attachment: IAttachment; Transaction: ITransaction;
      sql: string;  SQLDialect: integer; GenerateParamNames: boolean =false);
    destructor Destroy; override;
    procedure Close;
    procedure TransactionEnding(aTransaction: ITransaction; Force: boolean);
    property SQLDialect: integer read FSQLDialect;

  public
    function GetSQLParams: ISQLParams; virtual; abstract;
    function GetMetaData: IMetaData;  virtual; abstract;
    function GetRowsAffected(var SelectCount, InsertCount, UpdateCount,
      DeleteCount: integer): boolean;
    function GetSQLStatementType: TIBSQLStatementTypes;
    function GetSQLText: string;
    function GetSQLDialect: integer;

    {GetDSQLInfo only supports isc_info_sql_stmt_type, isc_info_sql_get_plan, isc_info_sql_records}
    procedure Prepare(aTransaction: ITransaction=nil); virtual;
    function Execute(aTransaction: ITransaction=nil): IResults;
    function OpenCursor(aTransaction: ITransaction=nil): IResultSet;
    function CreateBlob(paramName: string): IBlob; overload;
    function CreateBlob(index: integer): IBlob; overload;
    function CreateBlob(column: TColumnMetaData): IBlob; overload; virtual; abstract;
    function CreateArray(paramName: string): IArray; overload;
    function CreateArray(index: integer): IArray;  overload;
    function CreateArray(column: TColumnMetaData): IArray; overload; virtual; abstract;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    function GetDSQLInfo(Request: byte): ISQLInfoResults; overload;
    procedure SetRetainInterfaces(aValue: boolean); virtual;
    procedure EnableStatistics(aValue: boolean);
    function GetPerfStatistics(var stats: TPerfCounters): boolean;
    property ChangeSeqNo: integer read FChangeSeqNo;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLStatementType: TIBSQLStatementTypes read GetSQLStatementType;
end;

implementation

uses FBMessages;

{ TFBStatement }

procedure TFBStatement.CheckTransaction(aTransaction: ITransaction);
begin
  if (aTransaction = nil) then
    IBError(ibxeTransactionNotAssigned,[]);

  if not aTransaction.InTransaction then
    IBError(ibxeNotInTransaction,[]);
end;

constructor TFBStatement.Create(Attachment: IAttachment;
  Transaction: ITransaction; sql: string; SQLDialect: integer);
begin
  inherited Create(Transaction as TFBTransaction,2);
  FAttachmentIntf := Attachment;
  FTransactionIntf := Transaction;
  FSQLDialect := SQLDialect;
  FSQL := sql;
end;

constructor TFBStatement.CreateWithParameterNames(Attachment: IAttachment;
  Transaction: ITransaction; sql: string; SQLDialect: integer;
  GenerateParamNames: boolean);
begin
  FHasParamNames := true;
  FGenerateParamNames := GenerateParamNames;
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
  if FOpen and (FExecTransactionIntf = aTransaction) then
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

function TFBStatement.GetSQLText: string;
begin
  Result := FSQL;
end;

function TFBStatement.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

procedure TFBStatement.Prepare(aTransaction: ITransaction);
begin
  if FPrepared then FreeHandle;
  if aTransaction <> nil then
  begin
    RemoveMonitor(FTransactionIntf as TFBTransaction);
    FTransactionIntf := aTransaction;
    AddMonitor(FTransactionIntf as TFBTransaction);
  end;
  InternalPrepare;
end;

function TFBStatement.Execute(aTransaction: ITransaction): IResults;
begin
  if aTransaction = nil then
    Result :=  InternalExecute(FTransactionIntf)
  else
    Result := InternalExecute(aTransaction);
end;

function TFBStatement.OpenCursor(aTransaction: ITransaction): IResultSet;
begin
  Close;
  if aTransaction = nil then
    Result := InternalOpenCursor(FTransactionIntf)
  else
    Result := InternalOpenCursor(aTransaction);
end;

function TFBStatement.CreateBlob(paramName: string): IBlob;
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

function TFBStatement.CreateArray(paramName: string): IArray;
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
  Result := TSQLInfoResultsBuffer.Create;
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

end.

