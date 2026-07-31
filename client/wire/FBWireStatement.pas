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
unit FBWireStatement;

{ The IStatement implementation for the wire protocol provider.

  Column and parameter values live in a flat message buffer laid out by
  FBWireMessage; TWireSQLVarData points TSQLDataItem at the right offset in
  it, so the whole of the fbintf data conversion layer applies unchanged. }

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
  Classes, SysUtils, IB, FBStatement, FBSQLData, FBClientAPI, FBTransaction,
  FBActivityMonitor, FBOutputBlock, FBWireClientAPI, FBWireProtocol,
  FBWireMessage, FBWireDescribe;

const
  {rows requested from the server in a single op_fetch}
  DefaultFetchBatchSize = 200;

type
  TFBWireStatement = class;
  TWireSQLDataArea = class;
  PWireSQLVarRec = ^TWireSQLVar;

  { TWireSQLVarData }

  TWireSQLVarData = class(TSQLVarData)
  private
    FStatement: TFBWireStatement;
    FOwner: TWireSQLDataArea;
    FBlob: IBlob;
    FBlobMetaData: IBlobMetaData;
    function GetVar: PWireSQLVarRec;
    function BufferBase: PByte;
  protected
    function GetSQLType: cardinal; override;
    function GetSubtype: integer; override;
    function GetAliasName: AnsiString; override;
    function GetFieldName: AnsiString; override;
    function GetOwnerName: AnsiString; override;
    function GetRelationName: AnsiString; override;
    function GetScale: integer; override;
    function GetCharSetID: cardinal; override;
    function GetIsNull: Boolean; override;
    function GetIsNullable: boolean; override;
    function GetSQLData: PByte; override;
    function GetDataLength: cardinal; override;
    function GetSize: cardinal; override;
    function GetDefaultTextSQLType: cardinal; override;
    procedure InternalSetSQLType(aValue: cardinal; aSubType: integer); override;
    procedure InternalSetScale(aValue: integer); override;
    procedure InternalSetDataLength(len: cardinal); override;
    procedure SetIsNull(Value: Boolean); override;
    procedure SetIsNullable(Value: Boolean); override;
    procedure SetSQLData(AValue: PByte; len: cardinal); override;
    procedure SetCharSetID(aValue: cardinal); override;
  public
    constructor Create(aParent: TWireSQLDataArea; aIndex: integer);
    procedure RowChange; override;
    function GetAsArray: IArray; override;
    function GetAsBlob(Blob_ID: TISC_QUAD; BPB: IBPB): IBlob; override;
    function CreateBlob: IBlob; override;
    function GetArrayMetaData: IArrayMetaData; override;
    function GetBlobMetaData: IBlobMetaData; override;
    procedure Initialize; override;
  end;

  { TWireSQLDataArea }

  TWireSQLDataArea = class(TSQLDataArea)
  private
    FStatement: TFBWireStatement;
    FIsInput: boolean;
    FFormat: TWireMessageFormat;
    FBuffer: TBytes;
  protected
    function GetStatement: IStatement; override;
    function GetPrepareSeqNo: integer; override;
    function GetTransactionSeqNo: integer; override;
    procedure SetCount(aValue: integer); override;
  public
    constructor Create(aStatement: TFBWireStatement; aIsInput: boolean);
    {rebuilds the column list from a describe response}
    procedure Bind(const aFormat: TWireMessageFormat; aBufferSize: cardinal);
    function IsInputDataArea: boolean; override;
    function CheckStatementStatus(Request: TStatementStatus): boolean; override;
    function StateChanged(var ChangeSeqNo: integer): boolean; override;
    function CanChangeMetaData: boolean; override;
    procedure ClearBuffer;
    property Format: TWireMessageFormat read FFormat;
    property Buffer: TBytes read FBuffer;
    property Statement: TFBWireStatement read FStatement;
  end;

  { TWireResultSet }

  TWireResultSet = class(TResults,IResultSet)
  private
    FResults: TWireSQLDataArea;
    FCursorSeqNo: integer;
  public
    constructor Create(aResults: TWireSQLDataArea);
    {IResultSet}
    function FetchNext: boolean;
    function FetchPrior: boolean;
    function FetchFirst: boolean;
    function FetchLast: boolean;
    function FetchAbsolute(position: Integer): boolean;
    function FetchRelative(offset: Integer): boolean;
    function GetCursorName: AnsiString;
    function IsBof: boolean;
    function IsEof: boolean;
    procedure Close;
  end;

  { TFBWireStatement }

  TFBWireStatement = class(TFBStatement,IStatement)
  private
    FWireAPI: TFBWireClientAPI;
    FHandle: integer;
    FHasHandle: boolean;
    FSQLParams: TWireSQLDataArea;
    FSQLRecord: TWireSQLDataArea;
    FStatementInfo: TWireStatementInfo;
    FCursorState: TWireCursorState;
    FCursorName: AnsiString;
    FCursorSeqNo: integer;
    function GetConnection: TFBWireConnection;
    function GetTransactionHandle: integer;
  protected
    procedure CheckHandle; override;
    function GetStatementIntf: IStatement; override;
    procedure GetDsqlInfo(info_request: byte; buffer: ISQLInfoResults); override;
    procedure InternalPrepare(CursorName: AnsiString = ''); override;
    function InternalExecute(Transaction: ITransaction): IResults; override;
    function InternalOpenCursor(aTransaction: ITransaction; Scrollable: boolean): IResultSet; override;
    procedure ProcessSQL(sql: AnsiString; GenerateParamNames: boolean;
                var processedSQL: AnsiString); override;
    procedure FreeHandle; override;
    procedure InternalClose(Force: boolean); override;
  public
    constructor Create(Attachment: IAttachment; Transaction: ITransaction;
                sql: AnsiString; SQLDialect: integer; CursorName: AnsiString = '');
    constructor CreateWithNamedParameters(Attachment: IAttachment;
                Transaction: ITransaction; sql: AnsiString; SQLDialect: integer;
                GenerateParamNames: boolean = false;
                CaseSensitiveParams: boolean = false; CursorName: AnsiString = '');
    destructor Destroy; override;
    function FetchNextRow: boolean;
    function GetSQLParams: ISQLParams; override;
    function GetMetaData: IMetaData; override;
    function CreateBlob(column: TColumnMetaData): IBlob; override;
    function CreateArray(column: TColumnMetaData): IArray; override;
    function GetPlan: AnsiString;
    function IsPrepared: boolean;

    property Connection: TFBWireConnection read GetConnection;
    property Handle: integer read FHandle;
    property WireAPI: TFBWireClientAPI read FWireAPI;
  end;

implementation

uses FBMessages, IBErrorCodes, FBWireAttachment, FBWireTransaction, FBWireBlob,
  FBWireConst, IBUtils;

{ TWireSQLVarData }

constructor TWireSQLVarData.Create(aParent: TWireSQLDataArea; aIndex: integer);
begin
  inherited Create(aParent,aIndex);
  FOwner := aParent;
  FStatement := aParent.Statement;
end;

function TWireSQLVarData.GetVar: PWireSQLVarRec;
begin
  if (Index < 0) or (Index >= Length(FOwner.FFormat)) then
    IBError(ibxeInvalidColumnIndex,[nil]);
  Result := @FOwner.FFormat[Index];
end;

function TWireSQLVarData.BufferBase: PByte;
begin
  if Length(FOwner.FBuffer) = 0 then
    IBError(ibxeInvalidStatementHandle,[nil]);
  Result := @FOwner.FBuffer[0];
end;

function TWireSQLVarData.GetSQLType: cardinal;
begin
  Result := GetVar^.SQLType;
end;

function TWireSQLVarData.GetSubtype: integer;
begin
  Result := GetVar^.SQLSubType;
end;

function TWireSQLVarData.GetAliasName: AnsiString;
begin
  Result := GetVar^.AliasName;
end;

function TWireSQLVarData.GetFieldName: AnsiString;
begin
  Result := GetVar^.FieldName;
end;

function TWireSQLVarData.GetOwnerName: AnsiString;
begin
  Result := GetVar^.OwnerName;
end;

function TWireSQLVarData.GetRelationName: AnsiString;
begin
  Result := GetVar^.RelationName;
end;

function TWireSQLVarData.GetScale: integer;
begin
  Result := GetVar^.Scale;
end;

function TWireSQLVarData.GetCharSetID: cardinal;
begin
  case GetSQLType of
  SQL_TEXT, SQL_VARYING:
    Result := GetVar^.CharSetID;
  SQL_BLOB:
    if GetSubType = 1 then  {text blob}
      Result := GetVar^.CharSetID
    else
      Result := 0;
  else
    Result := 0;
  end;
end;

function TWireSQLVarData.GetIsNull: Boolean;
begin
  {the null indicator in the message buffer is the single source of truth:
   it is what XDREncodeMessage transmits}
  Result := PInteger(BufferBase + GetVar^.NullOffset)^ <> 0;
end;

function TWireSQLVarData.GetIsNullable: boolean;
begin
  {an input parameter may always be set to null, whatever the column it is
   compared against. Reporting otherwise would stop the base class clearing
   the null indicator when a value is assigned.}
  Result := FOwner.IsInputDataArea or GetVar^.Nullable;
end;

function TWireSQLVarData.GetSQLData: PByte;
begin
  Result := BufferBase + GetVar^.DataOffset;
end;

function TWireSQLVarData.GetDataLength: cardinal;
begin
  {for VARYING the current length is the two byte prefix}
  if GetSQLType = SQL_VARYING then
    Result := PWord(GetSQLData)^
  else
    Result := GetVar^.DataSize;
end;

function TWireSQLVarData.GetSize: cardinal;
begin
  Result := GetVar^.DataSize;
end;

function TWireSQLVarData.GetDefaultTextSQLType: cardinal;
begin
  Result := SQL_TEXT;
end;

procedure TWireSQLVarData.InternalSetSQLType(aValue: cardinal; aSubType: integer);
begin
  GetVar^.SQLType := aValue;
  GetVar^.SQLSubType := aSubType;
end;

procedure TWireSQLVarData.InternalSetScale(aValue: integer);
begin
  GetVar^.Scale := aValue;
end;

procedure TWireSQLVarData.InternalSetDataLength(len: cardinal);
begin
  if GetSQLType = SQL_VARYING then
    PWord(GetSQLData)^ := len
  else
    GetVar^.DataSize := len;
end;

procedure TWireSQLVarData.SetIsNull(Value: Boolean);
begin
  if Value then
  begin
    GetVar^.Nullable := true;
    PInteger(BufferBase + GetVar^.NullOffset)^ := -1;
  end
  else
    PInteger(BufferBase + GetVar^.NullOffset)^ := 0;
  Changed;
end;

procedure TWireSQLVarData.SetIsNullable(Value: Boolean);
begin
  GetVar^.Nullable := Value;
end;

procedure TWireSQLVarData.SetSQLData(AValue: PByte; len: cardinal);
var p: PByte;
begin
  if len > GetVar^.BufferSize then
    IBError(ibxeStringOverflow,[len,GetVar^.BufferSize]);
  p := GetSQLData;
  case GetSQLType of
  SQL_VARYING:
    begin
      {a varying value is a two byte length followed by the characters}
      if len > 0 then
        Move(AValue^,(p+2)^,len);
      PWord(p)^ := len;
    end;
  SQL_TEXT:
    begin
      {CHAR values are blank padded to their full width: the whole field is
       transmitted, and trailing nulls would not compare equal to a blank
       padded column}
      if len > 0 then
        Move(AValue^,p^,len);
      if len < GetVar^.DataSize then
        FillChar((p+len)^,GetVar^.DataSize - len,' ');
    end;
  else
    if len > 0 then
      Move(AValue^,p^,len);
  end;
  {writing a value clears the null indicator}
  PInteger(BufferBase + GetVar^.NullOffset)^ := 0;
end;

procedure TWireSQLVarData.SetCharSetID(aValue: cardinal);
begin
  GetVar^.CharSetID := aValue;
end;

procedure TWireSQLVarData.RowChange;
begin
  inherited RowChange;
  FBlob := nil;
end;

function TWireSQLVarData.GetAsArray: IArray;
begin
  IBError(ibxeNotSupported,[nil]);
  Result := nil;
end;

function TWireSQLVarData.GetAsBlob(Blob_ID: TISC_QUAD; BPB: IBPB): IBlob;
begin
  if FBlob <> nil then
    Result := FBlob
  else
  begin
    Result := TFBWireBlob.Create(FStatement.GetAttachment as TFBWireAttachment,
                FStatement.GetTransaction as TFBWireTransaction,
                GetBlobMetaData,Blob_ID,BPB);
    FBlob := Result;
  end;
end;

function TWireSQLVarData.CreateBlob: IBlob;
begin
  Result := TFBWireBlob.Create(FStatement.GetAttachment as TFBWireAttachment,
              FStatement.GetTransaction as TFBWireTransaction,
              GetBlobMetaData,nil);
end;

function TWireSQLVarData.GetArrayMetaData: IArrayMetaData;
begin
  IBError(ibxeNotSupported,[nil]);
  Result := nil;
end;

function TWireSQLVarData.GetBlobMetaData: IBlobMetaData;
begin
  if GetSQLType <> SQL_BLOB then
    IBError(ibxeInvalidDataConversion,[nil]);
  if FBlobMetaData = nil then
    FBlobMetaData := TFBWireBlobMetaData.Create(
      FStatement.GetAttachment as TFBWireAttachment,
      FStatement.GetTransaction as TFBWireTransaction,
      GetRelationName,GetFieldName,GetSubType,GetVar^.CharSetID);
  Result := FBlobMetaData;
end;

procedure TWireSQLVarData.Initialize;
begin
  inherited Initialize;
  FBlob := nil;
end;

{ TWireSQLDataArea }

constructor TWireSQLDataArea.Create(aStatement: TFBWireStatement;
  aIsInput: boolean);
begin
  inherited Create;
  FStatement := aStatement;
  FIsInput := aIsInput;
end;

function TWireSQLDataArea.GetStatement: IStatement;
begin
  Result := FStatement;
end;

function TWireSQLDataArea.GetPrepareSeqNo: integer;
begin
  Result := FStatement.FPrepareSeqNo;
end;

function TWireSQLDataArea.GetTransactionSeqNo: integer;
begin
  Result := (FStatement.FTransactionIntf as TObject as TFBTransaction).TransactionSeqNo;
end;

procedure TWireSQLDataArea.SetCount(aValue: integer);
var i, oldCount: integer;
begin
  oldCount := Length(FColumnList);
  SetLength(FColumnList,aValue);
  for i := oldCount to aValue - 1 do
    FColumnList[i] := TWireSQLVarData.Create(self,i);
end;

procedure TWireSQLDataArea.Bind(const aFormat: TWireMessageFormat;
  aBufferSize: cardinal);
var i: integer;
begin
  FFormat := aFormat;
  SetLength(FBuffer,aBufferSize);
  ClearBuffer;
  SetCount(Length(FFormat));
  for i := 0 to Length(FFormat) - 1 do
  begin
    FColumnList[i].Name := FFormat[i].AliasName;
    FColumnList[i].Initialize;
  end;
  SetUniqueRelationName;
end;

procedure TWireSQLDataArea.ClearBuffer;
var i: integer;
begin
  if Length(FBuffer) > 0 then
    FillChar(FBuffer[0],Length(FBuffer),0);
  {an unset input parameter defaults to null}
  if FIsInput then
    for i := 0 to Length(FFormat) - 1 do
      PInteger(@FBuffer[0] + FFormat[i].NullOffset)^ := -1;
end;

function TWireSQLDataArea.IsInputDataArea: boolean;
begin
  Result := FIsInput;
end;

function TWireSQLDataArea.CheckStatementStatus(Request: TStatementStatus): boolean;
begin
  Result := false;
  case Request of
  ssPrepared:
    Result := FStatement.FPrepared;
  ssExecuteResults:
    Result := not FStatement.FOpen and FStatement.FSingleResults;
  ssCursorOpen:
    Result := FStatement.FOpen;
  ssBOF:
    Result := FStatement.FBOF;
  ssEOF:
    Result := FStatement.FEOF;
  end;
end;

function TWireSQLDataArea.StateChanged(var ChangeSeqNo: integer): boolean;
begin
  Result := ChangeSeqNo <> FStatement.ChangeSeqNo;
  if Result then
    ChangeSeqNo := FStatement.ChangeSeqNo;
end;

function TWireSQLDataArea.CanChangeMetaData: boolean;
begin
  {the message format is fixed by the prepare}
  Result := false;
end;

{ TWireResultSet }

constructor TWireResultSet.Create(aResults: TWireSQLDataArea);
begin
  inherited Create(aResults);
  FResults := aResults;
  FCursorSeqNo := aResults.Statement.FCursorSeqNo;
end;

function TWireResultSet.FetchNext: boolean;
begin
  Result := FResults.Statement.FetchNextRow;
  if Result then
    FResults.RowChange;
end;

function TWireResultSet.FetchPrior: boolean;
begin
  {scrollable cursors need protocol 18 and op_fetch_scroll}
  IBError(ibxeNotSupported,[nil]);
  Result := false;
end;

function TWireResultSet.FetchFirst: boolean;
begin
  IBError(ibxeNotSupported,[nil]);
  Result := false;
end;

function TWireResultSet.FetchLast: boolean;
begin
  IBError(ibxeNotSupported,[nil]);
  Result := false;
end;

function TWireResultSet.FetchAbsolute(position: Integer): boolean;
begin
  IBError(ibxeNotSupported,[nil]);
  Result := false;
end;

function TWireResultSet.FetchRelative(offset: Integer): boolean;
begin
  IBError(ibxeNotSupported,[nil]);
  Result := false;
end;

function TWireResultSet.GetCursorName: AnsiString;
begin
  Result := FResults.Statement.FCursorName;
end;

function TWireResultSet.IsBof: boolean;
begin
  Result := FResults.Statement.FBOF;
end;

function TWireResultSet.IsEof: boolean;
begin
  Result := FResults.Statement.FEOF;
end;

procedure TWireResultSet.Close;
begin
  if FCursorSeqNo = FResults.Statement.FCursorSeqNo then
    FResults.Statement.Close;
end;

{ TFBWireStatement }

constructor TFBWireStatement.Create(Attachment: IAttachment;
  Transaction: ITransaction; sql: AnsiString; SQLDialect: integer;
  CursorName: AnsiString);
begin
  FWireAPI := (Attachment as TObject as TFBWireAttachment).WireAPI;
  FSQLParams := TWireSQLDataArea.Create(self,true);
  FSQLRecord := TWireSQLDataArea.Create(self,false);
  FCursorName := CursorName;
  inherited Create(Attachment,Transaction,sql,SQLDialect);
  InternalPrepare(CursorName);
end;

constructor TFBWireStatement.CreateWithNamedParameters(Attachment: IAttachment;
  Transaction: ITransaction; sql: AnsiString; SQLDialect: integer;
  GenerateParamNames: boolean; CaseSensitiveParams: boolean;
  CursorName: AnsiString);
begin
  FWireAPI := (Attachment as TObject as TFBWireAttachment).WireAPI;
  FSQLParams := TWireSQLDataArea.Create(self,true);
  FSQLRecord := TWireSQLDataArea.Create(self,false);
  FCursorName := CursorName;
  inherited CreateWithParameterNames(Attachment,Transaction,sql,SQLDialect,
                                     GenerateParamNames,CaseSensitiveParams);
  FSQLParams.CaseSensitiveParams := CaseSensitiveParams;
  InternalPrepare(CursorName);
end;

destructor TFBWireStatement.Destroy;
begin
  inherited Destroy;
  if FSQLParams <> nil then FSQLParams.Free;
  if FSQLRecord <> nil then FSQLRecord.Free;
end;

function TFBWireStatement.GetConnection: TFBWireConnection;
begin
  Result := (GetAttachment as TObject as TFBWireAttachment).Connection;
end;

function TFBWireStatement.GetTransactionHandle: integer;
begin
  Result := (FTransactionIntf as TObject as TFBWireTransaction).Handle;
end;

procedure TFBWireStatement.CheckHandle;
begin
  if not FHasHandle then
    IBError(ibxeInvalidStatementHandle,[nil]);
end;

function TFBWireStatement.GetStatementIntf: IStatement;
begin
  Result := self;
end;

procedure TFBWireStatement.ProcessSQL(sql: AnsiString;
  GenerateParamNames: boolean; var processedSQL: AnsiString);
begin
  FSQLParams.PreprocessSQL(sql,GenerateParamNames,processedSQL);
end;

procedure TFBWireStatement.InternalPrepare(CursorName: AnsiString);
var response: TBytes;
begin
  if FPrepared then Exit;
  if CursorName <> '' then
    FCursorName := CursorName;
  if (FSQL = '') then
    IBError(ibxeEmptyQuery,[nil]);
  CheckTransaction(FTransactionIntf);
  try
    if not FHasHandle then
    begin
      FHandle := Connection.AllocateStatement(
                   (GetAttachment as TObject as TFBWireAttachment).Handle);
      FHasHandle := true;
    end;
    if FHasParamNames then
    begin
      if FProcessedSQL = '' then
        ProcessSQL(FSQL,FGenerateParamNames,FProcessedSQL);
      response := Connection.PrepareStatement(GetTransactionHandle,FHandle,
                    FSQLDialect,FProcessedSQL,DescribeItems,DefaultBufferSize);
    end
    else
      response := Connection.PrepareStatement(GetTransactionHandle,FHandle,
                    FSQLDialect,FSQL,DescribeItems,DefaultBufferSize);
    FStatementInfo := ParsePrepareResponse(response);
    if FStatementInfo.Truncated then
      IBError(ibxeInfoBufferTypeError,[nil]);
    FSQLStatementType := TIBSQLStatementTypes(FStatementInfo.StatementType);
    FSQLParams.Bind(FStatementInfo.InputFormat,FStatementInfo.InputBufferSize);
    FSQLRecord.Bind(FStatementInfo.OutputFormat,FStatementInfo.OutputBufferSize);
    if FCursorName <> '' then
      Connection.SetCursorName(FHandle,FCursorName);
  except
    on E: Exception do
    begin
      FPrepared := false;
      WireIBError(FWireAPI,E);
    end;
  end;
  FPrepared := true;
  FSingleResults := false;
  Inc(FPrepareSeqNo);
  Inc(FChangeSeqNo);
end;

function TFBWireStatement.InternalExecute(Transaction: ITransaction): IResults;
var paramPtr, outPtr: PByte;
begin
  Result := nil;
  CheckTransaction(Transaction);
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  FBOF := false;
  FEOF := false;
  FSingleResults := false;
  FillChar(FCursorState,SizeOf(FCursorState),0);

  paramPtr := nil;
  if Length(FSQLParams.Buffer) > 0 then
    paramPtr := @FSQLParams.Buffer[0];
  outPtr := nil;
  if Length(FSQLRecord.Buffer) > 0 then
    outPtr := @FSQLRecord.Buffer[0];
  try
    if (FSQLStatementType = SQLExecProcedure) and (FSQLRecord.Count > 0) then
    begin
      {a singleton result comes back with the execute}
      Connection.ExecuteStatement2((FTransactionIntf as TObject as TFBWireTransaction).Handle,
        FHandle,FSQLParams.Format,paramPtr,FSQLRecord.Format,outPtr);
      FSingleResults := true;
      FSQLRecord.RowChange;
      Result := TResults.Create(FSQLRecord);
    end
    else
      Connection.ExecuteStatement(FHandle,
        (FTransactionIntf as TObject as TFBWireTransaction).Handle,
        FSQLParams.Format,paramPtr);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  Inc(FChangeSeqNo);
end;

function TFBWireStatement.InternalOpenCursor(aTransaction: ITransaction;
  Scrollable: boolean): IResultSet;
var paramPtr: PByte;
begin
  if Scrollable then
    IBError(ibxeNotSupported,[nil]);
  CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  if FSQLRecord.Count = 0 then
    IBError(ibxeIsASelectStatement,[nil]);
  paramPtr := nil;
  if Length(FSQLParams.Buffer) > 0 then
    paramPtr := @FSQLParams.Buffer[0];
  try
    Connection.ExecuteStatement(FHandle,
      (aTransaction as TObject as TFBWireTransaction).Handle,
      FSQLParams.Format,paramPtr);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  FillChar(FCursorState,SizeOf(FCursorState),0);
  FOpen := true;
  FBOF := true;
  FEOF := false;
  Inc(FCursorSeqNo);
  Inc(FChangeSeqNo);
  FExecTransactionIntf := aTransaction;
  Result := TWireResultSet.Create(FSQLRecord);
end;

function TFBWireStatement.FetchNextRow: boolean;
begin
  Result := false;
  if not FOpen then Exit;
  try
    Result := Connection.FetchRow(FHandle,FSQLRecord.Format,
                @FSQLRecord.Buffer[0],DefaultFetchBatchSize,FCursorState);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  if Result then
  begin
    FBOF := false;
    Inc(FChangeSeqNo);
  end
  else
    FEOF := true;
end;

procedure TFBWireStatement.InternalClose(Force: boolean);
begin
  if not Connection.Connected then
    Force := true;
  if FHasHandle and FOpen and not Force then
  try
    Connection.FreeStatement(FHandle,DSQL_close);
  except
    on E: EFBWireProtocolError do
      {ending the transaction closes its cursors, so the server may have
       closed this one already}
      if not ((Length(E.Status) > 0) and
              (E.Status[0].IntValue = isc_dsql_cursor_close_err)) then
        WireIBError(FWireAPI,E);
    on E: Exception do
      WireIBError(FWireAPI,E);
  end;
  FOpen := false;
  FEOF := true;
  FBOF := false;
  FillChar(FCursorState,SizeOf(FCursorState),0);
  FExecTransactionIntf := nil;
  Inc(FChangeSeqNo);
end;

procedure TFBWireStatement.FreeHandle;
begin
  if not FHasHandle then Exit;
  if Connection.Connected then
  try
    Connection.FreeStatement(FHandle,DSQL_drop);
  except
    {the server has already released the statement with the connection}
  end;
  FHasHandle := false;
  FHandle := 0;
  FPrepared := false;
end;

procedure TFBWireStatement.GetDsqlInfo(info_request: byte;
  buffer: ISQLInfoResults);
var items, response: TBytes;
    len: integer;
begin
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  SetLength(items,1);
  items[0] := info_request;
  SetLength(response,0);
  try
    response := Connection.GetInfo(op_info_sql,FHandle,items,
                                   (buffer as TSQLInfoResultsBuffer).getBufSize);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  len := Length(response);
  if len > (buffer as TSQLInfoResultsBuffer).getBufSize then
    len := (buffer as TSQLInfoResultsBuffer).getBufSize;
  if len > 0 then
    Move(response[0],(buffer as TSQLInfoResultsBuffer).Buffer^,len);
end;

function TFBWireStatement.GetSQLParams: ISQLParams;
begin
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  Result := TSQLParams.Create(FSQLParams);
end;

function TFBWireStatement.GetMetaData: IMetaData;
begin
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  Result := TMetaData.Create(FSQLRecord);
end;

function TFBWireStatement.CreateBlob(column: TColumnMetaData): IBlob;
begin
  if column.SQLType <> SQL_BLOB then
    IBError(ibxeNotABlob,[nil]);
  Result := TFBWireBlob.Create(GetAttachment as TFBWireAttachment,
              GetTransaction as TFBWireTransaction,column.GetBlobMetaData,nil);
end;

function TFBWireStatement.CreateArray(column: TColumnMetaData): IArray;
begin
  IBError(ibxeNotSupported,[nil]);
  Result := nil;
end;

function TFBWireStatement.GetPlan: AnsiString;
var info: ISQLInfoResults;
begin
  Result := '';
  if not (FSQLStatementType in [SQLSelect,SQLSelectForUpdate,SQLExecProcedure,
                                SQLUpdate,SQLDelete,SQLInsert]) then
    Exit;
  info := GetDSQLInfo(isc_info_sql_get_plan);
  if info.Count > 0 then
    Result := Trim(info[0].GetAsString);
end;

function TFBWireStatement.IsPrepared: boolean;
begin
  Result := FPrepared;
end;

end.
