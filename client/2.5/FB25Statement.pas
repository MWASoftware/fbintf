unit FB25Statement;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$DEFINE HAS_ANSISTRING_CODEPAGE}
{$ENDIF}

{This unit is hacked from IBSQL and contains the code for managing an XSQLDA and
 SQLVars, along with statement preparation, execution and cursor management.
 Most of the SQLVar code has been moved to unit FBSQLData. Client access is
 provided through interface rather than direct access to the XSQLDA and XSQLVar
 objects.}

{
  Note on reference counted interfaces.
  ------------------------------------

  TFB25Statement manages both an input and an output SQLDA through the TIBXINPUTSQLDA
  and TIBXOUTPUTSQLDA objects. As pure objects, these are explicitly destroyed
  when the statement is destroyed.

  However, IResultSet is an interface and is returned when a cursor is opened and
  has a reference for the TIBXOUTPUTSQLDA. The   user may discard their reference
  to the IStatement while still using the   IResultSet. This would be a problem if t
  he underlying TFB25Statement object and its TIBXOUTPUTSQLDA is destroyed while
  still leaving the TIBXResultSet object in place. Calls to (e.g.)   FetchNext would fail.

  To avoid this problem, TResultsSet objects  have a reference to the IStatement
  interface of the TFB25Statement object. Thus, as long as these "copies" exist,
  the owning statement is not destroyed even if the user discards their reference
  to the statement. Note: the TFB25Statement does not have a reference to the TIBXResultSet
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
  Classes, SysUtils, IB,  FBClientAPI, FB25ClientAPI, FB25Transaction, FB25Attachment,
  IBHeader, IBExternals, FBSQLData, FBOutputBlock, FBActivityMonitor;

type
  TFB25Statement = class;
  TIBXSQLDA = class;

   { TIBXSQLVAR }

  TIBXSQLVAR = class(TSQLVarData)
  private
    FStatement: TFB25Statement;
    FBlob: IBlob;             {Cache references}
    FArray: IArray;
    FNullIndicator: short;
    FOwnsSQLData: boolean;
    FBlobMetaData: IBlobMetaData;
    FArrayMetaData: IArrayMetaData;
    FXSQLVAR: PXSQLVAR;       { Point to the PXSQLVAR in the owner object }
  protected
    function GetSQLType: cardinal; override;
    function GetSubtype: cardinal; override;
    function GetAliasName: string;  override;
    function GetFieldName: string; override;
    function GetOwnerName: string;  override;
    function GetRelationName: string;  override;
    function GetScale: cardinal; override;
    function GetCharSetID: cardinal; override;
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    function GetCodePage: TSystemCodePage; override;
    {$ENDIF}
    function GetIsNull: Boolean;   override;
    function GetIsNullable: boolean; override;
    function GetSQLData: PChar;  override;
    function GetDataLength: cardinal; override;
    function GetArrayMetaData: IArrayMetaData; override;
    function GetBlobMetaData: IBlobMetaData; override;
    procedure SetIsNull(Value: Boolean); override;
    procedure SetIsNullable(Value: Boolean);  override;
    procedure SetSQLData(AValue: PChar; len: cardinal); override;
    procedure SetScale(aValue: cardinal); override;
    procedure SetDataLength(len: cardinal); override;
    procedure SetSQLType(aValue: cardinal); override;
    procedure SetCharSetID(aValue: cardinal); override;
  public
    constructor Create(aParent: TIBXSQLDA; aIndex: integer);
    procedure FreeSQLData;
    procedure RowChange; override;
    function GetAsArray(Array_ID: TISC_QUAD): IArray; override;
    function GetAsBlob(Blob_ID: TISC_QUAD): IBlob; override;
    function CreateBlob: IBlob; override;
    procedure Initialize; override;

    property Statement: TFB25Statement read FStatement;
  end;

  TIBXINPUTSQLDA = class;

  { TIBXSQLDA }

  TIBXSQLDA = class(TSQLDataArea)
  private
    FCount: Integer; {Columns in use - may be less than inherited columns}
    FSize: Integer;  {Number of TIBXSQLVARs in column list}
    FXSQLDA: PXSQLDA;
    FTransactionSeqNo: integer;
    function GetRecordSize: Integer;
    function GetXSQLDA: PXSQLDA;
  protected
    FStatement: TFB25Statement;
    function GetTransactionSeqNo: integer; override;
    procedure FreeXSQLDA;
    function GetStatement: IStatement; override;
    function GetPrepareSeqNo: integer; override;
    procedure SetCount(Value: Integer); override;
  public
    constructor Create(aStatement: TFB25Statement);
    destructor Destroy; override;
    function CheckStatementStatus(Request: TStatementStatus): boolean; override;
    function ColumnsInUseCount: integer; override;
    function GetTransaction: TFB25Transaction; virtual;
    procedure Initialize; override;
    property AsXSQLDA: PXSQLDA read GetXSQLDA;
    property Count: Integer read FCount write SetCount;
    property RecordSize: Integer read GetRecordSize;
    property Statement: TFB25Statement read FStatement;
  end;

  { TIBXINPUTSQLDA }

  TIBXINPUTSQLDA = class(TIBXSQLDA)
  public
    procedure Bind;
    function IsInputDataArea: boolean; override;
  end;


  { TIBXOUTPUTSQLDA }

  TIBXOUTPUTSQLDA = class(TIBXSQLDA)
  private
     FTransaction: TFB25Transaction; {transaction used to execute the statement}
  public
    procedure Bind;
    function GetTransaction: TFB25Transaction; override;
    function IsInputDataArea: boolean; override;
  end;

  { TResultSet }

  TResultSet = class(TResults,IResultSet)
  private
    FResults: TIBXOUTPUTSQLDA;
  public
    constructor Create(aResults: TIBXOUTPUTSQLDA);
    destructor Destroy; override;
    {IResultSet}
    function FetchNext: boolean;
    function GetCursorName: string;
    function GetTransaction: ITransaction; override;
    procedure Close;
  end;

  { TFB25Statement }

  TFB25Statement = class(TActivityReporter,IStatement)
  private
    FAttachment: TFB25Attachment;
    FAttachmentIntf: IAttachment;
    FTransaction: TFB25Transaction;
    FTransactionIntf: ITransaction;
    FExecTransactionIntf: ITransaction;
    FHandle: TISC_STMT_HANDLE;
    FSQLStatementType: TIBSQLStatementTypes;         { Select, update, delete, insert, create, alter, etc...}
    FSQLDialect: integer;
    FSQLParams: TIBXINPUTSQLDA;
    FSQLRecord: TIBXOUTPUTSQLDA;
    FOpen: boolean;
    FCursor: String;               { Cursor name...}
    FPrepared: boolean;
    FPrepareSeqNo: integer; {used to check for out of date references from interfaces}
    FSQL: string;
    FProcessedSQL: string;
    FHasParamNames: boolean;
    FBOF: boolean;
    FEOF: boolean;
    FSingleResults: boolean;
    FGenerateParamNames: boolean;
    FUniqueParamNames: boolean;
    procedure CheckTransaction(aTransaction: TFB25Transaction);
    procedure CheckHandle;
    procedure InternalPrepare;
    function InternalExecute(aTransaction: TFB25Transaction): IResults;
    function InternalOpenCursor(aTransaction: TFB25Transaction): IResultSet;
    procedure FreeHandle;
    procedure InternalClose(Force: boolean);
  public
    constructor Create(Attachment: TFB25Attachment; Transaction: ITransaction;
      sql: string; SQLDialect: integer);
    constructor CreateWithParameterNames(Attachment: TFB25Attachment; Transaction: ITransaction;
      sql: string;  SQLDialect: integer; GenerateParamNames: boolean =false; UniqueParamNames: boolean=false);
    destructor Destroy; override;
    procedure Close;
    function FetchNext: boolean;
    procedure TransactionEnding(aTransaction: TFB25Transaction; Force: boolean);
    property SQLDialect: integer read FSQLDialect;
    property Attachment: TFB25Attachment read FAttachment;
    property Transaction: TFB25Transaction read FTransaction;

  public
    {IStatement}
    function GetSQLParams: ISQLParams;
    function GetMetaData: IMetaData;
    function GetPlan: String;
    function GetRowsAffected(var SelectCount, InsertCount, UpdateCount,
      DeleteCount: integer): boolean;
    function GetSQLStatementType: TIBSQLStatementTypes;
    function GetSQLText: string;
    function GetSQLDialect: integer;
    function IsPrepared: boolean;
    procedure Prepare(aTransaction: ITransaction=nil);
    function Execute(aTransaction: ITransaction=nil): IResults;
    function OpenCursor(aTransaction: ITransaction=nil): IResultSet;
    function CreateBlob: IBlob;
    function CreateArray(column: IColumnMetaData): IArray; overload;
    function CreateArray(columnName: string): IArray; overload;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    property Handle: TISC_STMT_HANDLE read FHandle;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLStatementType: TIBSQLStatementTypes read GetSQLStatementType;

end;

implementation

uses IBUtils, FBMessages, FB25Blob, variants, IBErrorCodes, FBArray, FB25Array;

type

  { ISQLInfoResultsItem }

  ISQLInfoResultsItem = interface
    function getItemType: byte;
    function getSize: integer;
    function getAsString: string;
    function getAsInteger: integer;
    function GetCount: integer;
    function GetItem(index: integer): ISQLInfoResultsItem;
    function Find(ItemType: byte): ISQLInfoResultsItem;
    property Count: integer read GetCount;
    property Items[index: integer]: ISQLInfoResultsItem read getItem; default;
  end;

  ISQLInfoResultsBuffer = interface
    function GetCount: integer;
    procedure Exec(info_request: byte);
    function GetItem(index: integer): ISQLInfoResultsItem;
    function Find(ItemType: byte): ISQLInfoResultsItem;
    property Count: integer read GetCount;
    property Items[index: integer]: ISQLInfoResultsItem read getItem; default;
  end;

  { TSQLInfoResultsBuffer }

  TSQLInfoResultsBuffer = class(TOutputBlock,ISQLInfoResultsBuffer)
  private
    FStatement: TFB25Statement;
  protected
    function AddListItem(BufPtr: PChar): POutputBlockItemData; override;
    procedure DoParseBuffer; override;
  public
    constructor Create(aStatement: TFB25Statement; aSize: integer = 1024);
    procedure Exec(info_request: byte);
    function GetItem(index: integer): ISQLInfoResultsItem;
    function Find(ItemType: byte): ISQLInfoResultsItem;
    property Count: integer read GetCount;
    property Items[index: integer]: ISQLInfoResultsItem read getItem; default;
  end;

  { TSQLInfoResultsItem }

  TSQLInfoResultsItem = class(TOutputBlockItemGroup,ISQLInfoResultsItem)
    function GetItem(index: integer): ISQLInfoResultsItem;
    function Find(ItemType: byte): ISQLInfoResultsItem;
  end;

{ TSQLInfoResultsItem }

function TSQLInfoResultsItem.GetItem(index: integer): ISQLInfoResultsItem;
var P: POutputBlockItemData;
begin
  P := inherited getItem(index);
  Result := TSQLInfoResultsItem.Create(self.Owner,P)
end;

function TSQLInfoResultsItem.Find(ItemType: byte): ISQLInfoResultsItem;
var P: POutputBlockItemData;
begin
  P := inherited Find(ItemType);
  Result := TSQLInfoResultsItem.Create(self.Owner,P)
end;

{ TSQLInfoResultsBuffer }

function TSQLInfoResultsBuffer.AddListItem(BufPtr: PChar): POutputBlockItemData;
var P: PChar;
    i: integer;
begin
  Result := inherited AddListItem(BufPtr);
  P := BufPtr + 1;
  i := 0;
  with Firebird25ClientAPI do
     Result^.FSize := isc_portable_integer(P,2) + 3;
  Inc(P,2);

  with Result^ do
  begin
    while P < FBufPtr + FSize do
    begin
      SetLength(FSubItems,i+1);
      case integer(P^) of
      isc_info_req_select_count,
      isc_info_req_insert_count,
      isc_info_req_update_count,
      isc_info_req_delete_count:
        FSubItems[i] := AddIntegerItem(P);

      else
        FSubItems[i] := AddSpecialItem(P);
      end;
      P +=  FSubItems[i]^.FSize;
      Inc(i);
    end;
  end;
end;

procedure TSQLInfoResultsBuffer.DoParseBuffer;
var P: PChar;
    index: integer;
    len: integer;
begin
  P := Buffer;
  index := 0;
  SetLength(FItems,0);
  while (P^ <> char(isc_info_end)) and (P < Buffer + getBufSize) do
  begin
    SetLength(FItems,index+1);
    case byte(P^) of
    isc_info_sql_stmt_type:
      FItems[index] := AddIntegerItem(P);

    isc_info_sql_get_plan:
      FItems[index] := AddStringItem(P);

    isc_info_sql_records:
      FItems[index] := AddListItem(P);

    else
      FItems[index] := AddSpecialItem(P);
    end;
    P += FItems[index]^.FSize;
    Inc(index);
  end;
end;

constructor TSQLInfoResultsBuffer.Create(aStatement: TFB25Statement;
  aSize: integer);
begin
  inherited Create(aSize);
  FStatement := aStatement;
  FIntegerType := dtInteger;
end;

procedure TSQLInfoResultsBuffer.Exec(info_request: byte);
begin
  with Firebird25ClientAPI do
  if isc_dsql_sql_info(StatusVector, @(FStatement.Handle), 1, @info_request,
                     GetBufSize, Buffer) > 0 then
    IBDatabaseError;
end;

function TSQLInfoResultsBuffer.GetItem(index: integer): ISQLInfoResultsItem;
var P: POutputBlockItemData;
begin
  P := inherited getItem(index);
  Result := TSQLInfoResultsItem.Create(self,P)
end;

function TSQLInfoResultsBuffer.Find(ItemType: byte): ISQLInfoResultsItem;
var P: POutputBlockItemData;
begin
  P := inherited Find(ItemType);
  Result := TSQLInfoResultsItem.Create(self,P)
end;

{ TIBXSQLVAR }

function TIBXSQLVAR.GetSQLType: cardinal;
begin
  result := FXSQLVAR^.sqltype and (not 1);
end;

function TIBXSQLVAR.GetSubtype: cardinal;
begin
  result := FXSQLVAR^.sqlsubtype;
end;

function TIBXSQLVAR.GetAliasName: string;
begin
  result := strpas(FXSQLVAR^.aliasname);
end;

function TIBXSQLVAR.GetFieldName: string;
begin
  result := strpas(FXSQLVAR^.sqlname);
end;

function TIBXSQLVAR.GetOwnerName: string;
begin
  result := strpas(FXSQLVAR^.ownname);
end;

function TIBXSQLVAR.GetRelationName: string;
begin
  result := strpas(FXSQLVAR^.relname);
end;

function TIBXSQLVAR.GetScale: cardinal;
begin
  result := FXSQLVAR^.sqlscale;
end;

function TIBXSQLVAR.GetCharSetID: cardinal;
begin
  result := 0;
  case SQLType of
  SQL_VARYING, SQL_TEXT:
    begin
      result := FXSQLVAR^.sqlsubtype and $FF;
      if (result <> 0) and FStatement.FAttachment.HasDefaultCharSet then
        result := FStatement.FAttachment.CharSetID;
    end;

  SQL_BLOB:
    if (SQLSubType = 1) and (GetRelationName <> '') and (GetFieldName <> '') then
      result := GetBlobMetaData.GetCharSetID;

  SQL_ARRAY:
    if (GetRelationName <> '') and (GetFieldName <> '') then
      result := GetArrayMetaData.GetCharSetID;
  end;
end;

{$IFDEF HAS_ANSISTRING_CODEPAGE}
function TIBXSQLVAR.GetCodePage: TSystemCodePage;
begin
  result := CP_NONE;
  with FirebirdClientAPI do
     CharSetID2CodePage(GetCharSetID,result);
end;
{$ENDIF}

function TIBXSQLVAR.GetIsNull: Boolean;
begin
  result := IsNullable and (FNullIndicator = -1);
end;

function TIBXSQLVAR.GetIsNullable: boolean;
begin
  result := (FXSQLVAR^.sqltype and 1 = 1);
end;

function TIBXSQLVAR.GetSQLData: PChar;
begin
  Result := FXSQLVAR^.sqldata;
end;

function TIBXSQLVAR.GetDataLength: cardinal;
begin
  Result := FXSQLVAR^.sqllen;
end;

function TIBXSQLVAR.GetArrayMetaData: IArrayMetaData;
begin
  if GetSQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if FArrayMetaData = nil then
    FArrayMetaData := TFB25ArrayMetaData.Create(FStatement.FAttachment,
                  FStatement.FTransaction,
                  GetRelationName,GetFieldName);
  Result := FArrayMetaData;
end;

function TIBXSQLVAR.GetBlobMetaData: IBlobMetaData;
begin
  if GetSQLType <> SQL_BLOB then
    IBError(ibxeInvalidDataConversion,[nil]);

  if FBlobMetaData = nil then
    FBlobMetaData := TFBBlobMetaData.Create(FStatement.FAttachment,
                FStatement.FTransaction,
                GetRelationName,GetFieldName);
  Result := FBlobMetaData;
end;

function TIBXSQLVAR.GetAsArray(Array_ID: TISC_QUAD): IArray;
begin
  if SQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if IsNull then
    Result := nil
  else
  begin
    if FArray = nil then
      FArray := TFB25Array.Create(FStatement.Attachment,
                                  TIBXSQLDA(Parent).GetTransaction,
                                  GetArrayMetaData,Array_ID);
    Result := FArray;
  end;
end;

function TIBXSQLVAR.GetAsBlob(Blob_ID: TISC_QUAD): IBlob;
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
      Result := TFBBlob.Create(FStatement.Attachment,
                               TIBXSQLDA(Parent).GetTransaction,
                               Blob_ID);
    FBlob := Result;
  end;
end;

function TIBXSQLVAR.CreateBlob: IBlob;
begin
  Result := TFBBlob.Create(FStatement.Attachment,FStatement.Transaction);
end;

procedure TIBXSQLVAR.Initialize;
begin
  inherited Initialize;
  FOwnsSQLData := true;
  with FirebirdClientAPI, FXSQLVar^ do
  begin
    case sqltype and (not 1) of
      SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP,
      SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT, SQL_BOOLEAN,
      SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: begin
        if (sqllen = 0) then
          { Make sure you get a valid pointer anyway
           select '' from foo }
          IBAlloc(sqldata, 0, 1)
        else
          IBAlloc(sqldata, 0, sqllen)
      end;
      SQL_VARYING: begin
        IBAlloc(sqldata, 0, sqllen + 2);
      end;
      else
        IBError(ibxeUnknownSQLDataType, [sqltype and (not 1)])
    end;
    if (sqltype and 1 = 1) then
    begin
      sqlInd := @FNullIndicator;
      FNullIndicator := 0;
    end
    else
      sqlInd :=  nil;
  end;
end;

procedure TIBXSQLVAR.SetIsNull(Value: Boolean);
begin
  if Value then
  begin
    if not IsNullable then
      IsNullable := True;

      FNullIndicator := -1;
    Changed;
  end
  else
    if ((not Value) and IsNullable) then
    begin
      FNullIndicator := 0;
      Changed;
    end;
end;

procedure TIBXSQLVAR.SetIsNullable(Value: Boolean);
begin
  if (Value <> IsNullable) then
  begin
    if Value then
    begin
      FXSQLVAR^.sqltype := FXSQLVAR^.sqltype or 1;
      FNullIndicator := 0;
      FXSQLVAR^.sqlInd := @FNullIndicator;
    end
    else
    begin
      FXSQLVAR^.sqltype := FXSQLVAR^.sqltype and (not 1);
      FXSQLVAR^.sqlind := nil;
    end;
  end;
end;

procedure TIBXSQLVAR.SetSQLData(AValue: PChar; len: cardinal);
begin
  if FOwnsSQLData then
    FreeMem(FXSQLVAR^.sqldata);
  FXSQLVAR^.sqldata := AValue;
  FXSQLVAR^.sqllen := len;
  FOwnsSQLData := false;
end;

procedure TIBXSQLVAR.SetScale(aValue: cardinal);
begin
  FXSQLVAR^.sqlscale := aValue;
end;

procedure TIBXSQLVAR.SetDataLength(len: cardinal);
begin
  if not FOwnsSQLData then
    FXSQLVAR^.sqldata := nil;
  FXSQLVAR^.sqllen := len;
  with FirebirdClientAPI do
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
  FOwnsSQLData := true;
end;

procedure TIBXSQLVAR.SetSQLType(aValue: cardinal);
begin
  FXSQLVAR^.sqltype := aValue or (FXSQLVAR^.sqltype and 1);
end;

procedure TIBXSQLVAR.SetCharSetID(aValue: cardinal);
begin
  if aValue <> GetCharSetID then
  case SQLType of
  SQL_VARYING, SQL_TEXT:
      FXSQLVAR^.sqlsubtype := (aValue and $FF) or (FXSQLVAR^.sqlsubtype and not $FF);

  SQL_BLOB,
  SQL_ARRAY:
    IBError(ibxeInvalidDataConversion,[nil]);
  end;
end;

constructor TIBXSQLVAR.Create(aParent: TIBXSQLDA; aIndex: integer);
begin
  inherited Create(aParent,aIndex);
  FStatement := aParent.Statement;
end;

procedure TIBXSQLVAR.FreeSQLData;
begin
  if FOwnsSQLData then
    FreeMem(FXSQLVAR^.sqldata);
  FXSQLVAR^.sqldata := nil;
  FOwnsSQLData := true;
end;

procedure TIBXSQLVAR.RowChange;
begin
  inherited RowChange;
  FBlob := nil;
  FArray := nil;
end;


{ TResultSet }

constructor TResultSet.Create(aResults: TIBXOUTPUTSQLDA);
begin
  inherited Create(aResults);
  FResults := aResults;
end;

destructor TResultSet.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TResultSet.FetchNext: boolean;
var i: integer;
begin
  CheckActive;
  Result := FResults.FStatement.FetchNext;
  if Result then
    for i := 0 to getCount - 1 do
      FResults.Column[i].RowChange;
end;

function TResultSet.GetCursorName: string;
begin
  Result := FResults.FStatement.FCursor;
end;

function TResultSet.GetTransaction: ITransaction;
begin
  Result := FResults.GetTransaction;
end;

procedure TResultSet.Close;
begin
  FResults.FStatement.Close;
end;

{ TIBXINPUTSQLDA }

procedure TIBXINPUTSQLDA.Bind;
begin
  if Count = 0 then
    Count := 1;
  with Firebird25ClientAPI do
  begin
    if (FXSQLDA <> nil) then
       if isc_dsql_describe_bind(StatusVector, @(FStatement.Handle), FStatement.SQLDialect,
                                    FXSQLDA) > 0 then
         IBDataBaseError;

    if FXSQLDA^.sqld > FXSQLDA^.sqln then
    begin
      Count := FXSQLDA^.sqld;
      if isc_dsql_describe_bind(StatusVector, @(FStatement.Handle), FStatement.SQLDialect,
                                   FXSQLDA) > 0 then
        IBDataBaseError;
    end
    else
    if FXSQLDA^.sqld = 0 then
      Count := 0;
  end;
  Initialize;
end;

function TIBXINPUTSQLDA.IsInputDataArea: boolean;
begin
  Result := true;
end;

{ TIBXOUTPUTSQLDA }

procedure TIBXOUTPUTSQLDA.Bind;
begin
  { Allocate an initial output descriptor (with one column) }
  Count := 1;
  with Firebird25ClientAPI do
  begin
    { Using isc_dsql_describe, get the right size for the columns... }
    if isc_dsql_describe(StatusVector, @(FStatement.Handle), FStatement.SQLDialect, FXSQLDA) > 0 then
      IBDataBaseError;

    if FXSQLDA^.sqld > FXSQLDA^.sqln then
    begin
      Count := FXSQLDA^.sqld;
      if isc_dsql_describe(StatusVector, @(FStatement.Handle), FStatement.SQLDialect, FXSQLDA) > 0 then
        IBDataBaseError;
    end
    else
    if FXSQLDA^.sqld = 0 then
      Count := 0;
  end;
  Initialize;
end;

function TIBXOUTPUTSQLDA.GetTransaction: TFB25Transaction;
begin
  Result := FTransaction;
end;

function TIBXOUTPUTSQLDA.IsInputDataArea: boolean;
begin
  Result := false;
end;

{ TIBXSQLDA }
constructor TIBXSQLDA.Create(aStatement: TFB25Statement);
begin
  inherited Create;
  FStatement := aStatement;
  FSize := 0;
//  writeln('Creating ',ClassName);
end;

destructor TIBXSQLDA.Destroy;
begin
  FreeXSQLDA;
//  writeln('Destroying ',ClassName);
  inherited Destroy;
end;

function TIBXSQLDA.CheckStatementStatus(Request: TStatementStatus): boolean;
begin
  Result := false;
  case Request of
  ssPrepared:
    Result := FStatement.IsPrepared;

  ssExecuteResults:
    Result :=FStatement.FSingleResults;

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

function TIBXSQLDA.GetRecordSize: Integer;
begin
  result := SizeOf(TIBXSQLDA) + XSQLDA_LENGTH(FSize);
end;

function TIBXSQLDA.GetXSQLDA: PXSQLDA;
begin
  result := FXSQLDA;
end;

function TIBXSQLDA.GetTransactionSeqNo: integer;
begin
  Result := FTransactionSeqNo;
end;

procedure TIBXSQLDA.Initialize;
begin
  if FXSQLDA <> nil then
    inherited Initialize;
end;

function TIBXSQLDA.GetTransaction: TFB25Transaction;
begin
  Result := FStatement.FTransaction;
end;

procedure TIBXSQLDA.SetCount(Value: Integer);
var
  i, OldSize: Integer;
  p : PXSQLVAR;
begin
  FCount := Value;
  if FCount = 0 then
    FUniqueRelationName := ''
  else
  begin
    if FSize > 0 then
      OldSize := XSQLDA_LENGTH(FSize)
    else
      OldSize := 0;
    if Count > FSize then
    begin
      Firebird25ClientAPI.IBAlloc(FXSQLDA, OldSize, XSQLDA_LENGTH(Count));
      SetLength(FColumnList, FCount);
      FXSQLDA^.version := SQLDA_VERSION1;
      p := @FXSQLDA^.sqlvar[0];
      for i := 0 to Count - 1 do
      begin
        if i >= FSize then
          FColumnList[i] := TIBXSQLVAR.Create(self,i);
        TIBXSQLVAR(Column[i]).FXSQLVAR := p;
        p := Pointer(PChar(p) + sizeof(FXSQLDA^.sqlvar));
      end;
      FSize := inherited Count;
    end;
    if FSize > 0 then
    begin
      FXSQLDA^.sqln := Value;
      FXSQLDA^.sqld := Value;
    end;
  end;
end;

procedure TIBXSQLDA.FreeXSQLDA;
var i: integer;
begin
  if FXSQLDA <> nil then
  begin
//    writeln('SQLDA Cleanup');
    for i := 0 to Count - 1 do
      TIBXSQLVAR(Column[i]).FreeSQLData;
    FreeMem(FXSQLDA);
    FXSQLDA := nil;
  end;
  for i := 0 to Count - 1  do
    TIBXSQLVAR(Column[i]).Free;
  SetLength(FColumnList,0);
  FSize := 0;
end;

function TIBXSQLDA.GetStatement: IStatement;
begin
  Result := FStatement;
end;

function TIBXSQLDA.GetPrepareSeqNo: integer;
begin
  Result := FStatement.FPrepareSeqNo;
end;

{ TFB25Statement }

procedure TFB25Statement.CheckTransaction(aTransaction: TFB25Transaction);
begin
  if (aTransaction = nil) then
    IBError(ibxeTransactionNotAssigned,[]);

  if not aTransaction.InTransaction then
    IBError(ibxeNotInTransaction,[]);

end;

procedure TFB25Statement.CheckHandle;
begin
  if FHandle = nil then
    IBError(ibxeInvalidStatementHandle,[nil]);
end;

procedure TFB25Statement.InternalPrepare;
var
  RB: ISQLInfoResultsBuffer;
begin
  if FPrepared then
    Exit;
  if (FSQL = '') then
    IBError(ibxeEmptyQuery, [nil]);
  try
    CheckTransaction(FTransaction);
    with Firebird25ClientAPI do
    begin
      Call(isc_dsql_alloc_statement2(StatusVector, @(FAttachment.Handle),
                                      @FHandle), True);
      if FHasParamNames then
      begin
        if FProcessedSQL = '' then
          FSQLParams.PreprocessSQL(FSQL,FGenerateParamNames,FUniqueParamNames,FProcessedSQL);
        Call(isc_dsql_prepare(StatusVector, @(FTransaction.Handle), @FHandle, 0,
                 PChar(FProcessedSQL), FSQLDialect, nil), True);
      end
      else
        Call(isc_dsql_prepare(StatusVector, @(FTransaction.Handle), @FHandle, 0,
                 PChar(FSQL), FSQLDialect, nil), True);
    end;
    { After preparing the statement, query the stmt type and possibly
      create a FSQLRecord "holder" }
    { Get the type of the statement }
    RB := TSQLInfoResultsBuffer.Create(self);
    RB.Exec(isc_info_sql_stmt_type);
    if RB.Count > 0 then
      FSQLStatementType := TIBSQLStatementTypes(RB[0].GetAsInteger)
    else
      FSQLStatementType := SQLUnknown;

    { Done getting the type }
    case FSQLStatementType of
      SQLGetSegment,
      SQLPutSegment,
      SQLStartTransaction: begin
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
        FSQLParams.Bind;

        {setup output sqlda}
        if FSQLStatementType in [SQLSelect, SQLSelectForUpdate,
                        SQLExecProcedure] then
          FSQLRecord.Bind;
      end;
    end;
  except
    on E: Exception do begin
      if (FHandle <> nil) then
        FreeHandle;
      if E is EIBInterBaseError then
        raise EIBInterBaseError.Create(EIBInterBaseError(E).SQLCode,
                                       EIBInterBaseError(E).IBErrorCode,
                                       EIBInterBaseError(E).Message +
                                       sSQLErrorSeparator + FSQL)
      else
        raise;
    end;
  end;
  FPrepared := true;
  FSingleResults := false;
  Inc(FPrepareSeqNo);
  FSQLParams.FTransactionSeqNo := FTransaction.TransactionSeqNo;
  FSQLRecord.FTransactionSeqNo := FTransaction.TransactionSeqNo;
end;

function TFB25Statement.InternalExecute(aTransaction: TFB25Transaction): IResults;
begin
  Result := nil;
  FBOF := false;
  FEOF := false;
  FSingleResults := false;
  CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  if aTransaction <> FTransaction then
    AddMonitor(aTransaction);

  try
    with Firebird25ClientAPI do
    case FSQLStatementType of
    SQLSelect:
      IBError(ibxeIsAExecuteProcedure,[]);

    SQLExecProcedure:
    begin
      Call(isc_dsql_execute2(StatusVector,
                          @(aTransaction.Handle),
                          @FHandle,
                          SQLDialect,
                          FSQLParams.AsXSQLDA,
                          FSQLRecord.AsXSQLDA), True);
      Result := TResults.Create(FSQLRecord);
      FSingleResults := true;
    end
    else
      Call(isc_dsql_execute(StatusVector,
                           @(aTransaction.Handle),
                           @FHandle,
                           SQLDialect,
                           FSQLParams.AsXSQLDA), True);

    end;
  finally
    if aTransaction <> FTransaction then
       RemoveMonitor(aTransaction);
  end;
end;

function TFB25Statement.InternalOpenCursor(aTransaction: TFB25Transaction
  ): IResultSet;
begin
  if FSQLStatementType <> SQLSelect then
   IBError(ibxeIsASelectStatement,[]);

 CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
 if aTransaction <> FTransaction then
   AddMonitor(aTransaction);
 CheckHandle;
 with Firebird25ClientAPI do
 begin
   Call(isc_dsql_execute2(StatusVector,
                       @(aTransaction.Handle),
                       @FHandle,
                       SQLDialect,
                       FSQLParams.AsXSQLDA,
                       nil), True);
   Call(
     isc_dsql_set_cursor_name(StatusVector, @FHandle, PChar(FCursor), 0),
     True);
 end;
 FSingleResults := false;
 FOpen := True;
 FExecTransactionIntf := aTransaction;
 FBOF := true;
 FEOF := false;
 FSQLRecord.FTransaction := aTransaction;
 FSQLRecord.FTransactionSeqNo := aTransaction.TransactionSeqNo;
 Result := TResultSet.Create(FSQLRecord);
end;

procedure TFB25Statement.FreeHandle;
var
  isc_res: ISC_STATUS;
begin
  Close;
  try
    if FHandle <> nil then
    with Firebird25ClientAPI do
    begin
      isc_res :=
        Call(isc_dsql_free_statement(StatusVector, @FHandle, DSQL_drop), False);
      if (StatusVector^ = 1) and (isc_res > 0) and (isc_res <> isc_bad_stmt_handle) then
        IBDataBaseError;
    end;
  finally
    FHandle := nil;
  end;
end;

procedure TFB25Statement.InternalClose(Force: boolean);
var
  isc_res: ISC_STATUS;
begin
  try
    if (FHandle <> nil) and (SQLStatementType = SQLSelect) and FOpen then
    with Firebird25ClientAPI do
    begin
      isc_res := Call(
                   isc_dsql_free_statement(StatusVector, @FHandle, DSQL_close),
                   False);
      if not Force and (StatusVector^ = 1) and (isc_res > 0) and
        not getStatus.CheckStatusVector(
              [isc_bad_stmt_handle, isc_dsql_cursor_close_err]) then
        IBDatabaseError;
    end;
  finally
    if (FSQLRecord.FTransaction <> nil) and (FSQLRecord.FTransaction <> FTransaction) then
      RemoveMonitor(FSQLRecord.FTransaction);
    FOpen := False;
    FExecTransactionIntf := nil;
    FSQLRecord.FTransaction := nil;
  end;
end;

constructor TFB25Statement.Create(Attachment: TFB25Attachment;
  Transaction: ITransaction; sql: string; SQLDialect: integer);
var GUID : TGUID;
begin
  inherited Create(Transaction as TFB25Transaction);
  FAttachment := Attachment;
  FAttachmentIntf := Attachment;
  FTransaction := transaction as TFB25Transaction;
  FTransactionIntf := Transaction;
  AddMonitor(FTransaction);
  FSQLDialect := SQLDialect;
  CreateGuid(GUID);
  FCursor := GUIDToString(GUID);
  FSQLParams := TIBXINPUTSQLDA.Create(self);
  FSQLRecord := TIBXOUTPUTSQLDA.Create(self);
  FSQL := sql;
  InternalPrepare;
end;

constructor TFB25Statement.CreateWithParameterNames(Attachment: TFB25Attachment;
  Transaction: ITransaction; sql: string; SQLDialect: integer;
  GenerateParamNames: boolean; UniqueParamNames: boolean);
begin
  FHasParamNames := true;
  FGenerateParamNames := GenerateParamNames;
  FUniqueParamNames := UniqueParamNames;
  Create(Attachment,Transaction,sql,SQLDialect);
end;

destructor TFB25Statement.Destroy;
begin
  Close;
  FreeHandle;
  if assigned(FSQLParams) then FSQLParams.Free;
  if assigned(FSQLRecord) then FSQLRecord.Free;
  inherited Destroy;
end;

procedure TFB25Statement.Close;
begin
   InternalClose(false);
end;

function TFB25Statement.FetchNext: boolean;
var
  fetch_res: ISC_STATUS;
begin
  result := false;
  if not FOpen then
    IBError(ibxeSQLClosed, [nil]);
  if FEOF then
    IBError(ibxeEOF,[nil]);

  with Firebird25ClientAPI do
  begin
    { Go to the next record... }
    fetch_res :=
      Call(isc_dsql_fetch(StatusVector, @FHandle, SQLDialect, FSQLRecord.AsXSQLDA), False);
    if (fetch_res = 100) or (getStatus.CheckStatusVector([isc_dsql_cursor_err])) then
    begin
      FEOF := true;
      Exit; {End of File}
    end
    else
    if (fetch_res > 0) then
    begin
      try
        IBDataBaseError;
      except
        Close;
        raise;
      end;
    end
    else
    begin
      FBOF := false;
      result := true;
    end;
  end;
end;

procedure TFB25Statement.TransactionEnding(aTransaction: TFB25Transaction;
  Force: boolean);
begin
  if FOpen and (FSQLRecord.FTransaction = aTransaction) then
    InternalClose(Force);

  if FTransaction = aTransaction then
  begin
    FreeHandle;
    FPrepared := false;
  end;
end;

function TFB25Statement.GetSQLParams: ISQLParams;
begin
  CheckHandle;
  Result := TSQLParams.Create(FSQLParams);
end;

function TFB25Statement.GetMetaData: IMetaData;
begin
  CheckHandle;
  Result := TMetaData.Create(FSQLRecord);
end;

function TFB25Statement.GetPlan: String;
var
    RB: TSQLInfoResultsBuffer;
begin
  if (not (FSQLStatementType in [SQLSelect, SQLSelectForUpdate,
       {TODO: SQLExecProcedure, }
       SQLUpdate, SQLDelete])) then
    result := ''
  else
  begin
    RB := TSQLInfoResultsBuffer.Create(self,4*4096);
    RB.Exec(isc_info_sql_get_plan);
     if RB.Count > 0 then
     Result := RB[0].GetAsString;
  end;
end;

function TFB25Statement.GetRowsAffected(var SelectCount, InsertCount, UpdateCount,
  DeleteCount: integer): boolean;
var
  RB: TSQLInfoResultsBuffer;
  i, j: integer;
begin
  InsertCount := 0;
  UpdateCount := 0;
  DeleteCount := 0;
  Result := FHandle <> nil;
  if not Result then Exit;

  RB := TSQLInfoResultsBuffer.Create(self);
  RB.Exec(isc_info_sql_records);

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

function TFB25Statement.GetSQLStatementType: TIBSQLStatementTypes;
begin
  Result := FSQLStatementType;
end;

function TFB25Statement.Execute(aTransaction: ITransaction): IResults;
begin
  if aTransaction = nil then
    Result :=  InternalExecute(FTransaction)
  else
    Result := InternalExecute(aTransaction as TFB25Transaction);
end;

function TFB25Statement.OpenCursor(aTransaction: ITransaction): IResultSet;
begin
  if aTransaction = nil then
    Result := InternalOpenCursor(FTransaction)
  else
    Result := InternalOpenCursor(aTransaction as TFB25Transaction);
end;

function TFB25Statement.CreateBlob: IBlob;
begin
  Result := TFBBlob.Create(FAttachment,FTransaction);
end;

function TFB25Statement.CreateArray(column: IColumnMetaData): IArray;
begin
  if column.SQLType <> SQL_ARRAY then
    IBError(ibxeNotAnArray,[nil]);
  Result := TFB25Array.Create(FAttachment,FTransaction,column.GetArrayMetaData);
end;

function TFB25Statement.CreateArray(columnName: string): IArray;
var col: IColumnMetaData;
begin
  col := GetMetaData.ByName(columnName);
  Result := CreateArray(col);
end;

function TFB25Statement.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TFB25Statement.GetTransaction: ITransaction;
begin
  Result := FTransaction;
end;

function TFB25Statement.GetSQLText: string;
begin
  Result := FSQL;
end;

function TFB25Statement.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

function TFB25Statement.IsPrepared: boolean;
begin
  Result := FHandle <> nil;
end;

procedure TFB25Statement.Prepare(aTransaction: ITransaction);
begin
  if FPrepared then FreeHandle;
  if aTransaction <> nil then
  begin
    RemoveMonitor(FTransaction);
    FTransaction := transaction as TFB25Transaction;
    FTransactionIntf := Transaction;
    AddMonitor(FTransaction);
  end;
  InternalPrepare;
end;

end.

