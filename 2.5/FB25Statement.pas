unit FB25Statement;

{$mode objfpc}{$H+}

{This unit is hacked from IBSQL and contains the code for managing an XSQLDA and
 SQLVars, along with statement preparation, execution and cursor management.
 Most of the SQLVar code has been moved to unit FB25SQLData. Client access is
 provided through interface rather than direct access to the XSQLDA and XSQLVar
 objects.}

{ $define ALLOWDIALECT3PARAMNAMES}

{$ifndef ALLOWDIALECT3PARAMNAMES}

{ Note on SQL Dialects and SQL Parameter Names
  --------------------------------------------

  Even when dialect 3 quoted format parameter names are not supported, IBX still processes
  parameter names case insensitive. This does result in some additional overhead
  due to a call to "AnsiUpperCase". This can be avoided by undefining
  "UseCaseInSensitiveParamName" below.

  Note: do not define "UseCaseSensitiveParamName" when "ALLOWDIALECT3PARAMNAMES"
  is defined. This will not give a useful result.
}
{$define UseCaseInSensitiveParamName}
{$endif}
{
  Note on reference counted interfaces.
  ------------------------------------

  TFBStatement manages both an input and an output SQLDA through the TIBXINPUTSQLDA
  and TIBXOUTPUTSQLDA objects. As pure objects, these are explicitly destroyed
  when the statement is destroyed.

  However, IResultSet is an interface and is returned when a cursor is opened and
  has a reference for the TIBXOUTPUTSQLDA. The   user may discard their reference
  to the IStatement while still using the   IResultSet. This would be a problem if t
  he underlying TFBStatement object and its TIBXOUTPUTSQLDA is destroyed while
  still leaving the TIBXResultSet object in place. Calls to (e.g.)   FetchNext would fail.

  To avoid this problem, TIBXResultSet objects  have a reference to the IStatement
  interface of the TFBStatement object. Thus, as long as these "copies" exist,
  the owning statement is not destroyed even if the user discards their reference
  to the statement. Note: the TFBStatement does not have a reference to the TIBXResultSet
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
  Classes, SysUtils, IB, FBTypes, FBLibrary, FB25ClientAPI, FB25Transaction, FB25Attachment,
  IBHeader, IBExternals, FB25SQLData, FB25OutputBlock;

type
  TFBStatement = class;
  TIBXSQLDA = class;

  { TIBXSQLVAR }

  TIBXSQLVAR = class
  public
    FIndex: Integer;
    FName: String;
    FParent: TIBXSQLDA;
    FUniqueName: boolean;
    FModified: boolean;
    FBlob: IBlob;             {Cache references}
    FArray: IArray;
    FXSQLVAR: PXSQLVAR;       { Point to the PXSQLVAR in the owner object }
    constructor Create(aParent: TIBXSQLDA);
    procedure RowChange;
  end;

  { TColumnMetaData }

  TColumnMetaData = class(TSQLDataItem,IColumnMetaData)
  private
    FIBXSQLVAR: TIBXSQLVAR;
    FStatement: IStatement;         {Keep reference to ensure statement not discarded}
    FPrepareSeqNo: integer;
    function GetParent: TIBXSQLDA;
  protected
    procedure CheckActive; override;
    function SQLData: PChar; override;
    function GetDataLength: short; override;

  public
    constructor Create(aIBXSQLVAR: TIBXSQLVAR);
    function GetAttachment: TFBAttachment; override;
    function GetTransaction: TFBTransaction; override;
    function GetSQLDialect: integer; override;
    property Parent: TIBXSQLDA read GetParent;

  public
    {IColumnMetaData}
    function GetIndex: integer;
    function GetSQLType: short; override;
    function getSubtype: short;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if not alias}
    function GetName: string; override;      {Disambiguated uppercase Field Name}
    function GetScale: short; override;
    function getCharSetID: cardinal;
    function GetIsNull: Boolean; override;
    function GetIsNullable: boolean; override;
    function GetSize: integer;
    function GetArrayMetaData: IArrayMetaData;
    function GetBlobMetaData: IBlobMetaData;
    property Name: string read GetName;
    property Size: Integer read GetSize;
    property CharSetID: cardinal read getCharSetID;
    property SQLSubtype: short read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  end;

  { TIBSQLData }

  TIBSQLData = class(TColumnMetaData,ISQLData)
  protected
    procedure CheckActive; override;
  public
    function GetAsArray: IArray;
    function GetAsBlob: IBlob; override;
  end;

  TIBXINPUTSQLDA = class;

  { TIBXSQLParam }

  { TSQLParam }

  TSQLParam = class(TIBSQLData,ISQLParam)
  private
    procedure InternalSetIsNull(Value: Boolean);
    procedure InternalSetIsNullable(Value: Boolean);
  protected
    procedure CheckActive; override;
    procedure Changed; override;
    procedure SetScale(aValue: short); override;
    procedure SetDataLength(len: short); override;
    procedure SetSQLType(aValue: short); override;
  public
    constructor Create(aIBXSQLVAR: TIBXSQLVAR);
    procedure Clear;
    function GetModified: boolean; override;
    procedure SetName(Value: string); override;
    procedure SetIsNull(Value: Boolean);  override;
    procedure SetIsNullable(Value: Boolean);  override;
    procedure SetAsArray(anArray: IArray);

    {overrides}
    procedure SetAsBoolean(AValue: boolean);
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsInt64(Value: Int64);
    procedure SetAsDate(Value: TDateTime);
    procedure SetAsLong(Value: Long);
    procedure SetAsTime(Value: TDateTime);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsDouble(Value: Double);
    procedure SetAsFloat(Value: Float);
    procedure SetAsPointer(Value: Pointer);
    procedure SetAsShort(Value: Short);
    procedure SetAsString(Value: String);  override;
    procedure SetAsVariant(Value: Variant);
    procedure SetAsBlob(Value: IBlob);
    procedure SetAsQuad(Value: TISC_QUAD);

    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
  end;

  TIBXSQLDAType = (daInput,daOutput);

  { TIBXSQLDA }

  TIBXSQLDA = class
  private
    FCount: Integer;
    FSize: Integer;
    FInputSQLDA: boolean;
    FXSQLDA: PXSQLDA;
    FXSQLVars: array of TIBXSQLVAR;
    FUniqueRelationName: String;
    function GetRecordSize: Integer;
    function GetXSQLDA: PXSQLDA;
    function GetXSQLVAR(Idx: Integer): TIBXSQLVAR;
    procedure SetCount(Value: Integer);
  protected
    FStatement: TFBStatement;
    function GetAttachment: TFBAttachment;
    function GetSQLDialect: integer;
    function GetTransaction: TFBTransaction; virtual;
    procedure FreeXSQLDA;
    function GetXSQLVARByName(Idx: String): TIBXSQLVAR;
  public
    constructor Create(aStatement: TFBStatement; sqldaType: TIBXSQLDAType);
    destructor Destroy; override;
    function VarByName(Idx: String): TIBXSQLVAR;
    function GetUniqueRelationName: string;
    procedure Initialize;
    property Vars[Idx: Integer]: TIBXSQLVAR read GetXSQLVAR; default;
    property AsXSQLDA: PXSQLDA read GetXSQLDA;
    property Count: Integer read FCount write SetCount;
    property RecordSize: Integer read GetRecordSize;
    property Statement: TFBStatement read FStatement;
  end;

  { TIBXINPUTSQLDA }

  TIBXINPUTSQLDA = class(TIBXSQLDA)
  private
    function GetModified: Boolean;
  public
    constructor Create(aOwner: TFBStatement);
    procedure Bind;
    procedure SetParamName(FieldName: String; Idx: Integer; UniqueName: boolean );
  end;

 { TSQLParams }

 TSQLParams = class(TInterfacedObject,ISQLParams)
 private
   FPrepareSeqNo: integer;
   FSQLParams: TIBXINPUTSQLDA;
   FStatement: IStatement; {ensure FStatement not destroyed until no longer needed}
   procedure CheckActive;
 public
   constructor Create(aSQLParams: TIBXINPUTSQLDA);
 public
   {ISQLParams}
   function getCount: integer;
   function getSQLParam(index: integer): ISQLParam;
   function ByName(Idx: String): ISQLParam ;
   function GetModified: Boolean;
 end;


  { TIBXOUTPUTSQLDA }

  TIBXOUTPUTSQLDA = class(TIBXSQLDA)
  private
     FTransaction: TFBTransaction; {transaction used to execute the statement}
  public
    constructor Create(aOwner: TFBStatement);
    procedure Bind;
  end;

  { TMetaData }

  TMetaData = class(TInterfacedObject,IMetaData)
  private
    FPrepareSeqNo: integer;
    FMetaData: TIBXOUTPUTSQLDA;
    FStatement: IStatement; {ensure FStatement not destroyed until no longer needed}
    procedure CheckActive;
  public
    constructor Create(aMetaData: TIBXOUTPUTSQLDA);
  public
    {IMetaData}
    function GetUniqueRelationName: string;
    function getCount: integer;
    function getColumnMetaData(index: integer): IColumnMetaData;
    function ByName(Idx: String): IColumnMetaData;
  end;

 { TResults }

  TResults = class(TInterfacedObject,IResults)
  private
    FPrepareSeqNo: integer;
    FResults: TIBXOUTPUTSQLDA;
    FSQLDataCache: array of ISQLData;
    FStatement: IStatement; {ensure FStatement not destroyed until no longer needed}
    procedure CheckActive;
    function GetISQLData(aIBXSQLVAR: TIBXSQLVAR): ISQLData;
   public
    constructor Create(aResults: TIBXOUTPUTSQLDA);
     {IResults}
    function getCount: integer;
    function ByName(Idx: String): ISQLData;
    function getSQLData(index: integer): ISQLData;
end;

  { TResultSet }

  TResultSet = class(TResults,IResultSet)
  public
    destructor Destroy; override;
    {IResultSet}
    function FetchNext: boolean;
    function GetCursorName: string;
    function GetTransaction: TFBTransaction;
    procedure Close;
  end;

  { TFBStatement }

  TFBStatement = class(TAPIObject,IStatement)
  private
    FAttachment: TFBAttachment;
    FAttachmentIntf: IAttachment;
    FTransaction: TFBTransaction;
    FTransactionIntf: ITransaction;
    FExecTransactionIntf: ITransaction;
    FHandle: TISC_STMT_HANDLE;
    FSQLType: TIBSQLTypes;         { Select, update, delete, insert, create, alter, etc...}
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
    procedure CheckTransaction(aTransaction: TFBTransaction);
    procedure CheckHandle;
    procedure InternalPrepare;
    function InternalExecute(aTransaction: TFBTransaction): IResults;
    function InternalOpenCursor(aTransaction: TFBTransaction): IResultSet;
    procedure FreeHandle;
    procedure PreprocessSQL;
    procedure InternalClose(Force: boolean);
  public
    constructor Create(Attachment: TFBAttachment; Transaction: ITransaction;
      sql: string; SQLDialect: integer);
    constructor CreateWithParameterNames(Attachment: TFBAttachment; Transaction: ITransaction;
      sql: string;  SQLDialect: integer; GenerateParamNames: boolean =false; UniqueParamNames: boolean=false);
    destructor Destroy; override;
    procedure Close;
    function FetchNext: boolean;
    procedure TransactionEnding(aTransaction: TFBTransaction; Force: boolean);
    property SQLDialect: integer read FSQLDialect;
    property Attachment: TFBAttachment read FAttachment;
    property Transaction: TFBTransaction read FTransaction;

  public
    {IStatement}
    function GetSQLParams: ISQLParams;
    function GetMetaData: IMetaData;
    function GetPlan: String;
    function GetRowsAffected(var SelectCount, InsertCount, UpdateCount,
      DeleteCount: integer): boolean;
    function GetSQLType: TIBSQLTypes;
    function GetSQLText: string;
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
    property SQLType: TIBSQLTypes read GetSQLType;

end;

implementation

uses IBUtils, FBErrorMessages, FB25Blob, variants, IBErrorCodes, FB25Array;

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
    FStatement: TFBStatement;
  protected
    function AddListItem(BufPtr: PChar): POutputBlockItemData; override;
    procedure DoParseBuffer; override;
  public
    constructor Create(aStatement: TFBStatement; aSize: integer = 1024);
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

constructor TSQLInfoResultsBuffer.Create(aStatement: TFBStatement;
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

constructor TIBXSQLVAR.Create(aParent: TIBXSQLDA);
begin
  inherited Create;
  FParent := aParent;
end;

procedure TIBXSQLVAR.RowChange;
begin
  FBlob := nil;
  FArray := nil;
  FModified := false;
end;

{ TResultSet }

destructor TResultSet.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TResultSet.FetchNext: boolean;
var i: integer;
begin
  Result := FResults.FStatement.FetchNext;
  if Result then
    for i := 0 to getCount - 1 do
      FResults.vars[i].RowChange;
end;

function TResultSet.GetCursorName: string;
begin
  Result := FResults.FStatement.FCursor;
end;

function TResultSet.GetTransaction: TFBTransaction;
begin
  Result := FResults.FTransaction;
end;

procedure TResultSet.Close;
begin
  FResults.FStatement.Close;
end;

{ TResults }

procedure TResults.CheckActive;
begin
  if FPrepareSeqNo < FResults.FStatement.FPrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FResults.FStatement.FPrepared  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

function TResults.GetISQLData(aIBXSQLVAR: TIBXSQLVAR): ISQLData;
begin
  if FSQLDataCache[aIBXSQLVAR.FIndex] <> nil then
    Result := FSQLDataCache[aIBXSQLVAR.FIndex]
  else
  begin
    Result := TIBSQLData.Create(aIBXSQLVAR);
    FSQLDataCache[aIBXSQLVAR.FIndex] := Result;
  end;
end;

constructor TResults.Create(aResults: TIBXOUTPUTSQLDA);
begin
  inherited Create;
  FResults := aResults;
  FStatement := aResults.FStatement;
  FPrepareSeqNo := aResults.FStatement.FPrepareSeqNo;
  SetLength(FSQLDataCache,aResults.Count);
end;

function TResults.getCount: integer;
begin
  CheckActive;
  Result := FResults.Count;
end;

function TResults.ByName(Idx: String): ISQLData;
begin
  CheckActive;
{  if FResults.FStatement.FBOF then
    IBError(ibxeBOF,[nil]);}
  if FResults.FStatement.FEOF then
    IBError(ibxeEOF,[nil]);

  if FResults.Count = 0 then
    Result := nil
  else
    Result := GetISQLData(FResults.VarByName(Idx));
end;

function TResults.getSQLData(index: integer): ISQLData;
begin
  CheckActive;
{  if FResults.FStatement.FBOF then
    IBError(ibxeBOF,[nil]);}
  if FResults.FStatement.FEOF then
    IBError(ibxeEOF,[nil]);
  Result := GetISQLData(FResults.Vars[index]);
end;

{ TMetaData }

procedure TMetaData.CheckActive;
begin
  if FPrepareSeqNo < FMetaData.FStatement.FPrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FMetaData.FStatement.FPrepared  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

constructor TMetaData.Create(aMetaData: TIBXOUTPUTSQLDA);
begin
  inherited Create;
  FMetaData := aMetaData;
  FStatement := aMetaData.FStatement;
  FPrepareSeqNo := aMetaData.FStatement.FPrepareSeqNo;
end;

function TMetaData.GetUniqueRelationName: string;
begin
  CheckActive;
  Result := FMetaData.GetUniqueRelationName;
end;

function TMetaData.getCount: integer;
begin
  CheckActive;
  Result := FMetaData.Count;
end;

function TMetaData.getColumnMetaData(index: integer): IColumnMetaData;
begin
  CheckActive;
  if FMetaData.Count = 0 then
    Result := nil
  else
    Result := TColumnMetaData.Create(FMetaData.Vars[index]);
end;

function TMetaData.ByName(Idx: String): IColumnMetaData;
begin
  CheckActive;
  if FMetaData.Count = 0 then
    Result := nil
  else
    Result := TColumnMetaData.Create(FMetaData.VarByName(Idx));
end;

{ TSQLParams }

procedure TSQLParams.CheckActive;
begin
  if FPrepareSeqNo < FSQLParams.FStatement.FPrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FSQLParams.FStatement.FPrepared  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

constructor TSQLParams.Create(aSQLParams: TIBXINPUTSQLDA);
begin
  inherited Create;
  FSQLParams := aSQLParams;
  FStatement := aSQLParams.FStatement;
  FPrepareSeqNo := aSQLParams.FStatement.FPrepareSeqNo;
end;

function TSQLParams.getCount: integer;
begin
  CheckActive;
  Result := FSQLParams.Count;
end;

function TSQLParams.getSQLParam(index: integer): ISQLParam;
begin
  CheckActive;
  Result := TSQLParam.Create(FSQLParams.Vars[index]);
end;

function TSQLParams.ByName(Idx: String): ISQLParam;
begin
  CheckActive;
  Result := TSQLParam.Create(FSQLParams.VarByName(Idx));
end;

function TSQLParams.GetModified: Boolean;
var
  i: Integer;
begin
  CheckActive;
  result := False;
  with FSQLParams do
  for i := 0 to FCount - 1 do
    if FXSQLVARs[i].FModified then
    begin
      result := True;
      exit;
    end;
end;

{ TIBSQLData }

procedure TIBSQLData.CheckActive;
begin
  inherited CheckActive;

  if not Parent.FStatement.FOpen and not Parent.FStatement.FSingleResults then
    IBError(ibxeSQLClosed, [nil]);

  if Parent.FStatement.FEOF then
    IBError(ibxeEOF,[nil]);

{  if Parent.FStatement.FBOF then
    IBError(ibxeBOF,[nil]);  }
end;

function TIBSQLData.GetAsArray: IArray;
begin
  CheckActive;
  if GetSQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if IsNull then
    Result := nil
  else
  begin
    if FIBXSQLVAR.FArray = nil then
      FIBXSQLVAR.FArray := TFBArray.Create(self);
    Result := FIBXSQLVAR.FArray;
  end;
end;

function TIBSQLData.GetAsBlob: IBlob;
begin
  CheckActive;
  if FIBXSQLVAR.FBlob <> nil then
    Result := FIBXSQLVAR.FBlob
  else
  begin
    Result := inherited GetAsBlob;
    FIBXSQLVAR.FBlob := Result;
  end;
end;

{ TSQLParam }

procedure TSQLParam.InternalSetIsNull(Value: Boolean);
begin
  with FIBXSQLVAR do
  if Value then
  begin
    if not IsNullable then
      IsNullable := True;

    if Assigned(FXSQLVAR^.sqlind) then
      FXSQLVAR^.sqlind^ := -1;
    Changed;
  end
  else
    if ((not Value) and IsNullable) then
    begin
      if Assigned(FXSQLVAR^.sqlind) then
        FXSQLVAR^.sqlind^ := 0;
      Changed;
    end;
end;

procedure TSQLParam.InternalSetIsNullable(Value: Boolean);
begin
  with FIBXSQLVAR do
  if (Value <> IsNullable) then
  begin
    if Value then
    begin
      FXSQLVAR^.sqltype := FXSQLVAR^.sqltype or 1;
      with Firebird25ClientAPI do
        IBAlloc(FXSQLVAR^.sqlind, 0, SizeOf(Short));
    end
    else
    begin
      FXSQLVAR^.sqltype := FXSQLVAR^.sqltype and (not 1);
      ReallocMem(FXSQLVAR^.sqlind, 0);
    end;
  end;
end;

procedure TSQLParam.CheckActive;
begin
  if FPrepareSeqNo < (FStatement as TFBStatement).FPrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not Parent.FStatement.FPrepared  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

procedure TSQLParam.SetScale(aValue: short);
begin
  CheckActive;
  with FIBXSQLVAR do
    FXSQLVAR^.sqlscale := aValue;
end;

procedure TSQLParam.SetDataLength(len: short);
begin
  CheckActive;
  with FIBXSQLVAR do
  begin
    FXSQLVAR^.sqllen := len;
    with Firebird25ClientAPI do
      IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen+1);
  end;
end;

procedure TSQLParam.SetSQLType(aValue: short);
begin
  CheckActive;
  with FIBXSQLVAR do
    FXSQLVAR^.sqltype := aValue or (FXSQLVAR^.sqltype and 1);
end;

constructor TSQLParam.Create(aIBXSQLVAR: TIBXSQLVAR);
begin
  inherited Create(aIBXSQLVAR);
  FIBXSQLVAR.FUniqueName := true;
end;

procedure TSQLParam.Clear;
begin
  IsNull := true;
end;

function TSQLParam.GetModified: boolean;
begin
  CheckActive;
  Result := FIBXSQLVAR.FModified;
end;

procedure TSQLParam.SetName(Value: string);
begin
  CheckActive;
  FIBXSQLVAR.FName := Value;
end;

procedure TSQLParam.SetIsNull(Value: Boolean);
var i: integer;
begin
  CheckActive;
  with FIBXSQLVAR do
  if FUniqueName then
    InternalSetIsNull(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        InternalSetIsNull(Value);
  end
end;

procedure TSQLParam.SetIsNullable(Value: Boolean);
var i: integer;
begin
  CheckActive;
  with FIBXSQLVAR do
  if FUniqueName then
    InternalSetIsNullable(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        InternalSetIsNullable(Value);
  end
end;

procedure TSQLParam.SetAsArray(anArray: IArray);
begin
  CheckActive;
  if GetSQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if not FIBXSQLVAR.FUniqueName then
    IBError(ibxeDuplicateParamName,[FIBXSQLVAR.FName]);

  SetAsQuad((AnArray as TFBArray).GetArrayID);
end;

procedure TSQLParam.Changed;
begin
  FIBXSQLVAR.FModified := true;
end;

procedure TSQLParam.SetAsBoolean(AValue: boolean);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsBoolean(AValue)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsBoolean(AValue);
  end;
end;

procedure TSQLParam.SetAsCurrency(Value: Currency);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsCurrency(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsCurrency(Value);
  end;
end;

procedure TSQLParam.SetAsInt64(Value: Int64);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsInt64(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsInt64(Value);
  end;
end;

procedure TSQLParam.SetAsDate(Value: TDateTime);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsDate(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsDate(Value);
  end;
end;

procedure TSQLParam.SetAsLong(Value: Long);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsLong(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsLong(Value);
  end;
end;

procedure TSQLParam.SetAsTime(Value: TDateTime);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsTime(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsTime(Value);
  end;
end;

procedure TSQLParam.SetAsDateTime(Value: TDateTime);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsDateTime(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsDateTime(Value);
  end;
end;

procedure TSQLParam.SetAsDouble(Value: Double);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsDouble(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsDouble(Value);
  end;
end;

procedure TSQLParam.SetAsFloat(Value: Float);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsFloat(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsFloat(Value);
  end;
end;

procedure TSQLParam.SetAsPointer(Value: Pointer);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsPointer(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsPointer(Value);
  end;
end;

procedure TSQLParam.SetAsShort(Value: Short);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsShort(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsShort(Value);
  end;
end;

procedure TSQLParam.SetAsString(Value: String);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsString(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsString(Value);
  end;
end;

procedure TSQLParam.SetAsVariant(Value: Variant);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsVariant(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsVariant(Value);
  end;
end;

procedure TSQLParam.SetAsBlob(Value: IBlob);
begin
  with FIBXSQLVAR do
  if not FUniqueName then
    IBError(ibxeDuplicateParamName,[FName]);
  inherited SetAsBlob(Value);
end;

procedure TSQLParam.SetAsQuad(Value: TISC_QUAD);
var i: integer;
begin
  with FIBXSQLVAR do
  if FUniqueName then
    inherited SetAsQuad(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        inherited SetAsQuad(Value);
  end;
end;

{ TIBXINPUTSQLDA }

function TIBXINPUTSQLDA.GetModified: Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to FCount - 1 do
    if FXSQLVARs[i].FModified then
    begin
      result := True;
      exit;
    end;
end;

constructor TIBXINPUTSQLDA.Create(aOwner: TFBStatement);
begin
  inherited Create(aOwner,daInput);
end;

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

procedure TIBXINPUTSQLDA.SetParamName(FieldName: String; Idx: Integer;
  UniqueName: boolean);
begin
  {$ifdef UseCaseInSensitiveParamName}
  FXSQLVARs[Idx].FName := AnsiUpperCase(FieldName);
  {$else}
  FXSQLVARs[Idx].FName := FieldName;
  {$endif}
  FXSQLVARs[Idx].FIndex := Idx;
  FXSQLVARs[Idx].FUniqueName :=  UniqueName
end;

{ TIBXOUTPUTSQLDA }

constructor TIBXOUTPUTSQLDA.Create(aOwner: TFBStatement);
begin
  inherited Create(aOwner,daOutput);
end;

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

{TColumnMetaData}

function TColumnMetaData.GetParent: TIBXSQLDA;
begin
  Result := FIBXSQLVAR.FParent;
end;

procedure TColumnMetaData.CheckActive;
begin
  if FPrepareSeqNo < (FStatement as TFBStatement).FPrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not Parent.FStatement.FPrepared  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

function TColumnMetaData.SQLData: PChar;
begin
  Result := FIBXSQLVAR.FXSQLVAR^.sqldata;
end;

function TColumnMetaData.GetDataLength: short;
begin
  Result := FIBXSQLVAR.FXSQLVAR^.sqllen;
end;

constructor TColumnMetaData.Create(aIBXSQLVAR: TIBXSQLVAR);
begin
  inherited Create;
  FIBXSQLVAR := aIBXSQLVAR;
  FStatement := FIBXSQLVAR.FParent.FStatement;
  FPrepareSeqNo := FIBXSQLVAR.FParent.FStatement.FPrepareSeqNo;
end;

function TColumnMetaData.GetAttachment: TFBAttachment;
begin
  Result := Parent.GetAttachment;
end;

function TColumnMetaData.GetTransaction: TFBTransaction;
begin
  Result := Parent.GetTransaction;
end;

function TColumnMetaData.GetSQLDialect: integer;
begin
  Result := FIBXSQLVAR.FParent.GetSQLDialect;
end;

function TColumnMetaData.GetIndex: integer;
begin
  Result := FIBXSQLVAR.FIndex;
end;

function TColumnMetaData.GetSQLType: short;
begin
  CheckActive;
  result := FIBXSQLVAR.FXSQLVAR^.sqltype and (not 1);
end;

function TColumnMetaData.getSubtype: short;
begin
  CheckActive;
  result := FIBXSQLVAR.FXSQLVAR^.sqlsubtype;
end;

function TColumnMetaData.getRelationName: string;
begin
   CheckActive;
 result := strpas(FIBXSQLVAR.FXSQLVAR^.relname);
end;

function TColumnMetaData.getOwnerName: string;
begin
  CheckActive;
  result := strpas(FIBXSQLVAR.FXSQLVAR^.ownname);
end;

function TColumnMetaData.getSQLName: string;
begin
  CheckActive;
  result := strpas(FIBXSQLVAR.FXSQLVAR^.sqlname);
end;

function TColumnMetaData.getAliasName: string;
begin
  CheckActive;
  result := strpas(FIBXSQLVAR.FXSQLVAR^.aliasname);
end;

function TColumnMetaData.GetName: string;
begin
  CheckActive;
  Result :=FIBXSQLVAR. FName;
end;

function TColumnMetaData.GetScale: short;
begin
  CheckActive;
  result := FIBXSQLVAR.FXSQLVAR^.sqlscale;
end;

function TColumnMetaData.getCharSetID: cardinal;
begin
  CheckActive;
  case SQLType of
  SQL_VARYING, SQL_TEXT:
    result := FIBXSQLVAR.FXSQLVAR^.sqlsubtype and $FF;

  else
    result := 0;
  end;
end;

function TColumnMetaData.GetIsNull: Boolean;
begin
  CheckActive;
  result := IsNullable and (FIBXSQLVAR.FXSQLVAR^.sqlind^ = -1);
end;

function TColumnMetaData.GetIsNullable: boolean;
begin
  CheckActive;
  result := (FIBXSQLVAR.FXSQLVAR^.sqltype and 1 = 1);
end;

function TColumnMetaData.GetSize: integer;
begin
  CheckActive;
  result := FIBXSQLVAR.FXSQLVAR^.sqllen;
end;

function TColumnMetaData.GetArrayMetaData: IArrayMetaData;
begin
  CheckActive;
  if GetSQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  Result := TFBArrayMetaData.Create(Parent.Statement.FAttachment,
                Parent.Statement.FTransaction,
                GetRelationName,GetSQLName);
end;

function TColumnMetaData.GetBlobMetaData: IBlobMetaData;
begin
   CheckActive;
 if GetSQLType <> SQL_BLOB then
    IBError(ibxeInvalidDataConversion,[nil]);

  Result := TFBBlobMetaData.Create(Parent.Statement.FAttachment,
                Parent.Statement.FTransaction,
                GetRelationName,GetSQLName);
end;

{ TIBXSQLDA }
constructor TIBXSQLDA.Create(aStatement: TFBStatement; sqldaType: TIBXSQLDAType);
begin
  inherited Create;
  FStatement := aStatement;
  FSize := 0;
  FUniqueRelationName := '';
  FInputSQLDA := sqldaType = daInput;
//  writeln('Creating ',ClassName);
end;

destructor TIBXSQLDA.Destroy;
var i: integer;
begin
  for i := 0 to Length(FXSQLVars) - 1 do
    FXSQLVars[i].Free;
  FreeXSQLDA;
//  writeln('Destroying ',ClassName);
  inherited Destroy;
end;

function TIBXSQLDA.GetRecordSize: Integer;
begin
  result := SizeOf(TIBXSQLDA) + XSQLDA_LENGTH(FSize);
end;

function TIBXSQLDA.GetXSQLDA: PXSQLDA;
begin
  result := FXSQLDA;
end;

function TIBXSQLDA.GetXSQLVAR(Idx: Integer): TIBXSQLVAR;
begin
  if (Idx < 0) or (Idx >= FCount) then
    IBError(ibxeXSQLDAIndexOutOfRange, [nil]);
  result := FXSQLVARs[Idx];
end;

function TIBXSQLDA.VarByName(Idx: String): TIBXSQLVAR;
begin
  result := GetXSQLVARByName(Idx);
  if result = nil then
    IBError(ibxeFieldNotFound, [Idx]);
end;

function TIBXSQLDA.GetUniqueRelationName: string;
begin
  Result := FUniqueRelationName;
end;

procedure TIBXSQLDA.Initialize;

    function FindVarByName(idx: string; limit: integer): TIBXSQLVAR;
    var
       k: integer;
    begin
         for k := 0 to limit do
             if FXSQLVARs[k].FName = idx then
             begin
                  Result := FXSQLVARs[k];
                  Exit;
             end;
         Result := nil;
    end;

var
  i, j, j_len: Integer;
  st: String;
  bUnique: Boolean;
  sBaseName: string;
begin
  bUnique := True;
  if FXSQLDA <> nil then
  begin
    for i := 0 to FCount - 1 do
    begin
      FXSQLVARs[i].RowChange;
      with Firebird25ClientAPI, FXSQLVARs[i].FXSQLVAR^ do
      begin

        {First get the unique relation name, if any}

        if bUnique and (strpas(relname) <> '') then
        begin
          if FUniqueRelationName = '' then
            FUniqueRelationName := strpas(relname)
          else
            if strpas(relname) <> FUniqueRelationName then
            begin
              FUniqueRelationName := '';
              bUnique := False;
            end;
        end;

        {If an output SQLDA then copy the aliasnames to the FName list. Ensure
         that they are all upper case only and disambiguated.
        }

        if not FInputSQLDA then
        begin
          st := Space2Underscore(AnsiUppercase(strpas(aliasname)));
          if st = '' then
          begin
            sBaseName := 'F_'; {do not localize}
            aliasname_length := 2;
            j := 1; j_len := 1;
            st := sBaseName + IntToStr(j);
          end
          else
          begin
            j := 0; j_len := 0;
            sBaseName := st;
          end;

          {Look for other columns with the same name and make unique}

          while FindVarByName(st,i-1) <> nil do
          begin
               Inc(j);
               j_len := Length(IntToStr(j));
               if j_len + Length(sBaseName) > 31 then
                  st := system.Copy(sBaseName, 1, 31 - j_len) + IntToStr(j)
               else
                  st := sBaseName + IntToStr(j);
          end;

          FXSQLVARs[i].FName := st;
        end;

        {Finally initialise the XSQLVAR}

        FXSQLVARs[i].FIndex := i;

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
          IBAlloc(sqlind, 0, SizeOf(Short))
        else
          if (sqlind <> nil) then
            ReallocMem(sqlind, 0);
      end;
    end;
  end;
end;

function TIBXSQLDA.GetAttachment: TFBAttachment;
begin
  Result := FStatement.FAttachment;
end;

function TIBXSQLDA.GetSQLDialect: integer;
begin
  Result := FStatement.SQLDialect;
end;

function TIBXSQLDA.GetTransaction: TFBTransaction;
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
    if FCount > FSize then
    begin
      Firebird25ClientAPI.IBAlloc(FXSQLDA, OldSize, XSQLDA_LENGTH(FCount));
      SetLength(FXSQLVARs, FCount);
      FXSQLDA^.version := SQLDA_VERSION1;
      p := @FXSQLDA^.sqlvar[0];
      for i := 0 to FCount - 1 do
      begin
        if i >= FSize then
          FXSQLVARs[i] := TIBXSQLVAR.Create(self);
        FXSQLVARs[i].FXSQLVAR := p;
        p := Pointer(PChar(p) + sizeof(FXSQLDA^.sqlvar));
      end;
      FSize := FCount;
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
    for i := 0 to FSize - 1 do
    begin
      FreeMem(FXSQLVARs[i].FXSQLVAR^.sqldata);
      FreeMem(FXSQLVARs[i].FXSQLVAR^.sqlind);
    end;
    FreeMem(FXSQLDA);
    FXSQLDA := nil;
    SetLength(FXSQLVARs,0);
  end;
end;

function TIBXSQLDA.GetXSQLVARByName(Idx: String): TIBXSQLVAR;
var
  s: String;
  i: Integer;
begin
  {$ifdef UseCaseInSensitiveParamName}
   s := AnsiUpperCase(Idx);
  {$else}
   s := Idx;
  {$endif}
  for i := 0 to FCount - 1 do
    if FXSQLVARs[i].FName = s then
    begin
         Result := FXSQLVARs[i];
         Exit;
    end;
  Result := nil;
end;

{ TFBStatement }

procedure TFBStatement.CheckTransaction(aTransaction: TFBTransaction);
begin
  if (aTransaction = nil) then
    IBError(ibxeTransactionNotAssigned,[]);

  if not aTransaction.InTransaction then
    IBError(ibxeNotInTransaction,[]);

end;

procedure TFBStatement.CheckHandle;
begin
  if FHandle = nil then
    IBError(ibxeInvalidStatementHandle,[nil]);
end;

procedure TFBStatement.InternalPrepare;
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
          PreprocessSQL;
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
      FSQLType := TIBSQLTypes(RB[0].GetAsInteger)
    else
      FSQLType := SQLUnknown;

    { Done getting the type }
    case FSQLType of
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
        if FSQLType in [SQLSelect, SQLSelectForUpdate,
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
end;

function TFBStatement.InternalExecute(aTransaction: TFBTransaction): IResults;
begin
  Result := nil;
  FBOF := false;
  FEOF := false;
  FSingleResults := false;
  CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  with Firebird25ClientAPI do
  case FSQLType of
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
end;

function TFBStatement.InternalOpenCursor(aTransaction: TFBTransaction
  ): IResultSet;
begin
  if FSQLType <> SQLSelect then
   IBError(ibxeIsASelectStatement,[]);

 CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
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
 Result := TResultSet.Create(FSQLRecord);
end;

procedure TFBStatement.FreeHandle;
var
  isc_res: ISC_STATUS;
begin
  try
    { The following two lines merely set the SQLDA count
     variable FCount to 0, but do not deallocate
     That way the allocations can be reused for
     a new query sring in the same SQL instance }
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

procedure TFBStatement.PreprocessSQL;
var
  cCurChar, cNextChar, cQuoteChar: Char;
  sSQL, sProcessedSQL, sParamName: String;
  i, iLenSQL, iSQLPos: Integer;
  iCurState {$ifdef ALLOWDIALECT3PARAMNAMES}, iCurParamState {$endif}: Integer;
  iParamSuffix: Integer;
  slNames: TStrings;

const
  DefaultState = 0;
  CommentState = 1;
  QuoteState = 2;
  ParamState = 3;
 {$ifdef ALLOWDIALECT3PARAMNAMES}
  ParamDefaultState = 0;
  ParamQuoteState = 1;
  {$endif}

  procedure AddToProcessedSQL(cChar: Char);
  begin
    sProcessedSQL[iSQLPos] := cChar;
    Inc(iSQLPos);
  end;

begin
  sParamName := '';
  slNames := TStringList.Create;
  try
    { Do some initializations of variables }
    iParamSuffix := 0;
    cQuoteChar := '''';
    sSQL := FSQL;
    iLenSQL := Length(sSQL);
    SetString(sProcessedSQL, nil, iLenSQL + 1);
    i := 1;
    iSQLPos := 1;
    iCurState := DefaultState;
    {$ifdef ALLOWDIALECT3PARAMNAMES}
    iCurParamState := ParamDefaultState;
    {$endif}
    { Now, traverse through the SQL string, character by character,
     picking out the parameters and formatting correctly for InterBase }
    while (i <= iLenSQL) do begin
      { Get the current token and a look-ahead }
      cCurChar := sSQL[i];
      if i = iLenSQL then
        cNextChar := #0
      else
        cNextChar := sSQL[i + 1];
      { Now act based on the current state }
      case iCurState of
        DefaultState: begin
          case cCurChar of
            '''', '"': begin
              cQuoteChar := cCurChar;
              iCurState := QuoteState;
            end;
            '?', ':': begin
              iCurState := ParamState;
              AddToProcessedSQL('?');
            end;
            '/': if (cNextChar = '*') then begin
              AddToProcessedSQL(cCurChar);
              Inc(i);
              iCurState := CommentState;
            end;
          end;
        end;
        CommentState: begin
          if (cNextChar = #0) then
            IBError(ibxeSQLParseError, [SEOFInComment])
          else if (cCurChar = '*') then begin
            if (cNextChar = '/') then
              iCurState := DefaultState;
          end;
        end;
        QuoteState: begin
          if cNextChar = #0 then
            IBError(ibxeSQLParseError, [SEOFInString])
          else if (cCurChar = cQuoteChar) then begin
            if (cNextChar = cQuoteChar) then begin
              AddToProcessedSQL(cCurChar);
              Inc(i);
            end else
              iCurState := DefaultState;
          end;
        end;
        ParamState:
        begin
          { collect the name of the parameter }
          {$ifdef ALLOWDIALECT3PARAMNAMES}
          if iCurParamState = ParamDefaultState then
          begin
            if cCurChar = '"' then
              iCurParamState := ParamQuoteState
            else
            {$endif}
            if (cCurChar in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']) then
                sParamName := sParamName + cCurChar
            else if FGenerateParamNames then
            begin
              sParamName := 'IBXParam' + IntToStr(iParamSuffix); {do not localize}
              Inc(iParamSuffix);
              iCurState := DefaultState;
              slNames.AddObject(sParamName,self); //Note local convention
                                                  //add pointer to self to mark entry
              sParamName := '';
            end
            else
              IBError(ibxeSQLParseError, [SParamNameExpected]);
          {$ifdef ALLOWDIALECT3PARAMNAMES}
          end
          else begin
            { determine if Quoted parameter name is finished }
            if cCurChar = '"' then
            begin
              Inc(i);
              slNames.Add(sParamName);
              SParamName := '';
              iCurParamState := ParamDefaultState;
              iCurState := DefaultState;
            end
            else
              sParamName := sParamName + cCurChar
          end;
          {$endif}
          { determine if the unquoted parameter name is finished }
          if {$ifdef ALLOWDIALECT3PARAMNAMES}(iCurParamState <> ParamQuoteState) and {$endif}
            (iCurState <> DefaultState) then
          begin
            if not (cNextChar in ['A'..'Z', 'a'..'z',
                                  '0'..'9', '_', '$']) then begin
              Inc(i);
              iCurState := DefaultState;
              slNames.Add(sParamName);
              sParamName := '';
            end;
          end;
        end;
      end;
      if iCurState <> ParamState then
        AddToProcessedSQL(sSQL[i]);
      Inc(i);
    end;
    AddToProcessedSQL(#0);
    FSQLParams.Count := slNames.Count;
    for i := 0 to slNames.Count - 1 do
      FSQLParams.SetParamName(slNames[i], i,FUniqueParamNames or (slNames.Objects[i] <> nil));
    FProcessedSQL := sProcessedSQL;
  finally
    slNames.Free;
  end;
end;

procedure TFBStatement.InternalClose(Force: boolean);
var
  isc_res: ISC_STATUS;
begin
  try
    if (FHandle <> nil) and (SQLType = SQLSelect) and FOpen then
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
    FOpen := False;
    FExecTransactionIntf := nil;
    FSQLRecord.FTransaction := nil;
  end;
end;

constructor TFBStatement.Create(Attachment: TFBAttachment;
  Transaction: ITransaction; sql: string; SQLDialect: integer);
var GUID : TGUID;
begin
  inherited Create;
  FAttachment := Attachment;
  FAttachmentIntf := Attachment;
  FTransaction := transaction as TFBTransaction;
  FTransactionIntf := Transaction;
  AddOwner(FTransaction);
  FSQLDialect := SQLDialect;
  CreateGuid(GUID);
  FCursor := GUIDToString(GUID);
  FSQLParams := TIBXINPUTSQLDA.Create(self);
  FSQLRecord := TIBXOUTPUTSQLDA.Create(self);
  FSQL := sql;
  InternalPrepare;
end;

constructor TFBStatement.CreateWithParameterNames(Attachment: TFBAttachment;
  Transaction: ITransaction; sql: string; SQLDialect: integer;
  GenerateParamNames: boolean; UniqueParamNames: boolean);
begin
  FHasParamNames := true;
  FGenerateParamNames := GenerateParamNames;
  FUniqueParamNames := UniqueParamNames;
  Create(Attachment,Transaction,sql,SQLDialect);
end;

destructor TFBStatement.Destroy;
begin
  Close;
  FreeHandle;
  if assigned(FSQLParams) then FSQLParams.Free;
  if assigned(FSQLRecord) then FSQLRecord.Free;
  inherited Destroy;
end;

procedure TFBStatement.Close;
begin
   InternalClose(false);
end;

function TFBStatement.FetchNext: boolean;
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

procedure TFBStatement.TransactionEnding(aTransaction: TFBTransaction;
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

function TFBStatement.GetSQLParams: ISQLParams;
begin
  CheckHandle;
  Result := TSQLParams.Create(FSQLParams);
end;

function TFBStatement.GetMetaData: IMetaData;
begin
  CheckHandle;
  Result := TMetaData.Create(FSQLRecord);
end;

function TFBStatement.GetPlan: String;
var
    RB: TSQLInfoResultsBuffer;
begin
  if (not (FSQLType in [SQLSelect, SQLSelectForUpdate,
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

function TFBStatement.GetRowsAffected(var SelectCount, InsertCount, UpdateCount,
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

function TFBStatement.GetSQLType: TIBSQLTypes;
begin
  Result := FSQLType;
end;

function TFBStatement.Execute(aTransaction: ITransaction): IResults;
begin
  if aTransaction = nil then
    Result :=  InternalExecute(FTransaction)
  else
    Result := InternalExecute(aTransaction as TFBTransaction);
end;

function TFBStatement.OpenCursor(aTransaction: ITransaction): IResultSet;
begin
  if aTransaction = nil then
    Result := InternalOpenCursor(FTransaction)
  else
    Result := InternalOpenCursor(aTransaction as TFBTransaction);
end;

function TFBStatement.CreateBlob: IBlob;
begin
  Result := TFBBlob.Create(FAttachment,FTransaction);
end;

function TFBStatement.CreateArray(column: IColumnMetaData): IArray;
begin
  if column.SQLType <> SQL_ARRAY then
    IBError(ibxeNotAnArray,[nil]);
  Result := TFBArray.Create(column.GetArrayMetaData);
end;

function TFBStatement.CreateArray(columnName: string): IArray;
var col: IColumnMetaData;
begin
  col := GetMetaData.ByName(columnName);
  Result := CreateArray(col);
end;

function TFBStatement.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TFBStatement.GetTransaction: ITransaction;
begin
  Result := FTransaction;
end;

function TFBStatement.GetSQLText: string;
begin
  Result := FSQL;
end;

function TFBStatement.IsPrepared: boolean;
begin
  Result := FHandle <> nil;
end;

procedure TFBStatement.Prepare(aTransaction: ITransaction);
begin
  if FPrepared then FreeHandle;
  if aTransaction <> nil then
  begin
    RemoveOwner(FTransaction);
    FTransaction := transaction as TFBTransaction;
    FTransactionIntf := Transaction;
    AddOwner(FTransaction);
  end;
  InternalPrepare;
end;

end.

