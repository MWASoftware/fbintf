unit FB30Statement;

{$mode objfpc}{$H+}

{This unit is hacked from IBSQL and contains the code for managing an XSQLDA and
 SQLVars, along with statement preparation, execution and cursor management.
 Most of the SQLVar code has been moved to unit FBSQLData. Client access is
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
  Classes, SysUtils, Firebird, IB,  FBClientAPI, FB30ClientAPI, FB30Transaction,
  FB30Attachment,IBExternals, FBSQLData, FBOutputBlock, FBActivityMonitor;

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
    FSQLType: short;
    FSQLData: PChar; {Address of SQL Data in Message Buffer}
    FSQLNullIndicator: PShort; {Address of null indicator}
    FDataLength: integer;
    FNullable: boolean;
    FScale: short;

    {input only}
    FVarString: RawByteString;  {Reference counted copy of string value FSQLData contains a pointer to it}
    FVarIsStringRef: boolean;   {true if FSQLData -> FVarString}
    FNullIndicator: short;
    constructor Create(aParent: TIBXSQLDA);
    procedure RowChange;
    procedure SetString(aValue: string);
    procedure ClearString;
  end;

  { TColumnMetaData }

  TColumnMetaData = class(TSQLDataItem,IColumnMetaData)
  private
    FIndex: integer;
    FIBXSQLVAR: TIBXSQLVAR;
    FStatement: IStatement;         {Keep reference to ensure statement not discarded}
    FPrepareSeqNo: integer;
    FBlobMetaData: IBlobMetaData;
    FArrayMetaData: IArrayMetaData;
    function GetParent: TIBXSQLDA;
  protected
    procedure CheckActive; override;
    function SQLData: PChar; override;
    function GetDataLength: short; override;

  public
    constructor Create(aIBXSQLVAR: TIBXSQLVAR);
    function GetAttachment: TFBAttachment;
    function GetTransaction: TFB30Transaction; virtual;
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
    function GetAsBlob: IBlob;
    function GetAsString: String; override;
    function GetIsNull: Boolean; override;
    property AsBlob: IBlob read GetAsBlob;
 end;

  TIBXINPUTSQLDA = class;

  { TSQLParam }

  TSQLParam = class(TIBSQLData,ISQLParam)
  private
    procedure InternalSetIsNull(Value: Boolean);
    procedure InternalSetAsString(Value: String);
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
    procedure SetAsBlob(aValue: IBlob);
    procedure SetAsQuad(Value: TISC_QUAD);

    property AsBlob: IBlob read GetAsBlob write SetAsBlob;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
  end;

  TIBXSQLDAType = (daInput,daOutput);

  { TIBXSQLDA }

  TIBXSQLDA = class
  private
    FCount: Integer;
    FSize: integer;
    FInputSQLDA: boolean;
    FMetaData: Firebird.IMessageMetadata;
    FMessageBuffer: PChar; {Message Buffer}
    FMsgLength: integer; {Message Buffer length}
    FXSQLVars: array of TIBXSQLVAR;
    FUniqueRelationName: String;
    function GetXSQLVAR(Idx: Integer): TIBXSQLVAR;
    procedure SetCount(Value: Integer);
  protected
    FStatement: TFBStatement;
    function GetAttachment: TFBAttachment;
    function GetSQLDialect: integer;
    function GetTransaction: TFB30Transaction; virtual;
    procedure FreeXSQLDA;
    function GetXSQLVARByName(Idx: String): TIBXSQLVAR;
  public
    constructor Create(aStatement: TFBStatement; sqldaType: TIBXSQLDAType);
    destructor Destroy; override;
    procedure Bind(metaData: Firebird.IMessageMetadata); virtual; abstract;
    function VarByName(Idx: String): TIBXSQLVAR;
    function GetUniqueRelationName: string;
    procedure Initialize;
    property MetaData: Firebird.IMessageMetadata read FMetaData;
    property MessageBuffer: PChar read FMessageBuffer;
    property MsgLength: integer read FMsgLength;
    property Vars[Idx: Integer]: TIBXSQLVAR read GetXSQLVAR; default;
    property Count: Integer read FCount write SetCount;
    property Statement: TFBStatement read FStatement;
  end;

  { TIBXINPUTSQLDA }

  TIBXINPUTSQLDA = class(TIBXSQLDA)
  private
    function GetModified: Boolean;
  public
    constructor Create(aOwner: TFBStatement);
    procedure Bind(aMetaData: Firebird.IMessageMetadata); override;
    procedure SetParamName(FieldName: String; Idx: Integer; UniqueName: boolean );
  end;

 { TSQLParams }

 TSQLParams = class(TInterfaceParent,ISQLParams)
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
     FTransaction: TFB30Transaction; {transaction used to execute the statement}
  public
    constructor Create(aOwner: TFBStatement);
    procedure Bind(aMetaData: Firebird.IMessageMetadata); override;
  end;

  { TMetaData }

  TMetaData = class(TInterfaceParent,IMetaData)
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

  TResults = class(TInterfaceParent,IResults)
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
    function GetTransaction: TFB30Transaction;
    procedure Close;
  end;

  { TFBStatement }

  TFBStatement = class(TActivityReporter,IStatement)
  private
    FAttachment: TFBAttachment;
    FAttachmentIntf: IAttachment;
    FTransaction: TFB30Transaction;
    FTransactionIntf: ITransaction;
    FExecTransactionIntf: ITransaction;
    FStatementIntf: Firebird.IStatement;
    FSQLType: TIBSQLTypes;         { Select, update, delete, insert, create, alter, etc...}
    FSQLDialect: integer;
    FSQLParams: TIBXINPUTSQLDA;
    FSQLRecord: TIBXOUTPUTSQLDA;
    FResultSet: Firebird.IResultSet;
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
    procedure CheckTransaction(aTransaction: TFB30Transaction);
    procedure CheckHandle;
    procedure InternalPrepare;
    function InternalExecute(aTransaction: TFB30Transaction): IResults;
    function InternalOpenCursor(aTransaction: TFB30Transaction): IResultSet;
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
    procedure TransactionEnding(aTransaction: TFB30Transaction; Force: boolean);
    property SQLDialect: integer read FSQLDialect;
    property Attachment: TFBAttachment read FAttachment;
    property Transaction: TFB30Transaction read FTransaction;
    property StatementIntf: Firebird.IStatement read FStatementIntf;

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

    property SQLParams: ISQLParams read GetSQLParams;
    property SQLType: TIBSQLTypes read GetSQLType;
end;

implementation

uses IBUtils, FBMessages, FB30Blob, variants, IBErrorCodes, FBArray, FB30Array;

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
  with FirebirdClientAPI do
     Result^.FSize := DEcodeInteger(P,2) + 3;
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
  with Firebird30ClientAPI do
  begin
    FStatement.StatementIntf.getInfo(StatusIntf,1,BytePtr(@info_request),
                     GetBufSize, BytePtr(Buffer));
    Check4DataBaseError;
  end;
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

procedure TIBXSQLVAR.SetString(aValue: string);
begin
  {we take full advantage here of reference counted strings. When setting a string
   value, a reference is kept in FVarString and a pointer to it placed in the
   SQLVar. This avoids string copies. Note that PChar is guaranteed to point to
   a zero byte when the string is empty, neatly avoiding a nil pointer error.}

  if not FVarIsStringRef then
    FreeMem(FSQLData);
  FVarString := aValue;
  FSQLType := SQL_TEXT;
  FSQLData := PChar(FVarString);
  FDataLength := Length(aValue);
  FVarIsStringRef := true;
end;

procedure TIBXSQLVAR.ClearString;
begin
  if not FVarIsStringRef then Exit;
  FVarString := '';
  FSQLData := nil;
  FDataLength := 0;
  FVarIsStringRef := false;
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

function TResultSet.GetTransaction: TFB30Transaction;
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
  if FResults.FStatement.FBOF then
    IBError(ibxeBOF,[nil]);
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
  if FResults.FStatement.FBOF then
    IBError(ibxeBOF,[nil]);
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

  if Parent.FStatement.FBOF then
    IBError(ibxeBOF,[nil]);
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
      FIBXSQLVAR.FArray := TFB30Array.Create(GetAttachment,GetTransaction,GetArrayMetaData,AsQuad);
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
    if SQLType <>  SQL_BLOB then
        IBError(ibxeInvalidDataConversion, [nil]);
    if IsNull then
      Result := nil
    else
      Result := TFBBlob.Create(GetAttachment,GetTransaction,AsQuad);
    FIBXSQLVAR.FBlob := Result;
  end;
end;

function TIBSQLData.GetAsString: String;
var
  ss: TStringStream;
  b: TFBBlob;
begin
  CheckActive;
  Result := '';
  { Check null, if so return a default string }
  if not IsNull then
  case SQLType of
    SQL_ARRAY:
      result := '(Array)'; {do not localize}
    SQL_BLOB: begin
      ss := TStringStream.Create('');
      try
        b := TFBBlob.Create(GetAttachment,GetTransaction,AsQuad);
        try
          b.SaveToStream(ss);
        finally
          b.Free;
        end;
        Result := ss.DataString;
      finally
        ss.Free;
      end;
    end;
    else
      Result := inherited GetAsString;
  end;
end;

function TIBSQLData.GetIsNull: Boolean;
begin
  CheckActive;
  result := IsNullable and (PShort(FIBXSQLVAR.FNullIndicator)^ = -1);
end;

{ TSQLParam }

procedure TSQLParam.InternalSetIsNull(Value: Boolean);
begin
  with FIBXSQLVAR do
  if Value then
  begin
    if not IsNullable then
      IsNullable := True;

    if Assigned(FSQLNullIndicator) then
      FSQLNullIndicator^ := -1;
    Changed;
  end
  else
    if ((not Value) and IsNullable) then
    begin
      if Assigned(FSQLNullIndicator) then
        FSQLNullIndicator^ := 0;
      Changed;
    end;
end;

procedure TSQLParam.InternalSetAsString(Value: String);
var
  ss: TStringStream;
  b: TFBBlob;
begin
  CheckActive;
  if IsNullable then
    IsNull := False;
  case SQLTYPE of
  SQL_BLOB:
  begin
    ss := TStringStream.Create(Value);
    try
      b := TFBBlob.Create(GetAttachment,GetTransaction);
      try
        b.LoadFromStream(ss);
      finally
        b.Close;
      end;
    finally
      ss.Free;
    end;
  end;

  SQL_TEXT, SQL_VARYING:
    FIBXSQLVar.SetString(Value);

  else
    inherited SetAsString(Value);
  end;
end;

procedure TSQLParam.InternalSetIsNullable(Value: Boolean);
begin
  with FIBXSQLVAR do
  if (Value <> IsNullable) then
  begin
    FNullable := Value;
    if Value then
    begin
      FNullIndicator := 0;
      FSQLNullIndicator := @FNullIndicator;
    end
    else
      FSQLNullIndicator := nil;
  end;
end;

procedure TSQLParam.CheckActive;
begin
  if FPrepareSeqNo < (FStatement as TFBStatement).FPrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not Parent.FStatement.FPrepared  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

procedure TSQLParam.SetDataLength(len: short);
begin
  CheckActive;
  with FIBXSQLVAR do
  begin
    ClearString;
    FDataLength := len;
    with FirebirdClientAPI do
      IBAlloc(FSQLData, 0, FDataLength);
  end;
end;

procedure TSQLParam.SetSQLType(aValue: short);
begin
  CheckActive;
  FIBXSQLVAR.FSQLType := aValue;
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

  SetAsQuad((AnArray as TFB30Array).GetArrayID);
end;

procedure TSQLParam.Changed;
begin
  FIBXSQLVAR.FModified := true;
end;

procedure TSQLParam.SetScale(aValue: short);
begin
  CheckActive;
  FIBXSQLVAR.FScale := aValue;
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
    InternalSetAsString(Value)
  else
  begin
    for i := 0 to FParent.FCount - 1 do
      with (FParent as TIBXINPUTSQLDA)[i] do
      if FName = self.FIBXSQLVAR.FName then
        InternalSetAsString(Value);
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

procedure TSQLParam.SetAsBlob(aValue: IBlob);
begin
  with FIBXSQLVAR do
  if not FUniqueName then
    IBError(ibxeDuplicateParamName,[FName]);
  CheckActive;
  aValue.Close;
  AsQuad := aValue.GetBlobID;
  Changed;
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

procedure TIBXINPUTSQLDA.Bind(aMetaData: Firebird.IMessageMetadata);
var i: integer;
begin
  FMetaData := aMetaData;
  with Firebird30ClientAPI do
  begin
    Count := metadata.getCount(StatusIntf);
    Check4DataBaseError;

    FMsgLength := metaData.getMessageLength(StatusIntf);
    Check4DataBaseError;
    IBAlloc(FMessageBuffer,0,FMsgLength);

    for i := 0 to Count - 1 do
    begin
      Vars[i].FIndex := i;
      Vars[i].FSQLData := FMessageBuffer + metaData.getOffset(StatusIntf,i);
      Check4DataBaseError;
      Vars[i].FDataLength := metaData.getLength(StatusIntf,i);
      Check4DataBaseError;
      Vars[i].FSQLNullIndicator := PShort(FMessageBuffer + metaData.getNullOffset(StatusIntf,i));
      Check4DataBaseError;
    end;
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

procedure TIBXOUTPUTSQLDA.Bind(aMetaData: Firebird.IMessageMetadata);
var i: integer;
begin
  FMetaData := aMetaData;
  with Firebird30ClientAPI do
  begin
    Count := metadata.getCount(StatusIntf);
    Check4DataBaseError;

    FMsgLength := metaData.getMessageLength(StatusIntf);
    Check4DataBaseError;
    IBAlloc(FMessageBuffer,0,FMsgLength);

    for i := 0 to Count - 1 do
    begin
      Vars[i].FIndex := i;
      Vars[i].FSQLType := aMetaData.getType(StatusIntf,i);
      Check4DataBaseError;
      Vars[i].FSQLData := FMessageBuffer + metaData.getOffset(StatusIntf,i);
      Check4DataBaseError;
      Vars[i].FDataLength := aMetaData.getLength(StatusIntf,i);
      Check4DataBaseError;
      Vars[i].FNullable := aMetaData.isNullable(StatusIntf,i);
      Check4DataBaseError;
      Vars[i].FSQLNullIndicator := PShort(FMessageBuffer + aMetaData.getNullOffset(StatusIntf,i));
      Check4DataBaseError;
      Vars[i].FScale := aMetaData.getScale(StatusIntf,i);
      Check4DataBaseError;
    end;
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
  Result := FIBXSQLVAR.FSQLData;
end;

function TColumnMetaData.GetDataLength: short;
begin
  Result := FIBXSQLVAR.FDataLength;
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

function TColumnMetaData.GetTransaction: TFB30Transaction;
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
  result := FIBXSQLVAR.FSQLType;
end;

function TColumnMetaData.getSubtype: short;
begin
  CheckActive;
  with Firebird30ClientAPI do
  begin
    result := FIBXSQLVAR.FParent.MetaData.getSubType(StatusIntf,FIBXSQLVAR.FIndex);
    Check4DataBaseError;
  end;
end;

function TColumnMetaData.getRelationName: string;
begin
  CheckActive;
  with Firebird30ClientAPI do
  begin
    result := strpas(FIBXSQLVAR.FParent.MetaData.getRelation(StatusIntf,FIBXSQLVAR.FIndex));
    Check4DataBaseError;
  end;
end;

function TColumnMetaData.getOwnerName: string;
begin
  CheckActive;
  with Firebird30ClientAPI do
  begin
    result := strpas(FIBXSQLVAR.FParent.MetaData.getOwner(StatusIntf,FIBXSQLVAR.FIndex));
    Check4DataBaseError;
  end;
end;

function TColumnMetaData.getSQLName: string;
begin
  CheckActive;
  with Firebird30ClientAPI do
  begin
    result := strpas(FIBXSQLVAR.FParent.MetaData.getField(StatusIntf,FIBXSQLVAR.FIndex));
    Check4DataBaseError;
  end;
end;

function TColumnMetaData.getAliasName: string;
begin
  CheckActive;
  with Firebird30ClientAPI do
  begin
    result := strpas(FIBXSQLVAR.FParent.MetaData.getAlias(StatusIntf,FIBXSQLVAR.FIndex));
    Check4DataBaseError;
  end;
end;

function TColumnMetaData.GetName: string;
begin
  CheckActive;
  Result :=FIBXSQLVAR. FName;
end;

function TColumnMetaData.GetScale: short;
begin
  CheckActive;
  result := FIBXSQLVAR.FScale;
end;

function TColumnMetaData.getCharSetID: cardinal;
begin
  CheckActive;
  with Firebird30ClientAPI do
  begin
    result := FIBXSQLVAR.FParent.MetaData.getCharSet(StatusIntf,FIBXSQLVAR.FIndex);
    Check4DataBaseError;
  end;
end;

function TColumnMetaData.GetIsNullable: boolean;
begin
  CheckActive;
  result := FIBXSQLVAR.FNullable;
end;

function TColumnMetaData.GetSize: integer;
begin
  CheckActive;
  result := FIBXSQLVAR.FDataLength;
end;

function TColumnMetaData.GetArrayMetaData: IArrayMetaData;
begin
  CheckActive;
  if GetSQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if FArrayMetaData = nil then
    FArrayMetaData := TFB30ArrayMetaData.Create(Parent.Statement.FAttachment,
                  Parent.Statement.FTransaction,
                  GetRelationName,GetSQLName);
  Result := FArrayMetaData;
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
begin
  FreeXSQLDA;
//  writeln('Destroying ',ClassName);
  inherited Destroy;
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
  relname: string;
  aliasname: string;
begin
  bUnique := True;
  for i := 0 to FCount - 1 do
  begin
    FXSQLVARs[i].RowChange;
    with Firebird30ClientAPI do
    begin

      {First get the unique relation name, if any}

      relname :=  strpas(MetaData.getRelation(StatusIntf,i));
      Check4DataBaseError;

      if bUnique and (relname <> '') then
      begin
        if FUniqueRelationName = '' then
          FUniqueRelationName := relname
        else
          if relname <> FUniqueRelationName then
          begin
            FUniqueRelationName := '';
            bUnique := False;
          end;
      end;

      {If an output SQLDA then copy the aliasnames to the FName list. Ensure
       that they are all upper case only and disambiguated.
      }

      aliasname := strpas(MetaData.getAlias(StatusIntf,i));
      Check4DataBaseError;

      if not FInputSQLDA then
      begin
        st := Space2Underscore(AnsiUppercase(aliasname));
        Check4DataBaseError;
        if st = '' then
        begin
          sBaseName := 'F_'; {do not localize}
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

function TIBXSQLDA.GetTransaction: TFB30Transaction;
begin
  Result := FStatement.FTransaction;
end;

function TIBXSQLDA.GetXSQLVAR(Idx: Integer): TIBXSQLVAR;
begin
  if (Idx < 0) or (Idx >= FCount) then
    IBError(ibxeXSQLDAIndexOutOfRange, [nil]);
  result := FXSQLVARs[Idx];
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
    SetLength(FXSQLVARs, FCount);
    for i := FSize to FCount - 1 do
      FXSQLVARs[i] := TIBXSQLVAR.Create(self);
    FSize := FCount;
  end;
end;

procedure TIBXSQLDA.FreeXSQLDA;
var i: integer;
begin
  for i := 0 to Length(FXSQLVARs) - 1 do
    FXSQLVARs[i].Free;
  SetLength(FXSQLVARs,0);
  FreeMem(FMessageBuffer);
  FMessageBuffer := nil;
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

procedure TFBStatement.CheckTransaction(aTransaction: TFB30Transaction);
begin
  if (aTransaction = nil) then
    IBError(ibxeTransactionNotAssigned,[]);

  if not aTransaction.InTransaction then
    IBError(ibxeNotInTransaction,[]);

end;

procedure TFBStatement.CheckHandle;
begin
  if FStatementIntf = nil then
    IBError(ibxeInvalidStatementHandle,[nil]);
end;

procedure TFBStatement.InternalPrepare;
begin
  if FPrepared then
    Exit;
  if (FSQL = '') then
    IBError(ibxeEmptyQuery, [nil]);
  try
    CheckTransaction(FTransaction);
    with Firebird30ClientAPI do
    begin
      if FHasParamNames then
      begin
        if FProcessedSQL = '' then
          PreprocessSQL;
        FStatementIntf := FAttachment.AttachmentIntf.prepare(StatusIntf,
                            FTransaction.TransactionIntf,
                            Length(FProcessedSQL),
                            PChar(FProcessedSQL),
                            FSQLDialect,
                            Firebird.IStatement.PREPARE_PREFETCH_METADATA);
      end
      else
      FStatementIntf := FAttachment.AttachmentIntf.prepare(StatusIntf,
                          FTransaction.TransactionIntf,
                          Length(FSQL),
                          PChar(FSQL),
                          FSQLDialect,
                          Firebird.IStatement.PREPARE_PREFETCH_METADATA);
      Check4DataBaseError;
      FSQLType := TIBSQLTypes(FStatementIntf.getType(StatusIntf));
      Check4DataBaseError;

      { Done getting the type }
      case FSQLType of
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
          FSQLParams.Bind(FStatementIntf.getInputMetadata(StatusIntf));
          Check4DataBaseError;

          {setup output sqlda}
          if FSQLType in [SQLSelect, SQLSelectForUpdate,
                          SQLExecProcedure] then
            FSQLRecord.Bind(FStatementIntf.getOutputMetadata(StatusIntf));
          Check4DataBaseError;
        end;
      end;
    end;
  except
    on E: Exception do begin
      if (FStatementIntf <> nil) then
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

function TFBStatement.InternalExecute(aTransaction: TFB30Transaction): IResults;
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
    with Firebird30ClientAPI do
    case FSQLType of
    SQLSelect:
      IBError(ibxeIsAExecuteProcedure,[]);

    SQLExecProcedure:
    begin
      FStatementIntf.execute(StatusIntf,
                             aTransaction.TransactionIntf,
                             FSQLParams.MetaData,
                             FSQLParams.MessageBuffer,
                             FSQLRecord.MetaData,
                             FSQLRecord.MessageBuffer);
      Check4DataBaseError;

      Result := TResults.Create(FSQLRecord);
      FSingleResults := true;
    end
    else
      FStatementIntf.execute(StatusIntf,
                             aTransaction.TransactionIntf,
                             FSQLParams.MetaData,
                             FSQLParams.MessageBuffer,
                             nil,
                             nil);
      Check4DataBaseError;
    end;
  finally
    if aTransaction <> FTransaction then
       RemoveMonitor(aTransaction);
  end;
end;

function TFBStatement.InternalOpenCursor(aTransaction: TFB30Transaction
  ): IResultSet;
begin
  if FSQLType <> SQLSelect then
   IBError(ibxeIsASelectStatement,[]);

 CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
 if aTransaction <> FTransaction then
   AddMonitor(aTransaction);
 CheckHandle;
 with Firebird30ClientAPI do
 begin
   FResultSet := FStatementIntf.openCursor(StatusIntf,
                          aTransaction.TransactionIntf,
                          FSQLParams.MetaData,
                          FSQLParams.MessageBuffer,
                          FSQLRecord.MetaData,
                          0);
   Check4DataBaseError;
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
begin
  Close;
  if FStatementIntf <> nil then
  begin
    FStatementIntf.release;
    FStatementIntf := nil;
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
begin
  try
    if (FStatementIntf <> nil) and (SQLType = SQLSelect) and FOpen then
    with Firebird30ClientAPI do
    begin
      FResultSet.close(StatusIntf);
      if not Force then Check4DataBaseError;
    end;
  finally
    if (FSQLRecord.FTransaction <> nil) and (FSQLRecord.FTransaction <> FTransaction) then
      RemoveMonitor(FSQLRecord.FTransaction);
    FOpen := False;
    FExecTransactionIntf := nil;
    FSQLRecord.FTransaction := nil;
  end;
end;

constructor TFBStatement.Create(Attachment: TFBAttachment;
  Transaction: ITransaction; sql: string; SQLDialect: integer);
var GUID : TGUID;
begin
  inherited Create(Transaction as TFB30Transaction);
  FAttachment := Attachment;
  FAttachmentIntf := Attachment;
  FTransaction := transaction as TFB30Transaction;
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
var fetchResult: integer;
begin
  result := false;
  if not FOpen then
    IBError(ibxeSQLClosed, [nil]);
  if FEOF then
    IBError(ibxeEOF,[nil]);

  with Firebird30ClientAPI do
  begin
    { Go to the next record... }
    fetchResult := FResultSet.fetchNext(StatusIntf,FSQLRecord.MessageBuffer);
    if fetchResult = Firebird.IStatus.RESULT_NO_DATA then
    begin
      FEOF := true;
      Exit; {End of File}
    end
    else
    if fetchResult <> Firebird.IStatus.RESULT_OK then
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

procedure TFBStatement.TransactionEnding(aTransaction: TFB30Transaction;
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
begin
  CheckHandle;
  if (not (FSQLType in [SQLSelect, SQLSelectForUpdate,
       {TODO: SQLExecProcedure, }
       SQLUpdate, SQLDelete])) then
    result := ''
  else
  with Firebird30ClientAPI do
  begin
    Result := FStatementIntf.getPlan(StatusIntf,true);
    Check4DataBaseError;
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
  Result := FStatementIntf <> nil;
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
    Result := InternalExecute(aTransaction as TFB30Transaction);
end;

function TFBStatement.OpenCursor(aTransaction: ITransaction): IResultSet;
begin
  if aTransaction = nil then
    Result := InternalOpenCursor(FTransaction)
  else
    Result := InternalOpenCursor(aTransaction as TFB30Transaction);
end;

function TFBStatement.CreateBlob: IBlob;
begin
  Result := TFBBlob.Create(FAttachment,FTransaction);
end;

function TFBStatement.CreateArray(column: IColumnMetaData): IArray;
begin
  if column.SQLType <> SQL_ARRAY then
    IBError(ibxeNotAnArray,[nil]);
  Result := TFB30Array.Create(FAttachment,FTransaction,column.GetArrayMetaData);
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
  Result := FStatementIntf <> nil;
end;

procedure TFBStatement.Prepare(aTransaction: ITransaction);
begin
  if FPrepared then FreeHandle;
  if aTransaction <> nil then
  begin
    RemoveMonitor(FTransaction);
    FTransaction := transaction as TFB30Transaction;
    FTransactionIntf := Transaction;
    AddMonitor(FTransaction);
  end;
  InternalPrepare;
end;

end.

