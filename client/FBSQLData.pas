unit FBSQLData;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$DEFINE HAS_ANSISTRING_CODEPAGE}
{$ENDIF}

{ This Unit was hacked out of the IBSQL unit and defines a class used as the
  base for interfaces accessing SQLDAVar data and Array Elements. The abstract
  methods are used to customise for an SQLDAVar or Array Element. The empty
  methods are needed for SQL parameters only. The string getters and setters
  are virtual as SQLVar and Array encodings of string data is different.}

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

interface

uses
  Classes, SysUtils, IBExternals, IBHeader, IB,  FBActivityMonitor;

type

  { TSQLDataItem }

  TSQLDataItem = class(TInterfaceParent)
  private
     function AdjustScale(Value: Int64; aScale: Integer): Double;
     function AdjustScaleToInt64(Value: Int64; aScale: Integer): Int64;
     function AdjustScaleToCurrency(Value: Int64; aScale: Integer): Currency;
     procedure SetAsInteger(AValue: Integer);
  protected
     function AdjustScaleFromCurrency(Value: Currency; aScale: Integer): Int64;
     function AdjustScaleFromDouble(Value: Double; aScale: Integer): Int64;
     procedure CheckActive; virtual;
     function GetSQLDialect: integer; virtual; abstract;
     procedure Changed; virtual;
     function SQLData: PChar; virtual; abstract;
     function GetDataLength: cardinal; virtual; abstract;
     {$IFDEF HAS_ANSISTRING_CODEPAGE}
     function GetCodePage: TSystemCodePage; virtual; abstract;
     function Transliterate(s: string; CodePage: TSystemCodePage): RawByteString;
     {$ENDIF}
     procedure SetScale(aValue: cardinal); virtual;
     procedure SetDataLength(len: cardinal); virtual;
     procedure SetSQLType(aValue: cardinal); virtual;
     property DataLength: cardinal read GetDataLength write SetDataLength;

  public
     function GetSQLType: cardinal; virtual; abstract;
     function GetSQLTypeName: string; overload;
     class function GetSQLTypeName(SQLType: short): string; overload;
     function GetName: string; virtual; abstract;
     function GetScale: cardinal; virtual; abstract;
     function GetAsBoolean: boolean;
     function GetAsCurrency: Currency;
     function GetAsInt64: Int64;
     function GetAsDateTime: TDateTime;
     function GetAsDouble: Double;
     function GetAsFloat: Float;
     function GetAsLong: Long;
     function GetAsPointer: Pointer;
     function GetAsQuad: TISC_QUAD;
     function GetAsShort: short;
     function GetAsString: String; virtual;
     function GetIsNull: Boolean; virtual;
     function getIsNullable: boolean; virtual;
     function GetAsVariant: Variant;
     function GetModified: boolean; virtual;
     procedure SetAsBoolean(AValue: boolean); virtual;
     procedure SetAsCurrency(Value: Currency); virtual;
     procedure SetAsInt64(Value: Int64); virtual;
     procedure SetAsDate(Value: TDateTime); virtual;
     procedure SetAsLong(Value: Long); virtual;
     procedure SetAsTime(Value: TDateTime); virtual;
     procedure SetAsDateTime(Value: TDateTime);
     procedure SetAsDouble(Value: Double); virtual;
     procedure SetAsFloat(Value: Float); virtual;
     procedure SetAsPointer(Value: Pointer);
     procedure SetAsQuad(Value: TISC_QUAD);
     procedure SetAsShort(Value: short); virtual;
     procedure SetAsString(Value: String); virtual;
     procedure SetAsVariant(Value: Variant);
     procedure SetIsNull(Value: Boolean);
     procedure SetIsNullable(Value: Boolean);
     procedure SetName(aValue: string); virtual;
     property AsDate: TDateTime read GetAsDateTime write SetAsDate;
     property AsBoolean:boolean read GetAsBoolean write SetAsBoolean;
     property AsTime: TDateTime read GetAsDateTime write SetAsTime;
     property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
     property AsDouble: Double read GetAsDouble write SetAsDouble;
     property AsFloat: Float read GetAsFloat write SetAsFloat;
     property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
     property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
     property AsInteger: Integer read GetAsLong write SetAsInteger;
     property AsLong: Long read GetAsLong write SetAsLong;
     property AsPointer: Pointer read GetAsPointer write SetAsPointer;
     property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
     property AsShort: short read GetAsShort write SetAsShort;
     property AsString: String read GetAsString write SetAsString;
     property AsVariant: Variant read GetAsVariant write SetAsVariant;
     property Modified: Boolean read getModified;
     property IsNull: Boolean read GetIsNull write SetIsNull;
     property IsNullable: Boolean read GetIsNullable write SetIsNullable;
     property Scale: cardinal read GetScale write SetScale;
     property SQLType: cardinal read GetSQLType write SetSQLType;
  end;

  TSQLVarData = class;

  TStatementStatus = (ssPrepared, ssExecuteResults, ssCursorOpen, ssBOF, ssEOF);

  { TSQLDataArea }

  TSQLDataArea = class
  private
    function GetColumn(index: integer): TSQLVarData;
    function GetCount: integer;
  protected
    FUniqueRelationName: string;
    FColumnList: array of TSQLVarData;
    function GetStatement: IStatement; virtual; abstract;
    function GetPrepareSeqNo: integer; virtual; abstract;
    function GetTransactionSeqNo: integer; virtual; abstract;
    procedure SetCount(aValue: integer); virtual; abstract;
  public
    procedure Initialize; virtual;
    function IsInputDataArea: boolean; virtual; abstract; {Input to Database}
    procedure PreprocessSQL(sSQL: string; GenerateParamNames,
      UniqueParamNames: boolean; var sProcessedSQL: string);
    function ColumnsInUseCount: integer; virtual;
    function ColumnByName(Idx: string): TSQLVarData;
    function CheckStatementStatus(Request: TStatementStatus): boolean; virtual; abstract;
    property Count: integer read GetCount;
    property Column[index: integer]: TSQLVarData read GetColumn;
    property UniqueRelationName: string read FUniqueRelationName;
    property Statement: IStatement read GetStatement;
    property PrepareSeqNo: integer read GetPrepareSeqNo;
    property TransactionSeqNo: integer read GetTransactionSeqNo;
  end;

  { TSQLVarData }

  TSQLVarData = class
  private
    FParent: TSQLDataArea;
    FName: string;
    FIndex: integer;
    FModified: boolean;
    FUniqueName: boolean;
    FVarString: RawByteString;
    function GetStatement: IStatement;
    procedure SetName(AValue: string);
  protected
    function GetSQLType: cardinal; virtual; abstract;
    function GetSubtype: cardinal; virtual; abstract;
    function GetAliasName: string;  virtual; abstract;
    function GetFieldName: string; virtual; abstract;
    function GetOwnerName: string;  virtual; abstract;
    function GetRelationName: string;  virtual; abstract;
    function GetScale: cardinal; virtual; abstract;
    function GetCharSetID: cardinal; virtual; abstract;
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    function GetCodePage: TSystemCodePage; virtual; abstract;
    {$ENDIF}
    function GetIsNull: Boolean;   virtual; abstract;
    function GetIsNullable: boolean; virtual; abstract;
    function GetSQLData: PChar;  virtual; abstract;
    function GetDataLength: cardinal; virtual; abstract;
    procedure SetIsNull(Value: Boolean); virtual; abstract;
    procedure SetIsNullable(Value: Boolean);  virtual; abstract;
    procedure SetSQLData(AValue: PChar; len: cardinal); virtual; abstract;
    procedure SetScale(aValue: cardinal); virtual; abstract;
    procedure SetDataLength(len: cardinal); virtual; abstract;
    procedure SetSQLType(aValue: cardinal); virtual; abstract;
  public
    constructor Create(aParent: TSQLDataArea; aIndex: integer);
    procedure SetString(aValue: string);
    procedure Changed; virtual;
    procedure RowChange; virtual;
    function GetAsArray(Array_ID: TISC_QUAD): IArray; virtual; abstract;
    function GetAsBlob(Blob_ID: TISC_QUAD): IBlob; virtual; abstract;
    function CreateBlob: IBlob; virtual; abstract;
    function GetArrayMetaData: IArrayMetaData; virtual; abstract;
    function GetBlobMetaData: IBlobMetaData; virtual; abstract;
    procedure Initialize; virtual;

  public
    property AliasName: string read GetAliasName;
    property FieldName: string read GetFieldName;
    property OwnerName: string read GetOwnerName;
    property RelationName: string read GetRelationName;
    property Parent: TSQLDataArea read FParent;
    property Index: integer read FIndex;
    property Name: string read FName write SetName;
    property CharSetID: cardinal read GetCharSetID;
    property SQLType: cardinal read GetSQLType write SetSQLType;
    property SQLSubtype: cardinal read GetSubtype;
    property SQLData: PChar read GetSQLData;
    property DataLength: cardinal read GetDataLength write SetDataLength;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
    property Scale: cardinal read GetScale write SetScale;
  public
    property Modified: Boolean read FModified;
    property Statement: IStatement read GetStatement;
    property UniqueName: boolean read FUniqueName write FUniqueName;
  end;

  { TColumnMetaData }

  TColumnMetaData = class(TSQLDataItem,IColumnMetaData)
  private
    FIBXSQLVAR: TSQLVarData;
    FStatement: IStatement;         {Keep reference to ensure statement not discarded}
    FPrepareSeqNo: integer;
    FBlobMetaData: IBlobMetaData;
    FArrayMetaData: IArrayMetaData;
  protected
    procedure CheckActive; override;
    function SQLData: PChar; override;
    function GetDataLength: cardinal; override;
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    function GetCodePage: TSystemCodePage; override;
    {$ENDIF}

  public
    constructor Create(aIBXSQLVAR: TSQLVarData);
    function GetSQLDialect: integer; override;

  public
    {IColumnMetaData}
    function GetIndex: integer;
    function GetSQLType: cardinal; override;
    function getSubtype: cardinal;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if not alias}
    function GetName: string; override;      {Disambiguated uppercase Field Name}
    function GetScale: cardinal; override;
    function getCharSetID: cardinal;
    function GetIsNullable: boolean; override;
    function GetSize: integer;
    function GetArrayMetaData: IArrayMetaData;
    function GetBlobMetaData: IBlobMetaData;
    property Name: string read GetName;
    property Size: Integer read GetSize;
    property CharSetID: cardinal read getCharSetID;
    property SQLSubtype: cardinal read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  end;

  { TIBSQLData }

  TIBSQLData = class(TColumnMetaData,ISQLData)
  protected
    procedure CheckActive; override;
    function GetIsNull: Boolean; override;
  public
    function GetAsArray: IArray;
    function GetAsBlob: IBlob;
    function GetAsString: String; override;
    property AsBlob: IBlob read GetAsBlob;
 end;

  { TSQLParam }

  TSQLParam = class(TIBSQLData,ISQLParam)
  private
    procedure InternalSetAsString(Value: String);
  protected
    procedure CheckActive; override;
    procedure Changed; override;
    procedure SetScale(aValue: cardinal); override;
    procedure SetDataLength(len: cardinal); override;
    procedure SetSQLType(aValue: cardinal); override;
  public
    constructor Create(aIBXSQLVAR: TSQLVarData);
    procedure Clear;
    function GetModified: boolean; override;
    procedure SetName(Value: string); override;
    procedure SetIsNull(Value: Boolean);
    procedure SetIsNullable(Value: Boolean);
    procedure SetAsArray(anArray: IArray);

    {overrides}
    procedure SetAsBoolean(AValue: boolean);
    procedure SetAsCurrency(AValue: Currency);
    procedure SetAsInt64(AValue: Int64);
    procedure SetAsDate(AValue: TDateTime);
    procedure SetAsLong(AValue: Long);
    procedure SetAsTime(AValue: TDateTime);
    procedure SetAsDateTime(AValue: TDateTime);
    procedure SetAsDouble(AValue: Double);
    procedure SetAsFloat(AValue: Float);
    procedure SetAsPointer(AValue: Pointer);
    procedure SetAsShort(AValue: Short);
    procedure SetAsString(AValue: String);
    procedure SetAsVariant(AValue: Variant);
    procedure SetAsBlob(aValue: IBlob);
    procedure SetAsQuad(AValue: TISC_QUAD);

    property AsBlob: IBlob read GetAsBlob write SetAsBlob;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
  end;

  { TMetaData }

  TMetaData = class(TInterfaceParent,IMetaData)
  private
    FPrepareSeqNo: integer;
    FMetaData: TSQLDataArea;
    FStatement: IStatement; {ensure FStatement not destroyed until no longer needed}
    procedure CheckActive;
  public
    constructor Create(aMetaData: TSQLDataArea);
  public
    {IMetaData}
    function GetUniqueRelationName: string;
    function getCount: integer;
    function getColumnMetaData(index: integer): IColumnMetaData;
    function ByName(Idx: String): IColumnMetaData;
  end;

  { TSQLParams }

  TSQLParams = class(TInterfaceParent,ISQLParams)
  private
    FPrepareSeqNo: integer;
    FSQLParams: TSQLDataArea;
    FStatement: IStatement; {ensure FStatement not destroyed until no longer needed}
    procedure CheckActive;
  public
    constructor Create(aSQLParams: TSQLDataArea);
  public
    {ISQLParams}
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ByName(Idx: String): ISQLParam ;
    function GetModified: Boolean;
  end;

  { TResults }

   TResults = class(TInterfaceParent,IResults)
   private
     FPrepareSeqNo: integer;
     FTransactionSeqNo: integer;
     FResults: TSQLDataArea;
     FSQLDataCache: array of ISQLData;
     FStatement: IStatement; {ensure FStatement not destroyed until no longer needed}
     function GetISQLData(aIBXSQLVAR: TSQLVarData): ISQLData;
   protected
     procedure CheckActive;
   public
     constructor Create(aResults: TSQLDataArea);
      {IResults}
     function getCount: integer;
     function ByName(Idx: String): ISQLData;
     function getSQLData(index: integer): ISQLData;
     function GetTransaction: ITransaction; virtual;
 end;

implementation

uses FBMessages, FBClientAPI, variants, IBUtils, FBTransaction;

{ TSQLDataArea }

function TSQLDataArea.GetColumn(index: integer): TSQLVarData;
begin
  Result := FColumnList[index];
end;

function TSQLDataArea.GetCount: integer;
begin
  Result := Length(FColumnList);
end;

procedure TSQLDataArea.Initialize;
var
  i: Integer;
  bUnique: Boolean;
  RelationName: string;
begin
  bUnique := True;
  for i := 0 to ColumnsInUseCount - 1 do
  begin
    RelationName := Column[i].RelationName;

    {First get the unique relation name, if any}

    if bUnique and (RelationName <> '') then
    begin
      if FUniqueRelationName = '' then
        FUniqueRelationName := RelationName
      else
      if RelationName <> FUniqueRelationName then
      begin
        FUniqueRelationName := '';
        bUnique := False;
      end;
    end;
  end;

  for i := 0 to ColumnsInUseCount - 1 do
    Column[i].Initialize;
end;

procedure TSQLDataArea.PreprocessSQL(sSQL: string; GenerateParamNames, UniqueParamNames: boolean;
  var sProcessedSQL: string);
var
  cCurChar, cNextChar, cQuoteChar: Char;
  sParamName: String;
  i, iLenSQL, iSQLPos: Integer;
  iCurState {$ifdef ALLOWDIALECT3PARAMNAMES}, iCurParamState {$endif}: Integer;
  iParamSuffix: Integer;
  slNames: TStrings;
  StrBuffer: PChar;

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
    StrBuffer[iSQLPos] := cChar;
    Inc(iSQLPos);
  end;

begin
  if not IsInputDataArea then
    IBError(ibxeNotPermitted,[nil]);

  sParamName := '';
  iLenSQL := Length(sSQL);
  GetMem(StrBuffer,iLenSQL + 1);
  slNames := TStringList.Create;
  try
    { Do some initializations of variables }
    iParamSuffix := 0;
    cQuoteChar := '''';
    i := 1;
    iSQLPos := 0;
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
            else if GenerateParamNames then
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
    sProcessedSQL := strpas(StrBuffer);
    SetCount(slNames.Count);
    for i := 0 to slNames.Count - 1 do
    begin
      Column[i].Name := slNames[i];
      Column[i].UniqueName :=  UniqueParamNames or (slNames.Objects[i] <> nil);
    end;
  finally
    slNames.Free;
    FreeMem(StrBuffer);
  end;
end;

function TSQLDataArea.ColumnsInUseCount: integer;
begin
  Result := Count;
end;

function TSQLDataArea.ColumnByName(Idx: string): TSQLVarData;
var
  s: String;
  i: Integer;
begin
  {$ifdef UseCaseInSensitiveParamName}
   s := AnsiUpperCase(Idx);
  {$else}
   s := Idx;
  {$endif}
  for i := 0 to Count - 1 do
    if Column[i].Name = s then
    begin
         Result := Column[i];
         Exit;
    end;
  Result := nil;
end;

{TSQLVarData}

function TSQLVarData.GetStatement: IStatement;
begin
  Result := FParent.Statement;
end;

procedure TSQLVarData.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  {$ifdef UseCaseInSensitiveParamName}
  if Parent.IsInputDataArea then
    FName := AnsiUpperCase(AValue)
  else
  {$endif}
    FName := AValue;
end;

constructor TSQLVarData.Create(aParent: TSQLDataArea; aIndex: integer);
begin
  inherited Create;
  FParent := aParent;
  FIndex := aIndex;
end;

procedure TSQLVarData.SetString(aValue: string);
begin
  {we take full advantage here of reference counted strings. When setting a string
   value, a reference is kept in FVarString and a pointer to it placed in the
   SQLVar. This avoids string copies. Note that PChar is guaranteed to point to
   a zero byte when the string is empty, neatly avoiding a nil pointer error.}

  FVarString := aValue;
  SQLType := SQL_TEXT;
  SetSQLData(PChar(FVarString),Length(aValue));
end;

procedure TSQLVarData.Changed;
begin
  FModified := true;
end;

procedure TSQLVarData.RowChange;
begin
  FModified := false;
  FVarString := '';
end;

procedure TSQLVarData.Initialize;

  function FindVarByName(idx: string; limit: integer): TSQLVarData;
  var
    k: integer;
  begin
      for k := 0 to limit do
          if Parent.Column[k].Name = idx then
          begin
               Result := Parent.Column[k];
               Exit;
          end;
      Result := nil;
  end;

var
  j, j_len: Integer;
  st: String;
  sBaseName: string;
begin
  RowChange;

  {If an output SQLDA then copy the aliasname to the FName. Ensure
    that they are all upper case only and disambiguated.
   }

   if not Parent.IsInputDataArea then
   begin
     st := Space2Underscore(AnsiUppercase(AliasName));
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

     while FindVarByName(st,Index-1) <> nil do
     begin
          Inc(j);
          j_len := Length(IntToStr(j));
          if j_len + Length(sBaseName) > 31 then
             st := system.Copy(sBaseName, 1, 31 - j_len) + IntToStr(j)
          else
             st := sBaseName + IntToStr(j);
     end;

     Name := st;
   end;
end;

{TSQLDataItem}

function TSQLDataItem.AdjustScale(Value: Int64; aScale: Integer): Double;
var
  Scaling : Int64;
  i: Integer;
  Val: Double;
begin
  Scaling := 1; Val := Value;
  if aScale > 0 then
  begin
    for i := 1 to aScale do
      Scaling := Scaling * 10;
    result := Val * Scaling;
  end
  else
    if aScale < 0 then
    begin
      for i := -1 downto aScale do
        Scaling := Scaling * 10;
      result := Val / Scaling;
    end
    else
      result := Val;
end;

function TSQLDataItem.AdjustScaleToInt64(Value: Int64; aScale: Integer): Int64;
var
  Scaling : Int64;
  i: Integer;
  Val: Int64;
begin
  Scaling := 1; Val := Value;
  if aScale > 0 then begin
    for i := 1 to aScale do Scaling := Scaling * 10;
    result := Val * Scaling;
  end else if aScale < 0 then begin
    for i := -1 downto aScale do Scaling := Scaling * 10;
    result := Val div Scaling;
  end else
    result := Val;
end;

function TSQLDataItem.AdjustScaleToCurrency(Value: Int64; aScale: Integer
  ): Currency;
var
  Scaling : Int64;
  i : Integer;
  FractionText, PadText, CurrText: string;
begin
  Result := 0;
  Scaling := 1;
  if aScale > 0 then
  begin
    for i := 1 to aScale do
      Scaling := Scaling * 10;
    result := Value * Scaling;
  end
  else
    if aScale < 0 then
    begin
      for i := -1 downto aScale do
        Scaling := Scaling * 10;
      FractionText := IntToStr(abs(Value mod Scaling));
      for i := Length(FractionText) to -aScale -1 do
        PadText := '0' + PadText;
      if Value < 0 then
        CurrText := '-' + IntToStr(Abs(Value div Scaling)) + DefaultFormatSettings.DecimalSeparator + PadText + FractionText
      else
        CurrText := IntToStr(Abs(Value div Scaling)) + DefaultFormatSettings.DecimalSeparator + PadText + FractionText;
      try
        result := StrToCurr(CurrText);
      except
        on E: Exception do
          IBError(ibxeInvalidDataConversion, [nil]);
      end;
    end
    else
      result := Value;
end;

procedure TSQLDataItem.SetAsInteger(AValue: Integer);
begin
  SetAsLong(aValue);
end;

function TSQLDataItem.AdjustScaleFromCurrency(Value: Currency; aScale: Integer
  ): Int64;
var
  Scaling : Int64;
  i : Integer;
begin
  Result := 0;
  Scaling := 1;
  if aScale < 0 then
  begin
    for i := -1 downto aScale do
      Scaling := Scaling * 10;
    result := trunc(Value * Scaling);
  end
  else
  if aScale > 0 then
  begin
    for i := 1 to aScale do
       Scaling := Scaling * 10;
    result := trunc(Value / Scaling);
  end
  else
    result := trunc(Value);
end;

function TSQLDataItem.AdjustScaleFromDouble(Value: Double; aScale: Integer
  ): Int64;
var
  Scaling : Int64;
  i : Integer;
begin
  Result := 0;
  Scaling := 1;
  if aScale < 0 then
  begin
    for i := -1 downto aScale do
      Scaling := Scaling * 10;
    result := trunc(Value * Scaling);
  end
  else
  if aScale > 0 then
  begin
    for i := 1 to aScale do
       Scaling := Scaling * 10;
    result := trunc(Value / Scaling);
  end
  else
    result := trunc(Value);
end;

procedure TSQLDataItem.CheckActive;
begin
  //Do nothing by default
end;

procedure TSQLDataItem.Changed;
begin
  //Do nothing by default
end;

function TSQLDataItem.Transliterate(s: string; CodePage: TSystemCodePage
  ): RawByteString;
begin
  Result := s;
  if StringCodePage(Result) <> CodePage then
    SetCodePage(Result,CodePage,CodePage <> CP_NONE);
end;

procedure TSQLDataItem.SetScale(aValue: cardinal);
begin
  //Do nothing by default
end;

procedure TSQLDataItem.SetDataLength(len: cardinal);
begin
  //Do nothing by default
end;

procedure TSQLDataItem.SetSQLType(aValue: cardinal);
begin
   //Do nothing by default
end;

function TSQLDataItem.GetSQLTypeName: string;
begin
  Result := GetSQLTypeName(GetSQLType);
end;

class function TSQLDataItem.GetSQLTypeName(SQLType: short): string;
begin
  Result := 'Unknown';
  case SQLType of
  SQL_VARYING:	        Result := 'SQL_VARYING';
  SQL_TEXT:		Result := 'SQL_TEXT';
  SQL_DOUBLE:		Result := 'SQL_DOUBLE';
  SQL_FLOAT:		Result := 'SQL_FLOAT';
  SQL_LONG:		Result := 'SQL_LONG';
  SQL_SHORT:		Result := 'SQL_SHORT';
  SQL_TIMESTAMP:	Result := 'SQL_TIMESTAMP';
  SQL_BLOB:		Result := 'SQL_BLOB';
  SQL_D_FLOAT:          Result := 'SQL_D_FLOAT';
  SQL_ARRAY:		Result := 'SQL_ARRAY';
  SQL_QUAD:		Result := 'SQL_QUAD';
  SQL_TYPE_TIME:	Result := 'SQL_TYPE_TIME';
  SQL_TYPE_DATE:	Result := 'SQL_TYPE_DATE';
  SQL_INT64:		Result := 'SQL_INT64';
  end;
end;

function TSQLDataItem.GetAsBoolean: boolean;
begin
  CheckActive;
  result := false;
  if not IsNull then
  begin
    if SQLType  = SQL_BOOLEAN then
      result := PByte(SQLData)^ = ISC_TRUE
    else
      IBError(ibxeInvalidDataConversion, [nil]);
  end
end;

function TSQLDataItem.GetAsCurrency: Currency;
begin
  CheckActive;
  result := 0;
  if GetSQLDialect < 3 then
    result := GetAsDouble
  else begin
    if not IsNull then
      case SQLType of
        SQL_TEXT, SQL_VARYING: begin
          try
            result := StrtoCurr(AsString);
          except
            on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
          end;
        end;
        SQL_SHORT:
          result := AdjustScaleToCurrency(Int64(PShort(SQLData)^),
                                      Scale);
        SQL_LONG:
          result := AdjustScaleToCurrency(Int64(PLong(SQLData)^),
                                      Scale);
        SQL_INT64:
          result := AdjustScaleToCurrency(PInt64(SQLData)^,
                                      Scale);
        SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
          result := Trunc(AsDouble);
        else
          IBError(ibxeInvalidDataConversion, [nil]);
      end;
    end;
end;

function TSQLDataItem.GetAsInt64: Int64;
begin
  CheckActive;
  result := 0;
  if not IsNull then
    case SQLType  of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToInt64(AsString);
        except
          on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := AdjustScaleToInt64(Int64(PShort(SQLData)^),
                                    Scale);
      SQL_LONG:
        result := AdjustScaleToInt64(Int64(PLong(SQLData)^),
                                    Scale);
      SQL_INT64:
        result := AdjustScaleToInt64(PInt64(SQLData)^,
                                    Scale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := Trunc(AsDouble);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetAsDateTime: TDateTime;
var
  tm_date: TCTimeStructure;
  msecs: word;
begin
  CheckActive;
  result := 0;
  if not IsNull then
    with FirebirdClientAPI do
    case SQLType of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToDate(AsString);
        except
          on E: EConvertError do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_TYPE_DATE:
        result := SQLDecodeDate(SQLData);
      SQL_TYPE_TIME:
        result := SQLDecodeTime(SQLData);
      SQL_TIMESTAMP:
        result := SQLDecodeDateTime(SQLData);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetAsDouble: Double;
begin
  CheckActive;
  result := 0;
  if not IsNull then begin
    case SQLType of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToFloat(AsString);
        except
          on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := AdjustScale(Int64(PShort(SQLData)^),
                              Scale);
      SQL_LONG:
        result := AdjustScale(Int64(PLong(SQLData)^),
                              Scale);
      SQL_INT64:
        result := AdjustScale(PInt64(SQLData)^, Scale);
      SQL_FLOAT:
        result := PFloat(SQLData)^;
      SQL_DOUBLE, SQL_D_FLOAT:
        result := PDouble(SQLData)^;
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
    if  Scale <> 0 then
      result :=
        StrToFloat(FloatToStrF(result, fffixed, 15,
                  Abs(Scale) ));
  end;
end;

function TSQLDataItem.GetAsFloat: Float;
begin
  CheckActive;
  result := 0;
  try
    result := AsDouble;
  except
    on E: EOverflow do
      IBError(ibxeInvalidDataConversion, [nil]);
  end;
end;

function TSQLDataItem.GetAsLong: Long;
begin
  CheckActive;
  result := 0;
  if not IsNull then
    case SQLType of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToInt(AsString);
        except
          on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := Trunc(AdjustScale(Int64(PShort(SQLData)^),
                                    Scale));
      SQL_LONG:
        result := Trunc(AdjustScale(Int64(PLong(SQLData)^),
                                    Scale));
      SQL_INT64:
        result := Trunc(AdjustScale(PInt64(SQLData)^, Scale));
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := Trunc(AsDouble);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetAsPointer: Pointer;
begin
  CheckActive;
  if not IsNull then
    result := SQLData
  else
    result := nil;
end;

function TSQLDataItem.GetAsQuad: TISC_QUAD;
begin
  CheckActive;
  result.gds_quad_high := 0;
  result.gds_quad_low := 0;
  if not IsNull then
    case SQLType of
      SQL_BLOB, SQL_ARRAY, SQL_QUAD:
        result := PISC_QUAD(SQLData)^;
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetAsShort: short;
begin
  CheckActive;
  result := 0;
  try
    result := AsLong;
  except
    on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
  end;
end;


function TSQLDataItem.GetAsString: String;
var
  sz: PChar;
  str_len: Integer;
  {$IFDEF HAS_ANSISTRING_CODEPAGE}
  rs: RawByteString;
  {$ENDIF}
begin
  CheckActive;
  result := '';
  { Check null, if so return a default string }
  if not IsNull then
  with FirebirdClientAPI do
    case SQLType of
      SQL_TEXT, SQL_VARYING:
      begin
        sz := SQLData;
        if (SQLType = SQL_TEXT) then
          str_len := DataLength
        else begin
          str_len := DecodeInteger(SQLData, 2);
          Inc(sz, 2);
        end;
        {$IFDEF HAS_ANSISTRING_CODEPAGE}
        SetString(rs, sz, str_len);
        SetCodePage(rs,GetCodePage,false);
        Result := rs;
        {$ELSE}
        SetString(Result, sz, str_len);
        {$ENDIF}
        if SQLType = SQL_TEXT then
          result := TrimRight(result);
      end;
      SQL_TYPE_DATE:
        case GetSQLDialect of
          1 : result := DateTimeToStr(AsDateTime);
          3 : result := DateToStr(AsDateTime);
        end;
      SQL_TYPE_TIME :
        result := TimeToStr(AsDateTime);
      SQL_TIMESTAMP:
        result := FormatDateTime(FormatSettings.ShortDateFormat + ' ' +
                            FormatSettings.LongTimeFormat+'.zzz',AsDateTime);
      SQL_SHORT, SQL_LONG:
        if Scale = 0 then
          result := IntToStr(AsLong)
        else if Scale >= (-4) then
          result := CurrToStr(AsCurrency)
        else
          result := FloatToStr(AsDouble);
      SQL_INT64:
        if Scale = 0 then
          result := IntToStr(AsInt64)
        else if Scale >= (-4) then
          result := CurrToStr(AsCurrency)
        else
          result := FloatToStr(AsDouble);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := FloatToStr(AsDouble);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetIsNull: Boolean;
begin
  CheckActive;
  Result := false;
end;

function TSQLDataItem.getIsNullable: boolean;
begin
  CheckActive;
  Result := false;
end;

function TSQLDataItem.GetAsVariant: Variant;
begin
  CheckActive;
  if IsNull then
    result := NULL
  { Check null, if so return a default string }
  else case SQLType of
      SQL_ARRAY:
        result := '(Array)'; {do not localize}
      SQL_BLOB:
        result := '(Blob)'; {do not localize}
      SQL_TEXT, SQL_VARYING:
        result := AsString;
      SQL_TIMESTAMP, SQL_TYPE_DATE, SQL_TYPE_TIME:
        result := AsDateTime;
      SQL_SHORT, SQL_LONG:
        if Scale = 0 then
          result := AsLong
        else if Scale >= (-4) then
          result := AsCurrency
        else
          result := AsDouble;
      SQL_INT64:
        if Scale = 0 then
          result := AsInt64
        else if Scale >= (-4) then
          result := AsCurrency
        else
          result := AsDouble;
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := AsDouble;
      SQL_BOOLEAN:
        result := AsBoolean;
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetModified: boolean;
begin
  Result := false;
end;


procedure TSQLDataItem.SetIsNull(Value: Boolean);
begin
  //ignore unless overridden
end;

procedure TSQLDataItem.SetIsNullable(Value: Boolean);
begin
  //ignore unless overridden
end;

procedure TSQLDataItem.SetName(aValue: string);
begin
  //ignore unless overridden
end;

procedure TSQLDataItem.SetAsCurrency(Value: Currency);
begin
  CheckActive;
  if GetSQLDialect < 3 then
    AsDouble := Value
  else
  begin
    if IsNullable then
      IsNull := False;
    SQLType := SQL_INT64;
    Scale := -4;
    DataLength := SizeOf(Int64);
    PCurrency(SQLData)^ := Value;
    Changed;
  end;
end;

procedure TSQLDataItem.SetAsInt64(Value: Int64);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_INT64;
  Scale := 0;
  DataLength := SizeOf(Int64);
  PInt64(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsDate(Value: TDateTime);
begin
  CheckActive;
  if GetSQLDialect < 3 then
  begin
    AsDateTime := Value;
    exit;
  end;

  if IsNullable then
    IsNull := False;

  SQLType := SQL_TYPE_DATE;
  DataLength := SizeOf(ISC_DATE);
  with FirebirdClientAPI do
    SQLEncodeDate(Value,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsTime(Value: TDateTime);
begin
  CheckActive;
  if GetSQLDialect < 3 then
  begin
    AsDateTime := Value;
    exit;
  end;

  if IsNullable then
    IsNull := False;

  SQLType := SQL_TYPE_TIME;
  DataLength := SizeOf(ISC_TIME);
  with FirebirdClientAPI do
    SQLEncodeTime(Value,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsDateTime(Value: TDateTime);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_TIMESTAMP;
  DataLength := SizeOf(TISC_QUAD);
  with FirebirdClientAPI do
    SQLEncodeDateTime(Value,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsDouble(Value: Double);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_DOUBLE;
  DataLength := SizeOf(Double);
  Scale := 0;
  PDouble(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsFloat(Value: Float);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_FLOAT;
  DataLength := SizeOf(Float);
  Scale := 0;
  PSingle(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsLong(Value: Long);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_LONG;
  DataLength := SizeOf(Long);
  Scale := 0;
  PLong(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsPointer(Value: Pointer);
begin
  CheckActive;
  if IsNullable and (Value = nil) then
    IsNull := True
  else begin
    IsNull := False;
    SQLType := SQL_TEXT;
    Move(Value^, SQLData^, DataLength);
  end;
  Changed;
end;

procedure TSQLDataItem.SetAsQuad(Value: TISC_QUAD);
begin
  CheckActive;
  if IsNullable then
      IsNull := False;
  if (SQLType <> SQL_BLOB) and
     (SQLType <> SQL_ARRAY) then
    IBError(ibxeInvalidDataConversion, [nil]);
  DataLength := SizeOf(TISC_QUAD);
  PISC_QUAD(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsShort(Value: short);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_SHORT;
  DataLength := SizeOf(Short);
  Scale := 0;
  PShort(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsString(Value: String);
var
   stype: Integer;

   procedure SetStringValue;
   var len: integer;
   begin
     {$IFDEF HAS_ANSISTRING_CODEPAGE}
     Value := Transliterate(Value,GetCodePage);
     {$ENDIF}
     len :=  Length(Value);
      if (GetName = 'DB_KEY') or {do not localize}
         (GetName = 'RDB$DB_KEY') then {do not localize}
          Move(Value[1], SQLData^,len)
      else
      begin
        SQLType := SQL_TEXT;
        DataLength := len;
        if (Length(Value) > 0) then
          Move(Value[1], SQLData^, len);
      end;
      Changed;
   end;

begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  stype := SQLType;
  if (stype = SQL_TEXT) or (stype = SQL_VARYING) then
    SetStringValue
  else
  begin
    if Value = '' then
      IsNull := True
    else if (stype = SQL_TIMESTAMP) or (stype = SQL_TYPE_DATE) or
      (stype = SQL_TYPE_TIME) then
      SetAsDateTime(StrToDateTime(Value))
    else
      SetStringValue;
  end;
end;

procedure TSQLDataItem.SetAsVariant(Value: Variant);
begin
  CheckActive;
  if VarIsNull(Value) then
    IsNull := True
  else case VarType(Value) of
    varEmpty, varNull:
      IsNull := True;
    varSmallint, varInteger, varByte,
      varWord, varShortInt:
      AsLong := Value;
    varInt64:
      AsInt64 := Value;
    varSingle, varDouble:
      AsDouble := Value;
    varCurrency:
      AsCurrency := Value;
    varBoolean:
      AsBoolean := Value;
    varDate:
      AsDateTime := Value;
    varOleStr, varString:
      AsString := Value;
    varArray:
      IBError(ibxeNotSupported, [nil]);
    varByRef, varDispatch, varError, varUnknown, varVariant:
      IBError(ibxeNotPermitted, [nil]);
  end;
end;

procedure TSQLDataItem.SetAsBoolean(AValue: boolean);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_BOOLEAN;
  DataLength := 1;
  Scale := 0;
  if AValue then
    PByte(SQLData)^ := ISC_TRUE
  else
    PByte(SQLData)^ := ISC_FALSE;
  Changed;
end;

{TColumnMetaData}

procedure TColumnMetaData.CheckActive;
begin
  if FPrepareSeqNo < FIBXSQLVAR.Parent.GetPrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FIBXSQLVAR.Parent.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

function TColumnMetaData.SQLData: PChar;
begin
  Result := FIBXSQLVAR.SQLData;
end;

function TColumnMetaData.GetDataLength: cardinal;
begin
  Result := FIBXSQLVAR.DataLength;
end;

function TColumnMetaData.GetCodePage: TSystemCodePage;
begin
   Result := FIBXSQLVAR.GetCodePage;
end;

constructor TColumnMetaData.Create(aIBXSQLVAR: TSQLVarData);
begin
  inherited Create;
  FIBXSQLVAR := aIBXSQLVAR;
  FStatement := FIBXSQLVAR.Statement;
  FPrepareSeqNo := FIBXSQLVAR.Parent.PrepareSeqNo;
end;


function TColumnMetaData.GetSQLDialect: integer;
begin
  Result := FIBXSQLVAR.Statement.GetSQLDialect;
end;

function TColumnMetaData.GetIndex: integer;
begin
  Result := FIBXSQLVAR.Index;
end;

function TColumnMetaData.GetSQLType: cardinal;
begin
  CheckActive;
  result := FIBXSQLVAR.SQLType;
end;

function TColumnMetaData.getSubtype: cardinal;
begin
  CheckActive;
  result := FIBXSQLVAR.SQLSubtype;
end;

function TColumnMetaData.getRelationName: string;
begin
  CheckActive;
   result :=  FIBXSQLVAR.RelationName;
end;

function TColumnMetaData.getOwnerName: string;
begin
  CheckActive;
  result :=  FIBXSQLVAR.OwnerName;
end;

function TColumnMetaData.getSQLName: string;
begin
  CheckActive;
  result :=  FIBXSQLVAR.FieldName;
end;

function TColumnMetaData.getAliasName: string;
begin
  CheckActive;
  result := FIBXSQLVAR.AliasName;
end;

function TColumnMetaData.GetName: string;
begin
  CheckActive;
  Result := FIBXSQLVAR. Name;
end;

function TColumnMetaData.GetScale: cardinal;
begin
  CheckActive;
  result := FIBXSQLVAR.Scale;
end;

function TColumnMetaData.getCharSetID: cardinal;
begin
  CheckActive;
  Result := FIBXSQLVAR.CharSetID;
end;

function TColumnMetaData.GetIsNullable: boolean;
begin
  CheckActive;
  result := FIBXSQLVAR.IsNullable;
end;

function TColumnMetaData.GetSize: integer;
begin
  CheckActive;
  result := FIBXSQLVAR.DataLength;
end;

function TColumnMetaData.GetArrayMetaData: IArrayMetaData;
begin
  CheckActive;
  result := FIBXSQLVAR.GetArrayMetaData;
end;

function TColumnMetaData.GetBlobMetaData: IBlobMetaData;
begin
  CheckActive;
  result := FIBXSQLVAR.GetBlobMetaData;
end;

{ TIBSQLData }

procedure TIBSQLData.CheckActive;
begin
  inherited CheckActive;

  if not FIBXSQLVAR.Parent.CheckStatementStatus(ssCursorOpen) and
                 not FIBXSQLVAR.Parent.CheckStatementStatus(ssExecuteResults) then
    IBError(ibxeSQLClosed, [nil]);

  if FIBXSQLVAR.Parent.CheckStatementStatus(ssEOF) then
    IBError(ibxeEOF,[nil]);

  if FIBXSQLVAR.Parent.CheckStatementStatus(ssBOF) then
    IBError(ibxeBOF,[nil]);
end;

function TIBSQLData.GetIsNull: Boolean;
begin
  CheckActive;
  result := FIBXSQLVAR.IsNull;
end;

function TIBSQLData.GetAsArray: IArray;
begin
  CheckActive;
  result := FIBXSQLVAR.GetAsArray(AsQuad);
end;

function TIBSQLData.GetAsBlob: IBlob;
begin
  CheckActive;
  result := FIBXSQLVAR.GetAsBlob(AsQuad);
end;

function TIBSQLData.GetAsString: String;
var
  ss: TStringStream;
  b: IBlob;
  {$IFDEF HAS_ANSISTRING_CODEPAGE}
  rs: rawbytestring;
  {$ENDIF}
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
        b := FIBXSQLVAR.GetAsBlob(AsQuad);
        b.SaveToStream(ss);
        {$IFDEF HAS_ANSISTRING_CODEPAGE}
        rs :=  ss.DataString;
        SetCodePage(rs,GetCodePage,false);
        Result := rs;
        {$ELSE}
        Result := ss.DataString;
        {$ENDIF}
      finally
        ss.Free;
      end;
    end;
    else
      Result := inherited GetAsString;
  end;
end;

{ TSQLParam }

procedure TSQLParam.InternalSetAsString(Value: String);
var
  ss: TStringStream;
  b: IBlob;
begin
  CheckActive;
  if IsNullable then
    IsNull := False;
  case SQLTYPE of
  SQL_BLOB:
  begin
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    ss := TStringStream.Create(Transliterate(Value,GetCodePage));
    {$ELSE}
    ss := TStringStream.Create(Value);
    {$ENDIF}
    try
      b := FIBXSQLVAR.CreateBlob;
      try
        b.LoadFromStream(ss);
        AsQuad := b.GetBlobID;
      finally
        b.Close;
      end;
    finally
      ss.Free;
    end;
  end;

  SQL_TEXT, SQL_VARYING:
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    FIBXSQLVar.SetString(Transliterate(Value,GetCodePage));
    {$ELSE}
    FIBXSQLVar.SetString(Value);
    {$ENDIF}

  else
    inherited SetAsString(Value);
  end;
end;

procedure TSQLParam.CheckActive;
begin
  if FPrepareSeqNo < FIBXSQLVAR.Parent.GetPrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FIBXSQLVAR.Parent.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

procedure TSQLParam.SetScale(aValue: cardinal);
begin
  CheckActive;
  FIBXSQLVAR.Scale := aValue;
end;

procedure TSQLParam.SetDataLength(len: cardinal);
begin
  CheckActive;
  FIBXSQLVAR.DataLength := len;
end;

procedure TSQLParam.SetSQLType(aValue: cardinal);
begin
  CheckActive;
  FIBXSQLVAR.SQLType := aValue;
end;

constructor TSQLParam.Create(aIBXSQLVAR: TSQLVarData);
begin
  inherited Create(aIBXSQLVAR);
  FIBXSQLVAR.UniqueName := true;
end;

procedure TSQLParam.Clear;
begin
  IsNull := true;
end;

function TSQLParam.GetModified: boolean;
begin
  CheckActive;
  Result := FIBXSQLVAR.Modified;
end;

procedure TSQLParam.SetName(Value: string);
begin
  CheckActive;
  FIBXSQLVAR.Name := Value;
end;

procedure TSQLParam.SetIsNull(Value: Boolean);
var i: integer;
begin
  CheckActive;
  if FIBXSQLVAR.UniqueName then
    FIBXSQLVAR.IsNull := Value
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
        Column[i].IsNull := Value;
  end
end;

procedure TSQLParam.SetIsNullable(Value: Boolean);
var i: integer;
begin
  CheckActive;
  if FIBXSQLVAR.UniqueName then
    FIBXSQLVAR.IsNullable := Value
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
        Column[i].IsNullable := Value;
  end
end;

procedure TSQLParam.SetAsArray(anArray: IArray);
begin
  CheckActive;
  if GetSQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if not FIBXSQLVAR.UniqueName then
    IBError(ibxeDuplicateParamName,[FIBXSQLVAR.Name]);

  SetAsQuad(AnArray.GetArrayID);
end;

procedure TSQLParam.Changed;
begin
  FIBXSQLVAR.Changed;
end;

procedure TSQLParam.SetAsBoolean(AValue: boolean);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsBoolean(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsBoolean(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsCurrency(AValue: Currency);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsCurrency(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsCurrency(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsInt64(AValue: Int64);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsInt64(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsInt64(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsDate(AValue: TDateTime);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsDate(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsDate(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsLong(AValue: Long);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsLong(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsLong(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsTime(AValue: TDateTime);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsTime(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsTime(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsDateTime(AValue: TDateTime);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsDateTime(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsDateTime(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsDouble(AValue: Double);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsDouble(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsDouble(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsFloat(AValue: Float);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsFloat(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsFloat(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsPointer(AValue: Pointer);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsPointer(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsPointer(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsShort(AValue: Short);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsShort(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsShort(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsString(AValue: String);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    InternalSetAsString(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          InternalSetAsString(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsVariant(AValue: Variant);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsVariant(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsVariant(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsBlob(aValue: IBlob);
begin
  with FIBXSQLVAR do
  if not UniqueName then
    IBError(ibxeDuplicateParamName,[Name]);
  CheckActive;
  aValue.Close;
  AsQuad := aValue.GetBlobID;
  Changed;
end;

procedure TSQLParam.SetAsQuad(AValue: TISC_QUAD);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsQuad(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsQuad(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

{ TMetaData }

procedure TMetaData.CheckActive;
begin
  if FPrepareSeqNo < FMetaData.PrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FMetaData.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

constructor TMetaData.Create(aMetaData: TSQLDataArea);
begin
  inherited Create;
  FMetaData := aMetaData;
  FStatement := aMetaData.Statement;
  FPrepareSeqNo := aMetaData.PrepareSeqNo;
end;

function TMetaData.GetUniqueRelationName: string;
begin
  CheckActive;
  Result := FMetaData.UniqueRelationName;
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
    Result := TColumnMetaData.Create(FMetaData.Column[index]);
end;

function TMetaData.ByName(Idx: String): IColumnMetaData;
begin
  CheckActive;
  if FMetaData.Count = 0 then
    Result := nil
  else
    Result := TColumnMetaData.Create(FMetaData.ColumnByName(Idx));
end;

{ TSQLParams }

procedure TSQLParams.CheckActive;
begin
  if FPrepareSeqNo < FSQLParams.PrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FSQLParams.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

constructor TSQLParams.Create(aSQLParams: TSQLDataArea);
begin
  inherited Create;
  FSQLParams := aSQLParams;
  FStatement := aSQLParams.Statement;
  FPrepareSeqNo := aSQLParams.PrepareSeqNo;
end;

function TSQLParams.getCount: integer;
begin
  CheckActive;
  Result := FSQLParams.Count;
end;

function TSQLParams.getSQLParam(index: integer): ISQLParam;
begin
  CheckActive;
  Result := TSQLParam.Create(FSQLParams.Column[index]);
end;

function TSQLParams.ByName(Idx: String): ISQLParam;
begin
  CheckActive;
  Result := TSQLParam.Create(FSQLParams.ColumnByName(Idx));
end;

function TSQLParams.GetModified: Boolean;
var
  i: Integer;
begin
  CheckActive;
  result := False;
  with FSQLParams do
  for i := 0 to Count - 1 do
    if Column[i].Modified then
    begin
      result := True;
      exit;
    end;
end;

{ TResults }

procedure TResults.CheckActive;
begin
  if FPrepareSeqNo < FResults.PrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FResults.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);

  with GetTransaction as TFBTransaction do
  if not InTransaction or (FResults.TransactionSeqNo <> FTransactionSeqNo) then
    IBError(ibxeInterfaceOutofDate,[nil]);
end;

function TResults.GetISQLData(aIBXSQLVAR: TSQLVarData): ISQLData;
begin
  if FSQLDataCache[aIBXSQLVAR.Index] <> nil then
    Result := FSQLDataCache[aIBXSQLVAR.Index]
  else
  begin
    Result := TIBSQLData.Create(aIBXSQLVAR);
    FSQLDataCache[aIBXSQLVAR.Index] := Result;
  end;
end;

constructor TResults.Create(aResults: TSQLDataArea);
begin
  inherited Create;
  FResults := aResults;
  FStatement := aResults.Statement;
  FPrepareSeqNo := aResults.PrepareSeqNo;
  FTransactionSeqNo := aResults.TransactionSeqNo;
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
  if FResults.CheckStatementStatus(ssBOF) then
    IBError(ibxeBOF,[nil]);
  if FResults.CheckStatementStatus(ssEOF) then
    IBError(ibxeEOF,[nil]);

  if FResults.Count = 0 then
    Result := nil
  else
    Result := GetISQLData(FResults.ColumnByName(Idx));
end;

function TResults.getSQLData(index: integer): ISQLData;
begin
  CheckActive;
  if FResults.CheckStatementStatus(ssBOF) then
    IBError(ibxeBOF,[nil]);
  if FResults.CheckStatementStatus(ssEOF) then
    IBError(ibxeEOF,[nil]);
  Result := GetISQLData(FResults.Column[index]);
end;

function TResults.GetTransaction: ITransaction;
begin
  Result := FStatement.GetTransaction;
end;


end.

