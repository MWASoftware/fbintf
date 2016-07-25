unit FB25Statement;

{$mode objfpc}{$H+}

{$define UseCaseSensitiveFieldName}

interface

uses
  Classes, SysUtils, IB, FBLibrary, FB25ClientAPI, FB25Transaction, FB25Attachment,
  IBHeader, IBExternals;

type

  TFBStatement = class;
  TIBXSQLDA = class;

  { TIBXSQLVAR }

  TIBXFieldMetaData = class(TInterfacedObject,IFieldMetadata)
  protected
    FStatement: TFBStatement;
    FIndex: Integer;
    FName: String;
    FXSQLVAR: PXSQLVAR;       { Point to the PXSQLVAR in the owner object }
  public
    constructor Create(aStatement: TFBStatement);
    property Data: PXSQLVAR read FXSQLVAR write FXSQLVAR;

  public
    {IFieldMetadata}
    function GetSQLType: cardinal;
    function getSubtype: integer;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if not alias}
    function getName: string;       {Disambiguated uppercase Field Name}
    function getScale: integer;
    function getCharSetID: cardinal;
    function getIsNullable: boolean;
    function GetSize: integer;
    property Name: string read GetName;
    property Size: Integer read GetSize;
    property CharSetID: cardinal read getCharSetID;
    property SQLType: cardinal read GetSQLType;
    property SQLSubtype: integer read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  end;

  TIBXSQLVAR = class(TIBXFieldMetaData,ISQLData)
  private
    function AdjustScale(Value: Int64; Scale: Integer): Double;
    function AdjustScaleToInt64(Value: Int64; Scale: Integer): Int64;
    function AdjustScaleToCurrency(Value: Int64; Scale: Integer): Currency;
  public
    {ISQLData}
    function GetAsBoolean: boolean;
    function GetAsCurrency: Currency;
    function GetAsInt64: Int64;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsFloat: Float;
    function GetAsLong: Long;
    function GetAsPointer: Pointer;
    function GetAsQuad: TISC_QUAD;
    function GetAsShort: Short;
    function GetAsString: String;
    function GetIsNull: Boolean;
    function GetAsVariant: Variant;
    property AsDate: TDateTime read GetAsDateTime;
    property AsBoolean:boolean read GetAsBoolean;
    property AsTime: TDateTime read GetAsDateTime;
    property AsDateTime: TDateTime read GetAsDateTime ;
    property AsDouble: Double read GetAsDouble;
    property AsFloat: Float read GetAsFloat;
    property AsCurrency: Currency read GetAsCurrency;
    property AsInt64: Int64 read GetAsInt64 ;
    property AsInteger: Integer read GetAsLong;
    property AsLong: Long read GetAsLong;
    property AsPointer: Pointer read GetAsPointer;
    property AsQuad: TISC_QUAD read GetAsQuad;
    property AsShort: Short read GetAsShort;
    property AsString: String read GetAsString;
    property AsVariant: Variant read GetAsVariant ;
    property IsNull: Boolean read GetIsNull;
  end;

  TIBXINPUTSQLDA = class;

  { TIBXSQLParam }

  TIBXSQLParam = class(TIBXSQLVAR,ISQLParam)
  private
    FModified: Boolean;
    FUniqueName: boolean;
    FParent: TIBXINPUTSQLDA;
    procedure SetAsInteger(AValue: Integer);
    procedure xSetAsBoolean(AValue: boolean);
    procedure xSetAsCurrency(Value: Currency);
    procedure xSetAsInt64(Value: Int64);
    procedure xSetAsDate(Value: TDateTime);
    procedure xSetAsTime(Value: TDateTime);
    procedure xSetAsDateTime(Value: TDateTime);
    procedure xSetAsDouble(Value: Double);
    procedure xSetAsFloat(Value: Float);
    procedure xSetAsLong(Value: Long);
    procedure xSetAsPointer(Value: Pointer);
    procedure xSetAsQuad(Value: TISC_QUAD);
    procedure xSetAsShort(Value: Short);
    procedure xSetAsString(Value: String);
    procedure xSetAsVariant(Value: Variant);
    procedure xSetIsNull(Value: Boolean);
    procedure xSetIsNullable(Value: Boolean);
  public
    constructor Create(Parent: TIBXSQLDA; aStatement: TFBStatement);
  public
    {ISQLParam support}
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
    procedure SetAsQuad(Value: TISC_QUAD);
    procedure SetAsShort(Value: Short);
    procedure SetAsString(Value: String);
    procedure SetAsVariant(Value: Variant);
    procedure SetIsNull(Value: Boolean);
    procedure SetIsNullable(Value: Boolean);
    function getModified: boolean;
    procedure Clear;
    procedure SetName(Value: string);
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
    property AsShort: Short read GetAsShort write SetAsShort;
    property AsString: String read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
    property Modified: Boolean read getModified;
    property Name: string read GetName write SetName;
  end;

   TIBXSQLVARArray = Array of TIBXSQLVAR;

  TIBXSQLDAType = (daInput,daOutput);

  { TIBXSQLDA }

  TIBXSQLDA = class(TInterfacedObject)
  private
    FCount: Integer;
    FSize: Integer;
    FInputSQLDA: boolean;
    FXSQLDA: PXSQLDA;
    FXSQLVARs: TIBXSQLVARArray; { array of IBXQLVARs }
    FUniqueRelationName: String;
    function GetRecordSize: Integer;
    function GetXSQLDA: PXSQLDA;
    procedure SetCount(Value: Integer);
  protected
    FClientAPI: TFBClientAPI;
    FStatement: TFBStatement;
    function CreateSQLVAR: TIBXSQLVAR; virtual; abstract;
    function GetXSQLVARByName(Idx: String): TIBXSQLVAR; virtual; abstract;
  public
    constructor Create(aStatement: TFBStatement; sqldaType: TIBXSQLDAType);
    destructor Destroy; override;
    function VarByName(Idx: String): TIBXSQLVAR;
    procedure Initialize;
    property AsXSQLDA: PXSQLDA read GetXSQLDA;
    property Count: Integer read FCount write SetCount;
    property RecordSize: Integer read GetRecordSize;
    property UniqueRelationName: String read FUniqueRelationName;
  end;

  { TIBXINPUTSQLDA }

  TIBXINPUTSQLDA = class(TIBXSQLDA,ISQLParams)
  private
    function GetModified: Boolean;
    function GetXSQLParam(Idx: Integer): TIBXSQLParam;
  protected
    function CreateSQLVAR: TIBXSQLVAR; override;
    function GetXSQLVARByName(Idx: String): TIBXSQLVAR; override;
  public
    constructor Create(aOwner: TFBStatement);
    procedure Bind;
    property Modified: Boolean read GetModified;
    property Vars[Idx: Integer]: TIBXSQLParam read GetXSQLParam; default;

  public
    {ISQLParams}
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ByName(Idx: String): ISQLParam ;
  end;

  { TIBXOUTPUTSQLDA }

  TIBXOUTPUTSQLDA = class(TIBXSQLDA, IMetaData)
  private
     function GetXSQLVAR(Idx: Integer): TIBXSQLVAR;
  protected
    function CreateSQLVAR: TIBXSQLVAR; override;
    function GetXSQLVARByName(Idx: String): TIBXSQLVAR; override;
  public
    constructor Create(aOwner: TFBStatement);
    procedure Bind;
    function MetaByName(Idx: String): IFieldMetaData;
    property Vars[Idx: Integer]: TIBXSQLVAR read GetXSQLVAR; default;

  public
    {IMetaData}
    function getCount: integer;
    function getFieldMetaData(index: integer): IFieldMetaData;
    function IMetaData.ByName = MetaByName;
  end;

  { TIBXResults }

  TIBXResults = class(TIBXOUTPUTSQLDA,IResults)
  public
    function ByName(Idx: String): ISQLData;
    {IResults}
    function getSQLData(index: integer): ISQLData;
//    function IResults.ByName = SQLDataByName;
  end;

  { TIBXResultSet }

  TIBXResultSet = class(TIBXResults,IResultSet)
  public
    {IResultSet}
    function FetchNext: boolean;
    procedure Close;
  end;


  { TFBStatement }

  TFBStatement = class(TInterfacedObject,IStatement)
  private
    FClientAPI: TFBClientAPI;
    FAttachment: TFBAttachment;
    FTransaction: TFBTransaction;
    FOwner: TObjectOwner;
    FHandle: TISC_STMT_HANDLE;
    FTRHandle: TISC_TR_HANDLE;
    FSQLType: TIBSQLTypes;         { Select, update, delete, insert, create, alter, etc...}
    FSQLDialect: integer;
    FSQLParams: TIBXINPUTSQLDA;
    FSQLRecord: TIBXResultSet;
    FOpen: boolean;
    FCursor: String;               { Cursor name...}
    procedure InternalPrepare(DBHandle: TISC_DB_HANDLE; sql: string);
    function InternalExecute(TRHandle: TISC_TR_HANDLE): IResults;
    procedure FreeHandle;
  public
    constructor Create(Attachment: TFBAttachment; Transaction: TFBTransaction;
      sql: string; SQLDialect: integer);
    destructor Destroy; override;
    procedure Close;
    function FetchNext: boolean;
    property SQLDialect: integer read FSQLDialect;
    property Attachment: TFBAttachment read FAttachment;
    property Transaction: TFBTransaction read FTransaction;

  public
    {IStatement}
    function GetStatus: IStatus;
    function GetSQLParams: ISQLParams;
    function GetOutMetaData: IMetaData;
    function GetPlan: String;
    function GetRowsAffected(var InsertCount, UpdateCount, DeleteCount: integer): boolean;
    function GetSQLType: TIBSQLTypes;
    function Execute: IResults; overload;
    function Execute(aTransaction: ITransaction): IResults; overload;
    function OpenCursor: IResultSet;
    property ClientAPI: TFBClientAPI read FClientAPI;
    property Handle: TISC_STMT_HANDLE read FHandle;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLType: TIBSQLTypes read GetSQLType;

end;

implementation

uses IBUtils, FBErrorMessages, FB25Blob, variants, IBErrorCodes, FB25ResultBuffer;

const
   sSQLErrorSeparator = ' When Executing: ';

{ TIBXResults }

function TIBXResults.getSQLData(index: integer): ISQLData;
begin
  Result := Vars[index];
end;

function TIBXResults.ByName(Idx: String): ISQLData;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TIBXSQLVar(VarByName(Idx));
end;


{ TIBXResultSet }

function TIBXResultSet.FetchNext: boolean;
begin
  Result := FStatement.FetchNext;
end;

procedure TIBXResultSet.Close;
begin
  FStatement.Close;
end;

{ TIBXINPUTSQLDA }

function TIBXINPUTSQLDA.GetModified: Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to FCount - 1 do
    if TIBXSQLParam(FXSQLVARs[i]).Modified then
    begin
      result := True;
      exit;
    end;
end;

function TIBXINPUTSQLDA.GetXSQLParam(Idx: Integer): TIBXSQLParam;
begin
  if (Idx < 0) or (Idx >= FCount) then
    IBError(ibxeXSQLDAIndexOutOfRange, [nil]);
  result := TIBXSQLParam(FXSQLVARs[Idx])
end;

function TIBXINPUTSQLDA.CreateSQLVAR: TIBXSQLVAR;
begin
  Result := TIBXSQLParam.Create(self,FStatement);
end;

function TIBXINPUTSQLDA.GetXSQLVARByName(Idx: String): TIBXSQLVAR;
var
  s: String;
  i: Integer;
begin
  {$ifdef UseCaseSensitiveFieldName}
   s := AnsiUpperCase(Idx);
  {$else}
   s := Idx;
  {$endif}
  for i := 0 to FCount - 1 do
    if Vars[i].FName = s then
    begin
         Result := FXSQLVARs[i];
         Exit;
    end;
  Result := nil;
end;

constructor TIBXINPUTSQLDA.Create(aOwner: TFBStatement);
begin
  inherited Create(aOwner,daInput);
end;

procedure TIBXINPUTSQLDA.Bind;
begin
  Count := 1;
  with FClientAPI do
  begin
    if (FXSQLDA <> nil) then
       Call(isc_dsql_describe_bind(StatusVector, @(FStatement.Handle), FStatement.SQLDialect,
                                    FXSQLDA), true);

    if FXSQLDA^.sqld > FXSQLDA^.sqln then
    begin
      Count := FXSQLDA^.sqld;
      Call(isc_dsql_describe_bind(StatusVector, @(FStatement.Handle), FStatement.SQLDialect,
                                   FXSQLDA), true);
    end
    else
    if FXSQLDA^.sqld = 0 then
      Count := 0;
  end;
  Initialize;
end;

function TIBXINPUTSQLDA.getCount: integer;
begin
  Result := Count;
end;

function TIBXINPUTSQLDA.getSQLParam(index: integer): ISQLParam;
begin
  Result := GetXSQLParam(index);
end;

function TIBXINPUTSQLDA.ByName(Idx: String): ISQLParam;
begin
  Result := TIBXSQLParam(VarByName(Idx));
end;

{ TIBXOUTPUTSQLDA }

function TIBXOUTPUTSQLDA.GetXSQLVAR(Idx: Integer): TIBXSQLVAR;
begin
  if (Idx < 0) or (Idx >= FCount) then
    IBError(ibxeXSQLDAIndexOutOfRange, [nil]);
  result := FXSQLVARs[Idx]
end;

function TIBXOUTPUTSQLDA.CreateSQLVAR: TIBXSQLVAR;
begin
  Result := TIBXSQLVAR.Create(FStatement);
end;

function TIBXOUTPUTSQLDA.GetXSQLVARByName(Idx: String): TIBXSQLVAR;
var
  s: String;
  i: Integer;
begin
  {$ifdef UseCaseSensitiveFieldName}
   s := AnsiUpperCase(Idx);
  {$else}
   s := Idx;
  {$endif}
  for i := 0 to FCount - 1 do
    if Vars[i].FName = s then
    begin
         Result := FXSQLVARs[i];
         Exit;
    end;
  Result := nil;
end;

constructor TIBXOUTPUTSQLDA.Create(aOwner: TFBStatement);
begin
  inherited Create(aOwner,daOutput);
end;

procedure TIBXOUTPUTSQLDA.Bind;
begin
  { Allocate an initial output descriptor (with one column) }
  Count := 1;
  with FClientAPI do
  begin
    { Using isc_dsql_describe, get the right size for the columns... }
    Call(isc_dsql_describe(StatusVector, @(FStatement.Handle), FStatement.SQLDialect, FXSQLDA), True);
    if FXSQLDA^.sqld > FXSQLDA^.sqln then
    begin
      Count := FXSQLDA^.sqld;
      Call(isc_dsql_describe(StatusVector, @(FStatement.Handle), FStatement.SQLDialect, FXSQLDA), True);
    end
    else
    if FXSQLDA^.sqld = 0 then
      Count := 0;
  end;
  Initialize;
end;

function TIBXOUTPUTSQLDA.MetaByName(Idx: String): IFieldMetaData;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TIBXFieldMetaData(VarByName(Idx));
end;

function TIBXOUTPUTSQLDA.getCount: integer;
begin
  Result := Count;
end;

function TIBXOUTPUTSQLDA.getFieldMetaData(index: integer): IFieldMetaData;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TIBXFieldMetaData(Vars[index]);
end;

  { TIBXSQLVAR }

function TIBXSQLVAR.AdjustScale(Value: Int64; Scale: Integer): Double;
var
  Scaling : Int64;
  i: Integer;
  Val: Double;
begin
  Scaling := 1; Val := Value;
  if Scale > 0 then
  begin
    for i := 1 to Scale do
      Scaling := Scaling * 10;
    result := Val * Scaling;
  end
  else
    if Scale < 0 then
    begin
      for i := -1 downto Scale do
        Scaling := Scaling * 10;
      result := Val / Scaling;
    end
    else
      result := Val;
end;

function TIBXSQLVAR.AdjustScaleToInt64(Value: Int64; Scale: Integer): Int64;
var
  Scaling : Int64;
  i: Integer;
  Val: Int64;
begin
  Scaling := 1; Val := Value;
  if Scale > 0 then begin
    for i := 1 to Scale do Scaling := Scaling * 10;
    result := Val * Scaling;
  end else if Scale < 0 then begin
    for i := -1 downto Scale do Scaling := Scaling * 10;
    result := Val div Scaling;
  end else
    result := Val;
end;

function TIBXSQLVAR.AdjustScaleToCurrency(Value: Int64; Scale: Integer): Currency;
var
  Scaling : Int64;
  i : Integer;
  FractionText, PadText, CurrText: string;
begin
  Result := 0;
  Scaling := 1;
  if Scale > 0 then
  begin
    for i := 1 to Scale do
      Scaling := Scaling * 10;
    result := Value * Scaling;
  end
  else
    if Scale < 0 then
    begin
      for i := -1 downto Scale do
        Scaling := Scaling * 10;
      FractionText := IntToStr(abs(Value mod Scaling));
      for i := Length(FractionText) to -Scale -1 do
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

constructor TIBXFieldMetaData.Create(aStatement: TFBStatement);
begin
  inherited Create;
  FStatement := aStatement;
end;

function TIBXFieldMetaData.GetSQLType: cardinal;
begin
  result := FXSQLVAR^.sqltype and (not 1);
end;

function TIBXFieldMetaData.getSubtype: integer;
begin
  result := FXSQLVAR^.sqltype and (not 1);
end;

function TIBXFieldMetaData.getRelationName: string;
begin
  result := strpas(FXSQLVAR^.relname);
end;

function TIBXFieldMetaData.getOwnerName: string;
begin
  result := strpas(FXSQLVAR^.ownname);
end;

function TIBXFieldMetaData.getSQLName: string;
begin
  result := strpas(FXSQLVAR^.sqlname);
end;

function TIBXFieldMetaData.getAliasName: string;
begin
  result := strpas(FXSQLVAR^.aliasname);
end;

function TIBXFieldMetaData.getName: string;
begin
  Result := FName;
end;

function TIBXFieldMetaData.getScale: integer;
begin
  result := FXSQLVAR^.sqlscale;
end;

function TIBXFieldMetaData.getCharSetID: cardinal;
begin
  case SQLType of
  SQL_VARYING, SQL_TEXT:
    result := SQLSubtype and $FF;

  else
    result := 0;
  end;
end;

function TIBXFieldMetaData.getIsNullable: boolean;
begin
  result := (FXSQLVAR^.sqltype and 1 = 1);
end;

function TIBXFieldMetaData.GetSize: integer;
begin
  result := FXSQLVAR^.sqllen;
end;

  {TIBXSQLVAR}

function TIBXSQLVAR.GetAsBoolean: boolean;
begin
  result := false;
  if not IsNull then
  begin
    if FXSQLVAR^.sqltype and (not 1) = SQL_BOOLEAN then
      result := PByte(FXSQLVAR^.sqldata)^ = ISC_TRUE
    else
      IBError(ibxeInvalidDataConversion, [nil]);
  end
end;

function TIBXSQLVAR.GetAsCurrency: Currency;
begin
  result := 0;
  if FStatement.SQLDialect < 3 then
    result := GetAsDouble
  else begin
    if not IsNull then
      case FXSQLVAR^.sqltype and (not 1) of
        SQL_TEXT, SQL_VARYING: begin
          try
            result := StrtoCurr(AsString);
          except
            on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
          end;
        end;
        SQL_SHORT:
          result := AdjustScaleToCurrency(Int64(PShort(FXSQLVAR^.sqldata)^),
                                      FXSQLVAR^.sqlscale);
        SQL_LONG:
          result := AdjustScaleToCurrency(Int64(PLong(FXSQLVAR^.sqldata)^),
                                      FXSQLVAR^.sqlscale);
        SQL_INT64:
          result := AdjustScaleToCurrency(PInt64(FXSQLVAR^.sqldata)^,
                                      FXSQLVAR^.sqlscale);
        SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
          result := Trunc(AsDouble);
        else
          IBError(ibxeInvalidDataConversion, [nil]);
      end;
    end;
end;

function TIBXSQLVAR.GetAsInt64: Int64;
begin
  result := 0;
  if not IsNull then
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToInt64(AsString);
        except
          on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := AdjustScaleToInt64(Int64(PShort(FXSQLVAR^.sqldata)^),
                                    FXSQLVAR^.sqlscale);
      SQL_LONG:
        result := AdjustScaleToInt64(Int64(PLong(FXSQLVAR^.sqldata)^),
                                    FXSQLVAR^.sqlscale);
      SQL_INT64:
        result := AdjustScaleToInt64(PInt64(FXSQLVAR^.sqldata)^,
                                    FXSQLVAR^.sqlscale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := Trunc(AsDouble);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TIBXSQLVAR.GetAsDateTime: TDateTime;
var
  tm_date: TCTimeStructure;
  msecs: word;
begin
  result := 0;
  if not IsNull then
    with FStatement.FClientAPI do
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToDate(AsString);
        except
          on E: EConvertError do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_TYPE_DATE:
      begin
        isc_decode_sql_date(PISC_DATE(FXSQLVAR^.sqldata), @tm_date);
        try
          result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1),
                               Word(tm_date.tm_mday));
        except
          on E: EConvertError do begin
            IBError(ibxeInvalidDataConversion, [nil]);
          end;
        end;
      end;
      SQL_TYPE_TIME: begin
        isc_decode_sql_time(PISC_TIME(FXSQLVAR^.sqldata), @tm_date);
        try
          msecs :=  (PISC_TIME(FXSQLVAR^.sqldata)^ mod 10000) div 10;
          result := EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                               Word(tm_date.tm_sec), msecs)
        except
          on E: EConvertError do begin
            IBError(ibxeInvalidDataConversion, [nil]);
          end;
        end;
      end;
      SQL_TIMESTAMP: begin
        isc_decode_date(PISC_QUAD(FXSQLVAR^.sqldata), @tm_date);
        try
          result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1),
                              Word(tm_date.tm_mday));
          msecs := (PISC_TIMESTAMP(FXSQLVAR^.sqldata)^.timestamp_time mod 10000) div 10;
          if result >= 0 then
            result := result + EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                                          Word(tm_date.tm_sec), msecs)
          else
            result := result - EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                                          Word(tm_date.tm_sec), msecs)
        except
          on E: EConvertError do begin
            IBError(ibxeInvalidDataConversion, [nil]);
          end;
        end;
      end;
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TIBXSQLVAR.GetAsDouble: Double;
begin
  result := 0;
  if not IsNull then begin
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToFloat(AsString);
        except
          on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := AdjustScale(Int64(PShort(FXSQLVAR^.sqldata)^),
                              FXSQLVAR^.sqlscale);
      SQL_LONG:
        result := AdjustScale(Int64(PLong(FXSQLVAR^.sqldata)^),
                              FXSQLVAR^.sqlscale);
      SQL_INT64:
        result := AdjustScale(PInt64(FXSQLVAR^.sqldata)^, FXSQLVAR^.sqlscale);
      SQL_FLOAT:
        result := PFloat(FXSQLVAR^.sqldata)^;
      SQL_DOUBLE, SQL_D_FLOAT:
        result := PDouble(FXSQLVAR^.sqldata)^;
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
    if  FXSQLVAR^.sqlscale <> 0 then
      result :=
        StrToFloat(FloatToStrF(result, fffixed, 15,
                  Abs(FXSQLVAR^.sqlscale) ));
  end;
end;

function TIBXSQLVAR.GetAsFloat: Float;
begin
  result := 0;
  try
    result := AsDouble;
  except
    on E: EOverflow do
      IBError(ibxeInvalidDataConversion, [nil]);
  end;
end;

function TIBXSQLVAR.GetAsLong: Long;
begin
  result := 0;
  if not IsNull then
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToInt(AsString);
        except
          on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := Trunc(AdjustScale(Int64(PShort(FXSQLVAR^.sqldata)^),
                                    FXSQLVAR^.sqlscale));
      SQL_LONG:
        result := Trunc(AdjustScale(Int64(PLong(FXSQLVAR^.sqldata)^),
                                    FXSQLVAR^.sqlscale));
      SQL_INT64:
        result := Trunc(AdjustScale(PInt64(FXSQLVAR^.sqldata)^, FXSQLVAR^.sqlscale));
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := Trunc(AsDouble);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TIBXSQLVAR.GetAsPointer: Pointer;
begin
  if not IsNull then
    result := FXSQLVAR^.sqldata
  else
    result := nil;
end;

function TIBXSQLVAR.GetAsQuad: TISC_QUAD;
begin
  result.gds_quad_high := 0;
  result.gds_quad_low := 0;
  if not IsNull then
    case FXSQLVAR^.sqltype and (not 1) of
      SQL_BLOB, SQL_ARRAY, SQL_QUAD:
        result := PISC_QUAD(FXSQLVAR^.sqldata)^;
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TIBXSQLVAR.GetAsShort: Short;
begin
  result := 0;
  try
    result := AsLong;
  except
    on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
  end;
end;


function TIBXSQLVAR.GetAsString: String;
var
  sz: PChar;
  str_len: Integer;
  ss: TStringStream;
  b: TFBBlob;
begin
  result := '';
  { Check null, if so return a default string }
  if not IsNull then
  with FStatement.FClientAPI do
    case FXSQLVar^.sqltype and (not 1) of
      SQL_ARRAY:
        result := '(Array)'; {do not localize}
      SQL_BLOB: begin
        ss := TStringStream.Create('');
        try
          b := TFBBlob.Create(FStatement.Attachment,FStatement.Transaction,AsQuad);
          try
            b.SaveToStream(ss);
          finally
            b.Free;
          end;
          result := ss.DataString;
        finally
          ss.Free;
        end;
      end;
      SQL_TEXT, SQL_VARYING: begin
        sz := FXSQLVAR^.sqldata;
        if (FXSQLVar^.sqltype and (not 1) = SQL_TEXT) then
          str_len := FXSQLVar^.sqllen
        else begin
          str_len := isc_vax_integer(FXSQLVar^.sqldata, 2);
          Inc(sz, 2);
        end;
        SetString(result, sz, str_len);
        if ((FXSQLVar^.sqltype and (not 1)) = SQL_TEXT) then
          result := TrimRight(result);
      end;
      SQL_TYPE_DATE:
        case FStatement.SQLDialect of
          1 : result := DateTimeToStr(AsDateTime);
          3 : result := DateToStr(AsDateTime);
        end;
      SQL_TYPE_TIME :
        result := TimeToStr(AsDateTime);
      SQL_TIMESTAMP:
        result := DateTimeToStr(AsDateTime);
      SQL_SHORT, SQL_LONG:
        if FXSQLVAR^.sqlscale = 0 then
          result := IntToStr(AsLong)
        else if FXSQLVAR^.sqlscale >= (-4) then
          result := CurrToStr(AsCurrency)
        else
          result := FloatToStr(AsDouble);
      SQL_INT64:
        if FXSQLVAR^.sqlscale = 0 then
          result := IntToStr(AsInt64)
        else if FXSQLVAR^.sqlscale >= (-4) then
          result := CurrToStr(AsCurrency)
        else
          result := FloatToStr(AsDouble);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := FloatToStr(AsDouble);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TIBXSQLVAR.GetIsNull: Boolean;
begin
  result := IsNullable and (FXSQLVAR^.sqlind^ = -1)
end;

function TIBXSQLVAR.GetAsVariant: Variant;
begin
  if IsNull then
    result := NULL
  { Check null, if so return a default string }
  else case FXSQLVar^.sqltype and (not 1) of
      SQL_ARRAY:
        result := '(Array)'; {do not localize}
      SQL_BLOB:
        result := '(Blob)'; {do not localize}
      SQL_TEXT, SQL_VARYING:
        result := AsString;
      SQL_TIMESTAMP, SQL_TYPE_DATE, SQL_TYPE_TIME:
        result := AsDateTime;
      SQL_SHORT, SQL_LONG:
        if FXSQLVAR^.sqlscale = 0 then
          result := AsLong
        else if FXSQLVAR^.sqlscale >= (-4) then
          result := AsCurrency
        else
          result := AsDouble;
      SQL_INT64:
        if FXSQLVAR^.sqlscale = 0 then
          result := AsInt64
        else if FXSQLVAR^.sqlscale >= (-4) then
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

  {TIBXSQLParam}

procedure TIBXSQLParam.SetAsBoolean(AValue: boolean);
var
  i: Integer;
begin
  if FUniqueName then
     xSetAsBoolean(AValue)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsBoolean(AValue);
end;

procedure TIBXSQLParam.xSetAsCurrency(Value: Currency);
begin
  if IsNullable then
    IsNull := False;
  FXSQLVAR^.sqltype := SQL_INT64 or (FXSQLVAR^.sqltype and 1);
  FXSQLVAR^.sqlscale := -4;
  FXSQLVAR^.sqllen := SizeOf(Int64);
  FStatement.FClientAPI.IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
  PCurrency(FXSQLVAR^.sqldata)^ := Value;
  FModified := True;
end;

procedure TIBXSQLParam.SetAsCurrency(Value: Currency);
var
  i: Integer;
begin
  if FStatement.SQLDialect < 3 then
    AsDouble := Value
  else
  begin

    if FUniqueName then
       xSetAsCurrency(Value)
    else
    for i := 0 to FParent.FCount - 1 do
      if FParent[i].FName = FName then
           FParent[i].xSetAsCurrency(Value);
  end;
end;

procedure TIBXSQLParam.xSetAsInt64(Value: Int64);
begin
  if IsNullable then
    IsNull := False;

  FXSQLVAR^.sqltype := SQL_INT64 or (FXSQLVAR^.sqltype and 1);
  FXSQLVAR^.sqlscale := 0;
  FXSQLVAR^.sqllen := SizeOf(Int64);
  FStatement.FClientAPI.IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
  PInt64(FXSQLVAR^.sqldata)^ := Value;
  FModified := True;
end;

procedure TIBXSQLParam.SetAsInt64(Value: Int64);
var
  i: Integer;
begin
  if FUniqueName then
     xSetAsInt64(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
          FParent[i].xSetAsInt64(Value);
end;

procedure TIBXSQLParam.xSetAsDate(Value: TDateTime);
var
   tm_date: TCTimeStructure;
   Yr, Mn, Dy: Word;
begin
  if IsNullable then
    IsNull := False;

  FXSQLVAR^.sqltype := SQL_TYPE_DATE or (FXSQLVAR^.sqltype and 1);
  DecodeDate(Value, Yr, Mn, Dy);
  with tm_date do begin
    tm_sec := 0;
    tm_min := 0;
    tm_hour := 0;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  FXSQLVAR^.sqllen := SizeOf(ISC_DATE);
  with FStatement.FClientAPI do
  begin
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
    isc_encode_sql_date(@tm_date, PISC_DATE(FXSQLVAR^.sqldata));
  end;
  FModified := True;
end;

procedure TIBXSQLParam.SetAsDate(Value: TDateTime);
var
  i: Integer;
begin
  if FStatement.SQLDialect < 3 then
  begin
    AsDateTime := Value;
    exit;
  end;

  if FUniqueName then
     xSetAsDate(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsDate(Value);
end;

procedure TIBXSQLParam.xSetAsTime(Value: TDateTime);
var
  tm_date: TCTimeStructure;
  Hr, Mt, S, Ms: Word;
begin
  if IsNullable then
    IsNull := False;

  FXSQLVAR^.sqltype := SQL_TYPE_TIME or (FXSQLVAR^.sqltype and 1);
  DecodeTime(Value, Hr, Mt, S, Ms);
  with tm_date do begin
    tm_sec := S;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := 0;
    tm_mon := 0;
    tm_year := 0;
  end;
  FXSQLVAR^.sqllen := SizeOf(ISC_TIME);
  with FStatement.FClientAPI do
  begin
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
    isc_encode_sql_time(@tm_date, PISC_TIME(FXSQLVAR^.sqldata));
  end;
  if Ms > 0 then
    Inc(PISC_TIME(FXSQLVAR^.sqldata)^,Ms*10);
  FModified := True;
end;

procedure TIBXSQLParam.SetAsTime(Value: TDateTime);
var
  i: Integer;
begin
  if FStatement.SQLDialect < 3 then
  begin
    AsDateTime := Value;
    exit;
  end;

  if FUniqueName then
     xSetAsTime(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsTime(Value);
end;

procedure TIBXSQLParam.xSetAsDateTime(Value: TDateTime);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy, Hr, Mt, S, Ms: Word;
begin
  if IsNullable then
    IsNull := False;

  FXSQLVAR^.sqltype := SQL_TIMESTAMP or (FXSQLVAR^.sqltype and 1);
  DecodeDate(Value, Yr, Mn, Dy);
  DecodeTime(Value, Hr, Mt, S, Ms);
  with tm_date do begin
    tm_sec := S;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  FXSQLVAR^.sqllen := SizeOf(TISC_QUAD);
  with FStatement.FClientAPI do
  begin
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
    isc_encode_date(@tm_date, PISC_QUAD(FXSQLVAR^.sqldata));
  end;
  if Ms > 0 then
    Inc(PISC_TIMESTAMP(FXSQLVAR^.sqldata)^.timestamp_time,Ms*10);
  FModified := True;
end;

procedure TIBXSQLParam.SetAsDateTime(Value: TDateTime);
var
  i: Integer;
begin
  if FUniqueName then
     xSetAsDateTime(value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsDateTime(Value);
end;

procedure TIBXSQLParam.xSetAsDouble(Value: Double);
begin
  if IsNullable then
    IsNull := False;

  FXSQLVAR^.sqltype := SQL_DOUBLE or (FXSQLVAR^.sqltype and 1);
  FXSQLVAR^.sqllen := SizeOf(Double);
  FXSQLVAR^.sqlscale := 0;
  with FStatement.FClientAPI do
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
  PDouble(FXSQLVAR^.sqldata)^ := Value;
  FModified := True;
end;

procedure TIBXSQLParam.SetAsDouble(Value: Double);
var
  i: Integer;
begin
  if FUniqueName then
     xSetAsDouble(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsDouble(Value);
end;

procedure TIBXSQLParam.xSetAsFloat(Value: Float);
begin
  if IsNullable then
    IsNull := False;

  FXSQLVAR^.sqltype := SQL_FLOAT or (FXSQLVAR^.sqltype and 1);
  FXSQLVAR^.sqllen := SizeOf(Float);
  FXSQLVAR^.sqlscale := 0;
  with FStatement.FClientAPI do
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
  PSingle(FXSQLVAR^.sqldata)^ := Value;
  FModified := True;
end;

procedure TIBXSQLParam.SetAsFloat(Value: Float);
var
  i: Integer;
begin
  if FUniqueName then
     xSetAsFloat(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsFloat(Value);
end;

procedure TIBXSQLParam.xSetAsLong(Value: Long);
begin
  if IsNullable then
    IsNull := False;

  FXSQLVAR^.sqltype := SQL_LONG or (FXSQLVAR^.sqltype and 1);
  FXSQLVAR^.sqllen := SizeOf(Long);
  FXSQLVAR^.sqlscale := 0;
  with FStatement.FClientAPI do
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
  PLong(FXSQLVAR^.sqldata)^ := Value;
  FModified := True;
end;

procedure TIBXSQLParam.SetAsLong(Value: Long);
var
  i: Integer;
begin
  if FUniqueName then
     xSetAsLong(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsLong(Value);
end;

procedure TIBXSQLParam.xSetAsPointer(Value: Pointer);
begin
  if IsNullable and (Value = nil) then
    IsNull := True
  else begin
    IsNull := False;
    FXSQLVAR^.sqltype := SQL_TEXT or (FXSQLVAR^.sqltype and 1);
    Move(Value^, FXSQLVAR^.sqldata^, FXSQLVAR^.sqllen);
  end;
  FModified := True;
end;

procedure TIBXSQLParam.SetAsPointer(Value: Pointer);
var
  i: Integer;
begin
    if FUniqueName then
       xSetAsPointer(Value)
    else
    for i := 0 to FParent.FCount - 1 do
      if FParent[i].FName = FName then
         FParent[i].xSetAsPointer(Value);
end;

procedure TIBXSQLParam.xSetAsQuad(Value: TISC_QUAD);
begin
  if IsNullable then
      IsNull := False;
  if (FXSQLVAR^.sqltype and (not 1) <> SQL_BLOB) and
     (FXSQLVAR^.sqltype and (not 1) <> SQL_ARRAY) then
    IBError(ibxeInvalidDataConversion, [nil]);
  FXSQLVAR^.sqllen := SizeOf(TISC_QUAD);
  with FStatement.FClientAPI do
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
  PISC_QUAD(FXSQLVAR^.sqldata)^ := Value;
  FModified := True;
end;

procedure TIBXSQLParam.SetAsQuad(Value: TISC_QUAD);
var
  i: Integer;
begin
  if FUniqueName then
     xSetAsQuad(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsQuad(Value);
end;

procedure TIBXSQLParam.xSetAsShort(Value: Short);
begin
  if IsNullable then
    IsNull := False;

  FXSQLVAR^.sqltype := SQL_SHORT or (FXSQLVAR^.sqltype and 1);
  FXSQLVAR^.sqllen := SizeOf(Short);
  FXSQLVAR^.sqlscale := 0;
  with FStatement.FClientAPI do
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
  PShort(FXSQLVAR^.sqldata)^ := Value;
  FModified := True;
end;

procedure TIBXSQLParam.SetAsShort(Value: Short);
var
  i: Integer;
begin
  if FUniqueName then
     xSetAsShort(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsShort(Value);
end;

procedure TIBXSQLParam.xSetAsString(Value: String);
var
   stype: Integer;
   ss: TStringStream;
   b: TFBBlob;

   procedure SetStringValue;
   begin
      if (FXSQLVAR^.sqlname = 'DB_KEY') or {do not localize}
         (FXSQLVAR^.sqlname = 'RDB$DB_KEY') then {do not localize}
        Move(Value[1], FXSQLVAR^.sqldata^, FXSQLVAR^.sqllen)
      else begin
        FXSQLVAR^.sqltype := SQL_TEXT or (FXSQLVAR^.sqltype and 1);
        FXSQLVAR^.sqllen := Length(Value);
        with FStatement.FClientAPI do
          IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen + 1);
        if (Length(Value) > 0) then
          Move(Value[1], FXSQLVAR^.sqldata^, FXSQLVAR^.sqllen);
      end;
      FModified := True;
   end;

begin
  if IsNullable then
    IsNull := False;

  stype := FXSQLVAR^.sqltype and (not 1);
  if (stype = SQL_TEXT) or (stype = SQL_VARYING) then
    SetStringValue
  else begin
    if (stype = SQL_BLOB) then
    begin
      ss := TStringStream.Create(Value);
      try
        b := TFBBlob.Create(FStatement.Attachment,FStatement.Transaction);
        try
          b.LoadFromStream(ss);
        finally
          b.Close;
        end;
      finally
        ss.Free;
      end;
    end
    else if Value = '' then
      IsNull := True
    else if (stype = SQL_TIMESTAMP) or (stype = SQL_TYPE_DATE) or
      (stype = SQL_TYPE_TIME) then
      xSetAsDateTime(StrToDateTime(Value))
    else
      SetStringValue;
  end;
end;

procedure TIBXSQLParam.SetAsString(Value: String);
var
   i: integer;
begin
  if FUniqueName then
     xSetAsString(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsString(Value);
end;

procedure TIBXSQLParam.xSetAsVariant(Value: Variant);
begin
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

procedure TIBXSQLParam.SetAsVariant(Value: Variant);
var
   i: integer;
begin
  if FUniqueName then
     xSetAsVariant(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetAsVariant(Value);
end;

procedure TIBXSQLParam.xSetIsNull(Value: Boolean);
begin
  if Value then
  begin
    if not IsNullable then
      IsNullable := True;

    if Assigned(FXSQLVAR^.sqlind) then
      FXSQLVAR^.sqlind^ := -1;
    FModified := True;
  end
  else
    if ((not Value) and IsNullable) then
    begin
      if Assigned(FXSQLVAR^.sqlind) then
        FXSQLVAR^.sqlind^ := 0;
      FModified := True;
    end;
end;

procedure TIBXSQLParam.SetIsNull(Value: Boolean);
var
  i: Integer;
begin
  if FUniqueName then
     xSetIsNull(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetIsNull(Value);
end;

procedure TIBXSQLParam.xSetIsNullable(Value: Boolean);
begin
  if (Value <> IsNullable) then
  begin
    if Value then
    begin
      FXSQLVAR^.sqltype := FXSQLVAR^.sqltype or 1;
      with FStatement.FClientAPI do
        IBAlloc(FXSQLVAR^.sqlind, 0, SizeOf(Short));
    end
    else
    begin
      FXSQLVAR^.sqltype := FXSQLVAR^.sqltype and (not 1);
      ReallocMem(FXSQLVAR^.sqlind, 0);
    end;
  end;
end;

constructor TIBXSQLParam.Create(Parent: TIBXSQLDA; aStatement: TFBStatement);
begin
  inherited Create(aStatement);
  FParent := TIBXINPUTSQLDA(Parent);
end;

procedure TIBXSQLParam.SetIsNullable(Value: Boolean);
var
  i: Integer;
begin
  if FUniqueName then
     xSetIsNullable(Value)
  else
  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
       FParent[i].xSetIsNullable(Value);
end;

function TIBXSQLParam.getModified: boolean;
begin
  Result := FModified;
end;

procedure TIBXSQLParam.xSetAsBoolean(AValue: boolean);
begin
  if IsNullable then
    IsNull := False;

  FXSQLVAR^.sqltype := SQL_BOOLEAN;
  FXSQLVAR^.sqllen := 1;
  FXSQLVAR^.sqlscale := 0;
  with FStatement.FClientAPI do
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
  if AValue then
    PByte(FXSQLVAR^.sqldata)^ := ISC_TRUE
  else
    PByte(FXSQLVAR^.sqldata)^ := ISC_FALSE;
  FModified := True;
end;

procedure TIBXSQLParam.SetAsInteger(AValue: Integer);
begin
  SetAsLong(AValue);
end;

procedure TIBXSQLParam.Clear;
begin
  IsNull := true;
end;

procedure TIBXSQLParam.SetName(Value: string);
var i: integer;
begin
  FName := Value;
  for i := 0 to FParent.Count - 1 do
    if (i <> FIndex) and (FParent.Vars[i].Name = FName) then
    begin
      FUniqueName := false;
      FParent.Vars[i].FUniqueName := false;
    end;
end;

{ TIBXSQLDA }
constructor TIBXSQLDA.Create(aStatement: TFBStatement; sqldaType: TIBXSQLDAType);
begin
  inherited Create;
  FStatement := aStatement;
  FClientAPI := aStatement.ClientAPI;
  FSize := 0;
  FUniqueRelationName := '';
  FInputSQLDA := sqldaType = daInput;
end;

destructor TIBXSQLDA.Destroy;
var
  i: Integer;
begin
  if FXSQLDA <> nil then
  begin
    for i := 0 to FSize - 1 do
    begin
      FreeMem(FXSQLVARs[i].FXSQLVAR^.sqldata);
      FreeMem(FXSQLVARs[i].FXSQLVAR^.sqlind);
      FXSQLVARs[i].Free ;
    end;
    FreeMem(FXSQLDA);
    FXSQLDA := nil;
    FXSQLVARs := nil;
  end;
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

function TIBXSQLDA.VarByName(Idx: String): TIBXSQLVAR;
begin
  result := GetXSQLVARByName(Idx);
  if result = nil then
    IBError(ibxeFieldNotFound, [Idx]);
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
      with FStatement.FClientAPI, FXSQLVARs[i].Data^ do
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
                  st := Copy(sBaseName, 1, 31 - j_len) + IntToStr(j)
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
      FStatement.FClientAPI.IBAlloc(FXSQLDA, OldSize, XSQLDA_LENGTH(FCount));
      SetLength(FXSQLVARs, FCount);
      FXSQLDA^.version := SQLDA_VERSION1;
      p := @FXSQLDA^.sqlvar[0];
      for i := 0 to FCount - 1 do
      begin
        if i >= FSize then
          FXSQLVARs[i] := CreateSQLVar;
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

{ TFBStatement }

procedure TFBStatement.InternalPrepare(DBHandle: TISC_DB_HANDLE; sql: string);
var
  RB: TSQLResultBuffer;
begin
  if (sql = '') then
    IBError(ibxeEmptyQuery, [nil]);
  try
    with FClientAPI do
    begin
      Call(isc_dsql_alloc_statement2(StatusVector, @DBHandle,
                                      @FHandle), True);
      Call(isc_dsql_prepare(StatusVector, @FTRHandle, @FHandle, 0,
                 PChar(sql), FSQLDialect, nil), True);
    end;
    { After preparing the statement, query the stmt type and possibly
      create a FSQLRecord "holder" }
    { Get the type of the statement }
    RB := TSQLResultBuffer.Create(self,isc_info_sql_stmt_type);
    try
      FSQLType := TIBSQLTypes(RB.GetValue(isc_info_sql_stmt_type));

    finally
      RB.Free;
    end;

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
                                       sSQLErrorSeparator + sql)
      else
        raise;
    end;
  end;
end;

function TFBStatement.InternalExecute(TRHandle: TISC_TR_HANDLE): IResults;
begin
  Result := nil;
  with FClientAPI do
  case FSQLType of
  SQLSelect:
    IBError(ibxeIsAExecuteProcedure,[]);

  SQLExecProcedure:
  begin
    Call(isc_dsql_execute2(StatusVector,
                        @TRHandle,
                        @FHandle,
                        SQLDialect,
                        FSQLParams.AsXSQLDA,
                        FSQLRecord.AsXSQLDA), True);
    Result := TIBXResults(FSQLRecord);
  end
  else
    Call(isc_dsql_execute(StatusVector,
                         @TRHandle,
                         @FHandle,
                         SQLDialect,
                         FSQLParams.AsXSQLDA), True);

  end;
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
    with FClientAPI do
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

constructor TFBStatement.Create(Attachment: TFBAttachment;
  Transaction: TFBTransaction; sql: string; SQLDialect: integer);
var DBHandle: TISC_DB_HANDLE;
    GUID : TGUID;
begin
  inherited Create;
  FClientAPI := Attachment.ClientAPI;
  FAttachment := Attachment;
  FTransaction := transaction;
  FOwner := Transaction;
  FOwner.RegisterObj(self);
  DBHandle := (Attachment as TFBAttachment).Handle;
  FTRHandle := (Transaction as TFBTransaction).Handle;
  FSQLDialect := SQLDialect;
  CreateGuid(GUID);
  FCursor := GUIDToString(GUID);
  FSQLParams := TIBXINPUTSQLDA.Create(self);
  FSQLRecord := TIBXResultSet.Create(self);
  InternalPrepare(DBHandle,sql);
end;

destructor TFBStatement.Destroy;
begin
  if assigned(FSQLParams) then
    FSQLParams.Free;
  if assigned(FSQLRecord) then
    FSQLRecord.Free;
  if assigned(FOwner) then
    FOwner.UnRegisterObj(self);
  inherited Destroy;
end;

procedure TFBStatement.Close;
var
  isc_res: ISC_STATUS;
begin
  try
    if (FHandle <> nil) and (SQLType = SQLSelect) and FOpen then
    with FClientAPI do
    begin
      isc_res := Call(
                   isc_dsql_free_statement(StatusVector, @FHandle, DSQL_close),
                   False);
      if (StatusVector^ = 1) and (isc_res > 0) and
        not getStatus.CheckStatusVector(
              [isc_bad_stmt_handle, isc_dsql_cursor_close_err]) then
        IBDatabaseError;
    end;
  finally
    FOpen := False;
  end;
end;

function TFBStatement.FetchNext: boolean;
var
  fetch_res: ISC_STATUS;
begin
  result := false;
  if not FOpen then
    IBError(ibxeSQLClosed, [nil]);
  with FClientAPI do
  begin
    { Go to the next record... }
    fetch_res :=
      Call(isc_dsql_fetch(StatusVector, @FHandle, SQLDialect, FSQLRecord.AsXSQLDA), False);
    if (fetch_res = 100) or (getStatus.CheckStatusVector([isc_dsql_cursor_err])) then
    begin
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
      result := true;
  end;
end;

function TFBStatement.GetStatus: IStatus;
begin
  Result := FClientAPI.Status;
end;

function TFBStatement.GetSQLParams: ISQLParams;
begin
  Result := FSQLParams;
end;

function TFBStatement.GetOutMetaData: IMetaData;
begin
  Result := TIBXOUTPUTSQLDA(FSQLRecord);
end;

function TFBStatement.GetPlan: String;
var
  RB: TSQLResultBuffer;
begin
  if (not (FSQLType in [SQLSelect, SQLSelectForUpdate,
       {TODO: SQLExecProcedure, }
       SQLUpdate, SQLDelete])) then
    result := ''
  else
  begin
    RB := TSQLResultBuffer.Create(self,isc_info_sql_get_plan,16384);
    try
      RB.GetString(isc_info_sql_get_plan,Result);
    finally
      RB.Free;
    end;
  end;
end;

function TFBStatement.GetRowsAffected(var InsertCount, UpdateCount,
  DeleteCount: integer): boolean;
var
  RB: TSQLResultBuffer;
begin
  InsertCount := 0;
  UpdateCount := 0;
  DeleteCount := 0;
  Result := true;
  begin
    RB := TSQLResultBuffer.Create(self,isc_info_sql_records);
    try
      case SQLType of
      SQLInsert, SQLUpdate: {Covers "Insert or Update" as well as individual update}
      begin
        InsertCount := RB.GetValue(isc_info_sql_records, isc_info_req_insert_count);
        UpdateCount := RB.GetValue(isc_info_sql_records, isc_info_req_update_count);
      end;

      SQLDelete:
        DeleteCount := RB.GetValue(isc_info_sql_records, isc_info_req_delete_count);

      SQLExecProcedure:
      begin
        InsertCount :=  RB.GetValue(isc_info_sql_records, isc_info_req_insert_count);
        UpdateCount :=  RB.GetValue(isc_info_sql_records, isc_info_req_update_count);
        DeleteCount :=  RB.GetValue(isc_info_sql_records, isc_info_req_delete_count);
      end
      else
        Result := false;
      end;
    finally
      RB.Free;
    end;
  end;
end;

function TFBStatement.GetSQLType: TIBSQLTypes;
begin
  Result := FSQLType;
end;

function TFBStatement.Execute: IResults;
begin
  Result := InternalExecute(FTRHandle);
end;

function TFBStatement.Execute(aTransaction: ITransaction): IResults;
begin
  Result := InternalExecute((aTransaction as TFBTransaction).Handle);
end;

function TFBStatement.OpenCursor: IResultSet;
begin
  if FSQLType <> SQLSelect then
    IBError(ibxeIsASelectStatement,[]);

  with FClientAPI do
  begin
    Call(isc_dsql_execute2(StatusVector,
                        @FTRHandle,
                        @FHandle,
                        SQLDialect,
                        FSQLParams.AsXSQLDA,
                        nil), True);
    Call(
      isc_dsql_set_cursor_name(StatusVector, @FHandle, PChar(FCursor), 0),
      True);
  end;
  FOpen := True;
  Result := FSQLRecord;
end;

end.

