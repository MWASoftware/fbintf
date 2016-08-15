unit FB25SQLData;

{$mode objfpc}{$H+}

{ This Unit was hacked out of the IBSQL unit and defines a class used as the
  base for interfaces accessing SQLDAVar data and Array Elements. The abstract
  methods are used to customise for an SQLDAVar or Array Element. The empty
  methods are needed for SQL parameters only. The string getters and setters
  are virtual as SQLVar and Array encodings of string data is different.}

interface

uses
  Classes, SysUtils, IBExternals, IBHeader, IB, FBTypes, FB25Attachment, FB25Transaction;

type

  { TSQLDataItem }

  TSQLDataItem = class(TInterfacedObject)
  private
     function AdjustScale(Value: Int64; aScale: Integer): Double;
     function AdjustScaleToInt64(Value: Int64; aScale: Integer): Int64;
     function AdjustScaleToCurrency(Value: Int64; aScale: Integer): Currency;
     procedure SetAsInteger(AValue: Integer);
  protected
     procedure CheckActive; virtual;
     function GetAttachment: TFBAttachment; virtual; abstract;
     function GetTransaction: TFBTransaction; virtual; abstract;
     function GetSQLDialect: integer; virtual; abstract;
     procedure Changed; virtual;
     function SQLData: PChar; virtual; abstract;
     function GetDataLength: short; virtual; abstract;
     procedure SetScale(aValue: short); virtual;
     procedure SetDataLength(len: short); virtual;
     procedure SetSQLType(aValue: short); virtual;
     property DataLength: short read GetDataLength write SetDataLength;

  public
     function GetSQLType: short; virtual; abstract;
     function GetSQLTypeName: string; overload;
     class function GetSQLTypeName(SQLType: short): string; overload;
     function GetName: string; virtual; abstract;
     function GetScale: short; virtual; abstract;
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
     function GetAsString: String; virtual;
     function GetIsNull: Boolean; virtual;
     function getIsNullable: boolean; virtual;
     function GetAsVariant: Variant;
     function GetAsBlob: IBlob; virtual;
     function GetModified: boolean; virtual;
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
     procedure SetAsString(Value: String); virtual;
     procedure SetAsVariant(Value: Variant);
     procedure SetAsBlob(Value: IBlob);
     procedure SetIsNull(Value: Boolean);  virtual;
     procedure SetIsNullable(Value: Boolean);  virtual;
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
     property AsShort: Short read GetAsShort write SetAsShort;
     property AsString: String read GetAsString write SetAsString;
     property AsVariant: Variant read GetAsVariant write SetAsVariant;
     property AsBlob: IBlob read GetAsBlob write SetAsBlob;
     property Modified: Boolean read getModified;
     property IsNull: Boolean read GetIsNull write SetIsNull;
     property IsNullable: Boolean read GetIsNullable write SetIsNullable;
     property Scale: short read GetScale write SetScale;
     property SQLType: short read GetSQLType write SetSQLType;
  end;

implementation

uses FBErrorMessages, FB25Blob, FB25ClientAPI, variants;

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

procedure TSQLDataItem.CheckActive;
begin
  //Do nothing by default
end;

procedure TSQLDataItem.Changed;
begin
  //Do nothing by default
end;

procedure TSQLDataItem.SetScale(aValue: short);
begin
  //Do nothing by default
end;

procedure TSQLDataItem.SetDataLength(len: short);
begin
  //Do nothing by default
end;

procedure TSQLDataItem.SetSQLType(aValue: short);
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
    with Firebird25ClientAPI do
    case SQLType of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToDate(AsString);
        except
          on E: EConvertError do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_TYPE_DATE:
      begin
        isc_decode_sql_date(PISC_DATE(SQLData), @tm_date);
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
        isc_decode_sql_time(PISC_TIME(SQLData), @tm_date);
        try
          msecs :=  (PISC_TIME(SQLData)^ mod 10000) div 10;
          result := EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                               Word(tm_date.tm_sec), msecs)
        except
          on E: EConvertError do begin
            IBError(ibxeInvalidDataConversion, [nil]);
          end;
        end;
      end;
      SQL_TIMESTAMP: begin
        isc_decode_date(PISC_QUAD(SQLData), @tm_date);
        try
          result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1),
                              Word(tm_date.tm_mday));
          msecs := (PISC_TIMESTAMP(SQLData)^.timestamp_time mod 10000) div 10;
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

function TSQLDataItem.GetAsShort: Short;
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
  ss: TStringStream;
  b: TFBBlob;
begin
  CheckActive;
  result := '';
  { Check null, if so return a default string }
  if not IsNull then
  with Firebird25ClientAPI do
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
          result := ss.DataString;
        finally
          ss.Free;
        end;
      end;
      SQL_TEXT, SQL_VARYING: begin
        sz := SQLData;
        if (SQLType = SQL_TEXT) then
          str_len := DataLength
        else begin
          str_len := isc_portable_integer(SQLData, 2);
          Inc(sz, 2);
        end;
        SetString(result, sz, str_len);
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
        result := DateTimeToStr(AsDateTime);
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

function TSQLDataItem.GetAsBlob: IBlob;
begin
  CheckActive;
  if SQLType <>  SQL_BLOB then
      IBError(ibxeInvalidDataConversion, [nil]);
  if IsNull then
    Result := nil
  else
    Result := TFBBlob.Create(GetAttachment,GetTransaction,AsQuad);
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
var
   tm_date: TCTimeStructure;
   Yr, Mn, Dy: Word;
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
  DecodeDate(Value, Yr, Mn, Dy);
  with tm_date do begin
    tm_sec := 0;
    tm_min := 0;
    tm_hour := 0;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  DataLength := SizeOf(ISC_DATE);
  with Firebird25ClientAPI do
    isc_encode_sql_date(@tm_date, PISC_DATE(SQLData));
  Changed;
end;

procedure TSQLDataItem.SetAsTime(Value: TDateTime);
var
  tm_date: TCTimeStructure;
  Hr, Mt, S, Ms: Word;
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
  DecodeTime(Value, Hr, Mt, S, Ms);
  with tm_date do begin
    tm_sec := S;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := 0;
    tm_mon := 0;
    tm_year := 0;
  end;
  DataLength := SizeOf(ISC_TIME);
  with Firebird25ClientAPI do
    isc_encode_sql_time(@tm_date, PISC_TIME(SQLData));
  if Ms > 0 then
    Inc(PISC_TIME(SQLData)^,Ms*10);
  Changed;
end;

procedure TSQLDataItem.SetAsDateTime(Value: TDateTime);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy, Hr, Mt, S, Ms: Word;
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_TIMESTAMP;
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
  DataLength := SizeOf(TISC_QUAD);
  with Firebird25ClientAPI do
    isc_encode_date(@tm_date, PISC_QUAD(SQLData));
  if Ms > 0 then
    Inc(PISC_TIMESTAMP(SQLData)^.timestamp_time,Ms*10);
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

procedure TSQLDataItem.SetAsShort(Value: Short);
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
   ss: TStringStream;
   b: TFBBlob;

   procedure SetStringValue;
   var len: integer;
   begin
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
  else begin
    if (stype = SQL_BLOB) then
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
    end
    else if Value = '' then
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

procedure TSQLDataItem.SetAsBlob(Value: IBlob);
begin
  CheckActive;
  Value.Close;
  AsQuad := Value.GetBlobID;
  Changed;
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


end.

