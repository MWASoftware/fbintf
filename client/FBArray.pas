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
unit FBArray;
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
  Classes, SysUtils, IBHeader, IB,  FBTransaction,
  FBSQLData,  FBClientAPI, IBExternals, FBActivityMonitor, FmtBCD;

(*

COMMENTS (copied from IBPP)

1)
For an array column of type CHAR(X), the internal type returned or expected is blr_text.
In such case, the byte array received or submitted to get/put_slice is formatted in
elements of X bytes, which correspond to what is reported in array_desc_length.
The elements are not '\0' terminated but are right-padded with spaces ' '.

2)
For an array column of type VARCHAR(X), the internal type is blr_varying.
The underlying format is rather curious and different than what is used in XSQLDA.
The element size is reported in array_desc_length as X.
Yet each element of the byte array is expected to be of size X+2 (just as if we were
to stuff a short in the first 2 bytes to store the length (as is done with XSQLDA).
No. The string of X characters maximum has to be stored in the chunks of X+2 bytes as
a zero-terminated c-string. Note that the buffer is indeed one byte too large.
Internally, the API probably convert in-place in these chunks the zero-terminated string
to a variable-size string with a short in front and the string data non zero-terminated
behind.

*)

type
  TFBArray = class;

  { TFBArrayElement }

  TFBArrayElement = class(TSQLDataItem)
  private
   FBufPtr: PByte;
   FArray: TFBArray;
  protected
   function GetAttachment: IAttachment; override;
   function GetSQLDialect: integer; override;
   procedure Changing; override;
   procedure Changed; override;
   function SQLData: PByte; override;
   function GetDataLength: cardinal; override;
   function GetCodePage: TSystemCodePage; override;
   function getCharSetID: cardinal; override;
   procedure SetDataLength(len: cardinal); override;
   procedure SetSQLType(aValue: cardinal); override;
  public
   constructor Create(anArray: TFBArray; P: PByte);
   function GetSQLType: cardinal; override;
   function GetName: AnsiString; override;
   function GetScale: integer; override;
   function GetSize: integer;
   function GetCharSetWidth: integer; override;
   function GetAsString: AnsiString; override;
   procedure SetAsLong(Value: Long); override;
   procedure SetAsShort(Value: Short); override;
   procedure SetAsInt64(Value: Int64); override;
   procedure SetAsString(Value: AnsiString); override;
   procedure SetAsDouble(Value: Double); override;
   procedure SetAsFloat(Value: Float); override;
   procedure SetAsCurrency(Value: Currency); override;
   procedure SetAsBcd(aValue: tBCD); override;
   procedure SetAsNumeric(Value: IFBNumeric); override;
  end;

  { TFBArrayMetaData }

  TFBArrayMetaData = class(TFBInterfacedObject,IArrayMetaData)
  private
   function GetDType(SQLType: cardinal): UChar;
  protected
   FArrayDesc: TISC_ARRAY_DESC;
   FCharSetID: integer;
   FAttachment: IAttachment;
   procedure LoadMetaData(aAttachment: IAttachment; aTransaction: ITransaction;
               relationName, columnName: AnsiString); virtual; abstract;
   function NumOfElements: integer;
  public
   constructor Create(aAttachment: IAttachment; aTransaction: ITransaction;
     relationName, columnName: AnsiString); overload;
   constructor Create(aAttachment: IAttachment;SQLType: cardinal; tableName: AnsiString; columnName: AnsiString;
     Scale: integer; size: cardinal; charSetID: cardinal;
     dimensions: cardinal; bounds: TArrayBounds); overload;
   function GetCodePage: TSystemCodePage; virtual; abstract;

  public
   {IArrayMetaData}
   function GetSQLType: cardinal;
   function GetSQLTypeName: AnsiString;
   function GetScale: integer;
   function GetSize: cardinal;
   function GetCharSetID: cardinal; virtual; abstract;
   function GetCharSetWidth: integer; virtual; abstract;
   function GetTableName: AnsiString;
   function GetColumnName: AnsiString;
   function GetDimensions: integer;
   function GetBounds: TArrayBounds;
  end;


  { TFBArray }

  TFBArray = class(TActivityReporter,IArray,ITransactionUser)
  private
    FConnectionCodePage: TSystemCodePage;
    FFirebirdClientAPI: TFBClientAPI;
    FMetaData: IArrayMetaData;
    FIsNew: boolean;
    FLoaded: boolean;
    FModified: boolean;
    FAttachment: IAttachment;
    FTransactionIntf: ITransaction;
    FTransactionSeqNo: integer;
    FSQLDialect: integer;
    FOffsets: array of integer;
    FElement: TFBArrayElement;
    FElementIntf: IUnknown;
    FElementSize: integer;
    FEventHandlers: array of TArrayEventHandler;
    procedure GetArraySlice;
    procedure PutArraySlice(Force: boolean=false);
    function GetOffset(index: array of integer): PByte;
    function GetDataLength: short;
  protected
    FBuffer: PByte;
    FBufSize: ISC_LONG;
    FArrayID: TISC_QUAD;
    procedure AllocateBuffer; virtual;
    procedure Changing; virtual;
    procedure Changed;  virtual;
    function GetArrayDesc: PISC_ARRAY_DESC;
    procedure InternalGetSlice; virtual; abstract;
    procedure InternalPutSlice(Force: boolean); virtual; abstract;
  public
    constructor Create(aAttachment: IAttachment; aTransaction: TFBTransaction;
      aField: IArrayMetaData); overload;
    constructor Create(aAttachment: IAttachment; aTransaction: TFBTransaction;
      aField: IArrayMetaData; ArrayID: TISC_QUAD); overload;
    destructor Destroy; override;
    function GetSQLDialect: integer;
    property ConnectionCodePage: TSystemCodePage read FConnectionCodePage;

  public
    {ITransactionUser}
    procedure TransactionEnding(aTransaction: ITransaction; Force: boolean);

   public
    {IArrayMetaData}
    function GetSQLType: cardinal;
    function GetSQLTypeName: AnsiString;
    function GetScale: integer;
    function GetSize: cardinal;
    function GetCharSetID: cardinal;
    function GetCharSetWidth: integer;
    function GetTableName: AnsiString;
    function GetColumnName: AnsiString;
    function GetDimensions: integer;
    function GetBounds: TArrayBounds;
    {IArray}
    function GetArrayID: TISC_QUAD;
    procedure Clear;
    function IsEmpty: boolean;
    procedure PreLoad;
    procedure CancelChanges;
    procedure SaveChanges;
    function GetAsInteger(index: array of integer): integer;
    function GetAsBoolean(index: array of integer): boolean;
    function GetAsCurrency(index: array of integer): Currency;
    function GetAsInt64(index: array of integer): Int64;
    function GetAsDateTime(index: array of integer): TDateTime; overload;
    procedure GetAsDateTime(index: array of integer; var aDateTime: TDateTime; var dstOffset: smallint; var aTimezoneID: TFBTimeZoneID); overload;
    procedure GetAsDateTime(index: array of integer; var aDateTime: TDateTime; var dstOffset: smallint; var aTimezone: AnsiString); overload;
    procedure GetAsTime(index: array of integer; var aTime: TDateTime; var dstOffset: smallint; var aTimezoneID: TFBTimeZoneID; OnDate: TDateTime); overload;
    procedure GetAsTime(index: array of integer; var aTime: TDateTime; var dstOffset: smallint; var aTimezone: AnsiString; OnDate: TDateTime); overload;
    function GetAsUTCDateTime(index: array of integer): TDateTime;
    function GetAsDouble(index: array of integer): Double;
    function GetAsFloat(index: array of integer): Float;
    function GetAsLong(index: array of integer): Long;
    function GetAsShort(index: array of integer): Short;
    function GetAsString(index: array of integer): AnsiString;
    function GetAsVariant(index: array of integer): Variant;
    function GetAsBCD(index: array of integer): tBCD;
    procedure SetAsInteger(index: array of integer; AValue: integer);
    procedure SetAsBoolean(index: array of integer; AValue: boolean);
    procedure SetAsCurrency(index: array of integer; Value: Currency);
    procedure SetAsInt64(index: array of integer; Value: Int64);
    procedure SetAsDate(index: array of integer; Value: TDateTime);
    procedure SetAsLong(index: array of integer; Value: Long);
    procedure SetAsTime(index: array of integer; Value: TDateTime); overload;
    procedure SetAsTime(index: array of integer; aValue: TDateTime; OnDate: TDateTime; aTimeZoneID: TFBTimeZoneID); overload;
    procedure SetAsTime(index: array of integer; aValue: TDateTime; OnDate: TDateTime; aTimeZone: AnsiString); overload;
    procedure SetAsDateTime(index: array of integer; Value: TDateTime); overload;
    procedure SetAsDateTime(index: array of integer; aValue: TDateTime; aTimeZoneID: TFBTimeZoneID); overload;
    procedure SetAsDateTime(index: array of integer; aValue: TDateTime; aTimeZone: AnsiString); overload;
    procedure SetAsUTCDateTime(index: array of integer; aUTCTime: TDateTime);
    procedure SetAsDouble(index: array of integer; Value: Double);
    procedure SetAsFloat(index: array of integer; Value: Float);
    procedure SetAsShort(index: array of integer; Value: Short);
    procedure SetAsString(index: array of integer; Value: AnsiString);
    procedure SetAsVariant(index: array of integer; Value: Variant);
    procedure SetAsBcd(index: array of integer; aValue: tBCD);
    procedure SetBounds(dim, UpperBound, LowerBound: integer);
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    procedure AddEventHandler(Handler: TArrayEventHandler);
    procedure RemoveEventHandler(Handler: TArrayEventHandler);
  end;

implementation

uses FBMessages, IBUtils, FBNumeric;

{ TFBArrayElement }

function TFBArrayElement.GetAttachment: IAttachment;
begin
  Result := FArray.GetAttachment;
end;

function TFBArrayElement.GetSQLDialect: integer;
begin
  Result := FArray.GetSQLDialect;
end;

procedure TFBArrayElement.Changing;
begin
  inherited Changing;
  FArray.Changing;
end;

procedure TFBArrayElement.Changed;
begin
  inherited Changed;
  FArray.Changed;
end;

function TFBArrayElement.SQLData: PByte;
begin
  Result := FBufPtr;
end;

function TFBArrayElement.GetDataLength: cardinal;
begin
  Result :=  FArray.GetDataLength
end;

function TFBArrayElement.GetCodePage: TSystemCodePage;
begin
  Result := (FArray.FMetaData as TFBArrayMetaData).GetCodePage;
end;

function TFBArrayElement.getCharSetID: cardinal;
begin
  Result := (FArray.FMetaData as TFBArrayMetaData).GetCharSetID;
end;

procedure TFBArrayElement.SetDataLength(len: cardinal);
begin
  if len > GetDataLength then
    IBError(ibxeArrayElementOverFlow,[nil]);
end;

constructor TFBArrayElement.Create(anArray: TFBArray; P: PByte);
begin
  inherited Create(anArray.FFirebirdClientAPI);
  FArray := anArray;
  FBufPtr := P;
end;

function TFBArrayElement.GetSQLType: cardinal;
begin
  Result :=  FArray.FMetaData.GetSQLType;
end;

function TFBArrayElement.GetName: AnsiString;
begin
  Result := FArray.FMetaData.GetColumnName;
end;

function TFBArrayElement.GetScale: integer;
begin
  Result := FArray.FMetaData.GetScale;
end;

function TFBArrayElement.GetSize: integer;
begin
  Result := GetDataLength;
end;

function TFBArrayElement.GetCharSetWidth: integer;
begin
  Result := FArray.FMetaData.GetCharSetWidth;
end;

function TFBArrayElement.GetAsString: AnsiString;
var rs: RawByteString;
begin
  case GetSQLType of
  SQL_VARYING:
    begin
      rs := strpas(PAnsiChar(FBufPtr));
      SetCodePage(rs,GetCodePage,false);
      Result := rs;
    end;
  SQL_TEXT:
    begin
      SetString(rs,PAnsiChar(FBufPtr),GetDataLength);
      SetCodePage(rs,GetCodePage,false);
      Result := rs;
    end
  else
    Result := inherited GetAsString;
  end;
end;

procedure TFBArrayElement.SetAsLong(Value: Long);
begin
  AsInt64 := Value;
end;

procedure TFBArrayElement.SetAsShort(Value: Short);
begin
  AsInt64 := Value;
end;

procedure TFBArrayElement.SetAsInt64(Value: Int64);
begin
  CheckActive;
  case GetSQLType of
  SQL_LONG:
    PLong(SQLData)^ := NumericFromRawValues(Value,getScale).getRawValue;
  SQL_SHORT:
    PShort(SQLData)^ := NumericFromRawValues(Value,getScale).getRawValue;
  SQL_INT64:
    PInt64(SQLData)^ := NumericFromRawValues(Value,getScale).getRawValue;
  SQL_TEXT, SQL_VARYING:
    SetAsString(IntToStr(Value));
  SQL_D_FLOAT,
  SQL_DOUBLE:
    PDouble(SQLData)^ := Value;
  SQL_FLOAT:
    PSingle(SQLData)^ := Value;
  else
    IBError(ibxeInvalidDataConversion, [nil]);
  end;
  Changed;
end;

procedure TFBArrayElement.SetAsString(Value: AnsiString);
var len: integer;
    ElementSize: integer;
    Int64Value: Int64;
    aScale: integer;
begin
  CheckActive;
  case GetSQLType of
  SQL_BOOLEAN:
    if AnsiCompareText(Value,STrue) = 0 then
      AsBoolean := true
    else
    if AnsiCompareText(Value,SFalse) = 0 then
      AsBoolean := false
    else
      IBError(ibxeInvalidDataConversion,[nil]);

  SQL_VARYING:
    begin
      Value := TransliterateToCodePage(Value,GetCodePage);
      len := Length(Value);
      ElementSize := GetDataLength;
      if len > ElementSize - 2 then
        len := ElementSize - 2;
      if Len > 0 then
        Move(Value[1],FBufPtr^,len);
      if Len < ElementSize - 2 then
        (FBufPtr+len)^ := 0;
      Changed;
    end;

  SQL_TEXT:
    begin
      Value := TransliterateToCodePage(Value,GetCodePage);
      ElementSize := GetDataLength;
      FillChar(FBufPtr^,ElementSize,' ');
      len := Length(Value);
      if len > ElementSize - 1 then len := ElementSize - 1;
      Move(Value[1],FBufPtr^,len);
      Changed;
    end;

  SQL_SHORT,
  SQL_LONG,
  SQL_INT64:
    if TryStrToNumeric(Value,Int64Value,aScale) then
      SetAsNumeric(NumericFromRawValues(Int64Value,aScale))
    else
      SetAsCurrency(StrToCurr(Value));

  SQL_D_FLOAT,
  SQL_DOUBLE,
  SQL_FLOAT:
    if TryStrToNumeric(Value,Int64Value,aScale) then
      SetAsDouble(NumericToDouble(Int64Value,AScale))
    else
      IBError(ibxeInvalidDataConversion,[nil]);

  SQL_DEC_FIXED,
  SQL_DEC16,
  SQL_DEC34,
  SQL_INT128:
    SetAsBCD(StrToBCD(Value));

  SQL_TIMESTAMP:
    SetAsDateTime(StrToDateTime(Value));

  SQL_TYPE_DATE:
    SetAsDate(StrToDateTime(Value));

  SQL_TYPE_TIME:
    SetAsTime(StrToDateTime(Value));

  else
    IBError(ibxeInvalidDataConversion,[nil]);
  end;
end;

procedure TFBArrayElement.SetAsDouble(Value: Double);
begin
  CheckActive;
  case GetSQLType of
  SQL_D_FLOAT,
  SQL_DOUBLE:
    PDouble(SQLData)^ := Value;
  SQL_FLOAT:
    PSingle(SQLData)^ := Value;
  SQL_SHORT:
    if Scale < 0 then
      PShort(SQLData)^ := SafeSmallInt(DoubleToNumeric(Value).AdjustScaleTo(Scale).getRawValue)
    else
      IBError(ibxeInvalidDataConversion, [nil]);
  SQL_LONG:
    if Scale < 0 then
      PLong(SQLData)^ := SafeInteger(DoubleToNumeric(Value).AdjustScaleTo(Scale).getRawValue)
    else
      IBError(ibxeInvalidDataConversion, [nil]);
  SQL_INT64:
    if Scale < 0 then
      PInt64(SQLData)^ := DoubleToNumeric(Value).AdjustScaleTo(Scale).getRawValue
    else
      IBError(ibxeInvalidDataConversion, [nil]);
  SQL_TEXT, SQL_VARYING:
    AsString := FloatToStr(Value);
  else
    IBError(ibxeInvalidDataConversion, [nil]);
  end;
  Changed;
end;

procedure TFBArrayElement.SetAsFloat(Value: Float);
begin
  AsDouble := Value;
end;

procedure TFBArrayElement.SetAsCurrency(Value: Currency);
begin
  CheckActive;
  if (GetSQLDialect < 3) or (SQLType <> SQL_INT64) then
    AsDouble := Value
  else
  begin
    if Scale = -4 then
      PCurrency(SQLData)^ := Value
    else
      PInt64(SQLData)^ := CurrToNumeric(Value).AdjustScaleTo(Scale).getRawValue;
    Changed;
  end
end;

procedure TFBArrayElement.SetAsBcd(aValue: tBCD);
var C: Currency;
begin
  CheckActive;
  with FirebirdClientAPI do
  case SQLType of
  SQL_DEC_FIXED,
  SQL_DEC16,
  SQL_DEC34:
    SQLDecFloatEncode(aValue,SQLType,SQLData);

  SQL_INT128:
    StrToInt128(Scale,BcdToStr(aValue),SQLData);

  else
    begin
      BCDToCurr(aValue,C);
      SetAsCurrency(C);
    end;
  end;
  Changed;
end;

procedure TFBArrayElement.SetAsNumeric(Value: IFBNumeric);
begin
  CheckActive;
  case GetSQLType of
  SQL_LONG:
      PLong(SQLData)^ := SafeInteger(Value.AdjustScaleTo(Scale).getRawValue);
  SQL_SHORT:
    PShort(SQLData)^ := SafeSmallInt(Value.AdjustScaleTo(Scale).getRawValue);
  SQL_INT64:
    PInt64(SQLData)^ := Value.AdjustScaleTo(Scale).getRawValue;
  SQL_TEXT, SQL_VARYING:
   SetAsString(Value.getAsString);
  SQL_D_FLOAT,
  SQL_DOUBLE:
    PDouble(SQLData)^ := Value.getAsDouble;
  SQL_FLOAT:
    PSingle(SQLData)^ := Value.getAsDouble;
  else
    IBError(ibxeInvalidDataConversion, [nil]);
  end;
  Changed;
end;

procedure TFBArrayElement.SetSQLType(aValue: cardinal);
begin
  if aValue <> GetSQLType then
    IBError(ibxeInvalidDataConversion, [nil]);
end;

{TFBArrayMetaData}

constructor TFBArrayMetaData.Create(aAttachment: IAttachment;
  aTransaction: ITransaction; relationName, columnName: AnsiString);
begin
  inherited Create;
  FAttachment := aAttachment;
  LoadMetaData(aAttachment,aTransaction,relationName, columnName);
end;

constructor TFBArrayMetaData.Create(aAttachment: IAttachment;
  SQLType: cardinal; tableName: AnsiString; columnName: AnsiString;
  Scale: integer; size: cardinal; charSetID: cardinal; dimensions: cardinal;
  bounds: TArrayBounds);
var i: integer;
begin
  inherited Create;
  FAttachment := aAttachment;
  with FArrayDesc do
  begin
    array_desc_dtype := GetDType(SQLType);
    array_desc_scale := Scale;
    array_desc_length := UShort(size);
    StrPLCopy(array_desc_field_name,columnName,sizeof(array_desc_field_name));
    StrPLCopy(array_desc_relation_name,tableName,sizeof(array_desc_relation_name));
    array_desc_dimensions := dimensions;
    array_desc_flags := 0;
    FCharSetID := charSetID;
    for i := 0 to Length(bounds) - 1 do
    begin
     array_desc_bounds[i].array_bound_lower := bounds[i].LowerBound;
     array_desc_bounds[i].array_bound_upper := bounds[i].UpperBound;
    end;
  end;
end;

function TFBArrayMetaData.GetSQLType: cardinal;
begin
  case  FArrayDesc.array_desc_dtype of
  blr_cstring,
  blr_cstring2,
  blr_text,blr_text2:
    Result := SQL_TEXT;
  blr_short:
    Result :=  SQL_SHORT;
  blr_long:
    Result := SQL_LONG;
  blr_quad, blr_blob_id:
    Result := SQL_QUAD;
  blr_float:
    Result := SQL_FLOAT;
  blr_double,blr_d_float:
    Result := SQL_D_FLOAT;
  blr_timestamp:
    Result := SQL_TIMESTAMP;
  blr_varying,blr_varying2:
    Result := SQL_VARYING;
  blr_sql_date:
    Result := SQL_TYPE_DATE;
  blr_sql_time:
    Result :=  SQL_TYPE_TIME;
  blr_int64:
    Result := SQL_INT64;
  blr_sql_time_tz:
    Result := SQL_TIME_TZ;
  blr_timestamp_tz:
    Result := SQL_TIMESTAMP_TZ;
  blr_ex_time_tz:
    Result := SQL_TIME_TZ_EX;
  blr_ex_timestamp_tz:
    Result := SQL_TIMESTAMP_TZ_EX;
  blr_dec64:
    Result := SQL_DEC16;
  blr_dec128:
    Result := SQL_DEC34;
  blr_int128:
    Result := SQL_INT128;
  end;
end;

function TFBArrayMetaData.GetSQLTypeName: AnsiString;
begin
  Result := TSQLDataItem.GetSQLTypeName(GetSQLType);
end;

function TFBArrayMetaData.GetScale: integer;
begin
  Result := FArrayDesc.array_desc_scale;
end;

function TFBArrayMetaData.GetSize: cardinal;
begin
  Result := FArrayDesc.array_desc_length;
end;

function TFBArrayMetaData.GetTableName: AnsiString;
begin
  with FArrayDesc do
   SetString(Result,PAnsiChar(@array_desc_relation_name),sizeof(array_desc_relation_name));
  Result := trim(Result);
end;

function TFBArrayMetaData.GetColumnName: AnsiString;
begin
  with FArrayDesc do
    SetString(Result,PAnsiChar(@FArrayDesc.array_desc_field_name),sizeof(array_desc_field_name));
  Result := trim(Result);
end;

function TFBArrayMetaData.GetDimensions: integer;
begin
  Result := FArrayDesc.array_desc_dimensions;
end;

function TFBArrayMetaData.GetBounds: TArrayBounds;
var i: integer;
begin
  SetLength(Result,GetDimensions);
  for i := 0 to GetDimensions - 1 do
  begin
    Result[i].UpperBound := FArrayDesc.array_desc_bounds[i].array_bound_upper;
    Result[i].LowerBound := FArrayDesc.array_desc_bounds[i].array_bound_lower;
  end;
end;

function TFBArrayMetaData.GetDType(SQLType: cardinal): UChar;
begin
  case  SQLType of
  SQL_TEXT:
    Result := blr_text;
  SQL_SHORT:
    Result :=  blr_short;
  SQL_LONG:
    Result := blr_long;
  SQL_QUAD:
    Result := blr_quad;
  SQL_FLOAT:
    Result := blr_float;
  SQL_D_FLOAT:
    Result := blr_double;
  SQL_TIMESTAMP:
    Result := blr_timestamp;
  SQL_VARYING:
    Result := blr_varying;
  SQL_TYPE_DATE:
    Result := blr_sql_date;
  SQL_TYPE_TIME:
    Result :=  blr_sql_time;
  SQL_INT64:
    Result := blr_int64;
  SQL_TIME_TZ:
    Result := blr_sql_time_tz;
  SQL_TIMESTAMP_TZ:
    Result := blr_timestamp_tz;
  SQL_TIME_TZ_EX:
    Result := blr_ex_time_tz;
  SQL_TIMESTAMP_TZ_EX:
    Result := blr_ex_timestamp_tz;
  SQL_DEC16:
    Result := blr_dec64;
  SQL_DEC34:
    Result := blr_dec128;
  SQL_INT128:
    Result := blr_int128;
  end;
end;

function TFBArrayMetaData.NumOfElements: integer;
var i: integer;
    Bounds: TArrayBounds;
begin
  Result := 1;
  Bounds := GetBounds;
  for i := 0 to Length(Bounds) - 1 do
    Result := Result * (Bounds[i].UpperBound - Bounds[i].LowerBound + 1);
end;


{ TFBArray }

procedure TFBArray.AllocateBuffer;
var i: integer;
    l: integer;
    Bounds: TArrayBounds;
    Dims: integer;
begin
  SetLength(FOffsets,0);
  FreeMem(FBuffer);
  FBuffer := nil;
  FLoaded := false;

  with FMetaData as TFBArrayMetaData do
  begin
    l := NumOfElements;
    FElementSize := FArrayDesc.array_desc_length;
    case GetSQLType of
    SQL_VARYING:
      FElementSize := FElementSize + 2;
    SQL_TEXT:
      FElementSize := FElementSize + 1;
    end;
    FBufSize := FElementSize * l;

    with FFirebirdClientAPI do
      IBAlloc(FBuffer,0,FBufSize);

    Dims := GetDimensions;
    SetLength(FOffsets,GetDimensions);
    Bounds := GetBounds;
    if FArrayDesc.array_desc_flags = 0 {row major} then
    begin
      FOffsets[0] := 1;
      for i := 0 to Dims - 2  do
        FOffsets[i+1] := FOffsets[i] * (Bounds[i].UpperBound - Bounds[i].LowerBound + 1);
    end
    else
    begin
      {column major}
      FOffsets[Dims-1] := 1;
      for i := Dims - 1  downto 1 do
        FOffsets[i-1] := FOffsets[i] * (Bounds[i].UpperBound - Bounds[i].LowerBound + 1);
    end;
  end;
end;

procedure TFBArray.Changing;
var i: integer;
begin
  for i := 0 to Length(FEventHandlers) - 1 do
    FEventHandlers[i](self,arChanging);
end;

procedure TFBArray.Changed;
var i: integer;
begin
  FModified := true;
  for i := 0 to Length(FEventHandlers) - 1 do
    FEventHandlers[i](self,arChanged);
end;

procedure TFBArray.GetArraySlice;
begin
  if FIsNew or FLoaded then Exit;
  InternalGetSlice;
  FLoaded := true;
end;

procedure TFBArray.PutArraySlice(Force: boolean);
begin
  if not FModified or not FTransactionIntf.InTransaction  or
    (FTransactionSeqNo < (FTransactionIntf as TFBTransaction).TransactionSeqNo) then Exit;

  InternalPutSlice(Force);
  FModified := false;
  FIsNew := false;
  FLoaded := true;
end;

function TFBArray.GetOffset(index: array of integer): PByte;
var i: integer;
    Bounds: TArrayBounds;
    FlatIndex: integer;
begin
  if FMetaData.GetDimensions <> Length(index) then
    IBError(ibxeInvalidArrayDimensions,[Length(index)]);

  FlatIndex := 0;
  Bounds := FMetaData.GetBounds;
  for i := 0 to Length(index) - 1  do
  begin
    if (index[i] < Bounds[i].LowerBound) or (index[i] > Bounds[i].UpperBound) then
      IBError(ibxeInvalidSubscript,[index[i],i]);

    FlatIndex := FlatIndex + FOffsets[i]*(index[i] - Bounds[i].LowerBound);
  end;
  Result := FBuffer + FlatIndex*FElementSize;
end;

function TFBArray.GetDataLength: short;
begin
  Result :=  FElementSize;
end;

function TFBArray.GetArrayDesc: PISC_ARRAY_DESC;
begin
  Result := @((FMetaData as TFBArrayMetaData).FArrayDesc);
end;

constructor TFBArray.Create(aAttachment: IAttachment; aTransaction: TFBTransaction; aField: IArrayMetaData);
begin
  inherited Create(aTransaction);
  FMetaData := aField;
  FAttachment := aAttachment;
  FFirebirdClientAPI := aTransaction.FirebirdAPI;
  FTransactionIntf :=  aTransaction;
  FTransactionSeqNo := aTransaction.TransactionSeqNo;
  FIsNew := true;
  FModified := false;
  FSQLDialect := aAttachment.GetSQLDialect;
  AllocateBuffer;
  FElement := TFBArrayElement.Create(self,FBuffer);
  FElementIntf := FElement;
  FConnectionCodePage := aAttachment.GetCodePage;
  Setlength(FEventHandlers,0);
end;

constructor TFBArray.Create(aAttachment: IAttachment; aTransaction: TFBTransaction;
  aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aTransaction);
  FMetaData := aField;
  FArrayID := ArrayID;
  FAttachment := aAttachment;
  FFirebirdClientAPI := aTransaction.FirebirdAPI;
  FTransactionIntf :=  aTransaction;
  FTransactionSeqNo := aTransaction.TransactionSeqNo;
  FIsNew := false;
  FModified := false;
  FSQLDialect := aAttachment.GetSQLDialect;
  AllocateBuffer;
  FElement := TFBArrayElement.Create(self,FBuffer);
  FElementIntf := FElement;
  FConnectionCodePage := aAttachment.GetCodePage;
  Setlength(FEventHandlers,0);
end;

destructor TFBArray.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TFBArray.GetArrayID: TISC_QUAD;
begin
  PutArraySlice;
  Result := FArrayID;
end;

procedure TFBArray.Clear;
begin
  FIsNew := true;
  FModified := false;
  FArrayID.gds_quad_high := 0;
  FArrayID.gds_quad_low := 0;
  AllocateBuffer;
end;

function TFBArray.IsEmpty: boolean;
begin
  Result := FIsNew and not FModified;
end;

procedure TFBArray.PreLoad;
begin
  GetArraySlice;
end;

procedure TFBArray.CancelChanges;
begin
  FModified := false;
  AllocateBuffer;
end;

procedure TFBArray.SaveChanges;
begin
  PutArraySlice;
end;

function TFBArray.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

procedure TFBArray.TransactionEnding(aTransaction: ITransaction; Force: boolean
  );
begin
  if ((aTransaction as TObject) = (FTransactionIntf as TObject)) and FModified and not FIsNew then
    PutArraySlice(Force);
end;

function TFBArray.GetSQLType: cardinal;
begin
  Result := FMetaData.GetSQLType;
end;

function TFBArray.GetSQLTypeName: AnsiString;
begin
  Result := FMetaData.GetSQLTypeName;
end;

function TFBArray.GetScale: integer;
begin
  Result := FMetaData.GetScale;
end;

function TFBArray.GetSize: cardinal;
begin
  Result := FMetaData.GetSize;
end;

function TFBArray.GetCharSetID: cardinal;
begin
  Result := FMetaData.GetCharSetID;
end;

function TFBArray.GetCharSetWidth: integer;
begin
  Result := FMetaData.GetCharSetWidth;
end;

function TFBArray.GetTableName: AnsiString;
begin
  Result := FMetaData.GetTableName;
end;

function TFBArray.GetColumnName: AnsiString;
begin
  Result := FMetaData.GetColumnName;
end;

function TFBArray.GetDimensions: integer;
begin
  Result := FMetaData.GetDimensions;
end;

function TFBArray.GetBounds: TArrayBounds;
begin
  Result := FMetaData.GetBounds;
end;

function TFBArray.GetAsInteger(index: array of integer): integer;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsLong;
end;

function TFBArray.GetAsBoolean(index: array of integer): boolean;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsBoolean;
end;

function TFBArray.GetAsCurrency(index: array of integer): Currency;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsCurrency;
end;

function TFBArray.GetAsInt64(index: array of integer): Int64;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsInt64;
end;

function TFBArray.GetAsDateTime(index: array of integer): TDateTime;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsDateTime;
end;

procedure TFBArray.GetAsDateTime(index: array of integer;
  var aDateTime: TDateTime; var dstOffset: smallint;
  var aTimezoneID: TFBTimeZoneID);
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  FElement.GetAsDateTime(aDateTime,dstOffset,aTimezoneID);
end;

procedure TFBArray.GetAsDateTime(index: array of integer;
  var aDateTime: TDateTime; var dstOffset: smallint; var aTimezone: AnsiString);
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  FElement.GetAsDateTime(aDateTime,dstOffset,aTimezone);
end;

procedure TFBArray.GetAsTime(index: array of integer; var aTime: TDateTime;
  var dstOffset: smallint; var aTimezoneID: TFBTimeZoneID; OnDate: TDateTime);
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  FElement.GetAsTime(aTime,dstOffset,aTimezoneID,OnDate);
end;

procedure TFBArray.GetAsTime(index: array of integer; var aTime: TDateTime;
  var dstOffset: smallint; var aTimezone: AnsiString; OnDate: TDateTime);
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  FElement.GetAsTime(aTime,dstOffset,aTimezone,OnDate);
end;

function TFBArray.GetAsUTCDateTime(index: array of integer): TDateTime;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsUTCDateTime;
end;

function TFBArray.GetAsDouble(index: array of integer): Double;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsDouble;
end;

function TFBArray.GetAsFloat(index: array of integer): Float;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsFloat;
end;

function TFBArray.GetAsLong(index: array of integer): Long;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsLong;
end;

function TFBArray.GetAsShort(index: array of integer): Short;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsShort;
end;

function TFBArray.GetAsString(index: array of integer): AnsiString;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsString;
end;

function TFBArray.GetAsVariant(index: array of integer): Variant;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsVariant;
end;

function TFBArray.GetAsBCD(index: array of integer): tBCD;
begin
  GetArraySlice;
  FElement.FBufPtr := GetOffset(index);
  Result := FElement.GetAsBCD;
end;

procedure TFBArray.SetAsInteger(index: array of integer; AValue: integer);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsLong(AValue);
end;

procedure TFBArray.SetAsBoolean(index: array of integer; AValue: boolean);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsBoolean(AValue);
end;

procedure TFBArray.SetAsCurrency(index: array of integer; Value: Currency);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsCurrency(Value);
end;

procedure TFBArray.SetAsInt64(index: array of integer; Value: Int64);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsInt64(Value);
end;

procedure TFBArray.SetAsDate(index: array of integer; Value: TDateTime);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsDate(Value);
end;

procedure TFBArray.SetAsLong(index: array of integer; Value: Long);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsLong(Value);
end;

procedure TFBArray.SetAsTime(index: array of integer; Value: TDateTime);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsTime(Value);
end;

procedure TFBArray.SetAsTime(index: array of integer; aValue: TDateTime; OnDate: TDateTime;
  aTimeZoneID: TFBTimeZoneID);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsTime(aValue,OnDate,aTimeZoneID);
end;

procedure TFBArray.SetAsTime(index: array of integer; aValue: TDateTime; OnDate: TDateTime;
  aTimeZone: AnsiString);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsTime(aValue,OnDate, aTimeZone);
end;

procedure TFBArray.SetAsDateTime(index: array of integer; Value: TDateTime);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsDateTime(Value);
end;

procedure TFBArray.SetAsDateTime(index: array of integer; aValue: TDateTime;
  aTimeZoneID: TFBTimeZoneID);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsDateTime(aValue,aTimeZoneID);
end;

procedure TFBArray.SetAsDateTime(index: array of integer; aValue: TDateTime;
  aTimeZone: AnsiString);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsDateTime(aValue,aTimeZone);
end;

procedure TFBArray.SetAsUTCDateTime(index: array of integer;
  aUTCTime: TDateTime);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsUTCDateTime(aUTCTime);
end;

procedure TFBArray.SetAsDouble(index: array of integer; Value: Double);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsDouble(Value);
end;

procedure TFBArray.SetAsFloat(index: array of integer; Value: Float);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsFloat(Value);
end;

procedure TFBArray.SetAsShort(index: array of integer; Value: Short);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsShort(Value);
end;

procedure TFBArray.SetAsString(index: array of integer; Value: AnsiString);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsString(Value);
end;

procedure TFBArray.SetAsVariant(index: array of integer; Value: Variant);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsVariant(Value);
end;

procedure TFBArray.SetAsBcd(index: array of integer; aValue: tBCD);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsBcd(aValue);
end;

procedure TFBArray.SetBounds(dim, UpperBound, LowerBound: integer);
begin
  with (FMetaData as TFBArrayMetaData) do
  begin
    if (dim < 0) or (dim > GetDimensions) then
      IBError(ibxeInvalidArrayDimensions,[dim]);

    if (UpperBound > FArrayDesc.array_desc_bounds[dim].array_bound_upper) or
       (LowerBound < FArrayDesc.array_desc_bounds[dim].array_bound_lower) or
       (UpperBound < FArrayDesc.array_desc_bounds[dim].array_bound_lower) or
       (LowerBound > FArrayDesc.array_desc_bounds[dim].array_bound_upper) then
      IBError(ibxArrayBoundsCantIncrease,[nil]);

    PutArraySlice;  {Save any changes}

    FArrayDesc.array_desc_bounds[dim].array_bound_upper := UpperBound;
    FArrayDesc.array_desc_bounds[dim].array_bound_lower := LowerBound;
  end;
  AllocateBuffer;
end;

function TFBArray.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TFBArray.GetTransaction: ITransaction;
begin
  Result := FTransactionIntf;
end;

procedure TFBArray.AddEventHandler(Handler: TArrayEventHandler);
begin
  SetLength(FEventHandlers,Length(FEventHandlers)+1);
  FEventHandlers[Length(FEventHandlers)-1] := Handler;
end;

procedure TFBArray.RemoveEventHandler(Handler: TArrayEventHandler);
var i,j : integer;
begin
  for i := Length(FEventHandlers) - 1 downto 0 do
    if (TMethod(FEventHandlers[i]).Code = TMethod(Handler).Code) and
      (TMethod(FEventHandlers[i]).Data = TMethod(Handler).Data) then
    begin
      for j := i to Length(FEventHandlers) - 2 do
        FEventHandlers[i] := FEventHandlers[i+1];
      SetLength(FEventHandlers,Length(FEventHandlers) - 1);
    end;
end;

end.

