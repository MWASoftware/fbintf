unit FBArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, IBHeader, FBTransaction,
  FBSQLData,  FBClientAPI, IBExternals, FBActivityMonitor;

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
   FBufPtr: PChar;
   FArray: TFBArray;
  protected
   function GetSQLDialect: integer; override;
   procedure Changed; override;
   function SQLData: PChar; override;
   function GetDataLength: short; override;
   procedure SetDataLength(len: short); override;
  public
   constructor Create(anArray: TFBArray; P: PChar);
   function GetSQLType: short; override;
   function GetName: string; override;
   function GetScale: short; override;
   function GetSize: integer;
   function GetAsString: string; override;
   procedure SetAsString(Value: String); override;
  end;

  { TFBArrayMetaData }

  TFBArrayMetaData = class(TInterfaceParent,IArrayMetaData)
  protected
   FArrayDesc: TISC_ARRAY_DESC;
   procedure LoadMetaData(aAttachment: IAttachment; aTransaction: ITransaction;
               relationName, columnName: string); virtual; abstract;
   function NumOfElements: integer;
  public
   constructor Create(aAttachment: IAttachment; aTransaction: ITransaction;
     relationName, columnName: string);

  public
   {IArrayMetaData}
   function GetSQLType: short;
   function GetSQLTypeName: string;
   function GetScale: integer;
   function GetTableName: string;
   function GetColumnName: string;
   function GetDimensions: integer;
   function GetBounds: TArrayBounds;
  end;


  { TFBArray }

  TFBArray = class(TActivityReporter,IArray)
  private
    FMetaData: IArrayMetaData;
    FIsNew: boolean;
    FLoaded: boolean;
    FModified: boolean;
    FAttachment: IAttachment;
    FTransactionIntf: ITransaction;
    FSQLDialect: integer;
    FOffsets: array of integer;
    FElement: TFBArrayElement;
    FElementIntf: IUnknown;
    FElementSize: integer;
    procedure GetArraySlice;
    procedure PutArraySlice(Force: boolean=false);
    function GetOffset(index: array of integer): PChar;
    function GetDataLength: short;
  protected
    FBuffer: PChar;
    FBufSize: ISC_LONG;
    FArrayID: TISC_QUAD;
    procedure AllocateBuffer; virtual;
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
    procedure TransactionEnding(aTransaction: ITransaction; Force: boolean);

   public
    {IArray}
    function GetArrayID: TISC_QUAD;
    function GetMetaData: IArrayMetaData;
    function GetAsInteger(index: array of integer): integer;
    function GetAsBoolean(index: array of integer): boolean;
    function GetAsCurrency(index: array of integer): Currency;
    function GetAsInt64(index: array of integer): Int64;
    function GetAsDateTime(index: array of integer): TDateTime;
    function GetAsDouble(index: array of integer): Double;
    function GetAsFloat(index: array of integer): Float;
    function GetAsLong(index: array of integer): Long;
    function GetAsShort(index: array of integer): Short;
    function GetAsString(index: array of integer): String;
    function GetAsVariant(index: array of integer): Variant;
    procedure SetAsInteger(index: array of integer; AValue: integer);
    procedure SetAsBoolean(index: array of integer; AValue: boolean);
    procedure SetAsCurrency(index: array of integer; Value: Currency);
    procedure SetAsInt64(index: array of integer; Value: Int64);
    procedure SetAsDate(index: array of integer; Value: TDateTime);
    procedure SetAsLong(index: array of integer; Value: Long);
    procedure SetAsTime(index: array of integer; Value: TDateTime);
    procedure SetAsDateTime(index: array of integer; Value: TDateTime);
    procedure SetAsDouble(index: array of integer; Value: Double);
    procedure SetAsFloat(index: array of integer; Value: Float);
    procedure SetAsShort(index: array of integer; Value: Short);
    procedure SetAsString(index: array of integer; Value: String);
    procedure SetAsVariant(index: array of integer; Value: Variant);
    procedure SetBounds(dim, UpperBound, LowerBound: integer);
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
  end;

implementation

uses FBMessages;

{ TFBArrayElement }

function TFBArrayElement.GetSQLDialect: integer;
begin
  Result := FArray.GetSQLDialect;
end;

procedure TFBArrayElement.Changed;
begin
  inherited Changed;
  FArray.FModified := true;
end;

function TFBArrayElement.SQLData: PChar;
begin
  Result := FBufPtr;
end;

function TFBArrayElement.GetDataLength: short;
begin
  Result :=  FArray.GetDataLength
end;

procedure TFBArrayElement.SetDataLength(len: short);
begin
  if len > GetDataLength then
    IBError(ibxeArrayElementOverFlow,[nil]);
end;

constructor TFBArrayElement.Create(anArray: TFBArray; P: PChar);
begin
  inherited Create;
  FArray := anArray;
  FBufPtr := P;
end;

function TFBArrayElement.GetSQLType: short;
begin
  Result :=  FArray.FMetaData.GetSQLType;
end;

function TFBArrayElement.GetName: string;
begin
  Result := FArray.FMetaData.GetColumnName;
end;

function TFBArrayElement.GetScale: short;
begin
  Result := FArray.FMetaData.GetScale;
end;

function TFBArrayElement.GetSize: integer;
begin
  Result := GetDataLength;
end;

function TFBArrayElement.GetAsString: string;
begin
  case GetSQLType of
  SQL_VARYING:
      Result := strpas(FBufPtr);
  SQL_TEXT:
      SetString(Result,FBufPtr,GetDataLength);
  else
    Result := inherited GetAsString;
  end;
end;

procedure TFBArrayElement.SetAsString(Value: String);
var len: integer;
    ElementSize: integer;
begin
  case GetSQLType of
  SQL_VARYING:
    begin
      len := Length(Value);
      ElementSize := GetDataLength;
      if len > ElementSize - 2 then len := ElementSize - 2;
      Move(Value[1],FBufPtr^,len);
      if Len < ElementSize - 2 then
        (FBufPtr+len)^ := #0
    end;
  SQL_TEXT:
    begin
      ElementSize := GetDataLength;
      FillChar(FBufPtr^,ElementSize,' ');
      len := Length(Value);
      if len > ElementSize - 1 then len := ElementSize - 1;
      Move(Value[1],FBufPtr^,len);
    end;
  else
    inherited SetAsString(Value);
  end;
end;

{TFBArrayMetaData}

constructor TFBArrayMetaData.Create(aAttachment: IAttachment;
  aTransaction: ITransaction; relationName, columnName: string);
begin
  inherited Create;
  LoadMetaData(aAttachment,aTransaction,relationName, columnName);
end;

function TFBArrayMetaData.GetSQLType: short;
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
  end;
end;

function TFBArrayMetaData.GetSQLTypeName: string;
begin
  Result := TSQLDataItem.GetSQLTypeName(GetSQLType);
end;

function TFBArrayMetaData.GetScale: integer;
begin
  Result := byte(FArrayDesc.array_desc_scale);
end;

function TFBArrayMetaData.GetTableName: string;
begin
  Result := strpas(FArrayDesc.array_desc_relation_name);
end;

function TFBArrayMetaData.GetColumnName: string;
begin
  Result := strpas(FArrayDesc.array_desc_field_name);
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

function TFBArrayMetaData.NumOfElements: integer;
var i: integer;
    Bounds: TArrayBounds;
begin
  Result := 1;
  Bounds := GetBounds;
  for i := 0 to Length(Bounds) - 1 do
    Result *= (Bounds[i].UpperBound - Bounds[i].LowerBound + 1);
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
      FElementSize += 2;
    SQL_TEXT:
      FElementSize += 1;
    end;
    FBufSize := FElementSize * l;

    with FirebirdClientAPI do
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

procedure TFBArray.GetArraySlice;
begin
  if FIsNew or FLoaded then Exit;
  InternalGetSlice;
  FLoaded := true;
end;

procedure TFBArray.PutArraySlice(Force: boolean);
begin
  if not FModified then Exit;

  InternalPutSlice(Force);
end;

function TFBArray.GetOffset(index: array of integer): PChar;
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

    FlatIndex += FOffsets[i]*(index[i] - Bounds[i].LowerBound);
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
  FTransactionIntf :=  aTransaction;
  FIsNew := true;
  FModified := true;
  FSQLDialect := aAttachment.GetSQLDialect;
  AllocateBuffer;
  FElement := TFBArrayElement.Create(self,FBuffer);
  FElementIntf := FElement;
end;

constructor TFBArray.Create(aAttachment: IAttachment; aTransaction: TFBTransaction;
  aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aTransaction);
  FMetaData := aField;
  FArrayID := ArrayID;
  FAttachment := aAttachment;
  FTransactionIntf :=  aTransaction;
  FIsNew := false;
  FModified := false;
  FSQLDialect := aAttachment.GetSQLDialect;
  AllocateBuffer;
  FElement := TFBArrayElement.Create(self,FBuffer);
  FElementIntf := FElement;
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

function TFBArray.GetMetaData: IArrayMetaData;
begin
  Result := FMetaData;
end;

function TFBArray.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

procedure TFBArray.TransactionEnding(aTransaction: ITransaction; Force: boolean
  );
begin
  if (aTransaction = FTransactionIntf) and FModified and not FIsNew then
    PutArraySlice(Force);
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

function TFBArray.GetAsString(index: array of integer): String;
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

procedure TFBArray.SetAsDateTime(index: array of integer; Value: TDateTime);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsDateTime(Value);
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

procedure TFBArray.SetAsString(index: array of integer; Value: String);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsString(Value);
end;

procedure TFBArray.SetAsVariant(index: array of integer; Value: Variant);
begin
  FElement.FBufPtr := GetOffset(index);
  FElement.SetAsVariant(Value);
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

end.

