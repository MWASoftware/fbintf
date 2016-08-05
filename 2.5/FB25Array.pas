unit FB25Array;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25Statement, FB25Attachment, FB25Transaction,
  FB25SQLData, IBHeader, FB25ClientAPI, IBExternals;

type
  TFBArray = class;

  { TFBArrayElement }

  TFBArrayElement = class(TSQLDataItem,ISQLElement)
  private
   FBufPtr: PChar;
   FArray: TFBArray;
   FArrayIntf: IArray;
  protected
   function GetAttachment: TFBAttachment; override;
   function GetTransaction: TFBTransaction; override;
   function GetSQLDialect: integer; override;
   procedure Changed; override;
   function SQLData: PChar; override;
   function GetDataLength: short; override;
  public
   constructor Create(anArray: TFBArray; P: PChar);
   constructor Copy(aElement: TFBArrayElement);
   function GetSQLType: short; override;
   function GetName: string; override;
   function GetScale: short; override;
   function GetSize: integer;
  end;

  { TFBArrayMetaData }

  TFBArrayMetaData = class(TInterfacedObject,IArrayMetaData)
  private
    FAttachment: TFBAttachment;
    FTransaction: TFBTransaction;
  protected
   FArrayDesc: TISC_ARRAY_DESC;
   function NumOfElements: integer;
  public
   constructor Create(aAttachment: TFBAttachment; aTransaction: TFBTransaction;
     relationName, columnName: string);

  public
   {IArrayMetaData}
   function GetSQLType: short;
   function GetTableName: string;
   function GetColumnName: string;
   function GetDimensions: integer;
   function GetBounds: TArrayBounds;
  end;


  { TFBArray }

  TFBArray = class(TFBArrayMetaData,IArray)
  private
    FBuffer: PChar;
    FBufSize: ISC_LONG;
    FIsNew: boolean;
    FLoaded: boolean;
    FArrayID: TISC_QUAD;
    FModified: boolean;
    FArrayElements: array of ISQLElement;
    FTransactionIntf: ITransaction;
    FSQLDialect: integer;
    procedure AllocateBuffer;
    procedure GetArraySlice;
    procedure PutArraySlice;
    function GetIndex(coords: array of integer): integer;
    function GetDataLength: short;
  public
    constructor Create(aField: TIBSQLData); overload;
    constructor Create(aField: IArrayMetaData); overload;
    destructor Destroy; override;
    function GetArrayID: TISC_QUAD;
    function GetSQLDialect: integer;
    procedure TransactionEnding(aTransaction: TFBTransaction);

   public
    {IArray}
    function GetElement(x: integer): ISQLElement; overload;
    function GetElement(x, y: integer): ISQLElement; overload;
    function GetElement(coords: TArrayCoords): ISQLElement; overload;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
  end;

implementation

uses FBErrorMessages;

{ TFBArrayElement }

function TFBArrayElement.GetAttachment: TFBAttachment;
begin
  Result := FArray.GetAttachment as TFBAttachment;
end;

function TFBArrayElement.GetTransaction: TFBTransaction;
begin
  Result := FArray.GetTransaction as TFBTransaction;
end;

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

constructor TFBArrayElement.Create(anArray: TFBArray; P: PChar);
begin
  inherited Create;
  FArray := anArray;
  FBufPtr := P;
end;

constructor TFBArrayElement.Copy(aElement: TFBArrayElement);
begin
  inherited Create;
  FArray := aElement.FArray;
  FBufPtr := aElement.FBufPtr;
  FArrayIntf := FArray;
end;

function TFBArrayElement.GetSQLType: short;
begin
  Result :=  FArray.GetSQLType;
end;

function TFBArrayElement.GetName: string;
begin
  Result := FArray.GetColumnName;
end;

function TFBArrayElement.GetScale: short;
begin
  Result :=  byte(FArray.FArrayDesc.array_desc_scale);
end;

function TFBArrayElement.GetSize: integer;
begin
  Result := GetDataLength;
end;

{TFBArrayMetaData}

constructor TFBArrayMetaData.Create(aAttachment: TFBAttachment;
  aTransaction: TFBTransaction; relationName, columnName: string);
begin
  inherited Create;
  FAttachment := aAttachment;
  FTransaction := aTransaction;

  with Firebird25ClientAPI do
    if isc_array_lookup_bounds(StatusVector,@(FAttachment.Handle),@(FTransaction.Handle),
        PChar(relationName),PChar(columnName),@FArrayDesc) > 0 then
          IBDatabaseError;
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
begin
  l := NumOfElements;
  FBufSize := GetDataLength * l;
  with Firebird25ClientAPI do
    IBAlloc(FBuffer,0,FBufSize);

  SetLength(FArrayElements, l);
  for i := 0 to l -1 do
    FArrayElements[i] := nil;
end;

procedure TFBArray.GetArraySlice;
begin
  if FIsNew or FLoaded then Exit;

  with Firebird25ClientAPI do
    if isc_array_get_slice(StatusVector,@(FAttachment.Handle),@(FTransaction.Handle),
                               @FArrayID, @FArrayDesc, Pointer(FBuffer), @FBufSize) > 0 then
      IBDatabaseError;
  FLoaded := true;
end;

procedure TFBArray.PutArraySlice;
begin
  if not FModified then Exit;

  with Firebird25ClientAPI do
    if isc_array_put_slice(StatusVector, @(FAttachment.Handle),@(FTransaction.Handle),
                               @FArrayID, @FArrayDesc,Pointer(FBuffer),@FBufSize) > 0 then
      IBDatabaseError;

  FIsNew := false;
  FModified := false;
  FLoaded := true;
end;

function TFBArray.GetIndex(coords: array of integer): integer;
var i: integer;
    Bounds: TArrayBounds;
    dimsize: array of integer; {no of elements in each dimension}
begin
  Result := 0;
  Bounds := GetBounds;
  SetLength(dimSize,Length(coords));
  if FArrayDesc.array_desc_flags = 0 {row major} then
  begin
    dimsize[0] := 1;
    for i := 0 to Length(coords) - 1  do
      dimsize[i+1] := dimsize[i] * (Bounds[i].UpperBound - Bounds[i].LowerBound + 1);
  end
  else
  begin
    {column major}
    dimsize[Length(coords)-1] := 1;
    for i := Length(coords) - 1  downto 0 do
      dimsize[i-1] := dimsize[i] * (Bounds[i].UpperBound - Bounds[i].LowerBound + 1);
  end;

  for i := 0 to Length(coords) - 1  do
    Result += dimsize[i]*(coords[i] - Bounds[i].LowerBound);
end;

function TFBArray.GetDataLength: short;
begin
  Result :=  FArrayDesc.array_desc_length;
  if GetSQLType = SQL_VARYING then
    Inc(Result);
end;

constructor TFBArray.Create(aField: TIBSQLData);
begin
  inherited Create(aField.GetAttachment,aField.GetTransaction,
                                 aField.getRelationName,aField.getSQLName);
  FTransaction.RegisterObj(self);
  FTransactionIntf := aField.GetTransaction;
  FIsNew := false;
  FArrayID := aField.AsQuad;
  FModified := false;
  FSQLDialect := aField.Parent.Statement.SQLDialect;
  AllocateBuffer;
end;

constructor TFBArray.Create(aField: IArrayMetaData);
begin
  with aField as TFBArrayMetaData do
    inherited Create(FAttachment,FTransaction,GetTableName,GetColumnName);
  FTransaction.RegisterObj(self);
  FTransactionIntf :=  (aField as TFBArrayMetaData).FTransaction;
  FIsNew := true;
  FModified := true;
  FSQLDialect := FAttachment.SQLDialect;
  AllocateBuffer;
end;

destructor TFBArray.Destroy;
begin
  if assigned(FTransaction) then
    FTransaction.UnRegisterObj(self);
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TFBArray.GetArrayID: TISC_QUAD;
begin
  PutArraySlice;
  Result := FArrayID;
end;

function TFBArray.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

procedure TFBArray.TransactionEnding(aTransaction: TFBTransaction);
begin
  if (aTransaction = FTransaction) and FModified and not FIsNew then
    PutArraySlice;
end;

function TFBArray.GetElement(x: integer): ISQLElement;
var P: PChar;
    ArrayElement: TFBArrayElement;
begin
   if GetDimensions <> 1 then
     IBError(ibxeInvalidArrayDimensions,[GetDimensions]);

   if (x < GetBounds[0].LowerBound) or (x > GetBounds[0].UpperBound) then
     IBError(ibxeInvalidSubscript,[x]);
   GetArraySlice;
   if FArrayElements[x] = nil then
   begin
     P := FBuffer + x*GetDataLength;
     ArrayElement := TFBArrayElement.Create(self,P);
     FArrayElements[x] := ArrayElement;
   end
   else
     ArrayElement := FArrayElements[x] as TFBArrayElement;
   Result := TFBArrayElement.Copy(ArrayElement);
end;

function TFBArray.GetElement(x, y: integer): ISQLElement;
var index: integer;
    coords: TArrayCoords;
    P: PChar;
    ArrayElement: TFBArrayElement;
begin
   if GetDimensions <> 2 then
     IBError(ibxeInvalidArrayDimensions,[GetDimensions]);

   GetArraySlice;
   SetLength(coords,2);
   coords[0] := x;
   coords[1] := y;
   index := GetIndex(coords);
   if FArrayElements[index] = nil then
   begin
     P := FBuffer + index*GetDataLength;
     ArrayElement := TFBArrayElement.Create(self,P);
     FArrayElements[index] := ArrayElement;
   end
   else
     ArrayElement := FArrayElements[index] as TFBArrayElement;
   Result := TFBArrayElement.Copy(ArrayElement);
end;

function TFBArray.GetElement(coords: TArrayCoords): ISQLElement;
var dims: integer;
    index: integer;
    P: PChar;
    ArrayElement: TFBArrayElement;
begin
  dims := Length(coords);
   if GetDimensions <> dims then
     IBError(ibxeInvalidArrayDimensions,[GetDimensions]);

   GetArraySlice;
   index := GetIndex(coords);
   if FArrayElements[index] = nil then
   begin
     P := FBuffer + index*GetDataLength;
     ArrayElement := TFBArrayElement.Create(self,P);
     FArrayElements[index] := ArrayElement;
   end
   else
     ArrayElement := FArrayElements[index] as TFBArrayElement;
   Result := TFBArrayElement.Copy(ArrayElement);
end;

function TFBArray.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TFBArray.GetTransaction: ITransaction;
begin
  Result := FTransaction;
end;

end.

