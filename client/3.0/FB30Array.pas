unit FB30Array;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Firebird, IB, FBArray, IBHeader, FB30Attachment,
  FB30Transaction, FBParamBlock;

type

  ISDLItem = interface
    function getParamType: byte;
    function getAsInteger: integer;
    function getAsString: string;
    function getAsByte: byte;
    procedure addByte(aValue: byte);
    procedure addShortInteger(aValue: integer);
    procedure setAsString(aValue: string);
    procedure setAsByte(aValue: byte);
    procedure SetAsInteger(aValue: integer);
    procedure SetAsShortInteger(aValue: integer);
    procedure SetAsTinyInteger(aValue: integer);
    property AsString: string read getAsString write setAsString;
    property AsByte: byte read getAsByte write setAsByte;
    property AsInteger: integer read getAsInteger write SetAsInteger;
  end;

  ISDL = interface
    function getCount: integer;
    function Add(ParamType: byte): ISDLItem;
    function getItems(index: integer): ISDLItem;
    function Find(ParamType: byte): ISDLItem;
    property Items[index: integer]: ISDLItem read getItems; default;
  end;

  TSDLItem = class(TParamBlockItem,ISDLItem);

  { TSDLBlock }

  TSDLBlock = class(TParamBlock,ISDL)
  public
    constructor Create;

  public
    {ISDL}
    function Add(ParamType: byte): ISDLItem;
    function getItems(index: integer): ISDLItem;
    function Find(ParamType: byte): ISDLItem;
end;

  { TFB30ArrayMetaData }

  TFB30ArrayMetaData = class(TFBArrayMetaData,IArrayMetaData)
  protected
    procedure LoadMetaData(aAttachment: IAttachment; aTransaction: ITransaction;
                   relationName, columnName: string); override;
  end;

  { TFB30Array }

  TFB30Array = class(TFBArray,IArray)
  private
    FAttachmentIntf: Firebird.IAttachment;
    FTransactionIntf: Firebird.ITransaction;
    FSDL: ISDL;
  protected
    procedure AllocateBuffer; override;
    procedure InternalGetSlice; override;
    procedure InternalPutSlice(Force: boolean); override;
  public
    constructor Create(aAttachment: TFBAttachment; aTransaction: TFB30Transaction; aField: IArrayMetaData); overload;
    constructor Create(aAttachment: TFBAttachment; aTransaction: TFB30Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD); overload;
 end;

implementation

uses FB30ClientAPI, FB30Statement;

const
  sGetArrayMetaData = 'Select F.RDB$FIELD_LENGTH, F.RDB$FIELD_SCALE, F.RDB$FIELD_TYPE, '+
                      'F.RDB$DIMENSIONS, RD.RDB$DIMENSION, RD.RDB$LOWER_BOUND, RD.RDB$UPPER_BOUND '+
                      'From RDB$FIELDS F JOIN RDB$RELATION_FIELDS RF '+
                      'On F.RDB$FIELD_NAME = RF.RDB$FIELD_SOURCE JOIN RDB$FIELD_DIMENSIONS FD '+
                      'On FD.RDB$FIELD_NAME = F.RDB$FIELD_NAME' +
                      'Where RF.RDB$RELATION_NAME = ? and RF.RDB$FIELD_NAME = ? Order by RD.RDB$DIMENSION asc';

{ TSDLBlock }

constructor TSDLBlock.Create;
begin
  inherited Create;
  FDataLength := 1;
  FBuffer^ := char(isc_sdl_version1);
end;

function TSDLBlock.Add(ParamType: byte): ISDLItem;
var Item: PParamBlockItemData;
begin
  Item := inherited Add(ParamType);
  Result := TSDLItem.Create(self,Item);
end;

function TSDLBlock.getItems(index: integer): ISDLItem;
var Item: PParamBlockItemData;
begin
  Item := inherited getItems(index);
  Result := TSDLItem.Create(self,Item);
end;

function TSDLBlock.Find(ParamType: byte): ISDLItem;
var Item: PParamBlockItemData;
begin
  Result := nil;
  Item := inherited Find(ParamType);
  if Item <> nil then
    Result := TSDLItem.Create(self,Item);
end;


{ TFB30ArrayMetaData }

{Assemble the array descriptor from the System Tables}

procedure TFB30ArrayMetaData.LoadMetaData(aAttachment: IAttachment;
  aTransaction: ITransaction; relationName, columnName: string);
var stmt: IStatement;
begin
  stmt := TFB30Statement.Create(aAttachment as TFBAttachment,aTransaction, sGetArrayMetaData ,aAttachment.GetSQLDialect);
  with stmt do
  begin
    SQLParams[0].AsString := RelationName;
    SQLParams[1].AsString := ColumnName;
    with OpenCursor do
    if FetchNext then
    begin
      FArrayDesc.array_desc_field_name := columnName;
      FArrayDesc.array_desc_relation_name := relationName;
      FArrayDesc.array_desc_length := Data[0].AsInteger;
      FArrayDesc.array_desc_scale := char(Data[1].AsInteger);
      FArrayDesc.array_desc_dtype := Data[2].AsInteger;
      FArrayDesc.array_desc_dimensions := Data[3].AsInteger;
      FArrayDesc.array_desc_flags := 0; {row major}
      repeat
        with FArrayDesc.array_desc_bounds[Data[4].AsInteger] do
        begin
          array_bound_lower := Data[5].AsInteger;
          array_bound_upper := Data[6].AsInteger;
        end;
      until not FetchNext;
    end;
  end;
end;

{ TFB30Array }

procedure TFB30Array.AllocateBuffer;

  procedure AddVarInteger(aValue: integer);
  begin
    if (aValue >= -128) and (aValue <= 127) then
      FSDL.Add(isc_sdl_tiny_integer).SetAsTinyInteger(aValue)
    else
    if (aValue >= -32768) and (aValue <= 32767) then
      FSDL.Add(isc_sdl_short_integer).SetAsShortInteger(aValue)
    else
      FSDL.Add(isc_sdl_long_integer).SetAsInteger(aValue);
  end;

var i: integer;
    SDLItem: ISDLItem;
begin
  inherited AllocateBuffer;
  {Now set up the SDL}

  FSDL := TSDLBlock.Create;
  with GetArrayDesc^ do
  {The following is based on gen_SDL from Firebird src/dsql/array.cpp}
  begin
    SDLItem := FSDL.Add(isc_sdl_struct);
    SDLItem.SetAsByte(array_desc_dtype);

    case array_desc_dtype of
    blr_short,blr_long,
    blr_int64,blr_quad:
        SDLItem.AddByte(byte(array_desc_scale));

    blr_text,blr_cstring, blr_varying:
        SDLItem.addShortInteger(array_desc_length);
    end;

    FSDL.Add(isc_sdl_relation).SetAsString(strpas(array_desc_relation_name));
    FSDL.Add(isc_sdl_field).SetAsString(strpas(array_desc_field_name));

    for i := 0 to array_desc_dimensions - 1 do
    begin
      if array_desc_bounds[i].array_bound_lower = 1 then
        FSDL.Add(isc_sdl_do1).SetAsTinyInteger(i)
      else
      begin
        FSDL.Add(isc_sdl_do2).SetAsTinyInteger(i);
        AddVarInteger(array_desc_bounds[i].array_bound_lower);
      end;
      AddVarInteger(array_desc_bounds[i].array_bound_upper);
    end;
  end;
end;

procedure TFB30Array.InternalGetSlice;
begin
  with Firebird30ClientAPI do
  begin
    FAttachmentIntf.getSlice(StatusIntf,FTransactionIntf,
                          @FArrayID,
                          (FSDL as TSDLBlock).getDataLength,
                          BytePtr((FSDL as TSDLBlock).getBuffer),
                          0,nil,
                          FBufSize,BytePtr(FBuffer)
                          );
    Check4DataBaseError;
  end;
  SignalActivity;
end;

procedure TFB30Array.InternalPutSlice(Force: boolean);
begin
  with Firebird30ClientAPI do
  begin
    FAttachmentIntf.putSlice(StatusIntf,FTransactionIntf, @FArrayID,
                          (FSDL as TSDLBlock).getDataLength,
                          BytePtr((FSDL as TSDLBlock).getBuffer),
                          0,nil,
                          FBufSize,BytePtr(FBuffer)
                          );
    Check4DataBaseError;
  end;
  SignalActivity;
end;

constructor TFB30Array.Create(aAttachment: TFBAttachment;
  aTransaction: TFB30Transaction; aField: IArrayMetaData);
begin
  inherited Create(aAttachment,aTransaction,aField);
  FAttachmentIntf := aAttachment.AttachmentIntf;
  FTransactionIntf := aTransaction.TransactionIntf;
end;

constructor TFB30Array.Create(aAttachment: TFBAttachment;
  aTransaction: TFB30Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aAttachment,aTransaction,aField,ArrayID);
  FAttachmentIntf := aAttachment.AttachmentIntf;
  FTransactionIntf := aTransaction.TransactionIntf;
end;

end.

