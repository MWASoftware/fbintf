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
unit FB30Array;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, Firebird, IB, FBArray, IBHeader, FB30Attachment, FBClientAPI,
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

  { TFB30ArrayMetaData }

  TFB30ArrayMetaData = class(TFBArrayMetaData,IArrayMetaData)
  private
    FCodePage: TSystemCodePage;
  protected
    procedure LoadMetaData(aAttachment: IAttachment; aTransaction: ITransaction;
                   relationName, columnName: string); override;
  public
    function GetCharSetID: cardinal; override;
    function GetCodePage: TSystemCodePage; override;
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
    constructor Create(aAttachment: TFB30Attachment; aTransaction: TFB30Transaction; aField: IArrayMetaData); overload;
    constructor Create(aAttachment: TFB30Attachment; aTransaction: TFB30Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD); overload;
 end;

  TSDLItem = class(TParamBlockItem,ISDLItem);

  { TSDLBlock }

  TSDLBlock = class (specialize TCustomParamBlock<TSDLItem,ISDLItem>, ISDL)
  public
    constructor Create;
  end;

implementation

uses FBAttachment, FB30ClientAPI, FB30Statement;

const
  sGetArrayMetaData = 'Select F.RDB$FIELD_LENGTH, F.RDB$FIELD_SCALE, F.RDB$FIELD_TYPE, '+
                      'F.RDB$DIMENSIONS, FD.RDB$DIMENSION, FD.RDB$LOWER_BOUND, FD.RDB$UPPER_BOUND, '+
                      'F.RDB$CHARACTER_SET_ID '+
                      'From RDB$FIELDS F JOIN RDB$RELATION_FIELDS RF '+
                      'On F.RDB$FIELD_NAME = RF.RDB$FIELD_SOURCE JOIN RDB$FIELD_DIMENSIONS FD '+
                      'On FD.RDB$FIELD_NAME = F.RDB$FIELD_NAME ' +
                      'Where RF.RDB$RELATION_NAME = ? and RF.RDB$FIELD_NAME = ? Order by FD.RDB$DIMENSION asc';


{ TFB30ArrayMetaData }

{Assemble the array descriptor from the System Tables}

procedure TFB30ArrayMetaData.LoadMetaData(aAttachment: IAttachment;
  aTransaction: ITransaction; relationName, columnName: string);
var stmt: IStatement;
    CharWidth: integer;
begin
  RelationName := AnsiUpperCase(RelationName);
  ColumnName := AnsiUpperCase(ColumnName);
  stmt := TFB30Statement.Create(aAttachment as TFB30Attachment,aTransaction,
                               sGetArrayMetaData ,aAttachment.GetSQLDialect);
  with stmt do
  begin
    SQLParams[0].AsString := RelationName;
    SQLParams[1].AsString := ColumnName;
    with OpenCursor do
    if FetchNext then
    begin
      FillChar(FArrayDesc.array_desc_field_name,sizeof(FArrayDesc.array_desc_field_name),' ');
      FillChar(FArrayDesc.array_desc_relation_name,sizeof(FArrayDesc.array_desc_relation_name),' ');
      Move(columnName[1],FArrayDesc.array_desc_field_name,Length(columnName));
      Move(relationName[1],FArrayDesc.array_desc_relation_name,length(relationName));
      FArrayDesc.array_desc_length := Data[0].AsInteger;
      FArrayDesc.array_desc_scale := char(Data[1].AsInteger);
      FArrayDesc.array_desc_dtype := Data[2].AsInteger;
      FArrayDesc.array_desc_dimensions := Data[3].AsInteger;
      FArrayDesc.array_desc_flags := 0; {row major}
      FCharSetID := Data[7].AsInteger;
      if (FCharSetID > 1) and (aAttachment as TFB30Attachment).HasDefaultCharSet then
        FCharSetID := (aAttachment as TFB30Attachment).CharSetID;
      FCodePage := CP_NONE;
      FirebirdClientAPI.CharSetID2CodePage(FCharSetID,FCodePage);
      if (FArrayDesc.array_desc_dtype in [blr_text,blr_cstring, blr_varying]) and
        (FCharSetID = 0) then {This really shouldn't be necessary - but it is :(}
      with aAttachment as TFBAttachment do
      begin
        if HasDefaultCharSet  and FirebirdClientAPI.CharSetWidth(CharSetID,CharWidth) then
          FArrayDesc.array_desc_length *= CharWidth;
      end;
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

function TFB30ArrayMetaData.GetCharSetID: cardinal;
begin
  Result := FCharSetID;
end;

function TFB30ArrayMetaData.GetCodePage: TSystemCodePage;
begin
  Result := FCodePage;
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

    FSDL.Add(isc_sdl_relation).SetAsString(array_desc_relation_name);
    FSDL.Add(isc_sdl_field).SetAsString(array_desc_field_name);

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

    SDLItem := FSDL.Add(isc_sdl_element);
    SDLItem.AddByte(1);
    SDLItem := FSDL.Add(isc_sdl_scalar);
    SDLItem.AddByte(0);
    SDLItem.AddByte(array_desc_dimensions);
    for i := 0 to array_desc_dimensions - 1 do
    begin
      SDLItem := FSDL.Add(isc_sdl_variable);
      SDLItem.AddByte(i);
    end;
    FSDL.Add(isc_sdl_eoc);
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
    if not Force then
      Check4DataBaseError;
  end;
  SignalActivity;
end;

constructor TFB30Array.Create(aAttachment: TFB30Attachment;
  aTransaction: TFB30Transaction; aField: IArrayMetaData);
begin
  inherited Create(aAttachment,aTransaction,aField);
  FAttachmentIntf := aAttachment.AttachmentIntf;
  FTransactionIntf := aTransaction.TransactionIntf;
end;

constructor TFB30Array.Create(aAttachment: TFB30Attachment;
  aTransaction: TFB30Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aAttachment,aTransaction,aField,ArrayID);
  FAttachmentIntf := aAttachment.AttachmentIntf;
  FTransactionIntf := aTransaction.TransactionIntf;
end;

{ TSDLBlock }

constructor TSDLBlock.Create;
begin
  inherited Create;
  FDataLength := 1;
  FBuffer^ := char(isc_sdl_version1);
end;

end.

