(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  This file is part of the pure Pascal wire protocol implementation
 *  (no fbclient required) and is subject to the Initial Developer's
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
 *  The Initial Developer of the Original Code is MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBWireArray;

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
  Classes, SysUtils, IB, IBHeader, FBArray, FBTransaction, FBSDL,
  FBWireClientAPI, FBWireProtocol, FBWireMessage;

type
  { TFBWireArrayMetaData }

  TFBWireArrayMetaData = class(TFBArrayMetaData,IArrayMetaData)
  private
    FCodePage: TSystemCodePage;
    FCharSetWidth: integer;
  protected
    procedure LoadMetaData(aAttachment: IAttachment; aTransaction: ITransaction;
                   relationName, columnName: AnsiString); override;
  public
    function GetCharSetID: cardinal; override;
    function GetCodePage: TSystemCodePage; override;
    function GetCharSetWidth: integer; override;
  end;

  { TFBWireArray }

  TFBWireArray = class(TFBArray,IArray)
  private
    FWireAttachment: TObject;   {TFBWireAttachment - typed in the implementation}
    FWireAPI: TFBWireClientAPI;
    FSDL: ISDL;
    function GetConnection: TFBWireConnection;
    function GetTransactionHandle: integer;
    function GetSliceLayout: TWireSliceLayout;
    function SDLBytes: TBytes;
    function ArrayIDAsInt64: Int64;
  protected
    procedure AllocateBuffer; override;
    procedure InternalGetSlice; override;
    procedure InternalPutSlice(Force: boolean); override;
  public
    constructor Create(aAttachment: IAttachment; aTransaction: TFBTransaction;
                aField: IArrayMetaData); overload;
    constructor Create(aAttachment: IAttachment; aTransaction: TFBTransaction;
                aField: IArrayMetaData; ArrayID: TISC_QUAD); overload;
  end;

implementation

uses IBUtils, FBAttachment, FBWireAttachment, FBWireTransaction;

const
  {the same system table lookup the 3.0 provider uses}
  sGetArrayMetaData = 'Select F.RDB$FIELD_LENGTH, F.RDB$FIELD_SCALE, F.RDB$FIELD_TYPE, '+
                      'F.RDB$DIMENSIONS, FD.RDB$DIMENSION, FD.RDB$LOWER_BOUND, FD.RDB$UPPER_BOUND, '+
                      'F.RDB$CHARACTER_SET_ID '+
                      'From RDB$FIELDS F JOIN RDB$RELATION_FIELDS RF '+
                      'On F.RDB$FIELD_NAME = RF.RDB$FIELD_SOURCE JOIN RDB$FIELD_DIMENSIONS FD '+
                      'On FD.RDB$FIELD_NAME = F.RDB$FIELD_NAME ' +
                      'Where RF.RDB$RELATION_NAME = ? and RF.RDB$FIELD_NAME = ? ' +
                      'UNION '+
                      'Select F.RDB$FIELD_LENGTH, F.RDB$FIELD_SCALE, F.RDB$FIELD_TYPE, '+
                      'F.RDB$DIMENSIONS, FD.RDB$DIMENSION, FD.RDB$LOWER_BOUND, FD.RDB$UPPER_BOUND, '+
                      'F.RDB$CHARACTER_SET_ID '+
                      'From RDB$FIELDS F JOIN RDB$PROCEDURE_PARAMETERS PP '+
                      'On F.RDB$FIELD_NAME = PP.RDB$FIELD_SOURCE JOIN RDB$FIELD_DIMENSIONS FD '+
                      'On FD.RDB$FIELD_NAME = F.RDB$FIELD_NAME ' +
                      'Where PP.RDB$PROCEDURE_NAME = ? and PP.RDB$PARAMETER_NAME = ? '+
                      'Order by 5 asc';

{ TFBWireArrayMetaData }

{Assemble the array descriptor from the system tables - the query runs
 over the wire like any other, so this is the 3.0 provider's LoadMetaData
 with an IAttachment.OpenCursor in place of the direct statement}

procedure TFBWireArrayMetaData.LoadMetaData(aAttachment: IAttachment;
  aTransaction: ITransaction; relationName, columnName: AnsiString);
var RS: IResultSet;
    CharWidth: integer;
begin
  CharWidth := 0;
  RelationName := SafeAnsiUpperCase(RelationName);
  ColumnName := SafeAnsiUpperCase(ColumnName);
  RS := aAttachment.OpenCursor(aTransaction,sGetArrayMetaData,
          [RelationName,ColumnName,RelationName,ColumnName]);
  if RS.FetchNext then
  begin
    FillChar(FArrayDesc.array_desc_field_name,sizeof(FArrayDesc.array_desc_field_name),' ');
    FillChar(FArrayDesc.array_desc_relation_name,sizeof(FArrayDesc.array_desc_relation_name),' ');
    Move(columnName[1],FArrayDesc.array_desc_field_name,Length(columnName));
    Move(relationName[1],FArrayDesc.array_desc_relation_name,length(relationName));
    FArrayDesc.array_desc_length := RS[0].AsInteger;
    FArrayDesc.array_desc_scale := RS[1].AsInteger;
    FArrayDesc.array_desc_dtype := RS[2].AsInteger;
    FArrayDesc.array_desc_dimensions := RS[3].AsInteger;
    FArrayDesc.array_desc_flags := 0; {row major}
    FCharSetID := RS[7].AsInteger;
    if (FCharSetID > 1) and aAttachment.HasDefaultCharSet then
      FCharSetID := aAttachment.GetDefaultCharSetID;
    FCodePage := CP_NONE;
    FAttachment.CharSetID2CodePage(FCharSetID,FCodePage);
    FCharSetWidth := 1;
    FAttachment.CharSetWidth(FCharSetID,FCharSetWidth);
    if (FArrayDesc.array_desc_dtype in [blr_text,blr_cstring, blr_varying]) and
      (FCharSetID = 0) then {This really shouldn't be necessary - but it is :(}
    with aAttachment as TFBAttachment do
    begin
      if HasDefaultCharSet and FAttachment.CharSetWidth(CharSetID,CharWidth) then
        FArrayDesc.array_desc_length := FArrayDesc.array_desc_length * CharWidth;
    end;
    repeat
      with FArrayDesc.array_desc_bounds[RS[4].AsInteger] do
      begin
        array_bound_lower := RS[5].AsInteger;
        array_bound_upper := RS[6].AsInteger;
      end;
    until not RS.FetchNext;
  end;
  RS.Close;
end;

function TFBWireArrayMetaData.GetCharSetID: cardinal;
begin
  Result := FCharSetID;
end;

function TFBWireArrayMetaData.GetCodePage: TSystemCodePage;
begin
  Result := FCodePage;
end;

function TFBWireArrayMetaData.GetCharSetWidth: integer;
begin
  Result := FCharSetWidth;
end;

{ TFBWireArray }

constructor TFBWireArray.Create(aAttachment: IAttachment;
  aTransaction: TFBTransaction; aField: IArrayMetaData);
begin
  inherited Create(aAttachment,aTransaction,aField);
  FWireAttachment := aAttachment as TObject;
  FWireAPI := (FWireAttachment as TFBWireAttachment).WireAPI;
end;

constructor TFBWireArray.Create(aAttachment: IAttachment;
  aTransaction: TFBTransaction; aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aAttachment,aTransaction,aField,ArrayID);
  FWireAttachment := aAttachment as TObject;
  FWireAPI := (FWireAttachment as TFBWireAttachment).WireAPI;
end;

function TFBWireArray.GetConnection: TFBWireConnection;
begin
  Result := (FWireAttachment as TFBWireAttachment).Connection;
end;

function TFBWireArray.GetTransactionHandle: integer;
begin
  Result := (GetTransaction as TObject as TFBWireTransaction).Handle;
end;

function TFBWireArray.GetSliceLayout: TWireSliceLayout;
var i: integer;
begin
  with GetArrayDesc^ do
  begin
    Result.Dtype := array_desc_dtype;
    Result.ElementLength := array_desc_length;
    {the buffer layout TFBArray.AllocateBuffer establishes: an extra count
     of two bytes for varying, one for the terminator of text}
    case array_desc_dtype of
    blr_varying, blr_varying2:
      Result.BufferStride := array_desc_length + 2;
    blr_text, blr_text2:
      Result.BufferStride := array_desc_length + 1;
    else
      Result.BufferStride := array_desc_length;
    end;
    Result.Count := 1;
    for i := 0 to array_desc_dimensions - 1 do
      Result.Count := Result.Count *
        cardinal(array_desc_bounds[i].array_bound_upper -
                 array_desc_bounds[i].array_bound_lower + 1);
  end;
end;

function TFBWireArray.SDLBytes: TBytes;
var len: integer;
begin
  len := (FSDL as TSDLBlock).getDataLength;
  SetLength(Result,len);
  if len > 0 then
    Move((FSDL as TSDLBlock).getBuffer^,Result[0],len);
end;

function TFBWireArray.ArrayIDAsInt64: Int64;
begin
  Result := (Int64(FArrayID.gds_quad_high) shl 32) or
             Int64(cardinal(FArrayID.gds_quad_low));
end;

procedure TFBWireArray.AllocateBuffer;
begin
  inherited AllocateBuffer;
  FSDL := GenerateSDL(FWireAPI,GetArrayDesc);
end;

procedure TFBWireArray.InternalGetSlice;
begin
  try
    GetConnection.GetSlice(GetTransactionHandle,ArrayIDAsInt64,SDLBytes,
                           GetSliceLayout,FBuffer);
  except
    on E: Exception do WireIBError(FWireAPI,E);
  end;
  SignalActivity;
end;

procedure TFBWireArray.InternalPutSlice(Force: boolean);
var id: Int64;
begin
  try
    id := GetConnection.PutSlice(GetTransactionHandle,ArrayIDAsInt64,SDLBytes,
                                 GetSliceLayout,FBuffer);
    FArrayID.gds_quad_high := Integer(id shr 32);
    FArrayID.gds_quad_low := Cardinal(id and $FFFFFFFF);
  except
    on E: Exception do
      if not Force then WireIBError(FWireAPI,E);
  end;
  SignalActivity;
end;

end.
