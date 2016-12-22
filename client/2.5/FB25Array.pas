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
unit FB25Array;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBArray, IBHeader, FB25Statement, FB25Attachment, FBClientAPI,
  FB25Transaction;

type

  { TFB25ArrayMetaData }

  TFB25ArrayMetaData = class(TFBArrayMetaData,IArrayMetaData)
  private
    FCodePage: TSystemCodePage;
  protected
    procedure LoadMetaData(aAttachment: IAttachment; aTransaction: ITransaction;
                   relationName, columnName: string); override;
  public
    function GetCharSetID: cardinal; override;
    function GetCodePage: TSystemCodePage; override;
  end;

  { TFB25Array }

  TFB25Array = class(TFBArray,IArray)
  private
    FDBHandle: TISC_DB_HANDLE;
    FTRHandle: TISC_TR_HANDLE;
  protected
    procedure InternalGetSlice; override;
    procedure InternalPutSlice(Force: boolean); override;
  public
    constructor Create(aAttachment: TFB25Attachment; aTransaction: TFB25Transaction; aField: IArrayMetaData); overload;
    constructor Create(aAttachment: TFB25Attachment; aTransaction: TFB25Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD); overload;
 end;

implementation

uses FBAttachment, FB25ClientAPI;

const
  sGetArrayMetaData = 'Select F.RDB$CHARACTER_SET_ID '+
                      'From RDB$FIELDS F JOIN RDB$RELATION_FIELDS RF '+
                      'On F.RDB$FIELD_NAME = RF.RDB$FIELD_SOURCE '+
                      'Where RF.RDB$RELATION_NAME = ? and RF.RDB$FIELD_NAME = ?';

  { TFB25ArrayMetaData }

procedure TFB25ArrayMetaData.LoadMetaData(aAttachment: IAttachment;
  aTransaction: ITransaction; relationName, columnName: string);
var
  DBHandle: TISC_DB_HANDLE;
  TRHandle: TISC_TR_HANDLE;
  stmt: IStatement;
  CharWidth: integer;
begin
  DBHandle := (aAttachment as TFB25Attachment).Handle;
  TRHandle := (aTransaction as TFB25Transaction).Handle;
  with Firebird25ClientAPI do
    if isc_array_lookup_bounds(StatusVector,@(DBHandle),@(TRHandle),
        PChar(AnsiUpperCase(relationName)),PChar(AnsiUpperCase(columnName)),@FArrayDesc) > 0 then
          IBDatabaseError;

  if (GetSQLType = SQL_TEXT) or (GetSQLType = SQL_VARYING) then
  begin
    stmt := TFB25Statement.Create(aAttachment as TFB25Attachment,aTransaction,
                                 sGetArrayMetaData ,aAttachment.GetSQLDialect);
    with stmt do
    begin
      SQLParams[0].AsString := RelationName;
      SQLParams[1].AsString := ColumnName;
      with OpenCursor do
      if FetchNext then
      begin
        FCharSetID := Data[0].AsInteger;
        with (aAttachment as TFB25Attachment) do
        if (FCharSetID > 1) and HasDefaultCharSet then
        begin
          FCharSetID := CharSetID;
          FCodePage := CodePage;
        end
        else
        begin
          FCodePage := CP_NONE;
          FirebirdClientAPI.CharSetID2CodePage(FCharSetID,FCodePage);
        end;
      end;
    end;
  end;
  if (FArrayDesc.array_desc_dtype in [blr_text,blr_cstring, blr_varying]) and
      (FCharSetID = 0) then {This really shouldn't be necessary - but it is :(}
  with aAttachment as TFBAttachment do
  begin
    if HasDefaultCharSet  and FirebirdClientAPI.CharSetWidth(CharSetID,CharWidth) then
      FArrayDesc.array_desc_length *= CharWidth;
  end;
end;

function TFB25ArrayMetaData.GetCharSetID: cardinal;
begin
  Result := FCharSetID;
end;

function TFB25ArrayMetaData.GetCodePage: TSystemCodePage;
begin
  Result := FCodePage;
end;

{ TFB25Array }

procedure TFB25Array.InternalGetSlice;
begin
  with Firebird25ClientAPI do
     Call(isc_array_get_slice(StatusVector,@(FDBHandle),@(FTRHandle),
                                @FArrayID, GetArrayDesc,
                                Pointer(FBuffer), @FBufSize));
end;

procedure TFB25Array.InternalPutSlice(Force: boolean);
begin
  with Firebird25ClientAPI do
     if (isc_array_put_slice(StatusVector, @(FDBHandle),@(FTRHandle),
                                @FArrayID, GetArrayDesc,
                                Pointer(FBuffer),@FBufSize) > 0) and not Force then
       IBDatabaseError;
  SignalActivity;
end;

constructor TFB25Array.Create(aAttachment: TFB25Attachment;
  aTransaction: TFB25Transaction; aField: IArrayMetaData);
begin
  inherited Create(aAttachment,aTransaction,aField);
  FDBHandle := aAttachment.Handle;
  FTRHandle := aTransaction.Handle;
end;

constructor TFB25Array.Create(aAttachment: TFB25Attachment;
  aTransaction: TFB25Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aAttachment,aTransaction,aField,ArrayID);
  FDBHandle := aAttachment.Handle;
  FTRHandle := aTransaction.Handle;
end;

end.

