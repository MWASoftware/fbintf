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
  Classes, SysUtils, IB, FBArray, IBHeader, FB25Statement, FB25Attachment, FBClientAPI,
  FB25Transaction, FB25ClientAPI;

type

  { TFB25ArrayMetaData }

  TFB25ArrayMetaData = class(TFBArrayMetaData,IArrayMetaData)
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

  { TFB25Array }

  TFB25Array = class(TFBArray,IArray)
  private
    FDBHandle: TISC_DB_HANDLE;
    FTRHandle: TISC_TR_HANDLE;
    FFirebird25ClientAPI: TFB25ClientAPI;
  protected
    procedure InternalGetSlice; override;
    procedure InternalPutSlice(Force: boolean); override;
  public
    constructor Create(aAttachment: TFB25Attachment; aTransaction: TFB25Transaction; aField: IArrayMetaData); overload;
    constructor Create(aAttachment: TFB25Attachment; aTransaction: TFB25Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD); overload;
 end;

implementation

uses IBUtils, FBAttachment{$ifdef WINDOWS}, Windows{$endif};

const
  sGetArrayMetaData = 'Select F.RDB$CHARACTER_SET_ID '+
                      'From RDB$FIELDS F JOIN RDB$RELATION_FIELDS RF '+
                      'On F.RDB$FIELD_NAME = RF.RDB$FIELD_SOURCE '+
                      'Where RF.RDB$RELATION_NAME = ? and RF.RDB$FIELD_NAME = ? '+
                      'UNION '+
                      'Select F.RDB$CHARACTER_SET_ID '+
                      'From RDB$FIELDS F JOIN RDB$PROCEDURE_PARAMETERS PP '+
                      'On F.RDB$FIELD_NAME = PP.RDB$FIELD_SOURCE '+
                      'Where PP.RDB$PROCEDURE_NAME = ? and PP.RDB$PARAMETER_NAME = ?';

  { TFB25ArrayMetaData }

procedure TFB25ArrayMetaData.LoadMetaData(aAttachment: IAttachment;
  aTransaction: ITransaction; relationName, columnName: AnsiString);
var
  DBHandle: TISC_DB_HANDLE;
  TRHandle: TISC_TR_HANDLE;
  stmt: IStatement;
  CharWidth: integer;
  RelName: AnsiString;
  ColName: AnsiString;
begin
  DBHandle := (aAttachment as TFB25Attachment).Handle;
  TRHandle := (aTransaction as TFB25Transaction).Handle;
  RelName := SafeAnsiUpperCase(relationName);
  ColName := SafeAnsiUpperCase(columnName);
  with (aAttachment as TFB25Attachment), Firebird25ClientAPI do
    if isc_array_lookup_bounds(StatusVector,@(DBHandle),@(TRHandle),
        PAnsiChar(RelName),PAnsiChar(ColName),@FArrayDesc) > 0 then
          IBDatabaseError;

  if (GetSQLType = SQL_TEXT) or (GetSQLType = SQL_VARYING) then
  begin
    stmt := TFB25Statement.Create(aAttachment as TFB25Attachment,aTransaction,
                                 sGetArrayMetaData ,aAttachment.GetSQLDialect);
    with stmt do
    begin
      SQLParams[0].AsString := RelationName;
      SQLParams[1].AsString := ColumnName;
      SQLParams[2].AsString := RelationName;
      SQLParams[3].AsString := ColumnName;
      with OpenCursor do
      if FetchNext then
      begin
        FCharSetID := Data[0].AsInteger;
        FCharSetWidth := 1;
        with (aAttachment as TFB25Attachment) do
        if (FCharSetID > 1) and HasDefaultCharSet then
        begin
          FCharSetID := CharSetID;
          FCodePage := CodePage;
        end
        else
        begin
          FCodePage := CP_NONE;
          FAttachment.CharSetID2CodePage(FCharSetID,FCodePage);
          FAttachment.CharSetWidth(FCharSetID,FCharSetWidth);
        end;
      end;
    end;
  end;
  if (FArrayDesc.array_desc_dtype in [blr_text,blr_cstring, blr_varying]) and
      (FCharSetID = 0) then {This really shouldn't be necessary - but it is :(}
  with aAttachment as TFBAttachment do
  begin
    if HasDefaultCharSet  and FAttachment.CharSetWidth(CharSetID,CharWidth) then
      FArrayDesc.array_desc_length := FArrayDesc.array_desc_length * CharWidth;
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

function TFB25ArrayMetaData.GetCharSetWidth: integer;
begin
  Result := FCharSetWidth;
end;

{ TFB25Array }

procedure TFB25Array.InternalGetSlice;
begin
  with FFirebird25ClientAPI do
     Call(isc_array_get_slice(StatusVector,@(FDBHandle),@(FTRHandle),
                                @FArrayID, GetArrayDesc,
                                Pointer(FBuffer), @FBufSize));
end;

procedure TFB25Array.InternalPutSlice(Force: boolean);
begin
  with FFirebird25ClientAPI do
     if (isc_array_put_slice(StatusVector, @(FDBHandle),@(FTRHandle),
                                @FArrayID, GetArrayDesc,
                                Pointer(FBuffer),@FBufSize) > 0) and not Force then
       OnDatabaseError;
  SignalActivity;
end;

constructor TFB25Array.Create(aAttachment: TFB25Attachment;
  aTransaction: TFB25Transaction; aField: IArrayMetaData);
begin
  inherited Create(aAttachment,aTransaction,aField);
  FDBHandle := aAttachment.Handle;
  FTRHandle := aTransaction.Handle;
  FFirebird25ClientAPI := aAttachment.Firebird25ClientAPI;
  OnDatabaseError := aAttachment.IBDataBaseError;
end;

constructor TFB25Array.Create(aAttachment: TFB25Attachment;
  aTransaction: TFB25Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aAttachment,aTransaction,aField,ArrayID);
  FDBHandle := aAttachment.Handle;
  FTRHandle := aTransaction.Handle;
  FFirebird25ClientAPI := aAttachment.Firebird25ClientAPI;
  OnDatabaseError := aAttachment.IBDataBaseError;
end;

end.

