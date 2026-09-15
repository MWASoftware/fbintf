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
  Classes, SysUtils, FirebirdOOAPI, IB, FBArray, IBHeader, FB30Attachment, FBClientAPI,
  FB30Transaction, FBParamBlock, FB30ClientAPI, FBSDL;

type

   { TFB30ArrayMetaData }

  TFB30ArrayMetaData = class(TFBArrayMetaData,IArrayMetaData)
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

  { TFB30Array }

  TFB30Array = class(TFBArray,IArray)
  private
    FAttachmentIntf: FirebirdOOAPI.IAttachment;
    FTransactionIntf: FirebirdOOAPI.ITransaction;
    FFirebird30ClientAPI: TFB30ClientAPI;
    FSDL: ISDL;
  protected
    procedure AllocateBuffer; override;
    procedure InternalGetSlice; override;
    procedure InternalPutSlice(Force: boolean); override;
  public
    constructor Create(aAttachment: TFB30Attachment; aTransaction: TFB30Transaction; aField: IArrayMetaData); overload;
    constructor Create(aAttachment: TFB30Attachment; aTransaction: TFB30Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD); overload;
  end;

  {The SDL generator now lives in FBSDL so that the wire protocol provider
   can share it - these aliases keep existing references compiling}
  TSDLItem = FBSDL.TSDLItem;
  TSDLBlock = FBSDL.TSDLBlock;

implementation

uses IBUtils, FBAttachment, FB30Statement{$ifdef WINDOWS}, Windows{$endif};

const
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


{ TFB30ArrayMetaData }

{Assemble the array descriptor from the System Tables}

procedure TFB30ArrayMetaData.LoadMetaData(aAttachment: IAttachment;
  aTransaction: ITransaction; relationName, columnName: AnsiString);
var stmt: IStatement;
    CharWidth: integer;
begin
  CharWidth := 0;
  RelationName := SafeAnsiUpperCase(RelationName);
  ColumnName := SafeAnsiUpperCase(ColumnName);
  stmt := TFB30Statement.Create(aAttachment as TFB30Attachment,aTransaction,
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
      FillChar(FArrayDesc.array_desc_field_name,sizeof(FArrayDesc.array_desc_field_name),' ');
      FillChar(FArrayDesc.array_desc_relation_name,sizeof(FArrayDesc.array_desc_relation_name),' ');
      Move(columnName[1],FArrayDesc.array_desc_field_name,Length(columnName));
      Move(relationName[1],FArrayDesc.array_desc_relation_name,length(relationName));
      FArrayDesc.array_desc_length := Data[0].AsInteger;
      FArrayDesc.array_desc_scale := Data[1].AsInteger;
      FArrayDesc.array_desc_dtype := Data[2].AsInteger;
      FArrayDesc.array_desc_dimensions := Data[3].AsInteger;
      FArrayDesc.array_desc_flags := 0; {row major}
      FCharSetID := Data[7].AsInteger;
      if (FCharSetID > 1) and (aAttachment as TFB30Attachment).HasDefaultCharSet then
        FCharSetID := (aAttachment as TFB30Attachment).CharSetID;
      FCodePage := CP_NONE;
      FAttachment.CharSetID2CodePage(FCharSetID,FCodePage);
      FCharSetWidth := 1;
      FAttachment.CharSetWidth(FCharSetID,FCharSetWidth);
      if (FArrayDesc.array_desc_dtype in [blr_text,blr_cstring, blr_varying]) and
        (FCharSetID = 0) then {This really shouldn't be necessary - but it is :(}
      with aAttachment as TFBAttachment do
      begin
        if HasDefaultCharSet  and FAttachment.CharSetWidth(CharSetID,CharWidth) then
          FArrayDesc.array_desc_length := FArrayDesc.array_desc_length * CharWidth;
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

function TFB30ArrayMetaData.GetCharSetWidth: integer;
begin
  Result := FCharSetWidth;
end;

{ TFB30Array }

procedure TFB30Array.AllocateBuffer;

begin
  inherited AllocateBuffer;
  {Now set up the SDL}
  FSDL := GenerateSDL(FFirebird30ClientAPI,GetArrayDesc);
end;

procedure TFB30Array.InternalGetSlice;
begin
  with FFirebird30ClientAPI do
  begin
    FAttachmentIntf.getSlice(StatusIntf,FTransactionIntf,
                          @FArrayID,
                          (FSDL as TSDLBlock).getDataLength,
                          BytePtr((FSDL as TSDLBlock).getBuffer),
                          0,nil,
                          FBufSize,BytePtr(FBuffer)
                          );
    Check4DataBaseError(ConnectionCodePage);
  end;
  SignalActivity;
end;

procedure TFB30Array.InternalPutSlice(Force: boolean);
begin
  with FFirebird30ClientAPI do
  begin
    FAttachmentIntf.putSlice(StatusIntf,FTransactionIntf, @FArrayID,
                          (FSDL as TSDLBlock).getDataLength,
                          BytePtr((FSDL as TSDLBlock).getBuffer),
                          0,nil,
                          FBufSize,BytePtr(FBuffer)
                          );
    if not Force then
      Check4DataBaseError(ConnectionCodePage);
  end;
  SignalActivity;
end;

constructor TFB30Array.Create(aAttachment: TFB30Attachment;
  aTransaction: TFB30Transaction; aField: IArrayMetaData);
begin
  inherited Create(aAttachment,aTransaction,aField);
  FAttachmentIntf := aAttachment.AttachmentIntf;
  FTransactionIntf := aTransaction.TransactionIntf;
  FFirebird30ClientAPI := aAttachment.Firebird30ClientAPI;
end;

constructor TFB30Array.Create(aAttachment: TFB30Attachment;
  aTransaction: TFB30Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aAttachment,aTransaction,aField,ArrayID);
  FAttachmentIntf := aAttachment.AttachmentIntf;
  FTransactionIntf := aTransaction.TransactionIntf;
  FFirebird30ClientAPI := aAttachment.Firebird30ClientAPI;
end;

end.

