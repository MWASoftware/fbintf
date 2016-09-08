unit FB25Array;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$DEFINE HAS_ANSISTRING_CODEPAGE}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBArray, IBHeader, FB25Statement, FB25Attachment, FBClientAPI,
  FB25Transaction;

type

  { TFB25ArrayMetaData }

  TFB25ArrayMetaData = class(TFBArrayMetaData,IArrayMetaData)
  private
    FCharSetID: integer;
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    FCodePage: TSystemCodePage;
    {$ENDIF}
  protected
    procedure LoadMetaData(aAttachment: IAttachment; aTransaction: ITransaction;
                   relationName, columnName: string); override;
  public
    function GetCharSetID: cardinal; override;
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    function GetCodePage: TSystemCodePage; override;
    {$ENDIF}
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
    constructor Create(aAttachment: TFBAttachment; aTransaction: TFB25Transaction; aField: IArrayMetaData); overload;
    constructor Create(aAttachment: TFBAttachment; aTransaction: TFB25Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD); overload;
 end;

implementation

uses FB25ClientAPI;

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
begin
  DBHandle := (aAttachment as TFBAttachment).Handle;
  TRHandle := (aTransaction as TFB25Transaction).Handle;
  with Firebird25ClientAPI do
    if isc_array_lookup_bounds(StatusVector,@(DBHandle),@(TRHandle),
        PChar(relationName),PChar(columnName),@FArrayDesc) > 0 then
          IBDatabaseError;

  if (GetSQLType = SQL_TEXT) or (GetSQLType = SQL_VARYING) then
  with (aAttachment as TFBAttachment) do
  if HasDefaultCharSet then
  begin
    FCharSetID := CharSetID;
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    FCodePage := CodePage;
    {$ENDIF}
  end
  else
  begin
    stmt := TFB25Statement.Create(aAttachment as TFBAttachment,aTransaction,
                                 sGetArrayMetaData ,aAttachment.GetSQLDialect);
    with stmt do
    begin
      SQLParams[0].AsString := RelationName;
      SQLParams[1].AsString := ColumnName;
      with OpenCursor do
      if FetchNext then
      begin
        FCharSetID := Data[0].AsInteger;
        {$IFDEF HAS_ANSISTRING_CODEPAGE}
        FCodePage := CP_NONE;
        FirebirdClientAPI.CharSetID2CodePage(FCharSetID,FCodePage);
        {$ENDIF}
      end;
    end;
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

constructor TFB25Array.Create(aAttachment: TFBAttachment;
  aTransaction: TFB25Transaction; aField: IArrayMetaData);
begin
  inherited Create(aAttachment,aTransaction,aField);
  FDBHandle := aAttachment.Handle;
  FTRHandle := aTransaction.Handle;
end;

constructor TFB25Array.Create(aAttachment: TFBAttachment;
  aTransaction: TFB25Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aAttachment,aTransaction,aField,ArrayID);
  FDBHandle := aAttachment.Handle;
  FTRHandle := aTransaction.Handle;
end;

end.

