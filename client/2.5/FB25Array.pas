unit FB25Array;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$DEFINE HAS_ANSISTRING_CODEPAGE}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBArray, IBHeader, FB25Statement, FB25Attachment,
  FB25Transaction;

type

  { TFB25ArrayMetaData }

  TFB25ArrayMetaData = class(TFBArrayMetaData,IArrayMetaData)
  protected
    procedure LoadMetaData(aAttachment: IAttachment; aTransaction: ITransaction;
                   relationName, columnName: string); override;
  end;

  { TFB25Array }

  TFB25Array = class(TFBArray,IArray)
  private
    FDBHandle: TISC_DB_HANDLE;
    FTRHandle: TISC_TR_HANDLE;
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    FCodePage: TSystemCodePage;
    {$ENDIF}
  protected
    procedure InternalGetSlice; override;
    procedure InternalPutSlice(Force: boolean); override;
  public
    constructor Create(aAttachment: TFBAttachment; aTransaction: TFB25Transaction; aField: IArrayMetaData); overload;
    constructor Create(aAttachment: TFBAttachment; aTransaction: TFB25Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD); overload;
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    function GetCodePage: TSystemCodePage; override;
    {$ENDIF}
 end;

implementation

uses FB25ClientAPI;

{ TFB25ArrayMetaData }

procedure TFB25ArrayMetaData.LoadMetaData(aAttachment: IAttachment;
  aTransaction: ITransaction; relationName, columnName: string);
var
  DBHandle: TISC_DB_HANDLE;
  TRHandle: TISC_TR_HANDLE;
begin
  DBHandle := (aAttachment as TFBAttachment).Handle;
  TRHandle := (aTransaction as TFB25Transaction).Handle;
  with Firebird25ClientAPI do
    if isc_array_lookup_bounds(StatusVector,@(DBHandle),@(TRHandle),
        PChar(relationName),PChar(columnName),@FArrayDesc) > 0 then
          IBDatabaseError;
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
  if aAttachment.HasDefaultCharSet then
    FCodePage := aAttachment.CodePage;
end;

constructor TFB25Array.Create(aAttachment: TFBAttachment;
  aTransaction: TFB25Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aAttachment,aTransaction,aField,ArrayID);
  FDBHandle := aAttachment.Handle;
  FTRHandle := aTransaction.Handle;
  if aAttachment.HasDefaultCharSet then
    FCodePage := aAttachment.CodePage;
end;

{$IFDEF HAS_ANSISTRING_CODEPAGE}
function TFB25Array.GetCodePage: TSystemCodePage;
begin
  Result := FCodePage;
end;
{$ENDIF}

end.

