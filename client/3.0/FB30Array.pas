unit FB30Array;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBArray, IBHeader, FB30Attachment,
  FB30Transaction;

(*type

  { TFB30ArrayMetaData }

  TFB30ArrayMetaData = class(TFBArrayMetaData,IArrayMetaData)
  protected
    procedure LoadMetaData(aAttachment: IAttachment; aTransaction: ITransaction;
                   relationName, columnName: string); override;
  end;

  { TFB30Array }

  TFB30Array = class(TFBArray,IArray)
  private
    FDBHandle: TISC_DB_HANDLE;
    FTRHandle: TISC_TR_HANDLE;
  protected
    procedure InternalGetSlice; override;
    procedure InternalPutSlice(Force: boolean); override;
  public
    constructor Create(aAttachment: TFBAttachment; aTransaction: TFB30Transaction; aField: IArrayMetaData); overload;
    constructor Create(aAttachment: TFBAttachment; aTransaction: TFB30Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD); overload;
 end;  *)

implementation

(*uses FB30ClientAPI;

{ TFB30ArrayMetaData }

procedure TFB30ArrayMetaData.LoadMetaData(aAttachment: IAttachment;
  aTransaction: ITransaction; relationName, columnName: string);
var
  DBHandle: TISC_DB_HANDLE;
  TRHandle: TISC_TR_HANDLE;
begin
  DBHandle := (aAttachment as TFBAttachment).Handle;
  TRHandle := (aTransaction as TFB30Transaction).Handle;
  with Firebird25ClientAPI do
    if isc_array_lookup_bounds(StatusVector,@(DBHandle),@(TRHandle),
        PChar(relationName),PChar(columnName),@FArrayDesc) > 0 then
          IBDatabaseError;
end;

{ TFB30Array }

procedure TFB30Array.InternalGetSlice;
begin
  with Firebird25ClientAPI do
     Call(isc_array_get_slice(StatusVector,@(FDBHandle),@(FTRHandle),
                                @FArrayID, GetArrayDesc,
                                Pointer(FBuffer), @FBufSize));
end;

procedure TFB30Array.InternalPutSlice(Force: boolean);
begin
  with Firebird25ClientAPI do
     if (isc_array_put_slice(StatusVector, @(FDBHandle),@(FTRHandle),
                                @FArrayID, GetArrayDesc,
                                Pointer(FBuffer),@FBufSize) > 0) and not Force then
       IBDatabaseError;
  SignalActivity;
end;

constructor TFB30Array.Create(aAttachment: TFBAttachment;
  aTransaction: TFB30Transaction; aField: IArrayMetaData);
begin
  inherited Create(aAttachment,aTransaction,aField);
  FDBHandle := aAttachment.Handle;
  FTRHandle := aTransaction.Handle;
end;

constructor TFB30Array.Create(aAttachment: TFBAttachment;
  aTransaction: TFB30Transaction; aField: IArrayMetaData; ArrayID: TISC_QUAD);
begin
  inherited Create(aAttachment,aTransaction,aField,ArrayID);
  FDBHandle := aAttachment.Handle;
  FTRHandle := aTransaction.Handle;
end;  *)

end.

