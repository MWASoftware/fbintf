unit FB25Attachment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBLibrary, FB25ClientAPI, IBHeader, IBExternals, FB25APIObject;

type
  TDPB = class;

  { TDPBItem }

  TDPBItem = class(TInterfacedObject,IDPBItem)
  private
    FOwner: TDPB;
    FBufPtr: PChar;
    FBuflength: integer;
    FIsByte: boolean;
    procedure MoveBy(delta: integer);
  public
    constructor Create(AOwner: TDPB; Param: byte; BufPtr: PChar;
       Buflength: integer);

  public
    {IDPBItem}
    function getParamType: byte;
    function getAsString: string;
    function getAsByte: byte;
    procedure setAsString(aValue: string);
    procedure setAsByte(aValue: byte);
  end;

  { TDPB }

  TDPB = class(TInterfacedObject, IDPB)
  private
    FItems: array of IDPBItem;
    FBuffer: PChar;
    FDataLength: integer;
    FBufferSize: integer;
    procedure AdjustBuffer;
    procedure UpdateRequestItemSize(Item: TDPBItem; NewSize: integer);
  public
    constructor Create;
    destructor Destroy; override;
    function getBuffer: PChar;
    function getBufferLength: integer;

  public
    {IDPB}
    function getCount: integer;
    function Add(ParamType: byte): IDPBItem;
    function Find(ParamType: byte): IDPBItem;
    function getItems(index: integer): IDPBItem;
  end;

  { TFBAttachment }

  TFBAttachment = class(TAPIObject,IAttachment)
  private
    FHandle: TISC_DB_HANDLE;
    FDatabaseName: string;
    FDPB: IDPB;
  public
    constructor Create(DatabaseName: string; DPB: IDPB);
    constructor CreateDatabase(DatabaseName: string; SQLDialect: integer;
      CreateParams: string; DPB: IDPB);
    destructor Destroy; override;
    property Handle: TISC_DB_HANDLE read FHandle;

  public
    {IAttachment}
    function GetStatus: IStatus;
    function getDPB: IDPB;
    procedure Connect;
    procedure Disconnect(Force: boolean=false);
    procedure DropDatabase;
    function StartTransaction(Params: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction;
    function CreateBlob(transaction: ITransaction): IBlob;
    function OpenBlob(transaction: ITransaction; BlobID: TISC_QUAD): IBlob;
    procedure ExecImmediate(transaction: ITransaction; sql: string; SQLDialect: integer);
    function Prepare(transaction: ITransaction; sql: string; SQLDialect: integer
      ): IStatement;
    function GetEventHandler(Events: TStrings): IEvents;

    {Database Information}
    function GetBlobCharSetID(transaction: ITransaction; tableName, columnName: string): short;
    function GetDBInformation(DBInfoCommand: byte): IDBInformation;
  end;

implementation

uses FB25Events, FB25Status, FB25Transaction, FBErrorMessages, FB25Blob,
  FB25Statement, FB25DBInfo;

{ TDPBItem }

procedure TDPBItem.MoveBy(delta: integer);
var src, dest: PChar;
    i: integer;
begin
  src := FBufptr;
  dest := FBufptr + delta ;
  if delta > 0 then
  begin
    for i := FBufLength - 1 downto 0 do
      (dest +i)^ := (src+i)^;
  end
  else
  begin
    for i := 0 to FBufLength - 1 do
    (dest +i)^ := (src+i)^;
  end;

  FBufPtr += delta;
end;

constructor TDPBItem.Create(AOwner: TDPB; Param: byte; BufPtr: PChar;
  Buflength: integer);
begin
  inherited Create;
  FOwner := AOwner;
  FBufPtr := BufPtr;
  FBufLength := BufLength;
  FBufPtr^ := char(Param);
  (FBufPtr+1)^ := #1;
  (FBufPtr+2)^ := #0;
  FIsByte := true; {default}
end;

function TDPBItem.getParamType: byte;
begin
  Result := byte(FBufPtr^);
end;

function TDPBItem.getAsString: string;
var len: byte;
begin
  if FIsByte then
    Result := IntToStr(getAsByte)
  else
  begin
    len := byte((FBufPtr+1)^);
    SetString(Result,FBufPtr+2,len);
  end;
end;

function TDPBItem.getAsByte: byte;
begin
  if FIsByte then
    Result := byte((FBufPtr+2)^)
  else
    IBError(ibxeDPBParamTypeError,[nil]);
end;

procedure TDPBItem.setAsString(aValue: string);
var len: integer;
begin
  len := Length(aValue);
  if len <> FBufLength - 2 then
    FOwner.UpdateRequestItemSize(self,len+2);
  (FBufPtr+1)^ := char(len);
  Move(aValue[1],(FBufPtr+2)^,len);
  FIsByte := false;
end;

procedure TDPBItem.setAsByte(aValue: byte);
begin
  if FBufLength <> 3 then
  FOwner.UpdateRequestItemSize(self,3);
  FIsByte := true;
  (FBufPtr+1)^ := #1;
  (FBufPtr+2)^ := char(aValue);
end;

{ TDPB }

procedure TDPB.AdjustBuffer;
begin
  if FDataLength > FBufferSize then
  begin
    FBufferSize := FDataLength;
    ReallocMem(FBuffer,FBufferSize);
  end;
end;

procedure TDPB.UpdateRequestItemSize(Item: TDPBItem; NewSize: integer
  );
var i, delta: integer;
begin
  delta := NewSize - Item.FBufLength;
  if delta > 0 then
  begin
    FDataLength += delta;
    AdjustBuffer;
    i := Length(FItems) - 1;
    while i >= 0  do
    begin
      if (FItems[i] as TDPBItem) = Item then
        break; {we're done}
      (FItems[i] as TDPBItem).Moveby(delta);
      Dec(i);
    end;
  end
  else
  begin
    i := 0;
    while i < Length(FItems) do
    begin
      if (FItems[i] as TDPBItem) = Item then
        break; {we're done}
      Inc(i);
    end;
    Inc(i);
    while i < Length(FItems) do
    begin
      (FItems[i] as TDPBItem).Moveby(delta);
      Inc(i);
    end;
    FDataLength += delta;
  end;
  Item.FBufLength := NewSize;
end;

constructor TDPB.Create;
begin
  inherited Create;
  GetMem(FBuffer,128);
  if FBuffer = nil then
    OutOfMemoryError;
  FBufferSize := 128;
  FDataLength := 1;
  FBuffer^ := char(isc_dpb_version1);
end;

destructor TDPB.Destroy;
begin
  Freemem(FBuffer);
  inherited Destroy;
end;

function TDPB.getBuffer: PChar;
begin
  Result := FBuffer;
end;

function TDPB.getBufferLength: integer;
begin
  Result :=  FDataLength
end;

function TDPB.getCount: integer;
begin
  Result := Length(FItems);
end;

function TDPB.Add(ParamType: byte): IDPBItem;
var P: PChar;
begin
  P := FBuffer + FDataLength;
  Inc(FDataLength,3); {assume byte}
  AdjustBuffer;
  Result := TDPBItem.Create(self,ParamType,P,3);
  SetLength(FItems,Length(FItems)+1);
  FItems[Length(FItems) - 1 ] := Result;
end;

function TDPB.Find(ParamType: byte): IDPBItem;
var i: integer;
begin
  Result := nil;
  for i := 0 to getCount - 1 do
    if FItems[i].getParamType = ParamType then
    begin
      Result := FItems[i];
      Exit;
    end;
end;

function TDPB.getItems(index: integer): IDPBItem;
begin
   if (index >= 0 ) and (index < Length(FItems)) then
    Result := FItems[index]
  else
    IBError(ibxeDPBIndexError,[index]);
end;

  { TFBAttachment }

constructor TFBAttachment.Create(DatabaseName: string; DPB: IDPB);
begin
  inherited Create;
  Firebird25ClientAPI.RegisterObj(self);
  FDatabaseName := DatabaseName;
  FDPB := DPB;
  Connect;
end;

constructor TFBAttachment.CreateDatabase(DatabaseName: string;
  SQLDialect: integer; CreateParams: string; DPB: IDPB);
var sql: string;
    tr_handle: TISC_TR_HANDLE;
begin
  inherited Create;
  Firebird25ClientAPI.RegisterObj(self);
  FDatabaseName := DatabaseName;
  FDPB := DPB;
  tr_handle := nil;
  sql := 'CREATE DATABASE ''' + DatabaseName + ''' ' + CreateParams; {do not localize}
  with Firebird25ClientAPI do
  if isc_dsql_execute_immediate(StatusVector, @FHandle, @tr_handle, 0, PChar(sql),
                                  SQLDialect, nil) > 0 then
    IBDataBaseError;

  if assigned(FDPB) and (FDPB.getCount > 0) then
  begin
    {If connect params specified then detach and connect properly}
    with Firebird25ClientAPI do
      Call(isc_detach_database(StatusVector, @FHandle));
    Connect;
  end
end;

destructor TFBAttachment.Destroy;
begin
  Disconnect(true);
  Firebird25ClientAPI.UnRegisterObj(self);
  inherited Destroy;
end;

function TFBAttachment.GetStatus: IStatus;
begin
  Result := Firebird25ClientAPI.Status;
end;

function TFBAttachment.getDPB: IDPB;
begin
  Result := FDPB;
end;

procedure TFBAttachment.Connect;
begin
  with Firebird25ClientAPI do
  if FDPB = nil then
  Call(isc_attach_database(StatusVector, Length(FDatabaseName),
                        PChar(FDatabaseName), @FHandle, 0, nil))
  else
   Call(isc_attach_database(StatusVector, Length(FDatabaseName),
                         PChar(FDatabaseName), @FHandle,
                         (FDPB as TDPB).getBufferLength,
                         (FDPB as TDPB).getBuffer));
end;

procedure TFBAttachment.Disconnect(Force: boolean);
var i: integer;
begin
  if FHandle = nil then
    Exit;

  {Rollback or Cancel dependent objects}
  for i := 0 to OwnedObjects.Count - 1 do
    if (TObject(OwnedObjects[i]) is TFBTransaction) then
          TFBTransaction(OwnedObjects[i]).Rollback
    else
    if TObject(OwnedObjects[i]) is TFBEvents then
      TFBEvents(OwnedObjects[i]).Cancel;

  {Disconnect}
  with Firebird25ClientAPI do
    Call(isc_detach_database(StatusVector, @FHandle),not Force);
  FHandle := nil;
end;

procedure TFBAttachment.DropDatabase;
begin
  with Firebird25ClientAPI do
    Call(isc_drop_database(StatusVector, @FHandle));
  FHandle := nil;
end;

function TFBAttachment.StartTransaction(Params: array of byte;
  DefaultCompletion: TTransactionCompletion): ITransaction;
begin
  Result := TFBTransaction.Create(self,Params,DefaultCompletion);
end;

function TFBAttachment.CreateBlob(transaction: ITransaction): IBlob;
begin
  Result := TFBBLob.Create(self,transaction);
end;

function TFBAttachment.OpenBlob(transaction: ITransaction; BlobID: TISC_QUAD
  ): IBlob;
begin
  Result := TFBBLob.Create(self,transaction,BlobID);
end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: string;
  SQLDialect: integer);
var TRHandle: TISC_TR_HANDLE;
begin
  TRHandle := (Transaction as TFBTransaction).Handle;
  with Firebird25ClientAPI do
    Call(isc_dsql_execute_immediate(StatusVector, @fHandle, @TRHandle, 0,PChar(sql), SQLDialect, nil));
end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: string;
  SQLDialect: integer): IStatement;
begin
  Result := TFBStatement.Create(self,transaction,sql,SQLDialect);
end;

function TFBAttachment.GetEventHandler(Events: TStrings): IEvents;
begin
  Result := TFBEvents.Create(self,Events);
end;

function TFBAttachment.GetBlobCharSetID(transaction: ITransaction; tableName,
  columnName: string): short;
var desc: TISC_BLOB_DESC;
    uGlobal: array [0..31] of char;
    trHandle: TISC_TR_HANDLE;
begin
  trHandle := (transaction as TFBTransaction).Handle;
  with Firebird25ClientAPI do
    Call(isc_blob_lookup_desc(StatusVector,@FHandle,@trHandle,
                PChar(tableName),PChar(columnName),@desc,@uGlobal));
  Result := desc.blob_desc_charset;
end;

function TFBAttachment.GetDBInformation(DBInfoCommand: byte): IDBInformation;
begin
  Result := TDBInformation.Create(self,DBInfoCommand);
end;

end.

