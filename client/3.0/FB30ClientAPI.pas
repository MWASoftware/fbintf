unit FB30ClientAPI;

{$mode delphi}

interface

uses
  Classes, SysUtils, FBClientAPI, Firebird, IB, IBExternals;

type

  { TFB30Status }

  TFB30Status = class(TFBStatus,IStatus)
  private
    FStatus: Firebird.IStatus;
  public
    function InErrorState: boolean;
    function GetStatus: Firebird.IStatus;
    function StatusVector: PStatusVector; override;
  end;

  Tfb_get_master_interface = function: IMaster;
                              {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  { TFB30ClientAPI }

  TFB30ClientAPI = class(TFBClientAPI,IFirebirdAPI)
  private
    fb_get_master_interface: Tfb_get_master_interface;
    FMaster: Firebird.IMaster;
    FUtil: Firebird.IUtil;
    FProvider: Firebird.IProvider;
    FStatus: TFB30Status;
    FStatusIntf: IStatus;   {Keep a reference to the interface - automatic destroy
                             when this class is freed and last reference to IStatus
                             goes out of scope.}
  protected
    procedure LoadInterface; override;
  public
    constructor Create;
    destructor Destroy; override;

    function StatusIntf: Firebird.IStatus;
    procedure Check4DataBaseError;
    function InErrorState: boolean;

  public
    {IFirebirdAPI}
    function GetStatus: IStatus; override;
    function AllocateDPB: IDPB;
    function AllocateTPB: ITPB;

    {Database connections}
    function OpenDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnConnectError: boolean=true): IAttachment;
    function CreateDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnError: boolean=true): IAttachment;

    {Start Transaction against multiple databases}
    function StartTransaction(Attachments: array of IAttachment;
             TPB: array of byte; DefaultCompletion: TTransactionAction): ITransaction; overload;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: ITPB; DefaultCompletion: TTransactionAction): ITransaction; overload;

    {Service Manager}
    function AllocateSPB: ISPB;
    function GetServiceManager(ServerName: string; Protocol: TProtocol; SPB: ISPB): IServiceManager;

    {Information}
    function HasServiceAPI: boolean;
    function HasMasterIntf: boolean;
    function HasRollbackRetaining: boolean;
    function GetImplementationVersion: string;

    {Encode/Decode}
    function DecodeInteger(bufptr: PChar; len: short): integer; override;
    procedure SQLEncodeDate(aDate: TDateTime; bufptr: PChar); override;
    function SQLDecodeDate(bufptr: PChar): TDateTime; override;
    procedure SQLEncodeTime(aTime: TDateTime; bufptr: PChar); override;
    function SQLDecodeTime(bufptr: PChar): TDateTime;  override;
    procedure SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PChar); override;
    function SQLDecodeDateTime(bufptr: PChar): TDateTime; override;

    {Firebird Interfaces}
    property MasterIntf: Firebird.IMaster read FMaster;
    property UtilIntf: Firebird.IUtil read FUtil;
    property ProviderIntf: Firebird.IProvider read FProvider;
  end;

var Firebird30ClientAPI: TFB30ClientAPI;

implementation

uses FBParamBlock, FB30Attachment, dynlibs, FBMessages, FB30Services,
  FB30Transaction;

type
  PISC_DATE = ^ISC_DATE;
  PISC_TIME = ^ISC_TIME;

{ TFB30Status }

function TFB30Status.InErrorState: boolean;
begin
  with GetStatus do
    Result := ((getState and STATE_ERRORS) <> 0);
end;

function TFB30Status.GetStatus: Firebird.IStatus;
begin
  if FStatus = nil then
  with FOwner do
    FStatus := (FOwner as TFB30ClientAPI).MasterIntf.GetStatus;
  Result := FStatus;
end;

function TFB30Status.StatusVector: PStatusVector;
begin
  Result := PStatusVector(GetStatus.getErrors);
end;

{ TFB30ClientAPI }

procedure TFB30ClientAPI.LoadInterface;
begin
  inherited LoadInterface;
  fb_get_master_interface := GetProcAddress(IBLibrary, 'fb_get_master_interface'); {do not localize}
  if assigned(fb_get_master_interface) then
  begin
    FMaster := fb_get_master_interface;
    FUtil := FMaster.getUtilInterface;
    FProvider := FMaster.getDispatcher;
  end;
end;

constructor TFB30ClientAPI.Create;
begin
  inherited;
  FStatus := TFB30Status.Create(self);
  FStatusIntf := FStatus;
  Firebird30ClientAPI := self;
end;

destructor TFB30ClientAPI.Destroy;
begin
  if assigned(FProvider) then
    FProvider.release;
  inherited Destroy;
end;

function TFB30ClientAPI.StatusIntf: Firebird.IStatus;
begin
  Result := FStatus.GetStatus;
end;

procedure TFB30ClientAPI.Check4DataBaseError;
begin
  if FStatus.InErrorState then
    IBDataBaseError;
end;

function TFB30ClientAPI.InErrorState: boolean;
begin
  Result := FStatus.InErrorState;
end;

function TFB30ClientAPI.GetStatus: IStatus;
begin
  Result := FStatusIntf;
end;

function TFB30ClientAPI.AllocateDPB: IDPB;
begin
  Result := TDPB.Create;
end;

function TFB30ClientAPI.AllocateTPB: ITPB;
begin
  Result := TTPB.Create;
end;

function TFB30ClientAPI.OpenDatabase(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnConnectError: boolean): IAttachment;
begin
  Result := TFBAttachment.Create(DatabaseName, DPB, RaiseExceptionOnConnectError);
  if not Result.IsConnected then
    Result := nil;
end;

function TFB30ClientAPI.CreateDatabase(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnError: boolean): IAttachment;
begin
  Result := TFBAttachment.CreateDatabase(DatabaseName,DPB, RaiseExceptionOnError);
  if not Result.IsConnected then
    Result := nil;
end;

function TFB30ClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: array of byte; DefaultCompletion: TTransactionAction): ITransaction;
begin
  Result := TFB30Transaction.Create(Attachments,TPB,DefaultCompletion);
end;

function TFB30ClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: ITPB; DefaultCompletion: TTransactionAction): ITransaction;
begin
  Result := TFB30Transaction.Create(Attachments,TPB,DefaultCompletion);
end;

function TFB30ClientAPI.AllocateSPB: ISPB;
begin
  Result := TSPB.Create;
end;

function TFB30ClientAPI.GetServiceManager(ServerName: string;
  Protocol: TProtocol; SPB: ISPB): IServiceManager;
begin
  Result := TFBServiceManager.Create(ServerName,Protocol,SPB);
end;

function TFB30ClientAPI.HasServiceAPI: boolean;
begin
  Result := true;
end;

function TFB30ClientAPI.HasMasterIntf: boolean;
begin
  Result := MasterIntf <> nil;
end;

function TFB30ClientAPI.HasRollbackRetaining: boolean;
begin
  Result := true;
end;

function TFB30ClientAPI.GetImplementationVersion: string;
begin
  Result := Format('3.%d',[UtilIntf.GetClientVersion]);
end;

function TFB30ClientAPI.DecodeInteger(bufptr: PChar; len: short): integer;
var P: PChar;
begin
  Result := 0;
  P := Bufptr + len - 1;
  while P >= bufptr do
  begin
    Result := (Result shl 8 ) or byte(P^);
    Dec(P);
  end;
end;

procedure TFB30ClientAPI.SQLEncodeDate(aDate: TDateTime; bufptr: PChar);
var
  Yr, Mn, Dy: Word;
begin
   DecodeDate(aDate, Yr, Mn, Dy);
   PISC_Date(Bufptr)^ := UtilIntf.encodeDate(Yr, Mn, Dy);
end;

function TFB30ClientAPI.SQLDecodeDate(bufptr: PChar): TDateTime;
var
  Yr, Mn, Dy: Word;
begin
  UtilIntf.decodeDate(PISC_DATE(bufptr)^,@Yr, @Mn, @Dy);
  try
    result := EncodeDate(Word(Yr + 1900), Mn,Dy);
  except
    on E: EConvertError do begin
      IBError(ibxeInvalidDataConversion, [nil]);
    end;
  end;
end;

procedure TFB30ClientAPI.SQLEncodeTime(aTime: TDateTime; bufptr: PChar);
var
  Hr, Mt, S, Ms: Word;
begin
  DecodeTime(aTime, Hr, Mt, S, Ms);
  PISC_TIME(bufptr)^ :=  UtilIntf.encodeTime(Hr, Mt, S, Ms*10);
end;

function TFB30ClientAPI.SQLDecodeTime(bufptr: PChar): TDateTime;
var
  Hr, Mt, S, Ms: Word;
begin
  UtilIntf.decodeTime(PISC_TIME(bufptr)^,@Hr, @Mt, @S, @Ms);
  try
    Result := EncodeTime(Hr, Mt, S, Ms div 10);
  except
    on E: EConvertError do begin
      IBError(ibxeInvalidDataConversion, [nil]);
    end;
  end;
end;

procedure TFB30ClientAPI.SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PChar);
begin
  SQLEncodeDate(aDateTime,bufPtr);
  Inc(bufptr,sizeof(ISC_DATE));
  SQLEncodeTime(aDateTime,bufPtr);
end;

function TFB30ClientAPI.SQLDecodeDateTime(bufptr: PChar): TDateTime;
begin
  Result := SQLDecodeDate(bufPtr);
  Inc(bufptr,sizeof(ISC_DATE));
  Result += SQLDecodeTime(bufPtr);
end;

end.

