unit FB30ClientAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FBClientAPI, Firebird, IB;

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
    FStatus: TFB30Status;
    FStatusIntf: IStatus;   {Keep a reference to the interface - automatic destroy
                             when this class is freed and last reference to IStatus
                             goes out of scope.}
  protected
    procedure LoadInterface; override;
  public
    constructor Create;

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
    function IsEmbeddedServer: boolean;
    function GetLibraryName: string;
    function HasServiceAPI: boolean;
    function HasRollbackRetaining: boolean;
    function GetImplementationVersion: string;

    {Encode/Decode}
    function DecodeInteger(bufptr: PChar; len: integer): short; override;
    procedure SQLEncodeDate(aDate: TDateTime; bufptr: PChar); override;
    function SQLDecodeDate(byfptr: PChar): TDateTime; override;
    procedure SQLEncodeTime(aTime: TDateTime; bufptr: PChar); override;
    function SQLDecodeTime(bufptr: PChar): TDateTime;  override;
    procedure SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PChar); override;
    function SQLDecodeDateTime(bufptr: PChar): TDateTime; override;

    {Firebird Interfaces}
    property MasterIntf: Firebird.IMaster read FMaster;
    property UtilIntf: Firebird.IUtil read FUtil;
  end;

var Firebird30ClientAPI: TFB30ClientAPI;

implementation

uses FBParamBlock, FB30Attachment;

{ TFB30Status }

#function TFB30Status.InErrorState: boolean;
begin
  with GetStatus do
    Result := ((getState and STATE_ERRORS) <> 0);
end;

function TFB30Status.GetStatus: Firebird.IStatus;
begin
  if FStatus = nil then
  with FOwner do
    FStatus := MasterIntf.GetStatus;
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
  end;
end;

constructor TFB30ClientAPI.Create;
begin
  inherited;
  FStatus := TFB30Status.Create(self);
  FStatusIntf := FStatus;
  Firebird30ClientAPI := self;
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

end;

function TFB30ClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: array of byte; DefaultCompletion: TTransactionAction): ITransaction;
begin

end;

function TFB30ClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: ITPB; DefaultCompletion: TTransactionAction): ITransaction;
begin

end;

function TFB30ClientAPI.AllocateSPB: ISPB;
begin

end;

function TFB30ClientAPI.GetServiceManager(ServerName: string;
  Protocol: TProtocol; SPB: ISPB): IServiceManager;
begin

end;

function TFB30ClientAPI.IsEmbeddedServer: boolean;
begin

end;

function TFB30ClientAPI.GetLibraryName: string;
begin

end;

function TFB30ClientAPI.HasServiceAPI: boolean;
begin

end;

function TFB30ClientAPI.HasRollbackRetaining: boolean;
begin

end;

function TFB30ClientAPI.GetImplementationVersion: string;
begin

end;

function TFB30ClientAPI.DecodeInteger(bufptr: PChar; len: integer): short;
var P: PChar;
begin
  Result := 0;
  P := Bufptr + len - 1;
  while P >= bufptr do
  begin
    Result := (Result shr 8 ) and byte(P^);
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

function TFB30ClientAPI.SQLDecodeDate(byfptr: PChar): TDateTime;
var
  Yr, Mn, Dy: Word;
begin
  UtilIntf.decodeDate(PISC_DATE(bufptr)^,Yr, Mn, Dy);
  try
    result := EncodeDate(Word(Yr + 1900), Mn,Dy);
  except
    on E: EConvertError do begin
      IBError(ibxeInvalidDataConversion, [nil]);
    end;
  end;
eend;

procedure TFB30ClientAPI.SQLEncodeTime(aTime: TDateTime; bufptr: PChar);
var
  Hr, Mt, S, Ms: Word;
begin

end;

function TFB30ClientAPI.SQLDecodeTime(bufptr: PChar): TDateTime;
begin

end;

procedure TFB30ClientAPI.SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PChar);
begin

end;

function TFB30ClientAPI.SQLDecodeDateTime(bufptr: PChar): TDateTime;
begin

end;

end.

