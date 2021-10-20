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
unit FB30ClientAPI;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, FBClientAPI, Firebird, IB, IBExternals, FmtBCD, FBClientLib,
  FBActivityMonitor;

type

  { TFB30Status }

  TFB30Status = class(TFBStatus,IStatus)
  protected
    FStatus: Firebird.IStatus;
    FDirty: boolean;
  public
    destructor Destroy; override;
    procedure Init;
    procedure FreeHandle;
    function InErrorState: boolean;
    function Warning: boolean;
    function GetStatus: Firebird.IStatus;
    function StatusVector: PStatusVector; override;
    property Dirty: boolean read FDirty;
  end;

  { TFB30StatusObject }

  TFB30StatusObject = class(TFB30Status)
  public
    constructor Create(aOwner: TFBClientAPI; status: Firebird.IStatus; prefix: Ansistring='');
  end;

  Tfb_get_master_interface = function: IMaster;
                              {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  { TFB30ClientAPI }

  TFB30ClientAPI = class(TFBClientAPI,IFirebirdAPI,IFBIMasterProvider)
  private
    FMaster: Firebird.IMaster;
    FUtil: Firebird.IUtil;
    FProvider: Firebird.IProvider;
    FConfigManager: Firebird.IConfigManager;
    FStatus: TFB30Status;
    FIsEmbeddedServer: boolean;
    FStatusIntf: IStatus;   {Keep a reference to the interface - automatic destroy
                             when this class is freed and last reference to IStatus
                             goes out of scope.}
    procedure CheckPlugins;
  public
    constructor Create(aFBLibrary: TFBLibrary);
    destructor Destroy; override;

    function StatusIntf: Firebird.IStatus;
    procedure Check4DataBaseError; overload;
    procedure Check4DataBaseError(st: Firebird.IStatus); overload;
    function InErrorState: boolean;
    function LoadInterface: boolean; override;
    procedure FBShutdown; override;
    function GetAPI: IFirebirdAPI; override;
    function Firebird4orLater: boolean;
    {$IFDEF UNIX}
    function GetFirebirdLibList: string; override;
    {$ENDIF}

  public
    {IFirebirdAPI}
    function GetStatus: IStatus; override;
    function AllocateDPB: IDPB;
    function AllocateTPB: ITPB;

    {Database connections}
    function OpenDatabase(DatabaseName: AnsiString; DPB: IDPB; RaiseExceptionOnConnectError: boolean=true): IAttachment;
    function CreateDatabase(DatabaseName: AnsiString; DPB: IDPB; RaiseExceptionOnError: boolean=true): IAttachment; overload;
    function CreateDatabase(sql: AnsiString; aSQLDialect: integer; RaiseExceptionOnError: boolean=true): IAttachment; overload;
    {Start Transaction against multiple databases}
    function StartTransaction(Attachments: array of IAttachment;
             TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction; overload;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: ITPB; DefaultCompletion: TTransactionCompletion): ITransaction; overload;

    {Service Manager}
    function AllocateSPB: ISPB;
    function GetServiceManager(ServerName: AnsiString; Protocol: TProtocol; SPB: ISPB): IServiceManager; overload;
    function GetServiceManager(ServerName: AnsiString; Port: Ansistring; Protocol: TProtocol; SPB: ISPB): IServiceManager; overload;

    {Information}
    function HasServiceAPI: boolean;
    function HasRollbackRetaining: boolean;
    function IsEmbeddedServer: boolean; override;
    function GetClientMajor: integer; override;
    function GetClientMinor: integer; override;
    function HasLocalTZDB: boolean; override;
    function HasTimeZoneSupport: boolean; override;
    function HasExtendedTZSupport: boolean; override;
    function HasInt128Support: boolean; override;
    function HasScollableCursors: boolean;

    {Firebird 3 API}
    function HasMasterIntf: boolean;
    function GetIMaster: TObject;

    {IFBIMasterProvider}
    function GetIMasterIntf: Firebird.IMaster;

    {Encode/Decode}
    procedure SQLEncodeDate(aDate: TDateTime; bufptr: PByte); override;
    function SQLDecodeDate(bufptr: PByte): TDateTime; override;
    procedure SQLEncodeTime(aTime: TDateTime; bufptr: PByte); override;
    function SQLDecodeTime(bufptr: PByte): TDateTime;  override;
    procedure SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PByte); override;
    function SQLDecodeDateTime(bufptr: PByte): TDateTime; override;
    function FormatStatus(Status: TFBStatus): AnsiString; override;
    function FormatFBStatus(Status: Firebird.IStatus): AnsiString;

    {Firebird 4 Extensions}
    procedure SQLDecFloatEncode(aValue: tBCD; SQLType: cardinal; bufptr: PByte);
      override;
    function SQLDecFloatDecode(SQLType: cardinal; bufptr: PByte): tBCD; override;
    function Int128ToStr(bufptr: PByte; scale: integer): AnsiString; override;
    procedure StrToInt128(scale: integer; aValue: AnsiString; bufptr: PByte); override;

    {Firebird Interfaces}
    property MasterIntf: Firebird.IMaster read FMaster;
    property UtilIntf: Firebird.IUtil read FUtil;
    property ProviderIntf: Firebird.IProvider read FProvider;

  end;

  { TXPBParameterBlock }

  TXPBParameterBlock = class(TFBInterfacedObject)
  private
    FBuilder: Firebird.IXpbBuilder;
    FFirebird30ClientAPI: TFB30ClientAPI;
  public
    constructor Create(api: TFB30ClientAPI; kind: cardinal);
    destructor Destroy; override;
    function getBuffer: PByte;
    function getDataLength: cardinal;
    procedure insertInt(tag: Byte; value: Integer);
    property Builder: Firebird.IXpbBuilder read FBuilder;
  public
    procedure PrintBuf;
  end;

implementation

uses FB30Attachment, {$IFDEF FPC}dynlibs{$ELSE} windows{$ENDIF},
     FBMessages, FB30Services, FB30Transaction, IBUtils, DateUtils,
     FBAttachment, FBTransaction, FBServices;

type
  PISC_DATE = ^ISC_DATE;
  PISC_TIME = ^ISC_TIME;

{ TXPBParameterBlock }

constructor TXPBParameterBlock.Create(api: TFB30ClientAPI; kind: cardinal);
begin
  inherited Create;
  FFirebird30ClientAPI := api;
  with FFirebird30ClientAPI do
  begin
    FBuilder := UtilIntf.getXpbBuilder(StatusIntf,kind,nil,0);
    Check4DataBaseError;
  end;
end;

destructor TXPBParameterBlock.Destroy;
begin
  if FBuilder <> nil then
  begin
    FBuilder.dispose;
    FBuilder := nil;
  end;
  inherited Destroy;
end;

function TXPBParameterBlock.getBuffer: PByte;
begin
  with FFirebird30ClientAPI do
  begin
    Result := PByte(FBuilder.getBuffer(StatusIntf));
    Check4DataBaseError;
  end;
end;

function TXPBParameterBlock.getDataLength: cardinal;
begin
  with FFirebird30ClientAPI do
  begin
    Result := FBuilder.getBufferLength(StatusIntf);
    Check4DataBaseError;
  end;
end;

procedure TXPBParameterBlock.insertInt(tag: Byte; value: Integer);
begin
  with FFirebird30ClientAPI do
  begin
    Builder.insertInt(StatusIntf,tag,value);
    Check4DataBaseError;
  end;
end;

procedure TXPBParameterBlock.PrintBuf;
var i: integer;
    buffer: PByte;
begin
  write(ClassName,': (',getDataLength,') ');
  buffer := getBuffer;
  for i := 0 to getDataLength - 1 do
  begin
    write(Format('%x ',[buffer^]));
    Inc(buffer);
  end;
  writeln
end;

{ TFB30StatusObject }

constructor TFB30StatusObject.Create(aOwner: TFBClientAPI;
  status: Firebird.IStatus; prefix: Ansistring);
begin
  inherited Create(aOwner,prefix);
  FStatus := status;
end;

{ TFB30Status }

destructor TFB30Status.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

procedure TFB30Status.Init;
begin
  if assigned(FStatus) and Dirty then
  begin
    FStatus.Init;
    FDirty := false;
  end;
end;

procedure TFB30Status.FreeHandle;
begin
  if FStatus <> nil then
  begin
    FStatus.dispose;
    FStatus := nil;
  end;
end;

function TFB30Status.InErrorState: boolean;
begin
  with GetStatus do
    Result := ((getState and STATE_ERRORS) <> 0);
  if Result then
    FDirty := true;
end;

function TFB30Status.Warning: boolean;
begin
  with GetStatus do
    Result := ((getState and STATE_WARNINGS) <> 0);
  if Result then
    FDirty := true;
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

procedure TFB30ClientAPI.CheckPlugins;
var FBConf: Firebird.IFirebirdConf;
    Plugins: AnsiString;
    PluginsList: TStringList;
begin
  FIsEmbeddedServer := false;
  FBConf := FConfigManager.getFirebirdConf;
  try
    Plugins := FBConf.asString(FBConf.getKey('Providers'));
  finally
    FBConf.release;
  end;
  if Plugins = '' then Exit;

  PluginsList := TStringList.Create;
  try
    PluginsList.CommaText := Plugins;
    FIsEmbeddedServer := (PluginsList.IndexOf('Engine12') <> -1) or {Firebird 3}
                         (PluginsList.IndexOf('Engine13') <> -1); {Firebird 4}
  finally
    PluginsList.Free;
  end;
end;

function TFB30ClientAPI.Firebird4orLater: boolean;
begin
  Result :=  (GetClientMajor > 4) or (
    (GetClientMajor = 4) and (UtilIntf.vtable.version >= 4)
    and (UtilIntf.vtable.version <> 21) {ignore FB4 Beta1}
    and (UtilIntf.vtable.version <> 24)) {ignore FB4 Beta2}
end;

{$IFDEF UNIX}
function TFB30ClientAPI.GetFirebirdLibList: string;
begin
  Result := 'libfbclient.so:libfbclient.so.2';
end;
{$ENDIF}

function TFB30ClientAPI.LoadInterface: boolean;
var
  fb_get_master_interface: Tfb_get_master_interface;
begin
  Result := inherited LoadInterface;
  fb_get_master_interface := GetProcAddress(GetFBLibrary.GetHandle, 'fb_get_master_interface'); {do not localize}
  if assigned(fb_get_master_interface) then
  begin
    FMaster := fb_get_master_interface;
    FUtil := FMaster.getUtilInterface;
    FProvider := FMaster.getDispatcher;
    FConfigManager := FMaster.getConfigManager;
    CheckPlugins;
  end;
  Result := Result and HasMasterIntf;
end;

procedure TFB30ClientAPI.FBShutdown;
begin
  if assigned(fb_shutdown) then
  begin
    FStatus.FreeHandle;
    if assigned(FProvider) then
    begin
      FProvider.release;
      FProvider := nil;
    end;
  end;
  inherited;
end;

function TFB30ClientAPI.GetAPI: IFirebirdAPI;
begin
  Result := self;
end;

constructor TFB30ClientAPI.Create(aFBLibrary: TFBLibrary);
begin
  inherited Create(aFBLibrary);
  FStatus := TFB30Status.Create(self);
  FStatusIntf := FStatus;
end;

destructor TFB30ClientAPI.Destroy;
begin
  FStatus.FreeHandle;
  if assigned(FProvider) then
    FProvider.release;
  inherited Destroy;
end;

function TFB30ClientAPI.StatusIntf: Firebird.IStatus;
begin
  Result := FStatus.GetStatus;
  Result.Init;
end;

procedure TFB30ClientAPI.Check4DataBaseError;
begin
  if FStatus.InErrorState then
    IBDataBaseError;
end;

procedure TFB30ClientAPI.Check4DataBaseError(st: Firebird.IStatus);
begin
  if ((st.getState and st.STATE_ERRORS) <> 0) then
    raise EIBInterBaseError.Create(TFB30StatusObject.Create(self,st));
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
  Result := TDPB.Create(self);
end;

function TFB30ClientAPI.AllocateTPB: ITPB;
begin
  Result := TTPB.Create(self);
end;

function TFB30ClientAPI.OpenDatabase(DatabaseName: AnsiString; DPB: IDPB;
  RaiseExceptionOnConnectError: boolean): IAttachment;
begin
  Result := TFB30Attachment.Create(self,DatabaseName, DPB, RaiseExceptionOnConnectError);
  if not Result.IsConnected then
    Result := nil;
end;

function TFB30ClientAPI.CreateDatabase(DatabaseName: AnsiString; DPB: IDPB;
  RaiseExceptionOnError: boolean): IAttachment;
begin
  Result := TFB30Attachment.CreateDatabase(self,DatabaseName,DPB, RaiseExceptionOnError);
  if not Result.IsConnected then
    Result := nil;
end;

function TFB30ClientAPI.CreateDatabase(sql: AnsiString; aSQLDialect: integer;
  RaiseExceptionOnError: boolean): IAttachment;
begin
  Result := TFB30Attachment.CreateDatabase(self,sql,aSQLDialect, RaiseExceptionOnError);
  if not Result.IsConnected then
    Result := nil;
end;

function TFB30ClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction;
begin
  Result := TFB30Transaction.Create(self,Attachments,TPB,DefaultCompletion);
end;

function TFB30ClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: ITPB; DefaultCompletion: TTransactionCompletion): ITransaction;
begin
  Result := TFB30Transaction.Create(self,Attachments,TPB,DefaultCompletion);
end;

function TFB30ClientAPI.AllocateSPB: ISPB;
begin
  Result := TSPB.Create(self);
end;

function TFB30ClientAPI.GetServiceManager(ServerName: AnsiString;
  Protocol: TProtocol; SPB: ISPB): IServiceManager;
begin
  Result := TFB30ServiceManager.Create(self,ServerName,Protocol,SPB);
end;

function TFB30ClientAPI.GetServiceManager(ServerName: AnsiString;
  Port: Ansistring; Protocol: TProtocol; SPB: ISPB): IServiceManager;
begin
  Result := TFB30ServiceManager.Create(self,ServerName,Protocol,SPB,Port);
end;

function TFB30ClientAPI.HasServiceAPI: boolean;
begin
  Result := true;
end;

function TFB30ClientAPI.HasMasterIntf: boolean;
begin
  Result := MasterIntf <> nil;
end;

function TFB30ClientAPI.GetIMaster: TObject;
begin
  Result := FMaster;
end;

function TFB30ClientAPI.GetIMasterIntf: Firebird.IMaster;
begin
  Result := FMaster;
end;

function TFB30ClientAPI.HasRollbackRetaining: boolean;
begin
  Result := true;
end;

function TFB30ClientAPI.IsEmbeddedServer: boolean;
begin
  Result := FIsEmbeddedServer;
end;

function TFB30ClientAPI.GetClientMajor: integer;
begin
  Result := UtilIntf.GetClientVersion div 256;
end;

function TFB30ClientAPI.GetClientMinor: integer;
begin
  Result := UtilIntf.GetClientVersion mod 256;
end;

procedure TFB30ClientAPI.SQLEncodeDate(aDate: TDateTime; bufptr: PByte);
var
  Yr, Mn, Dy: Word;
begin
   DecodeDate(aDate, Yr, Mn, Dy);
   PISC_Date(Bufptr)^ := UtilIntf.encodeDate(Yr, Mn, Dy);
end;

function TFB30ClientAPI.SQLDecodeDate(bufptr: PByte): TDateTime;
var
  Yr, Mn, Dy: cardinal;
begin
  UtilIntf.decodeDate(PISC_DATE(bufptr)^,@Yr, @Mn, @Dy);
  try
    result := EncodeDate(Yr, Mn,Dy);
  except
    on E: EConvertError do begin
      IBError(ibxeInvalidDataConversion, [nil]);
    end;
  end;
end;

procedure TFB30ClientAPI.SQLEncodeTime(aTime: TDateTime; bufptr: PByte);
var
  Hr, Mt, S: word;
  DMs: cardinal;
begin
  FBDecodeTime(aTime,Hr, Mt, S, DMs);
  PISC_TIME(bufptr)^ :=  UtilIntf.encodeTime(Hr, Mt, S, DMs);
end;

function TFB30ClientAPI.SQLDecodeTime(bufptr: PByte): TDateTime;
var
  Hr, Mt, S, DMs: cardinal;
begin
  UtilIntf.decodeTime(PISC_TIME(bufptr)^,@Hr, @Mt, @S, @DMs);
  try
    Result := FBEncodeTime(Hr, Mt, S, DMs);
  except
    on E: EConvertError do begin
      IBError(ibxeInvalidDataConversion, [nil]);
    end;
  end;
end;

procedure TFB30ClientAPI.SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PByte);
begin
  SQLEncodeDate(aDateTime,bufPtr);
  Inc(bufptr,sizeof(ISC_DATE));
  SQLEncodeTime(aDateTime,bufPtr);
end;

function TFB30ClientAPI.SQLDecodeDateTime(bufptr: PByte): TDateTime;
begin
  Result := SQLDecodeDate(bufPtr);
  Inc(bufptr,sizeof(ISC_DATE));
  Result := Result + SQLDecodeTime(bufPtr);
end;

function TFB30ClientAPI.FormatStatus(Status: TFBStatus): AnsiString;
begin
  Result := FormatFBStatus((Status as TFB30Status).GetStatus);
end;

function TFB30ClientAPI.FormatFBStatus(Status: Firebird.IStatus): AnsiString;
var local_buffer: array[0..IBHugeLocalBufferLength - 1] of AnsiChar;
begin
  Result := '';
  if UtilIntf.formatStatus(@local_buffer,sizeof(local_buffer) - 1,Status) > 0 then
    Result := strpas(local_buffer);
end;

procedure TFB30ClientAPI.SQLDecFloatEncode(aValue: tBCD; SQLType: cardinal;
  bufptr: PByte);
var DecFloat16: IDecFloat16;
    DecFloat34: IDecFloat34;
    sign: integer;
    exp: integer;
    buffer: array [1..34] of byte;

    procedure UnpackBuffer(width: integer);
    var i,j: integer;
    begin
      Fillchar(buffer,sizeof(buffer),0);
      if BCDPrecision(aValue) > width then
        IBError(ibxeBCDTooBig,[BCDPrecision(aValue),width]);
      j := 1 + (width - aValue.Precision);
      for i := 0 to (aValue.Precision - 1) div 2 do
      if j <= width then
      begin
          buffer[j] := (aValue.Fraction[i] and $f0) shr 4;
          Inc(j);
          if j <= width then
          begin
            buffer[j] := (aValue.Fraction[i] and $0f);
            Inc(j);
          end;
      end;
      {writeln('Precision = ',aValue.Precision,' Places = ',aValue.SignSpecialPlaces and $2f);
      write('BCD Buffer = ');
      for i := 1 to 34 do
        write(buffer[i],' ');
      writeln; }
    end;

begin
  inherited SQLDecFloatEncode(aValue, SQLType, bufptr);
  sign := (aValue.SignSpecialPlaces and $80) shr 7;
  exp := -(aValue.SignSpecialPlaces and $2f);

  case SQLType of
  SQL_DEC16:
    begin
      UnPackbuffer(16);
      DecFloat16 := UtilIntf.getDecFloat16(StatusIntf);
      Check4DataBaseError;
      DecFloat16.fromBcd(sign,@buffer,exp,FB_DEC16Ptr(bufptr));
      Check4DataBaseError;
    end;

  SQL_DEC34:
    begin
      UnPackbuffer(34);
      DecFloat34 := UtilIntf.getDecFloat34(StatusIntf);
      Check4DataBaseError;
      DecFloat34.fromBcd(sign,@buffer,exp,FB_DEC34Ptr(bufptr));
      Check4DataBaseError;
    end;

  else
    IBError(ibxeInvalidDataConversion,[]);
  end;
end;

function TFB30ClientAPI.SQLDecFloatDecode(SQLType: cardinal; bufptr: PByte): tBCD;

var DecFloat16: IDecFloat16;
    DecFloat34: IDecFloat34;
    sign: integer;
    exp: integer;
    buffer: array [1..38] of byte;

  procedure packbuffer(buflen: integer);
  var i,j: integer;
  begin
{    write('Decode: BCD Buffer = ');
    for i := 1 to 34 do
      write(buffer[i],' ');
    writeln; }
    {pack buffer}
    i := 1;
    while (i <= buflen) and (buffer[i] = 0) do  {skip leading zeroes}
      inc(i);

    j := 0;
    Result.Precision := 0;
    while i <= buflen do
    begin
      inc(Result.Precision);
      if odd(Result.Precision) then
        Result.Fraction[j] := (buffer[i] and $0f) shl 4
      else
      begin
        Result.Fraction[j] := Result.Fraction[j] or (buffer[i] and $0f);
        Inc(j);
      end;
      inc(i);
    end;
  end;

begin
  Result := inherited SQLDecFloatDecode(SQLType, bufptr);
  FillChar(Result, sizeof(tBCD),0);
  case SQLType of
  SQL_DEC16:
    begin
      DecFloat16 := UtilIntf.getDecFloat16(StatusIntf);
      Check4DataBaseError;
      DecFloat16.toBcd(FB_DEC16Ptr(bufptr),@sign,@buffer,@exp);
      Check4DataBaseError;
      packbuffer(16);
    end;

  SQL_DEC34:
    begin
      DecFloat34 := UtilIntf.getDecFloat34(StatusIntf);
      Check4DataBaseError;
      DecFloat34.toBcd(FB_DEC34Ptr(bufptr),@sign,@buffer,@exp);
      Check4DataBaseError;
      packbuffer(34);
    end;

  else
    IBError(ibxeInvalidDataConversion,[]);
  end;
  Result.SignSpecialPlaces :=  (-exp and $2f);
  if sign <> 0 then
    Result.SignSpecialPlaces := Result.SignSpecialPlaces or $80;
end;

procedure TFB30ClientAPI.StrToInt128(scale: integer; aValue: AnsiString;
  bufptr: PByte);
begin
  inherited StrToInt128(scale,aValue,bufPtr);

  UtilIntf.getInt128(StatusIntf).fromString(StatusIntf,scale,PAnsiChar(aValue),FB_I128Ptr(bufptr));
  Check4DatabaseError;
end;

function TFB30ClientAPI.Int128ToStr(bufptr: PByte; scale: integer
  ): AnsiString;
const
  bufLength = 64;
var Buffer: array[ 0.. bufLength] of AnsiChar;
begin
  Result := inherited Int128ToStr(bufPtr,scale);

  UtilIntf.getInt128(StatusIntf).toString(StatusIntf,FB_I128Ptr(bufptr),scale,buflength,PAnsiChar(@Buffer));
  Check4DatabaseError;
  Result := strpas(PAnsiChar(@Buffer));
end;

function TFB30ClientAPI.HasLocalTZDB: boolean;
const
  bufLength = 128;
var Buffer: ISC_TIME_TZ;
    Hr, Mt, S, DMs: cardinal;
    tzBuffer: array[ 0.. bufLength] of AnsiChar;
begin
  Result := HasTimeZoneSupport;
  if Result then
  begin
    Buffer.utc_time := 0;
    Buffer.time_zone := TimeZoneID_GMT;
    UtilIntf.decodeTimeTz(StatusIntf, ISC_TIME_TZPtr(@Buffer),@Hr, @Mt, @S, @DMs,bufLength,PAnsiChar(@tzBuffer));
    Check4DataBaseError;
    Result := strpas(PAnsiChar(@tzBuffer)) <> 'GMT*';
  end;
end;

function TFB30ClientAPI.HasTimeZoneSupport: boolean;
begin
  Result := Firebird4orLater;
end;

function TFB30ClientAPI.HasExtendedTZSupport: boolean;
begin
  Result := Firebird4orLater;
end;

function TFB30ClientAPI.HasInt128Support: boolean;
begin
  Result := Firebird4orLater;
end;

function TFB30ClientAPI.HasScollableCursors: boolean;
begin
  Result := true;
end;

end.


