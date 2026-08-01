(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  This file is part of the pure Pascal wire protocol implementation
 *  (no fbclient required) and is subject to the Initial Developer's
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
 *  The Initial Developer of the Original Code is MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBWireClientAPI;

{ The IFirebirdAPI implementation for the pure Pascal wire protocol client.

  Unlike the 2.5 and 3.0 providers this one loads no client library: every
  call is a packet exchange with the server. Consequently:

  - date and time conversions are performed arithmetically here instead of
    by isc_encode_* or IUtil
  - error message text is built from the status vector strings that the
    server itself provides, because firebird.msg is a client library
    resource and is not available
  - IsEmbeddedServer is always false and there is no IMaster interface

  The provider is obtained with TFBWireClientAPI.Create(nil) or, more
  usually, through the WireFirebirdAPI function below.
}

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
  Classes, SysUtils, IB, FBClientAPI, IBExternals, IBHeader, FBActivityMonitor,
  FBWireProtocol, FBWireStream;

const
  {the wire client reports itself with the highest protocol it implements}
  WireClientMajorVersion = 5;
  WireClientMinorVersion = 0;

type
  TFBWireClientAPI = class;

  { TFBWireStatus }

  TFBWireStatus = class(TFBStatus,IStatus)
  private
    FStatusVector: TStatusVector;
    FMessage: AnsiString;
  protected
    function GetIBMessage(CodePage: TSystemCodePage): AnsiString; override;
  public
    constructor Create(aOwner: TFBClientAPI; prefix: AnsiString = '');
    constructor Copy(src: TFBWireStatus);
    function StatusVector: PStatusVector; override;
    function Clone: IStatus; override;
    function InErrorState: boolean; override;
    procedure Clear;
    {builds an ISC status vector and the message text from a decoded wire
     status vector}
    procedure SetFromWireStatus(const aStatus: TWireStatusVector);
    procedure SetError(aErrorCode: TStatusCode; const aMessage: AnsiString);
  end;

  { TFBWireClientAPI }

  TFBWireClientAPI = class(TFBClientAPI,IFirebirdAPI)
  private
    FStatus: TFBWireStatus;
    FStatusIntf: IStatus;   {keeps FStatus alive}
  public
    constructor Create(aFBLibrary: TFBLibrary);
    destructor Destroy; override;

    {TFBClientAPI}
    function LoadInterface: boolean; override;
    function GetAPI: IFirebirdAPI; override;
    {$IFDEF UNIX}
    function GetFirebirdLibList: string; override;
    {$ENDIF}
    procedure SQLEncodeDate(aDate: TDateTime; bufptr: PByte); override;
    function SQLDecodeDate(bufptr: PByte): TDateTime; override;
    procedure SQLEncodeTime(aTime: TDateTime; bufptr: PByte); override;
    function SQLDecodeTime(bufptr: PByte): TDateTime; override;
    procedure SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PByte); override;
    function SQLDecodeDateTime(bufptr: PByte): TDateTime; override;
    function HasInt128Support: boolean; override;
    function HasTimeZoneSupport: boolean; override;

    {the working status object - the wire objects report errors through it}
    property WireStatus: TFBWireStatus read FStatus;

  public
    {IFirebirdAPI}
    function AllocateDPB: IDPB;
    function OpenDatabase(DatabaseName: AnsiString; DPB: IDPB;
                RaiseExceptionOnConnectError: boolean = true): IAttachment;
    function CreateDatabase(DatabaseName: AnsiString; DPB: IDPB;
                RaiseExceptionOnError: boolean = true): IAttachment; overload;
    function CreateDatabase(sql: AnsiString; aSQLDialect: integer;
                RaiseExceptionOnError: boolean = true): IAttachment; overload;
    function AllocateTPB: ITPB;
    function StartTransaction(Attachments: array of IAttachment;
                TPB: array of byte; DefaultCompletion: TTransactionCompletion = taCommit;
                aName: AnsiString = ''): ITransaction; overload;
    function StartTransaction(Attachments: array of IAttachment; TPB: ITPB;
                DefaultCompletion: TTransactionCompletion = taCommit;
                aName: AnsiString = ''): ITransaction; overload;
    function HasServiceAPI: boolean;
    function AllocateSPB: ISPB;
    function GetServiceManager(ServerName: AnsiString; Protocol: TProtocol;
                SPB: ISPB): IServiceManager; overload;
    function GetServiceManager(ServerName: AnsiString; Port: AnsiString;
                Protocol: TProtocol; SPB: ISPB): IServiceManager; overload;
    function GetStatus: IStatus; override;
    function HasRollbackRetaining: boolean;
    function IsEmbeddedServer: boolean; override;
    function GetClientMajor: integer; override;
    function GetClientMinor: integer; override;
    function HasLocalTZDB: boolean; override;
    function HasExtendedTZSupport: boolean; override;
    function HasMasterIntf: boolean;
  end;

{The wire protocol provider. Unlike IB.FirebirdAPI this never loads a
 client library, so it is available even where fbclient is not installed.}
function WireFirebirdAPI: IFirebirdAPI;

{raises an EIBInterBaseError built from a wire protocol error}
procedure WireIBError(aAPI: TFBWireClientAPI; E: Exception);

{copies a DPB/TPB/SPB/BPB clumplet buffer into a byte array ready to be
 sent. The parameter block interfaces do not expose the raw buffer, so the
 implementation class is used.}
function ParamBlockToBytes(aBlock: IUnknown): TBytes;

implementation

uses FBMessages, IBErrorCodes, FBParamBlock, FBAttachment, FBTransaction,
  IBUtils, FBServices, FBWireAttachment, FBWireServices;

const
  {days between the Delphi TDateTime zero (1899-12-30) and the Firebird
   ISC_DATE zero (17 November 1858, the modified Julian day epoch)}
  FBDateDelta = 15018;

var
  FWireFirebirdAPI: IFirebirdAPI;

function WireFirebirdAPI: IFirebirdAPI;
begin
  if FWireFirebirdAPI = nil then
    FWireFirebirdAPI := TFBWireClientAPI.Create(nil) as IFirebirdAPI;
  Result := FWireFirebirdAPI;
end;

procedure WireIBError(aAPI: TFBWireClientAPI; E: Exception);
begin
  if E is EFBWireProtocolError then
  begin
    aAPI.WireStatus.SetFromWireStatus(EFBWireProtocolError(E).Status);
    raise EIBInterBaseError.Create(aAPI.GetStatus,CP_ACP);
  end
  else
  if E is EFBWireError then
  begin
    aAPI.WireStatus.SetError(isc_network_error,E.Message);
    raise EIBInterBaseError.Create(aAPI.GetStatus,CP_ACP);
  end
  else
    raise E;
end;

function ParamBlockToBytes(aBlock: IUnknown): TBytes;
var Block: TParamBlock;
    p: PByte;
    i: integer;
begin
  SetLength(Result,0);
  if aBlock = nil then Exit;
  Block := aBlock as TObject as TParamBlock;
  SetLength(Result,Block.getDataLength);
  p := Block.getBuffer;
  for i := 0 to Length(Result) - 1 do
    Result[i] := p[i];
end;

{ TFBWireStatus }

constructor TFBWireStatus.Create(aOwner: TFBClientAPI; prefix: AnsiString);
begin
  inherited Create(aOwner,prefix);
  Clear;
end;

constructor TFBWireStatus.Copy(src: TFBWireStatus);
begin
  inherited Copy(src);
  FStatusVector := src.FStatusVector;
  FMessage := src.FMessage;
end;

procedure TFBWireStatus.Clear;
begin
  FillChar(FStatusVector,SizeOf(FStatusVector),0);
  FStatusVector[0] := isc_arg_gds;
  FStatusVector[1] := 0;
  FStatusVector[2] := isc_arg_end;
  FMessage := '';
end;

function TFBWireStatus.GetIBMessage(CodePage: TSystemCodePage): AnsiString;
begin
  {The engine sends interpreted message text with most errors. When it does
   not, all we can offer is the error code itself: firebird.msg belongs to
   the client library which this provider deliberately does not use.}
  Result := FMessage;
  if Result = '' then
    Result := Format('Firebird Error Code: %d',[FStatusVector[1]]);
end;

function TFBWireStatus.StatusVector: PStatusVector;
begin
  Result := @FStatusVector;
end;

function TFBWireStatus.Clone: IStatus;
begin
  Result := TFBWireStatus.Copy(self);
end;

function TFBWireStatus.InErrorState: boolean;
begin
  Result := (FStatusVector[0] = isc_arg_gds) and (FStatusVector[1] <> 0);
end;

procedure TFBWireStatus.SetFromWireStatus(const aStatus: TWireStatusVector);
var i, v: integer;
begin
  Clear;
  v := 0;
  FMessage := '';
  for i := 0 to Length(aStatus) - 1 do
  begin
    {leave room for the isc_arg_end terminator}
    if v > High(FStatusVector) - 2 then
      break;
    case aStatus[i].Kind of
    isc_arg_gds, isc_arg_number, isc_arg_warning:
      begin
        FStatusVector[v] := aStatus[i].Kind;
        FStatusVector[v+1] := NativeInt(aStatus[i].IntValue);
        Inc(v,2);
      end;
    isc_arg_string, isc_arg_interpreted, isc_arg_sql_state:
      begin
        {the string must outlive this call - the status vector holds a
         pointer to it, so keep it in FMessage}
        if FMessage <> '' then
          FMessage := FMessage + LineEnding + '-';
        FMessage := FMessage + aStatus[i].StrValue;
      end;
    end;
  end;
  FStatusVector[v] := isc_arg_end;
end;

procedure TFBWireStatus.SetError(aErrorCode: TStatusCode;
  const aMessage: AnsiString);
begin
  Clear;
  FStatusVector[0] := isc_arg_gds;
  FStatusVector[1] := aErrorCode;
  FStatusVector[2] := isc_arg_end;
  FMessage := aMessage;
end;

{ TFBWireClientAPI }

constructor TFBWireClientAPI.Create(aFBLibrary: TFBLibrary);
begin
  inherited Create(aFBLibrary);
  FStatus := TFBWireStatus.Create(self);
  FStatusIntf := FStatus;
end;

destructor TFBWireClientAPI.Destroy;
begin
  FStatusIntf := nil;
  inherited Destroy;
end;

function TFBWireClientAPI.LoadInterface: boolean;
begin
  {nothing to load - there is no client library}
  Result := true;
end;

function TFBWireClientAPI.GetAPI: IFirebirdAPI;
begin
  Result := self as IFirebirdAPI;
end;

{$IFDEF UNIX}
function TFBWireClientAPI.GetFirebirdLibList: string;
begin
  Result := '';
end;
{$ENDIF}

procedure TFBWireClientAPI.SQLEncodeDate(aDate: TDateTime; bufptr: PByte);
begin
  PISC_DATE(bufptr)^ := Trunc(aDate) + FBDateDelta;
end;

function TFBWireClientAPI.SQLDecodeDate(bufptr: PByte): TDateTime;
begin
  Result := PISC_DATE(bufptr)^ - FBDateDelta;
end;

procedure TFBWireClientAPI.SQLEncodeTime(aTime: TDateTime; bufptr: PByte);
var Hr, Mt, S: word;
    DMs: cardinal;
begin
  FBDecodeTime(aTime,Hr,Mt,S,DMs);
  PISC_TIME(bufptr)^ := cardinal(Hr)*36000000 + cardinal(Mt)*600000 +
                        cardinal(S)*10000 + DMs;
end;

function TFBWireClientAPI.SQLDecodeTime(bufptr: PByte): TDateTime;
var t: cardinal;
begin
  t := PISC_TIME(bufptr)^;
  Result := FBEncodeTime(t div 36000000,(t div 600000) mod 60,
                         (t div 10000) mod 60,t mod 10000);
end;

procedure TFBWireClientAPI.SQLEncodeDateTime(aDateTime: TDateTime; bufptr: PByte);
begin
  SQLEncodeDate(aDateTime,bufptr);
  Inc(bufptr,SizeOf(ISC_DATE));
  SQLEncodeTime(aDateTime,bufptr);
end;

function TFBWireClientAPI.SQLDecodeDateTime(bufptr: PByte): TDateTime;
var aDate: TDateTime;
begin
  aDate := SQLDecodeDate(bufptr);
  Inc(bufptr,SizeOf(ISC_DATE));
  {negative dates count the time backwards from the date}
  if aDate < 0 then
    Result := aDate - SQLDecodeTime(bufptr)
  else
    Result := aDate + SQLDecodeTime(bufptr);
end;

function TFBWireClientAPI.HasInt128Support: boolean;
begin
  Result := true;
end;

function TFBWireClientAPI.HasTimeZoneSupport: boolean;
begin
  Result := true;
end;

function TFBWireClientAPI.AllocateDPB: IDPB;
begin
  Result := TDPB.Create(self);
end;

function TFBWireClientAPI.OpenDatabase(DatabaseName: AnsiString; DPB: IDPB;
  RaiseExceptionOnConnectError: boolean): IAttachment;
begin
  Result := TFBWireAttachment.Create(self,DatabaseName,DPB,
                                     RaiseExceptionOnConnectError);
  if not Result.IsConnected then
    Result := nil;
end;

function TFBWireClientAPI.CreateDatabase(DatabaseName: AnsiString; DPB: IDPB;
  RaiseExceptionOnError: boolean): IAttachment;
begin
  Result := TFBWireAttachment.CreateDatabase(self,DatabaseName,DPB,
                                             RaiseExceptionOnError);
  if not Result.IsConnected then
    Result := nil;
end;

function TFBWireClientAPI.CreateDatabase(sql: AnsiString; aSQLDialect: integer;
  RaiseExceptionOnError: boolean): IAttachment;
begin
  Result := TFBWireAttachment.CreateDatabase(self,sql,aSQLDialect,
                                             RaiseExceptionOnError);
  if (Result <> nil) and not Result.IsConnected then
    Result := nil;
end;

function TFBWireClientAPI.AllocateTPB: ITPB;
begin
  Result := TTPB.Create(self);
end;

function TFBWireClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: array of byte; DefaultCompletion: TTransactionCompletion;
  aName: AnsiString): ITransaction;
begin
  {a transaction spanning several attachments needs a two phase commit
   coordinator, which this provider does not implement}
  if Length(Attachments) <> 1 then
    IBError(ibxeNotSupported,[nil]);
  Result := Attachments[0].StartTransaction(TPB,DefaultCompletion,aName);
end;

function TFBWireClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: ITPB; DefaultCompletion: TTransactionCompletion;
  aName: AnsiString): ITransaction;
begin
  if Length(Attachments) <> 1 then
    IBError(ibxeNotSupported,[nil]);
  Result := Attachments[0].StartTransaction(TPB,DefaultCompletion,aName);
end;

function TFBWireClientAPI.HasServiceAPI: boolean;
begin
  Result := true;
end;

function TFBWireClientAPI.AllocateSPB: ISPB;
begin
  Result := TSPB.Create(self);
end;

function TFBWireClientAPI.GetServiceManager(ServerName: AnsiString;
  Protocol: TProtocol; SPB: ISPB): IServiceManager;
begin
  Result := GetServiceManager(ServerName,'',Protocol,SPB);
end;

function TFBWireClientAPI.GetServiceManager(ServerName: AnsiString;
  Port: AnsiString; Protocol: TProtocol; SPB: ISPB): IServiceManager;
begin
  Result := TFBWireServiceManager.Create(self,ServerName,Protocol,SPB,Port);
end;

function TFBWireClientAPI.GetStatus: IStatus;
begin
  Result := FStatus;
end;

function TFBWireClientAPI.HasRollbackRetaining: boolean;
begin
  Result := true;
end;

function TFBWireClientAPI.IsEmbeddedServer: boolean;
begin
  {a wire protocol client is by definition a remote client}
  Result := false;
end;

function TFBWireClientAPI.GetClientMajor: integer;
begin
  Result := WireClientMajorVersion;
end;

function TFBWireClientAPI.GetClientMinor: integer;
begin
  Result := WireClientMinorVersion;
end;

function TFBWireClientAPI.HasLocalTZDB: boolean;
begin
  {time zone names are resolved by the server}
  Result := false;
end;

function TFBWireClientAPI.HasExtendedTZSupport: boolean;
begin
  Result := true;
end;

function TFBWireClientAPI.HasMasterIntf: boolean;
begin
  Result := false;
end;

initialization
  FWireFirebirdAPI := nil;

finalization
  FWireFirebirdAPI := nil;

end.
