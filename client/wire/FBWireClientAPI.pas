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
  FBWireProtocol, FBWireStream, FmtBCD;

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
    FWireStatus: TWireStatusVector;  {the decoded vector, argument
                                      structure intact, for formatting}
    FMessage: AnsiString;
  protected
    function GetIBMessage(CodePage: TSystemCodePage): AnsiString; override;
  public
    constructor Create(aOwner: TFBClientAPI; prefix: AnsiString = '');
    constructor Copy(src: TFBWireStatus);
    {SQLCODE support without the client library: the generated message
     table carries each engine code's SQLCODE and the per SQLCODE texts
     (facility 13 at 1000+sqlcode, facility 14 for warnings) - the same
     lookups isc_sqlcode and isc_sql_interprete perform}
    function SQLCodeSupported: boolean; override;
    function Getsqlcode: TStatusCode; override;
    function GetSQLMessage(CodePage: TSystemCodePage): Ansistring; override;
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
    {IEEE 754 densely packed decimal codec - the stock providers use the
     client library's IDecFloat16/34, which is not available here}
    procedure SQLDecFloatEncode(aValue: tBCD; SQLType: cardinal; bufptr: PByte); override;
    function SQLDecFloatDecode(SQLType: cardinal; bufptr: PByte): tBCD; override;

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
  IBUtils, FBServices, FBWireAttachment, FBWireServices, FBWireMessages;

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
  begin
    {this procedure is called from inside the caller's exception handler:
     take ownership of the object so that the handler's cleanup does not
     free it while it propagates}
    AcquireExceptionObject;
    raise E;
  end;
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
  FWireStatus := system.copy(src.FWireStatus);
  FMessage := src.FMessage;
end;

procedure TFBWireStatus.Clear;
begin
  FillChar(FStatusVector,SizeOf(FStatusVector),0);
  FStatusVector[0] := isc_arg_gds;
  FStatusVector[1] := 0;
  FStatusVector[2] := isc_arg_end;
  SetLength(FWireStatus,0);
  FMessage := '';
end;

function TFBWireStatus.SQLCodeSupported: boolean;
begin
  Result := true;
end;

function TFBWireStatus.Getsqlcode: TStatusCode;
var i: integer;
    sqlcode: integer;
begin
  {the gds__sqlcode rules: an isc_sqlerr item anywhere in the vector
   carries the SQLCODE as its number argument and wins outright;
   otherwise the first item's own mapping decides, -999 by default}
  for i := 0 to Length(FWireStatus) - 1 do
    if (FWireStatus[i].Kind = isc_arg_gds) and
       (FWireStatus[i].IntValue = isc_sqlerr) and
       (i + 1 < Length(FWireStatus)) and
       (FWireStatus[i+1].Kind = isc_arg_number) then
      Exit(FWireStatus[i+1].IntValue);
  Result := -999; {generic SQL Code}
  for i := 0 to Length(FWireStatus) - 1 do
    if (FWireStatus[i].Kind = isc_arg_gds) and (FWireStatus[i].IntValue <> 0) then
    begin
      sqlcode := EngineMessageSQLCode(cardinal(FWireStatus[i].IntValue));
      if sqlcode <> NoSQLCode then
        Result := sqlcode;
      break; {only the first item's mapping counts}
    end;
end;

function TFBWireStatus.GetSQLMessage(CodePage: TSystemCodePage): Ansistring;
var sqlcode: integer;
    fmt: AnsiString;
    code: cardinal;
    i: integer;
begin
  {what isc_sql_interprete answers: facility 13 message 1000+sqlcode for
   errors, facility 14 message sqlcode for warnings}
  Result := '';
  sqlcode := Getsqlcode;
  if sqlcode < 0 then
    code := cardinal($14000000) or (13 shl 16) or cardinal(1000 + sqlcode)
  else
    code := cardinal($14000000) or (14 shl 16) or cardinal(sqlcode);
  if FindEngineMessage(code,fmt) then
  begin
    {the per SQLCODE texts carry no useful arguments here - strip any
     placeholders, as isc_sql_interprete substitutes empties}
    i := 1;
    while i <= Length(fmt) do
    begin
      if (fmt[i] = '@') and (i < Length(fmt)) and (fmt[i+1] in ['1'..'9']) then
        Inc(i,2)
      else
      begin
        Result := Result + fmt[i];
        Inc(i);
      end;
    end;
  end;
end;

function TFBWireStatus.GetIBMessage(CodePage: TSystemCodePage): AnsiString;
begin
  {formatted from the decoded vector and the generated message table -
   the same text fb_interpret produces from firebird.msg, without the
   file. SetError stores its text in FMessage instead.}
  if Length(FWireStatus) > 0 then
    Result := FormatWireStatus(FWireStatus)
  else
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
  {keep the decoded vector: GetIBMessage formats it the way fb_interpret
   would, from the generated message table}
  FWireStatus := system.copy(aStatus);
  v := 0;
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
  SetLength(FWireStatus,0);
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

{--- IEEE 754 densely packed decimal ---

 A DECFLOAT travels as the IEEE 754-2008 decimal64/decimal128 bit image
 (the XDR layer has already put it into little endian memory order). The
 layout is sign(1), combination(5), exponent continuation(8/12), then the
 coefficient as 10 bit declets of three digits each. The combination
 field holds the two high exponent bits and the most significant digit.}

var
  DPDDecodeTable: array[0..1023] of word;  {declet -> d2*100+d1*10+d0}
  DPDEncodeTable: array[0..999] of word;   {3 digits -> canonical declet}

procedure InitDPDTables;
var b, digits: integer;
    b9, b8, b7, b6, b5, b4, b3, b2, b1, b0: integer;
    d2, d1, d0: integer;
begin
  for b := 0 to 1023 do
  begin
    b9 := (b shr 9) and 1; b8 := (b shr 8) and 1; b7 := (b shr 7) and 1;
    b6 := (b shr 6) and 1; b5 := (b shr 5) and 1; b4 := (b shr 4) and 1;
    b3 := (b shr 3) and 1; b2 := (b shr 2) and 1; b1 := (b shr 1) and 1;
    b0 := b and 1;
    if b3 = 0 then
    begin
      d2 := b9*4 + b8*2 + b7; d1 := b6*4 + b5*2 + b4; d0 := b2*4 + b1*2 + b0;
    end
    else
    case b2*2 + b1 of
    0: begin d2 := b9*4 + b8*2 + b7; d1 := b6*4 + b5*2 + b4; d0 := 8 + b0; end;
    1: begin d2 := b9*4 + b8*2 + b7; d1 := 8 + b4; d0 := b6*4 + b5*2 + b0; end;
    2: begin d2 := 8 + b7; d1 := b6*4 + b5*2 + b4; d0 := b9*4 + b8*2 + b0; end;
    else
      case b6*2 + b5 of
      0: begin d2 := 8 + b7; d1 := 8 + b4; d0 := b9*4 + b8*2 + b0; end;
      1: begin d2 := 8 + b7; d1 := b9*4 + b8*2 + b4; d0 := 8 + b0; end;
      2: begin d2 := b9*4 + b8*2 + b7; d1 := 8 + b4; d0 := 8 + b0; end;
      else begin d2 := 8 + b7; d1 := 8 + b4; d0 := 8 + b0; end;
      end;
    end;
    DPDDecodeTable[b] := d2*100 + d1*10 + d0;
  end;
  {the canonical encoding is the variant with the don't care bits zero,
   which is the numerically smallest declet decoding to those digits}
  for digits := 0 to 999 do
    DPDEncodeTable[digits] := $FFFF;
  for b := 1023 downto 0 do
    DPDEncodeTable[DPDDecodeTable[b]] := b;
end;

{aDigits[1..aWidth] receive the coefficient, left padded with zeroes.
 Returns the unbiased exponent; aSign true = negative}
procedure DecFloatToDigits(aHi, aLo: QWord; aWidth: integer;
  out aDigits: TBytes; out aExponent: integer; out aSign: boolean);
var g, biased, declets, i, k, pos: integer;
    msd: integer;
    declet: cardinal;
    combined: word;

  function GetBits(aPos, aCount: integer): cardinal;
  begin
    {aPos is the low bit position within the 128/64 bit image}
    if aPos >= 64 then
      Result := (aHi shr (aPos - 64)) and ((QWord(1) shl aCount) - 1)
    else
    if aPos + aCount <= 64 then
      Result := (aLo shr aPos) and ((QWord(1) shl aCount) - 1)
    else
      Result := ((aLo shr aPos) or (aHi shl (64 - aPos))) and
                ((QWord(1) shl aCount) - 1);
  end;

var signBit, gPos, contBits, bias: integer;
begin
  if aWidth = 16 then
  begin
    {only the low qword is used for decimal64}
    signBit := 63; contBits := 8; bias := 398; declets := 5; gPos := 58;
  end
  else
  begin
    signBit := 127; contBits := 12; bias := 6176; declets := 11;
    gPos := 122;
  end;
  aSign := GetBits(signBit,1) <> 0;
  g := GetBits(gPos,5);
  if (g shr 1) = 15 then
    {infinity or NaN}
    IBError(ibxeInvalidDataConversion,[nil]);
  if (g shr 3) <> 3 then
  begin
    biased := (g shr 3) shl contBits;
    msd := g and 7;
  end
  else
  begin
    biased := ((g shr 1) and 3) shl contBits;
    msd := 8 + (g and 1);
  end;
  biased := biased or integer(GetBits(gPos - contBits,contBits));
  aExponent := biased - bias;

  SetLength(aDigits,aWidth + 1);   {1 based, like the engine's toBcd}
  aDigits[1] := msd;
  pos := (declets - 1) * 10;
  k := 2;
  for i := 0 to declets - 1 do
  begin
    declet := GetBits(pos,10);
    combined := DPDDecodeTable[declet];
    aDigits[k] := combined div 100;
    aDigits[k+1] := (combined div 10) mod 10;
    aDigits[k+2] := combined mod 10;
    Inc(k,3);
    Dec(pos,10);
  end;
end;

procedure DigitsToDecFloat(const aDigits: TBytes; aWidth: integer;
  aExponent: integer; aSign: boolean; out aHi, aLo: QWord);
var biased, declets, i, k, pos: integer;
    msd, g: integer;
    contBits, bias, gPos, signBit, maxBiased: integer;
    declet: cardinal;

  procedure OrBits(aValue: QWord; aPos: integer);
  begin
    if aPos >= 64 then
      aHi := aHi or (aValue shl (aPos - 64))
    else
    begin
      aLo := aLo or (aValue shl aPos);
      if aPos > 0 then
        aHi := aHi or (aValue shr (64 - aPos))
      {a value at position 0 cannot straddle the boundary};
    end;
  end;

begin
  if aWidth = 16 then
  begin
    signBit := 63; contBits := 8; bias := 398; declets := 5; gPos := 58;
    maxBiased := 3 shl contBits - 1;
  end
  else
  begin
    signBit := 127; contBits := 12; bias := 6176; declets := 11; gPos := 122;
    maxBiased := 3 shl contBits - 1;
  end;
  biased := aExponent + bias;
  if (biased < 0) or (biased > maxBiased) then
    IBError(ibxeInvalidDataConversion,[nil]);

  aHi := 0;
  aLo := 0;
  if aSign then
    OrBits(1,signBit);
  msd := aDigits[1];
  if msd <= 7 then
    g := ((biased shr contBits) shl 3) or msd
  else
    g := 24 or (((biased shr contBits) and 3) shl 1) or (msd and 1);
  OrBits(QWord(g),gPos);
  OrBits(QWord(biased and ((1 shl contBits) - 1)),gPos - contBits);
  pos := (declets - 1) * 10;
  k := 2;
  for i := 0 to declets - 1 do
  begin
    declet := DPDEncodeTable[aDigits[k]*100 + aDigits[k+1]*10 + aDigits[k+2]];
    OrBits(declet,pos);
    Inc(k,3);
    Dec(pos,10);
  end;
end;

function TFBWireClientAPI.HasInt128Support: boolean;
begin
  Result := true;
end;

function TFBWireClientAPI.HasTimeZoneSupport: boolean;
begin
  Result := true;
end;

procedure TFBWireClientAPI.SQLDecFloatEncode(aValue: tBCD; SQLType: cardinal;
  bufptr: PByte);
var width, i, j: integer;
    digits: TBytes;
    hi, lo: QWord;
    aSign: boolean;
    exponent: integer;
begin
  case SQLType of
  SQL_DEC16: width := 16;
  SQL_DEC34: width := 34;
  else
    IBError(ibxeInvalidDataConversion,[nil]);
  end;
  if BCDPrecision(aValue) > width then
    IBError(ibxeBCDTooBig,[BCDPrecision(aValue),width]);
  aSign := (aValue.SignSpecialPlaces and $80) <> 0;
  exponent := -(aValue.SignSpecialPlaces and $3F);

  {right align the BCD digits in a width sized buffer - the same layout
   the engine's fromBcd expects}
  SetLength(digits,width + 2);
  FillChar(digits[0],Length(digits),0);
  j := 1 + (width - aValue.Precision);
  for i := 0 to (aValue.Precision - 1) div 2 do
  if j <= width then
  begin
    digits[j] := (aValue.Fraction[i] and $f0) shr 4;
    Inc(j);
    if j <= width then
    begin
      digits[j] := aValue.Fraction[i] and $0f;
      Inc(j);
    end;
  end;

  DigitsToDecFloat(digits,width,exponent,aSign,hi,lo);
  PQWord(bufptr)^ := lo;
  if width = 34 then
    PQWord(bufptr+8)^ := hi;
end;

function TFBWireClientAPI.SQLDecFloatDecode(SQLType: cardinal; bufptr: PByte): tBCD;
var width, i, j: integer;
    digits: TBytes;
    hi, lo: QWord;
    aSign: boolean;
    exponent: integer;
begin
  FillChar(Result,sizeof(tBCD),0);
  case SQLType of
  SQL_DEC16:
    begin
      width := 16;
      lo := PQWord(bufptr)^;
      hi := 0;
    end;
  SQL_DEC34:
    begin
      width := 34;
      lo := PQWord(bufptr)^;
      hi := PQWord(bufptr+8)^;
    end;
  else
    IBError(ibxeInvalidDataConversion,[nil]);
  end;

  DecFloatToDigits(hi,lo,width,digits,exponent,aSign);

  {a positive exponent becomes trailing zeroes so that the exponent can be
   expressed as decimal places}
  while exponent > 0 do
  begin
    if digits[1] <> 0 then
      IBError(ibxeInvalidDataConversion,[nil]);
    for i := 1 to width - 1 do
      digits[i] := digits[i+1];
    digits[width] := 0;
    Dec(exponent);
  end;

  {pack, skipping leading zeroes - mirrors the 3.0 provider}
  i := 1;
  while (i <= width) and (digits[i] = 0) do
    Inc(i);
  j := 0;
  Result.Precision := 0;
  while i <= width do
  begin
    Inc(Result.Precision);
    if odd(Result.Precision) then
      Result.Fraction[j] := (digits[i] and $0f) shl 4
    else
    begin
      Result.Fraction[j] := Result.Fraction[j] or (digits[i] and $0f);
      Inc(j);
    end;
    Inc(i);
  end;
  Result.SignSpecialPlaces := (-exponent) and $3F;
  if aSign then
    Result.SignSpecialPlaces := Result.SignSpecialPlaces or $80;
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
  InitDPDTables;

finalization
  FWireFirebirdAPI := nil;

end.
