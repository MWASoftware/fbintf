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
unit FBWireProtocol;

{ The wire protocol engine: connection handshake with protocol negotiation
  (protocols 13..19, i.e. Firebird 3.0 up to and including Firebird 6;
  older servers negotiate down), Srp/Srp256 authentication, optional wire
  encryption (Arc4, ChaCha, ChaCha64) and the request/response packet
  exchanges for attachments, transactions, DSQL statements, blobs,
  information calls and services.

  The reference for the packet layouts is src/remote/protocol.h and
  src/remote/protocol.cpp in the Firebird source tree.
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
  Classes, SysUtils, FBWireStream, FBWireConst, FBWireCrypto, FBWireSRP,
  FBWireMessage;

const
  {highest protocol version this client implements. Firebird 3 accepts up
   to 15, Firebird 4 up to 17, Firebird 5 accepts 18 (op_fetch_scroll and
   the op_execute cursor flags word), Firebird 5.0.3 accepts 19
   (op_inline_blob and the op_execute inline blob size limit), Firebird 6
   accepts 20 (SQL schemas and the prepare flags word).}
  MaxSupportedProtocol = PROTOCOL_VERSION20;
  INVALID_OBJECT = $FFFF;

  {key advertisement clumplet tags (see plugins/crypt in protocol.cpp)}
  TAG_KEY_TYPE        = 0;
  TAG_KEY_PLUGINS     = 1;
  TAG_KNOWN_PLUGINS   = 2;
  TAG_PLUGIN_SPECIFIC = 3;

type
  TWireCryptOption = (wcDisabled, wcEnabled, wcRequired);

  {receives an op_inline_blob pushed by a protocol 19 server: the blob id
   under that transaction, its info response buffer and the segmented
   data stream. Installed by the attachment - see TFBWireAttachment.}
  TInlineBlobHandler = procedure(aTrHandle: integer; aBlobID: Int64;
                        const aInfo, aData: TBytes) of object;

  TWireStatusItem = record
    Kind: integer;      {isc_arg_gds etc}
    IntValue: Int64;
    StrValue: AnsiString;
  end;
  TWireStatusVector = array of TWireStatusItem;

const
  {row states in a batch completion - IBatchCompletionState}
  BATCH_EXECUTE_FAILED  = -1;
  BATCH_SUCCESS_NO_INFO = -2;

type

  { TWireCursorState: per cursor fetch bookkeeping.

    The server answers an op_fetch requesting N rows with a sequence of
    op_fetch_response packets, each carrying at most one message, and
    terminates the batch with a packet whose message count is zero (or
    whose status is 100 at end of cursor). The whole batch must be drained
    before any other request is sent on the connection, otherwise the
    queued row packets would be mistaken for that request's response. The
    rows are therefore decoded into this cache as soon as they arrive and
    handed out one at a time. }

  TWireCursorState = record
    Rows: array of TBytes;  {decoded rows not yet consumed}
    NextRow: integer;
    EndOfCursor: boolean;
  end;

  { TWireBatchCS : decoded op_batch_cs - the batch completion state.
    States[i] is the row's update count, or BATCH_EXECUTE_FAILED /
    BATCH_SUCCESS_NO_INFO; StatusVectors[i] is non empty for rows whose
    failure came with a status vector. }

  TWireBatchCS = record
    States: array of integer;
    StatusVectors: array of TWireStatusVector;
  end;

  { TWireResponse : decoded op_response }

  TWireResponse = record
    ObjectHandle: integer;
    ObjectID: Int64;          {blob id/quad: high word in bits 32..63}
    Data: TBytes;
    Status: TWireStatusVector;
    function HasError: boolean;
    function HasWarning: boolean;
  end;

  { EFBWireProtocolError carries the decoded status vector }

  EFBWireProtocolError = class(EFBWireError)
  private
    FStatus: TWireStatusVector;
  public
    constructor CreateFromStatus(const aStatus: TWireStatusVector);
    property Status: TWireStatusVector read FStatus;
  end;

  TRC4WireCipher = class(TWireCipher)
  private
    FRC4: TRC4;
  public
    constructor Create(const aKey: TBytes);
    destructor Destroy; override;
    procedure Process(var aData; aLen: integer); override;
  end;

  TChaChaWireCipher = class(TWireCipher)
  private
    FChaCha: TChaCha20;
  public
    constructor Create(const aKey, aNonce: TBytes; aCounter: QWord);
    destructor Destroy; override;
    procedure Process(var aData; aLen: integer); override;
  end;

  { TFBWireConnection }

  TFBWireConnection = class
  private
    FTransport: TFBWireTransport;
    FXDR: TXDRStream;
    FProtocolVersion: cardinal;    {negotiated, masked: 13..17}
    FAcceptType: cardinal;
    FUser: AnsiString;
    FPassword: AnsiString;
    FSRP: TSRPClient;
    FAuthPluginName: AnsiString;
    FAuthData: AnsiString;         {hex proof for isc_dpb_specific_auth_data}
    FAuthComplete: boolean;
    FSessionKey: TBytes;
    FKeyClumplets: TBytes;         {accumulated server key advertisements}
    FCryptPlugin: AnsiString;      {active wire encryption plugin, '' = none}
    FConnected: boolean;
    FMaxProtocol: cardinal;
    FOnInlineBlob: TInlineBlobHandler;
    procedure ReadInlineBlob;
    procedure SendUserIdentification(aWireCrypt: TWireCryptOption);
    procedure DoAuthHandshake(aWireCrypt: TWireCryptOption);
    function ComputeProof(const aData: TBytes; const aPluginName: AnsiString): AnsiString;
    procedure StartWireEncryption(aWireCrypt: TWireCryptOption);
    procedure AppendKeyClumplets(const aKeys: TBytes);
    function FindWireCryptPlugin(var aSpecificData: TBytes;
                        var aKeyType: AnsiString): AnsiString;
  public
    constructor Create;
    destructor Destroy; override;

    {Connects and authenticates. aDatabasePath is the path as sent to the
     server (used in op_connect for routing only - the attach names the
     database again).}
    procedure ConnectTo(const aHost: AnsiString; aPort: integer;
                        const aDatabasePath, aUser, aPassword: AnsiString;
                        aWireCrypt: TWireCryptOption = wcEnabled;
                        aTimeout: integer = 0);
    procedure Disconnect;

    {--- packet level api ---}
    function ReadOperation: integer;   {skips op_dummy / op_response_piggyback}
    {reads a response body - the op_response operation code must already
     have been consumed}
    function ReadResponseBody: TWireResponse;
    function ReceiveResponse: TWireResponse;  {reads to next op_response}
    procedure CheckResponse(const R: TWireResponse);
    function ReceiveAndCheckResponse: TWireResponse;
    function ReadStatusVector: TWireStatusVector;

    {--- attachments ---}
    function AttachDatabase(const aDatabasePath: AnsiString; DPB: TBytes): integer;
    function CreateDatabase(const aDatabasePath: AnsiString; DPB: TBytes): integer;
    procedure DetachDatabase(aDbHandle: integer);
    procedure DropDatabase(aDbHandle: integer);

    {--- transactions ---}
    function StartTransaction(aDbHandle: integer; TPB: TBytes): integer;
    procedure Commit(aTrHandle: integer);
    procedure CommitRetaining(aTrHandle: integer);
    procedure Rollback(aTrHandle: integer);
    procedure RollbackRetaining(aTrHandle: integer);
    procedure PrepareTransaction(aTrHandle: integer); {2PC phase 1}

    {--- DSQL ---}
    function AllocateStatement(aDbHandle: integer): integer;
    {returns the isc_info_sql response buffer for the describe items}
    function PrepareStatement(aTrHandle, aStmtHandle: integer;
                        aDialect: integer; const sql: AnsiString;
                        const aInfoItems: TBytes; aBufferLength: integer): TBytes;
    {aTimeout is the statement timeout in milliseconds (0 = none), carried
     in the p_sqldata_timeout field from protocol 16. aCursorFlags is the
     protocol 18 cursor flags word - CURSOR_TYPE_SCROLLABLE requests a
     scrollable cursor. aInlineBlobLimit is the protocol 19 inline blob
     size limit: blobs whose segmented size fits are pushed with the rows
     as op_inline_blob packets; zero asks for none.}
    procedure ExecuteStatement(aStmtHandle, aTrHandle: integer;
                        const aParamFormat: TWireMessageFormat; aParamBuffer: PByte;
                        aTimeout: cardinal = 0; aCursorFlags: cardinal = 0;
                        aInlineBlobLimit: cardinal = 0);
    {op_execute2 for singleton results (execute procedure/returning)}
    procedure ExecuteStatement2(aStmtHandle, aTrHandle: integer;
                        const aParamFormat: TWireMessageFormat; aParamBuffer: PByte;
                        const aOutFormat: TWireMessageFormat; aOutBuffer: PByte;
                        aTimeout: cardinal = 0; aInlineBlobLimit: cardinal = 0);
    {fetches the next row into aOutBuffer, requesting a new batch of
     aFetchCount rows from the server when needed. Returns false when the
     cursor is exhausted. aState must be zero initialised before the first
     call for a cursor (see TWireCursorState).}
    function FetchRow(aStmtHandle: integer;
                        const aOutFormat: TWireMessageFormat; aOutBuffer: PByte;
                        aFetchCount: integer; var aState: TWireCursorState): boolean;
    {op_fetch_scroll (protocol 18): a positioned fetch on a scrollable
     cursor. Discards any read ahead rows in aState first - they describe
     a cursor position the scroll abandons - and requests a single row:
     the server disables prefetch for every direction except next/prior
     anyway. Returns false when there is no row in that direction.}
    function FetchRowScroll(aStmtHandle: integer;
                        const aOutFormat: TWireMessageFormat; aOutBuffer: PByte;
                        aDirection: integer; aPosition: integer;
                        var aState: TWireCursorState): boolean;
    procedure FreeStatement(aStmtHandle: integer; aOption: integer);
    procedure SetCursorName(aStmtHandle: integer; const aName: AnsiString);
    procedure ExecImmediate(aTrHandle, aDbHandle: integer; aDialect: integer;
                        const sql: AnsiString);

    {--- information calls ---}
    function GetInfo(aOperation: integer; aHandle: integer;
                        const aItems: TBytes; aBufferLength: integer): TBytes;

    {--- blobs ---}
    procedure CreateBlob(aTrHandle: integer; const BPB: TBytes;
                        var aBlobHandle: integer; var aBlobID: Int64);
    function OpenBlob(aTrHandle: integer; const BPB: TBytes;
                        aBlobID: Int64): integer;
    {returns segment data (already unpacked from the 2 byte length prefixed
     form). aEOB set when end of blob reached}
    function GetSegment(aBlobHandle: integer; aBufferLength: integer;
                        var aEOB: boolean): TBytes;
    procedure PutSegment(aBlobHandle: integer; const aData: TBytes);
    procedure CloseBlob(aBlobHandle: integer);
    procedure CancelBlob(aBlobHandle: integer);

    {--- array slices ---}
    {op_get_slice: reads the slice of aArrayID into aBuffer as described
     by aLayout. Returns the slice length reported by the server (in dsc
     length units - see SliceElementDscLength).}
    function GetSlice(aTrHandle: integer; aArrayID: Int64; const SDL: TBytes;
                        const aLayout: TWireSliceLayout; aBuffer: PByte): integer;
    {op_put_slice: writes the slice and returns the (possibly new) array id}
    function PutSlice(aTrHandle: integer; aArrayID: Int64; const SDL: TBytes;
                        const aLayout: TWireSliceLayout; aBuffer: PByte): Int64;

    {--- events ---}
    {op_connect_request with P_REQ_async: asks the server to open the
     auxiliary port that delivers op_event packets. Returns the TCP port
     number. Only the port of the returned address is usable: the address
     itself is the server's own view of itself, which behind NAT is not
     reachable, so the caller connects to the host it already knows (the
     stock client does the same - see aux_connect in inet.cpp).}
    function ConnectRequest(aDbHandle: integer): integer;
    procedure QueEvents(aDbHandle: integer; const aEPB: TBytes; aEventID: integer);
    procedure CancelEvents(aDbHandle, aEventID: integer);

    {--- services ---}
    function ServiceAttach(const aServiceName: AnsiString; SPB: TBytes): integer;
    procedure ServiceDetach(aSvcHandle: integer);
    procedure ServiceStart(aSvcHandle: integer; const aItems: TBytes);
    function ServiceQuery(aSvcHandle: integer; const aSendItems, aRecvItems: TBytes;
                        aBufferLength: integer): TBytes;

    {--- batches (protocol 16) ---}
    {op_batch_create: opens a batch on the statement. aMsgLen is
     EngineMessageLength(aFormat) - the server validates it against the
     format it parses from the BLR - and aPB the IBatch parameter block
     (a wide tagged clumplet buffer)}
    procedure BatchCreate(aStmtHandle: integer;
                        const aFormat: TWireMessageFormat; aMsgLen: cardinal;
                        const aPB: TBytes);
    {op_batch_msg: sends aRows messages, each encoded exactly as a row
     message (xdr_packed_message is the regular message encoding)}
    procedure BatchMsg(aStmtHandle: integer;
                        const aFormat: TWireMessageFormat;
                        const aRows: array of TBytes);
    {op_batch_regblob: registers an existing blob id for use in batch
     messages. The engine translates every non null, non zero blob id in
     a batch message through its registration map - and consumes the
     entry - so an id must be registered once for each row that carries
     it (the 3.0 provider does the same through IBatch.registerBlob).}
    procedure BatchRegBlob(aStmtHandle: integer; aExistingID, aBatchID: Int64);
    {op_batch_exec: runs the batch and parses the op_batch_cs reply}
    function BatchExec(aStmtHandle, aTrHandle: integer): TWireBatchCS;
    procedure BatchRelease(aStmtHandle: integer);
    procedure BatchCancel(aStmtHandle: integer);

    {--- misc ---}
    procedure Ping;
    {op_cancel: sent out of band, typically from a different thread while
     this connection's owner is blocked reading an operation's response.
     There is no response packet: the cancelled operation itself fails
     with isc_cancelled on the normal path. aKind is one of the
     fb_cancel_* constants.}
    procedure SendCancel(aKind: integer);

    property ProtocolVersion: cardinal read FProtocolVersion;
    {caps the highest protocol version offered to the server. Defaults to
     MaxSupportedProtocol; lower it to exercise or force an older dialect
     of the protocol.}
    property MaxProtocol: cardinal read FMaxProtocol write FMaxProtocol;
    property Connected: boolean read FConnected;
    {sink for op_inline_blob packets; unset, they are read and discarded}
    property OnInlineBlob: TInlineBlobHandler read FOnInlineBlob write FOnInlineBlob;
    property AuthData: AnsiString read FAuthData;
    property AuthPluginName: AnsiString read FAuthPluginName;
    property CryptPlugin: AnsiString read FCryptPlugin;
    {raw key advertisement clumplets received from the server - exposed for
     diagnostics}
    property KeyClumplets: TBytes read FKeyClumplets;
    property XDR: TXDRStream read FXDR;
    property Transport: TFBWireTransport read FTransport;
  end;

{empties a cursor state, releasing the cached rows. Use this rather than
 FillChar: the record holds a managed dynamic array which a FillChar
 reset would leak.}
procedure ResetCursorState(var aState: TWireCursorState);

implementation

uses IBErrorCodes;

const
  isc_arg_end          = 0;
  isc_arg_gds          = 1;
  isc_arg_string       = 2;
  isc_arg_number       = 4;
  isc_arg_interpreted  = 5;
  isc_arg_unix         = 7;
  isc_arg_next_mach    = 15;
  isc_arg_win32        = 17;
  isc_arg_warning      = 18;
  isc_arg_sql_state    = 19;

procedure ResetCursorState(var aState: TWireCursorState);
begin
  SetLength(aState.Rows,0);
  aState.NextRow := 0;
  aState.EndOfCursor := false;
end;

{ TWireResponse }

function TWireResponse.HasError: boolean;
begin
  Result := (Length(Status) > 0) and (Status[0].Kind = isc_arg_gds) and
            (Status[0].IntValue <> 0);
end;

function TWireResponse.HasWarning: boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to Length(Status) - 1 do
    if Status[i].Kind = isc_arg_warning then
      Exit(true);
end;

{ EFBWireProtocolError }

constructor EFBWireProtocolError.CreateFromStatus(const aStatus: TWireStatusVector);
var msg: AnsiString;
    i: integer;
begin
  FStatus := aStatus;
  msg := '';
  for i := 0 to Length(aStatus) - 1 do
    case aStatus[i].Kind of
    isc_arg_string, isc_arg_interpreted:
      begin
        if msg <> '' then
          msg := msg + LineEnding + '-';
        msg := msg + aStatus[i].StrValue;
      end;
    isc_arg_gds:
      if (msg = '') and (aStatus[i].IntValue <> 0) then
        msg := Format('Engine Code: %d',[aStatus[i].IntValue]);
    end;
  if msg = '' then
    msg := 'Firebird error';
  inherited Create(msg);
end;

{ TRC4WireCipher }

constructor TRC4WireCipher.Create(const aKey: TBytes);
begin
  inherited Create;
  FRC4 := TRC4.Create(aKey);
end;

destructor TRC4WireCipher.Destroy;
begin
  FRC4.Free;
  inherited Destroy;
end;

procedure TRC4WireCipher.Process(var aData; aLen: integer);
begin
  FRC4.Process(aData,aLen);
end;

{ TChaChaWireCipher }

constructor TChaChaWireCipher.Create(const aKey, aNonce: TBytes; aCounter: QWord);
begin
  inherited Create;
  FChaCha := TChaCha20.Create(aKey,aNonce,aCounter);
end;

destructor TChaChaWireCipher.Destroy;
begin
  FChaCha.Free;
  inherited Destroy;
end;

procedure TChaChaWireCipher.Process(var aData; aLen: integer);
begin
  FChaCha.Process(aData,aLen);
end;

{ TFBWireConnection }

constructor TFBWireConnection.Create;
begin
  inherited Create;
  FTransport := TFBWireTransport.Create;
  FXDR := TXDRStream.Create(FTransport);
  FMaxProtocol := MaxSupportedProtocol;
end;

destructor TFBWireConnection.Destroy;
begin
  Disconnect;
  if FSRP <> nil then FSRP.Free;
  FXDR.Free;
  FTransport.Free;
  inherited Destroy;
end;

procedure TFBWireConnection.Disconnect;
begin
  if FConnected and FTransport.Connected then
  try
    FXDR.WriteInt32(op_disconnect);
    FXDR.Flush;
  except
    {ignore - connection may already be gone}
  end;
  FTransport.Disconnect;
  FConnected := false;
end;

function GetOSUser: AnsiString;
begin
  Result := GetEnvironmentVariable('USER');
  if Result = '' then
    Result := GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := 'unknown';
end;

function GetHostName: AnsiString;
begin
  Result := GetEnvironmentVariable('HOSTNAME');
  if Result = '' then
    Result := GetEnvironmentVariable('COMPUTERNAME');
  if Result = '' then
    Result := 'localhost';
end;

procedure TFBWireConnection.SendUserIdentification(aWireCrypt: TWireCryptOption);
var buffer: TBytes;
    len: integer;

  procedure AddByte(aValue: byte);
  begin
    if len >= Length(buffer) then
      SetLength(buffer,Length(buffer)+256);
    buffer[len] := aValue;
    Inc(len);
  end;

  procedure AddClumplet(aTag: byte; const aData: AnsiString);
  var i: integer;
  begin
    if Length(aData) > 255 then
      raise EFBWireError.Create('user identification item too long');
    AddByte(aTag);
    AddByte(Length(aData));
    for i := 1 to Length(aData) do
      AddByte(byte(aData[i]));
  end;

  procedure AddMultipart(aTag: byte; const aData: AnsiString);
  var remaining, chunk, part, offset, i: integer;
  begin
    remaining := Length(aData);
    part := 0;
    offset := 1;
    repeat
      chunk := remaining;
      if chunk > 254 then chunk := 254;
      AddByte(aTag);
      AddByte(chunk + 1);
      AddByte(part);
      for i := 0 to chunk - 1 do
        AddByte(byte(aData[offset+i]));
      Inc(offset,chunk);
      Dec(remaining,chunk);
      Inc(part);
    until remaining <= 0;
  end;

begin
  SetLength(buffer,512);
  len := 0;
  AddClumplet(CNCT_login,FUser);
  AddClumplet(CNCT_plugin_name,FAuthPluginName);
  AddClumplet(CNCT_plugin_list,sSrp256PluginName + ',' + sSrpPluginName);
  AddMultipart(CNCT_specific_data,UpperCase(FSRP.PublicKeyHex));
  {client crypt level - 4 bytes little endian}
  AddByte(CNCT_client_crypt);
  AddByte(4);
  AddByte(ord(aWireCrypt));
  AddByte(0);
  AddByte(0);
  AddByte(0);
  AddClumplet(CNCT_user,GetOSUser);
  AddClumplet(CNCT_host,LowerCase(GetHostName));
  AddClumplet(CNCT_user_verification,'');
  SetLength(buffer,len);
  FXDR.WriteString(buffer);
end;

procedure TFBWireConnection.ConnectTo(const aHost: AnsiString; aPort: integer;
  const aDatabasePath, aUser, aPassword: AnsiString;
  aWireCrypt: TWireCryptOption; aTimeout: integer);
const
  OfferedProtocols: array[0..7] of cardinal = (
    PROTOCOL_VERSION13, PROTOCOL_VERSION14, PROTOCOL_VERSION15,
    PROTOCOL_VERSION16, PROTOCOL_VERSION17, PROTOCOL_VERSION18,
    PROTOCOL_VERSION19, PROTOCOL_VERSION20);
var i: integer;
    offered: integer;
begin
  Disconnect;
  FUser := aUser;
  FPassword := aPassword;
  FAuthData := '';
  FAuthComplete := false;
  SetLength(FSessionKey,0);
  SetLength(FKeyClumplets,0);
  FCryptPlugin := '';
  FAuthPluginName := sSrp256PluginName;
  if FSRP <> nil then FreeAndNil(FSRP);
  FSRP := TSRPClient.Create;

  FTransport.ConnectTo(aHost,aPort,aTimeout);
  FConnected := true;
  try
    {op_connect}
    FXDR.WriteInt32(op_connect);
    FXDR.WriteInt32(op_attach);           {p_cnct_operation}
    FXDR.WriteInt32(CONNECT_VERSION3);
    FXDR.WriteInt32(arch_generic);
    FXDR.WriteString(aDatabasePath);
    offered := 0;
    for i := 0 to High(OfferedProtocols) do
      if OfferedProtocols[i] <= FMaxProtocol then
        Inc(offered);
    if offered = 0 then
      raise EFBWireError.Create('No protocol version left to offer');
    FXDR.WriteInt32(offered);
    SendUserIdentification(aWireCrypt);
    for i := 0 to High(OfferedProtocols) do
    begin
      if OfferedProtocols[i] > FMaxProtocol then continue;
      FXDR.WriteUInt32(OfferedProtocols[i]);
      FXDR.WriteInt32(arch_generic);
      FXDR.WriteInt32(0);                  {min type}
      FXDR.WriteInt32(ptype_batch_send);   {max type - avoid lazy semantics}
      FXDR.WriteInt32((i + 1) * 2);        {preference weight}
    end;
    FXDR.Flush;

    DoAuthHandshake(aWireCrypt);
    StartWireEncryption(aWireCrypt);
  except
    on E: Exception do
    begin
      FTransport.Disconnect;
      FConnected := false;
      raise;
    end;
  end;
end;

function TFBWireConnection.ComputeProof(const aData: TBytes;
  const aPluginName: AnsiString): AnsiString;
var saltLen, keyLen: integer;
    salt: TBytes;
    serverKeyHex: AnsiString;
    proof: TBytes;
    proofHash: TSRPProofHash;
    i: integer;
begin
  if Length(aData) < 4 then
    raise EFBWireError.Create('Invalid SRP authentication data from server');
  saltLen := aData[0] or (aData[1] shl 8);
  if 2 + saltLen + 2 > Length(aData) then
    raise EFBWireError.Create('Invalid SRP salt length from server');
  SetLength(salt,saltLen);
  if saltLen > 0 then
    Move(aData[2],salt[0],saltLen);
  keyLen := aData[2+saltLen] or (aData[3+saltLen] shl 8);
  if 4 + saltLen + keyLen > Length(aData) then
    raise EFBWireError.Create('Invalid SRP key length from server');
  SetLength(serverKeyHex,keyLen);
  for i := 0 to keyLen - 1 do
    serverKeyHex[i+1] := AnsiChar(aData[4+saltLen+i]);

  if aPluginName = sSrpPluginName then
    proofHash := sphSHA1
  else if aPluginName = sSrp256PluginName then
    proofHash := sphSHA256
  else
    raise EFBWireError.CreateFmt('Unsupported authentication plugin "%s"',
                                  [aPluginName]);

  proof := FSRP.ClientProof(FUser,FPassword,salt,serverKeyHex,proofHash);
  FSessionKey := FSRP.SessionKey;

  Result := '';
  for i := 0 to Length(proof) - 1 do
    Result := Result + UpperCase(IntToHex(proof[i],2));
end;

procedure TFBWireConnection.AppendKeyClumplets(const aKeys: TBytes);
var oldLen: integer;
begin
  if Length(aKeys) = 0 then Exit;
  oldLen := Length(FKeyClumplets);
  SetLength(FKeyClumplets,oldLen + Length(aKeys));
  Move(aKeys[0],FKeyClumplets[oldLen],Length(aKeys));
end;

procedure TFBWireConnection.DoAuthHandshake(aWireCrypt: TWireCryptOption);
var op: integer;
    aVersion: cardinal;
    aArch, aType: cardinal;
    acptData, acptKeys: TBytes;
    acptPlugin: AnsiString;
    authenticated: integer;
    proofHex: AnsiString;
    R: TWireResponse;
    contData, contKeys: TBytes;
    contPlugin, contList: AnsiString;
    sentPluginList: boolean;
    isCondAccept: boolean;
begin
  sentPluginList := true; {already sent in op_connect}
  repeat
    op := ReadOperation;
    case op of
    op_reject:
      raise EFBWireError.Create('Connection rejected by server - no acceptable protocol');

    op_response:
      begin
        {either an error at connect time (e.g. bad login) or the successful
         completion of an op_cont_auth exchange}
        R := ReadResponseBody;
        CheckResponse(R);
        {a success response ends the authentication phase}
        AppendKeyClumplets(R.Data);
        FAuthComplete := true;
      end;

    op_accept, op_accept_data, op_cond_accept:
      begin
        aVersion := FXDR.ReadUInt32;
        aArch := FXDR.ReadUInt32;
        aType := FXDR.ReadUInt32;
        {shorts are sign extended on the wire: mask down}
        FProtocolVersion := aVersion and FB_PROTOCOL_MASK;
        FAcceptType := aType and ptype_mask;
        if aArch <> arch_generic then
          raise EFBWireError.Create('Server accepted non generic architecture');
        if FProtocolVersion < (PROTOCOL_VERSION13 and FB_PROTOCOL_MASK) then
          raise EFBWireError.CreateFmt(
            'Server protocol version %d too old - Firebird 3 or later required',
            [FProtocolVersion]);
        if op = op_accept then
        begin
          FAuthComplete := true;
          break;
        end;
        isCondAccept := op = op_cond_accept;
        acptData := FXDR.ReadString;
        acptPlugin := FXDR.ReadStringAsAnsi;
        authenticated := FXDR.ReadInt32;
        acptKeys := FXDR.ReadString;
        AppendKeyClumplets(acptKeys);
        if authenticated = 1 then
        begin
          FAuthComplete := true;
          break;
        end;
        if acptPlugin <> '' then
          FAuthPluginName := acptPlugin;
        if Length(acptData) = 0 then
        begin
          {server wants us to (re)start with the named plugin: send our
           public key}
          FXDR.WriteInt32(op_cont_auth);
          FXDR.WriteString(UpperCase(FSRP.PublicKeyHex));
          FXDR.WriteString(FAuthPluginName);
          FXDR.WriteString('');
          FXDR.WriteString('');
          FXDR.Flush;
          continue;
        end;
        proofHex := ComputeProof(acptData,FAuthPluginName);
        if isCondAccept then
        begin
          {authentication must complete before attach}
          FXDR.WriteInt32(op_cont_auth);
          FXDR.WriteString(proofHex);
          FXDR.WriteString(FAuthPluginName);
          FXDR.WriteString('');
          FXDR.WriteString('');
          FXDR.Flush;
          continue;
        end
        else
        begin
          {op_accept_data: the proof travels in the DPB with op_attach}
          FAuthData := proofHex;
          FAuthComplete := true;
          break;
        end;
      end;

    op_cont_auth:
      begin
        contData := FXDR.ReadString;
        contPlugin := FXDR.ReadStringAsAnsi;
        contList := FXDR.ReadStringAsAnsi;
        contKeys := FXDR.ReadString;
        AppendKeyClumplets(contKeys);
        if contPlugin <> '' then
          FAuthPluginName := contPlugin;
        if Length(contData) = 0 then
        begin
          FXDR.WriteInt32(op_cont_auth);
          FXDR.WriteString(UpperCase(FSRP.PublicKeyHex));
          FXDR.WriteString(FAuthPluginName);
          FXDR.WriteString('');
          FXDR.WriteString('');
          FXDR.Flush;
          continue;
        end;
        proofHex := ComputeProof(contData,FAuthPluginName);
        FXDR.WriteInt32(op_cont_auth);
        FXDR.WriteString(proofHex);
        FXDR.WriteString(FAuthPluginName);
        FXDR.WriteString('');
        FXDR.WriteString('');
        FXDR.Flush;
      end;

    op_crypt_key_callback:
      begin
        {database crypt key callback - we have no key to offer; reply with
         empty data and continue}
        FXDR.ReadString; {p_cc_data}
        {p_cc_reply present during connect phase}
        FXDR.ReadInt32;
        FXDR.WriteInt32(op_crypt_key_callback);
        FXDR.WriteString('');
        FXDR.WriteInt32(0);
        FXDR.Flush;
      end;
    else
      raise EFBWireError.CreateFmt('Unexpected operation %d during connection handshake',[op]);
    end;
  until FAuthComplete;
end;

function TFBWireConnection.FindWireCryptPlugin(var aSpecificData: TBytes;
  var aKeyType: AnsiString): AnsiString;
var bufPos: integer;
    tag: byte;
    len: integer;
    data: TBytes;
    currentKeyType: AnsiString;
    candidates: AnsiString;
    candidateKeyType: AnsiString;
    specificFor: AnsiString;
    nameEnd: integer;
    chachaIV, chacha64IV: TBytes;

  function BytesToAnsi(const b: TBytes): AnsiString;
  var k: integer;
  begin
    SetLength(Result,Length(b));
    for k := 0 to Length(b)-1 do
      Result[k+1] := AnsiChar(b[k]);
  end;

  {the plugin list is a space or comma separated list of names - match
   whole tokens only so that "ChaCha" does not match "ChaCha64"}
  function ListHasPlugin(const aList, aName: AnsiString): boolean;
  var token: AnsiString;
      i: integer;
      c: AnsiChar;
  begin
    Result := false;
    token := '';
    for i := 1 to Length(aList) + 1 do
    begin
      if i <= Length(aList) then c := aList[i] else c := ' ';
      if (c = ' ') or (c = ',') or (c = #9) then
      begin
        if (token <> '') and SameText(token,aName) then
          Exit(true);
        token := '';
      end
      else
        token := token + c;
    end;
  end;

begin
  Result := '';
  aKeyType := '';
  SetLength(aSpecificData,0);
  candidates := '';
  candidateKeyType := '';
  currentKeyType := '';
  SetLength(chachaIV,0);
  SetLength(chacha64IV,0);
  bufPos := 0;
  while bufPos + 1 < Length(FKeyClumplets) do
  begin
    tag := FKeyClumplets[bufPos];
    len := FKeyClumplets[bufPos+1];
    Inc(bufPos,2);
    if bufPos + len > Length(FKeyClumplets) then break;
    SetLength(data,len);
    if len > 0 then
      Move(FKeyClumplets[bufPos],data[0],len);
    Inc(bufPos,len);
    case tag of
    TAG_KEY_TYPE:
      currentKeyType := BytesToAnsi(data);
    TAG_KEY_PLUGINS:
      begin
        {the key type is whatever the server called it - typically
         "Symmetric" - and must be echoed back in op_crypt}
        candidates := candidates + ' ' + BytesToAnsi(data);
        if candidateKeyType = '' then
          candidateKeyType := currentKeyType;
      end;
    TAG_PLUGIN_SPECIFIC:
      begin
        {data = plugin name + #0 + plugin specific data (the IV)}
        nameEnd := 0;
        while (nameEnd < len) and (data[nameEnd] <> 0) do
          Inc(nameEnd);
        specificFor := Copy(BytesToAnsi(data),1,nameEnd);
        if SameText(specificFor,sChaChaPluginName) then
          chachaIV := Copy(data,nameEnd+1,len-nameEnd-1)
        else
        if SameText(specificFor,sChaCha64PluginName) then
          chacha64IV := Copy(data,nameEnd+1,len-nameEnd-1);
      end;
    end;
  end;
  aKeyType := candidateKeyType;
  if aKeyType = '' then
    aKeyType := sSymmetricKeyName;

  {prefer ChaCha64, then ChaCha, then Arc4. The ChaCha plugins need the
   server supplied IV (protocol 16 and later); without it use Arc4}
  if ListHasPlugin(candidates,sChaCha64PluginName) and (Length(chacha64IV) >= 8) then
  begin
    Result := sChaCha64PluginName;
    aSpecificData := chacha64IV;
  end
  else
  if ListHasPlugin(candidates,sChaChaPluginName) and (Length(chachaIV) >= 16) then
  begin
    Result := sChaChaPluginName;
    aSpecificData := chachaIV;
  end
  else
  if ListHasPlugin(candidates,sArc4PluginName) or (candidates = '') then
    Result := sArc4PluginName;
end;

procedure TFBWireConnection.StartWireEncryption(aWireCrypt: TWireCryptOption);
var plugin: AnsiString;
    keyType: AnsiString;
    specific: TBytes;
    key32: TBytes;
    nonce: TBytes;
    counter: QWord;
    R: TWireResponse;
begin
  if (aWireCrypt = wcDisabled) or (Length(FSessionKey) = 0) then
    Exit;
  if FProtocolVersion < (PROTOCOL_VERSION14 and FB_PROTOCOL_MASK) then
    Exit;
  plugin := FindWireCryptPlugin(specific,keyType);
  if plugin = '' then
  begin
    if aWireCrypt = wcRequired then
      raise EFBWireError.Create('Wire encryption required but no usable plugin');
    Exit;
  end;

  {send op_crypt in plaintext}
  FXDR.WriteInt32(op_crypt);
  FXDR.WriteString(plugin);
  FXDR.WriteString(keyType);   {the key type the server advertised}
  FXDR.Flush;

  {the server encrypts everything it sends after it receives op_crypt}
  if plugin = sArc4PluginName then
  begin
    FTransport.EnableRecvCipher(TRC4WireCipher.Create(FSessionKey));
    R := ReceiveResponse;
    CheckResponse(R);
    FTransport.EnableSendCipher(TRC4WireCipher.Create(FSessionKey));
  end
  else
  begin
    {ChaCha plugins use SHA256(session key) and the server supplied IV}
    key32 := SHA256DigestToBytes(TSHA256.Digest(FSessionKey));
    if plugin = sChaCha64PluginName then
    begin
      nonce := Copy(specific,0,8);
      counter := 0;
    end
    else
    begin
      nonce := Copy(specific,0,12);
      {bytes 12..15: big endian initial counter}
      counter := (QWord(specific[12]) shl 24) or (QWord(specific[13]) shl 16) or
                 (QWord(specific[14]) shl 8) or QWord(specific[15]);
    end;
    FTransport.EnableRecvCipher(TChaChaWireCipher.Create(key32,nonce,counter));
    R := ReceiveResponse;
    CheckResponse(R);
    FTransport.EnableSendCipher(TChaChaWireCipher.Create(key32,nonce,counter));
  end;
  FCryptPlugin := plugin;
end;

function TFBWireConnection.ReadOperation: integer;
begin
  repeat
    Result := FXDR.ReadInt32;
    if Result = op_dummy then
      continue;
    if Result = op_response_piggyback then
    begin
      {unsolicited response - consume and discard}
      ReadResponseBody;
      continue;
    end;
    if Result = op_inline_blob then
    begin
      {a protocol 19 server pushes small blobs ahead of the rows that
       reference them - cache and carry on with whatever the caller was
       actually waiting for}
      ReadInlineBlob;
      continue;
    end;
    break;
  until false;
end;

procedure TFBWireConnection.ReadInlineBlob;
var aTrHandle: integer;
    aBlobID: Int64;
    info, data: TBytes;
begin
  aTrHandle := FXDR.ReadInt32;
  aBlobID := FXDR.ReadInt64;
  info := FXDR.ReadString;
  data := FXDR.ReadString;   {the segmented stream, as one counted blob}
  if assigned(FOnInlineBlob) then
    FOnInlineBlob(aTrHandle,aBlobID,info,data);
end;

function TFBWireConnection.ReadStatusVector: TWireStatusVector;
var code: integer;
    count: integer;

  procedure Append(aKind: integer; aInt: Int64; const aStr: AnsiString);
  begin
    if count >= Length(Result) then
      SetLength(Result,Length(Result)+8);
    Result[count].Kind := aKind;
    Result[count].IntValue := aInt;
    Result[count].StrValue := aStr;
    Inc(count);
  end;

begin
  SetLength(Result,8);
  count := 0;
  repeat
    code := FXDR.ReadInt32;
    case code of
    isc_arg_end:
      break;
    isc_arg_gds, isc_arg_number, isc_arg_unix, isc_arg_next_mach,
    isc_arg_win32, isc_arg_warning:
      Append(code,FXDR.ReadInt32,'');
    isc_arg_string, isc_arg_interpreted, isc_arg_sql_state:
      Append(code,0,FXDR.ReadStringAsAnsi);
    else
      raise EFBWireError.CreateFmt('Invalid status vector item %d',[code]);
    end;
  until false;
  SetLength(Result,count);
end;

function TFBWireConnection.ReadResponseBody: TWireResponse;
begin
  Result.ObjectHandle := FXDR.ReadInt32;
  Result.ObjectID := FXDR.ReadInt64;
  Result.Data := FXDR.ReadString;
  Result.Status := ReadStatusVector;
end;

function TFBWireConnection.ReceiveResponse: TWireResponse;
var op: integer;
begin
  op := ReadOperation;
  if op <> op_response then
    raise EFBWireError.CreateFmt('Expected op_response, received %d',[op]);
  Result := ReadResponseBody;
end;

procedure TFBWireConnection.CheckResponse(const R: TWireResponse);
begin
  if R.HasError then
    raise EFBWireProtocolError.CreateFromStatus(R.Status);
end;

function TFBWireConnection.ReceiveAndCheckResponse: TWireResponse;
begin
  Result := ReceiveResponse;
  CheckResponse(Result);
end;

{--- attachments ---}

function TFBWireConnection.AttachDatabase(const aDatabasePath: AnsiString;
  DPB: TBytes): integer;
begin
  FXDR.WriteInt32(op_attach);
  FXDR.WriteInt32(0);
  FXDR.WriteString(aDatabasePath);
  FXDR.WriteString(DPB);
  FXDR.Flush;
  Result := ReceiveAndCheckResponse.ObjectHandle;
end;

function TFBWireConnection.CreateDatabase(const aDatabasePath: AnsiString;
  DPB: TBytes): integer;
begin
  FXDR.WriteInt32(op_create);
  FXDR.WriteInt32(0);
  FXDR.WriteString(aDatabasePath);
  FXDR.WriteString(DPB);
  FXDR.Flush;
  Result := ReceiveAndCheckResponse.ObjectHandle;
end;

procedure TFBWireConnection.DetachDatabase(aDbHandle: integer);
begin
  FXDR.WriteInt32(op_detach);
  FXDR.WriteInt32(aDbHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.DropDatabase(aDbHandle: integer);
begin
  FXDR.WriteInt32(op_drop_database);
  FXDR.WriteInt32(aDbHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

{--- transactions ---}

function TFBWireConnection.StartTransaction(aDbHandle: integer; TPB: TBytes): integer;
begin
  FXDR.WriteInt32(op_transaction);
  FXDR.WriteInt32(aDbHandle);
  FXDR.WriteString(TPB);
  FXDR.Flush;
  Result := ReceiveAndCheckResponse.ObjectHandle;
end;

procedure TFBWireConnection.Commit(aTrHandle: integer);
begin
  FXDR.WriteInt32(op_commit);
  FXDR.WriteInt32(aTrHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.CommitRetaining(aTrHandle: integer);
begin
  FXDR.WriteInt32(op_commit_retaining);
  FXDR.WriteInt32(aTrHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.Rollback(aTrHandle: integer);
begin
  FXDR.WriteInt32(op_rollback);
  FXDR.WriteInt32(aTrHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.RollbackRetaining(aTrHandle: integer);
begin
  FXDR.WriteInt32(op_rollback_retaining);
  FXDR.WriteInt32(aTrHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.PrepareTransaction(aTrHandle: integer);
begin
  FXDR.WriteInt32(op_prepare);
  FXDR.WriteInt32(aTrHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

{--- DSQL ---}

function TFBWireConnection.AllocateStatement(aDbHandle: integer): integer;
begin
  FXDR.WriteInt32(op_allocate_statement);
  FXDR.WriteInt32(aDbHandle);
  FXDR.Flush;
  Result := ReceiveAndCheckResponse.ObjectHandle;
end;

function TFBWireConnection.PrepareStatement(aTrHandle, aStmtHandle: integer;
  aDialect: integer; const sql: AnsiString; const aInfoItems: TBytes;
  aBufferLength: integer): TBytes;
begin
  FXDR.WriteInt32(op_prepare_statement);
  FXDR.WriteInt32(aTrHandle);
  FXDR.WriteInt32(aStmtHandle);
  FXDR.WriteInt32(aDialect);
  FXDR.WriteString(sql);
  FXDR.WriteString(aInfoItems);
  FXDR.WriteInt32(aBufferLength);
  if FProtocolVersion >= (PROTOCOL_VERSION20 and FB_PROTOCOL_MASK) then
    FXDR.WriteInt32(0);        {p_sqlst_flags - no special prepare flags}
  FXDR.Flush;
  Result := ReceiveAndCheckResponse.Data;
end;

procedure TFBWireConnection.ExecuteStatement(aStmtHandle, aTrHandle: integer;
  const aParamFormat: TWireMessageFormat; aParamBuffer: PByte;
  aTimeout: cardinal; aCursorFlags: cardinal; aInlineBlobLimit: cardinal);
var blr: TBytes;
begin
  FXDR.WriteInt32(op_execute);
  FXDR.WriteInt32(aStmtHandle);
  FXDR.WriteInt32(aTrHandle);
  if Length(aParamFormat) > 0 then
  begin
    blr := BuildMessageBlr(aParamFormat);
    FXDR.WriteString(blr);
    FXDR.WriteInt32(0);   {message number}
    FXDR.WriteInt32(1);   {messages follow}
    XDREncodeMessage(FXDR,aParamFormat,aParamBuffer);
  end
  else
  begin
    FXDR.WriteString('');
    FXDR.WriteInt32(0);
    FXDR.WriteInt32(0);
  end;
  if FProtocolVersion >= (PROTOCOL_VERSION16 and FB_PROTOCOL_MASK) then
    FXDR.WriteUInt32(aTimeout);   {p_sqldata_timeout}
  if FProtocolVersion >= (PROTOCOL_VERSION18 and FB_PROTOCOL_MASK) then
    FXDR.WriteUInt32(aCursorFlags); {p_sqldata_cursor_flags}
  if FProtocolVersion >= (PROTOCOL_VERSION19 and FB_PROTOCOL_MASK) then
    FXDR.WriteUInt32(aInlineBlobLimit); {p_sqldata_inline_blob_size}
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.ExecuteStatement2(aStmtHandle, aTrHandle: integer;
  const aParamFormat: TWireMessageFormat; aParamBuffer: PByte;
  const aOutFormat: TWireMessageFormat; aOutBuffer: PByte;
  aTimeout: cardinal; aInlineBlobLimit: cardinal);
var blr: TBytes;
    op: integer;
    messages: integer;
    R: TWireResponse;
begin
  FXDR.WriteInt32(op_execute2);
  FXDR.WriteInt32(aStmtHandle);
  FXDR.WriteInt32(aTrHandle);
  if Length(aParamFormat) > 0 then
  begin
    blr := BuildMessageBlr(aParamFormat);
    FXDR.WriteString(blr);
    FXDR.WriteInt32(0);
    FXDR.WriteInt32(1);
    XDREncodeMessage(FXDR,aParamFormat,aParamBuffer);
  end
  else
  begin
    FXDR.WriteString('');
    FXDR.WriteInt32(0);
    FXDR.WriteInt32(0);
  end;
  blr := BuildMessageBlr(aOutFormat);
  FXDR.WriteString(blr);
  FXDR.WriteInt32(0);   {out message number}
  if FProtocolVersion >= (PROTOCOL_VERSION16 and FB_PROTOCOL_MASK) then
    FXDR.WriteUInt32(aTimeout); {p_sqldata_timeout}
  if FProtocolVersion >= (PROTOCOL_VERSION18 and FB_PROTOCOL_MASK) then
    FXDR.WriteUInt32(0); {p_sqldata_cursor_flags - a singleton has no cursor}
  if FProtocolVersion >= (PROTOCOL_VERSION19 and FB_PROTOCOL_MASK) then
    FXDR.WriteUInt32(aInlineBlobLimit); {p_sqldata_inline_blob_size}
  FXDR.Flush;

  op := ReadOperation;
  if op = op_sql_response then
  begin
    messages := FXDR.ReadInt32;
    if messages > 0 then
      XDRDecodeMessage(FXDR,aOutFormat,aOutBuffer);
    ReceiveAndCheckResponse;
  end
  else
  if op = op_response then
  begin
    {error before the sql response}
    R := ReadResponseBody;
    CheckResponse(R);
  end
  else
    raise EFBWireError.CreateFmt('Unexpected operation %d in execute2 response',[op]);
end;

function TFBWireConnection.FetchRow(aStmtHandle: integer;
  const aOutFormat: TWireMessageFormat; aOutBuffer: PByte;
  aFetchCount: integer; var aState: TWireCursorState): boolean;
var op: integer;
    status, messages: integer;
    blr: TBytes;
    R: TWireResponse;
    rowSize: cardinal;
    rowCount: integer;
    row: TBytes;
begin
  Result := false;
  {a row left over from the previous batch?}
  if aState.NextRow < Length(aState.Rows) then
  begin
    Move(aState.Rows[aState.NextRow][0],aOutBuffer^,Length(aState.Rows[aState.NextRow]));
    Inc(aState.NextRow);
    Exit(true);
  end;
  if aState.EndOfCursor then Exit;
  if aFetchCount < 1 then
    aFetchCount := 1;

  rowSize := MessageBufferSize(aOutFormat);
  SetLength(aState.Rows,0);
  aState.NextRow := 0;
  rowCount := 0;

  FXDR.WriteInt32(op_fetch);
  FXDR.WriteInt32(aStmtHandle);
  blr := BuildMessageBlr(aOutFormat);
  FXDR.WriteString(blr);
  FXDR.WriteInt32(0);            {message number}
  FXDR.WriteInt32(aFetchCount);  {rows requested}
  FXDR.Flush;

  {drain the whole batch - nothing else may be sent until it is complete}
  repeat
    op := ReadOperation;
    if op = op_response then
    begin
      {an error terminated the fetch}
      R := ReadResponseBody;
      aState.EndOfCursor := true;
      CheckResponse(R);
      break;
    end;
    if op <> op_fetch_response then
      raise EFBWireError.CreateFmt('Unexpected operation %d in fetch response',[op]);
    status := FXDR.ReadInt32;
    messages := FXDR.ReadInt32;
    if status = FETCH_status_eof then
    begin
      aState.EndOfCursor := true;
      break;
    end;
    if messages = 0 then
      break;   {batch complete, more rows may be available}
    SetLength(row,rowSize);
    if rowSize > 0 then
      FillChar(row[0],rowSize,0);
    XDRDecodeMessage(FXDR,aOutFormat,@row[0]);
    Inc(rowCount);
    SetLength(aState.Rows,rowCount);
    aState.Rows[rowCount-1] := row;
    row := nil;
  until false;

  if Length(aState.Rows) = 0 then
    Exit;
  Move(aState.Rows[0][0],aOutBuffer^,Length(aState.Rows[0]));
  aState.NextRow := 1;
  Result := true;
end;

function TFBWireConnection.FetchRowScroll(aStmtHandle: integer;
  const aOutFormat: TWireMessageFormat; aOutBuffer: PByte;
  aDirection: integer; aPosition: integer;
  var aState: TWireCursorState): boolean;
var op: integer;
    status, messages: integer;
    blr: TBytes;
    R: TWireResponse;
    rowSize: cardinal;
    row: TBytes;
begin
  Result := false;
  if FProtocolVersion < (PROTOCOL_VERSION18 and FB_PROTOCOL_MASK) then
    raise EFBWireError.Create('op_fetch_scroll needs protocol 18 or later');

  {any rows read ahead describe the cursor position this scroll abandons}
  ResetCursorState(aState);
  rowSize := MessageBufferSize(aOutFormat);

  FXDR.WriteInt32(op_fetch_scroll);
  FXDR.WriteInt32(aStmtHandle);
  blr := BuildMessageBlr(aOutFormat);
  FXDR.WriteString(blr);
  FXDR.WriteInt32(0);            {message number}
  FXDR.WriteInt32(1);            {one row - no read ahead on a scroll}
  FXDR.WriteInt32(aDirection);
  FXDR.WriteInt32(aPosition);
  FXDR.Flush;

  {drain the batch exactly as FetchRow does - the server terminates it
   with a packet whose message count is zero, status 100 meaning no row
   in the requested direction}
  repeat
    op := ReadOperation;
    if op = op_response then
    begin
      {an error terminated the fetch}
      R := ReadResponseBody;
      CheckResponse(R);
      break;
    end;
    if op <> op_fetch_response then
      raise EFBWireError.CreateFmt('Unexpected operation %d in fetch response',[op]);
    status := FXDR.ReadInt32;
    messages := FXDR.ReadInt32;
    if messages = 0 then
      break;
    SetLength(row,rowSize);
    if rowSize > 0 then
      FillChar(row[0],rowSize,0);
    XDRDecodeMessage(FXDR,aOutFormat,@row[0]);
    if not Result then
    begin
      Move(row[0],aOutBuffer^,Length(row));
      Result := true;
    end;
    row := nil;
  until false;
  {status 100 - no row in the requested direction - needs no bookkeeping:
   the state was reset above and any following sequential fetch starts a
   fresh batch. The cursor itself sits at BOF or EOF, which the statement
   layer tracks.}
end;

procedure TFBWireConnection.FreeStatement(aStmtHandle: integer; aOption: integer);
begin
  FXDR.WriteInt32(op_free_statement);
  FXDR.WriteInt32(aStmtHandle);
  FXDR.WriteInt32(aOption);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.SetCursorName(aStmtHandle: integer;
  const aName: AnsiString);
begin
  FXDR.WriteInt32(op_set_cursor);
  FXDR.WriteInt32(aStmtHandle);
  FXDR.WriteString(aName + #0);
  FXDR.WriteInt32(0);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.ExecImmediate(aTrHandle, aDbHandle: integer;
  aDialect: integer; const sql: AnsiString);
begin
  FXDR.WriteInt32(op_exec_immediate);
  FXDR.WriteInt32(aTrHandle);
  FXDR.WriteInt32(0);          {statement}
  FXDR.WriteInt32(aDialect);
  FXDR.WriteString(sql);
  FXDR.WriteString('');        {items}
  FXDR.WriteInt32(0);          {buffer length}
  if FProtocolVersion >= (PROTOCOL_VERSION20 and FB_PROTOCOL_MASK) then
    FXDR.WriteInt32(0);        {p_sqlst_flags}
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

{--- information calls ---}

function TFBWireConnection.GetInfo(aOperation: integer; aHandle: integer;
  const aItems: TBytes; aBufferLength: integer): TBytes;
begin
  FXDR.WriteInt32(aOperation);
  FXDR.WriteInt32(aHandle);
  FXDR.WriteInt32(0);   {incarnation}
  FXDR.WriteString(aItems);
  FXDR.WriteInt32(aBufferLength);
  FXDR.Flush;
  Result := ReceiveAndCheckResponse.Data;
end;

{--- blobs ---}

procedure TFBWireConnection.CreateBlob(aTrHandle: integer; const BPB: TBytes;
  var aBlobHandle: integer; var aBlobID: Int64);
var R: TWireResponse;
begin
  FXDR.WriteInt32(op_create_blob2);
  FXDR.WriteString(BPB);
  FXDR.WriteInt32(aTrHandle);
  FXDR.WriteInt64(0);
  FXDR.Flush;
  R := ReceiveAndCheckResponse;
  aBlobHandle := R.ObjectHandle;
  aBlobID := R.ObjectID;
end;

function TFBWireConnection.OpenBlob(aTrHandle: integer; const BPB: TBytes;
  aBlobID: Int64): integer;
begin
  FXDR.WriteInt32(op_open_blob2);
  FXDR.WriteString(BPB);
  FXDR.WriteInt32(aTrHandle);
  FXDR.WriteInt64(aBlobID);
  FXDR.Flush;
  Result := ReceiveAndCheckResponse.ObjectHandle;
end;

function TFBWireConnection.GetSegment(aBlobHandle: integer;
  aBufferLength: integer; var aEOB: boolean): TBytes;
var R: TWireResponse;
    pos, segLen, outLen: integer;
begin
  FXDR.WriteInt32(op_get_segment);
  FXDR.WriteInt32(aBlobHandle);
  FXDR.WriteInt32(aBufferLength);
  FXDR.WriteString('');   {no data for get}
  FXDR.Flush;
  R := ReceiveAndCheckResponse;
  {p_resp_object: 0 = more, 1 = fragment, 2 = end of blob}
  aEOB := R.ObjectHandle = 2;
  {response data contains segments in 2 byte little endian length prefixed
   form - concatenate them}
  SetLength(Result,Length(R.Data));
  pos := 0;
  outLen := 0;
  while pos + 2 <= Length(R.Data) do
  begin
    segLen := R.Data[pos] or (R.Data[pos+1] shl 8);
    Inc(pos,2);
    if pos + segLen > Length(R.Data) then
      segLen := Length(R.Data) - pos;
    if segLen > 0 then
    begin
      Move(R.Data[pos],Result[outLen],segLen);
      Inc(outLen,segLen);
      Inc(pos,segLen);
    end;
  end;
  SetLength(Result,outLen);
end;

procedure TFBWireConnection.PutSegment(aBlobHandle: integer; const aData: TBytes);
begin
  FXDR.WriteInt32(op_put_segment);
  FXDR.WriteInt32(aBlobHandle);
  FXDR.WriteInt32(Length(aData));
  FXDR.WriteString(aData);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.CloseBlob(aBlobHandle: integer);
begin
  FXDR.WriteInt32(op_close_blob);
  FXDR.WriteInt32(aBlobHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.CancelBlob(aBlobHandle: integer);
begin
  FXDR.WriteInt32(op_cancel_blob);
  FXDR.WriteInt32(aBlobHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

{--- array slices ---}

{Both packets are P_SLC: transaction, array id quad, slice length, the
 SDL, a (here always empty) parameter vector, then the slice data - which
 for the request side of op_get_slice is just a zero length. The reply to
 op_get_slice is op_slice; op_put_slice gets a normal op_response with the
 array id in ObjectID. See op_get_slice/op_put_slice in
 src/remote/protocol.cpp.}

function TFBWireConnection.GetSlice(aTrHandle: integer; aArrayID: Int64;
  const SDL: TBytes; const aLayout: TWireSliceLayout; aBuffer: PByte): integer;
var op: integer;
    R: TWireResponse;
    wireLen: cardinal;
begin
  FXDR.WriteInt32(op_get_slice);
  FXDR.WriteInt32(aTrHandle);
  FXDR.WriteInt64(aArrayID);
  FXDR.WriteInt32(SliceLength(aLayout));
  FXDR.WriteString(SDL);
  FXDR.WriteInt32(0);              {p_slc_parameters: no longs}
  FXDR.WriteInt32(0);              {slice data: none on a get request}
  FXDR.Flush;
  op := ReadOperation;
  if op = op_response then
  begin
    {an error - a success op_response here would be a protocol violation}
    R := ReadResponseBody;
    CheckResponse(R);
    raise EFBWireError.Create('Unexpected op_response to op_get_slice');
  end;
  if op <> op_slice then
    raise EFBWireError.CreateFmt('Unexpected operation %d in get_slice response',[op]);
  Result := FXDR.ReadInt32;        {p_slr_length}
  wireLen := FXDR.ReadUInt32;      {lstr_length}
  XDRDecodeSlice(FXDR,aLayout,aBuffer,wireLen);
end;

function TFBWireConnection.PutSlice(aTrHandle: integer; aArrayID: Int64;
  const SDL: TBytes; const aLayout: TWireSliceLayout; aBuffer: PByte): Int64;
begin
  FXDR.WriteInt32(op_put_slice);
  FXDR.WriteInt32(aTrHandle);
  FXDR.WriteInt64(aArrayID);
  FXDR.WriteInt32(SliceLength(aLayout));
  FXDR.WriteString(SDL);
  FXDR.WriteInt32(0);              {p_slc_parameters: no longs}
  FXDR.WriteInt32(SliceLength(aLayout));  {lstr_length prefix of the data}
  XDREncodeSlice(FXDR,aLayout,aBuffer);
  FXDR.Flush;
  Result := ReceiveAndCheckResponse.ObjectID;
end;

{--- events ---}

function TFBWireConnection.ConnectRequest(aDbHandle: integer): integer;
var R: TWireResponse;
begin
  FXDR.WriteInt32(op_connect_request);
  FXDR.WriteInt32(P_REQ_async);
  FXDR.WriteInt32(aDbHandle);
  FXDR.WriteInt32(0);            {p_req_partner}
  FXDR.Flush;
  R := ReceiveAndCheckResponse;
  {the response data is the server's sockaddr. The port is a big endian
   16 bit value at offset 2 for both AF_INET and AF_INET6.}
  if Length(R.Data) < 4 then
    raise EFBWireError.Create('op_connect_request returned no address');
  Result := (integer(R.Data[2]) shl 8) or R.Data[3];
end;

procedure TFBWireConnection.QueEvents(aDbHandle: integer; const aEPB: TBytes;
  aEventID: integer);
begin
  FXDR.WriteInt32(op_que_events);
  FXDR.WriteInt32(aDbHandle);
  FXDR.WriteString(aEPB);
  FXDR.WriteInt32(0);            {p_event_ast - parsed but ignored}
  FXDR.WriteInt32(0);            {p_event_arg - ditto}
  FXDR.WriteInt32(aEventID);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.CancelEvents(aDbHandle, aEventID: integer);
begin
  FXDR.WriteInt32(op_cancel_events);
  FXDR.WriteInt32(aDbHandle);
  FXDR.WriteInt32(aEventID);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

{--- services ---}

function TFBWireConnection.ServiceAttach(const aServiceName: AnsiString;
  SPB: TBytes): integer;
begin
  FXDR.WriteInt32(op_service_attach);
  FXDR.WriteInt32(0);
  FXDR.WriteString(aServiceName);
  FXDR.WriteString(SPB);
  FXDR.Flush;
  Result := ReceiveAndCheckResponse.ObjectHandle;
end;

procedure TFBWireConnection.ServiceDetach(aSvcHandle: integer);
begin
  FXDR.WriteInt32(op_service_detach);
  FXDR.WriteInt32(aSvcHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.ServiceStart(aSvcHandle: integer; const aItems: TBytes);
begin
  FXDR.WriteInt32(op_service_start);
  FXDR.WriteInt32(aSvcHandle);
  FXDR.WriteInt32(0);
  FXDR.WriteString(aItems);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

function TFBWireConnection.ServiceQuery(aSvcHandle: integer; const aSendItems,
  aRecvItems: TBytes; aBufferLength: integer): TBytes;
begin
  FXDR.WriteInt32(op_service_info);
  FXDR.WriteInt32(aSvcHandle);
  FXDR.WriteInt32(0);
  FXDR.WriteString(aSendItems);
  FXDR.WriteString(aRecvItems);
  FXDR.WriteInt32(aBufferLength);
  FXDR.Flush;
  Result := ReceiveAndCheckResponse.Data;
end;

{--- batches (protocol 16) ---}

procedure TFBWireConnection.BatchCreate(aStmtHandle: integer;
  const aFormat: TWireMessageFormat; aMsgLen: cardinal; const aPB: TBytes);
var blr: TBytes;
begin
  if FProtocolVersion < (PROTOCOL_VERSION16 and FB_PROTOCOL_MASK) then
    raise EFBWireError.Create('the batch operations need protocol 16 or later');
  FXDR.WriteInt32(op_batch_create);
  FXDR.WriteInt32(aStmtHandle);
  blr := BuildMessageBlr(aFormat);
  FXDR.WriteString(blr);
  FXDR.WriteUInt32(aMsgLen);
  FXDR.WriteString(aPB);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.BatchMsg(aStmtHandle: integer;
  const aFormat: TWireMessageFormat; const aRows: array of TBytes);
var i: integer;
begin
  FXDR.WriteInt32(op_batch_msg);
  FXDR.WriteInt32(aStmtHandle);
  FXDR.WriteUInt32(Length(aRows));
  for i := 0 to High(aRows) do
    XDREncodeMessage(FXDR,aFormat,@aRows[i][0]);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.BatchRegBlob(aStmtHandle: integer;
  aExistingID, aBatchID: Int64);
begin
  FXDR.WriteInt32(op_batch_regblob);
  FXDR.WriteInt32(aStmtHandle);
  FXDR.WriteInt64(aExistingID);
  FXDR.WriteInt64(aBatchID);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

function TFBWireConnection.BatchExec(aStmtHandle, aTrHandle: integer): TWireBatchCS;
var op: integer;
    R: TWireResponse;
    recCount, updates, vectors, errors: cardinal;
    i: cardinal;
    pos: cardinal;
begin
  SetLength(Result.States,0);
  SetLength(Result.StatusVectors,0);
  FXDR.WriteInt32(op_batch_exec);
  FXDR.WriteInt32(aStmtHandle);
  FXDR.WriteInt32(aTrHandle);
  FXDR.Flush;
  op := ReadOperation;
  if op = op_response then
  begin
    {an error before execution}
    R := ReadResponseBody;
    CheckResponse(R);
    raise EFBWireError.Create('Unexpected op_response to op_batch_exec');
  end;
  if op <> op_batch_cs then
    raise EFBWireError.CreateFmt('Unexpected operation %d in batch_exec response',[op]);

  FXDR.ReadInt32;                 {statement handle}
  recCount := FXDR.ReadUInt32;
  updates := FXDR.ReadUInt32;
  vectors := FXDR.ReadUInt32;
  errors := FXDR.ReadUInt32;

  {with no update vector every row processed reports "success, no info"}
  if updates = 0 then
  begin
    SetLength(Result.States,recCount);
    for i := 1 to recCount do
      Result.States[i-1] := BATCH_SUCCESS_NO_INFO;
  end
  else
  begin
    SetLength(Result.States,updates);
    for i := 1 to updates do
      Result.States[i-1] := FXDR.ReadInt32;
  end;
  SetLength(Result.StatusVectors,Length(Result.States));

  {failed rows with a status vector}
  for i := 1 to vectors do
  begin
    pos := FXDR.ReadUInt32;
    if pos < cardinal(Length(Result.States)) then
    begin
      Result.States[pos] := BATCH_EXECUTE_FAILED;
      Result.StatusVectors[pos] := ReadStatusVector;
    end
    else
      ReadStatusVector;  {out of range - consume and discard}
  end;

  {failed rows reported without a vector}
  for i := 1 to errors do
  begin
    pos := FXDR.ReadUInt32;
    if pos < cardinal(Length(Result.States)) then
      Result.States[pos] := BATCH_EXECUTE_FAILED;
  end;
end;

procedure TFBWireConnection.BatchRelease(aStmtHandle: integer);
begin
  FXDR.WriteInt32(op_batch_rls);
  FXDR.WriteInt32(aStmtHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.BatchCancel(aStmtHandle: integer);
begin
  FXDR.WriteInt32(op_batch_cancel);
  FXDR.WriteInt32(aStmtHandle);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

{--- misc ---}

procedure TFBWireConnection.Ping;
begin
  FXDR.WriteInt32(op_ping);
  FXDR.Flush;
  ReceiveAndCheckResponse;
end;

procedure TFBWireConnection.SendCancel(aKind: integer);
var pkt: array[0..7] of byte;
begin
  {assembled by hand and sent through SendDirect rather than the shared
   XDR buffer: the whole point is that another thread may be mid exchange}
  pkt[0] := (op_cancel shr 24) and $FF;
  pkt[1] := (op_cancel shr 16) and $FF;
  pkt[2] := (op_cancel shr 8) and $FF;
  pkt[3] := op_cancel and $FF;
  pkt[4] := (aKind shr 24) and $FF;
  pkt[5] := (aKind shr 16) and $FF;
  pkt[6] := (aKind shr 8) and $FF;
  pkt[7] := aKind and $FF;
  FTransport.SendDirect(pkt,8);
end;

end.
