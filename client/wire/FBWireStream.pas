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
unit FBWireStream;

{ TCP transport and XDR encoding layer for the Firebird wire protocol.

  The remote protocol encodes all packets using a subset of XDR (RFC 4506):
  - all integers are 4 byte big-endian, 64 bit values as two 4 byte words
  - opaque byte strings are padded with zeroes to a multiple of 4 bytes
  - counted strings ("cstring") are a 4 byte length followed by padded opaque

  TFBWireTransport provides a buffered TCP connection with optional
  symmetric stream encryption (wire encryption) that can be switched on
  mid-stream after the op_crypt handshake.
}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils
  {$IFDEF FPC}
  , sockets, ssockets
  {$ENDIF}
  ;

const
  DefaultRecvBufferSize = 32 * 1024;
  DefaultSendBufferSize = 32 * 1024;

type
  { TWireCipher: a symmetric stream cipher filter. Separate instances are
    used for the send and receive directions. }

  TWireCipher = class
  public
    procedure Process(var aData; aLen: integer); virtual; abstract;
  end;

  { Raised on network level errors. The protocol layer translates this
    into an EIBInterBaseError with isc_network_error where appropriate. }
  EFBWireError = class(Exception);

  { TFBWireTransport }

  TFBWireTransport = class
  private
    {$IFDEF FPC}
    FSocket: TInetSocket;
    {$ENDIF}
    FConnected: boolean;
    FSendCipher: TWireCipher;
    FRecvCipher: TWireCipher;
    FRecvBuffer: TBytes;
    FRecvPos: integer;      {next unread byte}
    FRecvLimit: integer;    {end of valid data}
    FSendBuffer: TBytes;
    FSendPos: integer;
    procedure FillRecvBuffer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConnectTo(const aHost: AnsiString; aPort: integer; aTimeout: integer = 0);
    procedure Disconnect;
    {read exactly aLen bytes (blocking)}
    procedure ReadBytes(var aData; aLen: integer);
    {queue bytes for sending}
    procedure WriteBytes(const aData; aLen: integer);
    {send all queued bytes}
    procedure Flush;
    {true if unread data is buffered locally (does not poll the socket)}
    function HasBufferedData: boolean;
    {switch on wire encryption. Transport takes ownership of the ciphers.
     The two directions are enabled separately: the receive cipher is
     installed after op_crypt has been sent (the server encrypts from the
     moment it receives op_crypt) while the send cipher is only installed
     once the op_crypt response has been validated. EnableRecvCipher must
     be called with an empty receive buffer, i.e. with no partially
     consumed server packet.}
    procedure EnableRecvCipher(aRecvCipher: TWireCipher);
    procedure EnableSendCipher(aSendCipher: TWireCipher);
    property Connected: boolean read FConnected;
  end;

  { TXDRStream: XDR encode/decode over a TFBWireTransport }

  TXDRStream = class
  private
    FTransport: TFBWireTransport;
  public
    constructor Create(aTransport: TFBWireTransport);

    {receive side}
    function ReadInt32: Integer;
    function ReadUInt32: Cardinal;
    function ReadInt64: Int64;
    {opaque of known length + skip padding}
    function ReadOpaque(aLen: integer): TBytes;
    procedure ReadRaw(var aData; aLen: integer);
    {4 byte count followed by padded opaque}
    function ReadString: TBytes;
    function ReadStringAsAnsi: AnsiString;
    procedure SkipBytes(aLen: integer);

    {send side - data is queued until Flush}
    procedure WriteInt32(aValue: Integer);
    procedure WriteUInt32(aValue: Cardinal);
    procedure WriteInt64(aValue: Int64);
    procedure WriteRaw(const aData; aLen: integer);
    {opaque padded to multiple of 4, no length prefix}
    procedure WriteOpaque(const aData: TBytes);
    {4 byte length followed by padded opaque}
    procedure WriteString(const aData: TBytes); overload;
    procedure WriteString(const aData: AnsiString); overload;
    procedure Flush;

    property Transport: TFBWireTransport read FTransport;
  end;

function XDRPadLength(aLen: integer): integer;

implementation

function XDRPadLength(aLen: integer): integer;
begin
  Result := (4 - (aLen and 3)) and 3;
end;

{ TFBWireTransport }

constructor TFBWireTransport.Create;
begin
  inherited Create;
  SetLength(FRecvBuffer,DefaultRecvBufferSize);
  SetLength(FSendBuffer,DefaultSendBufferSize);
  FRecvPos := 0;
  FRecvLimit := 0;
  FSendPos := 0;
end;

destructor TFBWireTransport.Destroy;
begin
  Disconnect;
  if FSendCipher <> nil then FSendCipher.Free;
  if FRecvCipher <> nil then FRecvCipher.Free;
  inherited Destroy;
end;

procedure TFBWireTransport.ConnectTo(const aHost: AnsiString; aPort: integer;
  aTimeout: integer);
{$IF declared(TCP_NODELAY) and declared(IPPROTO_TCP)}
var NoDelay: integer;
{$IFEND}
begin
  Disconnect;
  {$IFDEF FPC}
  try
    FSocket := TInetSocket.Create(aHost,aPort);
    if aTimeout > 0 then
      FSocket.IOTimeout := aTimeout;
    {disable Nagle: the protocol is strictly request/response, so waiting
     to coalesce a packet only adds latency. Not every platform unit
     declares the option, and it is only an optimisation.}
    {$IF declared(TCP_NODELAY) and declared(IPPROTO_TCP)}
    NoDelay := 1;
    fpsetsockopt(FSocket.Handle,IPPROTO_TCP,TCP_NODELAY,@NoDelay,SizeOf(NoDelay));
    {$IFEND}
  except
    on E: Exception do
      raise EFBWireError.Create(
        Format('Unable to connect to server "%s" port %d: %s',
        [aHost,aPort,E.Message]));
  end;
  {$ELSE}
  raise Exception.Create('Wire protocol transport is not implemented for this compiler');
  {$ENDIF}
  FConnected := true;
end;

procedure TFBWireTransport.Disconnect;
begin
  {$IFDEF FPC}
  if FSocket <> nil then
    FreeAndNil(FSocket);
  {$ENDIF}
  FConnected := false;
  FRecvPos := 0;
  FRecvLimit := 0;
  FSendPos := 0;
end;

procedure TFBWireTransport.FillRecvBuffer;
var got: integer;
begin
  if not FConnected then
    raise EFBWireError.Create('Wire transport is not connected');
  FRecvPos := 0;
  FRecvLimit := 0;
  {$IFDEF FPC}
  got := FSocket.Read(FRecvBuffer[0],Length(FRecvBuffer));
  {$ELSE}
  got := 0;
  {$ENDIF}
  if got <= 0 then
  begin
    Disconnect;
    raise EFBWireError.Create('Connection lost to database server');
  end;
  if FRecvCipher <> nil then
    FRecvCipher.Process(FRecvBuffer[0],got);
  FRecvLimit := got;
end;

procedure TFBWireTransport.ReadBytes(var aData; aLen: integer);
var p: PByte;
    chunk: integer;
begin
  p := @aData;
  while aLen > 0 do
  begin
    if FRecvPos = FRecvLimit then
      FillRecvBuffer;
    chunk := FRecvLimit - FRecvPos;
    if chunk > aLen then chunk := aLen;
    Move(FRecvBuffer[FRecvPos],p^,chunk);
    Inc(FRecvPos,chunk);
    Inc(p,chunk);
    Dec(aLen,chunk);
  end;
end;

procedure TFBWireTransport.WriteBytes(const aData; aLen: integer);
var p: PByte;
    chunk: integer;
begin
  p := @aData;
  while aLen > 0 do
  begin
    if FSendPos = Length(FSendBuffer) then
      Flush;
    chunk := Length(FSendBuffer) - FSendPos;
    if chunk > aLen then chunk := aLen;
    Move(p^,FSendBuffer[FSendPos],chunk);
    Inc(FSendPos,chunk);
    Inc(p,chunk);
    Dec(aLen,chunk);
  end;
end;

procedure TFBWireTransport.Flush;
var written, total: integer;
begin
  if FSendPos = 0 then Exit;
  if not FConnected then
    raise EFBWireError.Create('Wire transport is not connected');
  if FSendCipher <> nil then
    FSendCipher.Process(FSendBuffer[0],FSendPos);
  total := 0;
  {$IFDEF FPC}
  while total < FSendPos do
  begin
    written := FSocket.Write(FSendBuffer[total],FSendPos - total);
    if written <= 0 then
    begin
      Disconnect;
      raise EFBWireError.Create('Connection lost to database server');
    end;
    Inc(total,written);
  end;
  {$ENDIF}
  FSendPos := 0;
end;

function TFBWireTransport.HasBufferedData: boolean;
begin
  Result := FRecvPos < FRecvLimit;
end;

procedure TFBWireTransport.EnableRecvCipher(aRecvCipher: TWireCipher);
begin
  if HasBufferedData then
    raise EFBWireError.Create(
      'Cannot enable wire encryption: unread data in receive buffer');
  FRecvCipher := aRecvCipher;
end;

procedure TFBWireTransport.EnableSendCipher(aSendCipher: TWireCipher);
begin
  Flush;
  FSendCipher := aSendCipher;
end;

{ TXDRStream }

constructor TXDRStream.Create(aTransport: TFBWireTransport);
begin
  inherited Create;
  FTransport := aTransport;
end;

function TXDRStream.ReadInt32: Integer;
begin
  Result := Integer(ReadUInt32);
end;

function TXDRStream.ReadUInt32: Cardinal;
var b: array[0..3] of byte;
begin
  FTransport.ReadBytes(b,4);
  Result := (Cardinal(b[0]) shl 24) or (Cardinal(b[1]) shl 16) or
            (Cardinal(b[2]) shl 8) or Cardinal(b[3]);
end;

function TXDRStream.ReadInt64: Int64;
var hi, lo: Cardinal;
begin
  {transmitted as two 32 bit words, high word first}
  hi := ReadUInt32;
  lo := ReadUInt32;
  Result := (Int64(hi) shl 32) or Int64(lo);
end;

function TXDRStream.ReadOpaque(aLen: integer): TBytes;
begin
  SetLength(Result,aLen);
  if aLen > 0 then
    FTransport.ReadBytes(Result[0],aLen);
  SkipBytes(XDRPadLength(aLen));
end;

procedure TXDRStream.ReadRaw(var aData; aLen: integer);
begin
  FTransport.ReadBytes(aData,aLen);
end;

function TXDRStream.ReadString: TBytes;
var len: integer;
begin
  len := ReadInt32;
  if (len < 0) or (len > 1 shl 28) then
    raise EFBWireError.Create(
      Format('Invalid string length %d in server response',[len]));
  Result := ReadOpaque(len);
end;

function TXDRStream.ReadStringAsAnsi: AnsiString;
var b: TBytes;
begin
  b := ReadString;
  SetLength(Result,Length(b));
  if Length(b) > 0 then
    Move(b[0],Result[1],Length(b));
end;

procedure TXDRStream.SkipBytes(aLen: integer);
var dummy: array[0..7] of byte;
    chunk: integer;
begin
  while aLen > 0 do
  begin
    chunk := aLen;
    if chunk > SizeOf(dummy) then chunk := SizeOf(dummy);
    FTransport.ReadBytes(dummy,chunk);
    Dec(aLen,chunk);
  end;
end;

procedure TXDRStream.WriteInt32(aValue: Integer);
begin
  WriteUInt32(Cardinal(aValue));
end;

procedure TXDRStream.WriteUInt32(aValue: Cardinal);
var b: array[0..3] of byte;
begin
  b[0] := (aValue shr 24) and $FF;
  b[1] := (aValue shr 16) and $FF;
  b[2] := (aValue shr 8) and $FF;
  b[3] := aValue and $FF;
  FTransport.WriteBytes(b,4);
end;

procedure TXDRStream.WriteInt64(aValue: Int64);
begin
  WriteUInt32(Cardinal(QWord(aValue) shr 32));
  WriteUInt32(Cardinal(QWord(aValue) and $FFFFFFFF));
end;

procedure TXDRStream.WriteRaw(const aData; aLen: integer);
begin
  FTransport.WriteBytes(aData,aLen);
end;

procedure TXDRStream.WriteOpaque(const aData: TBytes);
const
  Zeroes: array[0..3] of byte = (0,0,0,0);
begin
  if Length(aData) > 0 then
    FTransport.WriteBytes(aData[0],Length(aData));
  if XDRPadLength(Length(aData)) > 0 then
    FTransport.WriteBytes(Zeroes,XDRPadLength(Length(aData)));
end;

procedure TXDRStream.WriteString(const aData: TBytes);
begin
  WriteInt32(Length(aData));
  WriteOpaque(aData);
end;

procedure TXDRStream.WriteString(const aData: AnsiString);
var b: TBytes;
begin
  SetLength(b,Length(aData));
  if Length(aData) > 0 then
    Move(aData[1],b[0],Length(aData));
  WriteString(b);
end;

procedure TXDRStream.Flush;
begin
  FTransport.Flush;
end;

end.
