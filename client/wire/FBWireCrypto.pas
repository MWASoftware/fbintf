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
unit FBWireCrypto;

{ Self-contained cryptographic primitives needed by the Firebird wire
  protocol: SHA-1 and SHA-256 message digests (Srp and Srp256 authentication),
  RC4 (Arc4 wire encryption plugin) and ChaCha20 (ChaCha/ChaCha64 wire
  encryption plugins). No external libraries are used. These are not intended
  as a general purpose crypto library. }

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$R-}{$Q-}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  TSHA1Digest = array[0..19] of byte;
  TSHA256Digest = array[0..31] of byte;

  { TSHA1 }

  TSHA1 = record
  private
    FState: array[0..4] of Cardinal;
    FBuffer: array[0..63] of byte;
    FBufLen: integer;
    FTotalLen: QWord;
    procedure Compress;
  public
    procedure Init;
    procedure Update(const aData; aLen: integer); overload;
    procedure Update(const aData: TBytes); overload;
    function Final: TSHA1Digest;
    class function Digest(const aData: TBytes): TSHA1Digest; static;
  end;

  { TSHA256 }

  TSHA256 = record
  private
    FState: array[0..7] of Cardinal;
    FBuffer: array[0..63] of byte;
    FBufLen: integer;
    FTotalLen: QWord;
    procedure Compress;
  public
    procedure Init;
    procedure Update(const aData; aLen: integer); overload;
    procedure Update(const aData: TBytes); overload;
    function Final: TSHA256Digest;
    class function Digest(const aData: TBytes): TSHA256Digest; static;
  end;

  { TRC4 - stream cipher used by the Arc4 wire encryption plugin }

  TRC4 = class
  private
    FS: array[0..255] of byte;
    Fi, Fj: byte;
  public
    constructor Create(const aKey: TBytes);
    {in-place encrypt/decrypt (RC4 is symmetric)}
    procedure Process(var aData; aLen: integer);
  end;

  { TChaCha20 - stream cipher used by the ChaCha wire encryption plugin.
    Supports the IETF variant (96 bit nonce, 32 bit counter - "ChaCha")
    and the original djb variant (64 bit nonce, 64 bit counter - "ChaCha64"). }

  TChaCha20 = class
  private
    FInput: array[0..15] of Cardinal;
    FKeyStream: array[0..63] of byte;
    FAvail: integer;   {bytes of keystream still available}
    FCounter64: boolean;
    procedure NextBlock;
  public
    {aKey must be 32 bytes. IETF: aNonce is 12 bytes, counter 32 bit.
     djb/ChaCha64: aNonce is 8 bytes, counter 64 bit.}
    constructor Create(const aKey, aNonce: TBytes; aCounter: QWord = 0);
    procedure Process(var aData; aLen: integer);
  end;

function SHA1DigestToBytes(const aDigest: TSHA1Digest): TBytes;
function SHA256DigestToBytes(const aDigest: TSHA256Digest): TBytes;

implementation

function SHA1DigestToBytes(const aDigest: TSHA1Digest): TBytes;
begin
  SetLength(Result,SizeOf(aDigest));
  Move(aDigest,Result[0],SizeOf(aDigest));
end;

function SHA256DigestToBytes(const aDigest: TSHA256Digest): TBytes;
begin
  SetLength(Result,SizeOf(aDigest));
  Move(aDigest,Result[0],SizeOf(aDigest));
end;

function RotL(x: Cardinal; n: integer): Cardinal; inline;
begin
  Result := (x shl n) or (x shr (32 - n));
end;

function RotR(x: Cardinal; n: integer): Cardinal; inline;
begin
  Result := (x shr n) or (x shl (32 - n));
end;

function SwapBE(x: Cardinal): Cardinal; inline;
begin
  Result := (x shr 24) or ((x shr 8) and $FF00) or
            ((x shl 8) and $FF0000) or (x shl 24);
end;

{ TSHA1 }

procedure TSHA1.Init;
begin
  FState[0] := $67452301;
  FState[1] := $EFCDAB89;
  FState[2] := $98BADCFE;
  FState[3] := $10325476;
  FState[4] := $C3D2E1F0;
  FBufLen := 0;
  FTotalLen := 0;
end;

procedure TSHA1.Compress;
var w: array[0..79] of Cardinal;
    a, b, c, d, e, f, k, temp: Cardinal;
    i: integer;
begin
  for i := 0 to 15 do
    w[i] := SwapBE(PCardinal(@FBuffer[i*4])^);
  for i := 16 to 79 do
    w[i] := RotL(w[i-3] xor w[i-8] xor w[i-14] xor w[i-16],1);
  a := FState[0]; b := FState[1]; c := FState[2]; d := FState[3]; e := FState[4];
  for i := 0 to 79 do
  begin
    case i of
    0..19:
      begin
        f := (b and c) or ((not b) and d);
        k := $5A827999;
      end;
    20..39:
      begin
        f := b xor c xor d;
        k := $6ED9EBA1;
      end;
    40..59:
      begin
        f := (b and c) or (b and d) or (c and d);
        k := $8F1BBCDC;
      end;
    else
      begin
        f := b xor c xor d;
        k := $CA62C1D6;
      end;
    end;
    temp := RotL(a,5) + f + e + k + w[i];
    e := d; d := c; c := RotL(b,30); b := a; a := temp;
  end;
  Inc(FState[0],a); Inc(FState[1],b); Inc(FState[2],c);
  Inc(FState[3],d); Inc(FState[4],e);
end;

procedure TSHA1.Update(const aData; aLen: integer);
var p: PByte;
    chunk: integer;
begin
  p := @aData;
  Inc(FTotalLen,aLen);
  while aLen > 0 do
  begin
    chunk := 64 - FBufLen;
    if chunk > aLen then chunk := aLen;
    Move(p^,FBuffer[FBufLen],chunk);
    Inc(FBufLen,chunk);
    Inc(p,chunk);
    Dec(aLen,chunk);
    if FBufLen = 64 then
    begin
      Compress;
      FBufLen := 0;
    end;
  end;
end;

procedure TSHA1.Update(const aData: TBytes);
begin
  if Length(aData) > 0 then
    Update(aData[0],Length(aData));
end;

function TSHA1.Final: TSHA1Digest;
var bitLen: QWord;
    i: integer;
begin
  bitLen := FTotalLen * 8;
  FBuffer[FBufLen] := $80;
  Inc(FBufLen);
  if FBufLen > 56 then
  begin
    while FBufLen < 64 do
    begin
      FBuffer[FBufLen] := 0;
      Inc(FBufLen);
    end;
    Compress;
    FBufLen := 0;
  end;
  while FBufLen < 56 do
  begin
    FBuffer[FBufLen] := 0;
    Inc(FBufLen);
  end;
  for i := 0 to 7 do
    FBuffer[56+i] := (bitLen shr ((7-i)*8)) and $FF;
  Compress;
  for i := 0 to 19 do
    Result[i] := (FState[i div 4] shr ((3 - (i mod 4))*8)) and $FF;
end;

class function TSHA1.Digest(const aData: TBytes): TSHA1Digest;
var ctx: TSHA1;
begin
  ctx.Init;
  ctx.Update(aData);
  Result := ctx.Final;
end;

{ TSHA256 }

const
  SHA256K: array[0..63] of Cardinal = (
    $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
    $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
    $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
    $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
    $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
    $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
    $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
    $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);

procedure TSHA256.Init;
begin
  FState[0] := $6a09e667; FState[1] := $bb67ae85;
  FState[2] := $3c6ef372; FState[3] := $a54ff53a;
  FState[4] := $510e527f; FState[5] := $9b05688c;
  FState[6] := $1f83d9ab; FState[7] := $5be0cd19;
  FBufLen := 0;
  FTotalLen := 0;
end;

procedure TSHA256.Compress;
var w: array[0..63] of Cardinal;
    a, b, c, d, e, f, g, h, t1, t2, s0, s1: Cardinal;
    i: integer;
begin
  for i := 0 to 15 do
    w[i] := SwapBE(PCardinal(@FBuffer[i*4])^);
  for i := 16 to 63 do
  begin
    s0 := RotR(w[i-15],7) xor RotR(w[i-15],18) xor (w[i-15] shr 3);
    s1 := RotR(w[i-2],17) xor RotR(w[i-2],19) xor (w[i-2] shr 10);
    w[i] := w[i-16] + s0 + w[i-7] + s1;
  end;
  a := FState[0]; b := FState[1]; c := FState[2]; d := FState[3];
  e := FState[4]; f := FState[5]; g := FState[6]; h := FState[7];
  for i := 0 to 63 do
  begin
    s1 := RotR(e,6) xor RotR(e,11) xor RotR(e,25);
    t1 := h + s1 + ((e and f) xor ((not e) and g)) + SHA256K[i] + w[i];
    s0 := RotR(a,2) xor RotR(a,13) xor RotR(a,22);
    t2 := s0 + ((a and b) xor (a and c) xor (b and c));
    h := g; g := f; f := e; e := d + t1;
    d := c; c := b; b := a; a := t1 + t2;
  end;
  Inc(FState[0],a); Inc(FState[1],b); Inc(FState[2],c); Inc(FState[3],d);
  Inc(FState[4],e); Inc(FState[5],f); Inc(FState[6],g); Inc(FState[7],h);
end;

procedure TSHA256.Update(const aData; aLen: integer);
var p: PByte;
    chunk: integer;
begin
  p := @aData;
  Inc(FTotalLen,aLen);
  while aLen > 0 do
  begin
    chunk := 64 - FBufLen;
    if chunk > aLen then chunk := aLen;
    Move(p^,FBuffer[FBufLen],chunk);
    Inc(FBufLen,chunk);
    Inc(p,chunk);
    Dec(aLen,chunk);
    if FBufLen = 64 then
    begin
      Compress;
      FBufLen := 0;
    end;
  end;
end;

procedure TSHA256.Update(const aData: TBytes);
begin
  if Length(aData) > 0 then
    Update(aData[0],Length(aData));
end;

function TSHA256.Final: TSHA256Digest;
var bitLen: QWord;
    i: integer;
begin
  bitLen := FTotalLen * 8;
  FBuffer[FBufLen] := $80;
  Inc(FBufLen);
  if FBufLen > 56 then
  begin
    while FBufLen < 64 do
    begin
      FBuffer[FBufLen] := 0;
      Inc(FBufLen);
    end;
    Compress;
    FBufLen := 0;
  end;
  while FBufLen < 56 do
  begin
    FBuffer[FBufLen] := 0;
    Inc(FBufLen);
  end;
  for i := 0 to 7 do
    FBuffer[56+i] := (bitLen shr ((7-i)*8)) and $FF;
  Compress;
  for i := 0 to 31 do
    Result[i] := (FState[i div 4] shr ((3 - (i mod 4))*8)) and $FF;
end;

class function TSHA256.Digest(const aData: TBytes): TSHA256Digest;
var ctx: TSHA256;
begin
  ctx.Init;
  ctx.Update(aData);
  Result := ctx.Final;
end;

{ TRC4 }

constructor TRC4.Create(const aKey: TBytes);
var i: integer;
    j: byte;
    t: byte;
begin
  inherited Create;
  if Length(aKey) = 0 then
    raise Exception.Create('RC4: empty key');
  for i := 0 to 255 do
    FS[i] := i;
  j := 0;
  for i := 0 to 255 do
  begin
    j := byte(j + FS[i] + aKey[i mod Length(aKey)]);
    t := FS[i]; FS[i] := FS[j]; FS[j] := t;
  end;
  Fi := 0;
  Fj := 0;
end;

procedure TRC4.Process(var aData; aLen: integer);
var p: PByte;
    n: integer;
    t: byte;
begin
  p := @aData;
  for n := 0 to aLen - 1 do
  begin
    Fi := byte(Fi + 1);
    Fj := byte(Fj + FS[Fi]);
    t := FS[Fi]; FS[Fi] := FS[Fj]; FS[Fj] := t;
    p[n] := p[n] xor FS[byte(FS[Fi] + FS[Fj])];
  end;
end;

{ TChaCha20 }

constructor TChaCha20.Create(const aKey, aNonce: TBytes; aCounter: QWord);
const
  Sigma: array[0..3] of Cardinal = ($61707865, $3320646e, $79622d32, $6b206574);
var i: integer;
begin
  inherited Create;
  if Length(aKey) <> 32 then
    raise Exception.Create('ChaCha20: key must be 32 bytes');
  for i := 0 to 3 do
    FInput[i] := Sigma[i];
  for i := 0 to 7 do
    FInput[4+i] := PCardinal(@aKey[i*4])^;  {little endian load}
  FCounter64 := Length(aNonce) = 8;
  case Length(aNonce) of
  12: {IETF variant: 32 bit counter + 96 bit nonce}
    begin
      FInput[12] := Cardinal(aCounter);
      FInput[13] := PCardinal(@aNonce[0])^;
      FInput[14] := PCardinal(@aNonce[4])^;
      FInput[15] := PCardinal(@aNonce[8])^;
    end;
  8: {original djb variant: 64 bit counter + 64 bit nonce}
    begin
      FInput[12] := Cardinal(aCounter and $FFFFFFFF);
      FInput[13] := Cardinal(aCounter shr 32);
      FInput[14] := PCardinal(@aNonce[0])^;
      FInput[15] := PCardinal(@aNonce[4])^;
    end;
  else
    raise Exception.Create('ChaCha20: nonce must be 8 or 12 bytes');
  end;
  FAvail := 0;
end;

procedure TChaCha20.NextBlock;
var x: array[0..15] of Cardinal;
    i: integer;

  procedure QR(a, b, c, d: integer);
  begin
    x[a] := x[a] + x[b]; x[d] := RotL(x[d] xor x[a],16);
    x[c] := x[c] + x[d]; x[b] := RotL(x[b] xor x[c],12);
    x[a] := x[a] + x[b]; x[d] := RotL(x[d] xor x[a],8);
    x[c] := x[c] + x[d]; x[b] := RotL(x[b] xor x[c],7);
  end;

begin
  for i := 0 to 15 do
    x[i] := FInput[i];
  for i := 1 to 10 do
  begin
    QR(0,4,8,12); QR(1,5,9,13); QR(2,6,10,14); QR(3,7,11,15);
    QR(0,5,10,15); QR(1,6,11,12); QR(2,7,8,13); QR(3,4,9,14);
  end;
  for i := 0 to 15 do
  begin
    x[i] := x[i] + FInput[i];
    {store little endian}
    FKeyStream[i*4] := x[i] and $FF;
    FKeyStream[i*4+1] := (x[i] shr 8) and $FF;
    FKeyStream[i*4+2] := (x[i] shr 16) and $FF;
    FKeyStream[i*4+3] := (x[i] shr 24) and $FF;
  end;
  {increment block counter}
  Inc(FInput[12]);
  if (FInput[12] = 0) and FCounter64 then
    Inc(FInput[13]);
  FAvail := 64;
end;

procedure TChaCha20.Process(var aData; aLen: integer);
var p: PByte;
    n: integer;
begin
  p := @aData;
  n := 0;
  while n < aLen do
  begin
    if FAvail = 0 then
      NextBlock;
    p[n] := p[n] xor FKeyStream[64 - FAvail];
    Dec(FAvail);
    Inc(n);
  end;
end;

end.
