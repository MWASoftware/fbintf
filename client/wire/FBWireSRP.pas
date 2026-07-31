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
unit FBWireSRP;

{ Client side of the SRP-6a authentication used by the Firebird Srp and
  Srp256 authentication plugins (also Srp384/Srp512 would only differ in
  the proof hash, which is parameterised here).

  IMPORTANT: the Firebird engine implementation deviates from the SRP
  specification in several ways and this unit deliberately mirrors those
  deviations (they are required for interoperability):

  - The exponent (a + u*x) is reduced modulo N (the spec says exponents
    are used unreduced, or reduced mod N-1).
  - The client proof is M = H(n1, n2, salt, A, B, K) where
    n1 = H(N)^H(g) mod N (a modPow, where the SRP spec has H(N) xor H(g))
    and n2 = H(uppercase(user)).
  - k = SHA1(pad128(N), pad128(g)) with both arguments left padded to
    128 bytes, whereas A, B, S, and the M components are hashed in
    minimal ("stripped") big-endian form without padding.
  - The session key K = SHA1(S) always uses SHA-1, even for Srp256; only
    the client/server proofs use the plugin's hash.

  The reference implementation is src/auth/SecureRemotePassword in the
  Firebird source tree.
}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, FBWireBigInt, FBWireCrypto;

const
  sSrpPluginName = 'Srp';         {SHA-1 proof}
  sSrp256PluginName = 'Srp256';   {SHA-256 proof}
  SRP_KEY_SIZE = 128;             {bytes in N}
  SRP_SALT_SIZE = 32;

type
  TSRPProofHash = (sphSHA1, sphSHA256);

  { TSRPClient }

  TSRPClient = class
  private
    FN: TBigInt;         {group prime}
    Fg: TBigInt;         {generator}
    Fk: TBigInt;         {multiplier k = SHA1(pad(N), pad(g))}
    FPrivKey: TBigInt;         {client private key}
    FPubKey: TBigInt;         {client public key A = g^a mod N}
    FSessionKey: TBytes; {K = SHA1(S), 20 bytes}
    function StrippedBytes(const aValue: TBigInt): TBytes;
    function HashToBigInt(const aDigest: TBytes): TBigInt;
  public
    {Generates a random client key pair. aPrivateKeyOverride is for
     testing only - pass an empty string in production use.}
    constructor Create(const aPrivateKeyOverride: AnsiString = '');
    {client public key as minimal big-endian hex - sent to the server in
     the connect user identification data}
    function PublicKeyHex: AnsiString;
    {Computes the session key and client proof from the server challenge.
     aUser: the login name (will be upper cased as required).
     aSalt: the user's salt, raw bytes as received from the server.
     aServerKeyHex: the server public key B as a hex string.
     Returns the client proof M to be sent in op_cont_auth, as raw bytes
     (send it hex encoded).}
    function ClientProof(aUser, aPassword: AnsiString; const aSalt: TBytes;
                         aServerKeyHex: AnsiString;
                         aProofHash: TSRPProofHash): TBytes;
    {20 byte session key - available after ClientProof; used for wire
     encryption (Arc4 key directly; SHA256(K) for ChaCha)}
    property SessionKey: TBytes read FSessionKey;
  end;

function GetRandomBytes(aCount: integer): TBytes;

implementation

const
  {Firebird's SRP group: 1024 bit prime (from srp.cpp) and generator 2}
  sSRPPrime =
    'E67D2E994B2F900C3F41F08F5BB2627ED0D49EE1FE767A52EFCD565CD6E76881' +
    '2C3E1E9CE8F0A8BEA6CB13CD29DDEBF7A96D4A93B55D488DF099A15C89DCB064' +
    '0738EB2CBDD9A8F7BAB561AB1B0DC1C6CDABF303264A08D1BCA932D1F1EE428B' +
    '619D970F342ABA9A65793B8B2F041AE5364350C16F735F56ECBCA87BD57B29E7';

function GetRandomBytes(aCount: integer): TBytes;
{$IFDEF UNIX}
var F: File;
    BytesRead: integer;
{$ENDIF}
var i: integer;
begin
  SetLength(Result,aCount);
  {$IFDEF UNIX}
  AssignFile(F,'/dev/urandom');
  Reset(F,1);
  try
    BlockRead(F,Result[0],aCount,BytesRead);
    if BytesRead = aCount then
      Exit;
  finally
    CloseFile(F);
  end;
  {$ENDIF}
  {fallback - randomness only protects the session key secrecy, the
   password itself is never exposed by SRP even with a weak nonce}
  for i := 0 to aCount - 1 do
    Result[i] := Random(256);
end;

{ TSRPClient }

function TSRPClient.StrippedBytes(const aValue: TBigInt): TBytes;
begin
  {minimal big-endian form, no leading zeroes}
  Result := aValue.ToBytes;
end;

function TSRPClient.HashToBigInt(const aDigest: TBytes): TBigInt;
begin
  Result := TBigInt.FromBytes(aDigest);
end;

constructor TSRPClient.Create(const aPrivateKeyOverride: AnsiString);
var ctx: TSHA1;
begin
  inherited Create;
  FN := TBigInt.FromHex(sSRPPrime);
  Fg := TBigInt.FromCardinal(2);
  {k = SHA1(pad128(N), pad128(g))}
  ctx.Init;
  ctx.Update(FN.ToBytes(SRP_KEY_SIZE));
  ctx.Update(Fg.ToBytes(SRP_KEY_SIZE));
  Fk := TBigInt.FromBytes(SHA1DigestToBytes(ctx.Final));
  {client key pair: a random in [0,N), A = g^a mod N}
  if aPrivateKeyOverride <> '' then
  begin
    FPrivKey := TBigInt.Modulus(TBigInt.FromHex(aPrivateKeyOverride),FN);
    FPubKey := TBigInt.ModPow(Fg,FPrivKey,FN);
  end
  else
  repeat
    FPrivKey := TBigInt.Modulus(TBigInt.FromBytes(GetRandomBytes(SRP_KEY_SIZE)),FN);
    FPubKey := TBigInt.ModPow(Fg,FPrivKey,FN);
  until TBigInt.Compare(FPubKey,TBigInt.FromCardinal(1)) > 0;
end;

function TSRPClient.PublicKeyHex: AnsiString;
begin
  Result := FPubKey.ToHex;
end;

function TSRPClient.ClientProof(aUser, aPassword: AnsiString;
  const aSalt: TBytes; aServerKeyHex: AnsiString;
  aProofHash: TSRPProofHash): TBytes;
var B, u, x, gx, kgx, diff, ux, aux, S: TBigInt;
    n1, n2: TBigInt;
    ctx: TSHA1;
    ctx256: TSHA256;
    userHash: TBytes;
    upperUser: AnsiString;

  function SHA1Of(const parts: array of TBytes): TBytes;
  var i: integer;
      c: TSHA1;
  begin
    c.Init;
    for i := 0 to High(parts) do
      c.Update(parts[i]);
    Result := SHA1DigestToBytes(c.Final);
  end;

  function StrToBytes(const s: AnsiString): TBytes;
  begin
    SetLength(Result,Length(s));
    if s <> '' then
      Move(s[1],Result[0],Length(s));
  end;

begin
  upperUser := AnsiUpperCase(aUser);
  B := TBigInt.FromHex(aServerKeyHex);
  if TBigInt.Compare(TBigInt.Modulus(B,FN),TBigInt.FromCardinal(2)) < 0 then
    raise EBigIntError.Create('SRP: illegal server public key');

  {u = SHA1(A, B) - stripped forms}
  u := HashToBigInt(SHA1Of([StrippedBytes(FPubKey),StrippedBytes(B)]));

  {x = SHA1(salt, SHA1(upper(user) ':' password))}
  userHash := SHA1Of([StrToBytes(upperUser + ':' + aPassword)]);
  x := HashToBigInt(SHA1Of([aSalt,userHash]));

  {S = (B - k*g^x) ^ (a + u*x) mod N, with Firebird's mod N exponent
   reduction}
  gx := TBigInt.ModPow(Fg,x,FN);
  kgx := TBigInt.Modulus(TBigInt.Multiply(Fk,gx),FN);
  if TBigInt.Compare(B,kgx) >= 0 then
    diff := TBigInt.Subtract(B,kgx)
  else
    diff := TBigInt.Subtract(TBigInt.Add(B,FN),kgx);
  diff := TBigInt.Modulus(diff,FN);
  ux := TBigInt.Modulus(TBigInt.Multiply(u,x),FN);
  aux := TBigInt.Modulus(TBigInt.Add(FPrivKey,ux),FN);
  S := TBigInt.ModPow(diff,aux,FN);

  {K = SHA1(S) - the raw 20 byte digest, leading zero bytes preserved}
  FSessionKey := SHA1Of([StrippedBytes(S)]);

  {M = H(n1, n2, salt, A, B, K) where n1 = SHA1(N)^SHA1(g) mod N and
   n2 = SHA1(upper(user)); H is the plugin's hash}
  n1 := HashToBigInt(SHA1Of([StrippedBytes(FN)]));
  n2 := HashToBigInt(SHA1Of([StrippedBytes(Fg)]));
  n1 := TBigInt.ModPow(n1,n2,FN);
  n2 := HashToBigInt(SHA1Of([StrToBytes(upperUser)]));
  case aProofHash of
  sphSHA1:
    begin
      ctx.Init;
      ctx.Update(StrippedBytes(n1));
      ctx.Update(StrippedBytes(n2));
      ctx.Update(aSalt);
      ctx.Update(StrippedBytes(FPubKey));
      ctx.Update(StrippedBytes(B));
      ctx.Update(FSessionKey);
      Result := SHA1DigestToBytes(ctx.Final);
    end;
  sphSHA256:
    begin
      ctx256.Init;
      ctx256.Update(StrippedBytes(n1));
      ctx256.Update(StrippedBytes(n2));
      ctx256.Update(aSalt);
      ctx256.Update(StrippedBytes(FPubKey));
      ctx256.Update(StrippedBytes(B));
      ctx256.Update(FSessionKey);
      Result := SHA256DigestToBytes(ctx256.Final);
    end;
  end;
end;

end.
