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
unit FBWireBigInt;

{ Minimal arbitrary precision unsigned integer arithmetic. Provides just
  enough functionality to support the SRP-6a authentication handshake used
  by the Firebird Srp/Srp256 authentication plugins: conversion to/from
  big-endian byte strings and hex strings, addition, subtraction,
  multiplication, division/modulus (Knuth Algorithm D) and modular
  exponentiation (square and multiply). }

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
  Classes, SysUtils;

type
  TLimb = Cardinal;          {32 bit limb}
  TDblLimb = QWord;          {64 bit intermediate}

  { TBigInt: value = sum(FLimbs[i] * 2^(32*i)) - little endian limb order.
    Always normalised so that the most significant limb is non-zero
    (zero is represented by an empty limb array). }

  TBigInt = record
  private
    FLimbs: array of TLimb;
    procedure Normalise;
    function GetLimbCount: integer;
  public
    class function FromBytes(const aValue: TBytes): TBigInt; static;
    class function FromHex(aValue: AnsiString): TBigInt; static;
    class function FromCardinal(aValue: cardinal): TBigInt; static;
    {big-endian, no leading zeroes (empty for zero)}
    function ToBytes: TBytes; overload;
    {big-endian, left padded with zeroes to aLength bytes}
    function ToBytes(aLength: integer): TBytes; overload;
    function ToHex: AnsiString;  {lower case, no leading zeroes, '0' for zero }
    function IsZero: boolean;
    function BitLength: integer;
    function ByteLength: integer;
    class function Compare(const a, b: TBigInt): integer; static;
    class function Add(const a, b: TBigInt): TBigInt; static;
    {requires a >= b}
    class function Subtract(const a, b: TBigInt): TBigInt; static;
    class function Multiply(const a, b: TBigInt): TBigInt; static;
    class procedure DivMod(const a, b: TBigInt; var Quotient, Remainder: TBigInt); static;
    class function Modulus(const a, b: TBigInt): TBigInt; static;
    {(aBase ^ aExponent) mod aModulus}
    class function ModPow(const aBase, aExponent, aModulus: TBigInt): TBigInt; static;
    property LimbCount: integer read GetLimbCount;
  end;

  EBigIntError = class(Exception);

implementation

{ TBigInt }

procedure TBigInt.Normalise;
var i: integer;
begin
  i := Length(FLimbs);
  while (i > 0) and (FLimbs[i-1] = 0) do
    Dec(i);
  if i <> Length(FLimbs) then
    SetLength(FLimbs,i);
end;

function TBigInt.GetLimbCount: integer;
begin
  Result := Length(FLimbs);
end;

class function TBigInt.FromBytes(const aValue: TBytes): TBigInt;
var i, limbIndex, shift: integer;
begin
  SetLength(Result.FLimbs,(Length(aValue) + 3) div 4);
  for i := 0 to Length(Result.FLimbs) - 1 do
    Result.FLimbs[i] := 0;
  {aValue is big-endian: last byte is least significant}
  for i := 0 to Length(aValue) - 1 do
  begin
    limbIndex := (Length(aValue) - 1 - i) div 4;
    shift := ((Length(aValue) - 1 - i) mod 4) * 8;
    Result.FLimbs[limbIndex] := Result.FLimbs[limbIndex] or (TLimb(aValue[i]) shl shift);
  end;
  Result.Normalise;
end;

class function TBigInt.FromHex(aValue: AnsiString): TBigInt;
var bytes: TBytes;
    i: integer;
    b: integer;

  function NibbleValue(c: AnsiChar): integer;
  begin
    case c of
    '0'..'9': Result := ord(c) - ord('0');
    'a'..'f': Result := ord(c) - ord('a') + 10;
    'A'..'F': Result := ord(c) - ord('A') + 10;
    else
      raise EBigIntError.CreateFmt('Invalid hex digit "%s"',[c]);
    end;
  end;

begin
  if Odd(Length(aValue)) then
    aValue := '0' + aValue;
  SetLength(bytes,Length(aValue) div 2);
  for i := 0 to Length(bytes) - 1 do
  begin
    b := (NibbleValue(aValue[2*i+1]) shl 4) or NibbleValue(aValue[2*i+2]);
    bytes[i] := b;
  end;
  Result := FromBytes(bytes);
end;

class function TBigInt.FromCardinal(aValue: cardinal): TBigInt;
begin
  if aValue = 0 then
    SetLength(Result.FLimbs,0)
  else
  begin
    SetLength(Result.FLimbs,1);
    Result.FLimbs[0] := aValue;
  end;
end;

function TBigInt.ToBytes: TBytes;
begin
  Result := ToBytes(ByteLength);
end;

function TBigInt.ToBytes(aLength: integer): TBytes;
var i, limbIndex, shift: integer;
begin
  SetLength(Result,aLength);
  for i := 0 to aLength - 1 do
  begin
    limbIndex := (aLength - 1 - i) div 4;
    shift := ((aLength - 1 - i) mod 4) * 8;
    if limbIndex < Length(FLimbs) then
      Result[i] := (FLimbs[limbIndex] shr shift) and $FF
    else
      Result[i] := 0;
  end;
end;

function TBigInt.ToHex: AnsiString;
const
  HexDigits: array[0..15] of AnsiChar = '0123456789abcdef';
var bytes: TBytes;
    i: integer;
begin
  if IsZero then
    Exit('0');
  bytes := ToBytes;
  SetLength(Result,Length(bytes)*2);
  for i := 0 to Length(bytes) - 1 do
  begin
    Result[2*i+1] := HexDigits[bytes[i] shr 4];
    Result[2*i+2] := HexDigits[bytes[i] and $F];
  end;
  {strip a single leading zero nibble if present}
  if (Length(Result) > 1) and (Result[1] = '0') then
    system.Delete(Result,1,1);
end;

function TBigInt.IsZero: boolean;
begin
  Result := Length(FLimbs) = 0;
end;

function TBigInt.BitLength: integer;
var top: TLimb;
begin
  if IsZero then
    Exit(0);
  Result := (Length(FLimbs) - 1) * 32;
  top := FLimbs[Length(FLimbs)-1];
  while top <> 0 do
  begin
    Inc(Result);
    top := top shr 1;
  end;
end;

function TBigInt.ByteLength: integer;
begin
  Result := (BitLength + 7) div 8;
end;

class function TBigInt.Compare(const a, b: TBigInt): integer;
var i: integer;
begin
  if Length(a.FLimbs) <> Length(b.FLimbs) then
  begin
    if Length(a.FLimbs) > Length(b.FLimbs) then
      Exit(1)
    else
      Exit(-1);
  end;
  for i := Length(a.FLimbs) - 1 downto 0 do
    if a.FLimbs[i] <> b.FLimbs[i] then
    begin
      if a.FLimbs[i] > b.FLimbs[i] then
        Exit(1)
      else
        Exit(-1);
    end;
  Result := 0;
end;

class function TBigInt.Add(const a, b: TBigInt): TBigInt;
var i, n: integer;
    carry: TDblLimb;
    av, bv: TDblLimb;
begin
  n := Length(a.FLimbs);
  if Length(b.FLimbs) > n then
    n := Length(b.FLimbs);
  SetLength(Result.FLimbs,n+1);
  carry := 0;
  for i := 0 to n - 1 do
  begin
    if i < Length(a.FLimbs) then av := a.FLimbs[i] else av := 0;
    if i < Length(b.FLimbs) then bv := b.FLimbs[i] else bv := 0;
    carry := carry + av + bv;
    Result.FLimbs[i] := TLimb(carry and $FFFFFFFF);
    carry := carry shr 32;
  end;
  Result.FLimbs[n] := TLimb(carry);
  Result.Normalise;
end;

class function TBigInt.Subtract(const a, b: TBigInt): TBigInt;
var i: integer;
    borrow: Int64;
    av, bv: Int64;
begin
  if Compare(a,b) < 0 then
    raise EBigIntError.Create('BigInt Subtract would give negative result');
  SetLength(Result.FLimbs,Length(a.FLimbs));
  borrow := 0;
  for i := 0 to Length(a.FLimbs) - 1 do
  begin
    av := a.FLimbs[i];
    if i < Length(b.FLimbs) then bv := b.FLimbs[i] else bv := 0;
    av := av - bv - borrow;
    if av < 0 then
    begin
      av := av + $100000000;
      borrow := 1;
    end
    else
      borrow := 0;
    Result.FLimbs[i] := TLimb(av);
  end;
  Result.Normalise;
end;

class function TBigInt.Multiply(const a, b: TBigInt): TBigInt;
var i, j: integer;
    carry: TDblLimb;
    t: TDblLimb;
begin
  if a.IsZero or b.IsZero then
  begin
    SetLength(Result.FLimbs,0);
    Exit;
  end;
  SetLength(Result.FLimbs,Length(a.FLimbs) + Length(b.FLimbs));
  for i := 0 to Length(Result.FLimbs) - 1 do
    Result.FLimbs[i] := 0;
  for i := 0 to Length(a.FLimbs) - 1 do
  begin
    carry := 0;
    for j := 0 to Length(b.FLimbs) - 1 do
    begin
      t := TDblLimb(a.FLimbs[i]) * TDblLimb(b.FLimbs[j]) +
           TDblLimb(Result.FLimbs[i+j]) + carry;
      Result.FLimbs[i+j] := TLimb(t and $FFFFFFFF);
      carry := t shr 32;
    end;
    Result.FLimbs[i + Length(b.FLimbs)] := TLimb(carry);
  end;
  Result.Normalise;
end;

{Knuth TAOCP Vol 2, 4.3.1 Algorithm D (see also Hacker's Delight divmnu)}
class procedure TBigInt.DivMod(const a, b: TBigInt; var Quotient, Remainder: TBigInt);
type
  TLimbArray = array of TLimb;
var
  shift: integer;
  un, vn: TLimbArray;  {normalised dividend (m+n+1 limbs) and divisor (n limbs)}
  n, m: integer;
  i, j: integer;
  qhat, rhat: TDblLimb;
  k, t: Int64;         {signed borrow accumulators}
  p, sum: TDblLimb;
  divisorTop: TLimb;

  {arithmetic (sign preserving) shift right by 32 of an Int64}
  function Sar32(aValue: Int64): Int64;
  begin
    Result := Int64(Integer(QWord(aValue) shr 32));
  end;

  {shift limb array left by byBits (0..31), result has count+extra limbs}
  function ShiftLeftLimbs(const src: array of TLimb; count, byBits, extra: integer): TLimbArray;
  var idx: integer;
  begin
    SetLength(Result,count + extra);
    for idx := 0 to High(Result) do Result[idx] := 0;
    for idx := 0 to count - 1 do
      if byBits = 0 then
        Result[idx] := src[idx]
      else
      begin
        Result[idx] := Result[idx] or (src[idx] shl byBits);
        if idx + 1 <= High(Result) then
          Result[idx+1] := src[idx] shr (32 - byBits);
      end;
  end;

begin
  if b.IsZero then
    raise EBigIntError.Create('BigInt division by zero');
  if Compare(a,b) < 0 then
  begin
    Quotient := FromCardinal(0);
    Remainder := a;
    Exit;
  end;
  n := Length(b.FLimbs);
  m := Length(a.FLimbs) - n;

  if n = 1 then
  begin
    {single limb divisor - simple case}
    SetLength(Quotient.FLimbs,Length(a.FLimbs));
    rhat := 0;
    for i := Length(a.FLimbs) - 1 downto 0 do
    begin
      sum := (rhat shl 32) or a.FLimbs[i];
      Quotient.FLimbs[i] := TLimb(sum div b.FLimbs[0]);
      rhat := sum mod b.FLimbs[0];
    end;
    Quotient.Normalise;
    Remainder := FromCardinal(TLimb(rhat));
    Exit;
  end;

  {D1: normalise so top bit of divisor is set}
  shift := 0;
  divisorTop := b.FLimbs[n-1];
  while (divisorTop and $80000000) = 0 do
  begin
    divisorTop := divisorTop shl 1;
    Inc(shift);
  end;
  vn := ShiftLeftLimbs(b.FLimbs,n,shift,0);
  un := ShiftLeftLimbs(a.FLimbs,Length(a.FLimbs),shift,1);

  SetLength(Quotient.FLimbs,m+1);
  {D2..D7: main loop}
  for j := m downto 0 do
  begin
    {D3: estimate qhat}
    sum := (TDblLimb(un[j+n]) shl 32) or un[j+n-1];
    qhat := sum div vn[n-1];
    rhat := sum mod vn[n-1];
    while (qhat > $FFFFFFFF) or
          (qhat * vn[n-2] > ((rhat shl 32) or un[j+n-2])) do
    begin
      Dec(qhat);
      rhat := rhat + vn[n-1];
      if rhat > $FFFFFFFF then break;
    end;
    {D4: multiply and subtract}
    k := 0;
    for i := 0 to n - 1 do
    begin
      p := qhat * vn[i];
      t := Int64(un[i+j]) - k - Int64(p and $FFFFFFFF);
      un[i+j] := TLimb(QWord(t) and $FFFFFFFF);
      k := Int64(p shr 32) - Sar32(t);
    end;
    t := Int64(un[j+n]) - k;
    un[j+n] := TLimb(QWord(t) and $FFFFFFFF);
    {D5/D6: if we subtracted too much, add back}
    if t < 0 then
    begin
      Dec(qhat);
      sum := 0;
      for i := 0 to n - 1 do
      begin
        sum := TDblLimb(un[i+j]) + vn[i] + (sum shr 32);
        un[i+j] := TLimb(sum and $FFFFFFFF);
      end;
      un[j+n] := TLimb(TDblLimb(un[j+n]) + (sum shr 32));
    end;
    Quotient.FLimbs[j] := TLimb(qhat);
  end;
  Quotient.Normalise;

  {D8: denormalise remainder}
  SetLength(Remainder.FLimbs,n);
  for i := 0 to n - 1 do
    if shift = 0 then
      Remainder.FLimbs[i] := un[i]
    else
      Remainder.FLimbs[i] := (un[i] shr shift) or
        (TLimb((TDblLimb(un[i+1]) shl (32 - shift)) and $FFFFFFFF));
  Remainder.Normalise;
end;

class function TBigInt.Modulus(const a, b: TBigInt): TBigInt;
var q: TBigInt;
begin
  DivMod(a,b,q,Result);
end;

class function TBigInt.ModPow(const aBase, aExponent, aModulus: TBigInt): TBigInt;
var acc: TBigInt;
    i: integer;
    bits: integer;
begin
  if aModulus.IsZero then
    raise EBigIntError.Create('BigInt ModPow with zero modulus');
  Result := Modulus(FromCardinal(1),aModulus);
  acc := Modulus(aBase,aModulus);
  bits := aExponent.BitLength;
  for i := 0 to bits - 1 do
  begin
    if (aExponent.FLimbs[i div 32] shr (i mod 32)) and 1 = 1 then
      Result := Modulus(Multiply(Result,acc),aModulus);
    if i < bits - 1 then
      acc := Modulus(Multiply(acc,acc),aModulus);
  end;
end;

end.
