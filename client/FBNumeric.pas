(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API. Although predominantly
 *  a new development they include source code taken from IBX and may be
 *  considered a derived product. This software thus also includes the copyright
 *  notice and license conditions from IBX.
 *
 *  Except for those parts dervied from IBX, contents of this file are subject
 *  to the Initial Developer's Public License Version 1.0 (the "License"); you
 *  may not use this file except in compliance with the License. You may obtain a
 *  copy of the License here:
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
 *  The Original Code is (C) 2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBNumeric;

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
  Classes, SysUtils, IB, FBActivityMonitor, FmtBCD;

{ The IFBNumeric interface is a managed type to hold a fixed point integer
  as a 64-bit signed integer plus a scale factor. i.e. the scale factor is
  the base 10 exponent in

    Fixed Point Value = <64-bit signed integer> * 10^<scale factor>

}

function StrToNumeric(aValue: AnsiString): IFBNumeric;
function DoubleToNumeric(aValue: double): IFBNumeric;
function BCDToNumeric(aValue: TBCD): IFBNumeric;
function CurrToNumeric(aValue: currency): IFBNumeric;
function IntToNumeric(aValue: Int64): IFBNumeric;
function NumericFromRawValues(aValue: Int64; aScale: integer): IFBNumeric;
function NumericToDouble(aValue: Int64; aScale: integer): double;

function TryStrToNumeric(S: Ansistring; out Value: int64; out scale: integer): boolean;

function SafeSmallInt(aValue: Int64): Smallint;
function SafeInteger(aValue: Int64): integer;

{Numeric Arithmetic}
function NumericAdd(x,y: IFBNumeric): IFBNumeric; overload; {returns x + y}
function NumericSubtract(x,y: IFBNumeric): IFBNumeric; overload; {returns x - y}
function NumericMultiply(x,y: IFBNumeric): IFBNumeric; overload; {returns x * y}
function NumericDivide(x,y: IFBNumeric): IFBNumeric; overload; {returns x / y}
function NumericCompare(x,y: IFBNumeric): integer; overload; {returns -1: x < y; 0: x = y; 1: x > y}
function NegateNumeric(x: IFBNumeric): IFBNumeric; overload; {returns -x}

{integer operations}
function NumericAdd(x: IFBNumeric; y: int64): IFBNumeric; overload; {returns x + y}
function NumericSubtract(x: IFBNumeric; y: int64): IFBNumeric; overload; {returns x - y}
function NumericSubtract(x: int64; y: IFBNumeric): IFBNumeric; overload; {returns x - y}
function NumericMultiply(x: IFBNumeric; y: int64): IFBNumeric; overload; {returns x * y}
function NumericDivide(x: IFBNumeric; y: int64): IFBNumeric; overload; {returns x / y}
function NumericDivide(x: int64; y: IFBNumeric): IFBNumeric; overload; {returns x / y}
function NumericCompare(x: IFBNumeric; y: int64): integer; overload; {returns -1: x < y; 0: x = y; 1: x > y}

{floating point operations}
function NumericAdd(x: IFBNumeric; y: double): IFBNumeric; overload; {returns x + y}
function NumericSubtract(x: IFBNumeric; y: double): IFBNumeric; overload; {returns x - y}
function NumericSubtract(x: double; y: IFBNumeric): IFBNumeric; overload; {returns x - y}
function NumericMultiply(x: IFBNumeric; y: double): IFBNumeric; overload; {returns x * y}
function NumericDivide(x: IFBNumeric; y: double): IFBNumeric; overload; {returns x div y}
function NumericDivide(x: double; y: IFBNumeric): IFBNumeric; overload; {returns x div y}
function NumericCompare(x: IFBNumeric; y: double): integer; overload; {returns -1: x < y; 0: x = y; 1: x > y}

implementation

uses IBUtils, FBMessages, Math;

type

  { TIBNumeric }

  { TFBNumeric }

  TFBNumeric = class(TFBInterfacedObject,IFBNumeric)
  private
    FValue: Int64;
    FScale: integer;
  public
    constructor Create(aValue: Int64; aScale: integer);
    constructor CreateFromInt(aValue: Int64);
    constructor CreateFromStr(aValue: AnsiString);
    constructor CreateFromDouble(aValue: double);
    constructor CreateFromCurr(aValue: Currency);
    constructor CreateFromBCD(aValue: TBCD);
  public
    {IFBNumeric}
    function getRawValue: Int64;
    function getScale: integer;
    function AdjustScaleTo(aNewScale: integer): IFBNumeric;
    function getAsString: AnsiString;
    function getAsDouble: double;
    function getAsCurrency: Currency;
    function getAsBCD: TBCD;
    function getAsInt64: Int64; {scaled}
    function getAsInteger: integer; {scaled - may be truncated}
    function getAsSmallInt: SmallInt; {scaled - may be truncated}
  end;

function StrToNumeric(aValue: AnsiString): IFBNumeric;
begin
  Result :=  TFBNumeric.CreateFromStr(aValue);
end;

function DoubleToNumeric(aValue: double): IFBNumeric;
begin
  Result :=  TFBNumeric.CreateFromDouble(aValue);
end;

function BCDToNumeric(aValue: TBCD): IFBNumeric;
begin
  Result :=  TFBNumeric.CreateFromBCD(aValue);
end;

function CurrToNumeric(aValue: currency): IFBNumeric;
begin
  Result :=  TFBNumeric.CreateFromCurr(aValue);
end;

function IntToNumeric(aValue: Int64): IFBNumeric;
begin
  Result :=  TFBNumeric.CreateFromINT(aValue);
end;

function NumericFromRawValues(aValue: Int64; aScale: integer): IFBNumeric;
begin
  Result := TFBNumeric.Create(aValue,aScale);
end;

function TryStrToNumeric(S: Ansistring; out Value: int64; out scale: integer): boolean;
var i: integer;
    ds: integer;
    exponent: integer;
begin
  Result := false;
  ds := 0;
  exponent := 0;
  S := Trim(S);
  Value := 0;
  scale := 0;
  if Length(S) = 0 then
    Exit;
  {$IF declared(DefaultFormatSettings)}
  with DefaultFormatSettings do
  {$ELSE}
  {$IF declared(FormatSettings)}
  with FormatSettings do
  {$IFEND}
  {$IFEND}
  begin
    for i := length(S) downto 1 do
    begin
      if S[i] = AnsiChar(DecimalSeparator) then
      begin
          if ds <> 0 then Exit; {only one allowed}
          ds := i;
          dec(exponent);
          system.Delete(S,i,1);
      end
      else
      if S[i] in ['+','-'] then
      begin
       if (i > 1) and not (S[i-1] in ['e','E']) then
          Exit; {malformed}
      end
      else
      if S[i] in ['e','E'] then {scientific notation}
      begin
        if ds <> 0 then Exit; {not permitted in exponent}
        if exponent <> 0 then Exit; {only one allowed}
        exponent := i;
      end
      else
      if not (S[i] in ['0'..'9']) then
      {Note: ThousandSeparator not allowed by Delphi specs}
          Exit; {bad character}
    end;

    if exponent > 0 then
    begin
      Result := TryStrToInt(system.copy(S,exponent+1,maxint),Scale);
      if Result then
      begin
        {adjust scale for decimal point}
        if ds <> 0 then
          Scale := Scale - (exponent - ds);
        Result := TryStrToInt64(system.copy(S,1,exponent-1),Value);
      end;
    end
    else
    begin
      if ds > 0 then
      begin
        {remove trailing zeroes}
        while (ds < length(S)) and (S[length(S)] = '0') do {no need to check length as ds > 0}
          system.delete(S,Length(S),1);
        scale := ds - Length(S) - 1;
      end;
      Result := TryStrToInt64(S,Value);
    end;
  end;
end;

function NumericToDouble(aValue: Int64; aScale: integer): double;
begin
  Result := aValue * IntPower(10,aScale);
end;

function SafeSmallInt(aValue: Int64): Smallint;
begin
  if aValue > High(smallint) then
    IBError(ibxeIntegerOverflow,[]);
  if aValue < Low(smallint) then
    IBError(ibxIntegerUnderflow,[]);
  Result := aValue;
end;

function SafeInteger(aValue: Int64): integer;
begin
  if aValue > High(integer) then
    IBError(ibxeIntegerOverflow,[]);
  if aValue < Low(integer) then
    IBError(ibxIntegerUnderflow,[]);
  Result := aValue;
end;

{AdjustScale returns a raw int64 value derived from x but with aNewScale}

function AdjustScale(x: IFBNumeric; aNewScale: integer): int64;
var rValue: double;
begin
  rValue := x.getrawValue;
  Result := Round(rValue * IntPower(10,x.getScale-aNewScale));
end;

function CompareInt(a,b: integer): integer;
begin
  if a < b then
    Result := -1
  else
  if a = b then
    Result := 0
  else
    Result := 1;
end;

function NumericAdd(x, y: IFBNumeric): IFBNumeric;
begin
  case CompareInt(x.getScale,y.getScale) of
  0:
    Result := NumericFromRawValues(x.getRawValue + y.getRawValue,x.getScale);
  1:
    Result := NumericFromRawValues(AdjustScale(x,y.getscale) + y.getRawValue,y.getScale);
  else
    Result := NumericFromRawValues(AdjustScale(y,x.getscale)  + x.getRawValue,x.getScale);
  end;
end;

function NumericSubtract(x, y: IFBNumeric): IFBNumeric;
begin
  case CompareInt(x.getScale,y.getScale) of
  0:
    Result := NumericFromRawValues(x.getRawValue - y.getRawValue,x.getScale);
  1:
    Result := NumericFromRawValues(AdjustScale(x,y.getscale) - y.getRawValue,y.getScale);
  else
    Result := NumericFromRawValues(AdjustScale(y,x.getscale) - x.getRawValue,x.getScale);
  end;
end;

function NumericMultiply(x, y: IFBNumeric): IFBNumeric;
begin
  Result := NumericFromRawValues(x.getRawValue * y.getRawValue,x.getScale+y.getScale);
end;

function NumericDivide(x, y: IFBNumeric): IFBNumeric;
var z: double;
begin
  {Compute actual value as a double}
  z := (x.getRawValue / y.getRawValue) * IntPower(10, x.getScale - y.getScale);
  {Return numeric at original no. of decimal places of numerator}
  Result := DoubleTONumeric(z).AdjustScaleTo(x.getScale);
end;

function NumericCompare(x, y: IFBNumeric): integer;
begin
  case CompareInt(x.getScale,y.getScale) of
  0:
    Result := CompareInt(x.getRawValue,y.getRawValue);
  1:
    Result := CompareInt(AdjustScale(x,y.getscale),y.getRawValue);
  else
    Result := CompareInt(x.getRawValue,AdjustScale(y,x.getscale));
  end;
end;

function NegateNumeric(x: IFBNumeric): IFBNumeric;
begin
  Result := NumericFromRawValues(-x.getRawValue,x.getScale);
end;

function NumericAdd(x: IFBNumeric; y: int64): IFBNumeric;
begin
  Result := NumericAdd(x,IntToNumeric(y));
end;

function NumericSubtract(x: IFBNumeric; y: int64): IFBNumeric;
begin
  Result := NumericSubtract(x,IntToNumeric(y));
end;

function NumericSubtract(x: int64; y: IFBNumeric): IFBNumeric;
begin
  Result := NumericSubtract(IntToNumeric(x),y);
end;

function NumericMultiply(x: IFBNumeric; y: int64): IFBNumeric;
begin
  Result := NumericMultiply(x,IntToNumeric(y));
end;

function NumericDivide(x: IFBNumeric; y: int64): IFBNumeric;
begin
  Result := NumericDivide(x,IntToNumeric(y));
end;

function NumericDivide(x: int64; y: IFBNumeric): IFBNumeric;
begin
  Result := NumericDivide(IntToNumeric(x),y);
end;

function NumericCompare(x: IFBNumeric; y: int64): integer;
begin
  Result := NumericCompare(x,IntToNumeric(y));
end;

function NumericAdd(x: IFBNumeric; y: double): IFBNumeric;
begin
  Result := NumericAdd(x,DoubleToNumeric(y));
end;

function NumericSubtract(x: IFBNumeric; y: double): IFBNumeric;
begin
  Result := NumericSubtract(x,DoubleToNumeric(y));
end;

function NumericSubtract(x: double; y: IFBNumeric): IFBNumeric;
begin
  Result := NumericSubtract(DoubleToNumeric(x),y);
end;

function NumericMultiply(x: IFBNumeric; y: double): IFBNumeric;
begin
  Result := NumericMultiply(x,DoubleToNumeric(y));
end;

function NumericDivide(x: IFBNumeric; y: double): IFBNumeric;
begin
  Result := NumericDivide(x,DoubleToNumeric(y));
end;

function NumericDivide(x: double; y: IFBNumeric): IFBNumeric;
begin
  Result := NumericDivide(DoubleToNumeric(x),y);
end;

function NumericCompare(x: IFBNumeric; y: double): integer;
begin
  Result := NumericCompare(x,DoubleToNumeric(y));
end;

constructor TFBNumeric.Create(aValue: Int64; aScale: integer);
begin
  inherited Create;
  FValue := aValue;
  FScale := aScale;
end;

constructor TFBNumeric.CreateFromInt(aValue: Int64);
begin
  inherited Create;
  FValue := aValue;
  FScale := 0;
end;

constructor TFBNumeric.CreateFromStr(aValue: AnsiString);
begin
  inherited Create;
  if not TryStrToNumeric(aValue,FValue,FScale) then
    IBError(ibxeInvalidDataConversion,[aValue]);
end;

constructor TFBNumeric.CreateFromDouble(aValue: double);

  function WithinLimits(a: double): boolean;
  begin
    a := abs(frac(a));
    Result := (a > 0.001) and (a < 0.999); {avoid small rounding errors converting to decimal}
  end;

var aScale: integer;
    i: int64;
begin
  aScale := 0;
  while WithinLimits(AValue) do
  begin
    aValue := aValue * 10;
    Inc(aScale);
  end;
  i := Round(aValue);
  Create(i,-aScale);
end;

constructor TFBNumeric.CreateFromCurr(aValue: Currency);
begin
  inherited Create;
  Move(aValue,FValue,sizeof(Int64));
  FScale := -4;
end;

constructor TFBNumeric.CreateFromBCD(aValue: TBCD);
var ScaledBCD: TBCD;
begin
  inherited Create;
  FScale := -BCDScale(aValue);
  BCDMultiply(aValue,Power(10,-FScale),ScaledBCD);
  FValue := BCDToInteger(ScaledBCD,true);
end;

function TFBNumeric.getRawValue: Int64;
begin
  Result := FValue;
end;

function TFBNumeric.getScale: integer;
begin
 Result := FScale;
end;

function TFBNumeric.AdjustScaleTo(aNewScale: integer): IFBNumeric;
begin
 if FScale = aNewScale then
   Result := TFBNumeric.Create(FValue,FScale)
 else
  Result := TFBNumeric.Create(AdjustScale(self,aNewScale),aNewScale);
end;

function TFBNumeric.getAsString: AnsiString;
var Scaling : AnsiString;
    i: Integer;
begin
  Result := IntToStr(FValue);
  Scaling := '';
  if FScale > 0 then
  begin
    for i := 1 to FScale do
      Result := Result + '0';
  end
  else
  if FScale < 0 then
  {$IF declared(DefaultFormatSettings)}
  with DefaultFormatSettings do
  {$ELSE}
  {$IF declared(FormatSettings)}
  with FormatSettings do
  {$IFEND}
  {$IFEND}
  begin
    if Length(Result) > -FScale then
      system.Insert(DecimalSeparator,Result,Length(Result) + FScale+1)
    else
    begin
      Scaling := '0' + DecimalSeparator;
      for i := -1 downto FScale + Length(Result) do
        Scaling := Scaling + '0';
      if FValue < 0 then
        system.insert(Scaling,Result,2)
      else
        Result := Scaling + Result;
    end;
  end;
end;

function TFBNumeric.getAsDouble: double;
begin
  Result := NumericToDouble(FValue,FScale);
end;

function TFBNumeric.getAsCurrency: Currency;
var value: int64;
begin
  if FScale <> -4 then
  begin
    value := AdjustScaleTo(-4).GetRawValue;
    Move(value,Result,sizeof(Currency));
  end
  else
    Move(FValue,Result,sizeof(Currency));
end;

function TFBNumeric.getAsBCD: TBCD;
begin
  Result := DoubleToBCD(getAsDouble);
end;

function TFBNumeric.getAsInt64: Int64;
var
  Scaling : Int64;
  i: Integer;
  Val: Int64;
begin
  Scaling := 1;
  Val := FValue;
  if FScale > 0 then begin
    for i := 1 to FScale do Scaling := Scaling * 10;
    result := Val * Scaling;
  end else if FScale < 0 then begin
    for i := -1 downto FScale do Scaling := Scaling * 10;
    result := Val div Scaling;
  end else
    result := Val;
end;

function TFBNumeric.getAsInteger: integer;
begin
  Result := SafeInteger(getAsInt64);
end;

function TFBNumeric.getAsSmallInt: SmallInt;
begin
  Result := SafeSmallInt(getAsInt64);
end;

end.

