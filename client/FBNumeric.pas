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

function NewNumeric(aValue: AnsiString): IFBNumeric; overload;
function NewNumeric(aValue: double; aScale: integer): IFBNumeric; overload;
function NewNumeric(aValue: TBCD): IFBNumeric; overload;
function NewNumeric(aValue: currency): IFBNumeric; overload;
function NewNumeric(aValue: Int64): IFBNumeric; overload;
function NumericFromRawValues(aValue: Int64; aScale: integer): IFBNumeric;

function TryStrToNumeric(S: Ansistring; out Value: int64; out scale: integer): boolean;
function NumericToDouble(aValue: Int64; aScale: integer): double;

function SafeSmallInt(aValue: Int64): Smallint;
function SafeInteger(aValue: Int64): integer;


implementation

uses IBUtils, FBMessages, Math;

type

  { TIBNumeric }

  { TFBNumeric }

  TFBNumeric = class(TFBInterfacedObject,IFBNumeric)
  private
    FValue: Int64;
    FScale: integer;
//    function AdjustScaleFromCurrency(Value: Currency; aScale: Integer): Int64;
  public
    constructor Create(aValue: Int64; aScale: integer); overload;
    constructor Create(aValue: Int64); overload;
    constructor Create(aValue: AnsiString); overload;
    constructor Create(aValue: double; aScale: integer); overload;
    constructor Create(aValue: Currency); overload;
    constructor Create(aValue: TBCD); overload;
  public
    {IFBNumeric}
    function getRawValue: Int64;
    function getScale: integer;
    function clone(aNewScale: integer): IFBNumeric;
    function getAsString: AnsiString;
    function getAsDouble: double;
    function getAsCurrency: Currency;
    function getAsBCD: TBCD;
    function getAsInt64: Int64; {scaled}
    function getAsInteger: integer; {scaled - may be truncated}
    function getAsSmallInt: SmallInt; {scaled - may be truncated}
  end;

function NewNumeric(aValue: AnsiString): IFBNumeric;
begin
  Result :=  TFBNumeric.Create(aValue);
end;

function NewNumeric(aValue: double; aScale: integer): IFBNumeric;
begin
  Result :=  TFBNumeric.Create(aValue,aScale);
end;

function NewNumeric(aValue: TBCD): IFBNumeric;
begin
  Result :=  TFBNumeric.Create(aValue);
end;

function NewNumeric(aValue: currency): IFBNumeric;
begin
  Result :=  TFBNumeric.Create(aValue);
end;

function NewNumeric(aValue: Int64): IFBNumeric;
begin
  Result :=  TFBNumeric.Create(aValue);
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

{ TFBNumeric }

(*function TFBNumeric.AdjustScaleFromCurrency(Value: Currency; aScale: Integer
  ): Int64;
var
  Scaling : Int64;
  i : Integer;
begin
  Result := 0;
  Scaling := 1;
  if aScale < 0 then
  begin
    for i := -1 downto aScale do
      Scaling := Scaling * 10;
    result := trunc(Value * Scaling);
  end
  else
  if aScale > 0 then
  begin
    for i := 1 to aScale do
       Scaling := Scaling * 10;
    result := trunc(Value / Scaling);
  end
  else
    result := trunc(Value);
end;*)

constructor TFBNumeric.Create(aValue: Int64; aScale: integer);
begin
  inherited Create;
  FValue := aValue;
  FScale := aScale;
end;

constructor TFBNumeric.Create(aValue: Int64);
begin
  inherited Create;
  FValue := aValue;
  FScale := 0;
end;

constructor TFBNumeric.Create(aValue: AnsiString);
begin
  inherited Create;
  if not TryStrToNumeric(aValue,FValue,FScale) then
    IBError(ibxeInvalidDataConversion,[aValue]);
end;

constructor TFBNumeric.Create(aValue: double; aScale: integer);
var
  Scaling : Int64;
  i : Integer;
begin
  inherited Create;
  FScale := aScale;
  FValue := 0;
  Scaling := 1;
  if aScale < 0 then
  begin
    for i := -1 downto aScale do
      Scaling := Scaling * 10;
    FValue := trunc(aValue * Scaling);
  end
  else
  if aScale > 0 then
  begin
    for i := 1 to aScale do
       Scaling := Scaling * 10;
    FValue := trunc(aValue / Scaling);
  end
  else
    FValue := trunc(aValue);
//  writeln('Adjusted ',Value,' to ',Result);
end;

constructor TFBNumeric.Create(aValue: Currency);
begin
  inherited Create;
  Move(aValue,FValue,sizeof(Int64));
  FScale := -4;
end;

constructor TFBNumeric.Create(aValue: TBCD);
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

function TFBNumeric.clone(aNewScale: integer): IFBNumeric;
begin
  Result := TFBNumeric.Create(Round(FValue * IntPower(10,FScale-aNewScale)),aNewScale);
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
    value := clone(-4).GetRawValue;
    Move(value,Result,sizeof(Currency));
  end
  else
    Move(FValue,Result,sizeof(Currency));
end;

(*var
  Scaling : Int64;
  i : Integer;
  FractionText, PadText, CurrText: AnsiString;
begin
  Result := 0;
  Scaling := 1;
  PadText := '';
  if FScale > 0 then
  begin
    for i := 1 to FScale do
      Scaling := Scaling * 10;
    result := FValue * Scaling;
  end
  else
    if FScale < 0 then
    begin
      for i := -1 downto FScale do
        Scaling := Scaling * 10;
      FractionText := IntToStr(abs(FValue mod Scaling));
      for i := Length(FractionText) to -FScale -1 do
        PadText := '0' + PadText;
      {$IF declared(DefaultFormatSettings)}
      with DefaultFormatSettings do
      {$ELSE}
      {$IF declared(FormatSettings)}
      with FormatSettings do
      {$IFEND}
      {$IFEND}
      if FValue < 0 then
        CurrText := '-' + IntToStr(Abs(FValue div Scaling)) + DecimalSeparator + PadText + FractionText
      else
        CurrText := IntToStr(Abs(FValue div Scaling)) + DecimalSeparator + PadText + FractionText;
      try
        result := StrToCurr(CurrText);
      except
        on E: Exception do
          IBError(ibxeInvalidDataConversion, [nil]);
      end;
    end
    else
      result := FValue;
end; *)

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

