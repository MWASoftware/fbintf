(*
 *  Firebird Interface (fbintf) Test suite. This program is used to
 *  test the Firebird Pascal Interface and provide a semi-automated
 *  pass/fail check for each test.
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

unit Test21;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{Test 21: Exercise setting and getting of numeric data types}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type

  { TTest21 }

  TTest21 = class(TFBTestBase)
  private
    procedure UpdateDatabase(Attachment: IAttachment);
    procedure QueryDatabase(Attachment: IAttachment);
    procedure ValidateStrToNumeric;
    procedure ValidateNumericInterface;
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;



implementation

uses FBNumeric, FmtBCD;

const
  sqlCreateTable =
    'Create Table TestData ('+
    'RowID Integer not null,'+
    'iType Integer,'+
    'i64Type BIGINT,'+
    'CurrType Numeric(12,4),'+
    'dType DOUBLE PRECISION,'+
    'FixedPoint Numeric(10,6),'+
    'Primary Key (RowID)'+
    ')';

  sqlInsert = 'Insert into TestData(RowID,iType,i64Type,CurrType,dType,FixedPoint) Values(?,?,?,?,?,?)';


{ TTest21 }

procedure TTest21.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,sqlInsert);
  ParamInfo(Statement.GetSQLParams);
  with Statement.GetSQLParams do
  begin
    Params[0].AsInteger := 1;
    Params[1].AsString := '101';
    Params[2].AsString := ' 9223372036854775807';
    Params[3].AsString := '10000.1234';
    Params[4].AsString := '9999.123456780';
    Params[5].AsString := '1234567890.12345678';
  end;
  Statement.Execute;
  with Statement.GetSQLParams do
  begin
    Params[0].AsInteger := 2;
    Params[1].AsString := '-32457';
    Params[2].AsString := ' -9223372036854775808 ';
    Params[3].AsString := '+1000001.12';
    Params[4].AsString := '1.7E308';
    Params[5].AsString := '-1234567890.12345678';
  end;
  Statement.Execute;
  with Statement.GetSQLParams do
  begin
    Params[0].AsInteger := 3;
    Params[1].AsString := '0';
    Params[2].AsString := '0';
    Params[3].AsString := '0';
    Params[4].AsString := '0';
    Params[5].AsString := '0';
  end;
  Statement.Execute;
  with Statement.GetSQLParams do
  begin
    Params[0].AsInteger := 4;
    Params[1].AsString := '1.0';
    Params[2].AsString := '10.';
    Params[3].AsString := '2.3E-2';
    Params[4].AsString := '11e-4';
    Params[5].AsString := '2.33456E2';
  end;
  Statement.Execute;
  {error handling}
  with Statement.GetSQLParams do
  try
     Params[0].AsInteger := 5;
     Params[1].AsString := '1,000';
     Statement.Execute;
  except on E: Exception do
    writeln(Outfile,'Expected Error - ',E.Message);
  end;
  with Statement.GetSQLParams do
  try
     Params[0].AsInteger := 6;
     Params[5].AsString := '10.0.0';
     Statement.Execute;
  except on E: Exception do
    writeln(Outfile,'Expected Error - ',E.Message);
  end;
  writeln(OutFile,'Test Numeric Type');
  with Statement.GetSQLParams do
  begin
    Clear;
    Params[0].AsInteger := 7;
    Params[1].AsNumeric := DoubleToNumeric(1.0);
    Params[2].AsVariant := 1234567;
    Params[3].AsNumeric := DoubleToNumeric(StrToFloat('2.3E-2'));
    Params[4].AsNumeric := StrToNumeric('11e-4');
    Params[5].AsVariant := 1234.25;
  end;
  Statement.Execute;
end;

procedure TTest21.QueryDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);
end;

procedure TTest21.ValidateStrToNumeric;
const
  TestValues: array of string = ['1234.567','-765.4321','0.1','0.01','+123',
                                 '1.23456E308','-1.2e-02','10.','.12', '0.12',
                                 '1.2E1.2', '1,000', '1e1e1', '1.2+3']; {bad syntax}
var
  i: integer;
  aValue: Int64;
  aScale: integer;
begin
  for i := 0 to Length(TestValues) - 1 do
  begin
    if TryStrToNumeric(TestValues[i],aValue,aScale) then
    begin
      writeln(Outfile,TestValues[i],' parsed to ',aValue,' scale = ',aScale);
      writeln(Outfile,'As Float = ',NumericToDouble(aValue,aScale));
    end
    else
      writeln(Outfile,'Parsing of ',TestValues[i],' failed');
  end;
end;

procedure TTest21.ValidateNumericInterface;
var numeric: IFBNumeric;
begin
  writeln(Outfile,'Validating Numeric Interface - IFBNumeric');
  numeric := CurrToNumeric(StrToCurr('9999.123456780'));
  writeln(Outfile,'Value from Currency = ',numeric.getAsString);
  writeln(Outfile,'Raw Value = ',numeric.getRawValue,' Scale = ',numeric.getScale);
  numeric := CurrToNumeric(StrToCurr('9999.123456780')).AdjustScaleTo(-2);
  writeln(Outfile,'Value from Currency(rescaled) = ',numeric.getAsString);
  writeln(Outfile,'Raw Value = ',numeric.getRawValue,' Scale = ',numeric.getScale);
  numeric := DoubleToNumeric(StrToFloat('9999.123456780'));
  writeln(Outfile,'Value from Double = ',numeric.getAsString);
  writeln(Outfile,'Raw Value = ',numeric.getRawValue,' Scale = ',numeric.getScale);
  numeric := IntToNumeric(StrToInt64('9223372036854775807'));
  writeln(Outfile,'Value from Integer = ',numeric.getAsString);
  writeln(Outfile,'Raw Value = ',numeric.getRawValue,' Scale = ',numeric.getScale);
  numeric := StrToNumeric('9223372036854775807');
  writeln(Outfile,'Value from string = ',numeric.getAsString);
  writeln(Outfile,'Raw Value = ',numeric.getRawValue,' Scale = ',numeric.getScale);
  numeric := StrToNumeric('9999.123456780');
  writeln(Outfile,'Value from string = ',numeric.getAsString);
  writeln(Outfile,'Raw Value = ',numeric.getRawValue,' Scale = ',numeric.getScale);
  numeric := StrToNumeric('-1.2e-02');
  writeln(Outfile,'Value from string = ',numeric.getAsString);
  writeln(Outfile,'Raw Value = ',numeric.getRawValue,' Scale = ',numeric.getScale);
  try
    numeric := BCDToNumeric(StrToBCD('9999.123456780'));
    writeln(Outfile,'Value from BCD = ',numeric.getAsString);
    writeln(Outfile,'Raw Value = ',numeric.getRawValue,' Scale = ',numeric.getScale);
  except on E:Exception do
    writeln(OutFile,'Delphi has a problem with this number: ',E.Message);
  end;
  numeric := NumericFromRawValues(9999123456780,-6);
  writeln(Outfile,'Value from Raw Data = ',numeric.getAsString);
  writeln(Outfile,'Raw Value = ',numeric.getRawValue,' Scale = ',numeric.getScale);

  writeln(outfile,'Numeric Operations');
  writeln(Outfile,'Add 2.23 + 24.12345 = ',NumericAdd(StrToNumeric('2.23'),StrToNumeric('24.12345')).GetAsString);
  writeln(Outfile,'Add Double 2.23 + 24.12645 = ',NumericAdd(StrToNumeric('2.23'),24.12645).GetAsString);
  writeln(Outfile,'Add integer 2.23 + 2412345 = ',NumericAdd(StrToNumeric('2.23'),2412345).GetAsString);
  writeln(Outfile,'Subtract 2.23 - 24.12345 = ',NumericSubtract(StrToNumeric('2.23'),StrToNumeric('24.12345')).GetAsString);
  writeln(Outfile,'Subtract Double 24.12645 - 2.23 = ',NumericSubtract(StrToNumeric('24.12645'),2.23).GetAsString);
  writeln(Outfile,'Subtract integer 24123.45 - 223 = ',NumericSubtract(StrToNumeric('24123.45'),223).GetAsString);
  writeln(Outfile,'Multiply 2.23 * 24.12345 = ',NumericMultiply(StrToNumeric('2.23'),StrToNumeric('24.12345')).GetAsString);
  writeln(Outfile,'Multiply Double 24.12645 * 2.23 = ',NumericMultiply(StrToNumeric('24.12645'),2.23).GetAsString);
  writeln(Outfile,'Multiply integer 241.2345 * 223 = ',NumericMultiply(StrToNumeric('241.2345'),223).GetAsString);
  writeln(Outfile,'Divide 24.12345 / 2.23 = ',NumericDivide(StrToNumeric('24.12345'),StrToNumeric('2.23')).GetAsString);
  writeln(Outfile,'Divide Double 2.23 / 24.12645 = ',NumericDivide(StrToNumeric('2.23'),24.12645).GetAsString);
  writeln(Outfile,'Divide integer 241.2345 / 223 = ',NumericDivide(StrToNumeric('241.2345'),223).GetAsString);
  writeln(Outfile,'Compare 2.23, -24.12345 = ',NumericCompare(StrToNumeric('2.23'),DoubleToNumeric(-24.12645)));
  writeln(Outfile,'Compare integer 2.23, 3 = ',NumericCompare(StrToNumeric('2.23'),3));
  writeln(Outfile,'Compare Double 2.23, 2.23 = ',NumericCompare(StrToNumeric('2.23'),2.23));
  writeln(Outfile,'Negate 24.12345 = ',NegateNumeric(StrToNumeric('24.12345')).GetAsString);
end;

function TTest21.TestTitle: AnsiString;
begin
  Result := 'Test 21: Exercise setting and getting of numeric data types';
end;

procedure TTest21.RunTest(CharSet: AnsiString; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  try
    Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
    ValidateStrToNumeric;
    ValidateNumericInterface;
    SetFloatTemplate('#,###.00000000');
    UpdateDatabase(Attachment);
    QueryDatabase(Attachment);
  finally
    Attachment.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest21);
end.

