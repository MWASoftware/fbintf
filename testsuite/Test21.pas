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
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;



implementation

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
    Params[4].AsString := '0';
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

end;

procedure TTest21.QueryDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);
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
