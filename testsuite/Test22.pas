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

unit Test22;
{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{Test 22: Journalling}


interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB, IBUtils;

type

  { TTest22 }

  TTest22 = class(TFBTestBase)
  private
    procedure PrintJournalTable(Attachment: IAttachment);
    procedure UpdateDatabase(Attachment: IAttachment);
    procedure QueryDatabase(Attachment: IAttachment);
    procedure ValidateStrToNumeric;
    procedure HandleOnJnlEntry(JnlEntryType: TJnlEntryType;
      JnlEntryTypeName: AnsiString; SessionID, TransactionID, PhaseNo: integer;
  Description, TPBText: AnsiString);
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

  { TTest22 }

procedure TTest22.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit,'Transaction_29');
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
end;

procedure TTest22.QueryDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);
end;

procedure TTest22.ValidateStrToNumeric;
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

procedure TTest22.HandleOnJnlEntry(JnlEntryType: TJnlEntryType;
  JnlEntryTypeName: AnsiString; SessionID, TransactionID, PhaseNo: integer;
  Description, TPBText: AnsiString);

begin
  writeln(OutFile,'Journal Entry = ',JnlEntryType,'(',JnlEntryTypeName,')');;
  writeln(OutFile,'Session ID = ',SessionID);
  writeln(OutFile,'Transaction ID = ',TransactionID);
  writeln(OutFile,'Phase No = ',PhaseNo);
  writeln(OutFile,'Description = "',Description,'"');
  writeln(OutFile,'TPBText = ',TPBText);
  writeln(OutFile);
end;

procedure TTest22.PrintJournalTable(Attachment: IAttachment);
var Results: IResultSet;
begin
  writeln(OutFile,'Journal Table');
  Results := Attachment.OpenCursorAtStart('Select * From IBX$JOURNALS');
  while not Results.IsEof do
  begin
    ReportResult(Results);
    Results.Fetchnext;
  end;
end;

function TTest22.TestTitle: AnsiString;
begin
  Result := 'Test 22: Journalling';
end;

procedure TTest22.RunTest(CharSet: AnsiString; SQLDialect: integer);
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
    Attachment.StartJournaling('Test'+GetTestID+'.log',true);
    ValidateStrToNumeric;
    SetFloatTemplate('#,###.00000000');
    UpdateDatabase(Attachment);
    QueryDatabase(Attachment);
    PrintJournalTable(Attachment);
  finally
    Attachment.DropDatabase;
  end;
  Attachment.StopJournaling;
  writeln(OutFile);
  writeln(OutFile,'Journal Entries');
  with TJournalProcessor.Create do
  try
     Execute('Test'+GetTestID+'.log',HandleOnJnlEntry);
  finally
    Free
  end;
end;

initialization
  RegisterTest(TTest22);

end.

