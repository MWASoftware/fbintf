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

unit Test15;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

{Test 15: Blob Handling and BPBs}

{
  1. A database is created with two tables. One has an untyped Blob. the other
     is UTF8 text.

  2. An image is inserted into the first.

  3. Win1252 text into the second with a Blob Filter request to transform to UTF8.

  4. The Data is read back and written out.
}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type
      { TTest15 }

  TTest15 = class(TFBTestBase)
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
    'Title VarChar(32) Character Set UTF8,'+
    'BlobData Blob sub_type 0, '+
    'Primary Key(RowID)'+
    ')';

    sqlCreateTable2 =
    'Create Table TestData2 ('+
    'RowID Integer not null,'+
    'Title VarChar(32) Character Set UTF8,'+
    'BlobData Blob sub_type 1 Character Set UTF8, '+
    'Primary Key(RowID)'+
    ')';

    sqlInsert = 'Insert into TestData(RowID,Title, BlobData) Values(:RowID,:Title,:BlobData)';
    sqlInsert2 = 'Insert into TestData2(RowID,Title, BlobData) Values(:RowID,:Title,:BlobData)';

{ TTest15 }

procedure TTest15.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    aBlob: IBlob;
    BPB: IBPB;
    aText: RawByteString;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlInsert);
  with Statement.GetSQLParams do
  begin
    ByName('rowid').AsInteger := 1;
    ByName('title').AsString := 'Blob Test';
    ByName('BlobData').AsBlob := Attachment.CreateBlob(Transaction,'TestData','BlobData').LoadFromFile('testimage.jpg');
  end;
  Statement.Execute;

  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlInsert);
  with Statement.GetSQLParams do
  begin
    ByName('rowid').AsInteger := 2;
    ByName('title').AsString := 'Blob Test with binary string';
    aText := #$0#$09#$0a {random digits} +
             #$C9#$63#$6F#$75#$74#$65#$20#$6D#$6F#$69;  {Écoute moi' encoded in Win1252}
    ByName('BlobData').AsString := aText;
  end;
  Statement.Execute;

  BPB := Attachment.AllocateBPB;
  BPB.Add(isc_bpb_target_type).AsInteger := 1;
  BPB.Add(isc_bpb_target_interp).AsInteger := 4; {utf8}
  BPB.Add(isc_bpb_source_type).AsInteger := 1;
  BPB.Add(isc_bpb_source_interp).AsInteger := 53; {WIN1252}
  aText := #$C9#$63#$6F#$75#$74#$65#$20#$6D#$6F#$69;  {Écoute moi' encoded in Win1252}
  aBlob := Attachment.CreateBlob(Transaction,'TestData2','BlobData',BPB).SetString(aText);
  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlInsert2);
  with Statement.GetSQLParams do
  begin
    ByName('rowid').AsInteger := 1;
    ByName('title').AsString := 'Blob Test';
    ByName('BlobData').AsBlob := aBlob;
  end;
  Statement.Execute;
end;

procedure TTest15.QueryDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from TestData Where RowID = 1');
  writeln(OutFile);
  writeln(OutFile,'Testdata');
  writeln(OutFile);
  ReportResults(Statement);
  FShowBinaryBlob := true;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData Where RowID = 2');
  ReportResults(Statement);
  FShowBinaryBlob := false;

  Statement := Attachment.Prepare(Transaction,'Select * from TestData2 ');
  writeln(OutFile);
  writeln(OutFile,'Testdata 2');
  writeln(OutFile);
  ReportResults(Statement);
end;

function TTest15.TestTitle: AnsiString;
begin
  Result := 'Test 15: Blob Handling and BPBs';
end;

procedure TTest15.RunTest(CharSet: AnsiString; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable2);

  UpdateDatabase(Attachment);
  QueryDatabase(Attachment);
  Attachment.DropDatabase;
end;

initialization
  RegisterTest(TTest15);
end.

