﻿(*
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

unit Test7;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{Test 7: Create and read back an Array}

{
  1. Create an empty database and populate with a single table including an array of integer column.

  2. Select all and show metadata including array metadata.

  3. Insert a row but leave array null

  4. Show result.

  5. Update row with a populated array and show results.

  7. Reopen cursor but before fetching array, shrink bounds

  8. Fetch and print array with reduced bounds.

}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type

  { TTest7 }

  TTest7 = class(TFBTestBase)
  private
    procedure UpdateDatabase(Attachment: IAttachment);
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
    'Dated TIMESTAMP, '+
    'Notes VarChar(64) Character Set ISO8859_1,'+
    'MyArray Integer [0:16],'+
    'MyArray2 Timestamp [0:16],'+
    'MyArray3 Numeric(10,2) [0:16],'+
    'Primary Key(RowID)'+
    ')';

  sqlInsert = 'Insert into TestData(RowID,Title,Dated,Notes) Values(:RowID,:Title,:Dated,:Notes)';

  sqlUpdate = 'Update TestData Set MyArray = :MyArray Where RowID = 1';
  sqlUpdate2 = 'Update TestData Set MyArray2 = :MyArray2 Where RowID = 1';
  sqlUpdate3 = 'Update TestData Set MyArray3 = :MyArray3 Where RowID = 1';

{ TTest7 }

procedure TTest7.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
    i,j: integer;
    ar: IArray;
    aDateTime: TDateTime;
    f: double;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  PrintMetaData(Statement.GetMetaData);
  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlInsert);
  ParamInfo(Statement.GetSQLParams);
  with Statement.GetSQLParams do
  begin
    for i := 0 to GetCount - 1 do
      writeln(OutFile,'Param Name = ',Params[i].getName);
    ByName('rowid').AsInteger := 1;
    {$IFDEF DCC}
    ByName('title').AsString := UTF8Encode('Blob Test ©€');
    {$ELSE}
    ByName('title').AsString := 'Blob Test ©€';
    {$ENDIF}
    ByName('Notes').AsString := 'Écoute moi';
    ByName('Dated').AsDateTime := EncodeDate(2016,4,1) + EncodeTime(9,30,0,100);
  end;
  Statement.Execute;
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlUpdate);
  ParamInfo(Statement.GetSQLParams);
  Transaction.CommitRetaining;
  ar := Attachment.CreateArray(Transaction,'TestData','MyArray');
  j := 100;
  for i := 0 to 16 do
  begin
    ar.SetAsInteger([i],j);
    dec(j);
  end;
  Statement.SQLParams[0].AsArray := ar;
  Statement.Execute;

  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlUpdate2);
  ParamInfo(Statement.GetSQLParams);
  ar := Attachment.CreateArray(Transaction,'TestData','MyArray2');
  for i := 0 to 16 do
  begin
    aDateTime := EncodeDate(2020,5,1) + EncodeTime(12,i,0,0);
    ar.SetAsDateTime(i,aDateTime);
  end;
  Statement.SQLParams[0].AsArray := ar;
  Statement.Execute;

  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlUpdate3);
  ParamInfo(Statement.GetSQLParams);
  ar := Attachment.CreateArray(Transaction,'TestData','MyArray3');
  f := 0;
  for i := 0 to 13 do
  begin
    ar.SetAsFloat(i,f);
    f := f + 1.05
  end;
  ar.SetAsString([14],'0.424567');
  ar.SetAsString([15],'42.4567');
  ar.SetAsString([16],'4269');
  Statement.SQLParams[0].AsArray := ar;
  Statement.Execute;

  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  PrintMetaData(Statement.GetMetaData);
  ReportResults(Statement);

  ResultSet := Statement.OpenCursor;
  if Resultset.FetchNext then
  begin
    ar := ResultSet.ByName('MyArray').AsArray;
    ar.SetBounds(0,10,2);
    writeln(OutFile,'Shrink to 2:10');
    WriteArray(ar);
  end
  else
    writeln(OutFile,'Unable to reopen cursor');

  {Now update the reduced slice}
  writeln(OutFile,'Write updated reduced slice');
  ar.SetAsInteger([2],1000);
  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlUpdate);
  Statement.SQLParams[0].AsArray := ar;
  Statement.Execute;
  writeln(OutFile,'Show update array');
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);

  Transaction.Commit;
end;

function TTest7.TestTitle: AnsiString;
begin
  Result := 'Test 7: Create and read back an Array';
end;

procedure TTest7.RunTest(CharSet: AnsiString; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
  UpdateDatabase(Attachment);

  Attachment.DropDatabase;
end;

initialization
  RegisterTest(TTest7);

end.

