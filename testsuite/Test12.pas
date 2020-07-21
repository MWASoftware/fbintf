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

unit Test12;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}

{$codepage UTF8}
{$ENDIF}

interface

{ Test 12: Character Sets

  This test creates strings in a database with various code pages and then
  reads them back with different connection character sets. The result is
  displayed as hex strings so that the actual encoding can be checked in
  each case.
}

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type

  { TTest12 }

  TTest12 = class(TFBTestBase)
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
    'Notes VarChar(64) Character Set ISO8859_1 collate FR_FR,'+
    'BlobData Blob sub_type 1 Character Set WIN1252, '+
    'BlobData2 Blob sub_type 1 Character Set UTF8, '+
    'InClear VarChar(16) Character Set OCTETS, '+
    'FixedWidth Char(4) Character set UTF8, '+
    'Primary Key(RowID)'+
    ')';

  sqlGetCharSets = 'Select RDB$CHARACTER_SET_NAME,RDB$CHARACTER_SET_ID from RDB$CHARACTER_SETS order by 2';

  sqlInsert = 'Insert into TestData(RowID,Title,Notes, BlobData,BlobData2,InClear,FixedWidth) '+
              'Values(:RowID,:Title,:Notes,:BlobData,:BlobData2,:InClear,:FixedWidth)';


{ TTest12 }

procedure TTest12.UpdateDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);

  Statement := Attachment.PrepareWithNamedParameters(Transaction,sqlInsert);
  ParamInfo(Statement.SQLParams);
  with Statement.GetSQLParams do
  begin
    ByName('rowid').AsInteger := 1;
    {$IFDEF DCC}
    ByName('title').AsString := UTF8Encode('Blob Test ©€');
    ByName('Notes').AsString := UTF8Encode('Écoute moi');
    {$ELSE}
    ByName('title').AsString := 'Blob Test ©€';
    ByName('Notes').AsString := 'Écoute moi';
    {$ENDIF}
    ByName('BlobData').AsString := 'Some German Special Characters like ÖÄÜöäüß';
    ByName('BlobData2').AsBlob := Attachment.CreateBlob(Transaction,'TestData','BlobData').SetString('Some German Special Characters like ÖÄÜöäüß');
    ByName('InClear').AsString := #$01'Test'#$0D#$C3;
    ByName('FixedWidth').AsString := 'É';
  end;
  Statement.Execute;
end;

procedure TTest12.QueryDatabase(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
begin
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'Select * from TestData');
  ReportResults(Statement);
end;

function TTest12.TestTitle: AnsiString;
begin
  Result := 'Test 12: Character Sets';
end;

procedure TTest12.RunTest(CharSet: AnsiString; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
begin
  FHexStrings := true;
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);

  UpdateDatabase(Attachment);

  writeln(OutFile,'Connection Character Set UTF8');
  {Query with UTF8}
  QueryDatabase(Attachment);
  Attachment.Disconnect;

  writeln(OutFile,'Connection Character Set NONE');
  {Query with No character set}
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetNewDatabaseName,DPB);
  QueryDatabase(Attachment);
  Attachment.Disconnect;

  writeln(OutFile,'Connection Character Set WIN1252');
  {Query with WIN1252}
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('WIN1252');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetNewDatabaseName,DPB);
  QueryDatabase(Attachment);

  Attachment.DropDatabase;

end;

initialization
  RegisterTest(TTest12);
end.

