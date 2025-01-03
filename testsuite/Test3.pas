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

unit Test3;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{ Test 3: ad hoc queries}

{ This test opens the employee example databases with the supplied user name/password
  and runs several queries:

  1. The convenience function OpenCursorAtStart is used to return a count of the
     records in the employee database. This creates its own read only transaction.

  2. A parameterised query is used to delete a record. The record count is repeated
     using the same transaction as the deletion. The transaction is rolled back.

  3. Rollback is demonstrated by returning a record count. This time creating the
     transaction in place.

  4. The above two steps are repeated but with a named parameter ad an implicit end to the transaction.

  5. Note implicit disconnect on end
}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type
  { TTest3 }

  TTest3 = class(TFBTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest3 }

procedure TTest3.DoQuery(Attachment: IAttachment);
var Transaction: ITransaction;
    ResultSet: IResultSet;
    Statement: IStatement;
    TPB: ITPB;
    us: UnicodeString;
begin
  writeln(OutFile,'Employee Count = ',Attachment.OpenCursorAtStart('Select count(*) from EMPLOYEE')[0].AsInteger);

  TPB := FirebirdAPI.AllocateTPB;
  TPB.Add(isc_tpb_write);
  TPB.Add(isc_tpb_nowait);
  TPB.Add(isc_tpb_concurrency);
  TPB.Add(isc_tpb_lock_read).AsString := 'EMPLOYEE';
  TPB.Add(isc_tpb_protected);
  Transaction := Attachment.StartTransaction(TPB,taRollback);
  writeln(OutFile,'Transaction ID = ',Transaction.GetTransactionID);
  if Transaction.GetIsReadOnly then
    writeln(OutFile,'Transaction is Read Only')
  else
    writeln(OutFile,'Transaction is Read/Write');
  writeTRInfo(Transaction.GetTrInformation([isc_info_tra_id,
                                                            isc_info_tra_oldest_interesting,
                                                            isc_info_tra_oldest_active,
                                                            isc_info_tra_oldest_snapshot,
                                                            fb_info_tra_snapshot_number,
                                                            isc_info_tra_lock_timeout,
                                                            isc_info_tra_access,
                                                            isc_info_tra_isolation,
                                                            fb_info_tra_dbpath]));
  Attachment.ExecuteSQL(Transaction, 'Execute Procedure DELETE_EMPLOYEE ?', [8]);

  ResultSet := Attachment.OpenCursorAtStart(
         Transaction,
         'Select count(*) from EMPLOYEE',3);

  writeln(OutFile,'Employee Count = ',ResultSet[0].AsInteger);

  ResultSet := Attachment.OpenCursorAtStart('Select count(*) from EMPLOYEE');
  writeln(OutFile,'Employee Count = ',ResultSet[0].AsInteger);

  {$IFNDEF FPC}
  Transaction.Rollback; {Delphi does not dispose of interfaces until the end of the function
                         so we need to explicitly rollback here. FPC will dispose of the
                         interface as soon as it is overwritten - hence this is not needed.}
  {$ENDIF}
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
  Statement := Attachment.PrepareWithNamedParameters(Transaction,'Execute Procedure DELETE_EMPLOYEE :EMP_NO',3);
  Statement.GetSQLParams.ByName('EMP_NO').AsInteger := 8;
  Statement.Execute;

  ResultSet := Attachment.OpenCursorAtStart(
         Transaction,
         'Select count(*) from EMPLOYEE',3);

  writeln(OutFile,'Employee Count = ',ResultSet[0].AsInteger);

  Transaction := nil; {implicit rollback}


  writeln(OutFile,'Employee Count = ',Attachment.OpenCursorAtStart(
         Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit),
         'Select count(*) As Counter from EMPLOYEE',3)[0].AsInteger);

  writeln(OutFile,'Constrained Employee Count = ',Attachment.OpenCursorAtStart(
         Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit),
         'Select count(*) As Counter from EMPLOYEE Where EMP_NO < ?',3,[8])[0].AsInteger);

  writeln(OutFile,'"Johnson" Employee Count = ',Attachment.OpenCursorAtStart(
         Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit),
         'Select count(*) As Counter from EMPLOYEE Where LAST_NAME = ?',3,['Johnson'])[0].AsInteger);

  us := UTF8Decode('Yanowski');   {Test a UnicodeString as a parameter}

  writeln(OutFile,'"Yanowski" Employee Count = ',Attachment.OpenCursorAtStart(
         Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit),
         'Select count(*) As Counter from EMPLOYEE Where LAST_NAME = ?',3,[us])[0].AsInteger);

end;

function TTest3.TestTitle: AnsiString;
begin
  Result := 'Test 3: ad hoc queries';
end;

procedure TTest3.RunTest(CharSet: AnsiString; SQLDialect: integer);
var Attachment: IAttachment;
    DPB: IDPB;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).AsString := Owner.GetUserName;
  DPB.Add(isc_dpb_password).AsString := Owner.GetPassword;
  DPB.Add(isc_dpb_lc_ctype).AsString := CharSet;

  writeln(OutFile,'Opening ',Owner.GetEmployeeDatabaseName);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  writeln(OutFile,'Database Open');
  DoQuery(Attachment);
end;

initialization
  RegisterTest(TTest3);

end.

