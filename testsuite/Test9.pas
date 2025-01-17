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

unit Test9;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{Test 9: Database Information tests}

{
  This test opens the employee example databases with the supplied user name/password
  and then reads and prints all defined database information.
}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB;

type

  { TTest9 }

  TTest9 = class(TFBTestBase)
  private
    procedure GetDBInformation(Attachment: IAttachment);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;

implementation

{ TTest9 }

procedure TTest9.GetDBInformation(Attachment: IAttachment);
var DBInfo: IDBInformation;
    DBRequest: IDIRB;
begin
  {First check ODS version to avoid information requests supported DB 2.5 and later}
  DBInfo := Attachment.GetDBInformation([isc_info_ods_version,isc_info_ods_minor_version]);
  if (DBInfo.Count > 1) and (DBInfo[0].AsInteger > 11) or
     ((DBInfo[0].AsInteger = 11) and (DBInfo[1].AsInteger > 1)) then
  begin
    WriteDBInfo(Attachment.GetDBInformation(isc_info_db_id));
    DBInfo := Attachment.GetDBInformation([isc_info_allocation,isc_info_base_level,
                              isc_info_implementation,isc_info_no_reserve,isc_info_ods_minor_version,
                              isc_info_ods_version,isc_info_page_size,isc_info_version,isc_info_db_read_only,
                              isc_info_creation_date,fb_info_pages_used,fb_info_pages_free]);
    WriteDBInfo(DBInfo);

    DBInfo := Attachment.GetDBInformation([isc_info_current_memory, isc_info_forced_writes,
                              isc_info_max_memory, isc_info_num_buffers, isc_info_sweep_interval,
                              isc_info_user_names,isc_info_active_tran_count]);
    WriteDBInfo(DBInfo);

    DBInfo := Attachment.GetDBInformation([isc_info_fetches,isc_info_marks,
                              isc_info_reads, isc_info_writes]);
    WriteDBInfo(DBInfo);

    DBInfo := Attachment.GetDBInformation([isc_info_backout_count, isc_info_delete_count,
                              isc_info_expunge_count,isc_info_insert_count, isc_info_purge_count,
                              isc_info_read_idx_count, isc_info_read_seq_count, isc_info_update_count]);
    WriteDBInfo(DBInfo);

    DBRequest := Attachment.AllocateDIRB;

    DBRequest.Add(isc_info_page_size);

    {Only enable during unit test. This result will always be different for
     each run and you will only get false positives in the log}
//    DBRequest.Add(fb_info_page_contents).AsInteger := 100;
    WriteDBInfo(Attachment.GetDBInformation(DBRequest));
  end
  else
  begin
    WriteDBInfo(Attachment.GetDBInformation(isc_info_db_id));
    DBInfo := Attachment.GetDBInformation([isc_info_allocation,isc_info_base_level,
                              isc_info_implementation,isc_info_no_reserve,isc_info_ods_minor_version,
                              isc_info_ods_version,isc_info_page_size,isc_info_version,isc_info_db_read_only]);
    WriteDBInfo(DBInfo);

    DBInfo := Attachment.GetDBInformation([isc_info_current_memory, isc_info_forced_writes,
                              isc_info_max_memory, isc_info_num_buffers, isc_info_sweep_interval,
                              isc_info_user_names]);
    WriteDBInfo(DBInfo);

    DBInfo := Attachment.GetDBInformation([isc_info_fetches,isc_info_marks,
                              isc_info_reads, isc_info_writes]);
    WriteDBInfo(DBInfo);

    DBInfo := Attachment.GetDBInformation([isc_info_backout_count, isc_info_delete_count,
                              isc_info_expunge_count,isc_info_insert_count, isc_info_purge_count,
                              isc_info_read_idx_count, isc_info_read_seq_count, isc_info_update_count]);
    WriteDBInfo(DBInfo);

    DBRequest := Attachment.AllocateDIRB;
    DBRequest.Add(isc_info_page_size);
    WriteDBInfo(Attachment.GetDBInformation(DBRequest));
  end;
end;

function TTest9.TestTitle: AnsiString;
begin
  Result := 'Test 9: Database Information tests';
end;

procedure TTest9.RunTest(CharSet: AnsiString; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);

  GetDBInformation(Attachment);
end;

initialization
  RegisterTest(TTest9);

end.

