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
  Classes, SysUtils, TestManager, IB;

type

  { TTest9 }

  TTest9 = class(TTestBase)
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
    WriteDBInfo(Attachment.GetDBInformation(isc_info_db_id));
    DBInfo := Attachment.GetDBInformation([isc_info_allocation,isc_info_base_level,
                              isc_info_implementation,isc_info_no_reserve,isc_info_ods_minor_version,
                              isc_info_ods_version,isc_info_page_size,isc_info_version,isc_info_db_read_only,
                              isc_info_creation_date]);
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
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);

  GetDBInformation(Attachment);
end;

initialization
  RegisterTest(TTest9);

end.

