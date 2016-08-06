unit Test9;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest9 }

  TTest9 = class(TTestBase)
  private
    procedure GetDBInformation(Attachment: IAttachment);
    procedure WriteDBInfo(DBInfo: IDBInformation);
    procedure WriteBytes(Bytes: TByteArray);
    procedure WriteOperationCounts(Category: string; ops: TDBOperationCounts);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;

implementation

{ TTest9 }

procedure TTest9.GetDBInformation(Attachment: IAttachment);
var DBInfo: IDBInformation;
begin
    DBInfo := Attachment.GetDBInformation([isc_info_db_id,isc_info_allocation,isc_info_base_level,
                              isc_info_implementation,isc_info_no_reserve,isc_info_ods_minor_version,
                              isc_info_ods_version,isc_info_page_size,isc_info_version]);
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
end;

procedure TTest9.WriteDBInfo(DBInfo: IDBInformation);
var i, j: integer;
    bytes: TByteArray;
    ConType: integer;
    DBFileName: string;
    DBSiteName: string;
    Version: byte;
    VersionString: string;
    Users: TStrings;
begin
  for i := 0 to DBInfo.GetCount - 1 do
  with DBInfo[i] do
  case getItemType of
  isc_info_allocation:
    writeln('Pages =',getAsInteger);
  isc_info_base_level:
    begin
      bytes := getAsBytes;
      write('Base Level = ');
      WriteBytes(Bytes);
    end;
   isc_info_db_id:
     begin
       DecodeIDCluster(ConType,DBFileName,DBSiteName);
       writeln('Database ID = ', ConType,' FB = ', DBFileName, ' SN = ',DBSiteName);
     end;
   isc_info_implementation:
     begin
       bytes := getAsBytes;
       write('Implementation = ');
       WriteBytes(Bytes);
     end;
   isc_info_no_reserve:
     writeln('Reserved = ',getAsInteger);
   isc_info_ods_minor_version:
     writeln('ODS minor = ',getAsInteger);
   isc_info_ods_version:
     writeln('ODS major = ',getAsInteger);
   isc_info_page_size:
     writeln('Page Size = ',getAsInteger);
   isc_info_version:
     begin
       DecodeVersionString(Version,VersionString);
       writeln('Version = ',Version,': ',VersionString);
     end;
   isc_info_current_memory:
     writeln('Server Memory = ',getAsInteger);
   isc_info_forced_writes:
     writeln('Forced Writes  = ',getAsInteger);
   isc_info_max_memory:
     writeln('Max Memory  = ',getAsInteger);
   isc_info_num_buffers:
     writeln('Num Buffers  = ',getAsInteger);
   isc_info_sweep_interval:
     writeln('Sweep Interval  = ',getAsInteger);
   isc_info_user_names:
     begin
       Users := TStringList.Create;
       try
        DecodeUserNames(Users);
        write('Logged in Users: ');
        for j := 0 to Users.Count - 1 do
          write(Users[j],',');
       finally
         Users.Free;
       end;
       writeln;
     end;
   isc_info_fetches:
     writeln('Fetches  = ',getAsInteger);
   isc_info_marks:
     writeln('Writes  = ',getAsInteger);
   isc_info_reads:
     writeln('Reads  = ',getAsInteger);
   isc_info_writes:
     writeln('Page Writes  = ',getAsInteger);
   isc_info_backout_count:
     WriteOperationCounts('Record Version Removals',getOperationCounts);
   isc_info_delete_count:
     WriteOperationCounts('Deletes',getOperationCounts);
   isc_info_expunge_count:
     WriteOperationCounts('Expunge Count',getOperationCounts);
   isc_info_insert_count:
     WriteOperationCounts('Insert Count',getOperationCounts);
   isc_info_purge_count:
     WriteOperationCounts('Purge Count Countites',getOperationCounts);
   isc_info_read_idx_count:
     WriteOperationCounts('Indexed Reads Count',getOperationCounts);
   isc_info_read_seq_count:
     WriteOperationCounts('Sequential Table Scans',getOperationCounts);
   isc_info_update_count:
     WriteOperationCounts('Update Count',getOperationCounts);
   else
     writeln('Unknown Response');
  end;
end;

procedure TTest9.WriteBytes(Bytes: TByteArray);
var i: integer;
begin
  for i := 0 to length(Bytes) - 1 do
    write(Bytes[i],',');
  writeln;
end;

procedure TTest9.WriteOperationCounts(Category: string; ops: TDBOperationCounts
  );
var i: integer;
begin
  writeln(Category,' Operation Counts');
  for i := 0 to Length(ops) - 1 do
  begin
    writeln('Table ID = ',ops[i].TableID);
    writeln('Count = ',ops[i].Count);
  end;
  writeln;
end;

function TTest9.TestTitle: string;
begin
  Result := 'Test 9: Database Information tests';
end;

procedure TTest9.RunTest(CharSet: string; SQLDialect: integer);
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

