unit FB25Attachment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBHeader, IBExternals;

type

  { TFBAttachment }

  TFBAttachment = class(TInterfacedObject,IAttachment)
  private
    FClientAPI: TFBClientAPI;
    FHandle: TISC_DB_HANDLE;
    function GetLongDatabaseInfo(DatabaseInfoCommand: Integer): Long;
    function GetOperationCounts(DBInfoCommand: Integer; FOperation: TStringList): TStringList;
  public
    constructor Create(ClientAPI: TFBClientAPI);
    property Handle: TISC_DB_HANDLE read FHandle;

  public
    {IAttachment}
    function GetStatus: IStatus;
    procedure Disconnect(Force: boolean);
    procedure DropDatabase;
    function StartTransaction(Params: TStrings): ITransaction;
    function CreateBlob(transaction: ITransaction): IBlob;
    function OpenBlob(transaction: ITransaction): IBlob;
    procedure ExecImmediate(transaction: ITransaction; sql: string);
    function Prepare(transaction: ITransaction; sql: string): IStatement;
    function GetEventHandler(Events: TStrings): IEvents;

    {Database Information}
    function GetBlobCharSetID(transaction: ITransaction; tableName, columnName: string): short;
    function GetAllocation: Long;
    function GetBaseLevel: Long;
    function GetDBFileName: String;
    function GetDBSiteName: String;
    function GetDBImplementationNo: Long;
    function GetDBImplementationClass: Long;
    function GetNoReserve: Long;
    function GetODSMinorVersion: Long;
    function GetODSMajorVersion: Long;
    function GetPageSize: Long;
    function GetVersion: String;
    function GetCurrentMemory: Long;
    function GetForcedWrites: Long;
    function GetMaxMemory: Long;
    function GetNumBuffers: Long;
    function GetSweepInterval: Long;
    function GetUserNames: TStringList;
    function GetFetches: Long;
    function GetMarks: Long;
    function GetReads: Long;
    function GetWrites: Long;
    function GetBackoutCount: TStringList;
    function GetDeleteCount: TStringList;
    function GetExpungeCount: TStringList;
    function GetInsertCount: TStringList;
    function GetPurgeCount: TStringList;
    function GetReadIdxCount: TStringList;
    function GetReadSeqCount: TStringList;
    function GetUpdateCount: TStringList;
    function GetReadOnly: Long;
    function GetDBSQLDialect: Long;
  end;

implementation

uses FB25Events;

{ TFBAttachment }

function TFBAttachment.GetLongDatabaseInfo(DatabaseInfoCommand: Integer): Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  length: Integer;
  _DatabaseInfoCommand: Char;
begin
  _DatabaseInfoCommand := Char(DatabaseInfoCommand);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @_DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
    length := isc_vax_integer(@local_buffer[1], 2);
    Result := isc_vax_integer(@local_buffer[3], length);
  end;
end;

function TFBAttachment.GetOperationCounts(DBInfoCommand: Integer;
  FOperation: TStringList): TStringList;
var
  local_buffer: array[0..IBHugeLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
  i, qtd_tables, id_table, qtd_operations: Integer;
begin
  if FOperation = nil then FOperation := TStringList.Create;
  result := FOperation;
  DatabaseInfoCommand := Char(DBInfoCommand);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                           IBHugeLocalBufferLength, local_buffer), True);
    FOperation.Clear;
    { 1. 1 byte specifying the item type requested (e.g., isc_info_insert_count).
      2. 2 bytes telling how many bytes compose the subsequent value pairs.
      3. A pair of values for each table in the database on wich the requested
        type of operation has occurred since the database was last attached.
      Each pair consists of:
      1. 2 bytes specifying the table ID.
      2. 4 bytes listing the number of operations (e.g., inserts) done on that table.
    }
    qtd_tables := trunc(isc_vax_integer(@local_buffer[1],2)/6);
    for i := 0 to qtd_tables - 1 do
    begin
      id_table := isc_vax_integer(@local_buffer[3+(i*6)],2);
      qtd_operations := isc_vax_integer(@local_buffer[5+(i*6)],4);
      FOperation.Add(IntToStr(id_table)+'='+IntToStr(qtd_operations));
    end;
  end;
end;

constructor TFBAttachment.Create(ClientAPI: TFBClientAPI);
begin
  inherited Create;
  FClientAPI := ClientAPI;
end;

function TFBAttachment.GetStatus: IStatus;
begin
  Result := FClientAPI.Status;
end;

procedure TFBAttachment.Disconnect(Force: boolean);
begin
  with FClientAPI do
    Call(isc_detach_database(StatusVector, @FHandle,not Force);
  Free;
end;

procedure TFBAttachment.DropDatabase;
begin
  with FClientAPI do
    Call(isc_drop_database(StatusVector, @FHandle));
  Free;
end;

function TFBAttachment.StartTransaction(Params: TStrings): ITransaction;
begin
  Result := TFBTransaction.Create(FClientAPI,self,Params);
end;

function TFBAttachment.CreateBlob(transaction: ITransaction): IBlob;
begin

end;

function TFBAttachment.OpenBlob(transaction: ITransaction): IBlob;
begin

end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: string);
begin

end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: string
  ): IStatement;
begin

end;

function TFBAttachment.GetEventHandler(Events: TStrings): IEvents;
begin
  Result := TFBEvents.Create(FClientAPI,FHandle,Events);
end;

function TFBAttachment.GetBlobCharSetID(transaction: ITransaction; tableName,
  columnName: string): short;
var desc: TISC_BLOB_DESC;
    uGlobal: array [0..31] of char;
begin
  with FClientAPI do
    Callisc_blob_lookup_desc(StatusVector,@FHandle,@(transaction as TFBTransaction).Handle),
                tableName,columnName,@desc,@uGlobal);
  Result := desc.blob_desc_charset;
end;

function TFBAttachment.GetAllocation: Long;
begin
  result := GetLongDatabaseInfo(isc_info_allocation);
end;

function TFBAttachment.GetBaseLevel: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_base_level);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
    result := isc_vax_integer(@local_buffer[4], 1);
  end;
end;

function TFBAttachment.GetDBFileName: String;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_id);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                           IBLocalBufferLength, local_buffer), True);
    local_buffer[5 + Int(local_buffer[4])] := #0;
    result := String(PChar(@local_buffer[5]));
  end;
end;

function TFBAttachment.GetDBSiteName: String;
var
  local_buffer: array[0..IBBigLocalBufferLength - 1] of Char;
  p: PChar;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_id);
  with FClientAPI do
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                        IBLocalBufferLength, local_buffer), True);
  p := @local_buffer[5 + Int(local_buffer[4])]; { DBSiteName Length }
  p := p + Int(p^) + 1;                         { End of DBSiteName }
  p^ := #0;                                     { Null it }
  result := String(PChar(@local_buffer[6 + Int(local_buffer[4])]));
end;

function TFBAttachment.GetDBImplementationNo: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_implementation);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                        IBLocalBufferLength, local_buffer), True);
    result := isc_vax_integer(@local_buffer[3], 1);
  end;
end;

function TFBAttachment.GetDBImplementationClass: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_implementation);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
    result := isc_vax_integer(@local_buffer[4], 1);
  end;
end;

function TFBAttachment.GetNoReserve: Long;
begin
  result := GetLongDatabaseInfo(isc_info_no_reserve);
end;

function TFBAttachment.GetODSMinorVersion: Long;
begin
  result := GetLongDatabaseInfo(isc_info_ods_minor_version);
end;

function TFBAttachment.GetODSMajorVersion: Long;
begin
  result := GetLongDatabaseInfo(isc_info_ods_version);
end;

function TFBAttachment.GetPageSize: Long;
begin
  result := GetLongDatabaseInfo(isc_info_page_size);
end;

function TFBAttachment.GetVersion: String;
var
  local_buffer: array[0..IBBigLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_version);
  with FClientAPI do
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                        IBBigLocalBufferLength, local_buffer), True);
  local_buffer[5 + Int(local_buffer[4])] := #0;
  result := String(PChar(@local_buffer[5]));
end;

function TFBAttachment.GetCurrentMemory: Long;
begin
  result := GetLongDatabaseInfo(isc_info_current_memory);
end;

function TFBAttachment.GetForcedWrites: Long;
begin
  result := GetLongDatabaseInfo(isc_info_forced_writes);
end;

function TFBAttachment.GetMaxMemory: Long;
begin
  result := GetLongDatabaseInfo(isc_info_max_memory);
end;

function TFBAttachment.GetNumBuffers: Long;
begin
  result := GetLongDatabaseInfo(isc_info_num_buffers);
end;

function TFBAttachment.GetSweepInterval: Long;
begin
  result := GetLongDatabaseInfo(isc_info_sweep_interval);
end;

function TFBAttachment.GetUserNames: TStringList;
var
  local_buffer: array[0..IBHugeLocalBufferLength - 1] of Char;
  temp_buffer: array[0..IBLocalBufferLength - 2] of Char;
  DatabaseInfoCommand: Char;
  i, user_length: Integer;
begin
  result := FUserNames;
  DatabaseInfoCommand := Char(isc_info_user_names);
  with FClientAPI do
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                        IBHugeLocalBufferLength, local_buffer), True);
  FUserNames.Clear;
  i := 0;
  while local_buffer[i] = Char(isc_info_user_names) do
  begin
    Inc(i, 3); { skip "isc_info_user_names byte" & two unknown bytes of structure (see below) }
    user_length := Long(local_buffer[i]);
    Inc(i,1);
    Move(local_buffer[i], temp_buffer[0], user_length);
    Inc(i, user_length);
    temp_buffer[user_length] := #0;
    FUserNames.Add(String(temp_buffer));
  end;
end;

function TFBAttachment.GetFetches: Long;
begin
  result := GetLongDatabaseInfo(isc_info_fetches);
end;

function TFBAttachment.GetMarks: Long;
begin
  result := GetLongDatabaseInfo(isc_info_marks);
end;

function TFBAttachment.GetReads: Long;
begin
  result := GetLongDatabaseInfo(isc_info_reads);
end;

function TFBAttachment.GetWrites: Long;
begin
  result := GetLongDatabaseInfo(isc_info_writes);
end;

function TFBAttachment.GetBackoutCount: TStringList;
begin
  result := GetOperationCounts(isc_info_backout_count,FBackoutCount);
end;

function TFBAttachment.GetDeleteCount: TStringList;
begin
  result := GetOperationCounts(isc_info_delete_count,FDeleteCount);
end;

function TFBAttachment.GetExpungeCount: TStringList;
begin
  result := GetOperationCounts(isc_info_expunge_count,FExpungeCount);
end;

function TFBAttachment.GetInsertCount: TStringList;
begin
  result := GetOperationCounts(isc_info_insert_count,FInsertCount);
end;

function TFBAttachment.GetPurgeCount: TStringList;
begin
  result := GetOperationCounts(isc_info_purge_count,FPurgeCount);
end;

function TFBAttachment.GetReadIdxCount: TStringList;
begin
  result := GetOperationCounts(isc_info_read_idx_count,FReadIdxCount);
end;

function TFBAttachment.GetReadSeqCount: TStringList;
begin
  result := GetOperationCounts(isc_info_read_seq_count,FReadSeqCount);
end;

function TFBAttachment.GetUpdateCount: TStringList;
begin
  result := GetOperationCounts(isc_info_update_count,FUpdateCount);
end;

function TFBAttachment.GetReadOnly: Long;
begin
  result := GetLongDatabaseInfo(isc_info_db_read_only);
end;

function TFBAttachment.GetDBSQLDialect: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  length: Integer;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_SQL_Dialect);
  with FClientAPI do
  begin
    Call(isc_database_info(StatusVector, @FHandle, 1, @DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
    if (local_buffer[0] <> Char(isc_info_db_SQL_dialect)) then
      result := 1
    else begin
      length := isc_vax_integer(@local_buffer[1], 2);
      result := isc_vax_integer(@local_buffer[3], length);
    end;
  end;
end;

end.

