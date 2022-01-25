program TestSelectInto;

{$mode delphi}
{$codepage UTF8}
{$interfaces COM}

uses Classes, FBUDRController, FBUdrPlugin, IB, UdrSelectInto;

procedure ReportResult(aValue: IResults);
var i: integer;
begin
  for i := 0 to aValue.getCount - 1 do
    writeln(aValue[i].Name,' = ',aValue[i].AsString);
end;

procedure ReportResults(cursor: IResultset);
begin
  while not cursor.IsEof do
  begin
    ReportResult(cursor);
    cursor.FetchNext;
  end;
end;

procedure TestSelectInto(UDRPlugin: TFBUdrPluginEmulator);
var SelectInto: TExternalProcedureWrapper;
    Transaction: ITransaction;
    Results: IProcedureResults;
begin
  {Get the emulator wrapper for the row_count function, declared as MyRowCount}
  SelectInto := UDRPlugin.makeProcedure('SELECT_INTO',  {Name of procedure in database - case sensitive}
                                       '',            {package name is empty}
                                       'selectinto!select_into' {entry point}
                                       );
  try
    SelectInto.InputParams.ByName('select_statement').AsString := 'Select * From EMPLOYEE Where Salary < 50000';
    SelectInto.InputParams.ByName('table_name').AsString := 'LOWER_PAID';
    SelectInto.InputParams.ByName('table_type').AsString := '';
    Transaction := UDRPlugin.Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_read_committed],taCommit);
    Results := SelectInto.Execute(Transaction);
    if Results.FetchNext then
      writeln('Procedure Returns ',Results.ByName('Status').AsString)
    else
       writeln('No results');

   writeln('Show Table');
   ReportResults(UDRPlugin.Attachment.OpenCursorAtStart(Transaction,'Select * From LOWER_PAID'));
  finally
    SelectInto.Free;
  end;
end;

const
  DDL: array [0..0] of Ansistring = ('create or alter procedure select_into (' +
                                     'select_statement blob sub_type 1 not null,'+
                                     'table_name varchar(63) not null,'+
                                     'table_type varchar(25) not null' +
                                     ') returns ('+
                                     'status varchar(100)) as begin end'
                                     );

  CleanUpDDL: array [0..0] of Ansistring = ('Drop procedure select_into'
                                            );


procedure RunTest;
var Attachment: IAttachment;
    DPB: IDPB;
    UDRPlugin: TFBUdrPluginEmulator;
    i: integer;
begin
  {Open a connection with the example employee database. Amend database parameters
   as needed.}
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString('SYSDBA');
  DPB.Add(isc_dpb_password).setAsString('masterkey');
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(3);
  Attachment := FirebirdAPI.OpenDatabase('localhost:employee',DPB);
  for i := 0 to Length(DDL) -1 do
    Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],DDL[i]);
  try
    UDRPlugin := TFBUdrPluginEmulator.Create(FBUDRControllerOptions.ModuleName);
    try
      {initialize the emulator with the database connection}
      UDRPlugin.Attachment := Attachment;
      TestSelectInto(UDRPlugin);
    finally
      UDRPlugin.Free;
    end;
  finally
    for i := 0 to Length(CleanUpDDL) -1 do
      Attachment.ExecImmediate([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],CleanUpDDL[i]);
    Attachment.Disconnect(true);
  end;
end;

begin
  with FBUDRControllerOptions do
  begin
    ModuleName := 'selectinto';
    AllowConfigFileOverrides := true;
    LogFileNameTemplate := 'SelectInto.log';
    LogOptions := [loLogFunctions, loLogProcedures, loLogTriggers, loDetails];
  end;
  RunTest;
  {$IFDEF WINDOWS}
  readln; {force console window to stay open}
  {$ENDIF}
end.

