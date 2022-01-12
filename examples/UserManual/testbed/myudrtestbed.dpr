program myudrtestbed;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  FBUDRController,
  FBUdrPlugin,
  IB,
  udr_myrowcount in '..\udrlibrary\udr_myrowcount.pas';

procedure TestRowCount(UDRPlugin: TFBUdrPluginEmulator);
var MyRowCount: TExternalFunctionWrapper;
    Transaction: ITransaction;
    Rows: integer;
begin
  {Get the emulator wrapper for the row_count function, declared as MyRowCount}
  MyRowCount := UDRPlugin.makeFunction('MYROWCOUNT',  {Name of Function in database - case sensitive}
                                       '',            {package name is empty}
                                       'myudrlibrary!row_count' {entry point}
                                       );
  try
    writeln('Row Count for Employee');
    {set the input parameter to the EMPLOYEE table}
    MyRowCount.InputParams[0].AsString := 'EMPLOYEE';

    Transaction := UDRPlugin.Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
    {invoke the function and print the result}
    writeln('Employee Row Count = ',MyRowCount.Execute(Transaction).AsInteger);
    writeln;
  finally
    MyRowCount.Free
  end;
end;

procedure RunTest;
var Attachment: IAttachment;
    DPB: IDPB;
    UDRPlugin: TFBUdrPluginEmulator;
begin
  {Open a connection with the example employee database. Amend database parameters
   as needed.}
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString('SYSDBA');
  DPB.Add(isc_dpb_password).setAsString('masterkey');
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(3);
  Attachment := FirebirdAPI.OpenDatabase('localhost:employee',DPB);
  try
    UDRPlugin := TFBUdrPluginEmulator.Create(FBUDRControllerOptions.ModuleName);
    try
      {initialize the emulator with the database connection}
      UDRPlugin.Attachment := Attachment;
      TestRowCount(UDRPlugin);
    finally
      UDRPlugin.Free;
    end;
  finally
    Attachment.Disconnect(true);
  end;
end;

begin
  with FBUDRControllerOptions do
  begin
    ModuleName := 'myudrlibrary';
    AllowConfigFileOverrides := true;
    LogFileNameTemplate := '$LOGDIR$MODULE.log';
    LogOptions := [loLogFunctions, loLogProcedures, loLogTriggers, loDetails];
  end;
  RunTest;
  readln; {force console window to stay open}
end.
