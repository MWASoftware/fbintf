library myudrlibrary;

uses
  System.SysUtils,
  System.Classes,
  FBUDRController,
  udr_myrowcount in 'udr_myrowcount.pas';

exports firebird_udr_plugin;

{$R *.res}

begin
  with FBUDRControllerOptions do
  begin
    ModuleName := 'myudrlibrary';
    AllowConfigFileOverrides := true;
    LogFileNameTemplate := '$LOGDIR$MODULE.log';
    LogOptions := [loLogFunctions, loLogProcedures, loLogTriggers, loDetails];
  end;
end.
