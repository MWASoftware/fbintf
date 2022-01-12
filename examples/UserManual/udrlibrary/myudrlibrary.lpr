library myudrlibrary;

{$mode delphi}
{$codepage UTF8}
{$interfaces COM}

uses
  Classes, sysutils,  FBUDRController, udr_myrowcount;

exports firebird_udr_plugin;

begin
  with FBUDRControllerOptions do
  begin
    ModuleName := 'myudrlibrary';
    AllowConfigFileOverrides := true;
    LogFileNameTemplate := '$LOGDIR$MODULE.log';
    LogOptions := [loLogFunctions, loLogProcedures, loLogTriggers, loDetails];
  end;
end.
