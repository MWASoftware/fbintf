library myudrlibrary;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

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
