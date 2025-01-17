(*
 *  Firebird UDR Support (fbudrtested). The fbudr components provide a set of
 *  Pascal language bindings for the Firebird API in support of server
 *  side User Defined Routines (UDRs). The fbudr package is an extension
 *  to the Firebird Pascal API.
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
 *  The Original Code is (C) 2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
library fbudrtests;

uses
  System.SysUtils,
  System.Classes,
  FBUDRController,
  udr_test01 in 'udr_test01.pas',
  udr_test02 in 'udr_test02.pas',
  udr_test03 in 'udr_test03.pas',
  udr_test04 in 'udr_test04.pas',
  udr_test05 in 'udr_test05.pas';

{$R *.res}

exports firebird_udr_plugin;

begin
  with FBUDRControllerOptions do
  begin
    ModuleName := 'fbudrtests';
    AllowConfigFileOverrides := true;
    LogFileNameTemplate := '$LOGDIR$MODULE.log';
    LogOptions := [loLogFunctions, loLogProcedures, loLogTriggers, loDetails];
  end;
end.
