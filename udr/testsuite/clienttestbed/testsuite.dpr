(*
 *  Firebird UDR Support (fbudrtestbed). The fbudr components provide a set of
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
*)program testsuite;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Classes, FBUDRController,
  FBUDRTestApp in 'FBUDRTestApp.pas',
  Test01 in 'Test01.pas',
  Test02 in 'Test02.pas',
  Test03 in 'Test03.pas',
  Test04 in 'Test04.pas',
  TestApplication in 'TestApplication.pas',
  udr_test01 in '..\udrlib\udr_test01.pas',
  udr_test02 in '..\udrlib\udr_test02.pas',
  udr_test03 in '..\udrlib\udr_test03.pas',
  udr_test04 in '..\udrlib\udr_test04.pas';

var
  Application: TFBUDRTestApp;
begin
  with FBUDRControllerOptions do
  begin
    ModuleName := 'fbudrtests';
    AllowConfigFileOverrides := true;
    LogFileNameTemplate := 'serverside.log';
    ConfigFileNameTemplate := 'testsuite.conf';
    LogOptions :=  [loLogFunctions, loLogProcedures, loLogTriggers, loDetails,
                                    loModifyQueries, loReadOnlyQueries];
  end;
  Application := TFBUDRTestApp.Create(nil);
  Application.Title := 'FB UDR Test Suite';
  Application.Run;
  Application.Free;
end.
