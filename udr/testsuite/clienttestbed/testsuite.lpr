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
*)
program testsuite;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, FBUDRController, fbudrTestbed, TestApplication, FBUDRTestApp, Test01,
  udr_test01, Test02, udr_test02, udr_test03, Test03, Test04, udr_test04;

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

