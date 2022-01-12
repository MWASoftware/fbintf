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
unit Test02;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

 {Execute Procedure Tests}

interface

uses
  Classes, SysUtils, TestApplication, FBUDRTestApp, IB, FBUdrPlugin;

type

  {Test 02 is used to perform client side testing (UDR engine emulation) for the
   UDR Execute Procedure declared in udr_test02.pas.}

  { TTest02 }

  TTest02 = class(TFBUDRTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
  protected
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest02 }

{This procedure is called to return the salary and full name of employee 24}

procedure TTest02.DoQuery(Attachment: IAttachment);
var MyTestProc: TExternalProcedureWrapper;
    Transaction: ITransaction;
    Results: IProcedureResults;
begin
  UDRPlugin.Attachment := Attachment;
  MyTestProc := UDRPlugin.makeProcedure('MYTESTPROC','','fbudrtests!test_proc');
  try
    writeln(OutFile,'Salary and Name of Employee 24');
    MyTestProc.InputParams[0].AsInteger := 24;
    ParamInfo(MyTestProc.InputParams);
    Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
    Results := MyTestProc.Execute(Transaction);
    if Results.FetchNext then
    begin
      writeln(OutFile,'Salary = ',Results[0].AsString);
      writeln(OutFile,Results[1].AsString);
    end;
    writeln(OutFile);
  finally
    MyTestProc.Free;
  end;
end;

function TTest02.GetTestID: AnsiString;
begin
  Result := '02';
end;

function TTest02.GetTestTitle: AnsiString;
begin
  Result := 'Execute Procedure Tests';
end;

const
  DDL: array [0..0] of Ansistring = ('create or alter procedure MyTestProc ('+
                                     'EMP_NO SMALLINT '+
                                     ') returns (Salary Numeric(10,2), FullName VarChar(36)) as begin end'
                                     );

  CleanUpDDL: array [0..0] of Ansistring = ('Drop procedure MyTestProc'
                                            );
  {The test is run using the employee database. Note that a dummy version of the
   UDR Execute procedure must be declared in the database in order to generate the input
   and output parameter metadata. These are always (re-)defined when the test is
   started and removed at the end.
   }


procedure TTest02.RunTest(CharSet: AnsiString; SQLDialect: integer);
var Attachment: IAttachment;
    DPB: IDPB;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  Attachment := FirebirdAPI.OpenDatabase(Owner.GetEmployeeDatabaseName,DPB);
  try
    if Attachment.HasProcedure('MYTESTPROC') then
      ApplyDDL(Attachment,CleanUpDDL);
    ApplyDDL(Attachment,DDL);
    DoQuery(Attachment);
  finally
    ApplyDDL(Attachment,CleanUpDDL);
    Attachment.Disconnect;
  end;
end;

initialization
  RegisterTest(TTest02);

end.

