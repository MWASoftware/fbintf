(*  Firebird UDR Support (fbudrtestbed). The fbudr components provide a set of
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
unit Test03;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

interface

uses
  Classes, SysUtils, TestApplication, FBUDRTestApp, IB, FBUdrPlugin;

type

  { TTest03 }

  {Test 03 is used to perform client side testing (UDR engine emulation) for the
   UDR Select procedure declared in udr_test03.pas.}

  TTest03 = class(TFBUDRTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
  protected
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;

implementation

{ TTest03 }

procedure TTest03.DoQuery(Attachment: IAttachment);
var MyTestProc: TExternalProcedureWrapper;
    Transaction: ITransaction;
    Results: IProcedureResults;
begin
  UDRPlugin.Attachment := Attachment;
  MyTestProc := UDRPlugin.GetExternalProcedure('MYSELECTPROC','','fbudrtests!select_proc');
  try
    writeln(OutFile,'List Employees, salaries and accumulated salary');
    Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
    Results := MyTestProc.Execute(Transaction);
    while Results.FetchNext do
    begin
      write(OutFile,Results[0].AsString);
      write(OutFile,', Salary = ',Results[1].AsString);
      writeln(Outfile,' Acc Salary = $',Results[2].AsString);
    end;
    writeln(OutFile);
  finally
    MyTestProc.Free;
  end;
end;

function TTest03.GetTestID: AnsiString;
begin
  Result := '03';
end;

function TTest03.GetTestTitle: AnsiString;
begin
  Result := 'UDR Select Procedure Test';
end;

const
  DDL: array [0..0] of Ansistring = ('create or alter procedure MySelectProc () '+
                                     'returns (FullName VarChar(36), '+
                                     'Salary Numeric(10,2), AccSalary Numeric(10,2) ) as begin SUSPEND;  end'
                                     );

  CleanUpDDL: array [0..0] of Ansistring = ('Drop procedure MySelectProc');


{The test is run using the employee database. Note that dummy versions of the
 UDR Select Procedure must be declared in the database in order to generate the input
 and output parameter metadata. These are always (re-)defined when the test is
 started and removed at the end.
 }

procedure TTest03.RunTest(CharSet: AnsiString; SQLDialect: integer);
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
    ApplyDDL(Attachment,DDL);
    DoQuery(Attachment);
  finally
    ApplyDDL(Attachment,CleanUpDDL);
    Attachment.Disconnect;
  end;
end;

initialization
  RegisterTest(TTest03);
end.

