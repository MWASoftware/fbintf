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
unit Test04;

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

  {Test 04 is used to perform client side testing (UDR engine emulation) for the
   UDR Trigger declared in udr_test04.pas.}

  { TTest04 }

  TTest04 = class(TFBUDRTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
  protected
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest04 }

procedure TTest04.DoQuery(Attachment: IAttachment);
var MyTestTrigger: TExternalTriggerWrapper;
    Transaction: ITransaction;
begin
  UDRPlugin.Attachment := Attachment;
  MyTestTrigger := UDRPlugin.makeTrigger('MyEmployeeUpdate','fbudrtests!my_employee_update','EMPLOYEE',1{trigger_before});
  try
    writeln(OutFile,'Update EMPLOYEE 2');
    Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taRollback);
    MyTestTrigger.OldValues[0].AsInteger := 2;
    MyTestTrigger.OldValues[3].AsString := '250';
    MyTestTrigger.NewValues[3].AsString := '999';
    MyTestTrigger.Execute(Transaction,2{update trigger});
    writeln(OutFile);
  finally
    MyTestTrigger.Free;
  end;
end;

function TTest04.GetTestID: AnsiString;
begin
  Result := '04';
end;

function TTest04.GetTestTitle: AnsiString;
begin
  Result := 'UDR Trigger Test';
end;

const
  DDL: array [0..1] of Ansistring =
    ('Alter Table EMPLOYEE Add PREVIOUS_PHONE_EXT VarChar(4)',
    'Create or Alter Trigger MyEmployeeUpdate Active After Update On  EMPLOYEE '+
    'as begin end'
    );

  CleanUpDDL: array [0..1] of Ansistring =
    ('drop trigger MyEmployeeUpdate',
     'Alter Table EMPLOYEE drop PREVIOUS_PHONE_EXT'
    );

{The test is run using the employee database. Note that dummy versions of the
 UDR Trigger must be declared in the database in order to generate the input
 and output parameter metadata. These are always (re-)defined when the test is
 started and removed at the end.
 }

procedure TTest04.RunTest(CharSet: AnsiString; SQLDialect: integer);
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
  RegisterTest(TTest04);

end.

