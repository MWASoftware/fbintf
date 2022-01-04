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

unit Test01;
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

  { TTest01 }

  {Test 01 is used to perform client side testing (UDR engine emulation) for the
   UDR functions declared in udr_test01.pas.}

  TTest01 = class(TFBUDRTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
    procedure DoCheckInfo(Attachment: IAttachment);
  protected
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;

implementation

{ TTest01 }

{DoQuery is where the functions that are expected to do work are tested.}

procedure TTest01.DoQuery(Attachment: IAttachment);
var MyRowCount: TExternalFunctionWrapper;
    Transaction: ITransaction;
    Rows: integer;
begin
  {initialize the emulator with the database connection}
  UDRPlugin.Attachment := Attachment;
  {Get the emulator wrapper for the row_count function, declared as MyRowCount}
  MyRowCount := UDRPlugin.GetExternalFunction('MYROWCOUNT','','fbudrtests!row_count');
  try
    writeln(OutFile,'Row Count for Employee');
    {set the input parameter to the EMPLOYEE table}
    MyRowCount.InputParams[0].AsString := 'EMPLOYEE';
    {check the settings of the input parameter}
    ParamInfo(MyRowCount.InputParams);
    Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
    {invoke the function and print the result}
    writeln('Employee Row Count = ',MyRowCount.Execute(Transaction).AsInteger);
    writeln(OutFile);

    {Try again with the DEPARTMENT table}
    MyRowCount.InputParams[0].AsString := 'DEPARTMENT';
    ParamInfo(MyRowCount.InputParams);
    writeln('Dept Row Count = ',MyRowCount.Execute(Transaction).AsInteger);
    writeln(OutFile);

    {And again but demonstrate exception handling by using an invalid table name}
    MyRowCount.InputParams[0].AsString := 'BAD';
    try
      ParamInfo(MyRowCount.InputParams);
      Rows := MyRowCount.Execute(Transaction).AsInteger;
      writeln('Dept Row Count = ',Rows);
      writeln(OutFile);
    except on E: Exception do
      writeln(Outfile,'Expected exception: ',E.Message);
    end;
  finally
    MyRowCount.Free
  end;

  writeln(OutFile);
  {Now call the external version with an error}
  MyRowCount := UDRPlugin.GetExternalFunction('BADROWCOUNT','','fbudrtests!bad_row_count');
  try
    try
      writeln(OutFile,'Row Count for Employee');
      MyRowCount.InputParams[0].AsString := 'EMPLOYEE';
      ParamInfo(MyRowCount.InputParams);
      Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
      writeln('Employee Row Count = ',MyRowCount.Execute(Transaction).AsInteger);
    except on E: Exception do
      writeln(Outfile,'Expected exception: ',E.Message);
    end;
  finally
    MyRowCount.Free;
  end;
end;

{The DoCheckInfo function simply reflects back the value of the "info" field in the
 external name. Here this is set in the emulator to "Hello World".}

procedure TTest01.DoCheckInfo(Attachment: IAttachment);
var ReturnInfo: TExternalFunctionWrapper;
    Transaction: ITransaction;
begin
  UDRPlugin.Attachment := Attachment;
  ReturnInfo := UDRPlugin.GetExternalFunction('RETURNINFO','','fbudrtests!return_info!Hello World');
  try
    Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
    writeln(OutFile,'Info returned = ',ReturnInfo.Execute(Transaction).AsString);
  finally
    ReturnInfo.Free;
  end;
end;

function TTest01.GetTestID: AnsiString;
begin
  Result := '01';
end;

function TTest01.GetTestTitle: AnsiString;
begin
  Result := 'Open the employee database and run a simple UDR Function';
end;

const
  DDL: array [0..2] of Ansistring = ('create or alter function MyRowCount ('+
                                     'table_name varchar(31) '+
                                     ') returns integer as begin end',

                                     'create or alter function BadRowCount ('+
                                     'table_name varchar(31) '+
                                     ') returns integer as begin end',

                                     'create or alter function ReturnInfo '+
                                     'returns VarChar(32) as begin end'
                                     );

  CleanUpDDL: array [0..2] of Ansistring = ('Drop function MyRowCount',
                                            'Drop function BadRowCount',
                                            'Drop function ReturnInfo'
                                            );
{The test is run using the employee database. Note that dummy versions of the
 UDR functions must be declared in the database in order to generate the input
 and output parameter metadata. These are always (re-)defined when the test is
 started and removed at the end.
 }

procedure TTest01.RunTest(CharSet: AnsiString; SQLDialect: integer);
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
    DoCheckInfo(Attachment);
  finally
    ApplyDDL(Attachment,CleanUpDDL);
    Attachment.Disconnect;
  end;
end;

initialization
  RegisterTest(TTest01);
end.

