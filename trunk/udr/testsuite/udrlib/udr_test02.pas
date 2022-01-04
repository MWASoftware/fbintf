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
unit udr_test02;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBUDRController, FBUDRIntf;

  {This unit provides the implementation of selected number of UDR Execute
   procedures used to test out various aspects of the TFBUDRExecuteProcedure class.
   Note that each class is registered with the FBUDRController at initialization time.}

type
  {TMyTestProcedure is a simple Execute procedure to demonstrate use of the
   UDR library. The Employee database is assumed. The input parameter selects a
   row in the EMPLOYEE table and the procedure returns the salary and full name
   of the selected employee.

   create or alter procedure MyTestProc (
      EMP_NO SMALLINT
    ) returns (Salary Numeric(10,2), FullName VarChar(36))
    external name 'fbudrtests!test_proc'
    engine udr;
  }

  TMyTestProcedure = class(TFBUDRExecuteProcedure)
  public
    procedure Execute(context: IFBUDRExternalContext;
                      ProcMetadata: IFBUDRProcMetadata;
                      InputParams: IFBUDRInputParams;
                      OutputData: IFBUDROutputData); override;
  end;

implementation

{ TMyTestProcedure }

procedure TMyTestProcedure.Execute(context: IFBUDRExternalContext;
  ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams;
  OutputData: IFBUDROutputData);
var Results: IResultSet;
begin
  with context do
  begin
    Results := GetAttachment.OpenCursorAtStart(GetTransaction,
                              'Select Salary, Full_Name From EMPLOYEE Where EMP_NO = ?',
                              [InputParams.ByName('EMP_NO').AsInteger]);
    OutputData.ByName('SALARY').AsCurrency := Results.ByName('Salary').AsCurrency;
    OutputData.ByName('FULLNAME').AsString := Results.ByName('Full_Name').AsString;

  end;
end;

Initialization
  FBRegisterUDRProcedure('test_proc',TMyTestProcedure);

end.

