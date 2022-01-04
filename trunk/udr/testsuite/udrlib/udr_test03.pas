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
unit udr_test03;

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

  {This unit provides the implementation of selected number of UDR Select
   procedures used to test out various aspects of the TFBUDRSelectProcedure class.
   Note that each class is registered with the FBUDRController at initialization time.}

type
  {TMySelectProcedure implements a simple select procedure to return the
   list of employee salaries plus a line by line accumulator. The employee
   database is assumed.

   create or alter procedure MySelectProc ()
    returns (FullName VarChar(36), Salary Numeric(10,2), AccSalary Numeric(10,2) )
    external name 'fbudrtests!select_proc'
    engine udr;
   }

  TMySelectProcedure  = class(TFBUDRSelectProcedure)
  private
    FAccSalary: currency;
    FResults: IResultset;
  public
    procedure open(context: IFBUDRExternalContext;
                     ProcMetadata: IFBUDRProcMetadata;
                     InputParams: IFBUDRInputParams); override;
    function fetch(OutputData: IFBUDROutputData): boolean; override;
    procedure close; override;
  end;

implementation

{ TMySelectProcedure }

{open is called first and opens the cursor. The IResultset returned is saved
 as a private property of the class, and the accumulator is initialized to zero.}

procedure TMySelectProcedure.open(context: IFBUDRExternalContext;
  ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams);
begin
  with context do
   FResults := GetAttachment.OpenCursor(GetTransaction,'Select Full_Name,Salary from EMPLOYEE order by EMP_NO');
  FAccSalary := 0;
end;

{fetch is called to return each row in the OutputData. The function returns
 false when at EOF.}

function TMySelectProcedure.fetch(OutputData: IFBUDROutputData): boolean;
begin
  Result := (FResults <> nil) and FResults.FetchNext;
  if Result then
  begin
    FAccSalary := FAccSalary + FResults.ByName('Salary').AsCurrency;
    OutputData.ByName('FullName').AsString := FResults.ByName('Full_Name').AsString;
    OutputData.ByName('Salary').AsCurrency := FResults.ByName('Salary').AsCurrency;
    OutputData.ByName('AccSalary').AsCurrency := FAccSalary;
  end;
end;

{close is called after fetch returned EOF. Here is is used to explicitly close
 the cursor. Although this will be closed automatically when the class is
 freed, or open called again.}

procedure TMySelectProcedure.close;
begin
  FResults := nil;
end;

Initialization
  FBRegisterUDRProcedure('select_proc',TMySelectProcedure);


end.

