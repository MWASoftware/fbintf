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
unit udr_test01;

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

  {This unit provides the implementation of selected number of UDR functions used
   to test out various aspects of the TFBUDRFunction class. Note that each class is
   registered with the FBUDRController at initialization time.}

type
  { TMyRowCountFunction implements a simple User Defined Routine (UDR) Function.
    The DDL for this UDR is:

    create or alter function MyRowCount (
      table_name varchar(31)
    ) returns integer
    external name 'fbudrtests!row_count'
    engine udr;

    It is called with a single parameter (the name of a table or view in the database.
    The function then runs a simple select query to determine the number of rows
    in the table and returns the row count.
  }

  { TMyRowCountFunction }

  TMyRowCountFunction = class(TFBUDRFunction)
  public
    function Execute(context: IFBUDRExternalContext;
                     ProcMetadata: IFBUDRProcMetadata;
                     InputParams: IFBUDRInputParams;
                     ResultSQLType: cardinal): variant; override;
  end;

  {UDR function with deliberate error. This has the same signature as above,
   but deliberately tries to invoke an OpenCursor with a null sql string.
   This is to demonstrate exception handling.

    create or alter function BadRowCount (
      table_name varchar(31)
    ) returns integer
    external name 'fbudrtests!bad_row_count'
    engine udr;
}

  { TBadRowCountFunction }

  TBadRowCountFunction = class(TFBUDRFunction)
  public
    function Execute(context: IFBUDRExternalContext;
                     ProcMetadata: IFBUDRProcMetadata;
                     InputParams: IFBUDRInputParams;
                     ResultSQLType: cardinal): variant;  override;
  end;

  {TReturnInfoFunction implements a function that obtains and returns the info
   string from the external name. This is to demonstrate correct handling and
   parsing of the function entry point, as well as an empty input string.

    create or alter function UDRInfo (
    ) returns VarChar(31)
    external name 'fbudrtests!return_info!Hello World'
    engine udr;

}

  { TReturnInfoFunction }

  TReturnInfoFunction = class(TFBUDRFunction)
  public
    function Execute(context: IFBUDRExternalContext;
                     ProcMetadata: IFBUDRProcMetadata;
                     InputParams: IFBUDRInputParams;
                     ResultSQLType: cardinal): variant;  override;
  end;


implementation

{ TReturnInfoFunction }

function TReturnInfoFunction.Execute(context: IFBUDRExternalContext;
  ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams;
  ResultSQLType: cardinal): variant;
begin
  Result := ProcMetadata.getInfo;
end;

{ TBadRowCountFunction }

function TBadRowCountFunction.Execute(context: IFBUDRExternalContext;
  ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams;
  ResultSQLType: cardinal): variant;
begin
  with context do
  begin
    Result := GetAttachment.OpenCursorAtStart(GetTransaction,'')[0].AsInteger;
  end;
end;

{ TMyRowCountFunction }

function TMyRowCountFunction.Execute(context: IFBUDRExternalContext;
  ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams;
  ResultSQLType: cardinal): variant;
begin
  with context do
  begin
    Result := GetAttachment.OpenCursorAtStart(GetTransaction,
    'Select count(*) from ' + InputParams.ByName('table_name').AsString)[0].AsInteger;
  end;
end;

Initialization
  FBRegisterUDRFunction('row_count',TMyRowCountFunction);
  FBRegisterUDRFunction('bad_row_count',TBadRowCountFunction);
  FBRegisterUDRFunction('return_info',TReturnInfoFunction);

end.

