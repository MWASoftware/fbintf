unit udr_myrowcount;

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

{ The DDL for this external function is

create or alter function MyRowCount (
      table_name varchar(31)
    ) returns integer
    external name 'fbudrtests!row_count'
    engine udr;
}

type
  TMyRowCountFunction = class(TFBUDRFunction)
  public
    function Execute(context: IFBUDRExternalContext;
                     ProcMetadata: IFBUDRProcMetadata;
                     InputParams: IFBUDRInputParams;
                     ResultSQLType: cardinal): variant; override;
  end;

implementation

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
  FBRegisterUDRFunction('row_count',TmyRowCountFunction);

end.

