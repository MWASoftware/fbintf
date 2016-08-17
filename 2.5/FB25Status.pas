unit FB25Status;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, IBExternals, FBMessages;

const
  IBLocalBufferLength = 512;
  IBBigLocalBufferLength = IBLocalBufferLength * 2;
  IBHugeLocalBufferLength = IBBigLocalBufferLength * 20;

type

  TStatusVector              = array[0..19] of ISC_STATUS;
  PStatusVector              = ^TStatusVector;

  { TFBStatus }

  TFBStatus = class(TInterfacedObject,IStatus)
  private
    FIBCS: TRTLCriticalSection; static;
    FIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
  public
    constructor Create;
    function StatusVector: PISC_STATUS;

    {IStatus}
    function GetIBErrorCode: Long;
    function Getsqlcode: Long;
    function GetMessage: string;
    function CheckStatusVector(ErrorCodes: array of TFBStatusCode): Boolean;
    function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
  end;

implementation

uses FB25ClientAPI, IBUtils;

threadvar
  FStatusVector: TStatusVector;


{ TFBStatus }

constructor TFBStatus.Create;
begin
  inherited Create;
  FIBDataBaseErrorMessages := [ShowSQLMessage, ShowIBMessage];
end;

function TFBStatus.GetIBErrorCode: Long;
begin
  Result := FStatusVector[1];
end;

function TFBStatus.Getsqlcode: Long;
begin
  with Firebird25ClientAPI do
    Result := isc_sqlcode(@FStatusVector);
end;

function TFBStatus.GetMessage: string;
var local_buffer: array[0..IBHugeLocalBufferLength - 1] of char;
    IBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    sqlcode: Long;
    psb: PISC_STATUS;
begin
  Result := '';
  IBDataBaseErrorMessages := FIBDataBaseErrorMessages;
  sqlcode := Getsqlcode;
  if (ShowSQLCode in IBDataBaseErrorMessages) then
    Result := Result + 'SQLCODE: ' + IntToStr(sqlcode); {do not localize}

  Exclude(IBDataBaseErrorMessages, ShowSQLMessage);
  if (ShowSQLMessage in IBDataBaseErrorMessages) then
  begin
    with Firebird25ClientAPI do
      isc_sql_interprete(sqlcode, local_buffer, IBLocalBufferLength);
    if (ShowSQLCode in FIBDataBaseErrorMessages) then
      Result := Result + CRLF;
    Result := Result + strpas(local_buffer);
  end;

  if (ShowIBMessage in IBDataBaseErrorMessages) then
  begin
    if (ShowSQLCode in IBDataBaseErrorMessages) or
       (ShowSQLMessage in IBDataBaseErrorMessages) then
      Result := Result + CRLF;
    psb := @FStatusVector;
    with Firebird25ClientAPI do
    while (isc_interprete(@local_buffer, @psb) > 0) do
    begin
      if (Result <> '') and (Result[Length(Result)] <> LF) then
        Result := Result + CRLF;
      Result := Result + strpas(local_buffer);
    end;
  end;
  if (Result <> '') and (Result[Length(Result)] = '.') then
    Delete(Result, Length(Result), 1);
end;

function TFBStatus.CheckStatusVector(ErrorCodes: array of TFBStatusCode
  ): Boolean;
var
  p: PISC_STATUS;
  i: Integer;
  procedure NextP(i: Integer);
  begin
    p := PISC_STATUS(PChar(p) + (i * SizeOf(ISC_STATUS)));
  end;
begin
  p := @FStatusVector;
  result := False;
  while (p^ <> 0) and (not result) do
    case p^ of
      3: NextP(3);
      1, 4:
      begin
        NextP(1);
        i := 0;
        while (i <= High(ErrorCodes)) and (not result) do
        begin
          result := p^ = ErrorCodes[i];
          Inc(i);
        end;
        NextP(1);
      end;
      else
        NextP(2);
    end;
end;

function TFBStatus.GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
begin
  EnterCriticalSection(FIBCS);
  try
    result := FIBDataBaseErrorMessages;
  finally
    LeaveCriticalSection(FIBCS);
  end;
end;

procedure TFBStatus.SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
begin
  EnterCriticalSection(FIBCS);
  try
    FIBDataBaseErrorMessages := Value;
  finally
    LeaveCriticalSection(FIBCS);
  end;
end;

function TFBStatus.StatusVector: PISC_STATUS;
begin
  Result := @FStatusVector;
end;

initialization
  InitCriticalSection(TFBStatus.FIBCS);

finalization
  DoneCriticalSection(TFBStatus.FIBCS);


end.

