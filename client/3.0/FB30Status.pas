unit FB30Status;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBActivityMonitor;

type
  PStatusVector = ^TStatusVector;
  TStatusVector = array[0..4] of Firebird.NativeIntPtr;

  { TFBStatus }

  TFBStatus = class(TInterfaceParent,IStatus)
  private
    FIBCS: TRTLCriticalSection; static;
    FIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    FStatus: Firebird.IStatus;
  public
    constructor Create;
    function GetStatus: Firebird.IStatus;
    function StatusVector: PStatusVector;

    {IStatus}
    function GetIBErrorCode: Long;
    function Getsqlcode: Long;
    function GetMessage: string;
    function CheckStatusVector(ErrorCodes: array of TFBStatusCode): Boolean;
    function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
  end;

implementation

{ TFBStatus }

constructor TFBStatus.Create;
begin
  inherited Create;
  FIBDataBaseErrorMessages := [ShowSQLMessage, ShowIBMessage];
end;

function TFBStatus.GetStatus: Firebird.IStatus;
begin
  if FStatus = nil then
  with Firebird30ClientAPI do
    FStatus := MasterIntf.GetStatus;
  Result := FStatus;
end;

function TFBStatus.StatusVector: PStatusVector;
begin
  Result := PStatusVector(GetStatus.getErrors);
end;

function TFBStatus.GetIBErrorCode: Long;
begin

end;

function TFBStatus.Getsqlcode: Long;
begin

end;

function TFBStatus.GetMessage: string;
begin

end;

function TFBStatus.CheckStatusVector(ErrorCodes: array of TFBStatusCode
  ): Boolean;
begin
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

initialization
  InitCriticalSection(TFBStatus.FIBCS);

finalization
  DoneCriticalSection(TFBStatus.FIBCS);
end.

