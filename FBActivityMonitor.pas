unit FBActivityMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FB25ClientAPI, IBExternals, FBLibrary;

  { TActivityReporter is a base class for objects that need to report their activity
    to an activity monitor, where activity is defined as use of a Firebird API call.
    Objects descending from this class always used the "Call" method as a wrapper
    for calls to the Firebird API. Each such call is then classed as activity
    and reported to one or more activity monitors.

    In practice, a transaction monitors statements, blobs and arrays. A Database
    monitors transactions and events. Transaction monitors use the ITransactionMonitor
    interface, implemented through the helper object TTransactionMonitor.
  }

  { $DEFINE DEBUGINTERFACES}   {Define this to check that all interfaces are
                                being destroyed.}

type
  { TMonitoredObject }

  {$IFDEF DEBUGINTERFACES}
  TMonitoredObject = class(TInterfacedObject)
  private
    FObjectCount: integer; static;
  public
    constructor Create;
    destructor Destroy; override;
  end;

    TInterfaceParent = TMonitoredObject;
  {$ELSE}
    TInterfaceParent = TInterfacedObject;
  {$ENDIF}

  IActivityMonitor = interface
    procedure SignalActivity;
  end;

  TActivityReporter = class(TInterfaceParent)
  private
    FMonitors: array of IActivityMonitor;
    function FindMonitor(aMonitor: IActivityMonitor): integer;
  protected
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean = true): ISC_STATUS;
    procedure AddMonitor(aMonitor: IActivityMonitor);
    procedure RemoveMonitor(aMonitor: IActivityMonitor);
    procedure SignalActivity; virtual;
  public
    constructor Create(aMonitor: IActivityMonitor);
  end;

implementation

{ TMonitoredObject }

{$IFDEF DEBUGINTERFACES}
constructor TMonitoredObject.Create;
begin
  inherited Create;
  Inc(FObjectCount);
  writeln('Creating ' + ClassName,', Obj Count = ',FObjectCount);
end;

destructor TMonitoredObject.Destroy;
begin
  Dec(FObjectCount);
  writeln('Destroying ' + ClassName,' Obj Count = ',FObjectCount);
  inherited Destroy;
end;
{$ENDIF}

{ TActivityReporter}

function TActivityReporter.FindMonitor(aMonitor: IActivityMonitor): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(FMonitors) - 1 do
    if FMonitors[i] = aMonitor then
    begin
      Result := i;
      Exit;
    end;
end;

function TActivityReporter.Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
begin
  result := ErrCode;
  SignalActivity;
  if RaiseError and (ErrCode > 0) then
    Firebird25ClientAPI.IBDataBaseError;
end;

procedure TActivityReporter.AddMonitor(aMonitor: IActivityMonitor);
var i: integer;
begin
  if FindMonitor(aMonitor) = -1 then
  begin
    i := Length(FMonitors);
    Setlength(FMonitors,i+1);
    FMonitors[i] := aMonitor;
  end;
end;

procedure TActivityReporter.RemoveMonitor(aMonitor: IActivityMonitor);
var i,j: integer;
begin
  i := FindMonitor(aMonitor);
  if i <> -1 then
  begin
    if Length(FMonitors) = 1 then
      SetLength(FMonitors,0)
    else
    begin
      for j := i + 1 to Length(FMonitors) - 1 do
        FMonitors[j-1] := FMonitors[j];
      SetLength(FMonitors,Length(FMonitors)-1);
    end;
  end;
end;

procedure TActivityReporter.SignalActivity;
var i: integer;
begin
  for i := 0 to Length(FMonitors) - 1 do
      FMonitors[i].SignalActivity;
end;

constructor TActivityReporter.Create(aMonitor: IActivityMonitor);
begin
  inherited Create;
  if aMonitor <> nil then
  begin
    SetLength(FMonitors,1);
    FMonitors[0] := aMonitor;
  end;
end;

end.

