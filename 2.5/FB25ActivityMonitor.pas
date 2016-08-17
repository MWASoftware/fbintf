unit FB25ActivityMonitor;

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

  TActivityReporter = class(TInterfaceParent)
  private
    FMonitors: TList;
  protected
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean = true): ISC_STATUS;
    procedure AddMonitor(aMonitor: TObject);
    procedure RemoveMonitor(aMonitor: TObject);
    procedure SignalActivity;
    property Monitors: TList read FMonitors;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  IActivityMonitor = interface ['{6943f656-6e51-4f9b-aede-3c9e5556f288}']
    function HasActivity: boolean;  {One shot - reset on read}
    procedure ResetActivity;
    procedure SignalActivity;
  end;

  { TActivityMonitor }

  TActivityMonitor = class(TInterfaceParent, IActivityMonitor)
  private
    FActivity: boolean;
  public
    function HasActivity: boolean;  {One shot - reset on read}
    procedure ResetActivity;
    procedure SignalActivity;
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

{ TActivityMonitor }

function TActivityMonitor.HasActivity: boolean;
begin
  Result := FActivity;
  FActivity := false;
end;

procedure TActivityMonitor.ResetActivity;
begin
  FActivity := false;
end;

procedure TActivityMonitor.SignalActivity;
begin
  FActivity := true;
end;

{ TActivityReporter}

function TActivityReporter.Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
begin
  result := ErrCode;
  SignalActivity;
  if RaiseError and (ErrCode > 0) then
    Firebird25ClientAPI.IBDataBaseError;
end;

procedure TActivityReporter.AddMonitor(aMonitor: TObject);
begin
  if (aMonitor is IActivityMonitor) and (FMonitors.IndexOf(aMonitor) = -1) then
    FMonitors.Add(aMonitor);
end;

procedure TActivityReporter.RemoveMonitor(aMonitor: TObject);
var i: integer;
begin
  for i := 0 to FMonitors.Count - 1 do
    if TObject(FMonitors[i]) = aMonitor then
      FMonitors.Delete(i);
end;

procedure TActivityReporter.SignalActivity;
var i: integer;
begin
  for i := 0 to FMonitors.Count - 1 do
    if TObject(FMonitors[i]) is IActivityMonitor then
      (TObject(FMonitors[i]) as IActivityMonitor).SignalActivity;
end;

constructor TActivityReporter.Create;
begin
  inherited Create;
  FMonitors := TList.Create;
end;

destructor TActivityReporter.Destroy;
begin
  if assigned(FMonitors) then
    FMonitors.Free;
  inherited Destroy;
end;

end.

