(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
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
 *  The Original Code is (C) 2016 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBActivityMonitor;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IBExternals;

  { $DEFINE DEBUGINTERFACES}   {Define this to check that all interfaces are
                                being destroyed.}

type
  { TMonitoredObject is an optional class used to journal all interface creatino
   and deletion as well as keeping a count of how many monitored interfaces
   exist at any one time. It is used at development to look for memory leaks
   due to interfaces not being discarded when no longer used.}

  {$IFDEF DEBUGINTERFACES}
  TMonitoredObject = class(TInterfacedObject)
  private
    FObjectCount: integer; static;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  {TFBInterfacedObject is used as the base class for interfaces objects and can
   be either a synonym for TInterfacedObject (default) or TMonitored object}

    TFBInterfacedObject = TMonitoredObject;
  {$ELSE}
    TFBInterfacedObject = TInterfacedObject;
  {$ENDIF}

  { TInterfaceOwner }

  TInterfaceOwner = class(TFBInterfacedObject)
  private
    FInterfaces: array of TInterfacedObject;
    FInterfaceRefs: array of IUnknown;
    FRetainInterfaces: boolean;
    FMinInterfaces: integer;
    function GetCount: integer;
    procedure SetRetainInterfaces(AValue: boolean);
  protected
    procedure AddInterface(index: integer; obj: TInterfacedObject);
    function HasInterface(index: integer): boolean;
    procedure ReleaseInterfaces;
  public
    constructor Create(aInterfaces: integer=0);
    destructor Destroy; override;
    procedure AddObject(obj: TInterfacedObject);
    function GetInterface(index: integer): TInterfacedObject;
    procedure Remove(intf: TInterfacedObject);
    property InterfaceCount: integer read GetCount;
    property RetainInterfaces: boolean read FRetainInterfaces write SetRetainInterfaces;
  end;

  {The IActivityMonitor interface is provided by classes that receive activity
   reports.}

  IActivityMonitor = interface
    procedure AddObject(obj: TInterfacedObject);
    procedure Remove(intf: TInterfacedObject);
    procedure SignalActivity;
  end;

  { TActivityReporter is a base class for objects that need to report their activity
    to an activity monitor, where activity is defined as use of a Firebird API call.
    Objects descending from this class always used the "Call" method as a wrapper
    for calls to the Firebird API. Each such call is then classed as activity
    and reported to one or more activity monitors.

    In practice, a transaction monitors statements, blobs and arrays. A Database
    monitors transactions and events. Transaction monitors use the ITransactionMonitor
    interface, implemented through the helper object TTransactionMonitor.
  }

  TActivityReporter = class(TInterfaceOwner)
  private
    FHasActivity: boolean;
    FMonitors: array of IActivityMonitor;
    function FindMonitor(aMonitor: IActivityMonitor): integer;
  protected
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean = true): ISC_STATUS;
    procedure AddMonitor(aMonitor: IActivityMonitor);
    procedure RemoveMonitor(aMonitor: IActivityMonitor);
  public
    constructor Create(aMonitor: IActivityMonitor;aInterfaces: integer=0);
    destructor Destroy; override;
    function HasActivity: boolean;
    procedure SignalActivity;
  end;

  { TActivityHandler is a base class for classes that receive activity reports.}

  TActivityHandler = class(TInterfaceOwner,IActivityMonitor)
  private
    FHasActivity: boolean;
  public
    function HasActivity: boolean;
    procedure SignalActivity;
  end;

implementation

uses FB25ClientAPI;

{ TActivityHandler }

function TActivityHandler.HasActivity: boolean;
begin
  Result := FHasActivity;
  FHasActivity := false;
end;

procedure TActivityHandler.SignalActivity;
begin
  FHasActivity := true;
end;

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
    aMonitor.AddObject(self);
  end;
end;

procedure TActivityReporter.RemoveMonitor(aMonitor: IActivityMonitor);
var i,j: integer;
begin
  i := FindMonitor(aMonitor);
  if i <> -1 then
  begin
    aMonitor.Remove(self);
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
  FHasActivity := true;
  for i := 0 to Length(FMonitors) - 1 do
      FMonitors[i].SignalActivity;
end;

constructor TActivityReporter.Create(aMonitor: IActivityMonitor;
  aInterfaces: integer);
begin
  inherited Create(aInterfaces);
  if aMonitor <> nil then
  begin
    SetLength(FMonitors,1);
    FMonitors[0] := aMonitor;
  end;
end;

destructor TActivityReporter.Destroy;
var i: integer;
begin
  for i := 0 to Length(FMonitors) - 1 do
    FMonitors[i].Remove(self);
  inherited Destroy;
end;

function TActivityReporter.HasActivity: boolean;
begin
  Result := FHasActivity;
  FHasActivity := false;
end;

{ TInterfaceOwner }

constructor TInterfaceOwner.Create(aInterfaces: integer);
begin
  inherited Create;
  FMinInterfaces := aInterfaces;
  SetLength(FInterfaces,aInterfaces);
  SetLength(FInterfaceRefs,aInterfaces);
end;

destructor TInterfaceOwner.Destroy;
begin
  ReleaseInterfaces;
  inherited Destroy;
end;

procedure TInterfaceOwner.AddObject(obj: TInterfacedObject);
var index: integer;
begin
  index := Length(FInterfaces);
  SetLength(FInterfaces,index+1);
  SetLength(FInterfaceRefs,index+1);
  AddInterface(index,obj);
end;

function TInterfaceOwner.GetInterface(index: integer): TInterfacedObject;
begin
  Result := FInterfaces[index];
end;

procedure TInterfaceOwner.SetRetainInterfaces(AValue: boolean);
begin
  if FRetainInterfaces = AValue then Exit;
  FRetainInterfaces := AValue;
  if not FRetainInterfaces then
    ReleaseInterfaces;
end;

function TInterfaceOwner.GetCount: integer;
begin
  Result := Length(FInterfaces);
end;

procedure TInterfaceOwner.AddInterface(index: integer; obj: TInterfacedObject);
begin
  FInterfaces[index] := obj;
  if RetainInterfaces then
    FInterfaceRefs[index] := obj;
end;

function TInterfaceOwner.HasInterface(index: integer): boolean;
begin
  Result := FInterfaces[index] <> nil;
end;

procedure TInterfaceOwner.Remove(intf: TInterfacedObject);
var i, j: integer;
begin
  for i := 0 to Length(FInterfaces) - 1 do
    if FInterfaces[i] = intf then
    begin
      if i < FMinInterfaces then
      begin
        FInterfaceRefs[i] := nil;
        FInterfaces[i] := nil;
      end
      else
      begin
        for j := i to Length(FInterfaces) - 2 do
        begin
          FInterfaceRefs[j] := FInterfaceRefs[j+1];
          FInterfaces[j] := FInterfaces[j+1];
        end;
        SetLength(FInterfaces,Length(FInterfaces)-1);
        SetLength(FInterfaceRefs,Length(FInterfaceRefs)-1);
      end;
      Exit;
    end;
end;

procedure TInterfaceOwner.ReleaseInterfaces;
var i: integer;
begin
  for i := 0 to Length(FInterfaces) - 1 do
    FInterfaceRefs[i] := nil;
end;

end.

