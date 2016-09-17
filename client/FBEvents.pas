unit FBEvents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBClientAPI, syncobjs, FBActivityMonitor;

type

  { TFBEvents }

  TFBEvents = class(TActivityReporter)
  private
    FEvents: TStringList;
    FAttachment: IAttachment;
  protected
    FEventBuffer: PChar;
    FEventBufferLen: integer;
    FResultBuffer: PChar;
    FEventHandler: TEventHandler;
    FCriticalSection: TCriticalSection;
    FInWaitState: boolean;
    procedure CreateEventBlock;
    procedure CancelEvents(Force: boolean = false); virtual;
    procedure EventSignaled;
    function GetIEvents: IEvents; virtual; abstract;
  public
    constructor Create(DBAttachment: IAttachment; aMonitor: IActivityMonitor; Events: TStrings);
    destructor Destroy; override;

    {IEvents}
    procedure GetEvents(EventNames: TStrings);
    procedure SetEvents(EventNames: TStrings); overload;
    procedure SetEvents(Event: string); overload;
    procedure Cancel;
    function ExtractEventCounts: TEventCounts;
    function GetAttachment: IAttachment;
  end;


implementation

uses FBMessages;

const
  MaxEvents = 15;

{ TFBEvents }

procedure TFBEvents.CreateEventBlock;
var
  i: integer;
  EventNames: array of PChar;
begin
  with FirebirdClientAPI do
  begin
    if FEventBuffer <> nil then
      isc_free( FEventBuffer);
    FEventBuffer := nil;
    if FResultBuffer <> nil then
      isc_free( FResultBuffer);
    FResultBuffer := nil;

    setlength(EventNames,MaxEvents);
    try
      for i := 0 to FEvents.Count-1 do
        EventNames[i] := PChar(FEvents[i]);

      FEventBufferlen := isc_event_block(@FEventBuffer,@FResultBuffer,
                          FEvents.Count,
                          EventNames[0],EventNames[1],EventNames[2],
                          EventNames[3],EventNames[4],EventNames[5],
                          EventNames[6],EventNames[7],EventNames[8],
                          EventNames[9],EventNames[10],EventNames[11],
                          EventNames[12],EventNames[13],EventNames[14]
                          );
    finally
      SetLength(EventNames,0)
    end;
  end;
end;

procedure TFBEvents.CancelEvents(Force: boolean);
begin
  FEventHandler := nil;
end;

procedure TFBEvents.EventSignaled;
var Handler: TEventHandler;
begin
  FCriticalSection.Enter;
  try
    if not FInWaitState then Exit;
    FInWaitState := false;
    if assigned(FEventHandler)  then
    begin
      Handler := FEventHandler;
      FEventHandler := nil;
      Handler(GetIEvents);
    end;
  finally
    FCriticalSection.Leave
  end;
end;

constructor TFBEvents.Create(DBAttachment: IAttachment;
  aMonitor: IActivityMonitor; Events: TStrings);
begin
  inherited Create(aMonitor);
  FAttachment := DBAttachment;
  if Events.Count > MaxEvents then
    IBError(ibxeMaximumEvents, [nil]);

  FCriticalSection := TCriticalSection.Create;
  FEvents := TStringList.Create;
  FEvents.Assign(Events);
  CreateEventBlock;
end;

destructor TFBEvents.Destroy;
begin
  if assigned(FCriticalSection) then FCriticalSection.Free;
  if assigned(FEvents) then FEvents.Free;
  with FirebirdClientAPI do
  begin
    if FEventBuffer <> nil then
      isc_free( FEventBuffer);
    if FResultBuffer <> nil then
      isc_free( FResultBuffer);
  end;
  inherited Destroy;
end;

procedure TFBEvents.GetEvents(EventNames: TStrings);
begin
  EventNames.Assign(FEvents)
end;

procedure TFBEvents.SetEvents(EventNames: TStrings);
begin
  if EventNames.Text <> FEvents.Text then
  begin
    Cancel;
    FEvents.Assign(EventNames);
    CreateEventBlock;
  end;
end;

procedure TFBEvents.SetEvents(Event: string);
var S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Add(Event);
    SetEvents(S);
  finally
    S.Free;
  end;
end;

procedure TFBEvents.Cancel;
begin
  if assigned(FEventHandler) then
    CancelEvents;
end;

function TFBEvents.ExtractEventCounts: TEventCounts;
var EventCountList: TStatusVector;
    i: integer;
    j: integer;
begin
  SetLength(Result,0);
  if FResultBuffer = nil then Exit;

  with FirebirdClientAPI do
     isc_event_counts( @EventCountList, FEventBufferLen, FEventBuffer, FResultBuffer);
  j := 0;
  for i := 0 to FEvents.Count - 1 do
  begin
    if EventCountList[i] > 0 then
    begin
      Inc(j);
      SetLength(Result,j);
      Result[j-1].EventName := FEvents[i];
      Result[j-1].Count := EventCountList[i];
    end;
  end;
end;

function TFBEvents.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

end.
