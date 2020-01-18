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
unit FBTimestamp;

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
  Classes, SysUtils, IB, FBActivityMonitor, IBExternals, FBClientAPI;

const
  decimillsecondsPerSecond = 10000;
  MinTimeZoneDBID = 64000;

type

  { TSQLTimestamp }

  TSQLTimestamp = class(TFBInterfacedObject, ISQLTimestamp)
  private
    FAttachment: IAttachment;
    FFirebirdClientAPI: TFBClientAPI;
    FTime: longint;  {deci-milliseconds since midnight}
    FHasDatePart: boolean;
    FDate: longint; { One plus number of days since 1/1/0001 }
    FHasTimePart: boolean;
    FHasTimeZone: boolean;
    FTimeZone: AnsiString;
    FTimeZoneID: ISC_USHORT; {native Firebird timezone integer identifier}
    FDSTStatus: TDaylightSavingsTime;
    FEffectiveTimeOffsetMins: integer;
    function GetTimeZoneID(aTimeZone: AnsiString): ISC_USHORT; overload;
    class function GetDateFormatStr: AnsiString;
    class function GetTimeFormatStr: AnsiString;
  protected
    function DecodeTZOffset(offset: AnsiString): integer;
    procedure UpdateDSTStatus;
    function TimeZoneID2Name(aTimeZoneID: ISC_USHORT): AnsiString;
    procedure GetTimeZoneInfo(aTimeZoneID: ISC_USHORT; OnDate: TDateTime;
      var aTimeZone: AnsiString; var ZoneOffset: integer;
      var DSTOffset: integer; var EffectiveOffset: integer);
  public
    constructor Create(attachment: IAttachment);
    procedure FromSQLData(SQLType: cardinal; SQLData: PByte);
    class function GetDateTimeStrLength(DateTimeFormat: TIBDateTimeFormats): integer;
    procedure Clear;
  public
    {ISQLTimestamp}
    function GetAsDateTime: TDateTime;
    function GetAsDate: TDateTime;
    function GetAsTime: TDateTime;
    function GetAsTimestamp: TTimestamp;
    function GetAsMilliseconds: comp;
    function GetAsSystemTime: TSystemTime;
    function GetAsFBSystemTime: TFBSystemTime;
    function GetTimezone: AnsiString;
    function GetTimezoneID: ISC_USHORT; overload; {native Firebird timezone integer identifier}
    function GetEffectiveTimeOffsetMins: integer;
    function GetAsString(IncludeTZifAvailable: boolean=true): AnsiString;
    function GetDatePart: longint;
    function GetTimePart: longint;
    function HasDatePart: boolean;
    function HasTimePart: boolean;
    function HasTimezone: boolean;
    function DSTStatus: TDaylightSavingsTime;
  end;

  TSQLTimestampParam = class(TSQLTimestamp, ISQLParamTimestamp)
  public
    procedure ToSQLData(SQLType: cardinal; SQLData: PByte);
  public
    {ISQLParamTimestamp}
    procedure SetAsDateTime(aValue: TDateTime);
    procedure SetAsDate(aValue: TDateTime);
    procedure SetAsTime(aValue: TDateTime); overload;
    procedure SetAsTime(Hr, Mn, S, DeciMS: word); overload;
    procedure SetAsTimeMS(aValue: longint);
    procedure SetAsTimestamp(aValue: TTimestamp);
    procedure SetAsMilliseconds(aValue: comp);
    procedure SetAsSystemTime(aValue: TSystemTime);
    procedure SetAsFBSystemTime(aValue: TFBSystemTime);
    procedure SetDatePart(aValue: longint);
    procedure SetTimePart(aValue: longint);
    procedure SetTimezone(aValue: AnsiString);
    procedure SetTimeZoneID(aTimeZoneID: ISC_USHORT);
  end;

implementation

uses FBMessages, StrUtils;

{ TSQLTimestamp }

function TSQLTimestamp.GetTimeZoneID(aTimeZone: AnsiString): ISC_USHORT;
var aTime: ISC_TIME_TZ;
begin
  with FFirebirdClientAPI do
    SQLEncodeTimeTZ(0,aTimeZone,@aTime);
  Result := aTime.time_zone;
end;

class function TSQLTimestamp.GetDateFormatStr: AnsiString;
begin
  {$IF declared(DefaultFormatSettings)}
  with DefaultFormatSettings do
  {$ELSE}
  {$IF declared(FormatSettings)}
  with FormatSettings do
  {$IFEND}
  {$IFEND}
      result := ShortDateFormat;
end;

class function TSQLTimestamp.GetTimeFormatStr: AnsiString;
begin
  {$IF declared(DefaultFormatSettings)}
  with DefaultFormatSettings do
  {$ELSE}
  {$IF declared(FormatSettings)}
  with FormatSettings do
  {$IFEND}
  {$IFEND}
  if Pos('z',LongTimeFormat) = 0 then
    Result := LongTimeFormat + '.zzz'
  else
    Result := LongTimeFormat
end;

function TSQLTimestamp.DecodeTZOffset(offset: AnsiString): integer;
var i: integer;
begin
  i := Pos(':',offset);
  if i = 0 then
    IBError(ibxeTimeZoneOffsetSyntaxError,[offset]);

  try
    Result := StrToInt(system.copy(offset,1,i-1))*60 + StrtoInt(system.copy(offset,i+1,MaxInt));
  except on E: Exception do
    IBError(ibxeTimeZoneOffsetSyntaxError,[offset]);
  end;
end;

procedure TSQLTimestamp.UpdateDSTStatus;
var ZoneOffset,DSTOffset: integer;
    aTimeZone: AnsiString;
begin
  if FHasTimeZone then
  begin
    if FTimeZoneID < MinTimeZoneDBID then
    begin
      FDSTStatus := dstUnknown;
      FEffectiveTimeOffsetMins := DecodeTZOffset(FTimeZone);
    end
    else
    begin
      if FHasDatePart then
        GetTimeZoneInfo(FTimeZoneID,GetAsDate,aTimeZone,ZoneOffset,DSTOffset,FEffectiveTimeOffsetMins)
      else
        GetTimeZoneInfo(FTimeZoneID,Now,aTimeZone,ZoneOffset,DSTOffset,FEffectiveTimeOffsetMins);
      if DSTOffset = 0 then
        FDSTStatus := dstNotInEffect
      else
        FDSTStatus := dstInEffect;
    end;
  end;
end;

function TSQLTimestamp.TimeZoneID2Name(aTimeZoneID: ISC_USHORT): AnsiString;
begin
  try
    Result := FAttachment.OpenCursorAtStart(
                 'Select RDB$TIME_ZONE_NAME From RDB$TIME_ZONES Where RDB$TIME_ZONE_ID = ?',
                 [aTimeZoneID])[0].AsString;
  except on E:EIBInterBaseError do
    IBError(ibxeBadTimeZoneID,[aTimeZoneID, E.IBErrorCode]);
  end;
end;

procedure TSQLTimestamp.GetTimeZoneInfo(aTimeZoneID: ISC_USHORT;
  OnDate: TDateTime; var aTimeZone: AnsiString; var ZoneOffset: integer;
  var DSTOffset: integer; var EffectiveOffset: integer);
var Stmt: IStatement;
    TZInfo: IResultSet;
begin
  aTimeZone := TimeZoneID2Name(aTimeZoneID);
  with FAttachment do
    Stmt := Prepare(StartTransaction([isc_tpb_read,isc_tpb_wait,isc_tpb_concurrency],taCommit),
                 'select * from rdb$time_zone_util.transitions(?,?,?)');
  Stmt.SQLParams[0].AsString := aTimeZone;
  Stmt.SQLParams[1].AsDateTime := OnDate;
  Stmt.SQLParams[2].AsDateTime := OnDate;
  TZInfo := Stmt.OpenCursor;
  if TZInfo.FetchNext then
  begin
    ZoneOffset := TZInfo.ByName('ZONE_OFFSET').AsInteger;
    DSTOffset := TZInfo.ByName('DST_OFFSET').AsInteger;
    EffectiveOffset := TZInfo.ByName('EFFECTIVE_OFFSET').AsInteger;
  end;
end;

constructor TSQLTimestamp.Create(attachment: IAttachment);
begin
  inherited Create;
  FAttachment := attachment;
  FFirebirdClientAPI := (attachment.getFirebirdAPI as TFBClientAPI);
end;

procedure TSQLTimestamp.Clear;
begin
  FDate := 0;
  FTime := 0;
  FHasDatePart := false;
  FHasTimePart := false;
  FHasTimeZone := false;
  FTimeZone := '';
  FTimeZoneID := 0;
end;

procedure TSQLTimestamp.FromSQLData(SQLType: cardinal; SQLData: PByte);
begin
  Clear;
  with FFirebirdClientAPI do
  case SQLType of
  SQL_TIMESTAMP_TZ:
    begin
      SQLDecodeTimeStampTZ(FDate,FTime,FTimeZone,FTimeZoneID,SQLData);
      FHasDatePart := true;
      FHasTimePart := true;
      FHasTimeZone := true;
    end;

  SQL_TIME_TZ:
    begin
      SQLDecodeTimeTZ(FTime,FTimeZone,FTimeZoneID,SQLData);
      FDate := 0;
      FHasDatePart := false;
      FHasTimePart := true;
      FHasTimeZone := true;
    end;

  SQL_TIMESTAMP:
    begin
      SQLDecodeDateTime(SQLData,FDate,FTime);
      FHasDatePart := true;
      FHasTimePart := true;
      FHasTimeZone := false;
    end;

  SQL_TYPE_DATE:
    begin
      FDate := SQLDecodeDate(SQLData);
      FTime := 0;
      FHasDatePart := true;
      FHasTimePart := false;
      FHasTimeZone := false;
    end;

  SQL_TYPE_TIME:
    begin
      FDate := 0;
      FTime := SQLDecodeTime(SQLData);
      FHasDatePart := false;
      FHasTimePart := true;
      FHasTimeZone := false;
    end;

  else
    IBError(ibxeInvalidDataConversion, [nil]);
  end;
  UpdateDSTStatus;
end;

class function TSQLTimestamp.GetDateTimeStrLength(
  DateTimeFormat: TIBDateTimeFormats): integer;
begin
  case DateTimeFormat of
  dfTimestamp:
    Result := Length(GetDateFormatStr) + 1 + Length(GetTimeFormatStr);
  dfDateTime:
    Result := Length(GetDateFormatStr);
  dfTime:
    Result := Length(GetTimeFormatStr);
  dfTimestampTZ:
    Result := Length(GetDateFormatStr) + 1 + Length(GetTimeFormatStr) + 6 {Time offset length};
  dfTimeTZ:
    Result := Length(GetTimeFormatStr) + 6 {Time offset length};
  else
    Result := 0;
  end;
end;

function TSQLTimestamp.GetAsDateTime: TDateTime;
begin
  Result := ComposeDateTime(FDate - DateDelta,FTime / (MSecsPerDay*10))
end;

function TSQLTimestamp.GetAsDate: TDateTime;
begin
  Result := FDate - DateDelta;
end;

function TSQLTimestamp.GetAsTime: TDateTime;
begin
  Result := FTime / (MSecsPerDay*10);
end;

function TSQLTimestamp.GetAsTimestamp: TTimestamp;
begin
  Result.Date := FDate;
  Result.Time := FTime div 10;
end;

function TSQLTimestamp.GetAsMilliseconds: comp;
begin
  Result := TimeStampToMSecs(GetAsTimestamp);
end;

function TSQLTimestamp.GetAsSystemTime: TSystemTime;
{Convert to system time (broken time) avoiding millisecond rounding issues}
var DeciMillisecond: word;
begin
  with FFirebirdClientAPI, Result do
  begin
    DecodeDateFully(TimeStampToDateTime(GetAsTimestamp),Year,Month,Day,DayOfWeek);
    DecodeFBExtTime(FTime, Hour, Minute, Second, DeciMillisecond);
    Millisecond := DeciMillisecond div 10;
  end;
end;

function TSQLTimestamp.GetAsFBSystemTime: TFBSystemTime;
{Convert to Ext system time avoiding millisecond rounding issues}
var aTime: word;
begin
  aTime := FTime;
  with FFirebirdClientAPI, Result do
  begin
    DecodeDateFully(TimeStampToDateTime(GetAsTimestamp),Year,Month,Day,DayOfWeek);
    DecodeFBExtTime(FTime, Hour, Minute, Second, DeciMillisecond);
  end;
end;

function TSQLTimestamp.GetTimezone: AnsiString;
var offset: integer;
begin
  if HasTimezone then
    Result := FTimezone
  else
  begin
    offset := GetLocalTimeOffset;
    Result := Format('%.2d%.2d',[offset div MinsPerHour, abs(offset) mod MinsPerHour]);
  end;
end;

function TSQLTimestamp.GetTimezoneID: ISC_USHORT;
begin
  if HasTimezone then
    Result := FTimeZoneID
  else
    Result := 0;
end;

function TSQLTimestamp.GetEffectiveTimeOffsetMins: integer;
begin
  Result := FEffectiveTimeOffsetMins;
end;

function TSQLTimestamp.GetAsString(IncludeTZifAvailable: boolean): AnsiString;
var TimeFormat: AnsiString;
begin
  Result := '';
  if HasDatePart then
    Result := FormatDateTime(GetDateFormatStr,FDate - DateDelta) + ' ';
  if FHasTimePart then
  begin
    TimeFormat := GetTimeFormatStr;
    if Pos('zzz',TimeFormat) > 1 then
      TimeFormat := ReplaceStr(TimeFormat,'zzz',Format('%.4d',[FTime mod decimillsecondsPerSecond]));
    if Pos('z',TimeFormat) > 1 then
      TimeFormat := ReplaceStr(TimeFormat,'z',Format('%d',[FTime mod decimillsecondsPerSecond]));
    Result := Result + FormatDateTime(TimeFormat,FTime / (MSecsPerDay*10));
  end;
  if IncludeTZifAvailable and HasTimeZone then
    Result := Result + ' ' + FTimeZone
end;

function TSQLTimestamp.GetDatePart: longint;
begin
  if FHasDatePart then
    Result := FDate
  else
    Result := 0;
end;

function TSQLTimestamp.GetTimePart: longint;
begin
  if FHasTimePart then
    Result := FTime
  else
    Result := 0;
end;

function TSQLTimestamp.HasDatePart: boolean;
begin
  Result := FHasDatePart;
end;

function TSQLTimestamp.HasTimePart: boolean;
begin
  Result := FHasTimePart;
end;

function TSQLTimestamp.HasTimezone: boolean;
begin
  Result := FHasTimeZone;
end;

function TSQLTimestamp.DSTStatus: TDaylightSavingsTime;
begin
  Result := FDSTStatus;
end;

{TSQLTimestampParam}

procedure TSQLTimestampParam.ToSQLData(SQLType: cardinal; SQLData: PByte);
begin
   with FFirebirdClientAPI do
   case SQLType of
   SQL_TIMESTAMP_TZ:
     SQLEncodeTimeStampTZ(FDate,FTime,GetTimeZone,SQLData);

   SQL_TIME_TZ:
     SQLEncodeTimeTZ(FTime,GetTimeZone,SQLData);

   SQL_TIMESTAMP:
     SQLEncodeDateTime(FDate,FTime,SQLData);

   SQL_TYPE_DATE:
      SQLEncodeDate(FDate,SQLData);

   SQL_TYPE_TIME:
     SQLEncodeTime(FTime,SQLData);

   else
     IBError(ibxeInvalidDataConversion, [nil]);
   end;
end;

procedure TSQLTimestampParam.SetAsDateTime(aValue: TDateTime);
Var
  D : Double;
begin
  Clear;
  {copied from DateTimeToTimeStamp and adjusted for deci-milliseconds}
  D:=aValue * MSecsPerDay*10; {Convert to deci-milliseconds}
  if D<0 then {round up}
    D:=D-0.5
  else
    D:=D+0.5;
  FTime := Abs(Trunc(D)) Mod (MSecsPerDay*10);
  FDate := DateDelta + Trunc(D) div (MSecsPerDay*10);
  FHasDatePart := true;
  FHasTimePart := true;
end;

procedure TSQLTimestampParam.SetAsDate(aValue: TDateTime);
begin
  Clear;
  FDate := Trunc(aValue) + DateDelta;
  FHasDatePart := true;
end;

procedure TSQLTimestampParam.SetAsTime(aValue: TDateTime);
Var
  D : Double;
begin
  Clear;
  {copied from DateTimeToTimeStamp and adjusted for deci-milliseconds}
  D:=aValue * MSecsPerDay*10; {Convert to deci-milliseconds}
  if D<0 then {round up}
    D:=D-0.5
  else
    D:=D+0.5;
  FTime := Trunc(D);
  FHasTimePart := true;
end;

procedure TSQLTimestampParam.SetAsTime(Hr, Mn, S, DeciMS: word);
begin
  Clear;
  with FFirebirdClientAPI do
    FTime := EncodeFBExtTime(Hr, Mn, S, DeciMS);
  FHasTimePart := true;
end;

procedure TSQLTimestampParam.SetAsTimeMS(aValue: longint);
begin
  Clear;
  FTime := aValue * 10;
  FHasTimePart := true;
end;

procedure TSQLTimestampParam.SetAsTimestamp(aValue: TTimestamp);
begin
  Clear;
  FDate := aValue.date;
  FTime := AValue.time*10;
  FHasDatePart := true;
  FHasTimePart := true;
end;

procedure TSQLTimestampParam.SetAsMilliseconds(aValue: comp);
begin
  SetAsTimestamp(MSecsToTimeStamp(aValue));
end;

procedure TSQLTimestampParam.SetAsSystemTime(aValue: TSystemTime);
begin
  Clear;
  with FFirebirdClientAPI, aValue do
  begin
    FDate := Trunc(EncodeDate(Year, Month, Day)) + DateDelta;
    FHasDatePart := true;
    FTime := EncodeFBExtTime(Hour,Minute,Second,Millisecond*10);
    FHasTimePart := true;
  end;
end;

procedure TSQLTimestampParam.SetAsFBSystemTime(aValue: TFBSystemTime);
begin
  Clear;
  with FFirebirdClientAPI, aValue do
  begin
    FDate := Trunc(EncodeDate(Year, Month, Day)) + DateDelta;
    FHasDatePart := true;
    FTime := EncodeFBExtTime(Hour,Minute,Second,DeciMilliSecond);
    FHasTimePart := true;
    FHasTimeZone := (TimeZone <> '') and HasTimeZoneSupport;
    if FHasTimeZone then
    begin
      FTimeZone := TimeZone;
      FTimeZoneID := GetTimeZoneID(FTimeZone);
    end;
  end;
end;

procedure TSQLTimestampParam.SetDatePart(aValue: longint);
begin
  FDate := aValue;
  FHasDatePart := true;
end;

procedure TSQLTimestampParam.SetTimePart(aValue: longint);
begin
  FTime := aValue;
  FHasTimePart := true;
end;

procedure TSQLTimestampParam.SetTimezone(aValue: AnsiString);
begin
  FTimezone := aValue;
  FHasTimezone := true;
  FTimeZoneID := GetTimeZoneID(FTimeZone);
end;

procedure TSQLTimestampParam.SetTimeZoneID(aTimeZoneID: ISC_USHORT);
var aTime: longint;
    aTimeTZ: ISC_TIME_TZ;
begin
  FHasTimeZone := true;
  aTimeTZ.time_zone := aTimeZoneID;
  aTimeTZ.utc_time := 0;
  FFirebirdClientAPI.SQLDecodeTimeTZ(aTime,FTimeZone,FTimeZoneID,@aTimeTZ);
end;

end.

