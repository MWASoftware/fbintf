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
  Classes, SysUtils, IB, FBActivityMonitor, IBExternals, FBClientAPI
  {$IFDEF WINDOWS}, Windows {$ENDIF};

const
  decimillsecondsPerSecond = 10000;
  TimeZoneDisplaymentDelta = 60*23 + 59; {23:59 in minutes}

{
The TSQLTimestamp and TSQLTimestampParam classes and corresponding interfaces provide
read/only and read/write access to columns and parameters, respectively, with the Date, Time,
Timestamp, Time with Time Zone and Timestamp with Time Zone types.

The ISQLData.GetAsDateTime and the ISQLParam.SetAsDateTime methods are still available for
simple use. However, the new interfaces provide more date/time formats and are the only way that
time zone data can be accessed.

Internally, Firebird stores date/time information as three 32-bit unsigned integers:

  • Date: In days from From 01.01.0001 AD to 31.12.9999 AD
  • Time: In deci-milliseconds from 0:00 to 23:59:59.9999
  • Time Zone:
     ◦ 0 to 2878 represent time zone offsets (in minutes) from -23:59 to 23:59
     ◦ Higher values identify time zones from the time zone database (see
RDB$TIME_ZONES).

Each Date/Time type stores its information in a different combination of the above.
TSQLTimestamp and TSQLTimestampParam internally store date/time information in Firebird
format.

Note that a time zone can be represented as either a displacement (in minutes) from GMT, or as a
time zone name (e.g. Europe/London). There is a subtle difference in the semantics of the two
representations. A displacement accurately reflections the different from GMT. However, it does not
identify the actual time zone, nor does it identify whether daylight savings time is in effect. On the
other hand, when a time zone name is provided, the time zone database can be used to determine
not just the displacement from GMT on any given date, but also whether daylight savings time is in
effect.

A displacement is simply the time difference in hours and minutes between the local time given by
the time value and GMT. For a timestamp (i.e. which includes the date) there is little to choose
between using a displacement and a time zone name. Both can be used to calculate the equivalent in
UTC. However, for a “Time with Time Zone” type, the time is devoid of any date information. It is
a local time – but on which day?

If your application needs to be able to translate a “Time with Time Zone” from a local time to GMT
and there is no context information otherwise available that can be used to determine the time zone,
then the time zone should be specified by a time zone name.
}

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
    function GetTimeZoneOffset(aEffectiveOffset: integer): AnsiString;
    class function GetDateFormatStr: AnsiString;
    class function GetTimeFormatStr: AnsiString;
  protected
    function InternalLocalTimeToUniversal(aDateTime: TDateTime): TDateTime;
    function DecodeTZOffset(offset: AnsiString; var aTimeZoneID: ISC_USHORT
      ): boolean;
    procedure UpdateTimeZoneInfo;
    function TimeZoneID2Name(aTimeZoneID: ISC_USHORT): AnsiString;
    function TimeZone2TimeZoneID(aTimeZone: AnsiString): ISC_USHORT;
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
    function GetAsString(IncludeTZifAvailable: boolean=true; ShowAsRegion: boolean = false): AnsiString;
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

uses FBMessages, StrUtils, DateUtils;

{ TSQLTimestamp }

function TSQLTimestamp.GetTimeZoneID(aTimeZone: AnsiString): ISC_USHORT;
var aTime: ISC_TIME_TZ;
begin
  if not DecodeTZOffset(aTimeZone,Result) then
    Result := TimeZone2TimeZoneID(aTimeZone);
end;

function TSQLTimestamp.GetTimeZoneOffset(aEffectiveOffset: integer): AnsiString;
begin
  if aEffectiveOffset > 0 then
    Result := Format('+%.2d:%.2d',[aEffectiveOffset div 60,abs(aEffectiveOffset mod 60)])
  else
    Result := Format('%.2d:%.2d',[aEffectiveOffset div 60,abs(aEffectiveOffset mod 60)]);
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
    Result := LongTimeFormat + '.zzzz'
  else
    Result := LongTimeFormat
end;

function TSQLTimestamp.InternalLocalTimeToUniversal(aDateTime: TDateTime): TDateTime;
begin
  {$IF declared(TTimeZone)}
  Result := TTimeZone.Local.ToUniversalTime(aDateTime);
  {$ELSE}
  {$IF declared(LocalTimeToUniversal)}
  Result := LocalTimeToUniversal(aDateTime);
  {$ELSE}
  Result := aDateTime; {No time zone support}
  {$IFEND}
  {$IFEND}
end;

function TSQLTimestamp.DecodeTZOffset(offset: AnsiString; var aTimeZoneID: ISC_USHORT
  ): boolean;
var i: integer;
    hour, mins: integer;
begin
  Result := false;
  i := Pos(':',offset);
  if i = 0 then
    Exit;

  if TryStrToInt(system.copy(offset,1,i-1),hour) and TryStrtoInt(system.copy(offset,i+1,MaxInt),mins) then
  begin
    aTimeZoneID := hour*60 + mins + TimeZoneDisplaymentDelta;
    Result := true;
  end;
end;

procedure TSQLTimestamp.UpdateTimeZoneInfo;
var ZoneOffset,DSTOffset: integer;
begin
  if FHasTimeZone then
  begin
    if FTimeZoneID < MaxOffsetTimeZoneID then
    begin
      FDSTStatus := dstUnknown;
      FEffectiveTimeOffsetMins := FTimeZoneID - TimeZoneDisplaymentDelta;
      FTimeZone := GetTimeZoneOffset(FEffectiveTimeOffsetMins);
    end
    else
    begin
      if FHasDatePart then
        GetTimeZoneInfo(FTimeZoneID,GetAsDate,FTimeZone,ZoneOffset,DSTOffset,FEffectiveTimeOffsetMins)
      else
        GetTimeZoneInfo(FTimeZoneID,InternalLocalTimeToUniversal(Now),FTimeZone,ZoneOffset,DSTOffset,FEffectiveTimeOffsetMins);
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

function TSQLTimestamp.TimeZone2TimeZoneID(aTimeZone: AnsiString): ISC_USHORT;
begin
  try
    Result := FAttachment.OpenCursorAtStart('Select RDB$TIME_ZONE_ID From RDB$TIME_ZONES Where RDB$TIME_ZONE_NAME = ?',
                           [aTimeZone])[0].AsInteger;
  except on E:EIBInterBaseError do
    IBError(ibxeBadTimeZoneName,[aTimeZone, E.IBErrorCode]);
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
  Clear;
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
  FEffectiveTimeOffsetMins := 0;
  FDSTStatus := dstUnknown;
end;

procedure TSQLTimestamp.FromSQLData(SQLType: cardinal; SQLData: PByte);
begin
  Clear;
  with FFirebirdClientAPI do
  case SQLType of
  SQL_TIMESTAMP_TZ:
    begin
      SQLDecodeDateTime(SQLData,FDate,FTime);
      FTimeZoneID := PISC_TIMESTAMP_TZ(SQLData)^.time_zone;
      FHasDatePart := true;
      FHasTimePart := true;
      FHasTimeZone := true;
      UpdateTimeZoneInfo;
      FTime := FTime + FEffectiveTimeOffsetMins * decimillsecondsPerSecond * 60; {adjust to local time}
    end;

  SQL_TIME_TZ:
    begin
      FTime := SQLDecodeTime(SQLData);
      FTimeZoneID := PISC_TIME_TZ(SQLData)^.time_zone;
      FDate := 0;
      FHasDatePart := false;
      FHasTimePart := true;
      FHasTimeZone := true;
      UpdateTimeZoneInfo;
      FTime := FTime + FEffectiveTimeOffsetMins * decimillsecondsPerSecond * 60; {adjust to local time}
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
  UpdateTimeZoneInfo;
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
var aDate, aTime: TDateTime;
begin
  aDate := FDate - DateDelta;
  aTime := FTime / (MSecsPerDay*10);
  if aDate < 0 then
    Result := trunc(aDate) - abs(frac(aTime))
  else
    Result := trunc(aDate) + abs(frac(aTime));
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
  Result.Time := FTime div 10; {deci-milliseconds to milliseconds}
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
    {$IFNDEF FPC}
    DecodeDateFully(TimeStampToDateTime(GetAsTimestamp),wYear,wMonth,wDay,wDayOfWeek);
    DecodeFBExtTime(FTime, wHour, wMinute, wSecond, DeciMillisecond);
    wMilliseconds := DeciMillisecond div 10;
    {$ELSE}
    DecodeDateFully(TimeStampToDateTime(GetAsTimestamp),Year,Month,Day,DayOfWeek);
    DecodeFBExtTime(FTime, Hour, Minute, Second, DeciMillisecond);
    Millisecond := DeciMillisecond div 10;
    {$ENDIF}
  end;
end;

function TSQLTimestamp.GetAsFBSystemTime: TFBSystemTime;
{Convert to Ext system time avoiding millisecond rounding issues}
begin
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
    {$IF declared(TTimeZone)}
    with TTimeZone.Local.GetUtcOffset(Now) do
    Result := Format('%.2d%.2d',[Trunc(Hours), Trunc(Minutes)]);
    {$ELSE}
    {$IF declared(GetLocalTimeOffset)}
    offset := GetLocalTimeOffset;
    Result := Format('%.2d%.2d',[offset div MinsPerHour, abs(offset) mod MinsPerHour]);
    {$ELSE}
    Result := ''; {No time zone support}
    {$IFEND}
    {$IFEND}
  end;
end;

function TSQLTimestamp.GetTimezoneID: ISC_USHORT;
begin
  if HasTimezone then
    Result := FTimeZoneID
  else
    IBError(ibxeTimeZoneUnknown,[]);
end;

function TSQLTimestamp.GetEffectiveTimeOffsetMins: integer;
begin
  Result := FEffectiveTimeOffsetMins;
end;

function TSQLTimestamp.GetAsString(IncludeTZifAvailable: boolean;
  ShowAsRegion: boolean): AnsiString;
var TimeFormat: AnsiString;
begin
  Result := '';
  if HasDatePart then
    Result := FormatDateTime(GetDateFormatStr,FDate - DateDelta) + ' ';

  if FHasTimePart then
  begin
    TimeFormat := GetTimeFormatStr;
    if Pos('zzzz',TimeFormat) > 1 then
      TimeFormat := ReplaceStr(TimeFormat,'zzzz',Format('%.4d',[FTime mod decimillsecondsPerSecond]))
    else
    if Pos('zzz',TimeFormat) > 1 then
      TimeFormat := ReplaceStr(TimeFormat,'zzz',Format('%.3d',[(FTime mod decimillsecondsPerSecond) div 10]))
    else
    if Pos('z',TimeFormat) > 1 then
      TimeFormat := ReplaceStr(TimeFormat,'z',Format('%d',[FTime mod decimillsecondsPerSecond]));
    Result := Result + FormatDateTime(TimeFormat,FTime / (MSecsPerDay*10));
  end;

  if IncludeTZifAvailable and HasTimeZone then
  begin
    if ShowAsRegion then
    begin
      if DSTStatus = dstUnknown then
        IBError(ibxeTZRegionNotAvailable,[FTimeZoneID])
      else
        Result := Result + ' ' + FTimeZone
    end
    else
    if FTimeZoneID < MaxOffsetTimeZoneID then
      Result := Result + ' ' + FTimeZone
    else
      Result := Result + ' ' + GetTimeZoneOffset(FEffectiveTimeOffsetMins);
  end;
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
     if not FHasTimeZone then
       IBError(ibxeTimeZoneUnknown,[])
     else
     begin
       SQLEncodeDateTime(FDate,FTime,SQLData);
       PISC_TIMESTAMP_TZ(SQLData)^.time_zone := FTimeZoneID;
     end;

   SQL_TIME_TZ:
     if not FHasTimeZone then
       IBError(ibxeTimeZoneUnknown,[])
     else
     begin
       SQLEncodeTime(FTime,SQLData);
        PISC_TIME_TZ(SQLData)^.time_zone := FTimeZoneID;
     end;

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
    {$IFNDEF FPC}
    FDate := Trunc(EncodeDate(wYear, wMonth, wDay)) + DateDelta;
    FTime := EncodeFBExtTime(wHour,wMinute,wSecond,wMilliseconds*10);
    {$ELSE}
    FDate := Trunc(EncodeDate(Year, Month, Day)) + DateDelta;
    FTime := EncodeFBExtTime(Hour,Minute,Second,Millisecond*10);
    {$ENDIF}
    FHasDatePart := true;
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
  FTimeZoneID := GetTimeZoneID(aValue);
  FHasTimezone := true;
  UpdateTimeZoneInfo;
end;

procedure TSQLTimestampParam.SetTimeZoneID(aTimeZoneID: ISC_USHORT);
var aTime: longint;
    aTimeTZ: ISC_TIME_TZ;
begin
  FTimeZoneID := aTimeZoneID;
  FHasTimeZone := true;
  UpdateTimeZoneInfo;
end;

end.

