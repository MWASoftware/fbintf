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

type

  { TSQLTimestamp }

  TSQLTimestamp = class(TFBInterfacedObject, ISQLTimestamp, ISQLParamTimestamp)
  private
    FFirebirdClientAPI: TFBClientAPI;
    FTime: longint;  {deci-milliseconds since midnight}
    FHasDatePart: boolean;
    FDate: longint; { One plus number of days since 1/1/0001 }
    FHasTimePart: boolean;
    FHasTimeZone: boolean;
    FTimeZone: AnsiString;
    FTimeZoneID: ISC_USHORT; {native Firebird timezone integer identifier}
    function GetTimeZoneID(aTimeZone: AnsiString): ISC_USHORT; overload;
    class function GetDateFormatStr: AnsiString;
    class function GetTimeFormatStr: AnsiString;
  public
    constructor Create(api: TFBClientAPI);
    procedure ToSQLData(SQLType: cardinal; SQLData: PByte);
    procedure FromSQLData(SQLType: cardinal; SQLData: PByte);
    class function GetDateTimeStrLength(DateTimeFormat: TIBDateTimeFormats): integer;
  public
    {ISQLTimestamp}
    function GetAsDateTime: TDateTime;
    function GetAsTimestamp: TTimestamp;
    function GetAsMilliseconds: comp;
    function GetAsSystemTime: TSystemTime;
    function GetAsFBSystemTime: TFBSystemTime;
    function GetTimezone: AnsiString;
    function GetTimezoneID: ISC_USHORT; overload; {native Firebird timezone integer identifier}
    function GetAsString(IncludeTZifAvailable: boolean=true): AnsiString;
    function GetDatePart: longint;
    function GetTimePart: longint;
    function HasDatePart: boolean;
    function HasTimePart: boolean;
    function HasTimezone: boolean;
    {ISQLParamTimestamp}
    procedure Clear;
    procedure SetAsDateTime(aValue: TDateTime);
    procedure SetAsTime(aValue: TDateTime); overload;
    procedure SetAsTimeMS(aValue: longint); overload;
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

constructor TSQLTimestamp.Create(api: TFBClientAPI);
begin
  inherited Create;
  FFirebirdClientAPI := api;
end;

procedure TSQLTimestamp.ToSQLData(SQLType: cardinal; SQLData: PByte);
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
  Result := GetTimeZoneID(FTimeZone);
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

procedure TSQLTimestamp.SetAsDateTime(aValue: TDateTime);
Var
  D : Double;
begin
  {copied from DateTimeToTimeStamp and adjusted for deci-milliseconds}
  D:=aValue * Single(MSecsPerDay*10); {Convert to deci-milliseconds}
  if D<0 then {round up}
    D:=D-0.5
  else
    D:=D+0.5;
  FTime := Abs(Trunc(D)) Mod (MSecsPerDay*10);
  FDate := DateDelta + Trunc(D) div (MSecsPerDay*10);
  FHasDatePart := true;
  FHasTimePart := true;
end;

procedure TSQLTimestamp.SetAsTime(aValue: TDateTime);
begin
  SetAsTimestamp(DateTimeToTimeStamp(AValue));
  FDate := 0;
  FHasDatePart := false;
  FHasTimePart := true;
end;

procedure TSQLTimestamp.SetAsTimeMS(aValue: longint);
begin
  SetAsTimestamp(MSecsToTimeStamp(aValue));
  FDate := 0;
  FHasDatePart := false;
  FHasTimePart := true;
end;

procedure TSQLTimestamp.SetAsTimestamp(aValue: TTimestamp);
begin
  FDate := aValue.date;
  FTime := AValue.time*10;
  FHasTimeZone := false;
  FTimeZone := '';
  FTimeZoneID := 0;
  FHasDatePart := true;
  FHasTimePart := true;
end;

procedure TSQLTimestamp.SetAsMilliseconds(aValue: comp);
begin
  SetAsTimestamp(MSecsToTimeStamp(aValue));
  FHasDatePart := true;
  FHasTimePart := true;
end;

procedure TSQLTimestamp.SetAsSystemTime(aValue: TSystemTime);
begin
  with FFirebirdClientAPI, aValue do
  begin
    FDate := Trunc(EncodeDate(Year, Month, Day)) + DateDelta;
    FHasDatePart := true;
    FTime := EncodeFBExtTime(Hour,Minute,Second,Millisecond*10);
    FHasTimePart := true;
    FHasTimeZone := false;
  end;
end;

procedure TSQLTimestamp.SetAsFBSystemTime(aValue: TFBSystemTime);
begin
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

procedure TSQLTimestamp.SetDatePart(aValue: longint);
begin
  FDate := aValue;
  FHasDatePart := true;
end;

procedure TSQLTimestamp.SetTimePart(aValue: longint);
begin
  FTime := aValue;
  FHasTimePart := true;
end;

procedure TSQLTimestamp.SetTimezone(aValue: AnsiString);
begin
  FTimezone := aValue;
  FHasTimezone := true;
  FTimeZoneID := GetTimeZoneID(FTimeZone);
end;

procedure TSQLTimestamp.SetTimeZoneID(aTimeZoneID: ISC_USHORT);
var aTime: longint;
    aTimeTZ: ISC_TIME_TZ;
begin
  aTimeTZ.time_zone := aTimeZoneID;
  aTimeTZ.utc_time := 0;
  FFirebirdClientAPI.SQLDecodeTimeTZ(aTime,FTimeZone,FTimeZoneID,@aTimeTZ);
end;

end.

