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
    FHasTimeZone: boolean;
    FTimeZone: AnsiString;
    FTimeZoneID: ISC_SHORT; {native Firebird timezone integer identifier}
    function GetTimeZoneID(aTimeZone: AnsiString): ISC_USHORT; overload;
  public
    constructor Create(api: TFBClientAPI);
    procedure ToSQLData(SQLType: cardinal; SQLData: PByte);
    procedure FromSQLData(SQLType: cardinal; SQLData: PByte);
    function TrySetAsString(aValue: AnsiString): boolean;
  public
    {ISQLTimestamp}
    function GetAsDateTime: TDateTime;
    function GetAsTimestamp: TTimestamp;
    function GetAsMilliseconds: comp;
    function GetAsSystemTime: TSystemTime;
    function GetAsExtSystemTime: TExtSystemTime;
    function GetTimezone: AnsiString;
    function GetTimezoneID: ISC_USHORT; overload; {native Firebird timezone integer identifier}
    function GetAsString(FormatSettings: TFormatSettings): AnsiString; overload;
    function GetAsString: AnsiString; overload;
    function HasDatePart: boolean;
    function HasTimezone: boolean;
    {ISQLParamTimestamp}
    procedure SetAsDateTime(aValue: TDateTime);
    procedure SetAsTime(aValue: TDateTime); overload;
    procedure SetAsTimeMS(aValue: longint); overload;
    procedure SetAsTimestamp(aValue: TTimestamp);
    procedure SetAsMilliseconds(aValue: comp);
    procedure SetAsSystemTime(aValue: TSystemTime);
    procedure SetAsExtSystemTime(aValue: TFBSystemTime);
    procedure SetTimezone(aValue: AnsiString);
    procedure SetAsString(aValue: AnsiString);
  end;

implementation

uses FBMessages;

{ TSQLTimestamp }

function TSQLTimestamp.GetTimeZoneID(aTimeZone: AnsiString): ISC_USHORT;
var aTime: ISC_TIME_TZ;
begin
  if SupportsTimeZone then
  with FFirebirdClientAPI do
  begin
    UtilIntf.encodeTimeTz(StatusIntf,@aTime,0,0,0,0, @aTimeZone);
    Check4DataBaseError;
    Result := aTime.time_zone;
  end
  else
    IBError(ibxeNotSupported,[]);
end;

constructor TSQLTimestamp.Create(api: TFBClientAPI);
begin
  inherited Create;
  FFirebirdClientAPI := api;
end;

procedure TSQLTimestamp.ToSQLData(SQLType: cardinal; SQLData: PByte);
var aTimeZone: AnsiString;
begin
   aTimeZone := GetTimeZone;
   with FFirebirdClientAPI, GetAsSystemTime do
   case SQLType of
   SQL_TIMESTAMP_TZ:
     if SupportsTimeZone then
     with (FFirebirdClientAPI as TFB30ClientAPI) do
     begin
       UtilIntf.encodeTimeStampTz(StatusIntf,
          ISC_TIMESTAMP_TZPtr(SQLData),Year, Month, Day, Hour, Minute, Second, DeciMilliSecond,
          @aTimeZone);
       Check4DataBaseError;
     end
   else
     IBError(ibxeNotSupported,[]);

   SQL_TIME_TZ:;
     if SupportsTimeZone then
     with (FFirebirdClientAPI as TFB30ClientAPI) do
     begin
       UtilIntf.encodeTimeTz(StatusIntf,
          ISC_TIME_TZPtr(SQLData),Hour,Minute, Second, DeciMilliSecond, @aTimeZone);
       Check4DataBaseError;
     end
     else
       IBError(ibxeNotSupported,[]);

   SQL_TIMESTAMP:
     SQLEncodeDateTime(FDate,FTime,SQLData);

   SQL_TYPE_DATE:
      SQLEncodeDate(FDate,SQLData);

   SQL_TYPE_TIME:
     SQLEncodeTime(FTime,SQLData);
   end;
end;

procedure TSQLTimestamp.FromSQLData(SQLType: cardinal; SQLData: PByte);
begin

end;

function TSQLTimestamp.TrySetAsString(aValue: AnsiString): boolean;
var Parts: TStringList;
    i: integer;
    TmpTimeStamp: ISC_TIMESTAMP_TZ;
    aTimestamp: TTimestamp;
    aTimeZone: AnsiString;
    aTimezoneID : ISC_SHORT;
    aHasDatePart: boolean;
    aHasTimeZone: boolean;
    datePart: TDateTime;
    timePart: TDateTime;

    function LTrim(S: AnsiString): AnsiString;
    begin
      while (S <> '') and (S[1] = ' ') do
        system.Delete(S,1,1);
    end;

begin
  Result := true;
  aTimestamp.date := 0;
  aTimestamp.Time := 0;
  aHasDatePart := false;
  aHasTimeZone := false;
  aTimeZone := '';
  aTimeZoneID := 0;

  Parts := TStringList.Create;
  try
    i := 0;
    repeat
      aValue := LTrim(aValue);
      i := AnsiPos(' ',aValue);
      if i > 0 then
      begin
        Parts.Add(system.copy(aValue,1,i-1));
        system.Delete(aValue,1,i);
      end;
    until i = 0;
    Parts.Add(aValue);

    with FFirebirdClientAPI do
    case Parts.Count of
    1:
      {date or time part only}
      begin
        Result := TryStrToDate(Parts[0],datePart);
        if  Result then
        begin
          aHasDatePart := true;
          aTimestamp.Date := Trunc(datePart);
        end
        else
        begin
          Result := TryStrToTime(Parts[0],timepart);
          if Result then
            aTimestamp.Time := timepart * MSecsPerDay;
        end;
      end;

    2:
      {date and time or time and TZ}
      if TryStrToDate(Parts[0],datePart) and TryStrToTime(Parts[1],timePart) then
      begin
        aHasDatePart := true;
        aTimestamp.Date := Trunc(datePart);
        aTimestamp.Time := timepart * MSecsPerDay;
      end
      else
      begin
        Result :=  TryStrToTime(Parts[0],timepart) and SupportsTimeZone;
        if Result then
        begin
          {Let Firebird validate the time zone part}
          Result := TrySQLEncodeTimeTZ(timepart,Parts[1],@TimeStamp);
          if Result then
          begin
            aHasTimeZone := true;
            aTimeZone := Parts[1];
            aTimeZoneID := TimeStamp.time_zone;
          end;
        end;
      end;

    3:
      {date, time and TZ}
      begin
        Result := TryStrToDate(Parts[0],datepart) and TryStrToTime(Parts[1],timePart) and SupportsTimeZone;
        if Result then
        begin
          aHasDatePart := true;
          aTimestamp.Date := Trunc(datePart);
          aTimestamp.Time := timepart * MSecsPerDay;
          {Let Firebird validate the time zone part}
          Result := TrySQLEncodeTimestampTZ(timepart,Parts[2],@TimeStamp);
          if Result then
          begin
            aHasTimeZone := true;
            aTimeZone := Parts[2];
            aTimeZoneID := TimeStamp.time_zone;
          end;
        end;
      end;

    else
      Result := false;
    end;
  finally
    Parts.Free;
  end;
  if Result then
  begin
    SetAsTimestamp(aTimestamp);
    FHasDatePart := aHasDatePart;
    FTimestamp.TimeZone := aTimeZone;
    FTimestamp.HasTimeZone := aHasTimeZone;
    FTimestamp.TimezoneID := aTimezoneID;
  end;
end;

function TSQLTimestamp.GetAsDateTime: TDateTime;
begin
  Result := TimeStampToDateTime(GetAsTimestamp);
end;

function TSQLTimestamp.GetAsTimestamp: TTimestamp;
begin
  Result.Date := FDate;
  Result.Time := FTime div 10;
end;

function TSQLTimestamp.GetAsMilliseconds: comp;
begin
  Result := TimeStampToMSecs(GetTimestamp);
end;

function TSQLTimestamp.GetAsSystemTime: TSystemTime;
{Convert to system time (broken time) avoiding millisecond rounding issues}
var DeciMillisecond: word;
begin
  aTime := FTime;
  with Result do
  begin
    DecodeDateFully(TimeStampToDateTime(GetTimestamp),Year,Month,Day,DayOfWeek);
    DecodeFBExtTime(FTime, Hour, Minute, Second, DeciMillisecond);
    Millisecond := DeciMillisecond div 10;
  end;
end;

function TSQLTimestamp.GetAsExtSystemTime: TExtSystemTime;
{Convert to Ext system time avoiding millisecond rounding issues}
var aTime: word;
begin
  aTime := FTime;
  with Result do
  begin
    DecodeDateFully(TimeStampToDateTime(GetTimestamp),Year,Month,Day,DayOfWeek);
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
    Result := Format('%.2d%.2d',offset div MinsPerHour, offset mod MinsPerHour);
  end;
end;

function TSQLTimestamp.GetTimezoneID: ISC_USHORT;
begin
  Result := GetTimeZoneID(FTimeZone);
end;

{Extends FormatSettings.LongTimeFormat to add TZ as a placeholder for the TimeZone}

function TSQLTimestamp.GetAsString(FormatSettings: TFormatSettings): AnsiString;
var TimeFormat: AnsiString;
begin
  Result := '';
  if HasDatePart then
    Result := DateToStr(GetAsDateTime,FormatSettings) + ' ';
  TimeFormat := FormatSettings.LongTimeFormat;
  if Pos('zzz',TimeFormat) > 1 then
    TimeFormat := ReplaceStr(TimeFormat,'zzz',Format('%.4d',FTimestamp.time mod 10000))
  if Pos('z',TimeFormat) > 1 then
    TimeFormat := ReplaceStr(TimeFormat,'z',Format('%d',FTimestamp.time mod 10000));
  if Pos('TZ', TimeFormat) > 1 then
  begin
    if HasTimeZone then
      ReplaceStr(TimeFormat,'TZ',TimeZone)
    else
      ReplaceStr(TimeFormat,'TZ','');
  end;
  Result := Result + FormatDateTime(TimeFormat,(FTimeStamp.Time div 10000) * 1000) / MSecsPerDay);
end;

function TSQLTimestamp.GetAsString: AnsiString;
var FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.LongTimeFormat := ' TZ';
  Result := GetAsString(FormatSettings);
end;

function TSQLTimestamp.HasDatePart: boolean;
begin
  Result := FHasDatePart;
end;

function TSQLTimestamp.HasTimezone: boolean;
begin
  Result := FTimestamp.HasTimeZone;
end;

procedure TSQLTimestamp.SetAsDateTime(aValue: TDateTime);
begin
  SetAsTimestamp(DateTimeToTimeStamp(AValue));
  FHasDatePart := true;
end;

procedure TSQLTimestamp.SetAsTime(aValue: TDateTime);
begin
  SetAsTimestamp(DateTimeToTimeStamp(AValue));
  FTimestamp.Date := 0;
  FHasDatePart := false;
end;

procedure TSQLTimestamp.SetAsTimeMS(aValue: longint);
begin
  SetAsTimestamp(MSecsToTimeStamp(aValue));
  FTimestamp.Date := 0;
  FHasDatePart := false;
end;

procedure TSQLTimestamp.SetAsTimestamp(aValue: TTimestamp);
begin
  FDate := aValue.date;
  FTime := AValue.time*10;
  FHasTimeZone := false;
  FTimeZone := '';
  FTimeZoneID := 0;
  FHasDatePart := true;
end;

procedure TSQLTimestamp.SetAsMilliseconds(aValue: comp);
begin
  SetAsTimestamp(MSecsToTimeStamp(aValue));
  FHasDatePart := true;
end;

procedure TSQLTimestamp.SetAsSystemTime(aValue: TSystemTime);
begin
  with aValue do
  begin
    FDate := Trunc(EncodeDate(Year, Month, Day));
    FHasDate := true;
    FTime := EncodeFBExtTime(Hour,Minute,Second,Millisecond*10);
    FHasTimeZone := false;
  end;
end;

procedure TSQLTimestamp.SetAsExtSystemTime(aValue: TFBSystemTime);
begin
  with FFirebirdClientAPI, aValue do
  begin
    FDate := Trunc(EncodeDate(Year, Month, Day));
    FHasDate := true;
    FTime := EncodeFBExtTime(Hour,Minute,Second,DeciMilliSecond);
    FHasTimeZone := (TimeZone <> '') and SupportsTimeZone;
    if FHasTimeZone then
    begin
      FTimeZone := TimeZone;
      FTimeZoneID := GetTimeZoneID(FTimeZone);
    end;
  end;
end;

procedure TSQLTimestamp.SetTimezone(aValue: AnsiString);
begin
  FTimezone := aValue;
  FHasTimezone := true;
  FTimeZoneID := GetTimeZoneID(FTimeZone);
end;

procedure TSQLTimestamp.SetAsString(aValue: AnsiString);
begin
  if not TrySetAsString(aValue) then
    IBError(ibxeInvalidDateTimeStr, [aValue]);
end;

end.

