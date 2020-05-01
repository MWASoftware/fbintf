unit FB30TimeZoneServices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Firebird, IB, IBExternals, FBActivityMonitor, FBClientAPI,
  FB30ClientAPI, FBAttachment, FB30Attachment, FBTransaction;

type

  { TFB30TimeZoneServices }

  TFB30TimeZoneServices = class(TInterfaceOwner, ITimeZoneServices)
  private
    FAttachment: TFB30Attachment;
    FFirebird30ClientAPI: TFB30ClientAPI;
    FUsingRemoteTZDB: boolean;
    FForceRemoteTZDB: boolean;
    function GetDstOffset(AtTimeStamp: TDateTime; timezoneID: TFBTimeZoneID; AtIsGMT: boolean=true): smallint;
    function DecodeGMTTimestampTZ(bufptr: PISC_TIMESTAMP_TZ): TDateTime;
    function LookupTimeZoneName(aTimeZoneID: TFBTimeZoneID): AnsiString;
    function LookupTimeZoneID(aTimeZone: AnsiString): TFBTimeZoneID;
  public
    constructor Create(attachment: TFB30Attachment; ForceRemoteTZDB: boolean);
  public
    {ITimeZoneServices}
    procedure EncodeTimestampTZ(timestamp: TDateTime; timezoneID: TFBTimeZoneID;
      bufptr: PByte); overload;
    procedure EncodeTimestampTZ(timestamp: TDateTime; timezone: AnsiString;
        bufptr: PByte); overload;
    procedure EncodeTimeTZ(time: TDateTime; timezoneID: TFBTimeZoneID; OnDate: TDateTime;
      bufptr: PByte); overload;
    procedure EncodeTimeTZ(time: TDateTime; timezone: AnsiString; OnDate: TDateTime;
      bufptr: PByte); overload;
    procedure DecodeTimestampTZ(bufptr: PByte; var timestamp: TDateTime;
      var dstOffset: smallint; var timezoneID: TFBTimeZoneID); overload;
    procedure DecodeTimestampTZ(bufptr: PByte; var timestamp: TDateTime;
      var dstOffset: smallint; var timezone: AnsiString); overload;
    procedure DecodeTimestampTZEx(bufptr: PByte; var timestamp: TDateTime;
      var dstOffset: smallint; var timezoneID: TFBTimeZoneID); overload;
    procedure DecodeTimestampTZEx(bufptr: PByte; var timestamp: TDateTime;
      var dstOffset: smallint; var timezone: AnisString); overload;
    procedure DecodeTimeTZ(bufptr: PByte; OnDate: TDateTime; var time: TDateTime;
      var dstOffset: smallint; var timezoneID: TFBTimeZoneID); overload;
    procedure DecodeTimeTZ(bufptr: PByte; OnDate: TDateTime; var time: TDateTime;
      var dstOffset: smallint; var timezone: AnsiString); overload;
    procedure DecodeTimeTZEx(bufptr: PByte; OnDate: TDateTime; var time: TDateTime;
      var dstOffset: smallint; var timezoneID: TFBTimeZoneID); overload;
    procedure DecodeTimeTZEx(bufptr: PByte; OnDate: TDateTime; var time: TDateTime;
      var dstOffset: smallint; var timezone: AnsiString); overload;

    {utility functions}
    function TimeZoneID2TimeZoneName(aTimeZoneID: TFBTimeZoneID): AnsiString;
    function TimeZoneName2TimeZoneID(aTimeZone: AnsiString): TFBTimeZoneID;
    function LocalTimeToUTCTime(aLocalTime: TDateTime; aTimeZone: AnsiString): TDateTime;
    function UTCTimeToLocalTime(aUTCTime: TDateTime; aTimeZone: AnsiString): TDateTime;
    function GetEffectiveOffsetMins(aLocalTime: TDateTime; aTimeZone: AnsiString): integer;

    {Time Zone DB Information}
    function UsingRemoteTZDB: boolean;
    function GetForceUseServerTZDB: boolean;
    procedure SetForceUseServerTZDB(aValue: boolean);
  end;

implementation

uses DateUtils, IBUtils, FBMessages;

{ TFB30TimeZoneServices }

function TFB30TimeZoneServices.GetDstOffset(AtTimeStamp: TDateTime;
  timezoneID: TFBTimeZoneID; AtIsGMT: boolean): smallint;
begin

end;

function TFB30TimeZoneServices.DecodeGMTTimestampTZ(bufptr: PISC_TIMESTAMP_TZ
  ): TDateTime;
var Yr, Mn, Dy: word;
    Hr, Mt, S: word;
    DMs: cardinal;
begin
  with FFirebird30ClientAPI do
  begin
    IUtilIntf.DecodeDate(bufptr^.utc_timestamp.timestamp_date,@Yr, @Mn, @Dy);
    IUtilIntf.DecodeTime(bufptr^.utc_timestamp.timestamp_time,@Hr, @Mt, @S, @DMs);
    Result := EncodeDate(Yr, Mn, Dy) + FBEncodeTime(Hr,Mt,S,DMs);
  end
end;

function TFB30TimeZoneServices.LookupTimeZoneName(aTimeZoneID: TFBTimeZoneID
  ): AnsiString;
begin

end;

function TFB30TimeZoneServices.LookupTimeZoneID(aTimeZone: AnsiString
  ): TFBTimeZoneID;
begin

end;

constructor TFB30TimeZoneServices.Create(attachment: TFB30Attachment;
  ForceRemoteTZDB: boolean);
begin
  inherited Create;
  FAttachment := attachment;
  FFirebird30ClientAPI := attachment.Firebird30ClientAPI;
  FForceRemoteTZDB := ForceRemoteTZDB;
  FUsingRemoteTZDB := ForceRemoteTZDB or not FFirebird30ClientAPI.HasLocalTZDB;
end;

procedure TFB30TimeZoneServices.EncodeTimestampTZ(timestamp: TDateTime;
  timezoneID: TFBTimeZoneID; bufptr: PByte);
var
  Yr, Mn, Dy: word;
  Hr, Mt, S: word;
  DMs: cardinal;
begin
  if not FUsingRemoteTZDB then
    EncodeTimestampTZ(timestamp,TimeZoneID2TimeZoneName(timezonID),bufptr)
  else
  with FFirebird30ClientAPI do
  begin
    timestamp := LocalTime2GMT(timestamp,timezoneID);
    DecodeDate(timestamp, Yr, Mn, Dy);
    FBDecodeTime(timestamp, Hr, Mt, S, DMs);
    with PISC_TIMESTAMP_TZ(Bufptr)^ do
    begin
      utc_timestamp.timestamp_date := UtilIntf.encodeDate(Yr, Mn, Dy);
      utc_timestamp.timestamp_time :=  UtilIntf.encodeTime(Hr, Mt, S, DMs);
      time_zone := timezoneID;
    end;
  end
end;

procedure TFB30TimeZoneServices.EncodeTimestampTZ(timestamp: TDateTime; timezone: AnsiString;
    bufptr: PByte);
var
  Yr, Mn, Dy: word;
  Hr, Mt, S: word;
  DMs: cardinal;
begin
  if FUsingRemoteTZDB then
    EncodeTimestampTZ(timestamp,TimezoneName2TimeZoneID(aTimeZone),bufptr)
  else
  with FFirebird30ClientAPI do
  begin
    DecodeDate(timestamp, Yr, Mn, Dy);
    FBDecodeTime(timestamp, Hr, Mt, S, DMs);
    UtilIntf.encodeTimeStampTz(StatusIntf,ISC_TIMESTAMP_TZPtr(bufPtr),Yr, Mn, Dy, Hr, Mt, S, DMs,PAnsiChar(aTimeZone));
    Check4DataBaseError;
  end;
end;

procedure TFB30TimeZoneServices.EncodeTimeTZ(time: TDateTime;
  timezoneID: TFBTimeZoneID; OnDate: TDateTime; bufptr: PByte);
var Hr, Mt, S: word;
    DMs: cardinal;
    localtime: TDateTime;
    gmtTimestamp: TDateTime;
begin
  localtime := DateOf(OnDate) + time;
  gmtTimestamp := IncMinutes(localtime,-GetDstOffset(localtime,timezoneID),false);
  FBDecodeTime(gmtTimestamp, Hr, Mt, S, DMs);
  with FFirebird30ClientAPI do
    PISC_TIME_TZ(bufptr)^.utc_time := UtilIntf.encodeTime(Hr, Mt, S, DMs);
  PISC_TIME_TZ(bufptr)^.time_zone := timezoneID;
end;

procedure TFB30TimeZoneServices.EncodeTimeTZ(time: TDateTime;
  timezone: AnsiString; OnDate: TDateTime; bufptr: PByte);
begin
  EncodeTimeTZ(time,TimeZoneName2TimeZoneID(timezone),OnDate,bufptr);
end;

procedure TFB30TimeZoneServices.DecodeTimestampTZ(bufptr: PByte;
  var timestamp: TDateTime; var dstOffset: smallint;
  var timezoneID: TFBTimeZoneID);

var aTimeZone: AnsiString;
    gmtTimestamp: TDateTime;
begin
  if not FUsingRemoteTZDB then
  begin
    DecodeTimestampTZ(bufptr,timestamp,dsOffset,aTimeZone);
    timezoneID := PISC_TIMESTAMP_TZ(bufptr)^.time_zone;
  end
  else
  with FFirebird30ClientAPI do
  begin
    gmtTimestamp := DecodeGMTTimestampTZ(PISC_TIMESTAMP_TZ(bufptr));
    timezoneID := PISC_TIMESTAMP_TZ(bufptr)^.time_zone;
    dstOffset := GetDstOffset(gmtTimestamp,timezoneID);
    timestamp := IncMinutes(gmtTimestamp, dstOffset);
  end;
end;

procedure TFB30TimeZoneServices.DecodeTimestampTZ(bufptr: PByte;
  var timestamp: TDateTime; var dstOffset: smallint; var timezone: AnsiString);
const
  bufLength = 128;

var Yr, Mn, Dy: word;
    Hr, Mt, S: word;
    DMs: cardinal;
    tzBuffer: array[ 0.. bufLength] of AnsiChar;
    gmtTimestamp: TDateTime;
    timezoneID: TFBTimeZoneID;
begin
  if FUsingRemoteTZDB then
  begin
    DecodeTimestampTZ(bufptr,timestamp,dstOffset,timezoneID);
    timezone := TimeZoneID2TimeZoneName(timeZoneID);
  end
  else
  with FFirebird30ClientAPI do
  begin
    UtilIntf.decodeTimeStampTz(StatusIntf,ISC_TIMESTAMP_TZPtr(bufPtr),@Yr,@ Mn, @Dy, @Hr, @Mt, @S, @DMs,bufLength,PAnsiChar(@tzBuffer));
    Check4DataBaseError;
    timestamp := EncodeDate(Yr, Mn, Dy) + FBEncodeTime(Hr,Mt,S,DMs);
    timezone := strpas(PAnsiChar(@tzBuffer));
    gmtTimestamp := DecodeGMTTimestampTZ(PISC_TIMESTAMP_TZ(bufptr));
    dstOffset := Round(MinuteSpan(timestamp,gmtTimestamp));
    if gmtTimestamp > timestamp then
      dstOffset := -dstOffset;
  end;
end;

procedure TFB30TimeZoneServices.DecodeTimestampTZEx(bufptr: PByte;
  var timestamp: TDateTime; var dstOffset: smallint;
  var timezoneID: TFBTimeZoneID);
var timezone: AnsiString;
    gmtTimestamp: TDateTime;
begin
  if not FUsingRemoteTZDB then
  begin
    DecodeTimestampTZEx(bufptr,timestamp,dstOffset,timezone);
    timezoneID := PISC_TIMESTAMP_TZ(bufptr)^.time_zone;
  end
  else
  with FFirebird30ClientAPI do
  begin
    if UtilIntf.vtable.version = 21 {FB4 Beta1} then
      IBError(ibxeNotSupported,[]);

    gmtTimestamp := DecodeGMTTimestampTZ(PISC_TIMESTAMP_TZ(bufptr));
    timezoneID := PISC_TIMESTAMP_TZ_EX(bufptr)^.time_zone;
    dstOffset := PISC_TIMESTAMP_TZ_EX(bufptr)^.ext_offset;
    timestamp := IncMinutes(gmtTimestamp, dstOffset);
  end;
end;

procedure TFB30TimeZoneServices.DecodeTimestampTZEx(bufptr: PByte;
  var timestamp: TDateTime; var dstOffset: smallint; var timezone: AnisString);

const
  bufLength = 128;
var
  Yr, Mn, Dy: cardinal;
  Hr, Mt, S, DMs: cardinal;
  tzBuffer: array[ 0.. bufLength] of AnsiChar;
  timezoneID: TFBTimeZoneID;
begin
  if FUsingRemoteTZDB then
  begin
    DecodeTimestampTZEx(bufptr,timestamp,dstOffset,timezoneID);
    timezone := TimeZoneID2TimeZoneName(timezoneID);
  end
  else
  with FFirebird30ClientAPI do
  begin
    if UtilIntf.vtable.version = 21 {FB4 Beta1} then
      IBError(ibxeNotSupported,[]);

    UtilIntf.decodeTimeStampTzEx(StatusIntf,ISC_TIMESTAMP_TZ_EXPtr(bufPtr),@Yr,@ Mn, @Dy, @Hr, @Mt, @S, @DMs,bufLength,PAnsiChar(@tzBuffer));
    Check4DataBaseError;
    timestamp := EncodeDate(Yr, Mn, Dy) + FBEncodeTime(Hr,Mt,S,DMs);
    dstOffset := ISC_TIMESTAMP_TZ_EXPtr(bufPtr)^.ext_offset;
    aTimeZone := strpas(PAnsiChar(@tzBuffer));
  end;
end;

procedure TFB30TimeZoneServices.DecodeTimeTZ(bufptr: PByte; OnDate: TDateTime;
  var time: TDateTime; var dstOffset: smallint; var timezoneID: TFBTimeZoneID);
var aTimeZone: AnsiString;
    gmtTimestamp: TDateTime;
    Hr, Mt, S, DMs: cardinal;
begin
  if not FUsingRemoteTZDB then
  begin
    DecodeTimeTZ(bufptr,OnDate,time,dsOffset,aTimeZone);
    timezoneID := PISC_TIMESTAMP_TZ(bufptr)^.time_zone;
  end
  else
  with FFirebird30ClientAPI do
  begin
    OnDate := DateOf(OnDate);
    timezoneID := PISC_TIME(bufptr)^.time_zone;
    UtilIntf.decodeTimeTz(StatusIntf, ISC_TIME_TZPtr(bufptr),@Hr, @Mt, @S, @DMs,bufLength,PAnsiChar(@tzBuffer));
    Check4DataBaseError;
    gmtTimestamp := OnDate + FBEncodeTime(Hr, Mt, S, DMs);
    dstOffset := GetDstOffset(gmtTimestamp,timezoneID);
    time := TimeOf(IncMinutes(gmtTimeStamp, dstOffset));
  end;
end;

procedure TFB30TimeZoneServices.DecodeTimeTZ(bufptr: PByte; OnDate: TDateTime;
  var time: TDateTime; var dstOffset: smallint; var timezone: AnsiString);
const
    bufLength = 128;
var
  Yr, Mn, Dy: cardinal;
  Hr, Mt, S, DMs: cardinal;
  tzBuffer: array[ 0.. bufLength] of AnsiChar;
  timezoneID: TFBTimeZoneID;
  gmtTimestamp: ISC_TIMESTAMP_TZ;
  localtime: TDateTime;
begin
  if FUsingRemoteTZDB then
  begin
    DecodeTimeTZ(bufptr,OnDate,time,dstOffset,timezoneID);
    timezone := TimeZoneID2TimeZoneName(timezoneID);
  end
  else
  with FFirebird30ClientAPI do
  begin
    {expand to a full timestamp}
    DecodeDate(OnDate, Yr, Mn, Dy);;
    gmtTimestamp.utc_timestamp.timestamp_date := IUtil.encodeDate(Yr, Mn, Dy);
    gmtTimestamp.utc_timestamp.timestamp_time := PISC_TIME_TZ(bufptr)^.utc_time;
    gmtTimestamp.time_zone := PISC_TIME_TZ(bufptr)^.time_zone;

    {now decode the full timestamp}
    UtilIntf.decodeTimeStampTz(StatusIntf,@gmtTimestamp,@Yr,@ Mn, @Dy, @Hr, @Mt, @S, @DMs,bufLength,PAnsiChar(@tzBuffer));
    Check4DataBaseError;
    localtime := EncodeDate(Yr, Mn, Dy) + FBEncodeTime(Hr,Mt,S,DMs);
    time := TimeOf(localtime);
    timezone := strpas(PAnsiChar(@tzBuffer));
    dstOffset := Round(MinuteSpan(localtime,gmtTimestamp));
    if gmtTimestamp > localtime then
      dstOffset := -dstOffset;
  end;
end;

procedure TFB30TimeZoneServices.DecodeTimeTZEx(bufptr: PByte;
  OnDate: TDateTime; var time: TDateTime; var dstOffset: smallint;
  var timezone: AnsiString);

var timezondID: TFBTimeZoneID;
begin
  DecodeTimeTZEx(bufptr,OnDate,time,dstOffset,timezoneID);
  timezone := TimeZoneID2TimeZoneName(timezoneID);
end;

procedure TFB30TimeZoneServices.DecodeTimeTZEx(bufptr: PByte;
  OnDate: TDateTime; var time: TDateTime; var dstOffset: smallint;
  var timezoneID: TFBTimeZoneID);

var
  Hr, Mt, S, DMs: cardinal;
  gmtTime: TDateTime;
  gmtTimestamp: TDateTime;
begin
  with FFirebird30ClientAPI do
  begin
    if UtilIntf.vtable.version = 21 {FB4 Beta1} then
      IBError(ibxeNotSupported,[]);

    {decode the GMT time}
    IUtilIntf.decodeTime(PISC_TIME_TZ_EX(bufptr)^.utc_time, @Hr, @Mt, @S, @DMs);
    gmtTime := FBEncodeTime(Hr, Mt, S, DMs);

    {expand to a timestamp}
    gmtTimestamp := DateOf(OnDate) + gmtTime;
    dstOffset :=  PISC_TIME_TZ_EX(bufptr)^.ext_offset;

    time := TimeOf(IncMinutes(gmtTimestamp,dstOffset));
    timezoneID := PISC_TIME_TZ_EX(bufptr)^.time_zone;
  end;
end;

function TFB30TimeZoneServices.TimeZoneID2TimeZoneName(
  aTimeZoneID: TFBTimeZoneID): AnsiString;
const
    bufLength = 128;
var Buffer: ISC_TIME_TZ;
    Hr, Mt, S, DMs: cardinal;
    aTime: TDateTime;
    tzBuffer: array[ 0.. bufLength] of AnsiChar;
begin
  with FFirebird30ClientAPI do
  if not FUsingRemoteTZDB then
  begin
    Buffer.utc_time := 0;
    Buffer.time_zone := aTimeZoneID;
    UtilIntf.decodeTimeTz(StatusIntf, @Buffer,@Hr, @Mt, @S, @DMs,bufLength,PAnsiChar(@tzBuffer));
    Check4DataBaseError;
    Result := strpas(PAnsiChar(@tzBuffer));
  end
  else
    Result := LookupTimeZoneName(aTimeZoneID);
end;

function TFB30TimeZoneServices.TimeZoneName2TimeZoneID(aTimeZone: AnsiString
  ): TFBTimeZoneID;
var Buffer: ISC_TIME_TZ;
begin
  with FFirebird30ClientAPI do
  if not FUsingRemoteICU then
  begin
    IUtilIntf.EncodeTimeTZ(StatusIntf,@Buffer.utc_time,0,0,0,0,@aTimeZone);
    Result := Buffer.time_zone;
  end
  else
    Result := LookupTimeZoneID(aTimeZone);
end;

function TFB30TimeZoneServices.LocalTimeToUTCTime(aLocalTime: TDateTime;
  aTimeZone: AnsiString): TDateTime;
begin
  Result := IncMinutes(aLocalTime,-GetDSTOffset(aLocalTime,aTimeZone,false));
end;

function TFB30TimeZoneServices.UTCTimeToLocalTime(aUTCTime: TDateTime;
  aTimeZone: AnsiString): TDateTime;
begin
  Result := IncMinutes(aUTCTime,GetDSTOffset(aUTCTime,aTimeZone,true));
end;

function TFB30TimeZoneServices.GetEffectiveOffsetMins(aLocalTime: TDateTime;
  aTimeZone: AnsiString): integer;
begin
  Result := GetDSTOffset(aLocalTime,aTimeZone,false);
end;

function TFB30TimeZoneServices.UsingRemoteTZDB: boolean;
begin
  Result := FUsingRemoteICU;
end;

function TFB30TimeZoneServices.GetForceUseServerTZDB: boolean;
begin
  Result := FForceRemoteTZDB;
end;

procedure TFB30TimeZoneServices.SetForceUseServerTZDB(aValue: boolean);
begin
 if aValue <> FForceRemoteTZDB then
 begin
   FForceRemoteTZDB := aValue;
   FUsingRemoteICU := FForceRemoteTZDB or not FFirebird30ClientAPI.HasLocalTZDB;
 end;
end;

end.

