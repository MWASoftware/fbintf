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
    FTransaction: ITransaction;
    FFirebird30ClientAPI: TFB30ClientAPI;
    FUsingRemoteTZDB: boolean;
    FForceRemoteTZDB: boolean;
    function GetTransaction: ITransaction;
    function GetDstOffset(AtTimeStamp: TDateTime; timezoneID: TFBTimeZoneID; AtIsGMT: boolean=true): smallint;
    function DecodeGMTTimestampTZ(bufptr: PISC_TIMESTAMP_TZ): TDateTime;
    function LookupTimeZoneName(aTimeZoneID: TFBTimeZoneID): AnsiString;
    function LookupTimeZoneID(aTimeZone: AnsiString): TFBTimeZoneID;
  public
    constructor Create(attachment: TFB30Attachment; ForceRemoteTZDB: boolean=true);
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
      var dstOffset: smallint; var timezone: AnsiString); overload;
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
    function LocalTimeToGMT(aLocalTime: TDateTime; aTimeZone: AnsiString): TDateTime; overload;
    function LocalTimeToGMT(aLocalTime: TDateTime; aTimeZoneID: TFBTimeZoneID): TDateTime; overload;
    function GMTToLocalTime(aUTCTime: TDateTime; aTimeZone: AnsiString): TDateTime; overload;
    function GMTToLocalTime(aUTCTime: TDateTime; aTimeZoneID: TFBTimeZoneID): TDateTime; overload;
    function GetEffectiveOffsetMins(aLocalTime: TDateTime; aTimeZone: AnsiString): integer; overload;
    function GetEffectiveOffsetMins(aLocalTime: TDateTime; aTimeZoneID: TFBTimeZoneID): integer; overload;

    {Time Zone DB Information}
    function UsingRemoteTZDB: boolean;
    function GetForceUseServerTZDB: boolean;
    procedure SetForceUseServerTZDB(aValue: boolean);
    function HasExtendedTZSupport: boolean;
  end;

implementation

uses DateUtils, IBUtils, FBMessages;

{ TFB30TimeZoneServices }

function TFB30TimeZoneServices.GetTransaction: ITransaction;
begin
  if FTransaction = nil then
    FTransaction := FAttachment.StartTransaction([isc_tpb_read,isc_tpb_wait,isc_tpb_concurrency],taCommit);
  Result := FTransaction;
end;

function TFB30TimeZoneServices.GetDstOffset(AtTimeStamp: TDateTime;
  timezoneID: TFBTimeZoneID; AtIsGMT: boolean): smallint;
begin
 with FAttachment.Prepare(GetTransaction,'Select EFFECTIVE_OFFSET From rdb$time_zone_util.transitions(?,?,?)') do
 begin
   SQLParams[0].AsString := LookupTimeZoneName(timezoneID);
   SQLParams[1].AsDateTime := AtTimeStamp;
   SQLParams[2].AsDateTime := AtTimeStamp;
   Result := OpenCursor[0].AsInteger;
 end;
 if not AtIsGMT then
   Result := -Result;
end;

function TFB30TimeZoneServices.DecodeGMTTimestampTZ(bufptr: PISC_TIMESTAMP_TZ
  ): TDateTime;
var Yr, Mn, Dy: word;
    Hr, Mt, S: word;
    DMs: cardinal;
begin
  with FFirebird30ClientAPI do
  begin
    UtilIntf.DecodeDate(bufptr^.utc_timestamp.timestamp_date,@Yr, @Mn, @Dy);
    UtilIntf.DecodeTime(bufptr^.utc_timestamp.timestamp_time,@Hr, @Mt, @S, @DMs);
    Result := EncodeDate(Yr, Mn, Dy) + FBEncodeTime(Hr,Mt,S,DMs);
  end
end;

function TFB30TimeZoneServices.LookupTimeZoneName(aTimeZoneID: TFBTimeZoneID
  ): AnsiString;
begin
 try
   Result := FAttachment.OpenCursorAtStart(GetTransaction,
       'Select RDB$TIME_ZONE_NAME From RDB$TIME_ZONES Where RDB$TIME_ZONE_ID = ?',3,
       [aTimeZoneID])[0].AsString;
 except
   IBError(ibxeBadTimeZoneID,[aTimeZoneID,0]);
 end;
end;

function TFB30TimeZoneServices.LookupTimeZoneID(aTimeZone: AnsiString
  ): TFBTimeZoneID;
begin
 try
   Result := FAttachment.OpenCursorAtStart(GetTransaction,
        'Select RDB$TIME_ZONE_ID From RDB$TIME_ZONES Where RDB$TIME_ZONE_Name = ?',3,
       [aTimeZone])[0].AsInteger;
 except
   IBError(ibxeBadTimeZoneName,[aTimeZone]);
 end;
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
    EncodeTimestampTZ(timestamp,TimeZoneID2TimeZoneName(timezoneID),bufptr)
  else
  with FFirebird30ClientAPI do
  begin
    timestamp := LocalTimeToGMT(timestamp,timezoneID);
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
    EncodeTimestampTZ(timestamp,TimezoneName2TimeZoneID(timezone),bufptr)
  else
  with FFirebird30ClientAPI do
  begin
    DecodeDate(timestamp, Yr, Mn, Dy);
    FBDecodeTime(timestamp, Hr, Mt, S, DMs);
    UtilIntf.encodeTimeStampTz(StatusIntf,ISC_TIMESTAMP_TZPtr(bufPtr),Yr, Mn, Dy, Hr, Mt, S, DMs,PAnsiChar(timezone));
    Check4DataBaseError;
  end;
end;

{When encoding a time it must first be converted to GMT on a given date.}

procedure TFB30TimeZoneServices.EncodeTimeTZ(time: TDateTime;
  timezoneID: TFBTimeZoneID; OnDate: TDateTime; bufptr: PByte);
var Hr, Mt, S: word;
    DMs: cardinal;
    localtime: TDateTime;
    gmtTimestamp: TDateTime;
begin
  localtime := DateOf(OnDate) + time;
  gmtTimestamp := IncMinute(localtime,GetDstOffset(localtime,timezoneID,false));
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
    DecodeTimestampTZ(bufptr,timestamp,dstOffset,aTimeZone);
    timezoneID := PISC_TIMESTAMP_TZ(bufptr)^.time_zone;
  end
  else
  with FFirebird30ClientAPI do
  begin
    gmtTimestamp := DecodeGMTTimestampTZ(PISC_TIMESTAMP_TZ(bufptr));
    timezoneID := PISC_TIMESTAMP_TZ(bufptr)^.time_zone;
    dstOffset := GetDstOffset(gmtTimestamp,timezoneID);
    timestamp := IncMinute(gmtTimestamp, dstOffset);
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
    gmtTimestamp := DecodeGMTTimestampTZ(PISC_TIMESTAMP_TZ(bufptr));
    timezoneID := PISC_TIMESTAMP_TZ_EX(bufptr)^.time_zone;
    dstOffset := PISC_TIMESTAMP_TZ_EX(bufptr)^.ext_offset;
    timestamp := IncMinute(gmtTimestamp, dstOffset);
  end;
end;

procedure TFB30TimeZoneServices.DecodeTimestampTZEx(bufptr: PByte;
  var timestamp: TDateTime; var dstOffset: smallint; var timezone: AnsiString);

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
    if not HasExtendedTZSupport then
      IBError(ibxeNotSupported,[]);

    UtilIntf.decodeTimeStampTzEx(StatusIntf,ISC_TIMESTAMP_TZ_EXPtr(bufPtr),@Yr,@ Mn, @Dy, @Hr, @Mt, @S, @DMs,bufLength,PAnsiChar(@tzBuffer));
    Check4DataBaseError;
    timestamp := EncodeDate(Yr, Mn, Dy) + FBEncodeTime(Hr,Mt,S,DMs);
    dstOffset := ISC_TIMESTAMP_TZ_EXPtr(bufPtr)^.ext_offset;
    timezone := strpas(PAnsiChar(@tzBuffer));
  end;
end;

procedure TFB30TimeZoneServices.DecodeTimeTZ(bufptr: PByte; OnDate: TDateTime;
  var time: TDateTime; var dstOffset: smallint; var timezoneID: TFBTimeZoneID);

const
  bufLength = 128;
var aTimeZone: AnsiString;
    gmtTimestamp: TDateTime;
    Hr, Mt, S, DMs: cardinal;
    tzBuffer: array[ 0.. bufLength] of AnsiChar;
begin
  if not FUsingRemoteTZDB then
  begin
    DecodeTimeTZ(bufptr,OnDate,time,dstOffset,aTimeZone);
    timezoneID := PISC_TIMESTAMP_TZ(bufptr)^.time_zone;
  end
  else
  with FFirebird30ClientAPI do
  begin
    OnDate := DateOf(OnDate);
    timezoneID := PISC_TIME_TZ(bufptr)^.time_zone;
    UtilIntf.decodeTimeTz(StatusIntf, ISC_TIME_TZPtr(bufptr),@Hr, @Mt, @S, @DMs,bufLength,PAnsiChar(@tzBuffer));
    Check4DataBaseError;
    gmtTimestamp := OnDate + FBEncodeTime(Hr, Mt, S, DMs);
    dstOffset := GetDstOffset(gmtTimestamp,timezoneID);
    time := TimeOf(IncMinute(gmtTimeStamp, dstOffset));
  end;
end;

procedure TFB30TimeZoneServices.DecodeTimeTZ(bufptr: PByte; OnDate: TDateTime;
  var time: TDateTime; var dstOffset: smallint; var timezone: AnsiString);
const
    bufLength = 128;
var
  wYr, wMn, wDy: word;
  Yr, Mn, Dy: cardinal;
  Hr, Mt, S, DMs: cardinal;
  tzBuffer: array[ 0.. bufLength] of AnsiChar;
  timezoneID: TFBTimeZoneID;
  gmtTimestamp: ISC_TIMESTAMP_TZ;
  localtime: TDateTime;
  gmtTime: TDateTime;
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
    DecodeDate(OnDate, wYr, wMn, wDy);
    gmtTimestamp.utc_timestamp.timestamp_date := UtilIntf.encodeDate(wYr, wMn, wDy);
    gmtTimestamp.utc_timestamp.timestamp_time := PISC_TIME_TZ(bufptr)^.utc_time;
    gmtTimestamp.time_zone := PISC_TIME_TZ(bufptr)^.time_zone;

    {now decode the full timestamp}
    UtilIntf.decodeTimeStampTz(StatusIntf,@gmtTimestamp,@Yr,@ Mn, @Dy, @Hr, @Mt, @S, @DMs,bufLength,PAnsiChar(@tzBuffer));
    Check4DataBaseError;
    time :=  FBEncodeTime(Hr,Mt,S,DMs);
    localtime := EncodeDate(Yr, Mn, Dy) + time;
    timezone := strpas(PAnsiChar(@tzBuffer));
    UtilIntf.decodeTime(gmtTimestamp.utc_timestamp.timestamp_time, @Hr, @Mt, @S, @DMs);
    gmtTime := OnDate + FBEncodeTime(Hr,Mt,S,DMs);
    dstOffset := Round(MinuteSpan(localtime,gmtTime));
    if gmtTime > localtime then
      dstOffset := -dstOffset;
  end;
end;

procedure TFB30TimeZoneServices.DecodeTimeTZEx(bufptr: PByte;
  OnDate: TDateTime; var time: TDateTime; var dstOffset: smallint;
  var timezone: AnsiString);

var timezoneID: TFBTimeZoneID;
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
    {decode the GMT time}
    UtilIntf.decodeTime(PISC_TIME_TZ_EX(bufptr)^.utc_time, @Hr, @Mt, @S, @DMs);
    gmtTime := FBEncodeTime(Hr, Mt, S, DMs);

    {expand to a timestamp}
    gmtTimestamp := DateOf(OnDate) + gmtTime;
    dstOffset :=  PISC_TIME_TZ_EX(bufptr)^.ext_offset;

    time := TimeOf(IncMinute(gmtTimestamp,dstOffset));
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
  if not FUsingRemoteTZDB then
  begin
    UtilIntf.EncodeTimeTZ(StatusIntf,@Buffer.utc_time,0,0,0,0,@aTimeZone);
    Result := Buffer.time_zone;
  end
  else
    Result := LookupTimeZoneID(aTimeZone);
end;

function TFB30TimeZoneServices.LocalTimeToGMT(aLocalTime: TDateTime;
  aTimeZone: AnsiString): TDateTime;
begin
  Result := LocalTimeToGMT(aLocalTime,TimeZoneName2TimeZoneID(aTimeZone));
end;

function TFB30TimeZoneServices.LocalTimeToGMT(aLocalTime: TDateTime;
  aTimeZoneID: TFBTimeZoneID): TDateTime;
begin
 Result := IncMinute(aLocalTime,GetDSTOffset(aLocalTime,aTimeZoneID,false));
end;

function TFB30TimeZoneServices.GMTToLocalTime(aUTCTime: TDateTime;
  aTimeZone: AnsiString): TDateTime;
begin
 Result := GMTToLocalTime(aUTCTime,TimeZoneName2TimeZoneID(aTimeZone));
end;

function TFB30TimeZoneServices.GMTToLocalTime(aUTCTime: TDateTime;
  aTimeZoneID: TFBTimeZoneID): TDateTime;
begin
 Result := IncMinute(aUTCTime,GetDSTOffset(aUTCTime,aTimeZoneID,true));
end;

function TFB30TimeZoneServices.GetEffectiveOffsetMins(aLocalTime: TDateTime;
  aTimeZone: AnsiString): integer;
begin
  Result :=  GetEffectiveOffsetMins(aLocalTime,TimeZoneName2TimeZoneID(aTimeZone));
end;

function TFB30TimeZoneServices.GetEffectiveOffsetMins(aLocalTime: TDateTime;
  aTimeZoneID: TFBTimeZoneID): integer;
begin
  Result := GetDSTOffset(aLocalTime,aTimeZoneID,false);
end;

function TFB30TimeZoneServices.UsingRemoteTZDB: boolean;
begin
  Result := FUsingRemoteTZDB;
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
   FUsingRemoteTZDB := FForceRemoteTZDB or not FFirebird30ClientAPI.HasLocalTZDB;
 end;
end;

function TFB30TimeZoneServices.HasExtendedTZSupport: boolean;
begin
 with FFirebird30ClientAPI do
  Result :=  UtilIntf.vtable.version <> 21 {FB4 Beta1}
end;

end.

