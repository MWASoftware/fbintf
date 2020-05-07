(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API. Although predominantly
 *  a new development they include source code taken from IBX and may be
 *  considered a derived product. This software thus also includes the copyright
 *  notice and license conditions from IBX.
 *
 *  Except for those parts dervied from IBX, contents of this file are subject
 *  to the Initial Developer's Public License Version 1.0 (the "License"); you
 *  may not use this file except in compliance with the License. You may obtain a
 *  copy of the License here:
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
 *  The Original Code is (C) 2020 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FB30TimeZoneServices;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}


interface

uses
  Classes, SysUtils, Firebird, IB, IBExternals, FBActivityMonitor, FBClientAPI,
  FB30ClientAPI, FBAttachment, FB30Attachment, FBTransaction, contnrs;

type

  { TFB30TimeZoneServices }

  TFB30TimeZoneServices = class(TFBInterfacedObject, ITimeZoneServices, ITransactionUser)
  private type
    PTimeZoneInfo = ^TTimeZoneInfo;
    TTimeZoneInfo = record
      Starts: TDateTime;
      Ends: TDateTime;
      ZoneOffset: Smallint;
      DstOffset: SmallInt;
      EffectiveOffset: SmallInt;
      Prev: PTimeZoneInfo;
      Next: PTimeZoneInfo;
    end;

  private type
    ITimeZone = interface
      function GetTimeZoneID: TFBTimeZoneID;
      function GetTimeZoneName: AnsiString;
      function GetTimeZoneData(timestamp: TDateTime; isLocalTime: boolean): PTimeZoneInfo;
      function GetFirstTimeZoneInfo: PTimeZoneInfo;
      function GetLastTimeZoneInfo: PTimeZoneInfo;
      function AddTimeZoneInfo(Starts_, Ends_: TDateTime; ZoneOffset_: Smallint;
                               DstOffer_: SmallInt; EffectiveOffset_: SmallInt): PTimeZoneInfo;
      function CompareTimeRange(timeZoneInfo: PTimeZoneInfo; timestamp: TDateTime; isLocalTime: boolean): integer;
    end;

  private type
    ITimeZoneCache = interface
      function GetTimeZone(aTimeZoneID: TFBTimeZoneID): ITimeZone; overload;
      function GetTimeZone(aTimeZone: AnsiString): ITimeZone; overload;
      function AddTimeZone(aTimeZoneID: TFBTimeZoneID; aTimeZone: AnsiString): ITimeZone;
    end;

  private
    FAttachment: TFB30Attachment;
    FTransaction: ITransaction;
    FFirebird30ClientAPI: TFB30ClientAPI;
    FUsingRemoteTZDB: boolean;
    FTimeZoneCache: ITimeZoneCache;
    FInLoadTimeZoneData: boolean;
    FLocalTimeZoneName: AnsiString;
    function ComputeDstOffset(localtime, gmtTimestamp: TDateTime): integer;
    function GetTransaction: ITransaction;
    function GetTimeZoneCache: ITimeZoneCache;
    function GetTimeZoneData(aTimeZone: ITimeZone; timestamp: TDateTime;
      isLocalTime: boolean): PTimeZoneInfo;
    function GetDstOffset(timestamp: TDateTime; timezoneID: TFBTimeZoneID;
      IsLocalTime: boolean): smallint;
    function DecodeGMTTimestampTZ(bufptr: PISC_TIMESTAMP_TZ): TDateTime;
    function LookupTimeZoneName(aTimeZoneID: TFBTimeZoneID): AnsiString;
    function LookupTimeZoneID(aTimeZone: AnsiString): TFBTimeZoneID;
    function LookupTimeZone(aTimeZoneID: TFBTimeZoneID): ITimeZone; overload;
    function LookupTimeZone(aTimeZone: AnsiString): ITimeZone; overload;
    function LookupTimeZoneInfo(aTimeZoneID: TFBTimeZoneID; timestamp: TDateTime;
      isLocalTime: boolean): PTimeZoneInfo; overload;
    function LookupTimeZoneInfo(aTimeZone: AnsiString; timestamp: TDateTime; isLocalTime: boolean): PTimeZoneInfo; overload;
  public
    constructor Create(attachment: TFB30Attachment);
    destructor Destroy; override;

  public
    {ITransactionUser}
    procedure TransactionEnding(aTransaction: ITransaction; Force: boolean);

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
    function GMTToLocalTime(aGMTTime: TDateTime; aTimeZone: AnsiString): TDateTime; overload;
    function GMTToLocalTime(aGMTTime: TDateTime; aTimeZoneID: TFBTimeZoneID): TDateTime; overload;
    function GetEffectiveOffsetMins(aLocalTime: TDateTime; aTimeZone: AnsiString): integer; overload;
    function GetEffectiveOffsetMins(aLocalTime: TDateTime; aTimeZoneID: TFBTimeZoneID): integer; overload;

    {Time Zone DB Information}
    function UsingRemoteTZDB: boolean;
    procedure SetUseLocalTZDB(useLocalTZDB: boolean);
    function GetLocalTimeZoneName: AnsiString;
    function GetLocalTimeZoneID: TFBTimeZoneID;
  end;

implementation

uses DateUtils, IBUtils, FBMessages, {$IFDEF UNIX} unix {$ENDIF}
  {$IFDEF WINDOWS} Windows {$ENDIF};

type

  { TTimeZoneCache }

  TTimeZoneCache = class(TFBInterfacedObject,TFB30TimeZoneServices.ITimeZoneCache)
  private const
    MaxZoneID = High(TFBTimeZoneID);
  private
    FTimeZoneIDIndex: array of TFB30TimeZoneServices.ITimeZone;
    FTimeZoneNameIndex: TFPHashList;
    FLowValue: integer;
  public
    constructor Create(aLowValue: integer);
    destructor Destroy; override;
  public
    {ITimeZoneCache}
    function GetTimeZone(aTimeZoneID: TFBTimeZoneID): TFB30TimeZoneServices.ITimeZone; overload;
    function GetTimeZone(aTimeZone: AnsiString): TFB30TimeZoneServices.ITimeZone; overload;
    function AddTimeZone(aTimeZoneID: TFBTimeZoneID; aTimeZone: AnsiString): TFB30TimeZoneServices.ITimeZone;
  end;

  {The TTimeZone class provides information about a time zone, including its
   ID, name and a cache of time zone records where each record provides the
   offset in minutes to GMT and any applicable daylight savings time offset
   for a timestamp range (in GMT). The time zone records are held as a
   bidirectional linked list.}

  TTimeZone = class(TFBInterfacedObject,TFB30TimeZoneServices.ITimeZone)
  private
    FTimeZoneID: TFBTimeZoneID;
    FTimeZone: AnsiString;
    FFirst: TFB30TimeZoneServices.PTimeZoneInfo;
    FLast: TFB30TimeZoneServices.PTimeZoneInfo;
    FCurrent: TFB30TimeZoneServices.PTimeZoneInfo;
  public
    constructor Create(aTimeZoneID: TFBTimeZoneID; aTimeZone: AnsiString);
    destructor Destroy; override;
  public
    {ITimeZone}
    function GetTimeZoneID: TFBTimeZoneID;
    function GetTimeZoneName: AnsiString;
    function GetTimeZoneData(timestamp: TDateTime; isLocalTime: boolean): TFB30TimeZoneServices.PTimeZoneInfo;
    function GetFirstTimeZoneInfo: TFB30TimeZoneServices.PTimeZoneInfo;
    function GetLastTimeZoneInfo: TFB30TimeZoneServices.PTimeZoneInfo;
    function AddTimeZoneInfo(Starts_, Ends_: TDateTime; ZoneOffset_: Smallint;
      DstOffset_: SmallInt; EffectiveOffset_: SmallInt): TFB30TimeZoneServices.PTimeZoneInfo;
    function CompareTimeRange(timeZoneInfo: TFB30TimeZoneServices.PTimeZoneInfo; timestamp: TDateTime; isLocalTime: boolean): integer;
  end;

constructor TTimeZone.Create(aTimeZoneID: TFBTimeZoneID; aTimeZone: AnsiString);
begin
  inherited Create;
  FTimeZoneID := aTimeZoneID;
  FTimeZone := aTimeZone;
end;

destructor TTimeZone.Destroy;
var P: TFB30TimeZoneServices.PTimeZoneInfo;
    tmp: TFB30TimeZoneServices.PTimeZoneInfo;
begin
  P := FFirst;
  while p <> nil do
  begin
    tmp := P^.Next;
    dispose(P);
    P := tmp;
  end;
  inherited Destroy;
end;

function TTimeZone.GetTimeZoneID: TFBTimeZoneID;
begin
 Result := FTimeZoneID;
end;

function TTimeZone.GetTimeZoneName: AnsiString;
begin
  Result := FTimeZone;
end;

{Walk the linked list from FCurrent to find the TimeZoneInfo record for the timestamp.
 Returns a nil interface if not record present.}

function TTimeZone.GetTimeZoneData(timestamp: TDateTime; isLocalTime: boolean
  ): TFB30TimeZoneServices.PTimeZoneInfo;
var CompareFlag: integer;
begin
  Result := nil;
  if FCurrent = nil then
    FCurrent := FFirst;

  while FCurrent <> nil do
  begin
    CompareFlag := CompareTimeRange(FCurrent,timestamp,isLocalTime);
    case CompareFlag of
    -1:
      if (FCurrent^.Prev <> nil) and (CompareTimeRange(FCurrent^.Prev,timestamp,isLocalTime) > 0) then
        FCurrent := nil
      else
        FCurrent := FCurrent^.Prev;
    0:
      begin
        Result := FCurrent;
        Exit;
      end;
    1:
      if (FCurrent^.Next <> nil) and (CompareTimeRange(FCurrent^.Next,timestamp,isLocalTime) < 0) then
        FCurrent := nil
      else
        FCurrent := FCurrent^.Next;
    end;
  end;
  Result := FCurrent;
end;

function TTimeZone.GetFirstTimeZoneInfo: TFB30TimeZoneServices.PTimeZoneInfo;
begin
  Result := FFirst;
end;

function TTimeZone.GetLastTimeZoneInfo: TFB30TimeZoneServices.PTimeZoneInfo;
begin
  Result := FLast;
end;

{Adds a new Time Zone Info record by inserting it into the list in start
 time ascending order.}

function TTimeZone.AddTimeZoneInfo(Starts_, Ends_: TDateTime;
  ZoneOffset_: Smallint; DstOffset_: SmallInt; EffectiveOffset_: SmallInt
  ): TFB30TimeZoneServices.PTimeZoneInfo;
var P: TFB30TimeZoneServices.PTimeZoneInfo;
    P1: TFB30TimeZoneServices.PTimeZoneInfo;
begin
  {create and initialise new entry}
  new(Result);
  with Result^ do
  begin
    Starts := Starts_;
    Ends := Ends_;
    ZoneOffset := ZoneOffset_;
    DstOffset := DstOffset_;
    EffectiveOffset := EffectiveOffset_;
  end;

  {empty list? then insert at front}
  if FFirst = nil then
  begin
    FFirst := Result;
    FCurrent := Result;
    FLast := Result;
    Result^.Prev := nil;
    Result^.Next := nil;
  end
  else
  {Before first entry in list? then insert before}
  if Result^.Starts < FFirst^.Starts then
  begin
    Result^.Next := FFirst;
    Result^.Prev := nil;
    FFirst := Result;
  end
  else
  {walk the list to find where we have to insert new entry}
  begin
    P := FFirst^.Next;
    P1 := FFirst;
    while (P <> nil) and (Result^.Starts > P^.Ends) do
    begin
      P1 := P;
      P := P^.Next;
    end;

    {ignore duplicate entry}
    if (P <> nil) and (Result^.Starts = P^.Starts) then
    begin
      dispose(Result);
      Result := P;
    end
    else
    {either at end of list (P=nil) or we insert after P1}
    begin
      Result^.Next := P;
      Result^.Prev := P1;
      P1^.Next := Result;
      if P <> nil then {P=nil => at end of list}
        P^.Prev := Result
      else
        FLast := Result;
    end
  end;
end;

function TTimeZone.CompareTimeRange(
  timeZoneInfo: TFB30TimeZoneServices.PTimeZoneInfo; timestamp: TDateTime;
  isLocalTime: boolean): integer;
begin
  if isLocalTime then {adjust to GMT}
    timestamp := IncMinute(timestamp,-timeZoneInfo^.EffectiveOffset);

  if timestamp < timeZoneInfo^.Starts then
    Result := -1
  else
  if timestamp > timeZoneInfo^.Ends then
    Result := 1
  else
    Result := 0;
end;

{ TTimeZoneCache }

constructor TTimeZoneCache.Create(aLowValue: integer);
begin
  inherited Create;
  FTimeZoneNameIndex := TFPHashList.Create;
  SetLength(FTimeZoneIDIndex,MaxZoneID - aLowValue + 1);
  FLowValue := aLowValue;
end;

destructor TTimeZoneCache.Destroy;
var i: integer;
begin
  for i := Low(FTimeZoneIDIndex) to high(FTimeZoneIDIndex) do
    FTimeZoneIDIndex[i] := nil;
  if FTimeZoneNameIndex <> nil then FTimeZoneNameIndex.Free;
  inherited Destroy;
end;

function TTimeZoneCache.GetTimeZone(aTimeZoneID: TFBTimeZoneID
  ): TFB30TimeZoneServices.ITimeZone;
begin
  Result := FTimeZoneIDIndex[aTimeZoneID - FLowValue];
end;

function TTimeZoneCache.GetTimeZone(aTimeZone: AnsiString
  ): TFB30TimeZoneServices.ITimeZone;
var index: Pointer;
begin
  index := FTimeZoneNameIndex.Find(aTimeZone);
  if index = nil then
    Result := nil
  else
    Result := FTimeZoneIDIndex[PtrUInt(index)];
end;

function TTimeZoneCache.AddTimeZone(aTimeZoneID: TFBTimeZoneID;
  aTimeZone: AnsiString): TFB30TimeZoneServices.ITimeZone;
var index: PtrUInt;
begin
  Result := nil;
  if aTimeZoneID < FLowValue then
    IBError(ibxeInvalidTimeZoneID,[aTimeZoneID]);

  index := aTimeZoneID - FLowValue;
  if FTimeZoneIDIndex[index] = nil then
  begin
    Result := TTimeZone.Create(aTimeZoneID,aTimeZone);
    FTimeZoneIDIndex[index] := Result;
    FTimeZoneNameIndex.Add(aTimeZone,Pointer(index));
  end;
end;

{ TFB30TimeZoneServices }

function TFB30TimeZoneServices.ComputeDstOffset(localtime,
  gmtTimestamp: TDateTime): integer;
begin
 Result := Round(MinuteSpan(localtime,gmtTimestamp));
 if gmtTimestamp > localtime then
   Result := -Result;
end;

function TFB30TimeZoneServices.GetTransaction: ITransaction;
begin
  if FTransaction = nil then
  begin
    FTransaction := FAttachment.StartTransaction([isc_tpb_read,isc_tpb_wait,isc_tpb_concurrency],taCommit);
    (FTransaction as TFBTransaction).AddObject(self);
  end;
  Result := FTransaction;
end;

function TFB30TimeZoneServices.GetTimeZoneCache: ITimeZoneCache;
var Data: IResultSet;
begin
  if FTimeZoneCache = nil then
  begin
    Data := FAttachment.OpenCursorAtStart(GetTransaction,
            'select min(RDB$TIME_ZONE_ID) as LowValue From RDB$TIME_ZONES');
    FTimeZoneCache := TTimeZoneCache.Create(Data[0].AsInteger);
  end;
  Result := FTimeZoneCache;
end;

{This method returns the TimeZoneInfo record for the TimeZone and for which the
 timestamp is between the start and end time for the record. If a matching record
 does not exist in the cache then the remote TZ Database is queried for records
 five years earlier than the timestamp and these are entered into the cache.
 If one of the returned record is a match for the timestamp then it is returned
 as the result of the function.

 Note: the function may return nil if there is no matching record in either the
 cache or remote database. For a localtime, this may be because it is an invalid
 timestamp when taking daylight savings time adjusts into account.}

function TFB30TimeZoneServices.GetTimeZoneData(aTimeZone: ITimeZone;
  timestamp: TDateTime; isLocalTime: boolean): PTimeZoneInfo;
var Data: IResultSet;
    TimeZoneInfo: PTimeZoneInfo;
begin
 Result := aTimeZone.GetTimeZoneData(timestamp,isLocalTime);
 if Result = nil then
 begin
   FInLoadTimeZoneData := true;
//   writeln('Looking up time zone data for ',aTimeZone.GetTimeZoneName,' at ',DateTimeToStr(timestamp));
   try
     {Lookup remote time zone database for ten year range and add result to cache}
     with FAttachment.Prepare(GetTransaction,'Select * From rdb$time_zone_util.transitions(?,?,?)') do
     begin
       SQLParams[0].AsString := aTimeZone.GetTimeZoneName;
       SQLParams[1].AsDateTime := timestamp;
       SQLParams[2].AsDateTime := timestamp;
       Data := OpenCursor;
       while Data.FetchNext do
       begin
         TimeZoneInfo := aTimeZone.AddTimeZoneInfo(Data[0].AsDateTime,Data[1].AsDateTime,
                                                      Data[2].AsInteger,Data[3].AsInteger,
                                                      Data[4].AsInteger);
         if aTimeZone.CompareTimeRange(TimeZoneInfo,timestamp,isLocalTime) = 0 then
            Result := TimeZoneInfo;
       end;
     end;
   finally
     FInLoadTimeZoneData:= false;
   end;
 end
// else
//   writeln('Cache hit for ',aTimeZone.GetTimeZoneName,' at ',DateTimeToStr(timestamp), ' Row Starts at ',DateTimeToStr(Result^.Starts));
end;

function TFB30TimeZoneServices.GetDstOffset(timestamp: TDateTime;
  timezoneID: TFBTimeZoneID; IsLocalTime: boolean): smallint;
var gmtTimeStamp: TDateTime;
    Buffer: ISC_TIMESTAMP_TZ;
    TimeZoneInfo: PTimeZoneInfo;
begin
  if DateOf(timestamp) = 0 then
    IBError(ibxeDatePartMissing,[DateTimeToStr(timestamp)]);
  if FInLoadTimeZoneData then
    Result := 0 {Assume GMT}
  else
  if timeZoneID < MaxOffsetTimeZoneID then
  begin
    if IsLocalTime then
      Result := TimeZoneDisplaymentDelta - timezoneID
    else
      Result := timezoneID - TimeZoneDisplaymentDelta
  end
  else
  if not FUsingRemoteTZDB then
  begin
    EncodeTimestampTZ(timestamp,timezoneID,@Buffer);
    gmtTimeStamp := FFirebird30ClientAPI.SQLDecodeDateTime(@Buffer);
    Result := ComputeDstOffset(timestamp,gmtTimestamp);
  end
  else
  begin
    TimeZoneInfo := LookupTimeZoneInfo(timezoneID,timestamp,isLocalTime);
    if TimeZoneInfo <> nil then
    begin
      Result := TimeZoneInfo^.EffectiveOffset;
      if IsLocalTime then
        Result := -Result;
    end
    else
      IBError(ibxeBadTimestampOrNoTimeZoneDBEntry,[DateTimeToStr(timestamp),timezoneID]);
  end;
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
 Result := LookupTimeZone(aTimeZoneID).GetTimeZoneName;
end;

function TFB30TimeZoneServices.LookupTimeZoneID(aTimeZone: AnsiString
  ): TFBTimeZoneID;
begin
  Result := LookupTimeZone(aTimeZone).GetTimeZoneID;
end;

function TFB30TimeZoneServices.LookupTimeZone(aTimeZoneID: TFBTimeZoneID
  ): ITimeZone;
var aTimeZone: Ansistring;
begin
 Result := GetTimeZoneCache.GetTimeZone(aTimeZoneID);
 if Result = nil then
 begin
   try
     aTimeZone := FAttachment.OpenCursorAtStart(GetTransaction,
              'Select RDB$TIME_ZONE_NAME From RDB$TIME_ZONES Where RDB$TIME_ZONE_ID = ?',3,
              [aTimeZoneID])[0].AsString;
   except
     IBError(ibxeBadTimeZoneID,[aTimeZoneID,0]);
   end;
   Result := GetTimeZoneCache.AddTimeZone(aTimeZoneID,aTimeZone);
 end;
end;

function TFB30TimeZoneServices.LookupTimeZone(aTimeZone: AnsiString): ITimeZone;
var aTimeZoneID: TFBTimeZoneID;
begin
 Result := GetTimeZoneCache.GetTimeZone(aTimeZone);
 if Result = nil then
 begin
   try
     aTimeZoneID := FAttachment.OpenCursorAtStart(GetTransaction,
          'Select RDB$TIME_ZONE_ID From RDB$TIME_ZONES Where RDB$TIME_ZONE_Name = ?',3,
         [aTimeZone])[0].AsInteger;
   except
     IBError(ibxeBadTimeZoneName,[aTimeZone]);
   end;
   Result := GetTimeZoneCache.AddTimeZone(aTimeZoneID,aTimeZone);
 end;
end;

function TFB30TimeZoneServices.LookupTimeZoneInfo(aTimeZoneID: TFBTimeZoneID;
  timestamp: TDateTime; isLocalTime: boolean): PTimeZoneInfo;
var TimeZone: ITimeZone;
begin
  TimeZone := LookupTimeZone(aTimeZoneID);
  if TimeZone <> nil then
    Result := GetTimeZoneData(TimeZone,timestamp,isLocalTime)
  else
    Result := nil;
end;

function TFB30TimeZoneServices.LookupTimeZoneInfo(aTimeZone: AnsiString;
  timestamp: TDateTime; isLocalTime: boolean): PTimeZoneInfo;
var TimeZone: ITimeZone;
begin
  TimeZone := LookupTimeZone(aTimeZone);
  if TimeZone <> nil then
    Result := GetTimeZoneData(TimeZone,timestamp,isLocalTime)
  else
    Result := nil;
end;

constructor TFB30TimeZoneServices.Create(attachment: TFB30Attachment);
{$IFDEF WINDOWS}
var TZInfo: TTimeZoneInformation;
{$ENDIF}
begin
  inherited Create;
  FAttachment := attachment;
  FFirebird30ClientAPI := attachment.Firebird30ClientAPI;
  FUsingRemoteTZDB := true;
  {$IFDEF UNIX}
  FLocalTimeZoneName := strpas(tzname[tzdaylight]);
  {$ENDIF}
  {$IFDEF WINDOWS}
  case GetTimeZoneInformation(TZInfo) of
    TIME_ZONE_ID_UNKNOWN:
      FLocalTimeZoneName := '';
    TIME_ZONE_ID_STANDARD:
      FLocalTimeZoneName := strpas(@TZInfo.StandardName);
    TIME_ZONE_ID_DAYLIGHT:
      FLocalTimeZoneName := strpas(@TZInfo.DaylightName);
  end;
  {$ENDIF}
end;

destructor TFB30TimeZoneServices.Destroy;
begin
  if FTransaction <> nil then
    (FTransaction as TFBTransaction).Remove(self);
  FTransaction := nil;
  FTimeZoneCache := nil;
  inherited Destroy;
end;

procedure TFB30TimeZoneServices.TransactionEnding(aTransaction: ITransaction;
  Force: boolean);
begin
  if (aTransaction as TObject) = (FTransaction as TObject) then
  begin
    (FTransaction as TFBTransaction).Remove(self);
    FTransaction := nil;
  end;
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
var localtime: TDateTime;
    buffer: ISC_TIMESTAMP_TZ;
begin
  localtime := DateOf(OnDate) + time;
  EncodeTimestampTZ(localtime,timezoneID,@buffer);
  PISC_TIME_TZ(bufptr)^.utc_time := buffer.utc_timestamp.timestamp_time;
  PISC_TIME_TZ(bufptr)^.time_zone := buffer.time_zone;
end;

procedure TFB30TimeZoneServices.EncodeTimeTZ(time: TDateTime;
  timezone: AnsiString; OnDate: TDateTime; bufptr: PByte);
var localtime: TDateTime;
    buffer: ISC_TIMESTAMP_TZ;
begin
 localtime := DateOf(OnDate) + time;
 EncodeTimestampTZ(localtime,timezone,@buffer);
 PISC_TIME_TZ(bufptr)^.utc_time := buffer.utc_timestamp.timestamp_time;
 PISC_TIME_TZ(bufptr)^.time_zone := buffer.time_zone;
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
    dstOffset := GetDstOffset(gmtTimestamp,timezoneID,false);
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
    dstOffset := ComputeDstOffset(timestamp,gmtTimestamp);
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

var
    wYr, wMn, wDy: word;
    gmtTimestamp: ISC_TIMESTAMP_TZ;
    localtime: TDateTime;
begin
 with FFirebird30ClientAPI do
 begin
   {expand to a full timestamp}
   DecodeDate(OnDate, wYr, wMn, wDy);
   gmtTimestamp.utc_timestamp.timestamp_date := UtilIntf.encodeDate(wYr, wMn, wDy);
   gmtTimestamp.utc_timestamp.timestamp_time := PISC_TIME_TZ(bufptr)^.utc_time;
   gmtTimestamp.time_zone := PISC_TIME_TZ(bufptr)^.time_zone;

   {now decode the full timestamp}
   DecodeTimestampTZ(@gmtTimestamp,localtime,dstOffset,timezoneID);
   time := TimeOf(localtime);
 end;
end;

procedure TFB30TimeZoneServices.DecodeTimeTZ(bufptr: PByte; OnDate: TDateTime;
  var time: TDateTime; var dstOffset: smallint; var timezone: AnsiString);
var
  wYr, wMn, wDy: word;
  gmtTimestamp: ISC_TIMESTAMP_TZ;
  localtime: TDateTime;
begin
  with FFirebird30ClientAPI do
  begin
    {expand to a full timestamp}
    DecodeDate(OnDate, wYr, wMn, wDy);
    gmtTimestamp.utc_timestamp.timestamp_date := UtilIntf.encodeDate(wYr, wMn, wDy);
    gmtTimestamp.utc_timestamp.timestamp_time := PISC_TIME_TZ(bufptr)^.utc_time;
    gmtTimestamp.time_zone := PISC_TIME_TZ(bufptr)^.time_zone;

    {now decode the full timestamp}
    DecodeTimestampTZ(@gmtTimestamp,localtime,dstOffset,timezone);
    time := TimeOf(localtime);
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
  if aTimeZoneID < MaxOffsetTimeZoneID then {Time Zone ID is for an offset}
    Result := FormatTimeZoneOffset(aTimeZoneID - TimeZoneDisplaymentDelta)
  else
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
    dstOffset: integer;
begin
  if DecodeTimeZoneOffset(aTimeZone,dstOffset) then
    Result := dstOffset + TimeZoneDisplaymentDelta
  else
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
 Result := IncMinute(aLocalTime,GetDSTOffset(aLocalTime,aTimeZoneID,true));
end;

function TFB30TimeZoneServices.GMTToLocalTime(aGMTTime: TDateTime;
  aTimeZone: AnsiString): TDateTime;
begin
 Result := GMTToLocalTime(aGMTTime,TimeZoneName2TimeZoneID(aTimeZone));
end;

function TFB30TimeZoneServices.GMTToLocalTime(aGMTTime: TDateTime;
  aTimeZoneID: TFBTimeZoneID): TDateTime;
begin
 Result := IncMinute(aGMTTime,GetDSTOffset(aGMTTime,aTimeZoneID,false));
end;

function TFB30TimeZoneServices.GetEffectiveOffsetMins(aLocalTime: TDateTime;
  aTimeZone: AnsiString): integer;
begin
  Result :=  GetEffectiveOffsetMins(aLocalTime,TimeZoneName2TimeZoneID(aTimeZone));
end;

function TFB30TimeZoneServices.GetEffectiveOffsetMins(aLocalTime: TDateTime;
  aTimeZoneID: TFBTimeZoneID): integer;
begin
  Result := -GetDSTOffset(aLocalTime,aTimeZoneID,true);
end;

function TFB30TimeZoneServices.UsingRemoteTZDB: boolean;
begin
  Result := FUsingRemoteTZDB;
end;

procedure TFB30TimeZoneServices.SetUseLocalTZDB(useLocalTZDB: boolean);
begin
  if FFirebird30ClientAPI.HasLocalTZDB then
    FUsingRemoteTZDB := not useLocalTZDB
  else
    FUsingRemoteTZDB := true;
end;

function TFB30TimeZoneServices.GetLocalTimeZoneName: AnsiString;
begin
  Result := FLocalTimeZoneName;
end;

function TFB30TimeZoneServices.GetLocalTimeZoneID: TFBTimeZoneID;
begin
  Result := TimeZoneName2TimeZoneID(FLocalTimeZoneName);
end;

end.

