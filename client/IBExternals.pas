{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

unit IBExternals;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

{ Some structures, declarations that we need for the IB stuff to work, but
  that aren't really part of the ib header file. }
interface

const
  MaxuShort            = 65535;
  ISC_TRUE             = 1;
  ISC_FALSE             = 0;

type
  {$IF not declared(FixedInt)}
  FixedInt             = LongInt;
  FixedUInt            = LongWord;
  {$IFEND}
  Int                  = FixedInt; { 32 bit signed }
  UInt                 = FixedUInt;   { 32 bit unsigned }
  Long                 = FixedInt; { 32 bit signed }
  ULong                = FixedUInt;   { 32 bit unsigned }
  Short                = SmallInt;{ 16 bit signed }
  UShort               = Word;    { 16 bit unsigned }
  Float                = Single;  { 32 bit }
  UChar                = Byte;    { 8 bit unsigned }
  ISC_LONG             = Long;    { 32 bit signed  }
  UISC_LONG            = ULong;   { 32 bit unsigned }
  ISC_USHORT           = UShort;  { 16 bit unsigned }
  ISC_INT64            = Int64;   { 64 bit signed  }
{$IFDEF CPU64}
  ISC_STATUS           = Int64;   { 64 bit signed }
  UISC_STATUS          = UInt64;   { 64 bit unsigned}
{$ELSE}
  ISC_STATUS           = Long;    { 32 bit signed }
  UISC_STATUS          = ULong;   { 32 bit unsigned}
{$ENDIF}
  FB_API_HANDLE        = ^Pointer;
  Void                 = Pointer;
  { Delphi Pointer types }
  PInt                 = ^Int;
  PShort               = ^Short;
  PUShort              = ^UShort;
  PLong                = ^Long;
  PULong               = ^ULong;
  PFloat               = ^Float;
  PUChar               = ^UChar;
  PVoid                = ^Pointer;
  PISC_LONG            = ^ISC_LONG;
  PUISC_LONG           = ^UISC_LONG;
  PISC_STATUS          = ^ISC_STATUS;
  PPISC_STATUS         = ^PISC_STATUS;
  PUISC_STATUS         = ^UISC_STATUS;
  ISC_SHORT            = SmallInt;

  FB_DEC16 = array [1..1] of Int64;
  FB_DEC34 = array [1..2] of Int64;
  FB_DEC_FIXED = array [1..2] of Int64; {FB4 Beta 1 only}
  FB_I128 = array [1..2] of Int64;

  ISC_DATE = Integer;
  ISC_TIME = Integer;

  PISC_TIME_TZ = ^ISC_TIME_TZ;
  ISC_TIME_TZ = record
    utc_time: ISC_TIME;
    time_zone: ISC_USHORT;
  end;

  PISC_TIMESTAMP = ^ISC_TIMESTAMP;
  ISC_TIMESTAMP = record
    timestamp_date: ISC_DATE;
    timestamp_time: ISC_TIME;
  end;

  PISC_TIMESTAMP_TZ = ^ISC_TIMESTAMP_TZ;
  ISC_TIMESTAMP_TZ = record
    utc_timestamp: ISC_TIMESTAMP;
    time_zone: ISC_USHORT;
  end;

  PISC_TIME_TZ_EX = ^ISC_TIME_TZ_EX;
  ISC_TIME_TZ_EX = record
    utc_time: ISC_TIME;
    time_zone: ISC_USHORT;
    ext_offset: ISC_SHORT;
  end;

  PISC_TIMESTAMP_TZ_EX = ^ISC_TIMESTAMP_TZ_EX;
  ISC_TIMESTAMP_TZ_EX = record
    utc_timestamp: ISC_TIMESTAMP;
    time_zone: ISC_USHORT;
    ext_offset: ISC_SHORT;
  end;

type

  { C Date/Time Structure }
  TCTimeStructure = record
    tm_sec : integer;   { Seconds }
    tm_min : integer;   { Minutes }
    tm_hour : integer;  { Hour (0--23) }
    tm_mday : integer;  { Day of month (1--31) }
    tm_mon : integer;   { Month (0--11) }
    tm_year : integer;  { Year (calendar year minus 1900) }
    tm_wday : integer;  { Weekday (0--6) Sunday = 0) }
    tm_yday : integer;  { Day of year (0--365) }
    tm_isdst : integer; { 0 if daylight savings time is not in effect) }
    tm_gmtoff: longint;
    tm_zone: PAnsiChar;
  end;
  PCTimeStructure = ^TCTimeStructure;
  TM              = TCTimeStructure;
  PTM             = ^TM;

  TISC_VARYING = record
    strlen: Short;
    str: array[0..0] of AnsiChar;
  end;

  {***************************}
  {* Some blob ctl structs   *}
  {* from IB help files for  *}
  {* implementing UDFs .     *}
  {* -- Taken from docs, not *}
  {*    in original ibase.h  *}
  {***************************}
  TISC_BlobGetSegment = function(BlobHandle: PInt;
                                 Buffer: PAnsiChar;
                                 BufferSize: Long;
                                 var ResultLength: Long): Short; cdecl;
  TISC_BlobPutSegment = procedure(BlobHandle: PInt;
                                  Buffer: PAnsiChar;
                                  BufferLength: Short); cdecl;
  TBlob = record
    GetSegment         : TISC_BlobGetSegment;
    BlobHandle         : PInt;
    SegmentCount       : Long;
    MaxSegmentLength   : Long;
    TotalSize          : Long;
    PutSegment         : TISC_BlobPutSegment;
  end;
  PBlob = ^TBlob;

const
  { Delphi consts }
  { Days of week }
  dSun = 1;  dMon = 2;  dTue = 3;  dWed = 4;  dThu = 5;  dFri = 6;  dSat = 7;
  { Months of year }
  dJan = 1;  dFeb = 2;  dMar = 3;  dApr = 4;  dMay = 5;  dJun = 6;
  dJul = 7;  dAug = 8;  dSep = 9;  dOct = 10;  dNov = 11;  dDec = 12;
  { C Consts }
  cYearOffset = 1900;
  { Days of week }
  cSun = 0;  cMon = 1;  cTue = 2;  cWed = 3;  cThu = 4;  cFri = 5;  cSat = 6;
  { Months of year }
  cJan = 0;  cFeb = 1;  cMar = 2;  cApr = 3;  cMay = 4;  cJun = 5;
  cJul = 6;  cAug = 7;  cSep = 8;  cOct = 9;  cNov = 10;  cDec = 11;

procedure InitializeTCTimeStructure(var tm_record: TCTimeStructure);

implementation

procedure InitializeTCTimeStructure(var tm_record: TCTimeStructure);
begin
  with tm_record do begin
    tm_sec    := 0;
    tm_min    := 0;
    tm_hour   := 0;
    tm_mday   := 0;
    tm_mon    := 0;
    tm_year   := 0;
    tm_wday   := 0;
    tm_yday   := 0;
    tm_isdst  := 0;
  end;
end;


end.
