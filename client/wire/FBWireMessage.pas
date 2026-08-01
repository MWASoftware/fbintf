(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  This file is part of the pure Pascal wire protocol implementation
 *  (no fbclient required) and is subject to the Initial Developer's
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
 *  The Initial Developer of the Original Code is MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBWireMessage;

{ SQL message formats for the wire protocol.

  A prepared statement's input parameters and output columns are described
  by a list of TWireSQLVar descriptors (populated from the isc_info_sql
  describe response). This unit:

  - computes a flat message buffer layout for a descriptor list (the same
    storage conventions as the Firebird client message buffer, i.e.
    SQL_VARYING = 2 byte length prefix + data, native endian integers)
  - generates the BLR message description sent to the server in
    op_prepare_statement / op_execute / op_fetch
  - encodes/decodes message data between the flat buffer and the XDR
    stream. From protocol 13 a message on the wire is a null bitmap
    followed by the values of the non-null fields.
}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBWireStream;

type
  { TWireSQLVar: describes one column or parameter }

  TWireSQLVar = record
    SQLType: cardinal;      {SQL_XXXX with the null flag (bit 0) removed}
    SQLSubType: integer;
    Scale: integer;
    DataSize: cardinal;     {size from metadata (chars part for VARYING)}
    CharSetID: cardinal;
    AliasName: AnsiString;
    FieldName: AnsiString;
    RelationName: AnsiString;
    OwnerName: AnsiString;
    Nullable: boolean;
    {computed layout in the message buffer}
    DataOffset: cardinal;
    NullOffset: cardinal;    {offset of 32 bit null indicator: -1 = null}
    BufferSize: cardinal;    {bytes reserved at DataOffset}
  end;

  TWireMessageFormat = array of TWireSQLVar;

{computes DataOffset/NullOffset/BufferSize for each var and returns the
 total buffer length}
function ComputeMessageLayout(var aFormat: TWireMessageFormat): cardinal;

{the buffer length of an already laid out format}
function MessageBufferSize(const aFormat: TWireMessageFormat): cardinal;

{the message length as the server computes it from our BLR - the
 PARSE_msg_format algorithm of src/remote/parser.cpp, with each value
 followed by its two byte null indicator. op_batch_create's message
 length field must be exactly this value: the server validates it against
 the format it parses from the BLR.}
function EngineMessageLength(const aFormat: TWireMessageFormat): cardinal;

{rounds aValue up to a multiple of aAlignment (a power of two)}
function AlignTo(aValue, aAlignment: cardinal): cardinal;

{BLR describing the message, as sent to the server}
function BuildMessageBlr(const aFormat: TWireMessageFormat): TBytes;

{writes a message (null bitmap + values) from the flat buffer to the XDR
 stream - protocol 13 and later format}
procedure XDREncodeMessage(XDR: TXDRStream; const aFormat: TWireMessageFormat;
                            aBuffer: PByte);

{reads a message from the XDR stream into the flat buffer}
procedure XDRDecodeMessage(XDR: TXDRStream; const aFormat: TWireMessageFormat;
                            aBuffer: PByte);

type
  { TWireSliceLayout: how the elements of an array slice are arranged in
    the local buffer and encoded on the wire (op_get_slice/op_put_slice).
    The local buffer uses the layout TFBArray.AllocateBuffer establishes;
    the wire uses xdr_slice/xdr_datum (src/remote/protocol.cpp and
    src/common/xdr.cpp) driven by the SDL element descriptor. }

  TWireSliceLayout = record
    Dtype: byte;             {blr type from the array descriptor}
    ElementLength: cardinal; {array_desc_length: the byte length of a text
                              or varying element - unused for other types}
    BufferStride: cardinal;  {spacing of the elements in the local buffer}
    Count: cardinal;         {number of elements in the slice}
  end;

{the element length as the server computes it from the SDL (sdl_desc in
 src/common/sdl.cpp). The slice length fields of the packet count in these
 units: length = elements * SliceElementDscLength.}
function SliceElementDscLength(const aLayout: TWireSliceLayout): cardinal;

{the p_slc_length/lstr_length value for the full slice}
function SliceLength(const aLayout: TWireSliceLayout): cardinal;

{writes the slice elements to the XDR stream - the lstr_length prefix is
 the caller's job}
procedure XDREncodeSlice(XDR: TXDRStream; const aLayout: TWireSliceLayout;
                            aBuffer: PByte);

{reads a slice of aWireLength (in dsc length units, as received in
 lstr_length) into the local buffer}
procedure XDRDecodeSlice(XDR: TXDRStream; const aLayout: TWireSliceLayout;
                            aBuffer: PByte; aWireLength: cardinal);

{Blob and array identifiers are ISC_QUADs: the high word is stored first,
 which is not the memory layout of an Int64 on a little endian machine.
 These helpers convert between the buffer layout and the (high shl 32) or
 low value used by TWireResponse.ObjectID and the Firebird API.}
function WireQuadToInt64(aQuad: PByte): Int64;
procedure Int64ToWireQuad(aQuad: PByte; aValue: Int64);

implementation

uses FBMessages;

function WireQuadToInt64(aQuad: PByte): Int64;
begin
  Result := (Int64(PInteger(aQuad)^) shl 32) or Int64(PCardinal(aQuad+4)^);
end;

procedure Int64ToWireQuad(aQuad: PByte; aValue: Int64);
begin
  PInteger(aQuad)^ := Integer(aValue shr 32);
  PCardinal(aQuad+4)^ := Cardinal(aValue and $FFFFFFFF);
end;

function AlignTo(aValue, aAlignment: cardinal): cardinal;
begin
  Result := (aValue + aAlignment - 1) and not (aAlignment - 1);
end;

procedure GetTypeLayout(const aVar: TWireSQLVar; var aSize, aAlignment: cardinal);
begin
  case aVar.SQLType of
  SQL_TEXT:
    begin
      aSize := aVar.DataSize;
      aAlignment := 1;
    end;
  SQL_VARYING:
    begin
      aSize := aVar.DataSize + 2;
      aAlignment := 2;
    end;
  SQL_SHORT:
    begin
      aSize := 2;
      aAlignment := 2;
    end;
  SQL_LONG, SQL_FLOAT, SQL_TYPE_DATE, SQL_TYPE_TIME:
    begin
      aSize := 4;
      aAlignment := 4;
    end;
  SQL_DOUBLE, SQL_D_FLOAT, SQL_INT64, SQL_TIMESTAMP, SQL_DEC16:
    begin
      aSize := 8;
      aAlignment := 8;
    end;
  SQL_BLOB, SQL_ARRAY, SQL_QUAD:
    begin
      aSize := 8;
      aAlignment := 4;
    end;
  SQL_BOOLEAN:
    begin
      aSize := 1;
      aAlignment := 1;
    end;
  SQL_INT128, SQL_DEC34:
    begin
      aSize := 16;
      aAlignment := 8;
    end;
  SQL_TIME_TZ:
    begin
      aSize := 8;  {ISC_TIME_TZ: 4 byte time + 2 byte zone id (+padding)}
      aAlignment := 4;
    end;
  SQL_TIMESTAMP_TZ:
    begin
      aSize := 12; {ISC_TIMESTAMP_TZ: 8 byte timestamp + 2 byte zone (+pad)}
      aAlignment := 4;
    end;
  SQL_TIME_TZ_EX:
    begin
      aSize := 8;  {ISC_TIME_TZ_EX: time + zone + displacement}
      aAlignment := 4;
    end;
  SQL_TIMESTAMP_TZ_EX:
    begin
      aSize := 12;
      aAlignment := 4;
    end;
  SQL_NULL:
    begin
      aSize := 0;
      aAlignment := 1;
    end;
  else
    IBError(ibxeInvalidDataConversion,[nil]);
  end;
end;

function ComputeMessageLayout(var aFormat: TWireMessageFormat): cardinal;
var i: integer;
    offset: cardinal;
    size, alignment: cardinal;
begin
  offset := 0;
  for i := 0 to Length(aFormat) - 1 do
  begin
    size := 0;
    alignment := 1;
    GetTypeLayout(aFormat[i],size,alignment);
    offset := AlignTo(offset,alignment);
    aFormat[i].DataOffset := offset;
    aFormat[i].BufferSize := size;
    Inc(offset,size);
  end;
  {null indicators at the end, 4 byte aligned}
  offset := AlignTo(offset,4);
  for i := 0 to Length(aFormat) - 1 do
  begin
    aFormat[i].NullOffset := offset;
    Inc(offset,4);
  end;
  Result := offset;
end;

function MessageBufferSize(const aFormat: TWireMessageFormat): cardinal;
begin
  {the null indicators are laid out last, one 4 byte word each}
  if Length(aFormat) = 0 then
    Result := 0
  else
    Result := aFormat[High(aFormat)].NullOffset + 4;
end;

function EngineMessageLength(const aFormat: TWireMessageFormat): cardinal;
var i: integer;
    offset: cardinal;
    size, align: cardinal;
begin
  offset := 0;
  for i := 0 to Length(aFormat) - 1 do
  begin
    {sizes and alignments per PARSE_msg_format and jrd/align.h - these are
     the server's rules, not the layout of our own message buffer}
    case aFormat[i].SQLType of
    SQL_TEXT:
      begin
        size := aFormat[i].DataSize;
        align := 1;
      end;
    SQL_VARYING:
      begin
        size := aFormat[i].DataSize + 2;
        align := 2;
      end;
    SQL_SHORT:
      begin
        size := 2;
        align := 2;
      end;
    SQL_LONG, SQL_FLOAT, SQL_TYPE_DATE, SQL_TYPE_TIME:
      begin
        size := 4;
        align := 4;
      end;
    SQL_DOUBLE, SQL_D_FLOAT, SQL_INT64, SQL_DEC16:
      begin
        size := 8;
        align := 8;
      end;
    SQL_TIMESTAMP:
      begin
        size := 8;
        align := 4;
      end;
    SQL_BLOB, SQL_ARRAY, SQL_QUAD:
      begin
        size := 8;
        align := 4;
      end;
    SQL_BOOLEAN:
      begin
        size := 1;
        align := 1;
      end;
    SQL_DEC34, SQL_INT128:
      begin
        size := 16;
        align := 8;
      end;
    SQL_TIME_TZ, SQL_TIME_TZ_EX:
      begin
        size := 8;
        align := 4;
      end;
    SQL_TIMESTAMP_TZ, SQL_TIMESTAMP_TZ_EX:
      begin
        size := 12;
        align := 4;
      end;
    else
      IBError(ibxeInvalidDataConversion,[nil]);
    end;
    offset := AlignTo(offset,align);
    Inc(offset,size);
    {the null indicator short that BuildMessageBlr emits after each value}
    offset := AlignTo(offset,2);
    Inc(offset,2);
  end;
  Result := offset;
end;

function BuildMessageBlr(const aFormat: TWireMessageFormat): TBytes;
var blr: TBytes;
    blrLen: integer;

  procedure AddByte(aValue: byte);
  begin
    if blrLen >= Length(blr) then
      SetLength(blr,Length(blr) + 64);
    blr[blrLen] := aValue;
    Inc(blrLen);
  end;

  procedure AddWord(aValue: word);
  begin
    {BLR is little endian}
    AddByte(aValue and $FF);
    AddByte((aValue shr 8) and $FF);
  end;

var i: integer;
begin
  SetLength(blr,16 + Length(aFormat)*8);
  blrLen := 0;
  AddByte(blr_version5);
  AddByte(blr_begin);
  AddByte(blr_message);
  AddByte(0);  {message number}
  AddWord(Length(aFormat)*2);  {field count incl. null indicator shorts}
  for i := 0 to Length(aFormat) - 1 do
  begin
    case aFormat[i].SQLType of
    {Text types must name their character set explicitly. Without it the
     engine assumes the connection character set and reinterprets the byte
     length as (length div max bytes per character), which silently
     truncates the column - see blr_text2/blr_varying2 in blr.h.}
    SQL_TEXT:
      begin
        AddByte(blr_text2);
        AddWord(aFormat[i].CharSetID);
        AddWord(aFormat[i].DataSize);
      end;
    SQL_VARYING:
      begin
        AddByte(blr_varying2);
        AddWord(aFormat[i].CharSetID);
        AddWord(aFormat[i].DataSize);
      end;
    SQL_SHORT:
      begin
        AddByte(blr_short);
        AddByte(byte(ShortInt(aFormat[i].Scale)));
      end;
    SQL_LONG:
      begin
        AddByte(blr_long);
        AddByte(byte(ShortInt(aFormat[i].Scale)));
      end;
    SQL_INT64:
      begin
        AddByte(blr_int64);
        AddByte(byte(ShortInt(aFormat[i].Scale)));
      end;
    SQL_INT128:
      begin
        AddByte(blr_int128);
        AddByte(byte(ShortInt(aFormat[i].Scale)));
      end;
    SQL_BLOB:
      begin
        {only the blob id travels in the message; the subtype and charset
         describe the blob contents fetched later with op_get_segment}
        AddByte(blr_blob2);
        AddWord(word(SmallInt(aFormat[i].SQLSubType)));
        AddWord(aFormat[i].CharSetID);
      end;
    SQL_QUAD, SQL_ARRAY:
      begin
        AddByte(blr_quad);
        AddByte(0);
      end;
    SQL_FLOAT:
      AddByte(blr_float);
    SQL_DOUBLE, SQL_D_FLOAT:
      AddByte(blr_double);
    SQL_TIMESTAMP:
      AddByte(blr_timestamp);
    SQL_TYPE_DATE:
      AddByte(blr_sql_date);
    SQL_TYPE_TIME:
      AddByte(blr_sql_time);
    SQL_BOOLEAN:
      AddByte(blr_bool);
    SQL_DEC16:
      AddByte(blr_dec64);
    SQL_DEC34:
      AddByte(blr_dec128);
    SQL_TIME_TZ:
      AddByte(blr_sql_time_tz);
    SQL_TIMESTAMP_TZ:
      AddByte(blr_timestamp_tz);
    SQL_TIME_TZ_EX:
      AddByte(blr_ex_time_tz);
    SQL_TIMESTAMP_TZ_EX:
      AddByte(blr_ex_timestamp_tz);
    else
      IBError(ibxeInvalidDataConversion,[nil]);
    end;
    {every field is followed by a null indicator described as a short}
    AddByte(blr_short);
    AddByte(0);
  end;
  AddByte(blr_end);
  AddByte(blr_eoc);
  SetLength(blr,blrLen);
  Result := blr;
end;

procedure XDREncodeMessage(XDR: TXDRStream; const aFormat: TWireMessageFormat;
  aBuffer: PByte);
var i: integer;
    bitmap: TBytes;
    bitmapLen: integer;
    p: PByte;
    varLen: integer;
    opaque: TBytes;

  function IsNull(index: integer): boolean;
  begin
    Result := PInteger(aBuffer + aFormat[index].NullOffset)^ <> 0;
  end;

begin
  {null bitmap - little endian bit order, padded to 4 bytes}
  bitmapLen := (Length(aFormat) + 7) div 8;
  SetLength(bitmap,AlignTo(bitmapLen,4));
  for i := 0 to High(bitmap) do bitmap[i] := 0;
  for i := 0 to Length(aFormat) - 1 do
    if IsNull(i) then
      bitmap[i div 8] := bitmap[i div 8] or (1 shl (i mod 8));
  XDR.WriteRaw(bitmap[0],Length(bitmap));

  for i := 0 to Length(aFormat) - 1 do
  begin
    if IsNull(i) then continue;
    p := aBuffer + aFormat[i].DataOffset;
    case aFormat[i].SQLType of
    SQL_TEXT:
      begin
        SetLength(opaque,aFormat[i].DataSize);
        if Length(opaque) > 0 then
          Move(p^,opaque[0],Length(opaque));
        XDR.WriteOpaque(opaque);
      end;
    SQL_VARYING:
      begin
        varLen := PWord(p)^;
        if varLen > integer(aFormat[i].DataSize) then
          varLen := aFormat[i].DataSize;
        SetLength(opaque,varLen);
        if varLen > 0 then
          Move((p+2)^,opaque[0],varLen);
        XDR.WriteString(opaque);
      end;
    SQL_SHORT:
      XDR.WriteInt32(PSmallInt(p)^);
    SQL_LONG:
      XDR.WriteInt32(PInteger(p)^);
    SQL_FLOAT:
      XDR.WriteUInt32(PCardinal(p)^);  {IEEE bits}
    SQL_DOUBLE, SQL_D_FLOAT:
      XDR.WriteInt64(PInt64(p)^);      {IEEE bits as hyper}
    SQL_INT64:
      XDR.WriteInt64(PInt64(p)^);
    SQL_TYPE_DATE:
      XDR.WriteInt32(PInteger(p)^);
    SQL_TYPE_TIME:
      XDR.WriteUInt32(PCardinal(p)^);
    SQL_TIMESTAMP:
      begin
        XDR.WriteInt32(PInteger(p)^);
        XDR.WriteUInt32(PCardinal(p+4)^);
      end;
    SQL_BLOB, SQL_ARRAY, SQL_QUAD:
      begin
        {ISC_QUAD: high then low}
        XDR.WriteInt32(PInteger(p)^);
        XDR.WriteUInt32(PCardinal(p+4)^);
      end;
    SQL_BOOLEAN:
      begin
        {xdr_datum sends a boolean as a one byte opaque value padded to 4
         bytes - the value byte comes first, it is not a big endian int}
        SetLength(opaque,1);
        opaque[0] := p^;
        XDR.WriteOpaque(opaque);
      end;
    SQL_DEC16:
      XDR.WriteInt64(PInt64(p)^);
    SQL_DEC34, SQL_INT128:
      begin
        {sent as two hypers, high part first}
        XDR.WriteInt64(PInt64(p+8)^);
        XDR.WriteInt64(PInt64(p)^);
      end;
    SQL_TIME_TZ:
      begin
        XDR.WriteUInt32(PCardinal(p)^);
        XDR.WriteInt32(PWord(p+4)^);
      end;
    SQL_TIMESTAMP_TZ:
      begin
        XDR.WriteInt32(PInteger(p)^);
        XDR.WriteUInt32(PCardinal(p+4)^);
        XDR.WriteInt32(PWord(p+8)^);
      end;
    SQL_TIME_TZ_EX:
      begin
        XDR.WriteUInt32(PCardinal(p)^);
        XDR.WriteInt32(PWord(p+4)^);
        XDR.WriteInt32(PSmallInt(p+6)^);
      end;
    SQL_TIMESTAMP_TZ_EX:
      begin
        XDR.WriteInt32(PInteger(p)^);
        XDR.WriteUInt32(PCardinal(p+4)^);
        XDR.WriteInt32(PWord(p+8)^);
        XDR.WriteInt32(PSmallInt(p+10)^);
      end;
    SQL_NULL: {no data} ;
    else
      IBError(ibxeInvalidDataConversion,[nil]);
    end;
  end;
end;

procedure XDRDecodeMessage(XDR: TXDRStream; const aFormat: TWireMessageFormat;
  aBuffer: PByte);
var i: integer;
    bitmap: TBytes;
    bitmapLen: integer;
    p: PByte;
    opaque: TBytes;
    varLen: integer;

begin
  bitmapLen := (Length(aFormat) + 7) div 8;
  bitmap := XDR.ReadOpaque(bitmapLen);

  for i := 0 to Length(aFormat) - 1 do
  begin
    if (bitmap[i div 8] shr (i mod 8)) and 1 = 1 then
    begin
      PInteger(aBuffer + aFormat[i].NullOffset)^ := -1;
      continue;
    end;
    PInteger(aBuffer + aFormat[i].NullOffset)^ := 0;
    p := aBuffer + aFormat[i].DataOffset;
    case aFormat[i].SQLType of
    SQL_TEXT:
      begin
        opaque := XDR.ReadOpaque(aFormat[i].DataSize);
        if Length(opaque) > 0 then
          Move(opaque[0],p^,Length(opaque));
      end;
    SQL_VARYING:
      begin
        opaque := XDR.ReadString;
        varLen := Length(opaque);
        if varLen > integer(aFormat[i].DataSize) then
          varLen := aFormat[i].DataSize;
        PWord(p)^ := varLen;
        if varLen > 0 then
          Move(opaque[0],(p+2)^,varLen);
      end;
    SQL_SHORT:
      PSmallInt(p)^ := XDR.ReadInt32;
    SQL_LONG:
      PInteger(p)^ := XDR.ReadInt32;
    SQL_FLOAT:
      PCardinal(p)^ := XDR.ReadUInt32;
    SQL_DOUBLE, SQL_D_FLOAT:
      PInt64(p)^ := XDR.ReadInt64;
    SQL_INT64:
      PInt64(p)^ := XDR.ReadInt64;
    SQL_TYPE_DATE:
      PInteger(p)^ := XDR.ReadInt32;
    SQL_TYPE_TIME:
      PCardinal(p)^ := XDR.ReadUInt32;
    SQL_TIMESTAMP:
      begin
        PInteger(p)^ := XDR.ReadInt32;
        PCardinal(p+4)^ := XDR.ReadUInt32;
      end;
    SQL_BLOB, SQL_ARRAY, SQL_QUAD:
      begin
        PInteger(p)^ := XDR.ReadInt32;
        PCardinal(p+4)^ := XDR.ReadUInt32;
      end;
    SQL_BOOLEAN:
      begin
        {one byte value followed by three pad bytes}
        opaque := XDR.ReadOpaque(1);
        p^ := opaque[0];
      end;
    SQL_DEC16:
      PInt64(p)^ := XDR.ReadInt64;
    SQL_DEC34, SQL_INT128:
      begin
        PInt64(p+8)^ := XDR.ReadInt64;
        PInt64(p)^ := XDR.ReadInt64;
      end;
    SQL_TIME_TZ:
      begin
        PCardinal(p)^ := XDR.ReadUInt32;
        PWord(p+4)^ := word(XDR.ReadInt32);
      end;
    SQL_TIMESTAMP_TZ:
      begin
        PInteger(p)^ := XDR.ReadInt32;
        PCardinal(p+4)^ := XDR.ReadUInt32;
        PWord(p+8)^ := word(XDR.ReadInt32);
      end;
    SQL_TIME_TZ_EX:
      begin
        PCardinal(p)^ := XDR.ReadUInt32;
        PWord(p+4)^ := word(XDR.ReadInt32);
        PSmallInt(p+6)^ := SmallInt(XDR.ReadInt32);
      end;
    SQL_TIMESTAMP_TZ_EX:
      begin
        PInteger(p)^ := XDR.ReadInt32;
        PCardinal(p+4)^ := XDR.ReadUInt32;
        PWord(p+8)^ := word(XDR.ReadInt32);
        PSmallInt(p+10)^ := SmallInt(XDR.ReadInt32);
      end;
    SQL_NULL: {no data} ;
    else
      IBError(ibxeInvalidDataConversion,[nil]);
    end;
  end;
end;

function SliceElementDscLength(const aLayout: TWireSliceLayout): cardinal;
begin
  case aLayout.Dtype of
  blr_text, blr_text2:
    Result := aLayout.ElementLength;
  blr_varying, blr_varying2:
    {blr_varying maps to a dtype_cstring element of the declared length
     plus room for a two byte count - see sdl_desc}
    Result := aLayout.ElementLength + 2;
  blr_short:
    Result := 2;
  blr_long, blr_sql_date, blr_sql_time, blr_float:
    Result := 4;
  blr_int64, blr_quad, blr_blob_id, blr_double, blr_d_float,
  blr_timestamp, blr_dec64, blr_sql_time_tz:
    Result := 8;
  blr_ex_time_tz:
    Result := 8;
  blr_timestamp_tz, blr_ex_timestamp_tz:
    Result := 12;
  blr_dec128, blr_int128:
    Result := 16;
  blr_bool:
    Result := 1;
  else
    IBError(ibxeInvalidDataConversion,[nil]);
  end;
end;

function SliceLength(const aLayout: TWireSliceLayout): cardinal;
begin
  Result := aLayout.Count * SliceElementDscLength(aLayout);
end;

{The encodings below follow xdr_datum for the descriptor sdl_desc derives
 from the SDL: notably a varying element travels in dtype_cstring form - a
 count followed by the characters - matching the zero terminated layout of
 the local buffer, not the counted layout used in messages.}

procedure XDREncodeSlice(XDR: TXDRStream; const aLayout: TWireSliceLayout;
  aBuffer: PByte);
var i: cardinal;
    p: PByte;
    n: cardinal;
    opaque: TBytes;
begin
  if aLayout.Count = 0 then Exit;
  for i := 0 to aLayout.Count - 1 do
  begin
    p := aBuffer + i * aLayout.BufferStride;
    case aLayout.Dtype of
    blr_text, blr_text2:
      begin
        SetLength(opaque,aLayout.ElementLength);
        if Length(opaque) > 0 then
          Move(p^,opaque[0],Length(opaque));
        XDR.WriteOpaque(opaque);
      end;
    blr_varying, blr_varying2:
      begin
        n := 0;
        while (n < aLayout.ElementLength) and ((p + n)^ <> 0) do
          Inc(n);
        XDR.WriteInt32(n);
        SetLength(opaque,n);
        if n > 0 then
          Move(p^,opaque[0],n);
        XDR.WriteOpaque(opaque);
      end;
    blr_short:
      XDR.WriteInt32(PSmallInt(p)^);
    blr_long, blr_sql_date:
      XDR.WriteInt32(PInteger(p)^);
    blr_sql_time:
      XDR.WriteUInt32(PCardinal(p)^);
    blr_float:
      XDR.WriteUInt32(PCardinal(p)^);  {IEEE bits}
    blr_int64, blr_quad, blr_blob_id, blr_double, blr_d_float, blr_dec64:
      XDR.WriteInt64(PInt64(p)^);
    blr_timestamp:
      begin
        XDR.WriteInt32(PInteger(p)^);
        XDR.WriteUInt32(PCardinal(p+4)^);
      end;
    blr_sql_time_tz:
      begin
        XDR.WriteUInt32(PCardinal(p)^);
        XDR.WriteInt32(PWord(p+4)^);
      end;
    blr_timestamp_tz:
      begin
        XDR.WriteInt32(PInteger(p)^);
        XDR.WriteUInt32(PCardinal(p+4)^);
        XDR.WriteInt32(PWord(p+8)^);
      end;
    blr_ex_time_tz:
      begin
        XDR.WriteUInt32(PCardinal(p)^);
        XDR.WriteInt32(PWord(p+4)^);
        XDR.WriteInt32(PSmallInt(p+6)^);
      end;
    blr_ex_timestamp_tz:
      begin
        XDR.WriteInt32(PInteger(p)^);
        XDR.WriteUInt32(PCardinal(p+4)^);
        XDR.WriteInt32(PWord(p+8)^);
        XDR.WriteInt32(PSmallInt(p+10)^);
      end;
    blr_dec128, blr_int128:
      begin
        {two hypers, high part first}
        XDR.WriteInt64(PInt64(p+8)^);
        XDR.WriteInt64(PInt64(p)^);
      end;
    blr_bool:
      begin
        SetLength(opaque,1);
        opaque[0] := p^;
        XDR.WriteOpaque(opaque);
      end;
    else
      IBError(ibxeInvalidDataConversion,[nil]);
    end;
  end;
end;

procedure XDRDecodeSlice(XDR: TXDRStream; const aLayout: TWireSliceLayout;
  aBuffer: PByte; aWireLength: cardinal);
var i, aCount: cardinal;
    p: PByte;
    n: cardinal;
    opaque: TBytes;
begin
  aCount := aWireLength div SliceElementDscLength(aLayout);
  if aCount > aLayout.Count then
    aCount := aLayout.Count;
  if aCount = 0 then Exit;
  for i := 0 to aCount - 1 do
  begin
    p := aBuffer + i * aLayout.BufferStride;
    case aLayout.Dtype of
    blr_text, blr_text2:
      begin
        opaque := XDR.ReadOpaque(aLayout.ElementLength);
        if Length(opaque) > 0 then
          Move(opaque[0],p^,Length(opaque));
      end;
    blr_varying, blr_varying2:
      begin
        n := XDR.ReadUInt32;
        if n > aLayout.ElementLength + 1 then
          n := aLayout.ElementLength + 1;
        opaque := XDR.ReadOpaque(n);
        if n > aLayout.ElementLength then
          n := aLayout.ElementLength;
        if n > 0 then
          Move(opaque[0],p^,n);
        (p + n)^ := 0;
      end;
    blr_short:
      PSmallInt(p)^ := XDR.ReadInt32;
    blr_long, blr_sql_date:
      PInteger(p)^ := XDR.ReadInt32;
    blr_sql_time:
      PCardinal(p)^ := XDR.ReadUInt32;
    blr_float:
      PCardinal(p)^ := XDR.ReadUInt32;
    blr_int64, blr_quad, blr_blob_id, blr_double, blr_d_float, blr_dec64:
      PInt64(p)^ := XDR.ReadInt64;
    blr_timestamp:
      begin
        PInteger(p)^ := XDR.ReadInt32;
        PCardinal(p+4)^ := XDR.ReadUInt32;
      end;
    blr_sql_time_tz:
      begin
        PCardinal(p)^ := XDR.ReadUInt32;
        PWord(p+4)^ := word(XDR.ReadInt32);
      end;
    blr_timestamp_tz:
      begin
        PInteger(p)^ := XDR.ReadInt32;
        PCardinal(p+4)^ := XDR.ReadUInt32;
        PWord(p+8)^ := word(XDR.ReadInt32);
      end;
    blr_ex_time_tz:
      begin
        PCardinal(p)^ := XDR.ReadUInt32;
        PWord(p+4)^ := word(XDR.ReadInt32);
        PSmallInt(p+6)^ := SmallInt(XDR.ReadInt32);
      end;
    blr_ex_timestamp_tz:
      begin
        PInteger(p)^ := XDR.ReadInt32;
        PCardinal(p+4)^ := XDR.ReadUInt32;
        PWord(p+8)^ := word(XDR.ReadInt32);
        PSmallInt(p+10)^ := SmallInt(XDR.ReadInt32);
      end;
    blr_dec128, blr_int128:
      begin
        PInt64(p+8)^ := XDR.ReadInt64;
        PInt64(p)^ := XDR.ReadInt64;
      end;
    blr_bool:
      begin
        opaque := XDR.ReadOpaque(1);
        p^ := opaque[0];
      end;
    else
      IBError(ibxeInvalidDataConversion,[nil]);
    end;
  end;
end;

end.
