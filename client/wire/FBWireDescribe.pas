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
unit FBWireDescribe;

{ Parses the isc_info_sql_* response buffer returned by op_prepare_statement
  into the input and output message formats used by FBWireMessage.

  The buffer is a sequence of clumplets: [item:1][length:2 little endian]
  [value] with isc_info_end/isc_info_sql_describe_end acting as markers. }

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB, FBWireMessage;

type
  TWireStatementInfo = record
    StatementType: integer;
    InputFormat: TWireMessageFormat;
    OutputFormat: TWireMessageFormat;
    InputBufferSize: cardinal;
    OutputBufferSize: cardinal;
    Truncated: boolean;   {the info buffer was too small}
  end;

{the standard describe item list sent with op_prepare_statement}
{aIncludeSchema adds the protocol 20 per column schema name item - only
 ask a server that knows it}
function DescribeItems(aIncludeSchema: boolean = false): TBytes;

function ParsePrepareResponse(const aBuffer: TBytes): TWireStatementInfo;

{extracts the row counts (isc_info_sql_records) from an op_info_sql response}
function ParseRowsAffected(const aBuffer: TBytes;
                    var aSelectCount, aInsertCount, aUpdateCount,
                        aDeleteCount: integer): boolean;

implementation

{the isc_info_* and isc_info_sql_* constants come from IB.pas which
 includes inf_pub.inc}

function DescribeItems(aIncludeSchema: boolean): TBytes;
begin
  if aIncludeSchema then
    Result := TBytes.Create(
      isc_info_sql_stmt_type,
      isc_info_sql_select,
      isc_info_sql_describe_vars,
      isc_info_sql_sqlda_seq,
      isc_info_sql_type,
      isc_info_sql_sub_type,
      isc_info_sql_scale,
      isc_info_sql_length,
      isc_info_sql_field,
      isc_info_sql_relation,
      isc_info_sql_relation_schema,
      isc_info_sql_owner,
      isc_info_sql_alias,
      isc_info_sql_describe_end,
      isc_info_sql_bind,
      isc_info_sql_describe_vars,
      isc_info_sql_sqlda_seq,
      isc_info_sql_type,
      isc_info_sql_sub_type,
      isc_info_sql_scale,
      isc_info_sql_length,
      isc_info_sql_field,
      isc_info_sql_relation,
      isc_info_sql_relation_schema,
      isc_info_sql_owner,
      isc_info_sql_alias,
      isc_info_sql_describe_end,
      isc_info_end)
  else
    Result := TBytes.Create(
      isc_info_sql_stmt_type,
      isc_info_sql_select,
      isc_info_sql_describe_vars,
      isc_info_sql_sqlda_seq,
      isc_info_sql_type,
      isc_info_sql_sub_type,
      isc_info_sql_scale,
      isc_info_sql_length,
      isc_info_sql_field,
      isc_info_sql_relation,
      isc_info_sql_owner,
      isc_info_sql_alias,
      isc_info_sql_describe_end,
      isc_info_sql_bind,
      isc_info_sql_describe_vars,
      isc_info_sql_sqlda_seq,
      isc_info_sql_type,
      isc_info_sql_sub_type,
      isc_info_sql_scale,
      isc_info_sql_length,
      isc_info_sql_field,
      isc_info_sql_relation,
      isc_info_sql_owner,
      isc_info_sql_alias,
      isc_info_sql_describe_end,
      isc_info_end);
end;

function ParsePrepareResponse(const aBuffer: TBytes): TWireStatementInfo;
var p: integer;
    item: byte;
    len: integer;
    index: integer;   {0 based index of the var being described}
    rawType: cardinal;
    fmt: ^TWireMessageFormat;

  function ReadLen: integer;
  begin
    Result := aBuffer[p] or (aBuffer[p+1] shl 8);
    Inc(p,2);
  end;

  {clumplet values are little endian two's complement integers}
  function ReadInt(aLen: integer): Int64;
  var i: integer;
      v: QWord;
      signBit: QWord;
  begin
    v := 0;
    for i := 0 to aLen - 1 do
      v := v or (QWord(aBuffer[p+i]) shl (8*i));
    Inc(p,aLen);
    if aLen < 8 then
    begin
      signBit := QWord(1) shl (8*aLen - 1);
      if (v and signBit) <> 0 then
        v := v or not ((QWord(1) shl (8*aLen)) - 1);
    end;
    Result := Int64(v);
  end;

  function ReadStr(aLen: integer): AnsiString;
  var i: integer;
  begin
    SetLength(Result,aLen);
    for i := 0 to aLen - 1 do
      Result[i+1] := AnsiChar(aBuffer[p+i]);
    Inc(p,aLen);
  end;

  procedure EnsureIndex;
  begin
    if index >= Length(fmt^) then
      SetLength(fmt^,index + 1);
  end;

begin
  Result.StatementType := 0;
  SetLength(Result.InputFormat,0);
  SetLength(Result.OutputFormat,0);
  Result.Truncated := false;
  fmt := @Result.OutputFormat;
  index := -1;
  p := 0;
  while p < Length(aBuffer) do
  begin
    item := aBuffer[p];
    Inc(p);
    case item of
    isc_info_end:
      break;
    isc_info_truncated:
      begin
        Result.Truncated := true;
        break;
      end;
    isc_info_sql_describe_end:
      continue;
    isc_info_sql_select:
      begin
        fmt := @Result.OutputFormat;
        index := -1;
        continue;
      end;
    isc_info_sql_bind:
      begin
        fmt := @Result.InputFormat;
        index := -1;
        continue;
      end;
    end;
    if p + 2 > Length(aBuffer) then break;
    len := ReadLen;
    if p + len > Length(aBuffer) then break;
    case item of
    isc_info_sql_stmt_type:
      Result.StatementType := ReadInt(len);
    isc_info_sql_num_variables:
      begin
        SetLength(fmt^,ReadInt(len));
      end;
    isc_info_sql_sqlda_seq:
      begin
        index := ReadInt(len) - 1;   {1 based on the wire}
        EnsureIndex;
      end;
    isc_info_sql_type:
      begin
        EnsureIndex;
        {the low bit of the reported type is the nullable flag}
        rawType := cardinal(ReadInt(len));
        fmt^[index].SQLType := rawType and not cardinal(1);
        fmt^[index].Nullable := (rawType and 1) <> 0;
      end;
    isc_info_sql_sub_type:
      begin
        EnsureIndex;
        fmt^[index].SQLSubType := ReadInt(len);
        {For CHAR and VARCHAR the reported subtype is the character set id
         and isc_info_sql_length is the byte length in that character set.
         Written as comparisons rather than a set test: the SQL type codes
         are far outside the 0..255 a Pascal set can hold, so "in" would
         silently compare truncated values.}
        if (fmt^[index].SQLType = SQL_TEXT) or
           (fmt^[index].SQLType = SQL_VARYING) then
          fmt^[index].CharSetID := cardinal(fmt^[index].SQLSubType);
      end;
    isc_info_sql_scale:
      begin
        EnsureIndex;
        fmt^[index].Scale := ReadInt(len);
      end;
    isc_info_sql_length:
      begin
        EnsureIndex;
        fmt^[index].DataSize := ReadInt(len);
      end;
    isc_info_sql_field:
      begin
        EnsureIndex;
        fmt^[index].FieldName := ReadStr(len);
      end;
    isc_info_sql_relation:
      begin
        EnsureIndex;
        fmt^[index].RelationName := ReadStr(len);
      end;
    isc_info_sql_relation_schema:
      begin
        EnsureIndex;
        fmt^[index].SchemaName := ReadStr(len);
      end;
    isc_info_sql_owner:
      begin
        EnsureIndex;
        fmt^[index].OwnerName := ReadStr(len);
      end;
    isc_info_sql_alias:
      begin
        EnsureIndex;
        fmt^[index].AliasName := ReadStr(len);
      end;
    else
      Inc(p,len);   {skip anything we do not use}
    end;
  end;
  Result.InputBufferSize := ComputeMessageLayout(Result.InputFormat);
  Result.OutputBufferSize := ComputeMessageLayout(Result.OutputFormat);
end;

function ParseRowsAffected(const aBuffer: TBytes;
  var aSelectCount, aInsertCount, aUpdateCount, aDeleteCount: integer): boolean;
var p: integer;
    item: byte;
    len: integer;
    subItem: byte;
    subLen: integer;
    endOfRecords: integer;

  function ReadLenAt(var aPos: integer): integer;
  begin
    Result := aBuffer[aPos] or (aBuffer[aPos+1] shl 8);
    Inc(aPos,2);
  end;

  function ReadIntAt(var aPos: integer; aLen: integer): integer;
  var i: integer;
      v: cardinal;
  begin
    v := 0;
    for i := 0 to aLen - 1 do
      v := v or (cardinal(aBuffer[aPos+i]) shl (8*i));
    Inc(aPos,aLen);
    Result := integer(v);
  end;

begin
  Result := false;
  aSelectCount := 0;
  aInsertCount := 0;
  aUpdateCount := 0;
  aDeleteCount := 0;
  p := 0;
  while p < Length(aBuffer) do
  begin
    item := aBuffer[p];
    Inc(p);
    if (item = isc_info_end) or (item = isc_info_truncated) then
      break;
    if p + 2 > Length(aBuffer) then break;
    len := ReadLenAt(p);
    if p + len > Length(aBuffer) then break;
    if item = isc_info_sql_records then
    begin
      endOfRecords := p + len;
      while p < endOfRecords do
      begin
        subItem := aBuffer[p];
        Inc(p);
        if subItem = isc_info_end then break;
        if p + 2 > endOfRecords then break;
        subLen := ReadLenAt(p);
        if p + subLen > endOfRecords then break;
        case subItem of
        isc_info_req_select_count: aSelectCount := ReadIntAt(p,subLen);
        isc_info_req_insert_count: aInsertCount := ReadIntAt(p,subLen);
        isc_info_req_update_count: aUpdateCount := ReadIntAt(p,subLen);
        isc_info_req_delete_count: aDeleteCount := ReadIntAt(p,subLen);
        else
          Inc(p,subLen);
        end;
      end;
      p := endOfRecords;
      Result := true;
    end
    else
      Inc(p,len);
  end;
end;

end.
