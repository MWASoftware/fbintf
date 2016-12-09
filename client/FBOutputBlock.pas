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
unit FBOutputBlock;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

{ $DEFINE DEBUGOUTPUTBLOCK}

interface

{Provides common handling for the DB Info results, SQL Info and Service Response Block}

uses
  Classes, SysUtils,  FBClientAPI, IB, FBActivityMonitor;

const
  DefaultBufferSize = 32000;
  DBInfoDefaultBufferSize = 512;

type
  TItemDataType = (dtString, dtString2, dtByte, dtBytes, dtInteger, dtIntegerFixed, dtnone,
    dtList,dtSpecial);

  POutputBlockItemData = ^TOutputBlockItemData;
  TOutputBlockItemData = record
     {Describes a Clumplet in the buffer. FBufPtr always points to the clumplet id
     the rest of the clumplet up to the FSize is data. The data format is
     given by FDataType, and the data length is given by FDataLength}
    FBufPtr: PChar;
    FDataLength: integer;
    FSize: integer;
    FDataType: TItemDataType;
    FTruncated: boolean;
    FError: boolean;
    FSubItems: array of POutputBlockItemData;
  end;

  { TOutputBlock }

  TOutputBlock = class(TFBInterfacedObject)
  private
    FBuffer: PChar;
    FBufSize: integer;
    FBufferParsed: boolean;
    procedure ParseBuffer;
    {$IFDEF DEBUGOUTPUTBLOCK}
    procedure FormattedPrint(const aItems: array of POutputBlockItemData;
      Indent: string);
    {$ENDIF}
    procedure PrintBuf;
  protected
    FIntegerType: TItemDataType;
    FError: boolean;
    FTruncated: boolean;
    FItems: array of POutputBlockItemData;
    procedure DoParseBuffer; virtual; abstract;
    function AddItem(BufPtr: PChar): POutputBlockItemData;
    function AddIntegerItem(BufPtr: PChar): POutputBlockItemData;
    function AddStringItem(BufPtr: PChar): POutputBlockItemData;
    function AddShortStringItem(BufPtr: PChar): POutputBlockItemData;
    function AddByteItem(BufPtr: PChar): POutputBlockItemData;
    function AddBytesItem(BufPtr: PChar): POutputBlockItemData;
    function AddListItem(BufPtr: PChar): POutputBlockItemData; virtual;
    function AddSpecialItem(BufPtr: PChar): POutputBlockItemData; virtual;
  public
    constructor Create(aSize: integer = DefaultBufferSize);
    destructor Destroy; override;
    function Buffer: PChar;
    function getBufSize: integer;

  public
    function GetCount: integer;
    function GetItem(index: integer): POutputBlockItemData;
    function Find(ItemType: byte): POutputBlockItemData;
    property Count: integer read GetCount;
    property Items[index: integer]: POutputBlockItemData read getItem; default;
  end;

  { TOutputBlockItem }

  TOutputBlockItem = class(TFBInterfacedObject,IUnknown)
  private
    FOwner: TOutputBlock;
    FOwnerIntf: IUnknown;
    FItemData: POutputBlockItemData;
  protected
    function GetItem(index: integer): POutputBlockItemData;
    function Find(ItemType: byte): POutputBlockItemData;
    procedure SetString(out S: AnsiString; Buf: PAnsiChar; Len: SizeInt;
                                           CodePage: TSystemCodePage);
    property ItemData: POutputBlockItemData read FItemData;
    property Owner: TOutputBlock read FOwner;
  public
     constructor Create(AOwner: TOutputBlock; Data: POutputBlockItemData);
  public
    function GetCount: integer;
    function getItemType: byte;
    function getSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsInteger: integer;
    function getParamType: byte;
    function getAsString: string;
    function getAsByte: byte;
    function getAsBytes: TByteArray;
    function CopyTo(stream: TStream; count: integer): integer;
  end;

  { TCustomOutputBlock }

  generic TCustomOutputBlock<_TItem,_IItem> = class(TOutputBlock)
  public
    function getItem(index: integer): _IItem;
    function find(ItemType: byte): _IItem;
    property Items[index: integer]: _IItem read getItem; default;
  end;

  { TOutputBlockItemGroup }

  generic TOutputBlockItemGroup<_TItem;_IItem> = class(TOutputBlockItem)
  public
    function GetItem(index: integer): _IItem;
    function Find(ItemType: byte): _IItem;
    property Items[index: integer]: _IItem read getItem; default;
  end;

  TDBInfoItem = class;

  { TDBInfoItem }

  TDBInfoItem = class(specialize TOutputBlockItemGroup<TDBInfoItem,IDBInfoItem>,IDBInfoItem)
  public
    procedure DecodeIDCluster(var ConnectionType: integer; var DBFileName, DBSiteName: string);
    procedure DecodeVersionString(var Version: byte; var VersionString: string);
    procedure DecodeUserNames(UserNames: TStrings);
    function getOperationCounts: TDBOperationCounts;
 end;

  { TDBInformation }

  TDBInformation = class(specialize TCustomOutputBlock<TDBInfoItem,IDBInfoItem>, IDBInformation)
  protected
    function AddSpecialItem(BufPtr: PChar): POutputBlockItemData; override;
    procedure DoParseBuffer; override;
  public
    constructor Create(aSize: integer=DBInfoDefaultBufferSize);
  end;

  TServiceQueryResultSubItem = class(TOutputBlockItem,IServiceQueryResultSubItem);

  { TServiceQueryResultItem }

  TServiceQueryResultItem = class(specialize TOutputBlockItemGroup<TServiceQueryResultSubItem,IServiceQueryResultSubItem>,
                      IServiceQueryResultItem);

  { TServiceQueryResults }

  TServiceQueryResults = class(specialize TCustomOutputBlock<TServiceQueryResultItem,IServiceQueryResultItem>, IServiceQueryResults)
  protected
    function AddListItem(BufPtr: PChar): POutputBlockItemData; override;
    function AddSpecialItem(BufPtr: PChar): POutputBlockItemData; override;
    procedure DoParseBuffer; override;
  end;

  { ISQLInfoItem }

  ISQLInfoItem = interface
    function getItemType: byte;
    function getSize: integer;
    function getAsString: string;
    function getAsInteger: integer;
    function GetCount: integer;
    function GetItem(index: integer): ISQLInfoItem;
    function Find(ItemType: byte): ISQLInfoItem;
    property Count: integer read GetCount;
    property Items[index: integer]: ISQLInfoItem read getItem; default;
  end;

  {ISQLInfoResults}

  ISQLInfoResults = interface
    function GetCount: integer;
    function GetItem(index: integer): ISQLInfoItem;
    function Find(ItemType: byte): ISQLInfoItem;
    property Count: integer read GetCount;
    property Items[index: integer]: ISQLInfoItem read getItem; default;
  end;

  TSQLInfoResultsItem = class;

  { TSQLInfoResultsItem }

  TSQLInfoResultsItem = class(specialize TOutputBlockItemGroup<TSQLInfoResultsItem,ISQLInfoItem>,ISQLInfoItem);

  { TSQLInfoResultsBuffer }

  TSQLInfoResultsBuffer = class(specialize TCustomOutputBlock<TSQLInfoResultsItem,ISQLInfoItem>, ISQLInfoResults)
  protected
    function AddListItem(BufPtr: PChar): POutputBlockItemData; override;
    procedure DoParseBuffer; override;
  public
    constructor Create(aSize: integer = 1024);
  end;

implementation

uses FBMessages;

{ TOutputBlockItemGroup }

function TOutputBlockItemGroup.GetItem(index: integer): _IItem;
var P: POutputBlockItemData;
begin
  P := inherited getItem(index);
  Result := _TItem.Create(self.Owner,P);
end;

function TOutputBlockItemGroup.Find(ItemType: byte): _IItem;
var P: POutputBlockItemData;
begin
  P := inherited Find(ItemType);
  Result := _TItem.Create(self.Owner,P);
end;

{ TCustomOutputBlock }

function TCustomOutputBlock.getItem(index: integer): _IItem;
var P: POutputBlockItemData;
begin
  P := inherited getItem(index);
  Result := _TItem.Create(self,P)
end;

function TCustomOutputBlock.find(ItemType: byte): _IItem;
var P: POutputBlockItemData;
begin
  P := inherited Find(ItemType);
  Result := _TItem.Create(self,P)
end;

{ TOutputBlockItemGroup }

function TOutputBlockItem.GetCount: integer;
begin
  Result := Length(FItemData^.FSubItems);
end;

function TOutputBlockItem.GetItem(index: integer): POutputBlockItemData;
begin
  if (index >= 0) and (index < Length(FItemData^.FSubItems)) then
    Result := FItemData^.FSubItems[index]
  else
  with FirebirdClientAPI do
    IBError(ibxeOutputBlockIndexError,[index]);
end;

function TOutputBlockItem.Find(ItemType: byte): POutputBlockItemData;
var i: integer;
begin
  Result := nil;
  for i := 0 to GetCount - 1 do
    if FItemData^.FSubItems[i]^.FBufPtr^ = char(ItemType) then
    begin
      Result := FItemData^.FSubItems[i];
      Exit;
    end;
end;

{ TOutputBlockItem }

procedure TOutputBlockItem.SetString(out S: AnsiString; Buf: PAnsiChar;
  Len: SizeInt; CodePage: TSystemCodePage);
var rs: RawByteString;
begin
  system.SetString(rs,Buf,len);
  SetCodePage(rs,CodePage,false);
  S := rs;
end;

constructor TOutputBlockItem.Create(AOwner: TOutputBlock;
  Data: POutputBlockItemData);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerIntf := AOwner;
  FItemData := Data;
end;

function TOutputBlockItem.getItemType: byte;
begin
  Result := byte(FItemData^.FBufPtr^);
end;

function TOutputBlockItem.getSize: integer;
begin
  Result := FItemData^.FDataLength;
end;

procedure TOutputBlockItem.getRawBytes(var Buffer);
begin
  with FItemData^ do
    Move(FBufPtr^,Buffer,FDatalength);
end;

function TOutputBlockItem.getAsInteger: integer;
var len: integer;
begin
  with FItemData^ do
  case FDataType of
  dtIntegerFixed:
    with FirebirdClientAPI do
      Result := DecodeInteger(FBufPtr+1,4);

  dtByte,
  dtInteger:
    with FirebirdClientAPI do
    begin
      len := DecodeInteger(FBufPtr+1,2);
      Result := DecodeInteger(FBufPtr+3,len);
    end;
  else
    IBError(ibxeOutputBlockTypeError,[nil]);
  end;
end;

function TOutputBlockItem.getParamType: byte;
begin
   Result := byte(FItemData^.FBufPtr^)
end;

function TOutputBlockItem.getAsString: string;
var len: integer;
begin
  Result := '';
  with FItemData^ do
  case FDataType of
  dtInteger:
    Result := IntToStr(getAsInteger);
  dtByte:
    Result := IntToStr(getAsByte);
  dtString:
    begin
      len := byte((FBufPtr+1)^);
      SetString(Result,FBufPtr+2,len,CP_ACP);
    end;
  dtString2:
    begin
      with FirebirdClientAPI do
        len := DecodeInteger(FBufPtr+1,2);
      SetString(Result,FBufPtr+3,len,CP_ACP);
    end;
  else
    IBError(ibxeOutputBlockTypeError,[nil]);
  end;
end;

function TOutputBlockItem.getAsByte: byte;
begin
  with FItemData^ do
  if FDataType = dtByte then
    Result := byte((FBufPtr+2)^)
  else
    IBError(ibxeOutputBlockTypeError,[nil]);
end;

function TOutputBlockItem.getAsBytes: TByteArray;
var i: integer;
    P: PChar;
begin
  with FItemData^ do
  if FDataType = dtBytes then
  begin
    SetLength(Result,FDataLength);
    P := FBufPtr;
    for i := 0 to FDataLength - 1 do
    begin
      Result[i] := byte(P^);
      Inc(P);
    end
  end
  else
    IBError(ibxeOutputBlockTypeError,[nil]);
end;

function TOutputBlockItem.CopyTo(stream: TStream; count: integer): integer;
var len: integer;
begin
  if count < 0 then count := 0;
  with FItemData^ do
  begin
    case FDataType of
    dtString:
      begin
        len := byte((FBufPtr+1)^);
        if (count > 0) and (count < len) then len := count;
        Result := stream.Write((FBufPtr+2)^,len);
      end;
    dtString2:
      begin
        with FirebirdClientAPI do
          len := DecodeInteger(FBufPtr+1,2);
        if (count > 0) and (count < len) then len := count;
        Result := stream.Write((FBufPtr+3)^,len);
      end;
    else
      IBError(ibxeOutputBlockTypeError,[nil]);
    end;
  end;
end;

{ TOutputBlock }

procedure TOutputBlock.ParseBuffer;
begin
  if not FBufferParsed then
  begin
    {$IFDEF DEBUGOUTPUTBLOCK}
    PrintBuf;
    {$ENDIF}
    DoParseBuffer;
    if FError or FTruncated then
      SetLength(FItems,Length(FItems)-1);
    {$IFDEF DEBUGOUTPUTBLOCK}
    FormattedPrint(FItems,'');
    {$ENDIF}
  end;
  FBufferParsed := true;
end;

function TOutputBlock.AddItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtNone;
    FBufPtr := BufPtr;
    FDataLength := 0;
    FSize := 1;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddIntegerItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := FIntegerType;
    FBufPtr := BufPtr;
    if FDataType = dtIntegerFixed then
    begin
      FDataLength := 4;
      FSize := 5;
    end
    else
    begin
      with FirebirdClientAPI do
        FDataLength := DecodeInteger(FBufPtr+1, 2);
      FSize := FDataLength + 3;
    end;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddStringItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtString2;
    FBufPtr := BufPtr;
    with FirebirdClientAPI do
      FDataLength := DecodeInteger(FBufPtr+1, 2);
    FSize := FDataLength + 3;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddShortStringItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtString;
    FBufPtr := BufPtr;
    FDataLength := byte((FBufPtr+1)^);
    FSize := FDataLength + 2;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddByteItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtByte;
    FBufPtr := BufPtr;
    FDataLength := 1;
    FSize := 2;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddBytesItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtBytes;
    FBufPtr := BufPtr;
    with FirebirdClientAPI do
      FDataLength := DecodeInteger(FBufPtr+1, 2);
    FSize := FDataLength + 3;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddListItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtList;
    FBufPtr := BufPtr;
    FSize := FBuffer + FBufSize - FBufPtr;
    FDataLength := FSize - 1;
    SetLength(FSubItems,0);
  end;
end;

function TOutputBlock.AddSpecialItem(BufPtr: PChar): POutputBlockItemData;
begin
  new(Result);
  with Result^ do
  begin
    FDataType := dtSpecial;
    FBufPtr := BufPtr;
    FSize := FBuffer + FBufSize - FBufPtr;
    FDataLength := FSize - 1;
    SetLength(FSubItems,0);
  end;
end;

constructor TOutputBlock.Create(aSize: integer);
begin
  inherited Create;
  FBufSize := aSize;
  GetMem(FBuffer,aSize);
  if FBuffer = nil then
    OutOfMemoryError;
  FillChar(FBuffer^,aSize,255);
  FBufferParsed := false;
  FIntegerType := dtIntegerFixed;
end;

destructor TOutputBlock.Destroy;
var i, j: integer;
begin
  for i := 0 to length(FItems) - 1 do
  begin
    for j := 0 to Length(FItems[i]^.FSubItems) -1 do
      dispose(FItems[i]^.FSubItems[j]);
    dispose(FItems[i]);
  end;
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TOutputBlock.Buffer: PChar;
begin
  Result := FBuffer;
end;

function TOutputBlock.getBufSize: integer;
begin
  Result := FBufSize;
end;

function TOutputBlock.GetCount: integer;
begin
  ParseBuffer;
  Result := length(FItems);
end;

function TOutputBlock.GetItem(index: integer): POutputBlockItemData;
begin
  ParseBuffer;
  if (index >= 0) and (index < Length(FItems)) then
    Result := FItems[index]
  else
    IBError(ibxeOutputBlockIndexError,[index]);
end;

function TOutputBlock.Find(ItemType: byte): POutputBlockItemData;
var i: integer;
begin
  Result := nil;
  for i := 0 to getCount - 1 do
    if FItems[i]^.FBufPtr^ = char(ItemType) then
    begin
      Result := FItems[i];
      Exit;
    end;
end;

{$IFDEF DEBUGOUTPUTBLOCK}
procedure TOutputBlock.FormattedPrint(
  const aItems: array of POutputBlockItemData; Indent: string);

var i: integer;
    item: TOutputBlockItem;
begin
  if FError then
    writeln('Error')
  else
  if FTruncated then
    writeln('Truncated')
  else
  for i := 0 to Length(aItems) - 1 do
  with aItems[i]^ do
  begin
    if FError then
      writeln('Error')
    else
    if FTruncated then
      writeln('Truncated')
    else
    case FDataType of
    dtList:
    begin
      writeln(Indent,'ItemType = ',byte(FBufPtr^));
      FormattedPrint(FSubItems,Indent + '  ');
    end;
    dtSpecial:
      writeln(Indent,'ItemType = ',byte(FBufPtr^),' Length = ',FSize);
    else
      begin
        item := TOutputBlockItem.Create(self,(aItems[i]));
        writeln(Indent,'ItemType = ',byte(FBufPtr^),' Value = ',(item as TOutputBlockItem).GetAsString);
      end;
    end;
  end;
end;
{$ENDIF}

procedure TOutputBlock.PrintBuf;
var i: integer;
begin
  write(classname,': ');
  for i := 0 to getBufSize - 1 do
  begin
    write(Format('%x ',[byte(Buffer[i])]));
    if byte(FBuffer[i]) = isc_info_end then break;
  end;
  writeln;
end;

{ TDBInfoItem }

procedure TDBInfoItem.DecodeIDCluster(var ConnectionType: integer;
  var DBFileName, DBSiteName: string);
var  P: PChar;
begin
  with ItemData^ do
  if FBufPtr^ = char(isc_info_db_id) then
  begin
    P := FBufPtr + 3;
    if FDataLength > 0 then
      ConnectionType := integer(P^);
    Inc(P);
    SetString(DBFileName,P+1,byte(P^),CP_ACP);
    P += Length(DBFileName) + 1;
    SetString(DBSiteName,P+1,byte(P^),CP_ACP);
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FBufPtr^)]);
end;

procedure TDBInfoItem.DecodeVersionString(var Version: byte;
  var VersionString: string);
var  P: PChar;
begin
  with ItemData^ do
  if FBufPtr^ = char(isc_info_version) then
  begin
   P := FBufPtr+3;
   VersionString := '';
   Version := byte(P^);
   Inc(P);
   SetString(VersionString,P+1,byte(P^),CP_ACP);
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FBufPtr^)]);
end;

procedure TDBInfoItem.DecodeUserNames(UserNames: TStrings);
var P: PChar;
    s: string;
begin
  with ItemData^ do
  if FBufPtr^ = char(isc_info_user_names) then
  begin
    P := FBufPtr+3;
    while (P < FBufPtr + FSize) do
    begin
      SetString(s,P+1,byte(P^),CP_ACP);
      UserNames.Add(s);
      P += Length(s) + 1;
    end;
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FBufPtr^)]);
end;

function TDBInfoItem.getOperationCounts: TDBOperationCounts;
var tableCounts: integer;
    P: PChar;
    i: integer;
begin
  with ItemData^ do
  if byte(FBufPtr^) in [isc_info_backout_count, isc_info_delete_count,
                              isc_info_expunge_count,isc_info_insert_count, isc_info_purge_count,
                              isc_info_read_idx_count, isc_info_read_seq_count, isc_info_update_count] then
  begin
    tableCounts := FDataLength div 6;
    SetLength(Result,TableCounts);
    P := FBufPtr + 3;
    for i := 0 to TableCounts -1 do
    with FirebirdClientAPI do
    begin
      Result[i].TableID := DecodeInteger(P,2);
      Inc(P,2);
      Result[i].Count := DecodeInteger(P,4);
      Inc(P,4);
    end;
  end
  else
    IBError(ibxeInfoBufferTypeError,[integer(FBufPtr^)]);
end;

{ TDBInformation }

function TDBInformation.AddSpecialItem(BufPtr: PChar): POutputBlockItemData;
begin
  Result := inherited AddSpecialItem(BufPtr);
  with Result^ do
  begin
    with FirebirdClientAPI do
      FDataLength := DecodeInteger(FBufPtr+1,2);
    FSize := FDataLength + 3;
  end;
end;

procedure TDBInformation.DoParseBuffer;
var P: PChar;
    index: integer;
begin
  P := Buffer;
  index := 0;
  SetLength(FItems,0);
  while (P^ <> char(isc_info_end)) and (P < Buffer + getBufSize) do
  begin
    SetLength(FItems,index+1);
    case byte(P^) of
    isc_info_no_reserve,
    isc_info_allocation,
    isc_info_ods_minor_version,
    isc_info_ods_version,
    isc_info_db_SQL_dialect,
    isc_info_page_size,
    isc_info_current_memory,
    isc_info_forced_writes,
    isc_info_max_memory,
    isc_info_num_buffers,
    isc_info_sweep_interval,
    isc_info_fetches,
    isc_info_marks,
    isc_info_reads,
    isc_info_writes:
      FItems[index] := AddIntegerItem(P);

    isc_info_implementation,
    isc_info_base_level:
      FItems[index] := AddBytesItem(P);

    isc_info_db_id,
    isc_info_version,
    isc_info_backout_count,
    isc_info_delete_count,
    isc_info_expunge_count,
    isc_info_insert_count,
    isc_info_purge_count,
    isc_info_read_idx_count,
    isc_info_read_seq_count,
    isc_info_update_count,
    isc_info_user_names:
      FItems[index] := AddSpecialItem(P);

    else
      FItems[index] := AddSpecialItem(P);
     end;
    P += FItems[index]^.FSize;
    Inc(index);
  end;
end;

constructor TDBInformation.Create(aSize: integer);
begin
  inherited Create(aSize);
  FIntegerType := dtInteger;
end;

{ TServiceQueryResults }

function TServiceQueryResults.AddListItem(BufPtr: PChar): POutputBlockItemData;
var P: PChar;
    i: integer;
    group: byte;
begin
  Result := inherited AddListItem(BufPtr);
  P := BufPtr + 1;
  i := 0;
  group := byte(BufPtr^);
  if group in [isc_info_svc_get_users,isc_info_svc_limbo_trans] then
  begin
    with FirebirdClientAPI do
       Result^.FSize := DecodeInteger(P,2) + 3;
    Inc(P,2);
  end;
  with Result^ do
  begin
    while (P < FBufPtr + FSize) and (P^ <> char(isc_info_flag_end)) do
    begin
      SetLength(FSubItems,i+1);
      case group of
      isc_info_svc_svr_db_info:
        case integer(P^) of
          isc_spb_num_att,
          isc_spb_num_db:
            FSubItems[i] := AddIntegerItem(P);

          isc_spb_dbname:
            FSubItems[i] := AddStringItem(P);

          else
            IBError(ibxeOutputParsingError, [integer(P^)]);
          end;

      isc_info_svc_get_license:
        case integer(P^) of
        isc_spb_lic_id,
        isc_spb_lic_key:
          FSubItems[i] := AddIntegerItem(P);
        else
          IBError(ibxeOutputParsingError, [integer(P^)]);
        end;

      isc_info_svc_limbo_trans:
       case integer(P^) of
       isc_spb_tra_id,
       isc_spb_single_tra_id,
       isc_spb_multi_tra_id:
         FSubItems[i] := AddIntegerItem(P);

       isc_spb_tra_host_site,
       isc_spb_tra_remote_site,
       isc_spb_tra_db_path:
         FSubItems[i] := AddStringItem(P);

       isc_spb_tra_advise,
       isc_spb_tra_state:
         FSubItems[i] := AddByteItem(P);
       else
         IBError(ibxeOutputParsingError, [integer(P^)]);
       end;

      isc_info_svc_get_users:
        case integer(P^) of
        isc_spb_sec_userid,
        isc_spb_sec_groupid:
          FSubItems[i] := AddIntegerItem(P);

        isc_spb_sec_username,
        isc_spb_sec_password,
        isc_spb_sec_firstname,
        isc_spb_sec_middlename,
        isc_spb_sec_lastname:
          FSubItems[i] := AddStringItem(P);

        else
          IBError(ibxeOutputParsingError, [integer(P^)]);
        end;

      end;
      P +=  FSubItems[i]^.FSize;
      Inc(i);
    end;
    FDataLength := 0;
    for i := 0 to Length(FSubItems) - 1 do
      FDataLength += FSubItems[i]^.FSize;
    if group in [isc_info_svc_get_users,isc_info_svc_limbo_trans] then
      Exit;

    if (P < FBufPtr + FSize) and (P^ = char(isc_info_flag_end)) then
      FSize := FDataLength + 2 {include start and end flag}
    else
      FSize := FDataLength + 1; {start flag only}
  end;
end;

function TServiceQueryResults.AddSpecialItem(BufPtr: PChar
  ): POutputBlockItemData;
var P: PChar;
    i: integer;
begin
  Result := inherited AddSpecialItem(BufPtr);
  with Result^ do
  begin
    with FirebirdClientAPI do
      FDataLength := DecodeInteger(FBufPtr+1, 2);

    P := FBufPtr + 3; {skip length bytes}
    i := 0;
    while P < FBufPtr + FDataLength do
    begin
      FSubItems[i] := AddIntegerItem(P);
      P +=  FSubItems[i]^.FSize;
      Inc(i);
    end;
  end;
end;

procedure TServiceQueryResults.DoParseBuffer;
var P: PChar;
    i: integer;
begin
  P := Buffer;
  i := 0;
  while  (P < Buffer + getBufSize) and (P^ <> char(isc_info_end)) do
  begin
    SetLength(FItems,i+1);
    case integer(P^) of
    isc_info_svc_line,
    isc_info_svc_get_env,
    isc_info_svc_get_env_lock,
    isc_info_svc_get_env_msg,
    isc_info_svc_user_dbpath,
    isc_info_svc_server_version,
    isc_info_svc_implementation,
    isc_info_svc_to_eof:
      FItems[i] := AddStringItem(P);

    isc_info_svc_get_license_mask,
    isc_info_svc_capabilities,
    isc_info_svc_version,
    isc_info_svc_running,
    isc_info_svc_stdin:
      FItems[i] := AddIntegerItem(P);

    isc_info_svc_timeout,
    isc_info_data_not_ready,
    isc_info_truncated:
      FItems[i] := AddItem(P);

    isc_info_svc_svr_db_info,
    isc_info_svc_get_license,
    isc_info_svc_limbo_trans,
    isc_info_svc_get_users:
      FItems[i] := AddListItem(P);

    isc_info_svc_get_config:
      FItems[i] := AddSpecialItem(P);


    else
       IBError(ibxeOutputParsingError, [integer(P^)]);
    end;
    P += FItems[i]^.FSize;
    Inc(i);
  end;
end;

{ TSQLInfoResultsBuffer }

function TSQLInfoResultsBuffer.AddListItem(BufPtr: PChar): POutputBlockItemData;
var P: PChar;
    i: integer;
begin
  Result := inherited AddListItem(BufPtr);
  P := BufPtr + 1;
  i := 0;

  if byte(BufPtr^) = isc_info_sql_records then
  begin
    with FirebirdClientAPI do
      Result^.FSize := DecodeInteger(P,2) + 3;
    Inc(P,2);
    with Result^ do
    begin
      while (P < FBufPtr + FSize) and (byte(P^) <> isc_info_end) do
      begin
        SetLength(FSubItems,i+1);
        case integer(P^) of
        isc_info_req_select_count,
        isc_info_req_insert_count,
        isc_info_req_update_count,
        isc_info_req_delete_count:
          FSubItems[i] := AddIntegerItem(P);

        isc_info_truncated:
          begin
            FTruncated := true;
            Exit;
          end;

        isc_info_error:
          begin
            FError := true;
            Exit;
          end;
        else
          FSubItems[i] := AddSpecialItem(P);
        end;
        P +=  FSubItems[i]^.FSize;
        Inc(i);
      end;
    end;
  end;
end;

procedure TSQLInfoResultsBuffer.DoParseBuffer;
var P: PChar;
    index: integer;
begin
  P := Buffer;
  index := 0;
  SetLength(FItems,0);
  while (P^ <> char(isc_info_end)) and (P < Buffer + getBufSize) do
  begin
    SetLength(FItems,index+1);
    case byte(P^) of
    isc_info_sql_stmt_type:
      FItems[index] := AddIntegerItem(P);

    isc_info_sql_get_plan:
      FItems[index] := AddStringItem(P);

    isc_info_sql_records:
      FItems[index] := AddListItem(P);

    isc_info_truncated:
      begin
        FTruncated := true;
        Exit;
      end;

    isc_info_error:
      begin
        FError := true;
        Exit;
      end;

    else
      FItems[index] := AddSpecialItem(P);
    end;
    P += FItems[index]^.FSize;
    Inc(index);
  end;
end;

constructor TSQLInfoResultsBuffer.Create(aSize: integer);
begin
  inherited Create(aSize);
  FIntegerType := dtInteger;
end;

end.

