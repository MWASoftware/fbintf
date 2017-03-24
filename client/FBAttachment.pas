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
unit FBAttachment;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB,  FBParamBlock, FBActivityMonitor;

type
  TCharsetMap = record
    CharsetID: integer;
    CharSetName: AnsiString;
    CharSetWidth: integer;
    CodePage: TSystemCodePage;
    AllowReverseLookup: boolean; {used to ensure that lookup of CP_UTF* does not return UNICODE_FSS}
  end;

  { TFBAttachment }

  TFBAttachment = class(TActivityHandler)
  private
    FDPB: IDPB;
    FFirebirdAPI: IFirebirdAPI;
    FUserCharSetMap: array of TCharSetMap;
  protected
    FDatabaseName: AnsiString;
    FRaiseExceptionOnConnectError: boolean;
    FSQLDialect: integer;
    FHasDefaultCharSet: boolean;
    FCharSetID: integer;
    FCodePage: TSystemCodePage;
    constructor Create(DatabaseName: AnsiString; DPB: IDPB;
      RaiseExceptionOnConnectError: boolean);
    procedure CheckHandle; virtual; abstract;
    function GenerateCreateDatabaseSQL(DatabaseName: AnsiString; aDPB: IDPB): AnsiString;
    procedure EndAllTransactions;
    procedure SetParameters(SQLParams: ISQLParams; params: array of const);
  public
    destructor Destroy; override;
    function getDPB: IDPB;
    function AllocateBPB: IBPB;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction; overload; virtual; abstract;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionCompletion): ITransaction; overload; virtual; abstract;
    procedure Disconnect(Force: boolean=false); virtual; abstract;
    procedure ExecImmediate(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer); overload; virtual; abstract;
    procedure ExecImmediate(TPB: array of byte; sql: AnsiString; aSQLDialect: integer); overload;
    procedure ExecImmediate(transaction: ITransaction; sql: AnsiString); overload;
    procedure ExecImmediate(TPB: array of byte; sql: AnsiString); overload;
    function ExecuteSQL(TPB: array of byte; sql: AnsiString; SQLDialect: integer; params: array of const): IResults; overload;
    function ExecuteSQL(transaction: ITransaction; sql: AnsiString; SQLDialect: integer; params: array of const): IResults; overload;
    function ExecuteSQL(TPB: array of byte; sql: AnsiString; params: array of const): IResults; overload;
    function ExecuteSQL(transaction: ITransaction; sql: AnsiString; params: array of const): IResults; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer;
                             params: array of const): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(sql: AnsiString): IResultSet; overload;
    function OpenCursorAtStart(sql: AnsiString;
                             params: array of const): IResultSet; overload;
    function Prepare(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer): IStatement; overload; virtual; abstract;
    function Prepare(transaction: ITransaction; sql: AnsiString): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: AnsiString;
                       aSQLDialect: integer; GenerateParamNames: boolean=false): IStatement; overload; virtual; abstract;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: AnsiString;
                       GenerateParamNames: boolean=false): IStatement; overload;
    function GetEventHandler(Events: TStrings): IEvents; overload; virtual; abstract;
    function GetEventHandler(Event: AnsiString): IEvents; overload;

    function GetSQLDialect: integer;
    function OpenBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob; overload; virtual; abstract;
    function OpenBlob(transaction: ITransaction; Field: ISQLData; BPB: IBPB=nil): IBlob; overload;
    property SQLDialect: integer read FSQLDialect;
    property DPB: IDPB read FDPB;
public
    {Character Sets}
  function GetCharsetName(CharSetID: integer): AnsiString;
  function CharSetID2CodePage(CharSetID: integer; var CodePage: TSystemCodePage): boolean;
  function CodePage2CharSetID(CodePage: TSystemCodePage; var CharSetID: integer): boolean;
  function CharSetName2CharSetID(CharSetName: AnsiString; var CharSetID: integer): boolean;
  function CharSetWidth(CharSetID: integer; var Width: integer): boolean;
  procedure RegisterCharSet(CharSetName: AnsiString; CodePage: TSystemCodePage;
    AllowReverseLookup:boolean; out CharSetID: integer);
  property HasDefaultCharSet: boolean read FHasDefaultCharSet;
  property CharSetID: integer read FCharSetID;
  property CodePage: TSystemCodePage read FCodePage;
  end;

implementation

uses FBMessages, FBTransaction;

const
  CharSetMap: array [0..69] of TCharsetMap = (
  (CharsetID: 0; CharSetName: 'NONE'; CharSetWidth: 1; CodePage: CP_ACP; AllowReverseLookup: true),
  (CharsetID: 1; CharSetName: 'OCTETS'; CharSetWidth: 1; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 2; CharSetName: 'ASCII'; CharSetWidth: 1; CodePage: CP_ASCII; AllowReverseLookup: true),
  (CharsetID: 3; CharSetName: 'UNICODE_FSS'; CharSetWidth: 3; CodePage: CP_UTF8; AllowReverseLookup: false),
  (CharsetID: 4; CharSetName: 'UTF8'; CharSetWidth: 4; CodePage: CP_UTF8; AllowReverseLookup: true),
  (CharsetID: 5; CharSetName: 'SJIS_0208'; CharSetWidth: 2; CodePage: 20932; AllowReverseLookup: true),
  (CharsetID: 6; CharSetName: 'EUCJ_0208'; CharSetWidth: 2; CodePage: 20932; AllowReverseLookup: true),
  (CharsetID: 7; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 8; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 9; CharSetName: 'DOS737'; CharSetWidth: 1; CodePage: 737; AllowReverseLookup: true),
  (CharsetID: 10; CharSetName: 'DOS437'; CharSetWidth: 1; CodePage: 437; AllowReverseLookup: true),
  (CharsetID: 11; CharSetName: 'DOS850'; CharSetWidth: 1; CodePage: 850; AllowReverseLookup: true),
  (CharsetID: 12; CharSetName: 'DOS865'; CharSetWidth: 1; CodePage: 865; AllowReverseLookup: true),
  (CharsetID: 13; CharSetName: 'DOS860'; CharSetWidth: 1; CodePage: 860; AllowReverseLookup: true),
  (CharsetID: 14; CharSetName: 'DOS863'; CharSetWidth: 1; CodePage: 863; AllowReverseLookup: true),
  (CharsetID: 15; CharSetName: 'DOS775'; CharSetWidth: 1; CodePage: 775; AllowReverseLookup: true),
  (CharsetID: 16; CharSetName: 'DOS858'; CharSetWidth: 1; CodePage: 858; AllowReverseLookup: true),
  (CharsetID: 17; CharSetName: 'DOS862'; CharSetWidth: 1; CodePage: 862; AllowReverseLookup: true),
  (CharsetID: 18; CharSetName: 'DOS864'; CharSetWidth: 1; CodePage: 864; AllowReverseLookup: true),
  (CharsetID: 19; CharSetName: 'NEXT'; CharSetWidth: 1; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 20; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 21; CharSetName: 'ISO8859_1'; CharSetWidth: 1; CodePage: 28591; AllowReverseLookup: true),
  (CharsetID: 22; CharSetName: 'ISO8859_2'; CharSetWidth: 1; CodePage: 28592; AllowReverseLookup: true),
  (CharsetID: 23; CharSetName: 'ISO8859_3'; CharSetWidth: 1; CodePage: 28593; AllowReverseLookup: true),
  (CharsetID: 24; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 25; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 26; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 27; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 28; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 29; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 30; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 31; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 32; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 33; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 34; CharSetName: 'ISO8859_4'; CharSetWidth: 1; CodePage: 28594; AllowReverseLookup: true),
  (CharsetID: 35; CharSetName: 'ISO8859_5'; CharSetWidth: 1; CodePage: 28595; AllowReverseLookup: true),
  (CharsetID: 36; CharSetName: 'ISO8859_6'; CharSetWidth: 1; CodePage: 28596; AllowReverseLookup: true),
  (CharsetID: 37; CharSetName: 'ISO8859_7'; CharSetWidth: 1; CodePage: 28597; AllowReverseLookup: true),
  (CharsetID: 38; CharSetName: 'ISO8859_8'; CharSetWidth: 1; CodePage: 28598; AllowReverseLookup: true),
  (CharsetID: 39; CharSetName: 'ISO8859_9'; CharSetWidth: 1; CodePage: 28599; AllowReverseLookup: true),
  (CharsetID: 40; CharSetName: 'ISO8859_13'; CharSetWidth: 1; CodePage: 28603; AllowReverseLookup: true),
  (CharsetID: 41; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 42; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 43; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 44; CharSetName: 'KSC_5601'; CharSetWidth: 2; CodePage: 949; AllowReverseLookup: true),
  (CharsetID: 45; CharSetName: 'DOS852'; CharSetWidth: 1; CodePage: 852; AllowReverseLookup: true),
  (CharsetID: 46; CharSetName: 'DOS857'; CharSetWidth: 1; CodePage: 857; AllowReverseLookup: true),
  (CharsetID: 47; CharSetName: 'DOS861'; CharSetWidth: 1; CodePage: 861; AllowReverseLookup: true),
  (CharsetID: 48; CharSetName: 'DOS866'; CharSetWidth: 1; CodePage: 866; AllowReverseLookup: true),
  (CharsetID: 49; CharSetName: 'DOS869'; CharSetWidth: 1; CodePage: 869; AllowReverseLookup: true),
  (CharsetID: 50; CharSetName: 'CYRL'; CharSetWidth: 1; CodePage: 1251; AllowReverseLookup: true),
  (CharsetID: 51; CharSetName: 'WIN1250'; CharSetWidth: 1; CodePage: 1250; AllowReverseLookup: true),
  (CharsetID: 52; CharSetName: 'WIN1251'; CharSetWidth: 1; CodePage: 1251; AllowReverseLookup: true),
  (CharsetID: 53; CharSetName: 'WIN1252'; CharSetWidth: 1; CodePage: 1252; AllowReverseLookup: true),
  (CharsetID: 54; CharSetName: 'WIN1253'; CharSetWidth: 1; CodePage: 1253; AllowReverseLookup: true),
  (CharsetID: 55; CharSetName: 'WIN1254'; CharSetWidth: 1; CodePage: 1254; AllowReverseLookup: true),
  (CharsetID: 56; CharSetName: 'BIG_5'; CharSetWidth: 2; CodePage: 950; AllowReverseLookup: true),
  (CharsetID: 57; CharSetName: 'GB_2312'; CharSetWidth: 2; CodePage: 936; AllowReverseLookup: true),
  (CharsetID: 58; CharSetName: 'WIN1255'; CharSetWidth: 1; CodePage: 1255; AllowReverseLookup: true),
  (CharsetID: 59; CharSetName: 'WIN1256'; CharSetWidth: 1; CodePage: 1256; AllowReverseLookup: true),
  (CharsetID: 60; CharSetName: 'WIN1257'; CharSetWidth: 1; CodePage: 1257; AllowReverseLookup: true),
  (CharsetID: 61; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 62; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 63; CharSetName: 'KOI8R'; CharSetWidth: 1; CodePage: 20866; AllowReverseLookup: true),
  (CharsetID: 64; CharSetName: 'KOI8U'; CharSetWidth: 1; CodePage: 21866; AllowReverseLookup: true),
  (CharsetID: 65; CharSetName: 'WIN1258'; CharSetWidth: 1; CodePage: 1258; AllowReverseLookup: true),
  (CharsetID: 66; CharSetName: 'TIS620'; CharSetWidth: 1; CodePage: 874; AllowReverseLookup: true),
  (CharsetID: 67; CharSetName: 'GBK'; CharSetWidth: 2; CodePage: 936; AllowReverseLookup: true),
  (CharsetID: 68; CharSetName: 'CP943C'; CharSetWidth: 2; CodePage: 943; AllowReverseLookup: true),
  (CharsetID: 69; CharSetName: 'GB18030'; CharSetWidth: 4; CodePage: 54936; AllowReverseLookup: true)
);




{ TFBAttachment }

constructor TFBAttachment.Create(DatabaseName: AnsiString; DPB: IDPB;
  RaiseExceptionOnConnectError: boolean);
begin
  inherited Create;
  FFirebirdAPI := FirebirdAPI; {Keep reference to interface}
  FSQLDialect := 3;
  FDatabaseName := DatabaseName;
  FDPB := DPB;
  SetLength(FUserCharSetMap,0);
  FRaiseExceptionOnConnectError := RaiseExceptionOnConnectError;
end;

function TFBAttachment.GenerateCreateDatabaseSQL(DatabaseName: AnsiString;  aDPB: IDPB): AnsiString;
var CreateParams: AnsiString;
    DPBItem: IDPBItem;
begin
  CreateParams := '';

  if aDPB <> nil then
  begin
    DPBItem :=  aDPB.Find(isc_dpb_user_name);
    if DPBItem <> nil then
      CreateParams := CreateParams + ' USER ''' + DPBItem.AsString + '''';

    DPBItem :=  aDPB.Find(isc_dpb_password);
    if DPBItem <> nil then
      CreateParams := CreateParams + ' Password ''' + DPBItem.AsString + '''';

    DPBItem :=  aDPB.Find(isc_dpb_page_size);
    if DPBItem <> nil then
      CreateParams := CreateParams + ' PAGE_SIZE ' + DPBItem.AsString;

    DPBItem :=  aDPB.Find(isc_dpb_lc_ctype);
    if DPBItem <> nil then
      CreateParams := CreateParams + ' DEFAULT CHARACTER SET ' + DPBItem.AsString;

    DPBItem :=  aDPB.Find(isc_dpb_sql_dialect);
    if DPBItem <> nil then
      FSQLDialect := DPBItem.AsInteger;
  end;

  Result := 'CREATE DATABASE ''' + DatabaseName + ''' ' + CreateParams; {do not localize}
end;

procedure TFBAttachment.EndAllTransactions;
var i: integer;
    intf: TInterfacedObject;
begin
  for i := 0 to InterfaceCount - 1 do
  begin
    intf := GetInterface(i);
    if (intf <> nil) and  (intf is TFBTransaction) then
      TFBTransaction(intf).DoDefaultTransactionEnd(true);
  end;
end;

procedure TFBAttachment.SetParameters(SQLParams: ISQLParams;
  params: array of const);
var i: integer;
begin
  if SQLParams.Count <> Length(params) then
    IBError(ibxeInvalidParamCount,[SQLParams.Count,Length(params)]);

  for i := 0 to High(params) do
  begin
    case params[i].vtype of
      vtinteger    :
        SQLParams[i].AsInteger := params[i].vinteger;
      vtboolean    :
        SQLParams[i].AsBoolean :=  params[i].vboolean;
      vtchar       :
        SQLParams[i].AsString := params[i].vchar;
      vtextended   :
        SQLParams[i].AsDouble := params[i].VExtended^;
      vtCurrency:
        SQLParams[i].AsDouble := params[i].VCurrency^;
      vtString     :
        SQLParams[i].AsString := params[i].VString^;
      vtPChar      :
        SQLParams[i].AsString := strpas(params[i].VPChar);
      vtAnsiString :
        SQLParams[i].AsString := AnsiString(params[i].VAnsiString^);
      vtVariant:
        SQLParams[i].AsVariant := params[i].VVariant^;
    else
        IBError(ibxeInvalidVariantType,[nil]);
    end;
  end;
end;

destructor TFBAttachment.Destroy;
begin
  Disconnect(true);
  inherited Destroy;
end;

function TFBAttachment.getDPB: IDPB;
begin
  Result := FDPB;
end;

function TFBAttachment.AllocateBPB: IBPB;
begin
  Result := TBPB.Create;
end;

procedure TFBAttachment.ExecImmediate(TPB: array of byte; sql: AnsiString;
  aSQLDialect: integer);
begin
  ExecImmediate(StartTransaction(TPB,taCommit),sql,aSQLDialect);
end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: AnsiString);
begin
  ExecImmediate(transaction,sql,FSQLDialect);
end;

procedure TFBAttachment.ExecImmediate(TPB: array of byte; sql: AnsiString);
begin
  ExecImmediate(StartTransaction(TPB,taCommit),sql,FSQLDialect);
end;

function TFBAttachment.ExecuteSQL(TPB: array of byte; sql: AnsiString;
  SQLDialect: integer; params: array of const): IResults;
begin
  Result := ExecuteSQL(StartTransaction(TPB,taCommit),sql,FSQLDialect,params);
end;

function TFBAttachment.ExecuteSQL(transaction: ITransaction; sql: AnsiString;
  SQLDialect: integer; params: array of const): IResults;
begin
  with Prepare(transaction,sql,SQLDialect) do
  begin
    SetParameters(SQLParams,params);
    Result := Execute;
  end;
end;

function TFBAttachment.ExecuteSQL(TPB: array of byte; sql: AnsiString;
  params: array of const): IResults;
begin
   Result := ExecuteSQL(StartTransaction(TPB,taCommit),sql,params);
end;

function TFBAttachment.ExecuteSQL(transaction: ITransaction; sql: AnsiString;
  params: array of const): IResults;
begin
  with Prepare(transaction,sql,FSQLDialect) do
  begin
    SetParameters(SQLParams,params);
    Result := Execute;
  end;
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect,[]);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer; params: array of const): IResultSet;
var Statement: IStatement;
begin
  CheckHandle;
  Statement := Prepare(transaction,sql,aSQLDialect);
  SetParameters(Statement.SQLParams,params);
  Result := Statement.OpenCursor;
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: AnsiString
  ): IResultSet;
begin
  Result := OpenCursor(transaction,sql,FSQLDialect,[]);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: AnsiString;
  params: array of const): IResultSet;
begin
  Result := OpenCursor(transaction,sql,FSQLDialect,params);
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: AnsiString; aSQLDialect: integer): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect,[]);
  Result.FetchNext;
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: AnsiString; aSQLDialect: integer; params: array of const): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect,params);
  Result.FetchNext;
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction; sql: AnsiString
  ): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect,[]);
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: AnsiString; params: array of const): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect,params);
end;

function TFBAttachment.OpenCursorAtStart(sql: AnsiString): IResultSet;
begin
  Result := OpenCursorAtStart(sql,[]);
end;

function TFBAttachment.OpenCursorAtStart(sql: AnsiString;
  params: array of const): IResultSet;
begin
  Result := OpenCursorAtStart(StartTransaction([isc_tpb_read,isc_tpb_wait,isc_tpb_concurrency],taCommit),sql,FSQLDialect,params);
end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: AnsiString
  ): IStatement;
begin
  Result := Prepare(transaction,sql,FSQLDialect);
end;

function TFBAttachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: AnsiString; GenerateParamNames: boolean): IStatement;
begin
  Result := PrepareWithNamedParameters(transaction,sql,FSQLDialect,GenerateParamNames);
end;

function TFBAttachment.GetEventHandler(Event: AnsiString): IEvents;
var S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Add(Event);
    Result := GetEventHandler(S);
  finally
    S.Free;
  end;
end;

function TFBAttachment.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

function TFBAttachment.OpenBlob(transaction: ITransaction; Field: ISQLData;
  BPB: IBPB): IBlob;
begin
  Result := OpenBlob(Transaction,Field.GetBlobMetadata, Field.AsQuad,BPB);
end;

function TFBAttachment.GetCharsetName(CharSetID: integer): AnsiString;
var i: integer;
begin
  Result := '';
  if (CharSetID >= Low(CharSetMap)) and (CharSetID <= High(CharSetMap)) and
                                  (CharSetMap[CharSetID].CharSetID = CharSetID) then
    begin
      Result := CharSetMap[CharSetID].CharSetName;
      Exit;
    end;

  for i := 0 to Length(FUserCharSetMap) - 1 do
    if FUserCharSetMap[i].CharSetID = CharSetID then
    begin
      Result := FUserCharSetMap[i].CharSetName;
      Exit;
    end;
end;

function TFBAttachment.CharSetID2CodePage(CharSetID: integer;
  var CodePage: TSystemCodePage): boolean;
var i: integer;
begin
  Result := (CharSetID >= Low(CharSetMap)) and (CharSetID <= High(CharSetMap))
               and (CharSetMap[CharSetID].CharSetID = CharSetID);
  if Result then
    begin
      CodePage := CharSetMap[CharSetID].CodePage;
      Result := true;
      Exit;
    end;

  for i := 0 to Length(FUserCharSetMap) - 1 do
    if FUserCharSetMap[i].CharSetID = CharSetID then
    begin
      CodePage := FUserCharSetMap[i].CodePage;
      Result := true;
      Exit;
    end;
end;

function TFBAttachment.CodePage2CharSetID(CodePage: TSystemCodePage;
  var CharSetID: integer): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(CharSetMap) to High(CharSetMap) do
    if (CharSetMap[i].AllowReverseLookup) and (CharSetMap[i].CodePage = CodePage) then
    begin
      CharSetID := CharSetMap[i].CharSetID;
      Result := true;
      Exit;
    end;

  for i := 0 to Length(FUserCharSetMap) - 1 do
    if (FUserCharSetMap[i].AllowReverseLookup) and (FUserCharSetMap[i].CodePage = CodePage) then
    begin
      CharSetID := FUserCharSetMap[i].CharSetID;
      Result := true;
      Exit;
    end;
end;

function TFBAttachment.CharSetName2CharSetID(CharSetName: AnsiString;
  var CharSetID: integer): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(CharSetMap) to High(CharSetMap) do
    if AnsiCompareStr(CharSetMap[i].CharSetName, CharSetName) = 0 then
    begin
      CharSetID := CharSetMap[i].CharSetID;
      Result := true;
      Exit;
    end;

    for i := 0 to Length(FUserCharSetMap) - 1 do
      if AnsiCompareStr(FUserCharSetMap[i].CharSetName, CharSetName) = 0 then
      begin
        CharSetID := FUserCharSetMap[i].CharSetID;
        Result := true;
        Exit;
      end;
end;

function TFBAttachment.CharSetWidth(CharSetID: integer; var Width: integer
  ): boolean;
var i: integer;
begin
  Result := (CharSetID >= Low(CharSetMap)) and (CharSetID <= High(CharSetMap))
               and (CharSetMap[CharSetID].CharSetID = CharSetID);
  if Result then
    begin
      Width := CharSetMap[CharSetID].CharSetWidth;
      Result := true;
      Exit;
    end;

  for i := 0 to Length(FUserCharSetMap) - 1 do
    if FUserCharSetMap[i].CharSetID = CharSetID then
    begin
      Width := FUserCharSetMap[i].CharSetWidth;
      Result := true;
      Exit;
    end;
end;

const
  sqlLookupCharSet = 'Select RDB$CHARACTER_SET_ID, RDB$BYTES_PER_CHARACTER From RDB$CHARACTER_SETS '+
                     'Where RDB$SYSTEM_FLAG = 0 and RDB$CHARACTER_SET_NAME = UPPER(?)';

procedure TFBAttachment.RegisterCharSet(CharSetName: AnsiString;
  CodePage: TSystemCodePage; AllowReverseLookup: boolean; out CharSetID: integer
  );
var CharSets: IResultSet;
    idx: integer;
begin
  if CharSetName2CharSetID(CharSetName,CharSetID) then
    IBError(ibxeCharacterSetExists,[CharSetName]);

  CharSets := OpenCursorAtStart(sqlLookupCharSet,[CharSetName]);
  if CharSets.IsEof then
    IBError(ibxeUnknownUserCharSet,[CharSetName]);

  idx := Length(FUserCharSetMap);
  SetLength(FUserCharSetMap,idx+1);
  FUserCharSetMap[idx].AllowReverseLookup := AllowReverseLookup;
  FUserCharSetMap[idx].CharSetID := CharSets[0].AsInteger;
  FUserCharSetMap[idx].CharSetName := AnsiUpperCase(CharSetName);
  FUserCharSetMap[idx].CharSetWidth := CharSets[1].AsInteger;
  FUserCharSetMap[idx].CodePage := CodePage;
  CharSetID := CharSets[0].AsInteger;
end;

end.

