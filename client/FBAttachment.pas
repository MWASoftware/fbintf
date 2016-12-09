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

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, IB,  FBParamBlock, FBActivityMonitor;

type

  { TFBAttachment }

  TFBAttachment = class(TActivityHandler)
  private
    FDPB: IDPB;
    FFirebirdAPI: IFirebirdAPI;
  protected
    FDatabaseName: string;
    FRaiseExceptionOnConnectError: boolean;
    FSQLDialect: integer;
    FHasDefaultCharSet: boolean;
    FCharSetID: integer;
    FCodePage: TSystemCodePage;
    constructor Create(DatabaseName: string; DPB: IDPB;
      RaiseExceptionOnConnectError: boolean);
    procedure CheckHandle; virtual; abstract;
    function GenerateCreateDatabaseSQL(DatabaseName: string; aDPB: IDPB): string;
    procedure EndAllTransactions;
    procedure SetParameters(SQLParams: ISQLParams; params: array of const);
  public
    destructor Destroy; override;
    function getDPB: IDPB;
    function AllocateBPB: IBPB;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction; overload; virtual; abstract;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionCompletion): ITransaction; overload; virtual; abstract;
    procedure Disconnect(Force: boolean=false); virtual; abstract;
    procedure ExecImmediate(transaction: ITransaction; sql: string; aSQLDialect: integer); overload; virtual; abstract;
    procedure ExecImmediate(TPB: array of byte; sql: string; aSQLDialect: integer); overload;
    procedure ExecImmediate(transaction: ITransaction; sql: string); overload;
    procedure ExecImmediate(TPB: array of byte; sql: string); overload;
    function ExecuteSQL(TPB: array of byte; sql: string; SQLDialect: integer; params: array of const): IResults; overload;
    function ExecuteSQL(transaction: ITransaction; sql: string; SQLDialect: integer; params: array of const): IResults; overload;
    function ExecuteSQL(TPB: array of byte; sql: string; params: array of const): IResults; overload;
    function ExecuteSQL(transaction: ITransaction; sql: string; params: array of const): IResults; overload;
    function OpenCursor(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: string; aSQLDialect: integer;
                             params: array of const): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: string): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: string;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string; aSQLDialect: integer;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(sql: string): IResultSet; overload;
    function OpenCursorAtStart(sql: string;
                             params: array of const): IResultSet; overload;
    function Prepare(transaction: ITransaction; sql: string; aSQLDialect: integer): IStatement; overload; virtual; abstract;
    function Prepare(transaction: ITransaction; sql: string): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       aSQLDialect: integer; GenerateParamNames: boolean=false): IStatement; overload; virtual; abstract;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       GenerateParamNames: boolean=false): IStatement; overload;
    function GetEventHandler(Events: TStrings): IEvents; overload; virtual; abstract;
    function GetEventHandler(Event: string): IEvents; overload;

    function GetSQLDialect: integer;
    function OpenBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob; virtual; abstract; overload;
    function OpenBlob(transaction: ITransaction; Field: ISQLData; BPB: IBPB=nil): IBlob; overload;
    property SQLDialect: integer read FSQLDialect;
    property HasDefaultCharSet: boolean read FHasDefaultCharSet;
    property CharSetID: integer read FCharSetID;
    property CodePage: TSystemCodePage read FCodePage;
    property DPB: IDPB read FDPB;
  end;

implementation

uses FBMessages, FBTransaction;

{ TFBAttachment }

constructor TFBAttachment.Create(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnConnectError: boolean);
begin
  inherited Create;
  FFirebirdAPI := FirebirdAPI; {Keep reference to interface}
  FSQLDialect := 3;
  FDatabaseName := DatabaseName;
  FDPB := DPB;
  FRaiseExceptionOnConnectError := RaiseExceptionOnConnectError;
end;

function TFBAttachment.GenerateCreateDatabaseSQL(DatabaseName: string;  aDPB: IDPB): string;
var CreateParams: string;
    DPBItem: IDPBItem;
begin
  CreateParams := '';

  if aDPB <> nil then
  begin
    DPBItem :=  aDPB.Find(isc_dpb_user_name);
    if DPBItem <> nil then
      CreateParams += ' USER ''' + DPBItem.AsString + '''';

    DPBItem :=  aDPB.Find(isc_dpb_password);
    if DPBItem <> nil then
      CreateParams += ' Password ''' + DPBItem.AsString + '''';

    DPBItem :=  aDPB.Find(isc_dpb_page_size);
    if DPBItem <> nil then
      CreateParams += ' PAGE_SIZE ' + DPBItem.AsString;

    DPBItem :=  aDPB.Find(isc_dpb_lc_ctype);
    if DPBItem <> nil then
      CreateParams += ' DEFAULT CHARACTER SET ' + DPBItem.AsString;

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

procedure TFBAttachment.ExecImmediate(TPB: array of byte; sql: string;
  aSQLDialect: integer);
begin
  ExecImmediate(StartTransaction(TPB,taCommit),sql,aSQLDialect);
end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: string);
begin
  ExecImmediate(transaction,sql,FSQLDialect);
end;

procedure TFBAttachment.ExecImmediate(TPB: array of byte; sql: string);
begin
  ExecImmediate(StartTransaction(TPB,taCommit),sql,FSQLDialect);
end;

function TFBAttachment.ExecuteSQL(TPB: array of byte; sql: string;
  SQLDialect: integer; params: array of const): IResults;
begin
  Result := ExecuteSQL(StartTransaction(TPB,taCommit),sql,FSQLDialect,params);
end;

function TFBAttachment.ExecuteSQL(transaction: ITransaction; sql: string;
  SQLDialect: integer; params: array of const): IResults;
begin
  with Prepare(transaction,sql,SQLDialect) do
  begin
    SetParameters(SQLParams,params);
    Result := Execute;
  end;
end;

function TFBAttachment.ExecuteSQL(TPB: array of byte; sql: string;
  params: array of const): IResults;
begin
   Result := ExecuteSQL(StartTransaction(TPB,taCommit),sql,params);
end;

function TFBAttachment.ExecuteSQL(transaction: ITransaction; sql: string;
  params: array of const): IResults;
begin
  with Prepare(transaction,sql,FSQLDialect) do
  begin
    SetParameters(SQLParams,params);
    Result := Execute;
  end;
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: string;
  aSQLDialect: integer): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect,[]);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: string;
  aSQLDialect: integer; params: array of const): IResultSet;
var Statement: IStatement;
begin
  CheckHandle;
  Statement := Prepare(transaction,sql,aSQLDialect);
  SetParameters(Statement.SQLParams,params);
  Result := Statement.OpenCursor;
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: string
  ): IResultSet;
begin
  Result := OpenCursor(transaction,sql,FSQLDialect,[]);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: string;
  params: array of const): IResultSet;
begin
  Result := OpenCursor(transaction,sql,FSQLDialect,params);
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: string; aSQLDialect: integer): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect,[]);
  Result.FetchNext;
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: string; aSQLDialect: integer; params: array of const): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect,params);
  Result.FetchNext;
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction; sql: string
  ): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect,[]);
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: string; params: array of const): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect,params);
end;

function TFBAttachment.OpenCursorAtStart(sql: string): IResultSet;
begin
  Result := OpenCursorAtStart(sql,[]);
end;

function TFBAttachment.OpenCursorAtStart(sql: string;
  params: array of const): IResultSet;
begin
  Result := OpenCursorAtStart(StartTransaction([isc_tpb_read,isc_tpb_wait,isc_tpb_concurrency],taCommit),sql,FSQLDialect,params);
end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: string
  ): IStatement;
begin
  Result := Prepare(transaction,sql,FSQLDialect);
end;

function TFBAttachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; GenerateParamNames: boolean): IStatement;
begin
  Result := PrepareWithNamedParameters(transaction,sql,FSQLDialect,GenerateParamNames);
end;

function TFBAttachment.GetEventHandler(Event: string): IEvents;
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

end.

