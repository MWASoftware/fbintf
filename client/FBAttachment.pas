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
  public
    destructor Destroy; override;
    function getDPB: IDPB;
    function AllocateBPB: IBPB;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionAction): ITransaction; overload; virtual; abstract;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionAction): ITransaction; overload; virtual; abstract;
    procedure Disconnect(Force: boolean=false); virtual; abstract;
    procedure ExecImmediate(transaction: ITransaction; sql: string; aSQLDialect: integer); overload; virtual; abstract;
    procedure ExecImmediate(TPB: array of byte; sql: string; aSQLDialect: integer); overload;
    procedure ExecImmediate(transaction: ITransaction; sql: string); overload;
    procedure ExecImmediate(TPB: array of byte; sql: string); overload;
    function OpenCursor(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: string): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string): IResultSet; overload;
    function OpenCursorAtStart(sql: string): IResultSet; overload;
    function Prepare(transaction: ITransaction; sql: string; aSQLDialect: integer): IStatement; overload; virtual; abstract;
    function Prepare(transaction: ITransaction; sql: string): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       aSQLDialect: integer; GenerateParamNames: boolean=false;
                       UniqueParamNames: boolean=false): IStatement; overload; virtual; abstract;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       GenerateParamNames: boolean=false;
                       UniqueParamNames: boolean=false): IStatement; overload;
    function GetEventHandler(Events: TStrings): IEvents; overload; virtual; abstract;
    function GetEventHandler(Event: string): IEvents; overload;

    function GetSQLDialect: integer;
    property SQLDialect: integer read FSQLDialect;
    property HasDefaultCharSet: boolean read FHasDefaultCharSet;
    property CharSetID: integer read FCharSetID;
    property CodePage: TSystemCodePage read FCodePage;
    property DPB: IDPB read FDPB;
  end;

implementation

uses FBMessages;

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

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: string;
  aSQLDialect: integer): IResultSet;
var Statement: IStatement;
begin
  CheckHandle;
  Statement := Prepare(transaction,sql,aSQLDialect);
  Result := Statement.OpenCursor;
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: string
  ): IResultSet;
begin
  Result := OpenCursor(transaction,sql,FSQLDialect);
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: string; aSQLDialect: integer): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect);
  Result.FetchNext;
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction; sql: string
  ): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect);
end;

function TFBAttachment.OpenCursorAtStart(sql: string): IResultSet;
begin
  Result := OpenCursorAtStart(StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit),sql,FSQLDialect);
end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: string
  ): IStatement;
begin
  Result := Prepare(transaction,sql,FSQLDialect);
end;

function TFBAttachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: string; GenerateParamNames: boolean; UniqueParamNames: boolean
  ): IStatement;
begin
  Result := PrepareWithNamedParameters(transaction,sql,FSQLDialect,GenerateParamNames,UniqueParamNames);
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

end.
