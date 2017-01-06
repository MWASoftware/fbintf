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
 *  The Original Code is (C) 2016 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
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
{    Associates Ltd 2011 - 2015                                                }
{                                                                        }
{************************************************************************}
unit FB25Statement;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

{This unit is hacked from IBSQL and contains the code for managing an XSQLDA and
 SQLVars, along with statement preparation, execution and cursor management.
 Most of the SQLVar code has been moved to unit FBSQLData. Client access is
 provided through interface rather than direct access to the XSQLDA and XSQLVar
 objects.}

{
  Note on reference counted interfaces.
  ------------------------------------

  TFB25Statement manages both an input and an output SQLDA through the TIBXINPUTSQLDA
  and TIBXOUTPUTSQLDA objects. As pure objects, these are explicitly destroyed
  when the statement is destroyed.

  However, IResultSet is an interface and is returned when a cursor is opened and
  has a reference for the TIBXOUTPUTSQLDA. The   user may discard their reference
  to the IStatement while still using the   IResultSet. This would be a problem if t
  he underlying TFB25Statement object and its TIBXOUTPUTSQLDA is destroyed while
  still leaving the TIBXResultSet object in place. Calls to (e.g.)   FetchNext would fail.

  To avoid this problem, TResultsSet objects  have a reference to the IStatement
  interface of the TFB25Statement object. Thus, as long as these "copies" exist,
  the owning statement is not destroyed even if the user discards their reference
  to the statement. Note: the TFB25Statement does not have a reference to the TIBXResultSet
  interface. This way circular references are avoided.

  To avoid an IResultSet interface being kept too long and no longer synchronised
  with the query, each statement includes a prepare sequence number, incremented
  each time the query is prepared. When the IResultSet interface is created, it
  noted the current prepare sequence number. Whe an IResult interface is accessed
  it checks this number against the statement's current prepare sequence number.
  If not the same, an error is raised.

  A similar strategy is used for the IMetaData, IResults and ISQLParams interfaces.
}

interface

uses
  Classes, SysUtils, IB,  FBClientAPI, FB25ClientAPI, FB25Transaction, FB25Attachment,
  IBHeader, IBExternals, FBSQLData, FBOutputBlock, FBStatement, FBActivityMonitor;

type
  TFB25Statement = class;
  TIBXSQLDA = class;

   { TIBXSQLVAR }

  TIBXSQLVAR = class(TSQLVarData)
  private
    FStatement: TFB25Statement;
    FBlob: IBlob;             {Cache references}
    FArray: IArray;
    FNullIndicator: short;
    FOwnsSQLData: boolean;
    FBlobMetaData: IBlobMetaData;
    FArrayMetaData: IArrayMetaData;
    FXSQLVAR: PXSQLVAR;       { Points to the PXSQLVAR in the owner object }
  protected
    function GetSQLType: cardinal; override;
    function GetSubtype: integer; override;
    function GetAliasName: string;  override;
    function GetFieldName: string; override;
    function GetOwnerName: string;  override;
    function GetRelationName: string;  override;
    function GetScale: integer; override;
    function GetCharSetID: cardinal; override;
    function GetCodePage: TSystemCodePage; override;
    function GetIsNull: Boolean;   override;
    function GetIsNullable: boolean; override;
    function GetSQLData: PChar;  override;
    function GetDataLength: cardinal; override;
    procedure SetIsNull(Value: Boolean); override;
    procedure SetIsNullable(Value: Boolean);  override;
    procedure SetSQLData(AValue: PChar; len: cardinal); override;
    procedure SetScale(aValue: integer); override;
    procedure SetDataLength(len: cardinal); override;
    procedure SetSQLType(aValue: cardinal); override;
    procedure SetCharSetID(aValue: cardinal); override;
  public
    constructor Create(aParent: TIBXSQLDA; aIndex: integer);
    procedure FreeSQLData;
    procedure RowChange; override;
    function GetAsArray(Array_ID: TISC_QUAD): IArray; override;
    function GetAsBlob(Blob_ID: TISC_QUAD; BPB: IBPB): IBlob; override;
    function GetArrayMetaData: IArrayMetaData; override;
    function GetBlobMetaData: IBlobMetaData; override;
    function CreateBlob: IBlob; override;
    procedure Initialize; override;

    property Statement: TFB25Statement read FStatement;
  end;

  TIBXINPUTSQLDA = class;

  { TIBXSQLDA }

  TIBXSQLDA = class(TSQLDataArea)
  private
    FCount: Integer; {Columns in use - may be less than inherited columns}
    FSize: Integer;  {Number of TIBXSQLVARs in column list}
    FXSQLDA: PXSQLDA;
    FTransactionSeqNo: integer;
    function GetRecordSize: Integer;
    function GetXSQLDA: PXSQLDA;
  protected
    FStatement: TFB25Statement;
    function GetTransactionSeqNo: integer; override;
    procedure FreeXSQLDA;
    function GetStatement: IStatement; override;
    function GetPrepareSeqNo: integer; override;
    procedure SetCount(Value: Integer); override;
  public
    constructor Create(aStatement: TFB25Statement);
    destructor Destroy; override;
    function CheckStatementStatus(Request: TStatementStatus): boolean; override;
    function ColumnsInUseCount: integer; override;
    function GetTransaction: TFB25Transaction; virtual;
    procedure Initialize; override;
    function StateChanged(var ChangeSeqNo: integer): boolean; override;
    property AsXSQLDA: PXSQLDA read GetXSQLDA;
    property Count: Integer read FCount write SetCount;
    property RecordSize: Integer read GetRecordSize;
    property Statement: TFB25Statement read FStatement;
  end;

  { TIBXINPUTSQLDA }

  TIBXINPUTSQLDA = class(TIBXSQLDA)
  public
    procedure Bind;
    function IsInputDataArea: boolean; override;
  end;


  { TIBXOUTPUTSQLDA }

  TIBXOUTPUTSQLDA = class(TIBXSQLDA)
  private
     FTransaction: TFB25Transaction; {transaction used to execute the statement}
  public
    procedure Bind;
    function GetTransaction: TFB25Transaction; override;
    procedure GetData(index: integer; var aIsNull: boolean; var len: short;
      var data: PChar); override;
    function IsInputDataArea: boolean; override;
  end;

  { TResultSet }

  TResultSet = class(TResults,IResultSet)
  private
    FResults: TIBXOUTPUTSQLDA;
    FCursorSeqNo: integer;
  public
    constructor Create(aResults: TIBXOUTPUTSQLDA);
    destructor Destroy; override;
    {IResultSet}
    function FetchNext: boolean;
    function GetCursorName: string;
    function GetTransaction: ITransaction; override;
    function IsEof: boolean;
    procedure Close;
  end;

  { TFB25Statement }

  TFB25Statement = class(TFBStatement,IStatement)
  private
    FDBHandle: TISC_DB_HANDLE;
    FHandle: TISC_STMT_HANDLE;
    FSQLParams: TIBXINPUTSQLDA;
    FSQLRecord: TIBXOUTPUTSQLDA;
    FCursor: String;               { Cursor name...}
    FCursorSeqNo: integer;
    procedure GetPerfCounters(var counters: TPerfStatistics);
  protected
    procedure CheckHandle; override;
    procedure GetDsqlInfo(info_request: byte; buffer: ISQLInfoResults); override;
    procedure InternalPrepare; override;
    function InternalExecute(aTransaction: ITransaction): IResults; override;
    function InternalOpenCursor(aTransaction: ITransaction): IResultSet; override;
    procedure FreeHandle; override;
    procedure InternalClose(Force: boolean); override;
  public
    constructor Create(Attachment: TFB25Attachment; Transaction: ITransaction;
      sql: string; aSQLDialect: integer);
    constructor CreateWithParameterNames(Attachment: TFB25Attachment;
      Transaction: ITransaction; sql: string; aSQLDialect: integer; GenerateParamNames: boolean);
    destructor Destroy; override;
    function FetchNext: boolean;

  public
    {IStatement}
    function GetSQLParams: ISQLParams; override;
    function GetMetaData: IMetaData; override;
    function GetPlan: String;
    function IsPrepared: boolean;
    function CreateBlob(column: TColumnMetaData): IBlob; override;
    function CreateArray(column: TColumnMetaData): IArray; override;
    procedure SetRetainInterfaces(aValue: boolean); override;
    property Handle: TISC_STMT_HANDLE read FHandle;

end;

implementation

uses IBUtils, FBMessages, FBBlob, FB25Blob, variants, IBErrorCodes, FBArray, FB25Array
  {$IFDEF UNIX}, BaseUnix {$ENDIF};


{ TIBXSQLVAR }

function TIBXSQLVAR.GetSQLType: cardinal;
begin
  result := FXSQLVAR^.sqltype and (not 1);
end;

function TIBXSQLVAR.GetSubtype: integer;
begin
  if GetSQLType = SQL_BLOB then
    result := FXSQLVAR^.sqlsubtype
  else
    result := 0;
end;

function TIBXSQLVAR.GetAliasName: string;
begin
  result := strpas(FXSQLVAR^.aliasname);
end;

function TIBXSQLVAR.GetFieldName: string;
begin
  result := strpas(FXSQLVAR^.sqlname);
end;

function TIBXSQLVAR.GetOwnerName: string;
begin
  result := strpas(FXSQLVAR^.ownname);
end;

function TIBXSQLVAR.GetRelationName: string;
begin
  result := strpas(FXSQLVAR^.relname);
end;

function TIBXSQLVAR.GetScale: integer;
begin
  if GetSQLType = SQL_BLOB then
    result := 0
  else
    result := FXSQLVAR^.sqlscale;
end;

function TIBXSQLVAR.GetCharSetID: cardinal;
begin
  result := 0;
  case SQLType of
  SQL_VARYING, SQL_TEXT:
      {see http://firebirdsql.org/rlsnotesh/rlsnotes210.html}
      result := FXSQLVAR^.sqlsubtype and $FF;

  SQL_BLOB:
    if (SQLSubType = 1)  then
      {see http://firebirdsql.org/rlsnotesh/rlsnotes210.html}
      result := FXSQLVAR^.sqlscale;

  SQL_ARRAY:
    if (GetRelationName <> '') and (GetFieldName <> '') then
      result := GetArrayMetaData.GetCharSetID;
  end;
end;

function TIBXSQLVAR.GetCodePage: TSystemCodePage;
begin
  result := CP_NONE;
  with FirebirdClientAPI do
     CharSetID2CodePage(GetCharSetID,result);
end;

function TIBXSQLVAR.GetIsNull: Boolean;
begin
  result := IsNullable and (FNullIndicator = -1);
end;

function TIBXSQLVAR.GetIsNullable: boolean;
begin
  result := (FXSQLVAR^.sqltype and 1 = 1);
end;

function TIBXSQLVAR.GetSQLData: PChar;
begin
  Result := FXSQLVAR^.sqldata;
end;

function TIBXSQLVAR.GetDataLength: cardinal;
begin
  Result := FXSQLVAR^.sqllen;
end;

function TIBXSQLVAR.GetArrayMetaData: IArrayMetaData;
begin
  if GetSQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if FArrayMetaData = nil then
    FArrayMetaData := TFB25ArrayMetaData.Create(FStatement.GetAttachment as TFB25Attachment,
                  FStatement.GetTransaction as TFB25Transaction,
                  GetRelationName,GetFieldName);
  Result := FArrayMetaData;
end;

function TIBXSQLVAR.GetBlobMetaData: IBlobMetaData;
begin
  if GetSQLType <> SQL_BLOB then
    IBError(ibxeInvalidDataConversion,[nil]);

  if FBlobMetaData = nil then
    FBlobMetaData := TFB25BlobMetaData.Create(FStatement.GetAttachment as TFB25Attachment,
                FStatement.GetTransaction as TFB25Transaction,
                GetRelationName,GetFieldName,GetSubType);
  (FBlobMetaData as TFBBlobMetaData).SetCharSetID(GetCharSetID);
  Result := FBlobMetaData;
end;

function TIBXSQLVAR.GetAsArray(Array_ID: TISC_QUAD): IArray;
begin
  if SQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if IsNull then
    Result := nil
  else
  begin
    if FArray = nil then
      FArray := TFB25Array.Create(FStatement.GetAttachment as TFB25Attachment,
                                  TIBXSQLDA(Parent).GetTransaction,
                                  GetArrayMetaData,Array_ID);
    Result := FArray;
  end;
end;

function TIBXSQLVAR.GetAsBlob(Blob_ID: TISC_QUAD; BPB: IBPB): IBlob;
begin
  if FBlob <> nil then
    Result := FBlob
  else
  begin
    if SQLType <>  SQL_BLOB then
        IBError(ibxeInvalidDataConversion, [nil]);
    if IsNull then
      Result := nil
    else
      Result := TFB25Blob.Create(FStatement.GetAttachment as TFB25Attachment,
                               TIBXSQLDA(Parent).GetTransaction,
                               GetBlobMetaData,
                               Blob_ID,BPB);
    FBlob := Result;
  end;
end;

function TIBXSQLVAR.CreateBlob: IBlob;
begin
  Result := TFB25Blob.Create(FStatement.GetAttachment as TFB25Attachment,
                 FStatement.GetTransaction as TFB25Transaction,GetSubType,GetCharSetID,nil);
end;

procedure TIBXSQLVAR.Initialize;
begin
  inherited Initialize;
  FOwnsSQLData := true;
  with FirebirdClientAPI, FXSQLVar^ do
  begin
    case sqltype and (not 1) of
      SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP,
      SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT, SQL_BOOLEAN,
      SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: begin
        if (sqllen = 0) then
          { Make sure you get a valid pointer anyway
           select '' from foo }
          IBAlloc(sqldata, 0, 1)
        else
          IBAlloc(sqldata, 0, sqllen)
      end;
      SQL_VARYING: begin
        IBAlloc(sqldata, 0, sqllen + 2);
      end;
      else
        IBError(ibxeUnknownSQLDataType, [sqltype and (not 1)])
    end;
    if (sqltype and 1 = 1) then
    begin
      sqlInd := @FNullIndicator;
      FNullIndicator := -1;
    end
    else
      sqlInd :=  nil;
  end;
end;

procedure TIBXSQLVAR.SetIsNull(Value: Boolean);
begin
  if Value then
  begin
    IsNullable := true;
    FNullIndicator := -1;
    Changed;
  end
  else
    if ((not Value) and IsNullable) then
    begin
      FNullIndicator := 0;
      Changed;
    end;
end;

procedure TIBXSQLVAR.SetIsNullable(Value: Boolean);
begin
  if (Value <> IsNullable) then
  begin
    if Value then
    begin
      FXSQLVAR^.sqltype := FXSQLVAR^.sqltype or 1;
      FNullIndicator := 0;
      FXSQLVAR^.sqlInd := @FNullIndicator;
    end
    else
    begin
      FXSQLVAR^.sqltype := FXSQLVAR^.sqltype and (not 1);
      FXSQLVAR^.sqlind := nil;
    end;
  end;
end;

procedure TIBXSQLVAR.SetSQLData(AValue: PChar; len: cardinal);
begin
  if FOwnsSQLData then
    FreeMem(FXSQLVAR^.sqldata);
  FXSQLVAR^.sqldata := AValue;
  FXSQLVAR^.sqllen := len;
  FOwnsSQLData := false;
end;

procedure TIBXSQLVAR.SetScale(aValue: integer);
begin
  FXSQLVAR^.sqlscale := aValue;
end;

procedure TIBXSQLVAR.SetDataLength(len: cardinal);
begin
  if not FOwnsSQLData then
    FXSQLVAR^.sqldata := nil;
  FXSQLVAR^.sqllen := len;
  with FirebirdClientAPI do
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
  FOwnsSQLData := true;
end;

procedure TIBXSQLVAR.SetSQLType(aValue: cardinal);
begin
  FXSQLVAR^.sqltype := aValue or (FXSQLVAR^.sqltype and 1);
end;

procedure TIBXSQLVAR.SetCharSetID(aValue: cardinal);
begin
  if aValue <> GetCharSetID then
  case SQLType of
  SQL_VARYING, SQL_TEXT:
      FXSQLVAR^.sqlsubtype := (aValue and $FF) or (FXSQLVAR^.sqlsubtype and not $FF);

  SQL_BLOB,
  SQL_ARRAY:
    IBError(ibxeInvalidDataConversion,[nil]);
  end;
end;

constructor TIBXSQLVAR.Create(aParent: TIBXSQLDA; aIndex: integer);
begin
  inherited Create(aParent,aIndex);
  FStatement := aParent.Statement;
end;

procedure TIBXSQLVAR.FreeSQLData;
begin
  if FOwnsSQLData then
    FreeMem(FXSQLVAR^.sqldata);
  FXSQLVAR^.sqldata := nil;
  FOwnsSQLData := true;
end;

procedure TIBXSQLVAR.RowChange;
begin
  inherited RowChange;
  FBlob := nil;
  FArray := nil;
end;


{ TResultSet }

constructor TResultSet.Create(aResults: TIBXOUTPUTSQLDA);
begin
  inherited Create(aResults);
  FResults := aResults;
  FCursorSeqNo := aResults.FStatement.FCursorSeqNo;
end;

destructor TResultSet.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TResultSet.FetchNext: boolean;
var i: integer;
begin
  CheckActive;
  Result := FResults.FStatement.FetchNext;
  if Result then
    for i := 0 to getCount - 1 do
      FResults.Column[i].RowChange;
end;

function TResultSet.GetCursorName: string;
begin
  Result := FResults.FStatement.FCursor;
end;

function TResultSet.GetTransaction: ITransaction;
begin
  Result := FResults.GetTransaction;
end;

function TResultSet.IsEof: boolean;
begin
  Result := FResults.FStatement.FEof;
end;

procedure TResultSet.Close;
begin
  if FCursorSeqNo = FResults.FStatement.FCursorSeqNo then
    FResults.FStatement.Close;
end;

{ TIBXINPUTSQLDA }

procedure TIBXINPUTSQLDA.Bind;
begin
  if Count = 0 then
    Count := 1;
  with Firebird25ClientAPI do
  begin
    if (FXSQLDA <> nil) then
       if isc_dsql_describe_bind(StatusVector, @(FStatement.Handle), FStatement.SQLDialect,
                                    FXSQLDA) > 0 then
         IBDataBaseError;

    if FXSQLDA^.sqld > FXSQLDA^.sqln then
    begin
      Count := FXSQLDA^.sqld;
      if isc_dsql_describe_bind(StatusVector, @(FStatement.Handle), FStatement.SQLDialect,
                                   FXSQLDA) > 0 then
        IBDataBaseError;
    end
    else
    if FXSQLDA^.sqld = 0 then
      Count := 0;
  end;
  Initialize;
end;

function TIBXINPUTSQLDA.IsInputDataArea: boolean;
begin
  Result := true;
end;

{ TIBXOUTPUTSQLDA }

procedure TIBXOUTPUTSQLDA.Bind;
begin
  { Allocate an initial output descriptor (with one column) }
  Count := 1;
  with Firebird25ClientAPI do
  begin
    { Using isc_dsql_describe, get the right size for the columns... }
    if isc_dsql_describe(StatusVector, @(FStatement.Handle), FStatement.SQLDialect, FXSQLDA) > 0 then
      IBDataBaseError;

    if FXSQLDA^.sqld > FXSQLDA^.sqln then
    begin
      Count := FXSQLDA^.sqld;
      if isc_dsql_describe(StatusVector, @(FStatement.Handle), FStatement.SQLDialect, FXSQLDA) > 0 then
        IBDataBaseError;
    end
    else
    if FXSQLDA^.sqld = 0 then
      Count := 0;
  end;
  Initialize;
  SetUniqueRelationName;
end;

function TIBXOUTPUTSQLDA.GetTransaction: TFB25Transaction;
begin
  Result := FTransaction;
end;

procedure TIBXOUTPUTSQLDA.GetData(index: integer; var aIsNull:boolean; var len: short;
  var data: PChar);
begin
  with TIBXSQLVAR(Column[index]), FXSQLVAR^ do
  begin
    aIsNull := (sqltype and 1 = 1) and (FNullIndicator = -1);
    data := sqldata;
    len := sqllen;
    if not IsNull and ((sqltype and (not 1)) = SQL_VARYING) then
    begin
      with FirebirdClientAPI do
        len := DecodeInteger(data,2);
      Inc(data,2);
    end;
  end;
end;

function TIBXOUTPUTSQLDA.IsInputDataArea: boolean;
begin
  Result := false;
end;

{ TIBXSQLDA }
constructor TIBXSQLDA.Create(aStatement: TFB25Statement);
begin
  inherited Create;
  FStatement := aStatement;
  FSize := 0;
//  writeln('Creating ',ClassName);
end;

destructor TIBXSQLDA.Destroy;
begin
  FreeXSQLDA;
//  writeln('Destroying ',ClassName);
  inherited Destroy;
end;

function TIBXSQLDA.CheckStatementStatus(Request: TStatementStatus): boolean;
begin
  Result := false;
  case Request of
  ssPrepared:
    Result := FStatement.IsPrepared;

  ssExecuteResults:
    Result :=FStatement.FSingleResults;

  ssCursorOpen:
    Result := FStatement.FOpen;

  ssBOF:
    Result := FStatement.FBOF;

  ssEOF:
    Result := FStatement.FEOF;
  end;
end;

function TIBXSQLDA.ColumnsInUseCount: integer;
begin
  Result := FCount;
end;

function TIBXSQLDA.GetRecordSize: Integer;
begin
  result := SizeOf(TIBXSQLDA) + XSQLDA_LENGTH(FSize);
end;

function TIBXSQLDA.GetXSQLDA: PXSQLDA;
begin
  result := FXSQLDA;
end;

function TIBXSQLDA.GetTransactionSeqNo: integer;
begin
  Result := FTransactionSeqNo;
end;

procedure TIBXSQLDA.Initialize;
begin
  if FXSQLDA <> nil then
    inherited Initialize;
end;

function TIBXSQLDA.StateChanged(var ChangeSeqNo: integer): boolean;
begin
  Result := FStatement.ChangeSeqNo <> ChangeSeqNo;
  if Result then
    ChangeSeqNo := FStatement.ChangeSeqNo;
end;

function TIBXSQLDA.GetTransaction: TFB25Transaction;
begin
  Result := FStatement.GetTransaction as TFB25Transaction;
end;

procedure TIBXSQLDA.SetCount(Value: Integer);
var
  i, OldSize: Integer;
  p : PXSQLVAR;
begin
  FCount := Value;
  if FCount = 0 then
    FUniqueRelationName := ''
  else
  begin
    if FSize > 0 then
      OldSize := XSQLDA_LENGTH(FSize)
    else
      OldSize := 0;
    if Count > FSize then
    begin
      Firebird25ClientAPI.IBAlloc(FXSQLDA, OldSize, XSQLDA_LENGTH(Count));
      SetLength(FColumnList, FCount);
      FXSQLDA^.version := SQLDA_VERSION1;
      p := @FXSQLDA^.sqlvar[0];
      for i := 0 to Count - 1 do
      begin
        if i >= FSize then
          FColumnList[i] := TIBXSQLVAR.Create(self,i);
        TIBXSQLVAR(Column[i]).FXSQLVAR := p;
        p := Pointer(PChar(p) + sizeof(FXSQLDA^.sqlvar));
      end;
      FSize := inherited Count;
    end;
    if FSize > 0 then
    begin
      FXSQLDA^.sqln := Value;
      FXSQLDA^.sqld := Value;
    end;
  end;
end;

procedure TIBXSQLDA.FreeXSQLDA;
var i: integer;
begin
  if FXSQLDA <> nil then
  begin
//    writeln('SQLDA Cleanup');
    for i := 0 to Count - 1 do
      TIBXSQLVAR(Column[i]).FreeSQLData;
    FreeMem(FXSQLDA);
    FXSQLDA := nil;
  end;
  for i := 0 to FSize - 1  do
    TIBXSQLVAR(Column[i]).Free;
  SetLength(FColumnList,0);
  FSize := 0;
end;

function TIBXSQLDA.GetStatement: IStatement;
begin
  Result := FStatement;
end;

function TIBXSQLDA.GetPrepareSeqNo: integer;
begin
  Result := FStatement.FPrepareSeqNo;
end;

{ TFB25Statement }

procedure TFB25Statement.GetPerfCounters(var counters: TPerfStatistics);
var DBInfo: IDBInformation;
    i: integer;
{$IFDEF UNIX}
  times: tms;
{$ENDIF}
begin
  {$IFDEF UNIX}
  FpTimes(times);
  counters[psUserTime] := times.tms_utime;
  {$ELSE}
  counters[psUserTime] := 0;
  {$ENDIF}
  counters[psRealTime] := Int64(TimeStampToMSecs(DateTimeToTimeStamp(Now)));

  DBInfo := GetAttachment.GetDBInformation([isc_info_reads,isc_info_writes,
         isc_info_fetches, isc_info_num_buffers, isc_info_current_memory,
         isc_info_max_memory]);
  if DBInfo <> nil then
  begin
    for i := 0 to DBInfo.Count - 1 do
    with DBInfo[i] do
    case getItemType of
    isc_info_reads:
      counters[psReads] := AsInteger;
    isc_info_writes:
      counters[psWrites] := AsInteger;
    isc_info_fetches:
      counters[psFetches] := AsInteger;
    isc_info_num_buffers:
      counters[psBuffers] := AsInteger;
    isc_info_current_memory:
      counters[psCurrentMemory] := AsInteger;
    isc_info_max_memory:
      counters[psMaxMemory] := AsInteger;
    end;
  end;
end;

procedure TFB25Statement.CheckHandle;
begin
  if FHandle = nil then
    IBError(ibxeInvalidStatementHandle,[nil]);
end;

procedure TFB25Statement.GetDsqlInfo(info_request: byte; buffer: ISQLInfoResults
  );
begin
  with Firebird25ClientAPI, buffer as TSQLInfoResultsBuffer do
  if isc_dsql_sql_info(StatusVector, @(FHandle), 1, @info_request,
                     GetBufSize, Buffer) > 0 then
    IBDatabaseError;
end;

procedure TFB25Statement.InternalPrepare;
var
  RB: ISQLInfoResults;
  TRHandle: TISC_TR_HANDLE;
begin
  if FPrepared then
    Exit;
  if (FSQL = '') then
    IBError(ibxeEmptyQuery, [nil]);
  try
    CheckTransaction(FTransactionIntf);
    with Firebird25ClientAPI do
    begin
      Call(isc_dsql_alloc_statement2(StatusVector, @(FDBHandle),
                                      @FHandle), True);
      TRHandle := (FTransactionIntf as TFB25Transaction).Handle;
      if FHasParamNames then
      begin
        if FProcessedSQL = '' then
          FSQLParams.PreprocessSQL(FSQL,FGenerateParamNames,FProcessedSQL);
        Call(isc_dsql_prepare(StatusVector, @(TRHandle), @FHandle, 0,
                 PChar(FProcessedSQL), FSQLDialect, nil), True);
      end
      else
        Call(isc_dsql_prepare(StatusVector, @(TRHandle), @FHandle, 0,
                 PChar(FSQL), FSQLDialect, nil), True);
    end;
    { After preparing the statement, query the stmt type and possibly
      create a FSQLRecord "holder" }
    { Get the type of the statement }
    RB := GetDsqlInfo(isc_info_sql_stmt_type);
    if RB.Count > 0 then
      FSQLStatementType := TIBSQLStatementTypes(RB[0].GetAsInteger)
    else
      FSQLStatementType := SQLUnknown;

    case FSQLStatementType of
      SQLGetSegment,
      SQLPutSegment,
      SQLStartTransaction: begin
        FreeHandle;
        IBError(ibxeNotPermitted, [nil]);
      end;
      SQLCommit,
      SQLRollback,
      SQLDDL, SQLSetGenerator,
      SQLInsert, SQLUpdate, SQLDelete, SQLSelect, SQLSelectForUpdate,
      SQLExecProcedure:
      begin
        {set up input sqlda}
        FSQLParams.Bind;

        {setup output sqlda}
        if FSQLStatementType in [SQLSelect, SQLSelectForUpdate,
                        SQLExecProcedure] then
          FSQLRecord.Bind;
      end;
    end;
  except
    on E: Exception do begin
      if (FHandle <> nil) then
        FreeHandle;
      if E is EIBInterBaseError then
        raise EIBInterBaseError.Create(EIBInterBaseError(E).SQLCode,
                                       EIBInterBaseError(E).IBErrorCode,
                                       EIBInterBaseError(E).Message +
                                       sSQLErrorSeparator + FSQL)
      else
        raise;
    end;
  end;
  FPrepared := true;
  FSingleResults := false;
  if RetainInterfaces then
  begin
    SetRetainInterfaces(false);
    SetRetainInterfaces(true);
  end;
  Inc(FPrepareSeqNo);
  Inc(FChangeSeqNo);
  with FTransactionIntf as TFB25Transaction do
  begin
    FSQLParams.FTransactionSeqNo := TransactionSeqNo;
    FSQLRecord.FTransactionSeqNo := TransactionSeqNo;
  end;
end;

function TFB25Statement.InternalExecute(aTransaction: ITransaction): IResults;
var TRHandle: TISC_TR_HANDLE;
begin
  Result := nil;
  FBOF := false;
  FEOF := false;
  FSingleResults := false;
  CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  if aTransaction <> FTransactionIntf then
    AddMonitor(aTransaction as TFB25Transaction);
  if (FSQLParams.FTransactionSeqNo < (FTransactionIntf as TFB25transaction).TransactionSeqNo) then
    IBError(ibxeInterfaceOutofDate,[nil]);

  try
    TRHandle := (aTransaction as TFB25Transaction).Handle;
    with Firebird25ClientAPI do
    begin
      if FCollectStatistics then
        GetPerfCounters(FBeforeStats);

      case FSQLStatementType of
      SQLSelect:
        IBError(ibxeIsAExecuteProcedure,[]);

      SQLExecProcedure:
      begin
        Call(isc_dsql_execute2(StatusVector,
                            @(TRHandle),
                            @FHandle,
                            SQLDialect,
                            FSQLParams.AsXSQLDA,
                            FSQLRecord.AsXSQLDA), True);
        Result := TResults.Create(FSQLRecord);
        FSingleResults := true;
      end
      else
        Call(isc_dsql_execute(StatusVector,
                             @(TRHandle),
                             @FHandle,
                             SQLDialect,
                             FSQLParams.AsXSQLDA), True);

      end;
      if FCollectStatistics then
      begin
        GetPerfCounters(FAfterStats);
        FStatisticsAvailable := true;
      end;
    end;
  finally
    if aTransaction <> FTransactionIntf then
       RemoveMonitor(aTransaction as TFB25Transaction);
  end;
  FExecTransactionIntf := aTransaction;
  Inc(FChangeSeqNo);
end;

function TFB25Statement.InternalOpenCursor(aTransaction: ITransaction
  ): IResultSet;
var TRHandle: TISC_TR_HANDLE;
    GUID : TGUID;
begin
  if FSQLStatementType <> SQLSelect then
   IBError(ibxeIsASelectStatement,[]);

 CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  if aTransaction <> FTransactionIntf then
    AddMonitor(aTransaction as TFB25Transaction);
  if (FSQLParams.FTransactionSeqNo < (FTransactionIntf as TFB25transaction).TransactionSeqNo) then
    IBError(ibxeInterfaceOutofDate,[nil]);

 with Firebird25ClientAPI do
 begin
   if FCollectStatistics then
     GetPerfCounters(FBeforeStats);

   TRHandle := (aTransaction as TFB25Transaction).Handle;
   Call(isc_dsql_execute2(StatusVector,
                       @(TRHandle),
                       @FHandle,
                       SQLDialect,
                       FSQLParams.AsXSQLDA,
                       nil), True);
   if FCursor = '' then
   begin
     CreateGuid(GUID);
     FCursor := GUIDToString(GUID);
     Call(
       isc_dsql_set_cursor_name(StatusVector, @FHandle, PChar(FCursor), 0),
       True);
   end;

   if FCollectStatistics then
   begin
     GetPerfCounters(FAfterStats);
     FStatisticsAvailable := true;
   end;
 end;
 Inc(FCursorSeqNo);
 FSingleResults := false;
 FOpen := True;
 FExecTransactionIntf := aTransaction;
 FBOF := true;
 FEOF := false;
 FSQLRecord.FTransaction := aTransaction as TFB25Transaction;
 FSQLRecord.FTransactionSeqNo := FSQLRecord.FTransaction.TransactionSeqNo;
 Result := TResultSet.Create(FSQLRecord);
 Inc(FChangeSeqNo);
end;

procedure TFB25Statement.FreeHandle;
var
  isc_res: ISC_STATUS;
begin
  Close;
  ReleaseInterfaces;
  try
    if FHandle <> nil then
    with Firebird25ClientAPI do
    begin
      isc_res :=
        Call(isc_dsql_free_statement(StatusVector, @FHandle, DSQL_drop), False);
      if (StatusVector^ = 1) and (isc_res > 0) and (isc_res <> isc_bad_stmt_handle) then
        IBDataBaseError;
    end;
  finally
    FHandle := nil;
    FCursor := '';
    FPrepared := false;
  end;
end;

procedure TFB25Statement.InternalClose(Force: boolean);
var
  isc_res: ISC_STATUS;
begin
  if (FHandle <> nil) and (SQLStatementType = SQLSelect) and FOpen then
  try
    with Firebird25ClientAPI do
    begin
      isc_res := Call(
                   isc_dsql_free_statement(StatusVector, @FHandle, DSQL_close),
                   False);
      if not Force and (StatusVector^ = 1) and (isc_res > 0) and
        not getStatus.CheckStatusVector(
              [isc_bad_stmt_handle, isc_dsql_cursor_close_err]) then
        IBDatabaseError;
    end;
  finally
    if (FSQLRecord.FTransaction <> nil) and (FSQLRecord.FTransaction <> FTransactionIntf) then
      RemoveMonitor(FSQLRecord.FTransaction);
    FOpen := False;
    FExecTransactionIntf := nil;
    FSQLRecord.FTransaction := nil;
    Inc(FChangeSeqNo);
  end;
end;

constructor TFB25Statement.Create(Attachment: TFB25Attachment;
  Transaction: ITransaction; sql: string; aSQLDialect: integer);
begin
  inherited Create(Attachment,Transaction,sql,aSQLDialect);
  FDBHandle := Attachment.Handle;
  FSQLParams := TIBXINPUTSQLDA.Create(self);
  FSQLRecord := TIBXOUTPUTSQLDA.Create(self);
  InternalPrepare;
end;

constructor TFB25Statement.CreateWithParameterNames(Attachment: TFB25Attachment;
  Transaction: ITransaction; sql: string; aSQLDialect: integer;
  GenerateParamNames: boolean);
begin
  inherited CreateWithParameterNames(Attachment,Transaction,sql,aSQLDialect,GenerateParamNames);
  FDBHandle := Attachment.Handle;
  FSQLParams := TIBXINPUTSQLDA.Create(self);
  FSQLRecord := TIBXOUTPUTSQLDA.Create(self);
  InternalPrepare;
end;

destructor TFB25Statement.Destroy;
begin
  inherited Destroy;
  if assigned(FSQLParams) then FSQLParams.Free;
  if assigned(FSQLRecord) then FSQLRecord.Free;
end;

function TFB25Statement.FetchNext: boolean;
var
  fetch_res: ISC_STATUS;
begin
  result := false;
  if not FOpen then
    IBError(ibxeSQLClosed, [nil]);
  if FEOF then
    IBError(ibxeEOF,[nil]);

  with Firebird25ClientAPI do
  begin
    { Go to the next record... }
    fetch_res :=
      Call(isc_dsql_fetch(StatusVector, @FHandle, SQLDialect, FSQLRecord.AsXSQLDA), False);
    if (fetch_res = 100) or (getStatus.CheckStatusVector([isc_dsql_cursor_err])) then
    begin
      FBOF := false;
      FEOF := true;
      Exit; {End of File}
    end
    else
    if (fetch_res > 0) then
    begin
      try
        IBDataBaseError;
      except
        Close;
        raise;
      end;
    end
    else
    begin
      FBOF := false;
      result := true;
    end;
  end;
  FSQLRecord.RowChange;
  if FEOF then
    Inc(FChangeSeqNo);
end;

function TFB25Statement.GetSQLParams: ISQLParams;
begin
  CheckHandle;
  if not HasInterface(0) then
    AddInterface(0,TSQLParams.Create(FSQLParams));
  Result := TSQLParams(GetInterface(0));
end;

function TFB25Statement.GetMetaData: IMetaData;
begin
  CheckHandle;
  if not HasInterface(1) then
    AddInterface(1, TMetaData.Create(FSQLRecord));
  Result := TMetaData(GetInterface(1));
end;

function TFB25Statement.GetPlan: String;
var
    RB: ISQLInfoResults;
begin
  if (not (FSQLStatementType in [SQLSelect, SQLSelectForUpdate,
       {TODO: SQLExecProcedure, }
       SQLUpdate, SQLDelete])) then
    result := ''
  else
  begin
    RB := TSQLInfoResultsBuffer.Create(4*4096);
    GetDsqlInfo(isc_info_sql_get_plan,RB);
     if RB.Count > 0 then
     Result := RB[0].GetAsString;
  end;
end;

function TFB25Statement.CreateBlob(column: TColumnMetaData): IBlob;
begin
  if assigned(column) and (column.SQLType <> SQL_Blob) then
    IBError(ibxeNotABlob,[nil]);
  Result := TFB25Blob.Create(GetAttachment as TFB25Attachment,FExecTransactionIntf as TFB25Transaction,
                                 column.GetBlobMetaData,nil);
end;

function TFB25Statement.CreateArray(column: TColumnMetaData): IArray;
begin
  if assigned(column) and (column.SQLType <> SQL_ARRAY) then
    IBError(ibxeNotAnArray,[nil]);
  Result := TFB25Array.Create(GetAttachment as TFB25Attachment,FExecTransactionIntf as TFB25Transaction,
         column.GetArrayMetaData);
end;

procedure TFB25Statement.SetRetainInterfaces(aValue: boolean);
begin
  inherited SetRetainInterfaces(aValue);
  if HasInterface(1) then
    TMetaData(GetInterface(1)).RetainInterfaces := aValue;
  if HasInterface(0) then
    TSQLParams(GetInterface(0)).RetainInterfaces := aValue;
end;

function TFB25Statement.IsPrepared: boolean;
begin
  Result := FHandle <> nil;
end;

end.

