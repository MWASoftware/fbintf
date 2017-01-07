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
unit FB30Statement;

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

  TFB30Statement manages both an input and an output SQLDA through the TIBXINPUTSQLDA
  and TIBXOUTPUTSQLDA objects. As pure objects, these are explicitly destroyed
  when the statement is destroyed.

  However, IResultSet is an interface and is returned when a cursor is opened and
  has a reference for the TIBXOUTPUTSQLDA. The   user may discard their reference
  to the IStatement while still using the   IResultSet. This would be a problem if t
  he underlying TFB30Statement object and its TIBXOUTPUTSQLDA is destroyed while
  still leaving the TIBXResultSet object in place. Calls to (e.g.)   FetchNext would fail.

  To avoid this problem, TResultsSet objects  have a reference to the IStatement
  interface of the TFB30Statement object. Thus, as long as these "copies" exist,
  the owning statement is not destroyed even if the user discards their reference
  to the statement. Note: the TFB30Statement does not have a reference to the TIBXResultSet
  interface. This way circular references are avoided.

  To avoid and IResultSet interface being kept to long and no longer synchronised
  with the query, each statement includes a prepare sequence number, incremented
  each time the query is prepared. When the IResultSet interface is created, it
  noted the current prepare sequence number. Whe an IResult interface is accessed
  it checks this number against the statement's current prepare sequence number.
  If not the same, an error is raised.

  A similar strategy is used for the IMetaData, IResults and ISQLParams interfaces.
}

interface

uses
  Classes, SysUtils, Firebird, IB,  FBStatement, FB30ClientAPI, FB30Transaction,
  FB30Attachment,IBExternals, FBSQLData, FBOutputBlock, FBActivityMonitor;

type

  TFB30Statement = class;
  TIBXSQLDA = class;

  { TIBXSQLVAR }

  TIBXSQLVAR = class(TSQLVarData)
  private
    FStatement: TFB30Statement;
    FBlob: IBlob;             {Cache references}
    FArray: IArray;
    FNullIndicator: short;
    FOwnsSQLData: boolean;
    FBlobMetaData: IBlobMetaData;
    FArrayMetaData: IArrayMetaData;

    {SQL Var Type Data}
    FSQLType: cardinal;
    FSQLSubType: integer;
    FSQLData: PChar; {Address of SQL Data in Message Buffer}
    FSQLNullIndicator: PShort; {Address of null indicator}
    FDataLength: integer;
    FNullable: boolean;
    FScale: integer;
    FCharSetID: cardinal;
    FRelationName: string;
    FFieldName: string;

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
    procedure Changed; override;
    procedure RowChange; override;
    procedure FreeSQLData;
    function GetAsArray(Array_ID: TISC_QUAD): IArray; override;
    function GetAsBlob(Blob_ID: TISC_QUAD; BPB: IBPB): IBlob; override;
    function GetArrayMetaData: IArrayMetaData; override;
    function GetBlobMetaData: IBlobMetaData; override;
    function CreateBlob: IBlob; override;
  end;

  { TIBXSQLDA }

  TIBXSQLDA = class(TSQLDataArea)
  private
    FCount: Integer; {Columns in use - may be less than inherited columns}
    FSize: Integer;  {Number of TIBXSQLVARs in column list}
    FMetaData: Firebird.IMessageMetadata;
    FTransactionSeqNo: integer;
  protected
    FStatement: TFB30Statement;
    function GetTransactionSeqNo: integer; override;
    procedure FreeXSQLDA; virtual;
    function GetStatement: IStatement; override;
    function GetPrepareSeqNo: integer; override;
    procedure SetCount(Value: Integer); override;
  public
    constructor Create(aStatement: TFB30Statement);
    destructor Destroy; override;
    procedure Changed; virtual;
    function CheckStatementStatus(Request: TStatementStatus): boolean; override;
    function ColumnsInUseCount: integer; override;
    function GetTransaction: TFB30Transaction; virtual;
    procedure Initialize; override;
    function StateChanged(var ChangeSeqNo: integer): boolean; override;
    property MetaData: Firebird.IMessageMetadata read FMetaData;
    property Count: Integer read FCount write SetCount;
    property Statement: TFB30Statement read FStatement;
  end;

  { TIBXINPUTSQLDA }

  TIBXINPUTSQLDA = class(TIBXSQLDA)
  private
    FMessageBuffer: PChar; {Message Buffer}
    FMsgLength: integer; {Message Buffer length}
    FCurMetaData: Firebird.IMessageMetadata;
    procedure FreeMessageBuffer;
    function GetMessageBuffer: PChar;
    function GetMetaData: Firebird.IMessageMetadata;
    function GetModified: Boolean;
    function GetMsgLength: integer;
    procedure PackBuffer;
  protected
    procedure FreeXSQLDA; override;
  public
    constructor Create(aStatement: TFB30Statement);
    destructor Destroy; override;
    procedure Bind(aMetaData: Firebird.IMessageMetadata);
    procedure Changed; override;
    function IsInputDataArea: boolean; override;
    property MetaData: Firebird.IMessageMetadata read GetMetaData;
    property MessageBuffer: PChar read GetMessageBuffer;
    property MsgLength: integer read GetMsgLength;
  end;

  { TIBXOUTPUTSQLDA }

  TIBXOUTPUTSQLDA = class(TIBXSQLDA)
  private
    FTransaction: TFB30Transaction; {transaction used to execute the statement}
    FMessageBuffer: PChar; {Message Buffer}
    FMsgLength: integer; {Message Buffer length}
  protected
    procedure FreeXSQLDA; override;
  public
    procedure Bind(aMetaData: Firebird.IMessageMetadata);
    procedure GetData(index: integer; var aIsNull: boolean; var len: short;
      var data: PChar); override;
    function IsInputDataArea: boolean; override;
    property MessageBuffer: PChar read FMessageBuffer;
    property MsgLength: integer read FMsgLength;
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

  { TFB30Statement }

  TFB30Statement = class(TFBStatement,IStatement)
  private
    FStatementIntf: Firebird.IStatement;
    FSQLParams: TIBXINPUTSQLDA;
    FSQLRecord: TIBXOUTPUTSQLDA;
    FResultSet: Firebird.IResultSet;
    FCursorSeqNo: integer;
  protected
    procedure CheckHandle; override;
    procedure GetDSQLInfo(info_request: byte; buffer: ISQLInfoResults); override;
    procedure InternalPrepare; override;
    function InternalExecute(aTransaction: ITransaction): IResults; override;
    function InternalOpenCursor(aTransaction: ITransaction): IResultSet; override;
    procedure FreeHandle; override;
    procedure InternalClose(Force: boolean); override;
  public
    constructor Create(Attachment: TFB30Attachment; Transaction: ITransaction;
      sql: string; aSQLDialect: integer);
    constructor CreateWithParameterNames(Attachment: TFB30Attachment; Transaction: ITransaction;
      sql: string;  aSQLDialect: integer; GenerateParamNames: boolean =false);
    destructor Destroy; override;
    function FetchNext: boolean;
    property StatementIntf: Firebird.IStatement read FStatementIntf;

  public
    {IStatement}
    function GetSQLParams: ISQLParams; override;
    function GetMetaData: IMetaData; override;
    function GetPlan: String;
    function IsPrepared: boolean;
    function CreateBlob(column: TColumnMetaData): IBlob; override;
    function CreateArray(column: TColumnMetaData): IArray; override;
    procedure SetRetainInterfaces(aValue: boolean); override;

end;

implementation

uses IBUtils, FBMessages, FBBLob, FB30Blob, variants,  FBArray, FB30Array;

const
  ISQL_COUNTERS = 'CurrentMemory, MaxMemory, RealTime, UserTime, Buffers, Reads, Writes, Fetches';

{ TIBXSQLVAR }

procedure TIBXSQLVAR.Changed;
begin
  inherited Changed;
  TIBXSQLDA(Parent).Changed;
end;

function TIBXSQLVAR.GetSQLType: cardinal;
begin
  Result := FSQLType;
end;

function TIBXSQLVAR.GetSubtype: integer;
begin
  Result := FSQLSubType;
end;

function TIBXSQLVAR.GetAliasName: string;
begin
  with Firebird30ClientAPI do
  begin
    result := strpas(TIBXSQLDA(Parent).MetaData.getAlias(StatusIntf,Index));
    Check4DataBaseError;
  end;
end;

function TIBXSQLVAR.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TIBXSQLVAR.GetOwnerName: string;
begin
  with Firebird30ClientAPI do
  begin
    result := strpas(TIBXSQLDA(Parent).MetaData.getOwner(StatusIntf,Index));
    Check4DataBaseError;
  end;
end;

function TIBXSQLVAR.GetRelationName: string;
begin
  Result := FRelationName;
end;

function TIBXSQLVAR.GetScale: integer;
begin
  Result := FScale;
end;

function TIBXSQLVAR.GetCharSetID: cardinal;
begin
  result := 0;
  case SQLType of
  SQL_VARYING, SQL_TEXT:
      result := FCharSetID;

  SQL_BLOB:
    if (SQLSubType = 1) then
      result := FCharSetID;

  SQL_ARRAY:
    if (FRelationName <> '') and (FFieldName <> '') then
      result := GetArrayMetaData.GetCharSetID
    else
      result := FCharSetID;
  end;
end;

function TIBXSQLVAR.GetCodePage: TSystemCodePage;
begin
  result := CP_NONE;
  with Firebird30ClientAPI do
     CharSetID2CodePage(GetCharSetID,result);
end;

function TIBXSQLVAR.GetIsNull: Boolean;
begin
  Result := IsNullable and (FSQLNullIndicator^ = -1);
end;

function TIBXSQLVAR.GetIsNullable: boolean;
begin
  Result := FSQLNullIndicator <> nil;
end;

function TIBXSQLVAR.GetSQLData: PChar;
begin
  Result := FSQLData;
end;

function TIBXSQLVAR.GetDataLength: cardinal;
begin
  Result := FDataLength;
end;

function TIBXSQLVAR.GetArrayMetaData: IArrayMetaData;
begin
  if GetSQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if FArrayMetaData = nil then
    FArrayMetaData := TFB30ArrayMetaData.Create(FStatement.GetAttachment as TFB30Attachment,
                FStatement.GetTransaction as TFB30Transaction,
                GetRelationName,GetFieldName);
  Result := FArrayMetaData;
end;

function TIBXSQLVAR.GetBlobMetaData: IBlobMetaData;
begin
  if GetSQLType <> SQL_BLOB then
    IBError(ibxeInvalidDataConversion,[nil]);

  if FBlobMetaData = nil then
    FBlobMetaData := TFB30BlobMetaData.Create(FStatement.GetAttachment as TFB30Attachment,
              FStatement.GetTransaction as TFB30Transaction,
              GetRelationName,GetFieldName,
              GetSubType);
  (FBlobMetaData as TFBBlobMetaData).SetCharSetID(GetCharSetID);
  Result := FBlobMetaData;
end;

procedure TIBXSQLVAR.SetIsNull(Value: Boolean);
begin
  if Value then
  begin
    IsNullable := true;
    FNullIndicator := -1;
  end
  else
  if IsNullable then
    FNullIndicator := 0;
  Changed;
end;

procedure TIBXSQLVAR.SetIsNullable(Value: Boolean);
begin
  if Value = IsNullable then Exit;
  if Value then
  begin
    FSQLNullIndicator := @FNullIndicator;
    FNullIndicator := 0;
  end
  else
    FSQLNullIndicator := nil;
end;

procedure TIBXSQLVAR.SetSQLData(AValue: PChar; len: cardinal);
begin
  if FOwnsSQLData then
    FreeMem(FSQLData);
  FSQLData := AValue;
  FDataLength := len;
  FOwnsSQLData := false;
end;

procedure TIBXSQLVAR.SetScale(aValue: integer);
begin
  FScale := aValue;
end;

procedure TIBXSQLVAR.SetDataLength(len: cardinal);
begin
  if not FOwnsSQLData then
    FSQLData := nil;
  FDataLength := len;
  with Firebird30ClientAPI do
    IBAlloc(FSQLData, 0, FDataLength);
  FOwnsSQLData := true;
end;

procedure TIBXSQLVAR.SetSQLType(aValue: cardinal);
begin
  FSQLType := aValue;
end;

procedure TIBXSQLVAR.SetCharSetID(aValue: cardinal);
begin
  FCharSetID := aValue;
end;

constructor TIBXSQLVAR.Create(aParent: TIBXSQLDA; aIndex: integer);
begin
  inherited Create(aParent,aIndex);
  FStatement := aParent.Statement;
end;

procedure TIBXSQLVAR.RowChange;
begin
  inherited;
  FBlob := nil;
  FArray := nil;
end;

procedure TIBXSQLVAR.FreeSQLData;
begin
  if FOwnsSQLData then
    FreeMem(FSQLData);
  FSQLData := nil;
  FOwnsSQLData := true;
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
      FArray := TFB30Array.Create(FStatement.GetAttachment as TFB30Attachment,
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
      Result := TFB30Blob.Create(FStatement.GetAttachment as TFB30Attachment,
                               TIBXSQLDA(Parent).GetTransaction,
                               GetBlobMetaData,
                               Blob_ID,BPB);
    FBlob := Result;
  end;
end;

function TIBXSQLVAR.CreateBlob: IBlob;
begin
  Result := TFB30Blob.Create(FStatement.GetAttachment as TFB30Attachment,
                             FStatement.GetTransaction as TFB30Transaction,
                             GetSubType,GetCharSetID,nil);
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
  IBError(ibxeNotSupported,[nil]);
  Result := '';
end;

function TResultSet.GetTransaction: ITransaction;
begin
  Result := FResults.FTransaction;
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

function TIBXINPUTSQLDA.GetModified: Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to FCount - 1 do
    if Column[i].Modified then
    begin
      result := True;
      exit;
    end;
end;

procedure TIBXINPUTSQLDA.FreeMessageBuffer;
begin
  if FCurMetaData <> nil then
  begin
    FCurMetaData.release;
    FCurMetaData := nil;
  end;
  if FMessageBuffer <> nil then
  begin
    FreeMem(FMessageBuffer);
    FMessageBuffer := nil;
  end;
  FMsgLength := 0;
end;

function TIBXINPUTSQLDA.GetMessageBuffer: PChar;
begin
  PackBuffer;
  Result := FMessageBuffer;
end;

function TIBXINPUTSQLDA.GetMetaData: Firebird.IMessageMetadata;
begin
  PackBuffer;
  Result := FCurMetaData;
end;

function TIBXINPUTSQLDA.GetMsgLength: integer;
begin
  PackBuffer;
  Result := FMsgLength;
end;

procedure TIBXINPUTSQLDA.PackBuffer;
var Builder: Firebird.IMetadataBuilder;
    i: integer;
begin
  if FMsgLength > 0 then Exit;

  with Firebird30ClientAPI do
  begin
    Builder := inherited MetaData.getBuilder(StatusIntf);
    Check4DataBaseError;
    try
      for i := 0 to Count - 1 do
      with TIBXSQLVar(Column[i]) do
      begin
        Builder.setType(StatusIntf,i,FSQLType);
        Check4DataBaseError;
        Builder.setSubType(StatusIntf,i,FSQLSubType);
        Check4DataBaseError;
        Builder.setLength(StatusIntf,i,FDataLength);
        Check4DataBaseError;
        Builder.setCharSet(StatusIntf,i,GetCharSetID);
        Check4DataBaseError;
        Builder.setScale(StatusIntf,i,FScale);
        Check4DataBaseError;
      end;
      FCurMetaData := Builder.getMetadata(StatusIntf);
      Check4DataBaseError;
    finally
      Builder.release;
    end;

    FMsgLength := FCurMetaData.getMessageLength(StatusIntf);
    Check4DataBaseError;

    IBAlloc(FMessageBuffer,0,FMsgLength);

    for i := 0 to Count - 1 do
    with TIBXSQLVar(Column[i]) do
    begin
      if IsNull then
        FillChar((FMessageBuffer + FCurMetaData.getOffset(StatusIntf,i))^,FDataLength,0)
      else
        Move(FSQLData^,(FMessageBuffer + FCurMetaData.getOffset(StatusIntf,i))^,FDataLength);
      Check4DataBaseError;
      if IsNullable then
      begin
        Move(FNullIndicator,(FMessageBuffer + FCurMetaData.getNullOffset(StatusIntf,i))^,sizeof(FNullIndicator));
        Check4DataBaseError;
      end;
    end;
  end;
end;

procedure TIBXINPUTSQLDA.FreeXSQLDA;
begin
  inherited FreeXSQLDA;
  FreeMessageBuffer;
end;

constructor TIBXINPUTSQLDA.Create(aStatement: TFB30Statement);
begin
  inherited Create(aStatement);
  FMessageBuffer := nil;
end;

destructor TIBXINPUTSQLDA.Destroy;
begin
  FreeMessageBuffer;
  inherited Destroy;
end;

procedure TIBXINPUTSQLDA.Bind(aMetaData: Firebird.IMessageMetadata);
var i: integer;
begin
  FMetaData := aMetaData;
  with Firebird30ClientAPI do
  begin
    Count := metadata.getCount(StatusIntf);
    Check4DataBaseError;
    Initialize;

    for i := 0 to Count - 1 do
    with TIBXSQLVar(Column[i]) do
    begin
      FSQLType := aMetaData.getType(StatusIntf,i);
      Check4DataBaseError;
      if FSQLType = SQL_BLOB then
      begin
        FSQLSubType := aMetaData.getSubType(StatusIntf,i);
        Check4DataBaseError;
      end
      else
        FSQLSubType := 0;
      FDataLength := aMetaData.getLength(StatusIntf,i);
      Check4DataBaseError;
      case SQLType of
        SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP,
        SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT, SQL_BOOLEAN,
        SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        begin
          if (FDataLength = 0) then
            { Make sure you get a valid pointer anyway
             select '' from foo }
            IBAlloc(FSQLData, 0, 1)
          else
            IBAlloc(FSQLData, 0, FDataLength)
        end;
        SQL_VARYING:
          IBAlloc(FSQLData, 0, FDataLength + 2);
       else
          IBError(ibxeUnknownSQLDataType, [sqltype and (not 1)])
      end;
      FNullable := aMetaData.isNullable(StatusIntf,i);
      FOwnsSQLData := true;
      Check4DataBaseError;
      FNullIndicator := -1;
      if FNullable then
        FSQLNullIndicator := @FNullIndicator
      else
        FSQLNullIndicator := nil;
      FScale := aMetaData.getScale(StatusIntf,i);
      Check4DataBaseError;
      FCharSetID :=  aMetaData.getCharSet(StatusIntf,i);
      Check4DataBaseError;
    end;
  end;
end;

procedure TIBXINPUTSQLDA.Changed;
begin
  inherited Changed;
  FreeMessageBuffer;
end;

function TIBXINPUTSQLDA.IsInputDataArea: boolean;
begin
  Result := true;
end;

{ TIBXOUTPUTSQLDA }

procedure TIBXOUTPUTSQLDA.FreeXSQLDA;
begin
  inherited FreeXSQLDA;
  FreeMem(FMessageBuffer);
  FMessageBuffer := nil;
  FMsgLength := 0;
end;

procedure TIBXOUTPUTSQLDA.Bind(aMetaData: Firebird.IMessageMetadata);
var i: integer;
begin
  FMetaData := aMetaData;
  with Firebird30ClientAPI do
  begin
    Count := metadata.getCount(StatusIntf);
    Check4DataBaseError;
    Initialize;

    FMsgLength := metaData.getMessageLength(StatusIntf);
    Check4DataBaseError;
    IBAlloc(FMessageBuffer,0,FMsgLength);

    for i := 0 to Count - 1 do
    with TIBXSQLVar(Column[i]) do
    begin
      FSQLType := aMetaData.getType(StatusIntf,i);
      Check4DataBaseError;
      if FSQLType = SQL_BLOB then
      begin
        FSQLSubType := aMetaData.getSubType(StatusIntf,i);
        Check4DataBaseError;
      end
      else
        FSQLSubType := 0;
      FBlob := nil;
      FArray := nil;
      FSQLData := FMessageBuffer + metaData.getOffset(StatusIntf,i);
      Check4DataBaseError;
      FDataLength := aMetaData.getLength(StatusIntf,i);
      Check4DataBaseError;
      FRelationName := strpas(aMetaData.getRelation(StatusIntf,i));
      Check4DataBaseError;
      FFieldName := strpas(aMetaData.getField(StatusIntf,i));
      Check4DataBaseError;
      FNullable := aMetaData.isNullable(StatusIntf,i);
      Check4DataBaseError;
      if FNullable then
      begin
        FSQLNullIndicator := PShort(FMessageBuffer + aMetaData.getNullOffset(StatusIntf,i));
        Check4DataBaseError;
      end
      else
        FSQLNullIndicator := nil;
      FScale := aMetaData.getScale(StatusIntf,i);
      Check4DataBaseError;
      FCharSetID :=  aMetaData.getCharSet(StatusIntf,i);
      Check4DataBaseError;
    end;
  end;
  SetUniqueRelationName;
end;

procedure TIBXOUTPUTSQLDA.GetData(index: integer; var aIsNull: boolean;
  var len: short; var data: PChar);
begin
  with TIBXSQLVAR(Column[index]) do
  begin
    aIsNull := FNullable and (FSQLNullIndicator^ = -1);
    data := FSQLData;
    len := FDataLength;
    if not IsNull and (FSQLType = SQL_VARYING) then
    begin
      with Firebird30ClientAPI do
        len := DecodeInteger(data,2);
      Inc(Data,2);
    end;
  end;
end;

function TIBXOUTPUTSQLDA.IsInputDataArea: boolean;
begin
  Result := false;
end;

{ TIBXSQLDA }
constructor TIBXSQLDA.Create(aStatement: TFB30Statement);
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

procedure TIBXSQLDA.Changed;
begin

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

function TIBXSQLDA.GetTransaction: TFB30Transaction;
begin
  Result := FStatement.GetTransaction as TFB30Transaction;
end;

procedure TIBXSQLDA.Initialize;
begin
  if FMetaData <> nil then
    inherited Initialize;
end;

function TIBXSQLDA.StateChanged(var ChangeSeqNo: integer): boolean;
begin
  Result := FStatement.ChangeSeqNo <> ChangeSeqNo;
  if Result then
    ChangeSeqNo := FStatement.ChangeSeqNo;
end;

procedure TIBXSQLDA.SetCount(Value: Integer);
var
  i: Integer;
begin
  FCount := Value;
  if FCount = 0 then
    FUniqueRelationName := ''
  else
  begin
    SetLength(FColumnList, FCount);
    for i := FSize to FCount - 1 do
      FColumnList[i] := TIBXSQLVAR.Create(self,i);
    FSize := FCount;
  end;
end;

function TIBXSQLDA.GetTransactionSeqNo: integer;
begin
  Result := FTransactionSeqNo;
end;

procedure TIBXSQLDA.FreeXSQLDA;
var i: integer;
begin
  if FMetaData <> nil then
    FMetaData.release;
  FMetaData := nil;
  for i := 0 to Count - 1 do
    TIBXSQLVAR(Column[i]).FreeSQLData;
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

{ TFB30Statement }

procedure TFB30Statement.CheckHandle;
begin
  if FStatementIntf = nil then
    IBError(ibxeInvalidStatementHandle,[nil]);
end;

procedure TFB30Statement.GetDSQLInfo(info_request: byte; buffer: ISQLInfoResults
  );
begin
  with Firebird30ClientAPI, buffer as TSQLInfoResultsBuffer do
  begin
    StatementIntf.getInfo(StatusIntf,1,BytePtr(@info_request),
                     GetBufSize, BytePtr(Buffer));
    Check4DataBaseError;
  end;
end;

procedure TFB30Statement.InternalPrepare;
begin
  if FPrepared then
    Exit;
  if (FSQL = '') then
    IBError(ibxeEmptyQuery, [nil]);
  try
    CheckTransaction(FTransactionIntf);
    with Firebird30ClientAPI do
    begin
      if FHasParamNames then
      begin
        if FProcessedSQL = '' then
          FSQLParams.PreprocessSQL(FSQL,FGenerateParamNames,FProcessedSQL);
        FStatementIntf := (GetAttachment as TFB30Attachment).AttachmentIntf.prepare(StatusIntf,
                            (FTransactionIntf as TFB30Transaction).TransactionIntf,
                            Length(FProcessedSQL),
                            PChar(FProcessedSQL),
                            FSQLDialect,
                            Firebird.IStatement.PREPARE_PREFETCH_METADATA);
      end
      else
      FStatementIntf := (GetAttachment as TFB30Attachment).AttachmentIntf.prepare(StatusIntf,
                          (FTransactionIntf as TFB30Transaction).TransactionIntf,
                          Length(FSQL),
                          PChar(FSQL),
                          FSQLDialect,
                          Firebird.IStatement.PREPARE_PREFETCH_METADATA);
      Check4DataBaseError;
      FSQLStatementType := TIBSQLStatementTypes(FStatementIntf.getType(StatusIntf));
      Check4DataBaseError;

      { Done getting the type }
      case FSQLStatementType of
        SQLGetSegment,
        SQLPutSegment,
        SQLStartTransaction:
        begin
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
          FSQLParams.Bind(FStatementIntf.getInputMetadata(StatusIntf));
          Check4DataBaseError;

          {setup output sqlda}
          if FSQLStatementType in [SQLSelect, SQLSelectForUpdate,
                          SQLExecProcedure] then
            FSQLRecord.Bind(FStatementIntf.getOutputMetadata(StatusIntf));
          Check4DataBaseError;
        end;
      end;
    end;
  except
    on E: Exception do begin
      if (FStatementIntf <> nil) then
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
  with GetTransaction as TFB30Transaction do
  begin
    FSQLParams.FTransactionSeqNo := TransactionSeqNo;
    FSQLRecord.FTransactionSeqNo := TransactionSeqNo;
  end;
  SignalActivity;
  Inc(FChangeSeqNo);
end;

function TFB30Statement.InternalExecute(aTransaction: ITransaction): IResults;
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
    AddMonitor(aTransaction as TFB30Transaction);
  if (FSQLParams.FTransactionSeqNo < (FTransactionIntf as TFB30transaction).TransactionSeqNo) then
    IBError(ibxeInterfaceOutofDate,[nil]);

  try
    with Firebird30ClientAPI do
    begin
      if FCollectStatistics then
      begin
        UtilIntf.getPerfCounters(StatusIntf,
                      (GetAttachment as TFB30Attachment).AttachmentIntf,
                      ISQL_COUNTERS,@FBeforeStats);
        Check4DataBaseError;
      end;

      case FSQLStatementType of
      SQLSelect:
        IBError(ibxeIsAExecuteProcedure,[]);

      SQLExecProcedure:
      begin
        FStatementIntf.execute(StatusIntf,
                               (aTransaction as TFB30Transaction).TransactionIntf,
                               FSQLParams.MetaData,
                               FSQLParams.MessageBuffer,
                               FSQLRecord.MetaData,
                               FSQLRecord.MessageBuffer);
        Check4DataBaseError;

        Result := TResults.Create(FSQLRecord);
        FSingleResults := true;
      end
      else
        FStatementIntf.execute(StatusIntf,
                               (aTransaction as TFB30Transaction).TransactionIntf,
                               FSQLParams.MetaData,
                               FSQLParams.MessageBuffer,
                               nil,
                               nil);
        Check4DataBaseError;
      end;
      if FCollectStatistics then
      begin
        UtilIntf.getPerfCounters(StatusIntf,
                  (GetAttachment as TFB30Attachment).AttachmentIntf,
                  ISQL_COUNTERS, @FAfterStats);
        Check4DataBaseError;
        FStatisticsAvailable := true;
      end;
    end;
  finally
    if aTransaction <> FTransactionIntf then
       RemoveMonitor(aTransaction as TFB30Transaction);
  end;
  FExecTransactionIntf := aTransaction;
  SignalActivity;
  Inc(FChangeSeqNo);
end;

function TFB30Statement.InternalOpenCursor(aTransaction: ITransaction
  ): IResultSet;
begin
  if FSQLStatementType <> SQLSelect then
   IBError(ibxeIsASelectStatement,[]);

 CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  if aTransaction <> FTransactionIntf then
    AddMonitor(aTransaction as TFB30Transaction);
  if (FSQLParams.FTransactionSeqNo < (FTransactionIntf as TFB30transaction).TransactionSeqNo) then
    IBError(ibxeInterfaceOutofDate,[nil]);

 with Firebird30ClientAPI do
 begin
   if FCollectStatistics then
   begin
     UtilIntf.getPerfCounters(StatusIntf,
                             (GetAttachment as TFB30Attachment).AttachmentIntf,
                              ISQL_COUNTERS, @FBeforeStats);
     Check4DataBaseError;
   end;

   FResultSet := FStatementIntf.openCursor(StatusIntf,
                          (aTransaction as TFB30Transaction).TransactionIntf,
                          FSQLParams.MetaData,
                          FSQLParams.MessageBuffer,
                          FSQLRecord.MetaData,
                          0);
   Check4DataBaseError;

   if FCollectStatistics then
   begin
     UtilIntf.getPerfCounters(StatusIntf,
                             (GetAttachment as TFB30Attachment).AttachmentIntf,
                             ISQL_COUNTERS,@FAfterStats);
     Check4DataBaseError;
     FStatisticsAvailable := true;
   end;
 end;
 Inc(FCursorSeqNo);
 FSingleResults := false;
 FOpen := True;
 FExecTransactionIntf := aTransaction;
 FBOF := true;
 FEOF := false;
 FSQLRecord.FTransaction := (aTransaction as TFB30Transaction);
 FSQLRecord.FTransactionSeqNo := FSQLRecord.FTransaction.TransactionSeqNo;
 Result := TResultSet.Create(FSQLRecord);
 SignalActivity;
 Inc(FChangeSeqNo);
end;

procedure TFB30Statement.FreeHandle;
begin
  Close;
  ReleaseInterfaces;
  if FStatementIntf <> nil then
  begin
    FStatementIntf.release;
    FStatementIntf := nil;
    FPrepared := false;
  end;
end;

procedure TFB30Statement.InternalClose(Force: boolean);
begin
  if (FStatementIntf <> nil) and (SQLStatementType = SQLSelect) and FOpen then
  try
    with Firebird30ClientAPI do
    begin
      if FResultSet <> nil then
      begin
        if FSQLRecord.FTransaction.InTransaction and
          (FSQLRecord.FTransactionSeqNo = FSQLRecord.FTransaction.TransactionSeqNo) then
          FResultSet.close(StatusIntf)
        else
          FResultSet.release;
      end;
      FResultSet := nil;
      if not Force then Check4DataBaseError;
    end;
  finally
    if (FSQLRecord.FTransaction <> nil) and (FSQLRecord.FTransaction <> FTransactionIntf) then
      RemoveMonitor(FSQLRecord.FTransaction);
    FOpen := False;
    FExecTransactionIntf := nil;
    FSQLRecord.FTransaction := nil;
  end;
  SignalActivity;
  Inc(FChangeSeqNo);
end;

constructor TFB30Statement.Create(Attachment: TFB30Attachment;
  Transaction: ITransaction; sql: string; aSQLDialect: integer);
begin
  inherited Create(Attachment,Transaction,sql,aSQLDialect);
  FSQLParams := TIBXINPUTSQLDA.Create(self);
  FSQLRecord := TIBXOUTPUTSQLDA.Create(self);
  InternalPrepare;
end;

constructor TFB30Statement.CreateWithParameterNames(
  Attachment: TFB30Attachment; Transaction: ITransaction; sql: string;
  aSQLDialect: integer; GenerateParamNames: boolean);
begin
  inherited CreateWithParameterNames(Attachment,Transaction,sql,aSQLDialect,GenerateParamNames);
  FSQLParams := TIBXINPUTSQLDA.Create(self);
  FSQLRecord := TIBXOUTPUTSQLDA.Create(self);
  InternalPrepare;
end;

destructor TFB30Statement.Destroy;
begin
  inherited Destroy;
  if assigned(FSQLParams) then FSQLParams.Free;
  if assigned(FSQLRecord) then FSQLRecord.Free;
end;

function TFB30Statement.FetchNext: boolean;
var fetchResult: integer;
begin
  result := false;
  if not FOpen then
    IBError(ibxeSQLClosed, [nil]);
  if FEOF then
    IBError(ibxeEOF,[nil]);

  with Firebird30ClientAPI do
  begin
    { Go to the next record... }
    fetchResult := FResultSet.fetchNext(StatusIntf,FSQLRecord.MessageBuffer);
    if fetchResult = Firebird.IStatus.RESULT_NO_DATA then
    begin
      FBOF := false;
      FEOF := true;
      Exit; {End of File}
    end
    else
    if fetchResult <> Firebird.IStatus.RESULT_OK then
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
  SignalActivity;
  if FEOF then
    Inc(FChangeSeqNo);
end;

function TFB30Statement.GetSQLParams: ISQLParams;
begin
  CheckHandle;
  if not HasInterface(0) then
    AddInterface(0,TSQLParams.Create(FSQLParams));
  Result := TSQLParams(GetInterface(0));
end;

function TFB30Statement.GetMetaData: IMetaData;
begin
  CheckHandle;
  if not HasInterface(1) then
    AddInterface(1, TMetaData.Create(FSQLRecord));
  Result := TMetaData(GetInterface(1));
end;

function TFB30Statement.GetPlan: String;
begin
  CheckHandle;
  if (not (FSQLStatementType in [SQLSelect, SQLSelectForUpdate,
       {TODO: SQLExecProcedure, }
       SQLUpdate, SQLDelete])) then
    result := ''
  else
  with Firebird30ClientAPI do
  begin
    Result := FStatementIntf.getPlan(StatusIntf,true);
    Check4DataBaseError;
  end;
end;

function TFB30Statement.CreateBlob(column: TColumnMetaData): IBlob;
begin
  if assigned(column) and (column.SQLType <> SQL_Blob) then
    IBError(ibxeNotABlob,[nil]);
  Result := TFB30Blob.Create(GetAttachment as TFB30Attachment,
                             GetTransaction as TFB30Transaction,
                             column.GetBlobMetaData,nil);
end;

function TFB30Statement.CreateArray(column: TColumnMetaData): IArray;
begin
  if assigned(column) and (column.SQLType <> SQL_ARRAY) then
    IBError(ibxeNotAnArray,[nil]);
  Result := TFB30Array.Create(GetAttachment as TFB30Attachment,
                             GetTransaction as TFB30Transaction,
                             column.GetArrayMetaData);
end;

procedure TFB30Statement.SetRetainInterfaces(aValue: boolean);
begin
  inherited SetRetainInterfaces(aValue);
  if HasInterface(1) then
    TMetaData(GetInterface(1)).RetainInterfaces := aValue;
  if HasInterface(0) then
    TSQLParams(GetInterface(0)).RetainInterfaces := aValue;
end;

function TFB30Statement.IsPrepared: boolean;
begin
  Result := FStatementIntf <> nil;
end;

end.

