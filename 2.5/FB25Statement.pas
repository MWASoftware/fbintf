unit FB25Statement;

{$mode objfpc}{$H+}

{ $define ALLOWDIALECT3PARAMNAMES}

{$ifndef ALLOWDIALECT3PARAMNAMES}

{ Even when dialect 3 quoted format parameter names are not supported, IBX still processes
  parameter names case insensitive. This does result in some additional overhead
  due to a call to "AnsiUpperCase". This can be avoided by undefining
  "UseCaseSensitiveParamName" below.

  Note: do not define "UseCaseSensitiveParamName" when "ALLOWDIALECT3PARAMNAMES"
  is defined. This will not give a useful result.
}
{ $define UseCaseSensitiveParamName}
{$endif}
{
  Note on reference counted interfaces.
  ------------------------------------

  TFBStatement manages both an input and an output SQLDA through the TIBXINPUTSQLDA
  and TIBXResultSet objects. Each of these are also reference counted interfaces.
  When TFBStatement is destroyed, its reference to TIBXINPUTSQLDA
  and TIBXResultSet objects is lost and they are also destroyed.

  However, for example, IResultSet is returned when a cursor is opened and the
  user may discard their reference to the IStatement while still using the
  IResultSet. This would be a problem if the underlying TFBStatement object is
  destroyed while still leaving the TIBXResultSet object in place. Calls to (e.g.)
  FetchNext would fail.

  To avoid this problem, copies of TIBXINPUTSQLDA and TIBXResultSet objects are
  returned as interfaces and not the original objects. These copies (but not the
  original have a reference to the IStatement interface of the TFBStatement object.
  Thus, as long as these "copies" exist, the owning statement is not destroyed
  even if the user discards their reference to the statement. Note: the TFBStatement
  does not have a reference to the copy, only to the original. This way circular
  references are avoided.
}

interface

uses
  Classes, SysUtils, IB, FBLibrary, FB25ClientAPI, FB25Transaction, FB25Attachment,
  IBHeader, IBExternals, FB25APIObject, FB25SQLData;

type

  TFBStatement = class;
  TIBXSQLDA = class;

  { TIBXSQLVAR }

  TIBXSQLVAR = class(TSQLDataItem,IFieldMetaData)
  private
    FIndex: Integer;
    FName: String;
    FXSQLVAR: PXSQLVAR;       { Point to the PXSQLVAR in the owner object }
  protected
    function SQLData: PChar; override;
    function GetDataLength: short; override;

  public
    function GetSQLType: short; override;
    function getSubtype: short;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if not alias}
    function GetName: string; override;      {Disambiguated uppercase Field Name}
    function GetScale: short; override;
    function getCharSetID: cardinal;
    function GetIsNull: Boolean; override;
    function GetIsNullable: boolean; override;
    function GetSize: integer;
    property Name: string read GetName;
    property Size: Integer read GetSize;
    property CharSetID: cardinal read getCharSetID;
    property SQLType;
    property SQLSubtype: short read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  end;

  TIBSQLData = class(TIBXSQLVAR,ISQLData);

  TIBXINPUTSQLDA = class;

  { TIBXSQLParam }

  TIBXSQLParam = class(TIBXSQLVAR,ISQLParam)
  private
    FParent: TIBXINPUTSQLDA;
    FChanging: boolean;
    FUniqueName: boolean;
  protected
    procedure Changed; override;
    procedure SetScale(aValue: short); override;
    procedure SetDataLength(len: short); override;
    procedure SetSQLType(aValue: short); override;
  public
    constructor Create(Parent: TIBXSQLDA; aStatement: TFBStatement);
  public

    procedure Clear;
    procedure SetName(Value: string); override;
    procedure SetIsNull(Value: Boolean);  override;
    procedure SetIsNullable(Value: Boolean);  override;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
  end;

  TIBXSQLVARArray = Array of TIBXSQLVAR;

  TIBXSQLDAType = (daInput,daOutput);

  { TIBXSQLDA }

  TIBXSQLDA = class(TInterfacedObject)
  private
    FCount: Integer;
    FSize: Integer;
    FInputSQLDA: boolean;
    FXSQLDA: PXSQLDA;
    FXSQLVARs: TIBXSQLVARArray; { array of IBXQLVARs }
    FUniqueRelationName: String;
    FStatementIntf: IStatement;
    function GetRecordSize: Integer;
    function GetXSQLDA: PXSQLDA;
    procedure SetCount(Value: Integer);
  protected
    FStatement: TFBStatement;
    procedure FreeXSQLDA;
    function CreateSQLVAR: TIBXSQLVAR; virtual; abstract;
    function GetXSQLVARByName(Idx: String): TIBXSQLVAR;
  public
    constructor Create(aStatement: TFBStatement; sqldaType: TIBXSQLDAType);
    constructor Copy(aIBXSQLDA: TIBXSQLDA);
    function VarByName(Idx: String): TIBXSQLVAR;
    function GetUniqueRelationName: string;
    procedure Initialize;
    property AsXSQLDA: PXSQLDA read GetXSQLDA;
    property Count: Integer read FCount write SetCount;
    property RecordSize: Integer read GetRecordSize;
  end;

  { TIBXINPUTSQLDA }

  TIBXINPUTSQLDA = class(TIBXSQLDA,ISQLParams)
  private
    FSQLParamIntf: array of ISQLParam;
    function GetXSQLParam(Idx: Integer): TIBXSQLParam;
  protected
    function CreateSQLVAR: TIBXSQLVAR; override;
  public
    constructor Create(aOwner: TFBStatement);
    constructor Copy(aIBXSQLDA: TIBXINPUTSQLDA);
    destructor Destroy; override;
    procedure Bind;
    procedure SetParamName(FieldName: String; Idx: Integer; UniqueName: boolean = false);
    property Vars[Idx: Integer]: TIBXSQLParam read GetXSQLParam; default;

  public
    {ISQLParams}
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ByName(Idx: String): ISQLParam ;
    function GetModified: Boolean;
 end;

  { TIBXOUTPUTSQLDA }

  TIBXOUTPUTSQLDA = class(TIBXSQLDA, IMetaData)
  private
    FSQLDataIntf: array of ISQLData;
    function GetXSQLVAR(Idx: Integer): TIBSQLData;
  protected
    function CreateSQLVAR: TIBXSQLVAR; override;
  public
    constructor Create(aOwner: TFBStatement);
    constructor Copy(aIBXSQLDA: TIBXOUTPUTSQLDA);
    destructor Destroy; override;
    procedure Bind;
    property Vars[Idx: Integer]: TIBSQLData read GetXSQLVAR; default;

  public
    {IMetaData}
    function getCount: integer;
    function getFieldMetaData(index: integer): IFieldMetaData;
    function ByName(Idx: String): IFieldMetaData;
  end;

  { TIBXResults }

  TIBXResults = class(TIBXOUTPUTSQLDA,IResults)
  public
    {IResults}
    function ByName(Idx: String): ISQLData;
    function getSQLData(index: integer): ISQLData;
 end;

  { TIBXResultSet }

  TIBXResultSet = class(TIBXResults,IResultSet)
  private
    FTransaction: TFBTransaction; {transaction used to execute the statement}
  public
    {IResultSet}
    function FetchNext: boolean;
    function GetCursorName: string;
    procedure Close;
  end;


  { TFBStatement }

  TFBStatement = class(TAPIObject,IStatement)
  private
    FAttachment: TFBAttachment;
    FAttachmentIntf: IAttachment;
    FTransaction: TFBTransaction;
    FTransactionIntf: ITransaction;
    FExecTransactionIntf: ITransaction;
    FParamSet: ISQLParams;
    FResultSet: IResultSet;
    FHandle: TISC_STMT_HANDLE;
    FSQLType: TIBSQLTypes;         { Select, update, delete, insert, create, alter, etc...}
    FSQLDialect: integer;
    FSQLParams: TIBXINPUTSQLDA;
    FSQLParamsRefCount: integer;
    FSQLRecord: TIBXResultSet;
    FSQLRecordRefCount: integer;
    FOpen: boolean;
    FCursor: String;               { Cursor name...}
    FPrepared: boolean;
    FSQL: string;
    FProcessedSQL: string;
    FHasParamNames: boolean;
    FBOF: boolean;
    FEOF: boolean;
    FGenerateParamNames: boolean;
    FUniqueParamNames: boolean;
    procedure CheckTransaction(aTransaction: TFBTransaction);
    procedure CheckHandle;
    procedure InternalPrepare;
    function InternalExecute(aTransaction: TFBTransaction): IResults;
    function InternalOpenCursor(aTransaction: TFBTransaction): IResultSet;
    procedure FreeHandle;
    procedure PreprocessSQL;
  public
    constructor Create(Attachment: TFBAttachment; Transaction: ITransaction;
      sql: string; SQLDialect: integer);
    constructor CreateWithParameterNames(Attachment: TFBAttachment; Transaction: ITransaction;
      sql: string;  SQLDialect: integer; GenerateParamNames: boolean =false; UniqueParamNames: boolean=false);
    destructor Destroy; override;
    procedure Close;
    function FetchNext: boolean;
    procedure TransactionEnding(aTransaction: TFBTransaction);
    property SQLDialect: integer read FSQLDialect;
    property Attachment: TFBAttachment read FAttachment;
    property Transaction: TFBTransaction read FTransaction;

  public
    {IStatement}
    function GetSQLParams: ISQLParams;
    function GetOutMetaData: IMetaData;
    function GetPlan: String;
    function GetRowsAffected(var InsertCount, UpdateCount, DeleteCount: integer): boolean;
    function GetSQLType: TIBSQLTypes;
    function GetSQLInfo(InfoRequest: byte): IDBInformation;
    function GetSQLText: string;
    function IsPrepared: boolean;
    procedure Prepare(aTransaction: ITransaction=nil);
    function Execute(aTransaction: ITransaction=nil): IResults;
    function OpenCursor(aTransaction: ITransaction=nil): IResultSet;
    function CreateBlob: IBlob;
    property Handle: TISC_STMT_HANDLE read FHandle;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLType: TIBSQLTypes read GetSQLType;

end;

implementation

uses IBUtils, FBErrorMessages, FB25Blob, variants, IBErrorCodes, FB25DBInfo;

const
   sSQLErrorSeparator = ' When Executing: ';

{ TIBXSQLParam }

procedure TIBXSQLParam.Changed;
var i: integer;
begin
  inherited Changed;
  if FChanging or (FName = '') or FUniqueName then Exit;
  if (SQLType = SQL_BLOB) or (SQLTYPE = SQL_ARRAY) then
    IBError(ibxeDuplicateParamName,[FName]);

  for i := 0 to FParent.FCount - 1 do
    if FParent[i].FName = FName then
    begin
      FParent[i].FChanging := true;
      try
        FParent[i].AsVariant := AsVariant;
      finally
        FParent[i].FChanging := false;
      end;
    end;
end;

procedure TIBXSQLParam.SetScale(aValue: short);
begin
  FXSQLVAR^.sqlscale := aValue;
end;

procedure TIBXSQLParam.SetDataLength(len: short);
begin
  FXSQLVAR^.sqllen := len;
  with Firebird25ClientAPI do
    IBAlloc(FXSQLVAR^.sqldata, 0, FXSQLVAR^.sqllen);
end;

procedure TIBXSQLParam.SetSQLType(aValue: short);
begin
  FXSQLVAR^.sqltype := aValue or (FXSQLVAR^.sqltype and 1);
end;

constructor TIBXSQLParam.Create(Parent: TIBXSQLDA; aStatement: TFBStatement);
begin
  inherited Create(aStatement);
  FParent := TIBXINPUTSQLDA(Parent);
end;

procedure TIBXSQLParam.Clear;
begin
  IsNull := true;
end;

procedure TIBXSQLParam.SetName(Value: string);
begin
  FName := Value;
end;

procedure TIBXSQLParam.SetIsNull(Value: Boolean);
begin
  if Value then
  begin
    if not IsNullable then
      IsNullable := True;

    if Assigned(FXSQLVAR^.sqlind) then
      FXSQLVAR^.sqlind^ := -1;
    Changed;
  end
  else
    if ((not Value) and IsNullable) then
    begin
      if Assigned(FXSQLVAR^.sqlind) then
        FXSQLVAR^.sqlind^ := 0;
      Changed;
    end;
end;

procedure TIBXSQLParam.SetIsNullable(Value: Boolean);
begin
  if (Value <> IsNullable) then
  begin
    if Value then
    begin
      FXSQLVAR^.sqltype := FXSQLVAR^.sqltype or 1;
      with Firebird25ClientAPI do
        IBAlloc(FXSQLVAR^.sqlind, 0, SizeOf(Short));
    end
    else
    begin
      FXSQLVAR^.sqltype := FXSQLVAR^.sqltype and (not 1);
      ReallocMem(FXSQLVAR^.sqlind, 0);
    end;
  end;
end;

{ TIBXResults }

function TIBXResults.getSQLData(index: integer): ISQLData;
begin
  if FStatement.FBOF then
    IBError(ibxeBOF,[nil]);
  if FStatement.FEOF then
    IBError(ibxeEOF,[nil]);
  Result := Vars[index];
end;

function TIBXResults.ByName(Idx: String): ISQLData;
begin
  if FStatement.FBOF then
    IBError(ibxeBOF,[nil]);
  if FStatement.FEOF then
    IBError(ibxeEOF,[nil]);

  if Count = 0 then
    Result := nil
  else
    Result := TIBSQLData(VarByName(Idx));
end;


{ TIBXResultSet }

function TIBXResultSet.FetchNext: boolean;
begin
  Result := FStatement.FetchNext;
end;

function TIBXResultSet.GetCursorName: string;
begin
  Result := FStatement.FCursor;
end;

procedure TIBXResultSet.Close;
begin
  FStatement.Close;
end;

{ TIBXINPUTSQLDA }

function TIBXINPUTSQLDA.GetModified: Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to FCount - 1 do
    if TIBXSQLParam(FXSQLVARs[i]).Modified then
    begin
      result := True;
      exit;
    end;
end;

function TIBXINPUTSQLDA.GetXSQLParam(Idx: Integer): TIBXSQLParam;
begin
  if (Idx < 0) or (Idx >= FCount) then
    IBError(ibxeXSQLDAIndexOutOfRange, [nil]);
  result := TIBXSQLParam(FXSQLVARs[Idx])
end;

function TIBXINPUTSQLDA.CreateSQLVAR: TIBXSQLVAR;
begin
  Result := TIBXSQLParam.Create(self,FStatement);
  SetLength(FSQLParamIntf,Length(FSQLParamIntf)+1);
  FSQLParamIntf[Length(FSQLParamIntf) - 1] := TIBXSQLParam(Result);
end;

constructor TIBXINPUTSQLDA.Create(aOwner: TFBStatement);
begin
  inherited Create(aOwner,daInput);
  aOwner.FSQLParamsRefCount := 1;
end;

constructor TIBXINPUTSQLDA.Copy(aIBXSQLDA: TIBXINPUTSQLDA);
begin
  inherited Copy(aIBXSQLDA);
  FSQLParamIntf := aIBXSQLDA.FSQLParamIntf;
  Inc(FStatement.FSQLParamsRefCount);
end;

destructor TIBXINPUTSQLDA.Destroy;
begin
  Dec(FStatement.FSQLParamsRefCount);
  if FStatement.FSQLParamsRefCount = 0 then
    FreeXSQLDA;
  writeln('Destroying ',ClassName,',',FStatement.FSQLParamsRefCount);
  inherited Destroy;
end;

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

procedure TIBXINPUTSQLDA.SetParamName(FieldName: String; Idx: Integer;
  UniqueName: boolean);
var
  fn: string;
begin
  {$ifdef UseCaseSensitiveParamName}
  FXSQLVARs[Idx].FName := AnsiUpperCase(FieldName);
  {$else}
  FXSQLVARs[Idx].FName := FieldName;
  {$endif}
  FXSQLVARs[Idx].FIndex := Idx;
  TIBXSQLParam(FXSQLVARs[Idx]).FUniqueName :=  UniqueName
end;

function TIBXINPUTSQLDA.getCount: integer;
begin
  Result := Count;
end;

function TIBXINPUTSQLDA.getSQLParam(index: integer): ISQLParam;
begin
  Result := GetXSQLParam(index);
end;

function TIBXINPUTSQLDA.ByName(Idx: String): ISQLParam;
begin
  Result := TIBXSQLParam(VarByName(Idx));
end;

{ TIBXOUTPUTSQLDA }

function TIBXOUTPUTSQLDA.GetXSQLVAR(Idx: Integer): TIBSQLData;
begin
  if (Idx < 0) or (Idx >= FCount) then
    IBError(ibxeXSQLDAIndexOutOfRange, [nil]);
  result := TIBSQLData(FXSQLVARs[Idx]);
end;

function TIBXOUTPUTSQLDA.CreateSQLVAR: TIBXSQLVAR;
begin
  Result := TIBSQLData.Create(FStatement);
  SetLength(FSQLDataIntf,Length(FSQLDataIntf) + 1);
  FSQLDataIntf[Length(FSQLDataIntf)-1] := TIBSQLData(Result);
end;

constructor TIBXOUTPUTSQLDA.Create(aOwner: TFBStatement);
begin
  inherited Create(aOwner,daOutput);
  aOwner.FSQLRecordRefCount := 1;
end;

constructor TIBXOUTPUTSQLDA.Copy(aIBXSQLDA: TIBXOUTPUTSQLDA);
begin
  inherited Copy(aIBXSQLDA);
  FSQLDataIntf := aIBXSQLDA.FSQLDataIntf;
  Inc(FStatement.FSQLRecordRefCount);
end;

destructor TIBXOUTPUTSQLDA.Destroy;
begin
  Dec(FStatement.FSQLRecordRefCount);
  if FStatement.FSQLRecordRefCount = 0 then
    FreeXSQLDA;
  writeln('Destroying ',ClassName,',',FStatement.FSQLRecordRefCount);
  inherited Destroy;
end;

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
end;

function TIBXOUTPUTSQLDA.ByName(Idx: String): IFieldMetaData;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TIBXSQLVAR(VarByName(Idx));
end;

function TIBXOUTPUTSQLDA.getCount: integer;
begin
  Result := Count;
end;

function TIBXOUTPUTSQLDA.getFieldMetaData(index: integer): IFieldMetaData;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TIBXSQLVAR(Vars[index]);
end;

function TIBXSQLVAR.SQLData: PChar;
begin
  Result := FXSQLVAR^.sqldata;
end;

function TIBXSQLVAR.GetDataLength: short;
begin
  Result := FXSQLVAR^.sqllen;
end;

function TIBXSQLVAR.GetSQLType: short;
begin
  result := FXSQLVAR^.sqltype and (not 1);
end;

function TIBXSQLVAR.getSubtype: short;
begin
  result := FXSQLVAR^.sqltype and (not 1);
end;

function TIBXSQLVAR.getRelationName: string;
begin
  result := strpas(FXSQLVAR^.relname);
end;

function TIBXSQLVAR.getOwnerName: string;
begin
  result := strpas(FXSQLVAR^.ownname);
end;

function TIBXSQLVAR.getSQLName: string;
begin
  result := strpas(FXSQLVAR^.sqlname);
end;

function TIBXSQLVAR.getAliasName: string;
begin
  result := strpas(FXSQLVAR^.aliasname);
end;

function TIBXSQLVAR.GetName: string;
begin
  Result := FName;
end;

function TIBXSQLVAR.GetScale: short;
begin
  result := FXSQLVAR^.sqlscale;
end;

function TIBXSQLVAR.getCharSetID: cardinal;
begin
  case SQLType of
  SQL_VARYING, SQL_TEXT:
    result := SQLSubtype and $FF;

  else
    result := 0;
  end;
end;

function TIBXSQLVAR.GetIsNull: Boolean;
begin
  result := IsNullable and (FXSQLVAR^.sqlind^ = -1);
end;

function TIBXSQLVAR.GetIsNullable: boolean;
begin
  result := (FXSQLVAR^.sqltype and 1 = 1);
end;

function TIBXSQLVAR.GetSize: integer;
begin
  result := FXSQLVAR^.sqllen;
end;

{ TIBXSQLDA }
constructor TIBXSQLDA.Create(aStatement: TFBStatement; sqldaType: TIBXSQLDAType);
begin
  inherited Create;
  FStatement := aStatement;
  FSize := 0;
  FUniqueRelationName := '';
  FInputSQLDA := sqldaType = daInput;
  writeln('Creating ',ClassName);
end;

constructor TIBXSQLDA.Copy(aIBXSQLDA: TIBXSQLDA);
begin
  inherited Create;
  FCount := aIBXSQLDA.FCount;
  FSize := aIBXSQLDA.FSize;
  FInputSQLDA := aIBXSQLDA.FInputSQLDA;
  FXSQLDA := aIBXSQLDA.FXSQLDA;
  FXSQLVARs := aIBXSQLDA.FXSQLVARs;
  FUniqueRelationName := aIBXSQLDA.FUniqueRelationName;
  FStatementIntf := aIBXSQLDA.FStatement;
  FStatement := aIBXSQLDA.FStatement;
end;

function TIBXSQLDA.GetRecordSize: Integer;
begin
  result := SizeOf(TIBXSQLDA) + XSQLDA_LENGTH(FSize);
end;

function TIBXSQLDA.GetXSQLDA: PXSQLDA;
begin
  result := FXSQLDA;
end;

function TIBXSQLDA.VarByName(Idx: String): TIBXSQLVAR;
begin
  result := GetXSQLVARByName(Idx);
  if result = nil then
    IBError(ibxeFieldNotFound, [Idx]);
end;

function TIBXSQLDA.GetUniqueRelationName: string;
begin
  Result := FUniqueRelationName;
end;

procedure TIBXSQLDA.Initialize;

    function FindVarByName(idx: string; limit: integer): TIBXSQLVAR;
    var
       k: integer;
    begin
         for k := 0 to limit do
             if FXSQLVARs[k].FName = idx then
             begin
                  Result := FXSQLVARs[k];
                  Exit;
             end;
         Result := nil;
    end;

var
  i, j, j_len: Integer;
  st: String;
  bUnique: Boolean;
  sBaseName: string;
begin
  bUnique := True;
  if FXSQLDA <> nil then
  begin
    for i := 0 to FCount - 1 do
    begin
      with Firebird25ClientAPI, FXSQLVARs[i].FXSQLVAR^ do
      begin

        {First get the unique relation name, if any}

        if bUnique and (strpas(relname) <> '') then
        begin
          if FUniqueRelationName = '' then
            FUniqueRelationName := strpas(relname)
          else
            if strpas(relname) <> FUniqueRelationName then
            begin
              FUniqueRelationName := '';
              bUnique := False;
            end;
        end;

        {If an output SQLDA then copy the aliasnames to the FName list. Ensure
         that they are all upper case only and disambiguated.
        }

        if not FInputSQLDA then
        begin
          st := Space2Underscore(AnsiUppercase(strpas(aliasname)));
          if st = '' then
          begin
            sBaseName := 'F_'; {do not localize}
            aliasname_length := 2;
            j := 1; j_len := 1;
            st := sBaseName + IntToStr(j);
          end
          else
          begin
            j := 0; j_len := 0;
            sBaseName := st;
          end;

          {Look for other columns with the same name and make unique}

          while FindVarByName(st,i-1) <> nil do
          begin
               Inc(j);
               j_len := Length(IntToStr(j));
               if j_len + Length(sBaseName) > 31 then
                  st := system.Copy(sBaseName, 1, 31 - j_len) + IntToStr(j)
               else
                  st := sBaseName + IntToStr(j);
          end;

          FXSQLVARs[i].FName := st;
        end;

        {Finally initialise the XSQLVAR}

        FXSQLVARs[i].FIndex := i;

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
          IBAlloc(sqlind, 0, SizeOf(Short))
        else
          if (sqlind <> nil) then
            ReallocMem(sqlind, 0);
      end;
    end;
  end;
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
    if FCount > FSize then
    begin
      Firebird25ClientAPI.IBAlloc(FXSQLDA, OldSize, XSQLDA_LENGTH(FCount));
      SetLength(FXSQLVARs, FCount);
      FXSQLDA^.version := SQLDA_VERSION1;
      p := @FXSQLDA^.sqlvar[0];
      for i := 0 to FCount - 1 do
      begin
        if i >= FSize then
          FXSQLVARs[i] := CreateSQLVar;
        FXSQLVARs[i].FXSQLVAR := p;
        p := Pointer(PChar(p) + sizeof(FXSQLDA^.sqlvar));
      end;
      FSize := FCount;
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
    writeln('SQLDA Cleanup');
    for i := 0 to FSize - 1 do
    begin
      FreeMem(FXSQLVARs[i].FXSQLVAR^.sqldata);
      FreeMem(FXSQLVARs[i].FXSQLVAR^.sqlind);
    end;
    FreeMem(FXSQLDA);
    FXSQLDA := nil;
    FXSQLVARs := nil;
  end;
end;

function TIBXSQLDA.GetXSQLVARByName(Idx: String): TIBXSQLVAR;
var
  s: String;
  i: Integer;
begin
  {$ifndef UseCaseSensitiveParamName}
   s := AnsiUpperCase(Idx);
  {$else}
   s := Idx;
  {$endif}
  for i := 0 to FCount - 1 do
    if FXSQLVARs[i].FName = s then
    begin
         Result := FXSQLVARs[i];
         Exit;
    end;
  Result := nil;
end;

{ TFBStatement }

procedure TFBStatement.CheckTransaction(aTransaction: TFBTransaction);
begin
  if (aTransaction = nil) then
    IBError(ibxeTransactionNotAssigned,[]);

  if not aTransaction.InTransaction then
    IBError(ibxeNotInTransaction,[]);

end;

procedure TFBStatement.CheckHandle;
begin
  if FHandle = nil then
    IBError(ibxeInvalidStatementHandle,[nil]);
end;

procedure TFBStatement.InternalPrepare;
var
  DBInfo: IDBInformation;
begin
  if FPrepared then
    Exit;
  if (FSQL = '') then
    IBError(ibxeEmptyQuery, [nil]);
  try
    CheckTransaction(FTransaction);
    with Firebird25ClientAPI do
    begin
      Call(isc_dsql_alloc_statement2(StatusVector, @(FAttachment.Handle),
                                      @FHandle), True);
      if FHasParamNames then
      begin
        if FProcessedSQL = '' then
          PreprocessSQL;
        Call(isc_dsql_prepare(StatusVector, @(FTransaction.Handle), @FHandle, 0,
                 PChar(FProcessedSQL), FSQLDialect, nil), True);
      end
      else
        Call(isc_dsql_prepare(StatusVector, @(FTransaction.Handle), @FHandle, 0,
                 PChar(FSQL), FSQLDialect, nil), True);
    end;
    { After preparing the statement, query the stmt type and possibly
      create a FSQLRecord "holder" }
    { Get the type of the statement }
    DBInfo := TDBInformation.Create(self,byte(isc_info_sql_stmt_type));
    if (DBInfo.getCount > 0) and (DBInfo[0].getItemType = byte(isc_info_sql_stmt_type)) then
      FSQLType := TIBSQLTypes(DBInfo[0].GetAsInteger);

    { Done getting the type }
    case FSQLType of
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
        if FSQLType in [SQLSelect, SQLSelectForUpdate,
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
end;

function TFBStatement.InternalExecute(aTransaction: TFBTransaction): IResults;
begin
  Result := nil;
  FBOF := false;
  FEOF := false;
  CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
  CheckHandle;
  with Firebird25ClientAPI do
  case FSQLType of
  SQLSelect:
    IBError(ibxeIsAExecuteProcedure,[]);

  SQLExecProcedure:
  begin
    Call(isc_dsql_execute2(StatusVector,
                        @(aTransaction.Handle),
                        @FHandle,
                        SQLDialect,
                        FSQLParams.AsXSQLDA,
                        FSQLRecord.AsXSQLDA), True);
    Result := TIBXResults.Copy(FSQLRecord);
  end
  else
    Call(isc_dsql_execute(StatusVector,
                         @(aTransaction.Handle),
                         @FHandle,
                         SQLDialect,
                         FSQLParams.AsXSQLDA), True);

  end;
end;

function TFBStatement.InternalOpenCursor(aTransaction: TFBTransaction
  ): IResultSet;
var ResultSet: TIBXResultSet;
begin
  if FSQLType <> SQLSelect then
   IBError(ibxeIsASelectStatement,[]);

 CheckTransaction(aTransaction);
  if not FPrepared then
    InternalPrepare;
 CheckHandle;
 with Firebird25ClientAPI do
 begin
   Call(isc_dsql_execute2(StatusVector,
                       @(aTransaction.Handle),
                       @FHandle,
                       SQLDialect,
                       FSQLParams.AsXSQLDA,
                       nil), True);
   Call(
     isc_dsql_set_cursor_name(StatusVector, @FHandle, PChar(FCursor), 0),
     True);
 end;
 FOpen := True;
 FExecTransactionIntf := aTransaction;
 FBOF := true;
 FEOF := false;
 ResultSet := TIBXResultSet.Copy(FSQLRecord);
 ResultSet.FTransaction := aTransaction;
 Result := ResultSet;
end;

procedure TFBStatement.FreeHandle;
var
  isc_res: ISC_STATUS;
begin
  try
    { The following two lines merely set the SQLDA count
     variable FCount to 0, but do not deallocate
     That way the allocations can be reused for
     a new query sring in the same SQL instance }
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
  end;
end;

procedure TFBStatement.PreprocessSQL;
var
  cCurChar, cNextChar, cQuoteChar: Char;
  sSQL, sProcessedSQL, sParamName: String;
  i, iLenSQL, iSQLPos: Integer;
  iCurState {$ifdef ALLOWDIALECT3PARAMNAMES}, iCurParamState {$endif}: Integer;
  iParamSuffix: Integer;
  slNames: TStrings;

const
  DefaultState = 0;
  CommentState = 1;
  QuoteState = 2;
  ParamState = 3;
 {$ifdef ALLOWDIALECT3PARAMNAMES}
  ParamDefaultState = 0;
  ParamQuoteState = 1;
  {$endif}

  procedure AddToProcessedSQL(cChar: Char);
  begin
    sProcessedSQL[iSQLPos] := cChar;
    Inc(iSQLPos);
  end;

begin
  sParamName := '';
  slNames := TStringList.Create;
  try
    { Do some initializations of variables }
    iParamSuffix := 0;
    cQuoteChar := '''';
    sSQL := FSQL;
    iLenSQL := Length(sSQL);
    SetString(sProcessedSQL, nil, iLenSQL + 1);
    i := 1;
    iSQLPos := 1;
    iCurState := DefaultState;
    {$ifdef ALLOWDIALECT3PARAMNAMES}
    iCurParamState := ParamDefaultState;
    {$endif}
    { Now, traverse through the SQL string, character by character,
     picking out the parameters and formatting correctly for InterBase }
    while (i <= iLenSQL) do begin
      { Get the current token and a look-ahead }
      cCurChar := sSQL[i];
      if i = iLenSQL then
        cNextChar := #0
      else
        cNextChar := sSQL[i + 1];
      { Now act based on the current state }
      case iCurState of
        DefaultState: begin
          case cCurChar of
            '''', '"': begin
              cQuoteChar := cCurChar;
              iCurState := QuoteState;
            end;
            '?', ':': begin
              iCurState := ParamState;
              AddToProcessedSQL('?');
            end;
            '/': if (cNextChar = '*') then begin
              AddToProcessedSQL(cCurChar);
              Inc(i);
              iCurState := CommentState;
            end;
          end;
        end;
        CommentState: begin
          if (cNextChar = #0) then
            IBError(ibxeSQLParseError, [SEOFInComment])
          else if (cCurChar = '*') then begin
            if (cNextChar = '/') then
              iCurState := DefaultState;
          end;
        end;
        QuoteState: begin
          if cNextChar = #0 then
            IBError(ibxeSQLParseError, [SEOFInString])
          else if (cCurChar = cQuoteChar) then begin
            if (cNextChar = cQuoteChar) then begin
              AddToProcessedSQL(cCurChar);
              Inc(i);
            end else
              iCurState := DefaultState;
          end;
        end;
        ParamState:
        begin
          { collect the name of the parameter }
          {$ifdef ALLOWDIALECT3PARAMNAMES}
          if iCurParamState = ParamDefaultState then
          begin
            if cCurChar = '"' then
              iCurParamState := ParamQuoteState
            else
            {$endif}
            if (cCurChar in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']) then
                sParamName := sParamName + cCurChar
            else if FGenerateParamNames then
            begin
              sParamName := 'IBXParam' + IntToStr(iParamSuffix); {do not localize}
              Inc(iParamSuffix);
              iCurState := DefaultState;
              slNames.AddObject(sParamName,self); //Note local convention
                                                  //add pointer to self to mark entry
              sParamName := '';
            end
            else
              IBError(ibxeSQLParseError, [SParamNameExpected]);
          {$ifdef ALLOWDIALECT3PARAMNAMES}
          end
          else begin
            { determine if Quoted parameter name is finished }
            if cCurChar = '"' then
            begin
              Inc(i);
              slNames.Add(sParamName);
              SParamName := '';
              iCurParamState := ParamDefaultState;
              iCurState := DefaultState;
            end
            else
              sParamName := sParamName + cCurChar
          end;
          {$endif}
          { determine if the unquoted parameter name is finished }
          if {$ifdef ALLOWDIALECT3PARAMNAMES}(iCurParamState <> ParamQuoteState) and {$endif}
            (iCurState <> DefaultState) then
          begin
            if not (cNextChar in ['A'..'Z', 'a'..'z',
                                  '0'..'9', '_', '$']) then begin
              Inc(i);
              iCurState := DefaultState;
              slNames.Add(sParamName);
              sParamName := '';
            end;
          end;
        end;
      end;
      if iCurState <> ParamState then
        AddToProcessedSQL(sSQL[i]);
      Inc(i);
    end;
    AddToProcessedSQL(#0);
    FSQLParams.Count := slNames.Count;
    for i := 0 to slNames.Count - 1 do
      FSQLParams.SetParamName(slNames[i], i,FUniqueParamNames or (slNames.Objects[i] <> nil));
    FProcessedSQL := sProcessedSQL;
  finally
    slNames.Free;
  end;
end;

constructor TFBStatement.Create(Attachment: TFBAttachment;
  Transaction: ITransaction; sql: string; SQLDialect: integer);
var GUID : TGUID;
begin
  inherited Create;
  FAttachment := Attachment;
  FAttachmentIntf := Attachment;
  FTransaction := transaction as TFBTransaction;
  FTransactionIntf := Transaction;
  AddOwner(FTransaction);
  FSQLDialect := SQLDialect;
  CreateGuid(GUID);
  FCursor := GUIDToString(GUID);
  FSQLParams := TIBXINPUTSQLDA.Create(self);
  FParamSet := FSQLParams;
  FSQLRecord := TIBXResultSet.Create(self);
  FResultSet := FSQLRecord;
  FSQL := sql;
  InternalPrepare;
end;

constructor TFBStatement.CreateWithParameterNames(Attachment: TFBAttachment;
  Transaction: ITransaction; sql: string; SQLDialect: integer;
  GenerateParamNames: boolean; UniqueParamNames: boolean);
begin
  FHasParamNames := true;
  FGenerateParamNames := GenerateParamNames;
  FUniqueParamNames := UniqueParamNames;
  Create(Attachment,Transaction,sql,SQLDialect);
end;

destructor TFBStatement.Destroy;
begin
  Close;
  FreeHandle;
  {Note no need to free reference counted interfaces}
  inherited Destroy;
end;

procedure TFBStatement.Close;
var
  isc_res: ISC_STATUS;
begin
  try
    if (FHandle <> nil) and (SQLType = SQLSelect) and FOpen then
    with Firebird25ClientAPI do
    begin
      isc_res := Call(
                   isc_dsql_free_statement(StatusVector, @FHandle, DSQL_close),
                   False);
      if (StatusVector^ = 1) and (isc_res > 0) and
        not getStatus.CheckStatusVector(
              [isc_bad_stmt_handle, isc_dsql_cursor_close_err]) then
        IBDatabaseError;
    end;
  finally
    FOpen := False;
    FExecTransactionIntf := nil;
    FSQLRecord.FTransaction := nil;
  end;
end;

function TFBStatement.FetchNext: boolean;
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
end;

procedure TFBStatement.TransactionEnding(aTransaction: TFBTransaction);
begin
  if FOpen and (FSQLRecord.FTransaction = aTransaction) then
    Close;

  if FTransaction = aTransaction then
  begin
    FreeHandle;
    FPrepared := false;
  end;
end;

function TFBStatement.GetSQLParams: ISQLParams;
begin
  CheckHandle;
  Result := FSQLParams;
end;

function TFBStatement.GetOutMetaData: IMetaData;
begin
  CheckHandle;
  Result := TIBXOUTPUTSQLDA.Copy(FSQLRecord);
end;

function TFBStatement.GetPlan: String;
var
  DBInfo: IDBInformation;
begin
  if (not (FSQLType in [SQLSelect, SQLSelectForUpdate,
       {TODO: SQLExecProcedure, }
       SQLUpdate, SQLDelete])) then
    result := ''
  else
  begin
     DBInfo := TDBInformation.Create(self,byte(isc_info_sql_get_plan),16384);
     if (DBInfo.getCount > 0) and (DBInfo[0].getItemType = byte(isc_info_sql_get_plan)) then
       Result := DBInfo[0].GetAsString;
  end;
end;

function TFBStatement.GetRowsAffected(var InsertCount, UpdateCount,
  DeleteCount: integer): boolean;
var
  DBInfo: IDBInformation;
  i: integer;
begin
  InsertCount := 0;
  UpdateCount := 0;
  DeleteCount := 0;
  Result := true;
  begin
    DBInfo := TDBInformation.Create(self,byte(isc_info_sql_records));
    for i := 0 to DBInfo.getCount - 1 do
    begin
      case char(DBInfo[i].getItemType) of
      isc_info_req_insert_count:
        InsertCount := DBInfo[i].GetAsInteger;

      isc_info_req_update_count:
        UpdateCount := DBInfo[i].GetAsInteger;

      isc_info_req_delete_count:
        DeleteCount := DBInfo[i].GetAsInteger;
      end;
    end;
  end;
end;

function TFBStatement.GetSQLType: TIBSQLTypes;
begin
  Result := FSQLType;
end;

function TFBStatement.Execute(aTransaction: ITransaction): IResults;
begin
  if aTransaction = nil then
    Result :=  InternalExecute(FTransaction)
  else
    Result := InternalExecute(aTransaction as TFBTransaction);
end;

function TFBStatement.OpenCursor(aTransaction: ITransaction): IResultSet;
begin
  if aTransaction = nil then
    Result := InternalOpenCursor(FTransaction)
  else
    Result := InternalOpenCursor(aTransaction as TFBTransaction);
end;

function TFBStatement.CreateBlob: IBlob;
begin
  Result := TFBBlob.Create(FAttachment,FTransaction);
end;

function TFBStatement.GetSQLInfo(InfoRequest: byte): IDBInformation;
begin
  Result := TDBInformation.Create(self,InfoRequest)
end;

function TFBStatement.GetSQLText: string;
begin
  Result := FSQL;
end;

function TFBStatement.IsPrepared: boolean;
begin
  Result := FHandle <> nil;
end;

procedure TFBStatement.Prepare(aTransaction: ITransaction);
begin
  if FPrepared then FreeHandle;
  if aTransaction <> nil then
  begin
    RemoveOwner(FTransaction);
    FTransaction := transaction as TFBTransaction;
    FTransactionIntf := Transaction;
    AddOwner(FTransaction);
  end;
  InternalPrepare;
end;

end.

