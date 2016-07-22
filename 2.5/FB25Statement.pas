unit FB25Statement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBLibrary, FB25ClientAPI, FB25Transaction, FB25Attachment,
  IBHeader, IBExternals;

type

  { TFBStatement }

  TFBStatement = class(TInterfacedObject,IStatement)
  private
    FClientAPI: TFBClientAPI;
    FOwner: TObjectOwner;
    FHandle: TISC_STMT_HANDLE;
    FSQLType: TIBSQLTypes;         { Select, update, delete, insert, create, alter, etc...}
    procedure InternalPrepare(DBHandle: TISC_DB_HANDLE; TRHandle: TISC_TR_HANDLE;
      sql: string; SQLDialect: integer);
    procedure FreeHandle;
  public
    constructor Create(Attachment: TFBAttachment; transaction: ITransaction; sql: string;
      SQLDialect: integer);
    destructor Destroy; override;

    {IStatement}
    function GetStatus: IStatus;
    function GetSQLParams: ISQLParams;
    function GetOutMetaData: IMetaData;
    function GetPlan: String;
    function GetRowsAffected: Integer;
    function Execute: ISQLData;
    function OpenCursor: IResultSet;
    property SQLParams: ISQLParams read GetSQLParams;
 end;

implementation

{ TFBStatement }

procedure TFBStatement.InternalPrepare(DBHandle: TISC_DB_HANDLE;
  TRHandle: TISC_TR_HANDLE; sql: string; SQLDialect: integer);
var
  stmt_len: Integer;
  res_buffer: array[0..7] of Char;
  type_item: Char;
begin
  if (sql = '') then
    IBError(ibxeEmptyQuery, [nil]);
  with FClientAPI do
  try
    Call(isc_dsql_alloc_statement2(StatusVector, DBHandle,
                                    @FHandle), True);
    Call(isc_dsql_prepare(StatusVector, TRHandle, @FHandle, 0,
               PChar(sql), SQLDialect, nil), True);
    { After preparing the statement, query the stmt type and possibly
      create a FSQLRecord "holder" }
    { Get the type of the statement }
    type_item := isc_info_sql_stmt_type;
    Call(isc_dsql_sql_info(StatusVector, @FHandle, 1, @type_item,
                         SizeOf(res_buffer), res_buffer), True);
    if (res_buffer[0] <> isc_info_sql_stmt_type) then
      IBError(ibxeUnknownError, [nil]);
    stmt_len := isc_vax_integer(@res_buffer[1], 2);
    FSQLType := TIBSQLTypes(isc_vax_integer(@res_buffer[3], stmt_len));
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
      SQLExecProcedure: begin
        { We already know how many inputs there are, so... }
        if (FSQLParams.FXSQLDA <> nil) and
           (Call(isc_dsql_describe_bind(StatusVector, @FHandle, SQLDialect,
                                        FSQLParams.FXSQLDA), False) > 0) then
          IBDataBaseError;
        FSQLParams.Initialize;
        if FSQLType in [SQLSelect, SQLSelectForUpdate,
                        SQLExecProcedure] then begin
          { Allocate an initial output descriptor (with one column) }
          FSQLRecord.Count := 1;
          { Using isc_dsql_describe, get the right size for the columns... }
          Call(isc_dsql_describe(StatusVector, @FHandle, Database.SQLDialect, FSQLRecord.FXSQLDA), True);
          if FSQLRecord.FXSQLDA^.sqld > FSQLRecord.FXSQLDA^.sqln then begin
            FSQLRecord.Count := FSQLRecord.FXSQLDA^.sqld;
            Call(isc_dsql_describe(StatusVector, @FHandle, Database.SQLDialect, FSQLRecord.FXSQLDA), True);
          end else if FSQLRecord.FXSQLDA^.sqld = 0 then
            FSQLRecord.Count := 0;
          FSQLRecord.Initialize;
        end;
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
                                       sSQLErrorSeparator + FProcessedSQL.Text)
      else
        raise;
    end;
  end;
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
    with FClientAPI do
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

constructor TFBStatement.Create(Attachment: TFBAttachment;
  transaction: ITransaction; sql: string; SQLDialect: integer);
var DBHandle: TISC_DB_HANDLE;
    TRHandle: TISC_TR_HANDLE;
begin
  inherited Create;
  FClientAPI := Attachment.ClientAPI;
  FOwner := Transaction as TFBAttachment;
  FOwner.RegisterObj(self);
  DBHandle := (Attachment as TFBAttachment).Handle;
  TRHandle := (Transaction as TFBTransaction).Handle;
  InternalPrepare(DBHandle,TRHandle,sql,SQLDialect);
end;

destructor TFBStatement.Destroy;
begin
  if assigned(FOwner) then
    FOwner.UnRegisterObj(self);
  inherited Destroy;
end;

function TFBStatement.GetStatus: IStatus;
begin
  Result := FClientAPI.Status;
end;

function TFBStatement.GetSQLParams: ISQLParams;
begin

end;

function TFBStatement.GetOutMetaData: IMetaData;
begin

end;

function TFBStatement.GetPlan: String;
begin

end;

function TFBStatement.GetRowsAffected: Integer;
begin

end;

function TFBStatement.Execute: ISQLData;
begin

end;

function TFBStatement.OpenCursor: IResultSet;
begin

end;

end.

