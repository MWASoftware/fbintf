unit UdrSelectInto;

{
create or alter procedure select_into (
  select_statement blob sub_type 1 not null,
  table_name varchar(63) not null,
  table_type varchar(25) not null    -- 'global temporary preserve', 'global temporary[ delete]', <empty string> for standard table
) returns (
  status varchar(100)
)
external name 'selectinto!select_into'
engine udr;
}

interface

uses Classes, SysUtils, IB, FBUDRController, FBUDRIntf;

type
  TSelectInto = class(TFBUDRExecuteProcedure)
  public
    procedure Execute(context: IFBUDRExternalContext;
                      ProcMetadata: IFBUDRProcMetadata;
                      InputParams: IFBUDRInputParams;
                      OutputData: IFBUDROutputData); override;
  end;


implementation

uses StrUtils, IBUtils;

function SQLType2Name(SQLType: Cardinal) : AnsiString;
begin
  case SQLType of
    SQL_VARYING:          Result := 'VARCHAR';
    SQL_TEXT:             Result := 'CHAR';
    SQL_DOUBLE:           Result := 'DOUBLE PRECISION';
    SQL_FLOAT:            Result := 'FLOAT';
    SQL_LONG:             Result := 'INTEGER';
    SQL_SHORT:            Result := 'SMALLINT';
    SQL_TIMESTAMP:        Result := 'TIMESTAMP';
    SQL_TIMESTAMP_TZ:     Result := 'TIMESTAMP WITH TIMEZONE';
    SQL_TIMESTAMP_TZ_EX:  Result := 'TIMESTAMP WITH TIMEZONE';
    SQL_BLOB:             Result := 'BLOB';
    SQL_D_FLOAT:          Result := 'FLOAT';
    SQL_TYPE_TIME:        Result := 'TIME';
    SQL_TYPE_DATE:        Result := 'DATE';
    SQL_INT64:            Result := 'BIGINT';
    SQL_TIME_TZ:          Result := 'TIME WITH TIMEZONE';
    SQL_TIME_TZ_EX:       Result := 'TIME WITH TIMEZONE';
    SQL_DEC16:            Result := 'DECFLOAT(16)';
    SQL_DEC34:            Result := 'DECFLOAT(34)';
    SQL_INT128:           Result := 'INT128';
    SQL_NULL:             Result := 'VARCHAR(1)';
    SQL_BOOLEAN:          Result := 'BOOLEAN';
  else
    Result := 'UNKNOWN';
  end
end;

function Fld2SQLTypeDef(SQLType: Cardinal; SubType: Integer; Size: Cardinal; Scale: Integer; CharSetClause: String) : AnsiString;
var
  TypeDef: AnsiString;
begin
  TypeDef := SQLType2Name(SQLType);

  if Scale < 0 then
    case SQLType of
      SQL_SHORT:            TypeDef := format('NUMERIC(4,%d)', [-Scale]);
      SQL_LONG:             TypeDef := format('NUMERIC(9,%d)', [-Scale]);
      SQL_DOUBLE:           TypeDef := format('NUMERIC(15,%d)', [-Scale]);
      SQL_INT64:            TypeDef := format('NUMERIC(18,%d)', [-Scale]);  
      SQL_INT128:           TypeDef := format('NUMERIC(35,%d)', [-Scale]);
    end
  else
    case SQLType of
      SQL_VARYING,
      SQL_TEXT:             TypeDef := format(TypeDef + '(%d) %s', [Size, CharSetClause]);
      SQL_BLOB:             TypeDef := format(TypeDef + ' SUB_TYPE %d %s', [SubType, CharSetClause]);
    end;
  Result := TypeDef;
end;

procedure TSelectInto.Execute(context: IFBUDRExternalContext; ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams; OutputData: IFBUDROutputData);
var
  Statement: IStatement;
  TX2: ITransaction;
  Dialect, DefaultCharSetID, i: Integer;
  SelectSQL, TableName, TableType: AnsiString;
  TypeDef, TypeDefs, CharSetClause, Command, CommitAction, SQL: AnsiString;
begin
  TypeDefs := '';
  with context do
  begin
    // prepare and analyse select_statement and create DDL of target table columns
    SelectSQL := InputParams.ByName('select_statement').AsString;
    Dialect := GetAttachment.GetSQLDialect;
    DefaultCharSetID := context.GetAttachment.getCharSetID;
    CharSetClause := '';

    Statement := GetAttachment.Prepare(GetTransaction, SelectSQL);
    with Statement.MetaData do
    begin
      for i := 0 to Count -1 do
      begin
        with ColMetaData[i] do
        begin
          case GetSQLType of SQL_VARYING,
                             SQL_TEXT,
                             SQL_BLOB: if (DefaultCharSetID <> getCharSetID) then
                                         CharSetClause := 'CHARACTER SET ' + GetAttachment.GetCharsetName(getCharSetID);
          end;
          TypeDef := Fld2SQLTypeDef(GetSQLType, SQLSubtype, Size, Scale, CharSetClause);
          TypeDefs := TypeDefs + ','#13#10 + QuoteIdentifierIfNeeded(Dialect, Name) + ' ' + TypeDef;
        end;
      end;
      Delete(TypeDefs, 1, 1);
    end;

    // create target table DDL
    TableName := InputParams.ByName('table_name').AsString;
    TableType := UpperCase(InputParams.ByName('table_type').AsString);

    // default: recreate standard table
    Command := 'RECREATE ';

    // global temporary?
    CommitAction := '';
    if AnsiContainsStr(TableType, 'GLOBAL TEMPORARY') then
    begin
      Command := Command + 'GLOBAL TEMPORARY';
      if AnsiContainsStr(TableType, 'PRESERVE') then
        CommitAction := 'ON COMMIT PRESERVE ROWS';
    end;

    // create target table within own TX, needs to be committed separately
    SQL := SQL + format('%s TABLE %s (%s) %s', [Command, TableName, TypeDefs, CommitAction]);
    TX2 := GetAttachment.StartTransaction([isc_tpb_write, isc_tpb_nowait, isc_tpb_read_committed], taCommit);
    GetAttachment.ExecImmediate(TX2, SQL);
    TX2.Commit();

    // fill target table
    SQL := format('INSERT INTO %s %s;', [TableName, SelectSQL]);
    GetAttachment.ExecImmediate(GetTransaction, SQL);
  end;
  OutputData.ByName('status').AsString := 'ok';
end;

initialization

FBRegisterUDRProcedure('select_into', TSelectInto);

end.
