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
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

unit IBUtils;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$Mode Delphi}
{$codepage UTF8}
{$define HASREQEX}
{$ENDIF}


interface

uses Classes, SysUtils, IB;

type
  TSQLTokens = (

  {Reserved Words}

  sqltAdd,
  sqltAdmin,
  sqltAll,
  sqltAlter,
  sqltAnd,
  sqltAny,
  sqltAs,
  sqltAt,
  sqltAvg,
  sqltBegin,
  sqltBetween,
  sqltBigint,
  sqltBit_Length,
  sqltBlob,
  sqltBoolean,
  sqltBoth,
  sqltBy,
  sqltCase,
  sqltCast,
  sqltChar,
  sqltChar_Length,
  sqltCharacter,
  sqltCharacter_Length,
  sqltCheck,
  sqltClose,
  sqltCollate,
  sqltColumn,
  sqltCommit,
  sqltConnect,
  sqltConstraint,
  sqltCorr,
  sqltCount,
  sqltCovar_Pop,
  sqltCovar_Samp,
  sqltCreate,
  sqltCross,
  sqltCurrent,
  sqltCurrent_Connection,
  sqltCurrent_Date,
  sqltCurrent_Role,
  sqltCurrent_Time,
  sqltCurrent_Timestamp,
  sqltCurrent_Transaction,
  sqltCurrent_User,
  sqltCursor,
  sqltDate,
  sqltDay,
  sqltDec,
  sqltDecimal,
  sqltDeclare,
  sqltDefault,
  sqltDelete,
  sqltDeleting,
  sqltDeterministic,
  sqltDisconnect,
  sqltDistinct,
  sqltDouble,
  sqltDrop,
  sqltElse,
  sqltEnd,
  sqltEscape,
  sqltExecute,
  sqltExists,
  sqltExternal,
  sqltExtract,
  sqltFalse,
  sqltFetch,
  sqltFilter,
  sqltFloat,
  sqltFor,
  sqltForeign,
  sqltFrom,
  sqltFull,
  sqltFunction,
  sqltGdscode,
  sqltGlobal,
  sqltGrant,
  sqltGroup,
  sqltHaving,
  sqltHour,
  sqltIn,
  sqltIndex,
  sqltInner,
  sqltInsensitive,
  sqltInsert,
  sqltInserting,
  sqltInt,
  sqltInteger,
  sqltInto,
  sqltIs,
  sqltJoin,
  sqltKey,
  sqltLeading,
  sqltLeft,
  sqltLike,
  sqltLong,
  sqltLower,
  sqltMax,
  sqltMaximum_Segment,
  sqltMerge,
  sqltMin,
  sqltMinute,
  sqltMonth,
  sqltNational,
  sqltNatural,
  sqltNchar,
  sqltNo,
  sqltNot,
  sqltNull,
  sqltNumeric,
  sqltOctet_Length,
  sqltOf,
  sqltOffset,
  sqltOn,
  sqltOnly,
  sqltOpen,
  sqltOr,
  sqltOrder,
  sqltOuter,
  sqltOver,
  sqltParameter,
  sqltPlan,
  sqltPosition,
  sqltPost_Event,
  sqltPrecision,
  sqltPrimary,
  sqltProcedure,
  sqltRdbDb_Key,
  sqltRdbRecord_Version,
  sqltReal,
  sqltRecord_Version,
  sqltRecreate,
  sqltRecursive,
  sqltReferences,
  sqltRegr_Avgx,
  sqltRegr_Avgy,
  sqltRegr_Count,
  sqltRegr_Intercept,
  sqltRegr_R2,
  sqltRegr_Slope,
  sqltRegr_Sxx,
  sqltRegr_Sxy,
  sqltRegr_Syy,
  sqltRelease,
  sqltReturn,
  sqltReturning_Values,
  sqltReturns,
  sqltRevoke,
  sqltRight,
  sqltRollback,
  sqltRow,
  sqltRow_Count,
  sqltRows,
  sqltSavepoint,
  sqltScroll,
  sqltSecond,
  sqltSelect,
  sqltSensitive,
  sqltSet,
  sqltSimilar,
  sqltSmallint,
  sqltSome,
  sqltSqlcode,
  sqltSqlstate,
  sqltStart,
  sqltStddev_Pop,
  sqltStddev_Samp,
  sqltSum,
  sqltTable,
  sqltThen,
  sqltTime,
  sqltTimestamp,
  sqltTo,
  sqltTrailing,
  sqltTrigger,
  sqltTrim,
  sqltTrue,
  sqltUnion,
  sqltUnique,
  sqltUnknown,
  sqltUpdate,
  sqltUpdating,
  sqltUpper,
  sqltUser,
  sqltUsing,
  sqltValue,
  sqltValues,
  sqltVar_Pop,
  sqltVar_Samp,
  sqltVarchar,
  sqltVariable,
  sqltVarying,
  sqltView,
  sqltWhen,
  sqltWhere,
  sqltWhile,
  sqltWith,
  sqltYear,

  {symbols}

  sqltSpace,
  sqltSemiColon,
  sqltPlaceholder,
  sqltSingleQuotes,
  sqltDoubleQuotes,
  sqltComma,
  sqltPeriod,
  sqltEquals,
  sqltIdentifier,
  sqltIdentifierInDoubleQuotes,
  sqltBadIdentifier,
  sqltNumberString,
  sqltString,
  sqlParam,
  sqltColon,
  sqltComment,
  sqltCommentLine,
  sqltQuotedString,
  sqltAsterisk,
  sqltForwardSlash,
  sqltOpenSquareBracket,
  sqltCloseSquareBracket,
  sqltOpenBracket,
  sqltCloseBracket,
  sqltPipe,
  sqltConcatSymbol,
  sqltLT,
  sqltGT,
  sqltCR,
  sqltEOL,
  sqltEOF,
  sqltInit
  );

  TSQLReservedWords = sqltAdd..sqltYear;

const
  CRLF = #13 + #10;
  CR   = #13;
  LF   = #10;
  TAB  = #9;
  NULL_TERMINATOR = #0;

  {SQL Reserved words in alphabetical order}

  sqlReservedWords: array [TSQLReservedWords] of string = (
  'ADD',
  'ADMIN',
  'ALL',
  'ALTER',
  'AND',
  'ANY',
  'AS',
  'AT',
  'AVG',
  'BEGIN',
  'BETWEEN',
  'BIGINT',
  'BIT_LENGTH',
  'BLOB',
  'BOOLEAN',
  'BOTH',
  'BY',
  'CASE',
  'CAST',
  'CHAR',
  'CHAR_LENGTH',
  'CHARACTER',
  'CHARACTER_LENGTH',
  'CHECK',
  'CLOSE',
  'COLLATE',
  'COLUMN',
  'COMMIT',
  'CONNECT',
  'CONSTRAINT',
  'CORR',
  'COUNT',
  'COVAR_POP',
  'COVAR_SAMP',
  'CREATE',
  'CROSS',
  'CURRENT',
  'CURRENT_CONNECTION',
  'CURRENT_DATE',
  'CURRENT_ROLE',
  'CURRENT_TIME',
  'CURRENT_TIMESTAMP',
  'CURRENT_TRANSACTION',
  'CURRENT_USER',
  'CURSOR',
  'DATE',
  'DAY',
  'DEC',
  'DECIMAL',
  'DECLARE',
  'DEFAULT',
  'DELETE',
  'DELETING',
  'DETERMINISTIC',
  'DISCONNECT',
  'DISTINCT',
  'DOUBLE',
  'DROP',
  'ELSE',
  'END',
  'ESCAPE',
  'EXECUTE',
  'EXISTS',
  'EXTERNAL',
  'EXTRACT',
  'FALSE',
  'FETCH',
  'FILTER',
  'FLOAT',
  'FOR',
  'FOREIGN',
  'FROM',
  'FULL',
  'FUNCTION',
  'GDSCODE',
  'GLOBAL',
  'GRANT',
  'GROUP',
  'HAVING',
  'HOUR',
  'IN',
  'INDEX',
  'INNER',
  'INSENSITIVE',
  'INSERT',
  'INSERTING',
  'INT',
  'INTEGER',
  'INTO',
  'IS',
  'JOIN',
  'KEY',
  'LEADING',
  'LEFT',
  'LIKE',
  'LONG',
  'LOWER',
  'MAX',
  'MAXIMUM_SEGMENT',
  'MERGE',
  'MIN',
  'MINUTE',
  'MONTH',
  'NATIONAL',
  'NATURAL',
  'NCHAR',
  'NO',
  'NOT',
  'NULL',
  'NUMERIC',
  'OCTET_LENGTH',
  'OF',
  'OFFSET',
  'ON',
  'ONLY',
  'OPEN',
  'OR',
  'ORDER',
  'OUTER',
  'OVER',
  'PARAMETER',
  'PLAN',
  'POSITION',
  'POST_EVENT',
  'PRECISION',
  'PRIMARY',
  'PROCEDURE',
  'RDB$DB_KEY',
  'RDB$RECORD_VERSION',
  'REAL',
  'RECORD_VERSION',
  'RECREATE',
  'RECURSIVE',
  'REFERENCES',
  'REGR_AVGX',
  'REGR_AVGY',
  'REGR_COUNT',
  'REGR_INTERCEPT',
  'REGR_R2',
  'REGR_SLOPE',
  'REGR_SXX',
  'REGR_SXY',
  'REGR_SYY',
  'RELEASE',
  'RETURN',
  'RETURNING_VALUES',
  'RETURNS',
  'REVOKE',
  'RIGHT',
  'ROLLBACK',
  'ROW',
  'ROW_COUNT',
  'ROWS',
  'SAVEPOINT',
  'SCROLL',
  'SECOND',
  'SELECT',
  'SENSITIVE',
  'SET',
  'SIMILAR',
  'SMALLINT',
  'SOME',
  'SQLCODE',
  'SQLSTATE',
  'START',
  'STDDEV_POP',
  'STDDEV_SAMP',
  'SUM',
  'TABLE',
  'THEN',
  'TIME',
  'TIMESTAMP',
  'TO',
  'TRAILING',
  'TRIGGER',
  'TRIM',
  'TRUE',
  'UNION',
  'UNIQUE',
  'UNKNOWN',
  'UPDATE',
  'UPDATING',
  'UPPER',
  'USER',
  'USING',
  'VALUE',
  'VALUES',
  'VAR_POP',
  'VAR_SAMP',
  'VARCHAR',
  'VARIABLE',
  'VARYING',
  'VIEW',
  'WHEN',
  'WHERE',
  'WHILE',
  'WITH',
  'YEAR'
  );

type
  {The TSQLTokeniser class provides a common means to parse an SQL statement, or
   even a stream of SQL Statements. The TSQLStringTokeniser class is instantiated
   with a single SQL statement or a set of concatenated statements. The TSQLStreamTokeniser
   is instantiated with a stream from which the SQL statements are read.

   Successive calls to GetNextToken then return each SQL token. The TokenText contains
   either the single character, the identifier or reserved word, the string or comment.}

  { TSQLTokeniser }

  TSQLTokeniser = class
  private
    const
      TokenQueueMaxSize = 32;
    type
      TLexState = (stDefault, stInCommentLine, stInComment, stSingleQuoted, stDoubleQuoted,
                   stInArrayBounds, stInIdentifier, stInNumeric);

      TTokenQueueItem = record
                          token: TSQLTokens;
                          text: string;
                        end;
      TTokenQueueState = (tsHold, tsRelease);

  private
    FLastChar: char;
    FState: TLexState;
    function GetNext: TSQLTokens;

    {The token Queue is available for use by descendents so that they can
     hold back tokens in order to lookahead by token rather than just a single
     character}

  private
    FCompressWhiteSpace: boolean;
    FTokenQueue: array[0..TokenQueueMaxSize] of TTokenQueueItem;
    FQueueState: TTokenQueueState;
    FQFirst: integer;  {first and last pointers first=last => queue empty}
    FQLast: integer;
    procedure PopQueue(var token: TSQLTokens);
  protected
    FString: string;
    FNextToken: TSQLTokens;
    function GetChar: char; virtual; abstract;
    function TokenFound(var token: TSQLTokens): boolean; virtual;
    function InternalGetNextToken: TSQLTokens; virtual;
    procedure Reset; virtual;

    {Token stack}
    procedure QueueToken(token: TSQLTokens; text:string); overload;
    procedure QueueToken(token: TSQLTokens); overload;
    procedure ResetQueue;
    procedure ReleaseQueue(var token: TSQLTokens);
    function GetQueuedText: string;
    procedure SetTokenText(text: string);

  public
    const
        DefaultTerminator = ';';
  public
    constructor Create;
    function GetNextToken: TSQLTokens;
    function EOF: boolean;
    property TokenText: string read FString;
    property CompressWhiteSpace: boolean read FCompressWhiteSpace
                                         write FCompressWhiteSpace default true;
  end;

  { TSQLStringTokeniser }

  TSQLStringTokeniser = class(TSQLTokeniser)
  private
    FInString: string;
    FIndex: integer;
  protected
    function GetChar: char; override;
  public
    constructor Create(S: string);
  end;

  { TSQLStreamTokeniser }

  TSQLStreamTokeniser = class(TSQLTokeniser)
  private
    FInStream: TStream;
  protected
    function GetChar: char; override;
  public
    constructor Create(S: TStream);
  end;

function Max(n1, n2: Integer): Integer;
function Min(n1, n2: Integer): Integer;
function RandomString(iLength: Integer): AnsiString;
function RandomInteger(iLow, iHigh: Integer): Integer;
function StripString(st: AnsiString; CharsToStrip: AnsiString): AnsiString;
function ExtractIdentifier(Dialect: Integer; Value: AnsiString): AnsiString;
function FindReservedWord(w: AnsiString; var token: TSQLTokens): boolean;
function IsReservedWord(w: AnsiString): boolean;
function QuoteIdentifier(Dialect: Integer; Value: AnsiString): AnsiString;
function QuoteIdentifierIfNeeded(Dialect: Integer; Value: AnsiString): AnsiString;
function Space2Underscore(s: AnsiString): AnsiString;
function SQLSafeString(const s: AnsiString): AnsiString;
function IsSQLIdentifier(Value: AnsiString): boolean;
function ExtractConnectString(const CreateSQL: AnsiString; var ConnectString: AnsiString): boolean;
function MakeConnectString(ServerName, DatabaseName: AnsiString; Protocol: TProtocol;
              PortNo: AnsiString = ''): AnsiString;
function ParseConnectString(ConnectString: AnsiString;
              var ServerName, DatabaseName: AnsiString; var Protocol: TProtocolAll;
              var PortNo: AnsiString): boolean;
function GetProtocol(ConnectString: AnsiString): TProtocolAll;

implementation

uses FBMessages

{$IFDEF HASREQEX}
,RegExpr
{$ENDIF};

function Max(n1, n2: Integer): Integer;
begin
  if (n1 > n2) then
    result := n1
  else
    result := n2;
end;

function Min(n1, n2: Integer): Integer;
begin
  if (n1 < n2) then
    result := n1
  else
    result := n2;
end;

function RandomString(iLength: Integer): AnsiString;
begin
  result := '';
  while Length(result) < iLength do
    result := result + IntToStr(RandomInteger(0, High(Integer)));
  if Length(result) > iLength then
    result := Copy(result, 1, iLength);
end;

function RandomInteger(iLow, iHigh: Integer): Integer;
begin
  result := Trunc(Random(iHigh - iLow)) + iLow;
end;

function StripString(st: AnsiString; CharsToStrip: AnsiString): AnsiString;
var
  i: Integer;
begin
  result := '';
  for i := 1 to Length(st) do begin
    if AnsiPos(st[i], CharsToStrip) = 0 then
      result := result + st[i];
  end;
end;

{Extracts SQL Identifier typically from a  Dialect 3 encoding}

function ExtractIdentifier(Dialect: Integer; Value: AnsiString): AnsiString;
begin
  Value := Trim(Value);
  if Dialect = 1 then
    Value := AnsiUpperCase(Value)  
  else
  begin
    if (Value <> '') and (Value[1] = '"') then
    begin
      Delete(Value, 1, 1);
      Delete(Value, Length(Value), 1);
      Value := StringReplace (Value, '""', '"', [rfReplaceAll]);
    end
    else
      Value := AnsiUpperCase(Value);
  end;
  Result := Value;
end;

{Returns true if "w" is a Firebird SQL reserved word, and the
 corresponding TSQLTokens value.}

function FindReservedWord(w: AnsiString; var token: TSQLTokens): boolean;
var i: TSQLTokens;
begin
   Result := true;
   w := AnsiUpperCase(Trim(w));
   for i := Low(TSQLReservedWords) to High(TSQLReservedWords) do
   begin
       if w = sqlReservedWords[i] then
       begin
         token := i;
         Exit;
       end;
       if w <  sqlReservedWords[i] then
         break;
   end;
   Result := false;
end;

{Returns true if "w" is a Firebird SQL reserved word}

function IsReservedWord(w: AnsiString): boolean;
var token: TSQLTokens;
begin
  Result := FindReservedWord(w,token);
end;

{Format an SQL Identifier according to SQL Dialect}

function QuoteIdentifier(Dialect: Integer; Value: AnsiString): AnsiString;
begin
  if Dialect = 1 then
    Value := AnsiUpperCase(Trim(Value))
  else
    Value := '"' + StringReplace (Value, '""', '"', [rfReplaceAll]) + '"';
  Result := Value;
end;

const
  ValidSQLIdentifierChars = ['A'..'Z','a'..'z','0'..'9','_','$'];

{Returns true if the value is a valid SQL Identifier - note lower case accepted}

function IsSQLIdentifier(Value: AnsiString): boolean;
var i: integer;
begin
  Result := false;
  for i := 1 to Length(Value) do
    if not (Value[i] in ValidSQLIdentifierChars) then Exit;
  Result := true;
end;

{Extracts the Database Connect string from a Create Database Statement}

{$IFDEF HASREQEX}
function ExtractConnectString(const CreateSQL: AnsiString;
  var ConnectString: AnsiString): boolean;
var RegexObj: TRegExpr;
begin
  RegexObj := TRegExpr.Create;
  try
    {extact database file spec}
    RegexObj.ModifierG := false; {turn off greedy matches}
    RegexObj.ModifierI := true; {case insensitive match}
    RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +''(.*)''';
    Result := RegexObj.Exec(CreateSQL);
    if Result then
      ConnectString := RegexObj.Match[2];
  finally
    RegexObj.Free;
  end;
end;

function ParseConnectString(ConnectString: AnsiString; var ServerName,
  DatabaseName: AnsiString; var Protocol: TProtocolAll; var PortNo: AnsiString
  ): boolean;

  function GetProtocol(scheme: AnsiString): TProtocolAll;
  begin
    scheme := AnsiUpperCase(scheme);
    if scheme = 'INET' then
      Result := inet
    else
    if scheme = 'INET4' then
      Result := inet4
    else
    if scheme = 'INET6' then
      Result := inet6
    else
    if scheme = 'XNET' then
      Result := xnet
    else
    if scheme = 'WNET' then
      Result := wnet
  end;

var RegexObj: TRegExpr;
begin
  ServerName := '';
  DatabaseName := ConnectString;
  PortNo := '';
  Protocol := unknownProtocol;
  RegexObj := TRegExpr.Create;
  try
    {extact database file spec}
    RegexObj.ModifierG := false; {turn off greedy matches}
    RegexObj.Expression := '^([a-zA-Z46]+)://([a-zA-Z0-9\-\.]*)(|:[0-9a-zA-Z\-]+)/(.*)$';
    Result := RegexObj.Exec(ConnectString);
    if Result then
    begin
      {URL type connect string}
      Protocol := GetProtocol(RegexObj.Match[1]);
      ServerName := RegexObj.Match[2];
      if RegexObj.MatchLen[3] > 0 then
        PortNo := system.Copy(ConnectString,RegexObj.MatchPos[3]+1,RegexObj.MatchLen[3]-1);
      DatabaseName := RegexObj.Match[4];
      if ServerName = '' then
        DatabaseName := '/' + DatabaseName;
    end
    else
    begin
      {URL type connect string - local loop}
      RegexObj.Expression := '^([a-zA-Z46]+)://(.*)$';
      Result := RegexObj.Exec(ConnectString);
      if Result then
      begin
        Protocol := GetProtocol(RegexObj.Match[1]);
        DatabaseName := RegexObj.Match[2];
      end
      else
      begin
        RegexObj.Expression := '^([a-zA-Z]:\\.*)';
        Result := RegexObj.Exec(ConnectString);
        if Result then
          Protocol := Local {Windows with leading drive ID}
        else
        begin
          RegexObj.Expression := '^([a-zA-Z0-9\-\.]+)(|/[0-9a-zA-Z\-]+):(.*)$';
          Result := RegexObj.Exec(ConnectString);
          if Result then
          begin
            {Legacy TCP Format}
            ServerName := RegexObj.Match[1];
            if RegexObj.MatchLen[2] > 0 then
              PortNo := system.Copy(ConnectString,RegexObj.MatchPos[2]+1,RegexObj.MatchLen[2]-1);
            DatabaseName := RegexObj.Match[3];
            Protocol := TCP;
          end
          else
          begin
            RegexObj.Expression := '^\\\\([a-zA-Z0-9\-\.]+)(|@[0-9a-zA-Z\-]+)\\(.*)$';
            Result := RegexObj.Exec(ConnectString);
            if Result then
            begin
              {Netbui}
              ServerName := RegexObj.Match[1];
              if RegexObj.MatchLen[2] > 0 then
                PortNo := system.Copy(ConnectString,RegexObj.MatchPos[2]+1,RegexObj.MatchLen[2]-1);
              DatabaseName := RegexObj.Match[3];
              Protocol := NamedPipe
            end
            else
            begin
              Result := true;
              Protocol := Local; {Assume local}
            end;
          end;
        end;
      end;
    end;
  finally
    RegexObj.Free;
  end;
end;

function GetProtocol(ConnectString: AnsiString): TProtocolAll;
var ServerName,
    DatabaseName: AnsiString;
    PortNo: AnsiString;
begin
  ParseConnectString(ConnectString,ServerName,DatabaseName,Result,PortNo);
end;

{$ELSE}
{cruder version of above for Delphi. Older versions lack regular expression
 handling.}
function ExtractConnectString(const CreateSQL: AnsiString;
  var ConnectString: AnsiString): boolean;
var i: integer;
begin
  Result := false;
  i := Pos('''',CreateSQL);
  if i > 0 then
  begin
    ConnectString := CreateSQL;
    delete(ConnectString,1,i);
    i := Pos('''',ConnectString);
    if i > 0 then
    begin
      delete(ConnectString,i,Length(ConnectString)-i+1);
      Result := true;
    end;
  end;
end;

function GetProtocol(ConnectString: AnsiString): TProtocolAll;
begin
  Result := unknownProtocol; {not implemented for Delphi}
end;

function ParseConnectString(ConnectString: AnsiString;
              var ServerName, DatabaseName: AnsiString; var Protocol: TProtocolAll;
              var PortNo: AnsiString): boolean;
begin
  Result := false;
end;

{$ENDIF}

{Make a connect string in format appropriate protocol}

function MakeConnectString(ServerName, DatabaseName: AnsiString;
  Protocol: TProtocol; PortNo: AnsiString): AnsiString;

  function FormatURL: AnsiString;
  begin
    if (ServerName = '') and (Pos('/',DatabaseName) <= 1) then
      Result := DatabaseName
    else
      Result := ServerName + '/' + DatabaseName;
  end;

begin
  if PortNo <> '' then
    case Protocol of
    NamedPipe:
      ServerName := ServerName + '@' + PortNo;
    Local,
    SPX,
    xnet: {do nothing};
    TCP:
      ServerName := ServerName + '/' + PortNo;
    else
      ServerName := ServerName + ':' + PortNo;
    end;

  case Protocol of
    TCP:        Result := ServerName + ':' + DatabaseName; {do not localize}
    SPX:        Result := ServerName + '@' + DatabaseName; {do not localize}
    NamedPipe:  Result := '\\' + ServerName + '\' + DatabaseName; {do not localize}
    Local:      Result := DatabaseName; {do not localize}
    inet:       Result := 'inet://' + FormatURL; {do not localize}
    inet4:       Result := 'inet4://' + FormatURL; {do not localize}
    inet6:       Result := 'inet6://' + FormatURL; {do not localize}
    wnet:       Result := 'wnet://' + FormatURL; {do not localize}
    xnet:       Result := 'xnet://' + FormatURL;  {do not localize}
  end;
end;

{Format an SQL Identifier according to SQL Dialect with encapsulation if necessary}

function QuoteIdentifierIfNeeded(Dialect: Integer; Value: AnsiString): AnsiString;
begin
  if (Dialect = 3) and
    (IsReservedWord(Value) or not IsSQLIdentifier(Value) or (AnsiUpperCase(Value) <> Value)) then
     Result := '"' + StringReplace (TrimRight(Value), '"', '""', [rfReplaceAll]) + '"'
  else
    Result := Value
end;

{Replaces unknown characters in a string with underscores}

function Space2Underscore(s: AnsiString): AnsiString;
var
   k: integer;
begin
     Result := s;
     for k := 1 to Length(s) do
         if not (Result[k] in ValidSQLIdentifierChars)  then
            Result[k] := '_';
end;

{Reformats an SQL string with single quotes duplicated.}

function SQLSafeString(const s: AnsiString): AnsiString;
begin
  Result := StringReplace(s,'''','''''',[rfReplaceAll]);
end;

{ TSQLStreamTokeniser }

function TSQLStreamTokeniser.GetChar: char;
begin
  if not EOF and assigned(FInStream) then
    Result := char(FInStream.ReadByte)
  else
    Result := #0;
end;

constructor TSQLStreamTokeniser.Create(S: TStream);
begin
  inherited Create;
  FInStream := S;
end;

{ TSQLStringTokeniser }

function TSQLStringTokeniser.GetChar: char;
begin
  if FIndex <= Length(FInString) then
  begin
    Result := FInString[FIndex];
    Inc(FIndex);
  end
  else
    Result := #0;
end;

constructor TSQLStringTokeniser.Create(S: string);
begin
  inherited Create;
  FInString := S;
  FIndex := 1;
end;

{ TSQLTokeniser }

function TSQLTokeniser.GetNext: TSQLTokens;
var C: char;
begin
  if EOF then
    Result := sqltEOF
  else
  begin
    C := GetChar;
    case C of
    #0:
      Result := sqltEOF;
    ' ',TAB:
      Result := sqltSpace;
    '0'..'9':
      Result := sqltNumberString;
    ';':
      Result := sqltSemiColon;
    '?':
      Result := sqltPlaceholder;
    '|':
      Result := sqltPipe;
    '"':
      Result := sqltDoubleQuotes;
    '''':
      Result := sqltSingleQuotes;
    '/':
      Result := sqltForwardSlash;
    '*':
      Result := sqltAsterisk;
    '(':
      Result := sqltOpenBracket;
    ')':
      Result := sqltCloseBracket;
    ':':
      Result := sqltColon;
    ',':
      Result := sqltComma;
    '.':
      Result := sqltPeriod;
    '=':
      Result := sqltEquals;
    '[':
      Result := sqltOpenSquareBracket;
    ']':
      Result := sqltCloseSquareBracket;
    '<':
      Result := sqltLT;
    '>':
      Result := sqltGT;
    CR:
      Result := sqltCR;
    LF:
      Result := sqltEOL;
    else
      Result := sqltIdentifier;
    end;
    FLastChar := C
  end;
  FNextToken := Result;
end;

procedure TSQLTokeniser.PopQueue(var token: TSQLTokens);
begin
  if FQFirst = FQLast then
    IBError(ibxeTokenQueueUnderflow,[]);
  token := FTokenQueue[FQFirst].token;
  FString := FTokenQueue[FQFirst].text;
  Inc(FQFirst);
  if FQFirst = FQLast then
    FQueueState := tsHold;
end;

function TSQLTokeniser.TokenFound(var token: TSQLTokens): boolean;
begin
  Result := (FState = stDefault);
end;

procedure TSQLTokeniser.QueueToken(token: TSQLTokens; text: string);
begin
  if FQLast > TokenQueueMaxSize then
    IBError(ibxeTokenQueueOverflow,[]);
  FTokenQueue[FQLast].token := token;
  FTokenQueue[FQLast].text := text;
  Inc(FQLast);
end;

procedure TSQLTokeniser.QueueToken(token: TSQLTokens);
begin
  QueueToken(token,TokenText);
end;

procedure TSQLTokeniser.ResetQueue;
begin
  FQFirst := 0;
  FQLast := 0;
  FQueueState := tsHold;
end;

procedure TSQLTokeniser.ReleaseQueue(var token: TSQLTokens);
begin
  PopQueue(token);
  FQueueState := tsRelease;
end;

function TSQLTokeniser.GetQueuedText: string;
var i: integer;
begin
  Result := '';
  for i := FQFirst to FQLast do
    Result += FTokenQueue[i].text;
end;

procedure TSQLTokeniser.SetTokenText(text: string);
begin
  FString := text;
end;

constructor TSQLTokeniser.Create;
begin
  inherited Create;
  Reset;
  FCompressWhiteSpace := true;
end;

procedure TSQLTokeniser.Reset;
begin
  FNextToken := sqltInit;
  FState := stDefault;
  FString := '';
  ResetQueue;
end;

function TSQLTokeniser.GetNextToken: TSQLTokens;
begin
  if FQueueState = tsRelease then
    PopQueue(Result)
  else
    Result := InternalGetNextToken;
end;

{a simple lookahead one algorithm to extra the next symbol}

function TSQLTokeniser.InternalGetNextToken: TSQLTokens;
var C: char;
begin
  Result := sqltEOF;

  if FNextToken = sqltInit then
    GetNext;

  repeat
    Result := FNextToken;
    C := FLastChar;
    GetNext;

    {Combine CR/LF to EOF. CR on its own is treated as a space}

    if Result = sqltCR then
    begin
      if not CompressWhiteSpace then
        Result := sqltSpace
      else
      if FNextToken = sqltEOL then
        continue
      else
      begin
        C := ' ';
        Result := sqltSpace;
      end;
    end;

    case FState of
    stInComment:
      begin
        if (Result = sqltAsterisk) and (FNextToken = sqltForwardSlash) then
        begin
          FState := stDefault;
          Result := sqltComment;
          GetNext;
          Exit;
        end
        else
          FString += C;
      end;

    stInCommentLine:
      if Result = sqltEOL then
      begin
        FState := stDefault;
        Result := sqltCommentLine;
        Exit;
      end
      else
        FString += C;

    stSingleQuoted:
      begin
        if (Result = sqltSingleQuotes) then
        begin
          if (FNextToken = sqltSingleQuotes) then
            GetNext
          else
          begin
            Result := sqltQuotedString;
            FState := stDefault;
            Exit;
          end;
        end;
        FString += C;
      end;

    stDoubleQuoted:
      begin
        if (Result = sqltDoubleQuotes) then
        begin
          if (FNextToken = sqltDoubleQuotes) then
            GetNext
          else
          begin
            Result := sqltIdentifierInDoubleQuotes;
            FState := stDefault;
            Exit;
          end;
        end;
        FString += C;
      end;

    stInIdentifier:
      begin
        FString += C;
        Result := sqltIdentifier;
        if not (FNextToken in [sqltIdentifier,sqltNumberString]) then
          FState := stDefault
      end;

    stInNumeric:
      begin
        FString += C;
        if (Result = sqltPeriod) and (FNextToken = sqltPeriod) then
        begin
          {malformed decimal}
          FState := stInIdentifier;
          Result := sqltIdentifier
        end
        else
        begin
          if not (FNextToken in [sqltNumberString,sqltPeriod]) then
            FState := stDefault;
          Result := sqltNumberString;
        end;
      end;

    else {stDefault}
      begin
        FString := C;
        case Result of
        sqltSpace:
          while CompressWhiteSpace and (FNextToken = sqltSpace) do {consume}
            GetNext;

        sqltEOL:
          if CompressWhiteSpace then
            FString := ' ';

        sqltPipe:
          if FNextToken = sqltPipe then
          begin
            Result := sqltConcatSymbol;
            FString := C + FLastChar;
            GetNext;
          end;

        sqltForwardSlash:
          begin
            if FNextToken = sqltAsterisk then
            begin
              FString := '';
              GetNext;
              FState := stInComment;
            end
            else
            if FNextToken = sqltForwardSlash then
            begin
              FString := '';
              GetNext;
              FState := stInCommentLine;
            end;
          end;

        sqltSingleQuotes:
          begin
            FString := '';
            FState := stSingleQuoted;
          end;

        sqltDoubleQuotes:
          begin
            FString := '';
            FState := stDoubleQuoted;
          end;

        sqltOpenSquareBracket:
          FState := stInArrayBounds;

        sqltIdentifier:
          if FNextToken = sqltIdentifier then
            FState := stInIdentifier;

        sqltNumberString:
          if FNextToken in [sqltNumberString,sqltPeriod] then
            FState := stInNumeric;
        end;
      end;
    end;

//    writeln(FString);
  until (FNextToken = sqltEOF) or TokenFound(FNextToken);

  if Result = sqltIdentifier then
  begin
    if not FindReservedWord(FString,Result) then
      if not IsSQLIdentifier(FString) then
        Result := sqltBadIdentifier;
  end;
end;

function TSQLTokeniser.EOF: boolean;
begin
  Result := FNextToken = sqltEOF;
end;

end.
