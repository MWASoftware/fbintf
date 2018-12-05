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
  sqltRows,
  sqltRow_Count,
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
  sqltOtherCharacter,
  sqltIdentifier,
  sqltIdentifierInDoubleQuotes,
  sqltNumberString,
  sqltString,
  sqltParam,
  sqltQuotedParam,
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

  {$IFNDEF FPC}
  LineEnding = CRLF;
  {$ENDIF}

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
  'ROWS',
  'ROW_COUNT',
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
      TokenQueueMaxSize = 64;
    type
      TLexState = (stDefault, stInCommentLine, stInComment, stSingleQuoted, stDoubleQuoted,
                   stInIdentifier, stInNumeric);

      TTokenQueueItem = record
                          token: TSQLTokens;
                          text: AnsiString;
                        end;
      TTokenQueueState = (tsHold, tsRelease);

  private
    FLastChar: AnsiChar;
    FState: TLexState;
    FSkipNext: boolean;
    function GetNext: TSQLTokens;

    {The token Queue is available for use by descendents so that they can
     hold back tokens in order to lookahead by token rather than just a single
     character}

  private
    FTokenQueue: array[0..TokenQueueMaxSize] of TTokenQueueItem;
    FQueueState: TTokenQueueState;
    FQFirst: integer;  {first and last pointers first=last => queue empty}
    FQLast: integer;
    FEOF: boolean;
    procedure PopQueue(var token: TSQLTokens);
  protected
    FString: AnsiString;
    FNextToken: TSQLTokens;
    procedure Assign(source: TSQLTokeniser); virtual;
    function GetChar: AnsiChar; virtual; abstract;
    function TokenFound(var token: TSQLTokens): boolean; virtual;
    function InternalGetNextToken: TSQLTokens; virtual;
    procedure Reset; virtual;

    {Token stack}
    procedure QueueToken(token: TSQLTokens; text:AnsiString); overload;
    procedure QueueToken(token: TSQLTokens); overload;
    procedure ResetQueue; overload;
    procedure ResetQueue(token: TSQLTokens; text:AnsiString); overload;
    procedure ResetQueue(token: TSQLTokens); overload;
    procedure ReleaseQueue(var token: TSQLTokens); overload;
    procedure ReleaseQueue; overload;
    function GetQueuedText: AnsiString;
    procedure SetTokenText(text: AnsiString);

  public
    const
        DefaultTerminator = ';';
  public
    constructor Create;
    destructor Destroy; override;
    function GetNextToken: TSQLTokens;
    property EOF: boolean read FEOF;
    property TokenText: AnsiString read FString;
  end;

  { TSQLwithNamedParamsTokeniser }

  TSQLwithNamedParamsTokeniser = class(TSQLTokeniser)
  private
    type
      TSQLState = (stInit,stInParam,stInBlock, stInArrayDim);
  private
    FState: TSQLState;
    FNested: integer;
  protected
    procedure Assign(source: TSQLTokeniser); override;
    procedure Reset; override;
    function TokenFound(var token: TSQLTokens): boolean; override;
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

{ TSQLwithNamedParamsTokeniser }

procedure TSQLwithNamedParamsTokeniser.Assign(source: TSQLTokeniser);
begin
  inherited Assign(source);
  if source is TSQLwithNamedParamsTokeniser then
  begin
    FState := TSQLwithNamedParamsTokeniser(source).FState;
    FNested := TSQLwithNamedParamsTokeniser(source).FNested;
  end;
end;

procedure TSQLwithNamedParamsTokeniser.Reset;
begin
  inherited Reset;
  FState := stInit;
  FNested := 0;
end;

function TSQLwithNamedParamsTokeniser.TokenFound(var token: TSQLTokens
  ): boolean;
begin
  Result := inherited TokenFound(token);
  if not Result then Exit;

  case FState of
  stInit:
    begin
      case token of
      sqltColon:
        begin
          FState := stInParam;
          ResetQueue(token);
        end;

      sqltBegin:
        begin
          FState := stInBlock;
          FNested := 1;
        end;

      sqltOpenSquareBracket:
          FState := stInArrayDim;

      end;
    end;

  stInParam:
    begin
      case token of
      sqltIdentifier:
        token := sqltParam;

      sqltIdentifierInDoubleQuotes:
        token := sqltQuotedParam;

      else
        begin
          QueueToken(token);
          ReleaseQueue(token);
        end;
      end;
      FState := stInit;
    end;

  stInBlock:
    begin
      case token of
      sqltBegin:
          Inc(FNested);

      sqltEnd:
        begin
          Dec(FNested);
          if FNested = 0 then
            FState := stInit;
        end;
      end;
    end;

    stInArrayDim:
      begin
        if token = sqltCloseSquareBracket then
            FState := stInit;
      end;
    end;

  Result := (FState <> stInParam);
end;

{ TSQLTokeniser }

function TSQLTokeniser.GetNext: TSQLTokens;
var C: AnsiChar;
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
      if C in ValidSQLIdentifierChars then
        Result := sqltIdentifier
      else
        Result := sqltOtherCharacter;
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

procedure TSQLTokeniser.Assign(source: TSQLTokeniser);
begin
  FString := source.FString;
  FNextToken := source.FNextToken;
  FTokenQueue := source.FTokenQueue;
  FQueueState := source.FQueueState;
  FQFirst := source.FQFirst;
  FQLast := source.FQLast;
end;

function TSQLTokeniser.TokenFound(var token: TSQLTokens): boolean;
begin
  Result := (FState = stDefault);
  if Result and (token = sqltIdentifier)  then
    FindReservedWord(FString,token);
end;

procedure TSQLTokeniser.QueueToken(token: TSQLTokens; text: AnsiString);
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

procedure TSQLTokeniser.ResetQueue(token: TSQLTokens; text: AnsiString);
begin
  ResetQueue;
  QueueToken(token,text);
end;

procedure TSQLTokeniser.ResetQueue(token: TSQLTokens);
begin
  ResetQueue;
  QueueToken(token);
end;

procedure TSQLTokeniser.ReleaseQueue(var token: TSQLTokens);
begin
  FQueueState := tsRelease;
  PopQueue(token);
end;

procedure TSQLTokeniser.ReleaseQueue;
begin
  FQueueState := tsRelease;
end;

function TSQLTokeniser.GetQueuedText: AnsiString;
var i: integer;
begin
  Result := '';
  for i := FQFirst to FQLast do
    Result := Result + FTokenQueue[i].text;
end;

procedure TSQLTokeniser.SetTokenText(text: AnsiString);
begin
  FString := text;
end;

constructor TSQLTokeniser.Create;
begin
  inherited Create;
  Reset;
end;

destructor TSQLTokeniser.Destroy;
begin
  Reset;
  inherited Destroy;
end;

procedure TSQLTokeniser.Reset;
begin
  FNextToken := sqltInit;
  FState := stDefault;
  FString := '';
  FEOF := false;
  ResetQueue;
end;

function TSQLTokeniser.GetNextToken: TSQLTokens;
begin
  if FQueueState = tsRelease then
  repeat
    PopQueue(Result);
    FEOF := Result = sqltEOF;
    if TokenFound(Result) then
      Exit;
  until FQueueState <> tsRelease;

  Result := InternalGetNextToken;
end;

{a simple lookahead one algorithm to extra the next symbol}

function TSQLTokeniser.InternalGetNextToken: TSQLTokens;
var C: AnsiChar;
begin
  Result := sqltEOF;

  if FNextToken = sqltInit then
    GetNext;

  repeat
    Result := FNextToken;
    C := FLastChar;
    GetNext;

    if FSkipNext then
    begin
      FSkipNext := false;
      continue;
    end;

    case FState of
    stInComment:
      begin
        if (Result = sqltAsterisk) and (FNextToken = sqltForwardSlash) then
        begin
          FState := stDefault;
          Result := sqltComment;
          GetNext;
        end
        else
          FString := FString + C;
      end;

    stInCommentLine:
      if Result = sqltEOL then
      begin
        FState := stDefault;
        Result := sqltCommentLine;
      end
      else
        FString := FString + C;

    stSingleQuoted:
      begin
        if (Result = sqltSingleQuotes) then
        begin
          if (FNextToken = sqltSingleQuotes) then
          begin
            FSkipNext := true;
            FString := FString + C;
          end
          else
          begin
            Result := sqltQuotedString;
            FState := stDefault;
          end;
        end
        else
          FString := FString + C;
      end;

    stDoubleQuoted:
      begin
        if (Result = sqltDoubleQuotes) then
        begin
          if (FNextToken = sqltDoubleQuotes) then
          begin
            FSkipNext := true;
            FString := FString + C;
          end
          else
          begin
            Result := sqltIdentifierInDoubleQuotes;
            FState := stDefault;
          end;
        end
        else
          FString := FString + C;
      end;

    stInIdentifier:
      begin
        FString := FString + C;
        Result := sqltIdentifier;
        if not (FNextToken in [sqltIdentifier,sqltNumberString]) then
          FState := stDefault
      end;

    stInNumeric:
      begin
        FString := FString + C;
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
    FEOF := Result = sqltEOF;
  until TokenFound(Result) or EOF;
end;

end.
