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
{$ENDIF}

{ $IF declared(CompilerVersion) and (CompilerVersion >= 22)}
{ $define HASDELPHIREQEX}
{ $IFEND}

interface

uses Classes, SysUtils, IB;

{$IF not defined(LineEnding)}
const
  {$IFDEF WINDOWS}
  LineEnding = #$0D#$0A;
  {$ELSE}
  LineEnding = #$0A;
  {$ENDIF}
{$IFEND}

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
  sqltBackslash,
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
  sqltMinus,
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
    function ReadCharacters(NumOfChars: integer): AnsiString;

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

    { TSQLParamProcessor }

  TSQLParamProcessor = class(TSQLwithNamedParamsTokeniser)
  private
  const
    sIBXParam = 'IBXParam';  {do not localize}
  private
    FInString: AnsiString;
    FIndex: integer;
    function DoExecute(GenerateParamNames: boolean;
        var slNames: TStrings): AnsiString;
  protected
    function GetChar: AnsiChar; override;
  public
    class function Execute(sSQL: AnsiString; GenerateParamNames: boolean;
        var slNames: TStrings): AnsiString;
  end;

  TOnProgressEvent = procedure (Sender: TObject; Reset: boolean; value: integer) of object;

  { TSQLXMLReader - used to save and read back blob and array data in a pseudo XML format}

  TSQLXMLReader = class(TSQLTokeniser)
  private
      type
        TXMLStates =  (stNoXML, stInTag,stInTagBody,
                       stAttribute,stAttributeValue,stQuotedAttributeValue,
                       stInEndTag, stInEndTagBody,
                       stXMLData);

        TXMLTag    =   (xtNone,xtBlob,xtArray,xtElt);

        TXMLTagDef = record
          XMLTag: TXMLTag;
          TagValue: string;
        end;

      const
        XMLTagDefs: array [xtBlob..xtElt] of TXMLTagDef = (
          (XMLTag: xtBlob;   TagValue: 'blob'),
          (XMLTag: xtArray;  TagValue: 'array'),
          (XMLTag: xtElt;    TagValue: 'elt')
          );
        MaxXMLTags = 20;
        BlobLineLength = 40;

    public
      const
        ibx_blob = 'IBX_BLOB';
        ibx_array = 'IBX_ARRAY';

      type
        TBlobData = record
          BlobIntf: IBlob;
          SubType: cardinal;
        end;

        TArrayData = record
          ArrayIntf: IArray;
          SQLType: cardinal;
          relationName: string;
          columnName: string;
          dim: cardinal;
          Size: cardinal;
          Scale: integer;
          CharSet: string;
          bounds: TArrayBounds;
          CurrentRow: integer;
          Index: array of integer;
        end;

  private
    FOnProgressEvent: TOnProgressEvent;
    FXMLState: TXMLStates;
    FXMLTagStack: array [1..MaxXMLTags] of TXMLTag;
    FXMLTagIndex: integer;
    FAttributeName: string;
    FBlobData: array of TBlobData;
    FCurrentBlob: integer;
    FBlobBuffer: PByte;
    FArrayData: array of TArrayData;
    FCurrentArray: integer;
    FXMLString: string;
    function FindTag(tag: string; var xmlTag: TXMLTag): boolean;
    function GetArrayData(index: integer): TArrayData;
    function GetArrayDataCount: integer;
    function GetBlobData(index: integer): TBlobData;
    function GetBlobDataCount: integer;
    function GetTagName(xmltag: TXMLTag): string;
    procedure ProcessAttributeValue(attrValue: string);
    procedure ProcessBoundsList(boundsList: string);
    procedure ProcessTagValue(tagValue: string);
    procedure XMLTagInit(xmltag: TXMLTag);
    function XMLTagEnd(var xmltag: TXMLTag): boolean;
    procedure XMLTagEnter;
  protected
    function GetAttachment: IAttachment; virtual; abstract;
    function GetTransaction: ITransaction; virtual; abstract;
    function GetErrorPrefix: string; virtual; abstract;
    function TokenFound(var token: TSQLTokens): boolean; override;
    procedure Reset; override;
    procedure ShowError(msg: string; params: array of const); overload; virtual;
    procedure ShowError(msg: string); overload;
  public
    constructor Create;
    procedure FreeDataObjects;
    class function FormatBlob(Field: ISQLData): string; overload;
    class function FormatBlob(contents: string; subtype:integer): string; overload;
    class function FormatArray(ar: IArray): string;
    property BlobData[index: integer]: TBlobData read GetBlobData;
    property BlobDataCount: integer read GetBlobDataCount;
    property ArrayData[index: integer]: TArrayData read GetArrayData;
    property ArrayDataCount: integer read GetArrayDataCount;
    property Attachment: IAttachment read GetAttachment;
    property Transaction: ITransaction read GetTransaction;
    property OnProgressEvent: TOnProgressEvent read FOnProgressEvent write FOnProgressEvent; {Progress Bar Support}
 end;

 TJnlEntryType = (jeTransStart, jeTransCommit, jeTransCommitFail, jeTransCommitRet, jeTransRollback,
                   jeTransRollbackFail, jeTransRollbackRet, jeTransEnd, jeQuery,jeUnknown);

 TJnlEntry = record
   JnlEntryType: TJnlEntryType;
   Timestamp: TDateTime;
   AttachmentID: cardinal;
   SessionID: cardinal;
   TransactionID: cardinal;
   OldTransactionID: cardinal;
   TransactionName: AnsiString;
   TPB: ITPB;
   DefaultCompletion: TTransactionCompletion;
   QueryText: AnsiString;
 end;

 TOnNextJournalEntry = procedure(JnlEntry: TJnlEntry) of object;

 { TJournalProcessor - used to parse a client side journal}

   TJournalProcessor = class(TSQLTokeniser)
    private
      type TLineState = (lsInit, lsJnlFound, lsGotTimestamp, lsGotJnlType,
                          lsGotAttachmentID, lsGotSessionID,
                          lsGotTransactionID,  lsGotOldTransactionID, lsGotText1Length,
                          lsGotText1, lsGotText2Length, lsGotText2);
    private
      FOnNextJournalEntry: TOnNextJournalEntry;
      FInStream: TStream;
      FFirebirdClientAPI: IFirebirdAPI;
      procedure DoExecute;
      function IdentifyJnlEntry(aTokenText: AnsiString): TJnlEntryType;
    protected
      function GetChar: AnsiChar; override;
      property OnNextJournalEntry: TOnNextJournalEntry read FOnNextJournalEntry write FOnNextJournalEntry;
    public
      destructor Destroy; override;
      class procedure Execute( aFileName: string; api: IFirebirdAPI; aOnNextJournalEntry: TOnNextJournalEntry);
      class function JnlEntryText(je: TJnlEntryType): string;
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

{$IF declared(TFormatSettings)}
function ParseDateTimeTZString(aDateTimeStr: Ansistring; var aDateTime: TDateTime;
              var aTimezone: AnsiString; aFormatSettings: TFormatSettings; TimeOnly: boolean=false): boolean; overload;
{$IFEND}
function ParseDateTimeTZString(aDateTimeStr: Ansistring; var aDateTime: TDateTime;
              var aTimezone: AnsiString; TimeOnly: boolean=false): boolean;  overload;
procedure FBDecodeTime(aTime: TDateTime; var Hour, Minute, Second: word; var DeciMillisecond: cardinal);
function FBEncodeTime(Hour, Minute, Second, DeciMillisecond: cardinal): TDateTime;
function FBFormatDateTime(fmt: AnsiString; aDateTime: TDateTime): AnsiString;
function FormatTimeZoneOffset(EffectiveTimeOffsetMins: integer): AnsiString;
function DecodeTimeZoneOffset(TZOffset: AnsiString; var dstOffset: integer): boolean;
function StripLeadingZeros(Value: AnsiString): AnsiString;
function StringToHex(octetString: string; MaxLineLength: integer=0): string; overload;
procedure StringToHex(octetString: string; TextOut: TStrings; MaxLineLength: integer=0); overload;


implementation

uses FBMessages, Math

{$IFDEF FPC}
,RegExpr
{$ELSE}
{$IF declared(CompilerVersion) and (CompilerVersion >= 22)}
, RegularExpressions
{$IFEND}
{$ENDIF};

resourcestring
  sXMLStackUnderflow = 'XML Stack Underflow';
  sInvalidEndTag = 'XML End Tag Mismatch - %s';
  sBadEndTagClosing = 'XML End Tag incorrectly closed';
  sXMLStackOverFlow = 'XML Stack Overflow';
  sXMLAttributeError = 'Unexpected attribute - "%s" = "%s"';
  sInvalidBoundsList = 'Invalid array bounds list - "%s"';
  sBinaryBlockMustbeEven = 'Binary block must have an even number of characters';
  sArrayIndexError = 'Array Index Error (%d)';
  sBlobIndexError = 'Blob Index Error (%d)';
  sNoDatabase = 'Missing database for xml tag import';
  sNoTransaction = 'Missing transaction for xml tag import';


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
  Value := TrimRight(Value);
  if Dialect = 1 then
    Value := AnsiUpperCase(Value)
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

function SchemeToProtocol(scheme: AnsiString): TProtocolAll;
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

{Extracts the Database Connect string from a Create Database Statement}

{$IF declared(TRegexpr)}
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
      Protocol := SchemeToProtocol(RegexObj.Match[1]);
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
        Protocol := SchemeToProtocol(RegexObj.Match[1]);
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

{$ELSE}
{$IF declared(TRegex)}
function ExtractConnectString(const CreateSQL: AnsiString;
  var ConnectString: AnsiString): boolean;
var Regex: TRegEx;
    Match: TMatch;
begin
  Regex := TRegEx.Create('^ *CREATE +(DATABASE|SCHEMA) +''(.*)''',[roIgnoreCase]);
  {extact database file spec}
  Match := Regex.Match(CreateSQL);
  Result := Match.Success and (Match.Groups.Count = 3);
  if Result then
    ConnectString := Match.Groups[2].Value;
end;

function ParseConnectString(ConnectString: AnsiString; var ServerName,
  DatabaseName: AnsiString; var Protocol: TProtocolAll; var PortNo: AnsiString
  ): boolean;

var Regex: TRegEx;
    Match: TMatch;
begin
  ServerName := '';
  DatabaseName := ConnectString;
  PortNo := '';
  Protocol := unknownProtocol;
  {extact database file spec}
  Match := Regex.Match(ConnectString,'^([a-zA-Z46]+)://([a-zA-Z0-9\-\.]*)(|:[0-9a-zA-Z\-]+)/(.*)$',[roIgnoreCase]);
  Result := Match.Success and (Match.Groups.Count = 5);
  if Result then
  begin
    {URL type connect string}
    Protocol := SchemeToProtocol(Match.Groups[1].Value);
    ServerName := Match.Groups[2].Value;
    PortNo := Match.Groups[3].Value;
    DatabaseName := Match.Groups[4].Value;
    if ServerName = '' then
      DatabaseName := '/' + DatabaseName;
  end
  else
  begin
    {URL type connect string - local loop}
    Match := Regex.Match(ConnectString,'^([a-zA-Z46]+)://(.*)$',[roIgnoreCase]);
    Result := Match.Success and (Match.Groups.Count = 3);
    if Result then
    begin
      Protocol := SchemeToProtocol(Match.Groups[1].Value);
      DatabaseName := Match.Groups[2].Value;
    end
    else
    begin
      Match := Regex.Match(ConnectString,'^([a-zA-Z]:\\.*)',[roIgnoreCase]);
      Result := Match.Success;
      if Result then
        Protocol := Local {Windows with leading drive ID}
      else
      begin
        Match := Regex.Match(ConnectString,'^([a-zA-Z0-9\-\.]+)(|/[0-9a-zA-Z\-]+):(.*)$',[roIgnoreCase]);
        Result := Match.Success and (Match.Groups.Count = 4);
        if Result then
        begin
          {Legacy TCP Format}
          ServerName := Match.Groups[1].Value;
          PortNo := Match.Groups[2].Value;
          DatabaseName := Match.Groups[3].Value;
          Protocol := TCP;
        end
        else
        begin
          Match := Regex.Match(ConnectString,'^\\\\([a-zA-Z0-9\-\.]+)(|@[0-9a-zA-Z\-]+)\\(.*)$',[roIgnoreCase]);
          Result := Match.Success and (Match.Groups.Count = 4);
          if Result then
          begin
            {Netbui}
            ServerName := Match.Groups[1].Value;
            PortNo := Match.Groups[2].Value;
            DatabaseName := Match.Groups[3].Value;
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
end;
{$ELSE}
{cruder version of above for Delphi < XE. Older versions lack regular expression
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

function ParseConnectString(ConnectString: AnsiString;
              var ServerName, DatabaseName: AnsiString; var Protocol: TProtocolAll;
              var PortNo: AnsiString): boolean;
begin
  Result := false;
end;

{$IFEND}
{$IFEND}

function GetProtocol(ConnectString: AnsiString): TProtocolAll;
var ServerName,
    DatabaseName: AnsiString;
    PortNo: AnsiString;
begin
  if not ParseConnectString(ConnectString,ServerName,DatabaseName,Result,PortNo) then
    Result := unknownProtocol;
end;

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
  if ServerName = '' then ServerName := 'localhost';
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
  Value := TrimRight(Value);
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

{ TSQLParamProcessor }

function TSQLParamProcessor.DoExecute(GenerateParamNames: boolean;
  var slNames: TStrings): AnsiString;
var token: TSQLTokens;
    iParamSuffix: Integer;
begin
  Result := '';
  iParamSuffix := 0;

  while not EOF do
  begin
    token := GetNextToken;
    case token of
    sqltParam,
    sqltQuotedParam:
      begin
        Result := Result + '?';
        slNames.Add(TokenText);
      end;

    sqltPlaceHolder:
      if GenerateParamNames then
      begin
        Inc(iParamSuffix);
        slNames.AddObject(sIBXParam + IntToStr(iParamSuffix),self); //Note local convention
                                            //add pointer to self to mark entry
        Result := Result + '?';
      end
      else
        IBError(ibxeSQLParseError, [SParamNameExpected]);

    sqltQuotedString:
      Result := Result + '''' + SQLSafeString(TokenText) + '''';

    sqltIdentifierInDoubleQuotes:
      Result := Result + '"' + StringReplace(TokenText,'"','""',[rfReplaceAll]) + '"';

    sqltComment:
      Result := Result + '/*' + TokenText + '*/';

    sqltCommentLine:
      Result := Result + '--' + TokenText + LineEnding;

    sqltEOL:
      Result := Result + LineEnding;

    else
      Result := Result + TokenText;
    end;
  end;
end;

function TSQLParamProcessor.GetChar: AnsiChar;
begin
  if FIndex <= Length(FInString) then
  begin
    Result := FInString[FIndex];
    Inc(FIndex);
  end
  else
    Result := #0;
end;

class function TSQLParamProcessor.Execute(sSQL: AnsiString;
  GenerateParamNames: boolean; var slNames: TStrings): AnsiString;
begin
  with self.Create do
  try
    FInString := sSQL;
    FIndex := 1;
    Result := DoExecute(GenerateParamNames,slNames);
  finally
    Free;
  end;
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
      sqltBegin,
      sqltCase:
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
    '\':
      Result := sqltBackslash;
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
    '-':
      Result := sqltMinus;
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

function TSQLTokeniser.ReadCharacters(NumOfChars: integer): AnsiString;
var i: integer;
begin
  Result := FLastChar;
  for i := 2 to NumOfChars do
  begin
    if GetNext = sqltEOF then Exit;
    Result := Result + FLastChar;
  end;
  GetNext;
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
    if FSkipNext then
    begin
      FSkipNext := false;
      GetNext;
    end;

    Result := FNextToken;
    C := FLastChar;
    GetNext;

    if (Result = sqltCR) and (FNextToken = sqltEOL) then
    begin
      FSkipNext := true;
      Result := sqltEOL;
      C := LF;
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
        if Result = sqltEOL then
          FString := FString + LineEnding
        else
          FString := FString + C;
      end;

    stInCommentLine:
      begin
        case Result of
        sqltEOL:
          begin
            FState := stDefault;
            Result := sqltCommentLine;
          end;

        else
          FString := FString + C;
        end;
      end;

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
        if Result = sqltEOL then
          FString := FString + LineEnding
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
        if Result = sqltEOL then
          FString := FString + LineEnding
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
          end;

        sqltMinus:
          begin
            if FNextToken = sqltMinus then
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
          if FNextToken in [sqltIdentifier,sqltNumberString] then
            FState := stInIdentifier;

        sqltNumberString:
          if FNextToken in [sqltNumberString,sqltPeriod] then
            FState := stInNumeric;

        sqltEOL:
          FString := LineEnding;
        end;
      end;
    end;

//    writeln(FString);
    FEOF := Result = sqltEOF;
  until TokenFound(Result) or EOF;
end;

function ParseDateTimeTZString(aDateTimeStr: Ansistring; var aDateTime: TDateTime;
  var aTimezone: AnsiString; TimeOnly: boolean): boolean;
{$IF declared(TFormatSettings)}
begin
    {$IF declared(DefaultFormatSettings)}
    Result := ParseDateTimeTZString(aDateTimeStr,aDateTime,aTimeZone,DefaultFormatSettings,TimeOnly);
    {$ELSE}
    {$IF declared(FormatSettings)}
    Result := ParseDateTimeTZString(aDateTimeStr,aDateTime,aTimeZone,FormatSettings,TimeOnly);
    {$IFEND} {$IFEND}
end;

function ParseDateTimeTZString(aDateTimeStr: Ansistring; var aDateTime: TDateTime;
              var aTimezone: AnsiString; aFormatSettings: TFormatSettings; TimeOnly: boolean=false): boolean;
{$IFEND}
const
  whitespacechars = [' ',#$09,#$0A,#$0D];
var i,j,l: integer;
    aTime: TDateTime;
    DMs: longint;
begin
  Result := false;
  aTimezone := '';
  if aDateTimeStr <> '' then
  {$if declared(TFormatSettings)}
  with aFormatSettings do
  {$IFEND}
  begin
    aDateTime := 0;
    {Parse to get time zone info}
    i := 1;
    while (i <= length(aDateTimeStr)) and (aDateTimeStr[i] in whitespacechars) do inc(i); {skip white space}
    if not TimeOnly then
    begin
      {decode date}
      j := i;
      while (j <= length(aDateTimeStr)) and (aDateTimeStr[j] in ['0'..'9',DateSeparator]) do inc(j);
      if TryStrToDate(system.copy(aDateTimeStr,i,j-i),aDateTime) then
        i := j; {otherwise start again i.e. assume time only}
    end;

    while (i <= length(aDateTimeStr)) and (aDateTimeStr[i] in whitespacechars) do inc(i); {skip white space}
    {decode time}
    j := i;
    while (j <= length(aDateTimeStr)) and (aDateTimeStr[j] in ['0'..'9',TimeSeparator]) do inc(j);
    Result := TryStrToTime(system.copy(aDateTimeStr,i,j-i),aTime);
    if not Result then Exit;
    aDateTime := aDateTime + aTime;
    i := j;

    {is there a factional second part}
    if (i <= length(aDateTimeStr)) and (aDateTimeStr[i] = '.') then
    begin
      inc(i);
      inc(j);
      while (j <= Length(aDateTimeStr)) and (aDateTimeStr[j] in ['0'..'9']) do inc(j);
      if j > i then
      begin
        l := j-i;
        if l > 4 then l := 4;
        Result := TryStrToInt(system.copy(aDateTimeStr,i,l),DMs);
        if not Result then Exit;

        {adjust for number of significant digits}
        case l of
        3:   DMs := DMs * 10;
        2:   DMs := DMs * 100;
        1:   DMs := DMs * 1000;
        end;
       aDateTime := aDateTime + (DMs / (MsecsPerDay*10));
      end;
    end;
    i := j;

    while (i <= length(aDateTimeStr)) and (aDateTimeStr[i] in whitespacechars) do inc(i); {skip white space}
    {decode time zone}
    if i < length(aDateTimeStr) then
    begin
      j := i;
      while (j <= length(aDateTimeStr)) and not (aDateTimeStr[j] in whitespacechars) do inc(j);
      aTimezone := system.copy(aDateTimeStr,i,j-i);
    end;
    Result := true;
  end
end;

{The following is similar to FPC DecodeTime except that the Firebird standard
 decimilliseconds is used instead of milliseconds for fractional seconds}

procedure FBDecodeTime(aTime: TDateTime; var Hour, Minute, Second: word;
  var DeciMillisecond: cardinal);
var D : Double;
    l : cardinal;
begin
  {conversion to decimilliseconds hacked from FPC DateTimeToTimeStamp}
  D := aTime * MSecsPerDay *10;
  if D < 0 then
    D := D - 0.5
  else
    D := D + 0.5;
  {rest hacked from FPC DecodeTIme}
  l := Abs(Trunc(D)) Mod (MSecsPerDay*10);
  Hour   := l div 36000000;
  l := l mod 36000000;
  Minute := l div 600000;
  l := l mod 600000;
  Second := l div 10000;
  DeciMillisecond := l mod 10000;
end;

{The following is similar to FPC EncodeTime except that the Firebird standard
 decimilliseconds is used instead of milliseconds for fractional seconds}

function FBEncodeTime(Hour, Minute, Second, DeciMillisecond: cardinal): TDateTime;
const DMSecsPerDay = MSecsPerDay*10;
var DMs: cardinal;
    D: Double;
begin
  if (Hour<24) and (Minute<60) and (Second<60) and (DeciMillisecond<10000) then
  begin
    DMs := Hour*36000000+Minute*600000+Second*10000+DeciMillisecond;
    D := DMs/DMSecsPerDay;
    Result:=TDateTime(d)
  end
  else
    IBError(ibxeBadTimeSpecification,[Hour, Minute, Second, DeciMillisecond]);
end;

{The following is similar to FPC FormatDateTime except that it additionally
 allows the timstamp to have a fractional seconds component with a resolution
 of four decimal places. This is appended to the result for FormatDateTime
 if the format string contains a "zzzz' string.}

function FBFormatDateTime(fmt: AnsiString; aDateTime: TDateTime): AnsiString;
var Hour, Minute, Second: word;
    DeciMillisecond: cardinal;
begin
  if Pos('zzzz',fmt) > 0 then
  begin
    FBDecodeTime(aDateTime, Hour, Minute, Second, DeciMillisecond);
    fmt := StringReplace(fmt, 'zzzz', Format('%.4d',[DeciMillisecond]), [rfReplaceAll]);
  end;
  Result := FormatDateTime(fmt,aDateTime);
end;

function FormatTimeZoneOffset(EffectiveTimeOffsetMins: integer): AnsiString;
begin
  if EffectiveTimeOffsetMins > 0 then
    Result := Format('+%.2d:%.2d',[EffectiveTimeOffsetMins div 60,abs(EffectiveTimeOffsetMins mod 60)])
  else
    Result := Format('%.2d:%.2d',[EffectiveTimeOffsetMins div 60,abs(EffectiveTimeOffsetMins mod 60)]);
end;

function DecodeTimeZoneOffset(TZOffset: AnsiString; var dstOffset: integer): boolean;
var i: integer;
begin
  Result := false;
  TZOffset := Trim(TZOffset);
  for i := 1 to Length(TZOffset) do
    if not (TZOffset[i] in ['0'..'9','-','+',':']) then Exit;

  Result := true;
  i := Pos(':',TZOffset);
  if i > 0 then
    dstOffset := StrToInt(copy(TZOffset,1,i-1)) * 60 + StrToInt(copy(TZOffset,i + 1))
  else
    dstOffset := StrToInt(TZOffset) * 60;
end;

function StripLeadingZeros(Value: AnsiString): AnsiString;
var i: Integer;
    start: integer;
begin
  Result := '';
  start := 1;
  if (Length(Value) > 0) and (Value[1] = '-') then
  begin
    Result := '-';
    start := 2;
  end;
  for i := start to Length(Value) do
    if Value[i] <> '0' then
    begin
      Result := Result + system.copy(Value, i, MaxInt);
      Exit;
    end;
end;

function StringToHex(octetString: string; MaxLineLength: integer): string; overload;

  function ToHex(aValue: byte): string;
  const
    HexChars: array [0..15] of char = '0123456789ABCDEF';
  begin
    Result := HexChars[aValue shr 4] +
               HexChars[(aValue and $0F)];
  end;

var i, j: integer;
begin
  i := 1;
  Result := '';
  if MaxLineLength = 0 then
  while i <= Length(octetString) do
  begin
    Result := Result +  ToHex(byte(octetString[i]));
    Inc(i);
  end
  else
  while i <= Length(octetString) do
  begin
      for j := 1 to MaxLineLength do
      begin
        if i > Length(octetString) then
          Exit
        else
          Result := Result + ToHex(byte(octetString[i]));
        inc(i);
      end;
      Result := Result + LineEnding;
  end;
end;

procedure StringToHex(octetString: string; TextOut: TStrings; MaxLineLength: integer); overload;
begin
    TextOut.Add(StringToHex(octetString,MaxLineLength));
end;

{ TSQLXMLReader }

function TSQLXMLReader.FindTag(tag: string; var xmlTag: TXMLTag): boolean;
var i: TXMLTag;
begin
  Result := false;
  for i := xtBlob to xtElt do
    if XMLTagDefs[i].TagValue = tag then
    begin
      xmlTag := XMLTagDefs[i].XMLTag;
      Result := true;
      break;
    end;
end;

function TSQLXMLReader.GetArrayData(index: integer): TArrayData;
begin
  if (index < 0) or (index > ArrayDataCount) then
    ShowError(sArrayIndexError,[index]);
  Result := FArrayData[index];
end;

function TSQLXMLReader.GetArrayDataCount: integer;
begin
  Result := Length(FArrayData);
end;

function TSQLXMLReader.GetBlobData(index: integer): TBlobData;
begin
  if (index < 0) or (index > BlobDataCount) then
    ShowError(sBlobIndexError,[index]);
  Result := FBlobData[index];
end;

function TSQLXMLReader.GetBlobDataCount: integer;
begin
  Result := Length(FBlobData);
end;

function TSQLXMLReader.GetTagName(xmltag: TXMLTag): string;
var i: TXMLTag;
begin
  Result := 'unknown';
  for i := xtBlob to xtElt do
    if XMLTagDefs[i].XMLTag = xmltag then
    begin
      Result := XMLTagDefs[i].TagValue;
      Exit;
    end;
end;

procedure TSQLXMLReader.ProcessAttributeValue(attrValue: string);
begin
  case FXMLTagStack[FXMLTagIndex] of
  xtBlob:
    if FAttributeName = 'subtype' then
      FBlobData[FCurrentBlob].SubType := StrToInt(attrValue)
    else
      ShowError(sXMLAttributeError,[FAttributeName,attrValue]);

  xtArray:
    if FAttributeName = 'sqltype' then
      FArrayData[FCurrentArray].SQLType := StrToInt(attrValue)
    else
    if FAttributeName = 'relation_name' then
      FArrayData[FCurrentArray].relationName := attrValue
    else
    if FAttributeName = 'column_name' then
      FArrayData[FCurrentArray].columnName := attrValue
    else
    if FAttributeName = 'dim' then
      FArrayData[FCurrentArray].Dim := StrToInt(attrValue)
    else
    if FAttributeName = 'length' then
      FArrayData[FCurrentArray].Size := StrToInt(attrValue)
    else
    if FAttributeName = 'scale' then
      FArrayData[FCurrentArray].Scale := StrToInt(attrValue)
    else
    if FAttributeName = 'charset' then
      FArrayData[FCurrentArray].CharSet := attrValue
    else
    if FAttributeName = 'bounds' then
      ProcessBoundsList(attrValue)
    else
      ShowError(sXMLAttributeError,[FAttributeName,attrValue]);

  xtElt:
    if FAttributeName = 'ix' then
      with FArrayData[FCurrentArray] do
        Index[CurrentRow] :=  StrToInt(attrValue)
     else
        ShowError(sXMLAttributeError,[FAttributeName,attrValue]);
  end;
end;

procedure TSQLXMLReader.ProcessBoundsList(boundsList: string);
var list: TStringList;
    i,j: integer;
begin
  list := TStringList.Create;
  try
    list.Delimiter := ',';
    list.DelimitedText := boundsList;
    with FArrayData[FCurrentArray] do
    begin
      if dim <> list.Count then
        ShowError(sInvalidBoundsList,[boundsList]);
      SetLength(bounds,dim);
      for i := 0 to list.Count - 1 do
      begin
        j := Pos(':',list[i]);
        if j = 0 then
          raise Exception.CreateFmt(sInvalidBoundsList,[boundsList]);
        bounds[i].LowerBound := StrToInt(system.copy(list[i],1,j-1));
        bounds[i].UpperBound := StrToInt(system.copy(list[i],j+1,length(list[i])-j));
      end;
    end;
  finally
    list.Free;
  end;
end;

procedure TSQLXMLReader.ProcessTagValue(tagValue: string);

  function nibble(hex: char): byte;
  begin
    case hex of
    '0': Result := 0;
    '1': Result := 1;
    '2': Result := 2;
    '3': Result := 3;
    '4': Result := 4;
    '5': Result := 5;
    '6': Result := 6;
    '7': Result := 7;
    '8': Result := 8;
    '9': Result := 9;
    'a','A': Result := 10;
    'b','B': Result := 11;
    'c','C': Result := 12;
    'd','D': Result := 13;
    'e','E': Result := 14;
    'f','F': Result := 15;
    end;
  end;

  procedure RemoveWhiteSpace(var hexData: string);
  var i: integer;
  begin
    {Remove White Space}
    i := 1;
    while i <= length(hexData) do
    begin
      case hexData[i] of
      ' ',#9,#10,#13:
        begin
          if i < Length(hexData) then
            Move(hexData[i+1],hexData[i],Length(hexData)-i);
          SetLength(hexData,Length(hexData)-1);
        end;
      else
        Inc(i);
      end;
    end;
  end;

  procedure WriteToBlob(hexData: string);
  var i,j : integer;
      blength: integer;
      P: PByte;
  begin
    RemoveWhiteSpace(hexData);
    if odd(length(hexData)) then
      ShowError(sBinaryBlockMustbeEven,[nil]);
    blength := Length(hexData) div 2;
    ReallocMem(FBlobBuffer,blength);
    j := 1;
    P := FBlobBuffer;
    for i := 1 to blength do
    begin
      P^ := (nibble(hexData[j]) shl 4) or nibble(hexdata[j+1]);
      Inc(j,2);
      Inc(P);
    end;
    FBlobData[FCurrentBlob].BlobIntf.Write(FBlobBuffer^,blength);
  end;

begin
  if tagValue = '' then Exit;
  case FXMLTagStack[FXMLTagIndex] of
  xtBlob:
    WriteToBlob(tagValue);

  xtElt:
    with FArrayData[FCurrentArray] do
      ArrayIntf.SetAsString(index,tagValue);

  end;
end;

procedure TSQLXMLReader.XMLTagInit(xmltag: TXMLTag);
begin
  if FXMLTagIndex > MaxXMLTags then
    ShowError(sXMLStackOverFlow,[nil]);
  Inc(FXMLTagIndex);
  FXMLTagStack[FXMLTagIndex] := xmltag;
  FXMLString := '';

  case xmltag of
  xtBlob:
    begin
      Inc(FCurrentBlob);
      SetLength(FBlobData,FCurrentBlob+1);
      FBlobData[FCurrentBlob].BlobIntf := nil;
      FBlobData[FCurrentBlob].SubType := 0;
    end;

  xtArray:
    begin
      Inc(FCurrentArray);
      SetLength(FArrayData,FCurrentArray+1);
      with FArrayData[FCurrentArray] do
      begin
        ArrayIntf := nil;
        SQLType := 0;
        dim := 0;
        Size := 0;
        Scale := 0;
        CharSet := 'NONE';
        SetLength(Index,0);
        CurrentRow := -1;
      end;
    end;

  xtElt:
      with FArrayData[FCurrentArray] do
        Inc(CurrentRow)
  end;
end;

function TSQLXMLReader.XMLTagEnd(var xmltag: TXMLTag): boolean;
begin
  if FXMLTagIndex = 0 then
    ShowError(sXMLStackUnderflow,[nil]);

  xmlTag := FXMLTagStack[FXMLTagIndex];
  case FXMLTagStack[FXMLTagIndex] of
  xtBlob:
    FBlobData[FCurrentBlob].BlobIntf.Close;

  xtArray:
    FArrayData[FCurrentArray].ArrayIntf.SaveChanges;

  xtElt:
    Dec(FArrayData[FCurrentArray].CurrentRow);
  end;
  Dec(FXMLTagIndex);
  Result := FXMLTagIndex = 0;
end;

procedure TSQLXMLReader.XMLTagEnter;
var aCharSetID: integer;
begin
  if (Attachment = nil) or not Attachment.IsConnected then
    ShowError(sNoDatabase);
  if Transaction = nil then
    ShowError(sNoTransaction);
  case FXMLTagStack[FXMLTagIndex] of
  xtBlob:
    begin
      if not Transaction.InTransaction then
        Transaction.Start;
      FBlobData[FCurrentBlob].BlobIntf := Attachment.CreateBlob(
        Transaction,FBlobData[FCurrentBlob].SubType);
    end;

  xtArray:
    with FArrayData[FCurrentArray] do
    begin
      if not Transaction.InTransaction then
        Transaction.Start;
      Attachment.CharSetName2CharSetID(CharSet,aCharSetID);
      SetLength(Index,dim);
      ArrayIntf := Attachment.CreateArray(
                     Transaction,
                     Attachment.CreateArrayMetaData(SQLType,
                       relationName,columnName,Scale,Size,
                       aCharSetID,dim,bounds)
                     );
    end;
  end;
end;

{This is where the XML tags are identified and the token stream modified in
 consequence}

function TSQLXMLReader.TokenFound(var token: TSQLTokens): boolean;

 procedure NotAnXMLTag;
 begin
   begin
     if FXMLTagIndex = 0 then
     {nothing to do with XML so go back to processing SQL}
     begin
       QueueToken(token);
       ReleaseQueue(token);
       FXMLState := stNoXML
     end
     else
     begin
       {Not an XML tag, so just push back to XML Data}
       FXMLState := stXMLData;
       FXMLString := FXMLString + GetQueuedText;
       ResetQueue;
     end;
   end;
 end;

var XMLTag: TXMLTag;
begin
  Result := inherited TokenFound(token);
  if not Result then Exit;

  case FXMLState of
  stNoXML:
    if token = sqltLT then
    begin
      ResetQueue;
      QueueToken(token); {save in case this is not XML}
      FXMLState := stInTag;
    end;

  stInTag:
    {Opening '<' found, now looking for tag name or end tag marker}
    case token of
    sqltIdentifier:
      begin
        if FindTag(TokenText,XMLTag) then
        begin
          XMLTagInit(XMLTag);
          QueueToken(token);
          FXMLState := stInTagBody;
        end
        else
          NotAnXMLTag;
      end;

    sqltForwardSlash:
      FXMLState := stInEndTag;

    else
      NotAnXMLTag;
    end {case token};

  stInTagBody:
    {Tag name found. Now looking for attribute or closing '>'}
    case token of
    sqltIdentifier:
      begin
        FAttributeName := TokenText;
        QueueToken(token);
        FXMLState := stAttribute;
      end;

    sqltGT:
      begin
        ResetQueue;
        XMLTagEnter;
        FXMLState := stXMLData;
      end;

    sqltSpace,
    sqltEOL:
      QueueToken(token);

    else
      NotAnXMLTag;
    end {case token};

  stAttribute:
    {Attribute name found. Must be followed by an '=', a '>' or another tag name}
    case token of
      sqltEquals:
      begin
        QueueToken(token);
        FXMLState := stAttributeValue;
      end;

      sqltSpace,
      sqltEOL:
        QueueToken(token);

      sqltIdentifier:
        begin
          ProcessAttributeValue('');
          FAttributeName := TokenText;
          QueueToken(token);
          FXMLState := stAttribute;
        end;

      sqltGT:
        begin
          ProcessAttributeValue('');
          ResetQueue;
          XMLTagEnter;
          FXMLState := stXMLData;
        end;

      else
        NotAnXMLTag;
    end; {case token}

  stAttributeValue:
    {Looking for attribute value as a single identifier or a double quoted value}
    case token of
    sqltIdentifier,sqltIdentifierInDoubleQuotes:
      begin
        ProcessAttributeValue(TokenText);
        QueueToken(token);
        FXMLState := stInTagBody;
      end;

    sqltSpace,
    sqltEOL:
      QueueToken(token);

    else
      NotAnXMLTag;
    end; {case token}

  stXMLData:
    if token = sqltLT then
    begin
      QueueToken(token); {save in case this is not XML}
      FXMLState := stInTag;
    end
    else
      FXMLString := FXMLString + TokenText;

  stInEndTag:
    {Opening '</' found, now looking for tag name}
    case token of
    sqltIdentifier:
      begin
        if FindTag(TokenText,XMLTag) and (XMLTag = FXMLTagStack[FXMLTagIndex]) then
        begin
          QueueToken(token);
          FXMLState := stInEndTagBody;
        end
        else
          ShowError(sInvalidEndTag,[TokenText]);
      end;
    else
      NotAnXMLTag;
    end {case token};

  stInEndTagBody:
  {End tag name found, now looping for closing '>'}
    case Token of
    sqltGT:
      begin
        ProcessTagValue(FXMLString);
        if XMLTagEnd(XMLTag) then
        begin
          ResetQueue;
          QueueToken(sqltColon,':');
          case XMLTag of
            xtBlob:
              QueueToken(sqltIdentifier,Format(ibx_blob+'%d',[FCurrentBlob]));

            xtArray:
              QueueToken(sqltIdentifier, Format(ibx_array+'%d',[FCurrentArray]));
          end;
          ReleaseQueue(token);
          FXMLState := stNoXML;
       end
       else
         FXMLState := stXMLData;
      end;

    sqltSpace,
    sqltEOL:
      QueueToken(token);

    else
      ShowError(sBadEndTagClosing);
    end; {case token}

  end {Case FState};

  {Only allow token to be returned if not processing an XML tag}

  Result := FXMLState = stNoXML;
end;

procedure TSQLXMLReader.ShowError(msg: string; params: array of const);
begin
  raise EIBClientError.CreateFmt(GetErrorPrefix + msg,params);
end;

procedure TSQLXMLReader.ShowError(msg: string);
begin
  ShowError(msg,[nil]);
end;

constructor TSQLXMLReader.Create;
begin
  inherited;
  FXMLState := stNoXML;
end;

procedure TSQLXMLReader.FreeDataObjects;
begin
  FXMLTagIndex := 0;
  SetLength(FBlobData,0);
  FCurrentBlob := -1;
  SetLength(FArrayData,0);
  FCurrentArray := -1;
end;

class function TSQLXMLReader.FormatBlob(Field: ISQLData): string;
begin
  Result := FormatBlob(Field.AsString,Field.getSubtype);
end;

class function TSQLXMLReader.FormatBlob(contents: string; subtype: integer
  ): string;
var TextOut: TStrings;
begin
  TextOut := TStringList.Create;
  try
    TextOut.Add(Format('<blob subtype="%d">',[subtype]));
    StringToHex(contents,TextOut,BlobLineLength);
    TextOut.Add('</blob>');
    Result := TextOut.Text;
  finally
    TextOut.Free;
  end;
end;


class function TSQLXMLReader.FormatArray(ar: IArray
  ): string;
var index: array of integer;
    TextOut: TStrings;

    procedure AddElements(dim: integer; indent:string = ' ');
    var i: integer;
        recurse: boolean;
    begin
      SetLength(index,dim+1);
      recurse := dim < ar.GetDimensions - 1;
      with ar.GetBounds[dim] do
      for i := LowerBound to UpperBound do
      begin
        index[dim] := i;
        if recurse then
        begin
          TextOut.Add(Format('%s<elt id="%d">',[indent,i]));
          AddElements(dim+1,indent + ' ');
          TextOut.Add('</elt>');
        end
        else
        if ((ar.GetSQLType = SQL_TEXT) or (ar.GetSQLType = SQL_VARYING)) and
           (ar.GetCharSetID = 1) then
           TextOut.Add(Format('%s<elt ix="%d">%s</elt>',[indent,i,StringToHex(ar.GetAsString(index))]))
        else
          TextOut.Add(Format('%s<elt ix="%d">%s</elt>',[indent,i,ar.GetAsString(index)]));
      end;
    end;

var
    s: string;
    bounds: TArrayBounds;
    i: integer;
    boundsList: string;
begin
  TextOut := TStringList.Create;
  try
    if ar.GetCharSetWidth = 0 then
      s := Format('<array dim = "%d" sqltype = "%d" length = "%d" relation_name = "%s" column_name = "%s"',
                              [ar.GetDimensions,ar.GetSQLType,ar.GetSize,
                               ar.GetTableName,ar.GetColumnName])
    else
      s := Format('<array dim = "%d" sqltype = "%d" length = "%d" relation_name = "%s" column_name = "%s"',
                                [ar.GetDimensions,ar.GetSQLType,ar.GetSize div ar.GetCharSetWidth,
                                 ar.GetTableName,ar.GetColumnName]);
    case ar.GetSQLType of
    SQL_DOUBLE, SQL_FLOAT, SQL_LONG, SQL_SHORT, SQL_D_FLOAT, SQL_INT64:
       s := s + Format(' scale = "%d"',[ ar.GetScale]);
    SQL_TEXT,
    SQL_VARYING:
      s := s + Format(' charset = "%s"',[ar.GetAttachment.GetCharsetName(ar.GetCharSetID)]);
    end;
    bounds := ar.GetBounds;
    boundsList := '';
    for i := 0 to length(bounds) - 1 do
    begin
      if i <> 0 then boundsList := boundsList + ',';
      boundsList := boundsList + Format('%d:%d',[bounds[i].LowerBound,bounds[i].UpperBound]);
    end;
    s := s + Format(' bounds="%s"',[boundsList]);
    s := s + '>';
    TextOut.Add(s);

    SetLength(index,0);
    AddElements(0);
    TextOut.Add('</array>');
    Result := TextOut.Text;
  finally
    TextOut.Free;
  end;
end;

procedure TSQLXMLReader.Reset;
begin
  inherited Reset;
  FreeDataObjects;
  FXMLString := '';
  FreeMem(FBlobBuffer);
end;

{ TJournalProcessor }

procedure TJournalProcessor.DoExecute;
var token: TSQLTokens;
    LineState: TLineState;
    JnlEntry: TJnlEntry;
    Len: integer;
    tz: AnsiString;

  procedure ClearJnlEntry;
  begin
    with JnlEntry do
    begin
      TransactionName := '';
      TPB := nil;
      QueryText :='';
      JnlEntryType := jeUnknown;
      SessionID := 0;
      TransactionID := 0;
      DefaultCompletion := taCommit;
    end;
  end;

  function CreateTPB(TPBText: AnsiString): ITPB;
  var index: integer;
  begin
    Result := nil;
    if Length(TPBText) = 0 then
      Exit;
    Result := FFirebirdClientAPI.AllocateTPB;
    try
      index := Pos('[',TPBText);
      if index > 0 then
        system.Delete(TPBText,1,index);
      repeat
        index := Pos(',',TPBText);
        if index = 0 then
        begin
          index := Pos(']',TPBText);
          if index <> 0 then
            system.Delete(TPBText,index,1);
          Result.AddByTypeName(TPBText);
          break;
        end;
        Result.AddByTypeName(system.copy(TPBText,1,index-1));
        system.Delete(TPBText,1,index);
      until false;
    except
      Result := nil;
      raise;
    end;
  end;

begin
  LineState := lsInit;
  JnlEntry.JnlEntryType := jeUnknown;
  while not EOF do
  begin
    if LineState = lsInit then
      ClearJnlEntry;
    token := GetNextToken;
    with JnlEntry do
    case token of
    sqltAsterisk:
      if LineState = lsInit then
        LineState := lsJnlFound;

    sqltIdentifier:
      if LineState = lsJnlFound then
        begin
          JnlEntryType := IdentifyJnlEntry(TokenText);
          LineState := lsGotJnlType;
        end
      else
        LineState := lsInit;

    sqltQuotedString:
      if (LineState = lsGotJnlType)
          and ParseDateTimeTZString(TokenText,TimeStamp,tz) then
            LineState := lsGotTimestamp
      else
        LineState := lsInit;

    sqltColon:
      case LineState of
      lsGotText1Length:
        begin
          if Len > 0 then
          begin
            if JnlEntryType = jeTransStart then
              TransactionName := ReadCharacters(Len)
            else
              QueryText := ReadCharacters(Len)
          end;
          if JnlEntryType = jeTransStart then
             LineState := lsGotText1
          else
          begin
            if assigned(FOnNextJournalEntry) then
              OnNextJournalEntry(JnlEntry);
            LineState := lsInit;
          end
        end;

      lsGotText2Length:
        begin
          if Len > 0 then
            TPB :=  CreateTPB(ReadCharacters(Len));
          LineState := lsGotText2;
        end;

      else
      if LineState <> lsGotJnlType then
        LineState := lsInit;
    end;

   sqltComma:
     if not (LineState in [lsGotTimestamp,lsGotAttachmentID,lsGotSessionID,lsGotTransactionID,lsGotText1,lsGotText2]) then
       LineState := lsInit;

   sqltNumberString:
     case LineState of
     lsGotTimestamp:
       begin
         AttachmentID := StrToInt(TokenText);
         LineState := lsGotAttachmentID;
       end;

     lsGotAttachmentID:
       begin
         SessionID := StrToInt(TokenText);
         LineState := lsGotSessionID;
       end;

     lsGotSessionID:
       begin
         TransactionID := StrToInt(TokenText);
         if JnlEntryType in [jeTransCommit, jeTransCommitFail, jeTransRollback, jeTransRollbackFail] then
         begin
           if assigned(FOnNextJournalEntry) then
             OnNextJournalEntry(JnlEntry);
           LineState := lsInit;
         end
         else
           LineState := lsGotTransactionID;
       end;

     lsGotTransactionID:
       begin
         case JnlEntryType of
         jeTransStart:
           begin
             len := StrToInt(TokenText);
             LineState := lsGotText1Length;
           end;

         jeQuery:
           begin
             len :=  StrToInt(TokenText);
             LineState := lsGotText1Length;
           end;

         jeTransCommitRet,
         jeTransCommitFail,
         jeTransRollbackFail,
         jeTransRollbackRet:
           begin
             OldTransactionID := StrToInt(TokenText);
             if assigned(FOnNextJournalEntry) then
               OnNextJournalEntry(JnlEntry);
             LineState := lsInit;
           end;

           else
             LineState := lsInit;
         end; {case JnlEntryType}

       end;

     lsGotText1:
       begin
         len := StrToInt(TokenText);
         LineState := lsGotText2Length;
       end;

     lsGotText2:
        begin
          if JnlEntryType = jeTransStart then
          begin
            DefaultCompletion := TTransactionCompletion(StrToInt(TokenText));
            if assigned(FOnNextJournalEntry) then
              OnNextJournalEntry(JnlEntry);
          end;
          LineState := lsInit;
        end;
     end; {case LineState}
    end; {case token}
  end; {while}
  ClearJnlEntry;
end;

function TJournalProcessor.IdentifyJnlEntry(aTokenText: AnsiString
  ): TJnlEntryType;
begin
  Result := jeUnknown;
  if Length(aTokenText) > 0 then
  case aTokenText[1] of
  'S':
    Result := jeTransStart;
  'C':
    Result := jeTransCommit;
  'c':
    Result := jeTransCommitRet;
  'R':
    Result := jeTransRollback;
  'r':
    Result := jeTransRollbackRet;
  'E':
    Result := jeTransEnd;
  'Q':
    Result := jeQuery;
  'F':
    Result := jeTransCommitFail;
  'f':
    Result := jeTransRollbackFail;
  end;
end;

class function TJournalProcessor.JnlEntryText(je: TJnlEntryType): string;
begin
  case je of
  jeTransStart:
    Result := 'Transaction Start';
  jeTransCommit:
    Result := 'Commit';
  jeTransCommitFail:
    Result := 'Commit (Failed)';
  jeTransCommitRet:
    Result := 'Commit Retaining';
  jeTransRollback:
    Result := 'Rollback';
  jeTransRollbackFail:
    Result := 'Rollback (Failed)';
  jeTransRollbackRet:
    Result := 'Rollback Retaining';
  jeTransEnd:
    Result := 'Transaction End';
  jeQuery:
    Result := 'Query';
  jeUnknown:
    Result := 'Unknown';
  end;
end;

function TJournalProcessor.GetChar: AnsiChar;
begin
  if FInStream.Read(Result,1) = 0 then
    Result := #0;
end;

destructor TJournalProcessor.Destroy;
begin
  FInStream.Free;
  inherited Destroy;
end;

class procedure TJournalProcessor.Execute(aFileName: string; api: IFirebirdAPI;
  aOnNextJournalEntry: TOnNextJournalEntry);
begin
  with TJournalProcessor.Create do
  try
    FInStream := TFileStream.Create(aFileName,fmOpenRead);
    FFirebirdClientAPI := api;
    OnNextJournalEntry := aOnNextJournalEntry;
    DoExecute;
  finally
    Free
  end;
end;


end.
