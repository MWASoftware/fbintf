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

const
  CRLF = #13 + #10;
  CR   = #13;
  LF   = #10;
  TAB  = #9;
  NULL_TERMINATOR = #0;

  sqlReservedWords: array [0..197] of string = (
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

function Max(n1, n2: Integer): Integer;
function Min(n1, n2: Integer): Integer;
function RandomString(iLength: Integer): AnsiString;
function RandomInteger(iLow, iHigh: Integer): Integer;
function StripString(st: AnsiString; CharsToStrip: AnsiString): AnsiString;
function ExtractIdentifier(Dialect: Integer; Value: AnsiString): AnsiString;
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

{$IFDEF HASREQEX}
uses RegExpr;
{$ENDIF}

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

{Returns true if "w" is a Firebird SQL reserved word}

function IsReservedWord(w: AnsiString): boolean;
var i: integer;
begin
     Result := true;
     for i := 0 to Length(sqlReservedWords) - 1 do
         if w = sqlReservedWords[i] then
            Exit;
     Result := false;
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
var RegexObj: TRegExpr;
    scheme: AnsiString;
begin
  ServerName := '';
  DatabaseName := ConnectString;
  PortNo := '';
  Protocol := unknownProtocol;
  RegexObj := TRegExpr.Create;
  try
    {extact database file spec}
    RegexObj.ModifierG := false; {turn off greedy matches}
    RegexObj.Expression := '^([a-zA-Z]+)://([a-zA-Z0-9\-\.]+)(|:[0-9a-zA-Z\-]+)/(.*)$';
    Result := RegexObj.Exec(ConnectString);
    if Result then
    begin
      {URL type connect string}
      scheme := AnsiUpperCase(RegexObj.Match[1]);
      ServerName := RegexObj.Match[2];
      if RegexObj.MatchLen[3] > 0 then
        PortNo := system.Copy(ConnectString,RegexObj.MatchPos[3]+1,RegexObj.MatchLen[3]-1);
      DatabaseName := RegexObj.Match[4];
      if scheme = 'INET' then
        Protocol := inet
      else
      if scheme = 'XNET' then
        Protocol := xnet
      else
      if scheme = 'WNET' then
        Protocol := wnet
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

function ParseConnectString(ConnectString: AnsiString; var ServerName,
  DatabaseName: AnsiString; var Protocol: TProtocolAll; var PortNo: integer
  ): boolean;
begin
  Result := false;
end;

{$ENDIF}

{Make a connect string in format appropriate protocol}

function MakeConnectString(ServerName, DatabaseName: AnsiString;
  Protocol: TProtocol; PortNo: AnsiString): AnsiString;
begin
  if PortNo <> '' then
    case Protocol of
    NamedPipe:
      ServerName += '@' + PortNo;
    Local,
    SPX,
    xnet: {do nothing};
    TCP:
      ServerName += '/' + PortNo;
    else
      ServerName += ':' + PortNo;
    end;

  case Protocol of
    TCP:        Result := ServerName + ':' + DatabaseName; {do not localize}
    SPX:        Result := ServerName + '@' + DatabaseName; {do not localize}
    NamedPipe:  Result := '\\' + ServerName + '\' + DatabaseName; {do not localize}
    Local:      Result := DatabaseName; {do not localize}
    inet:       Result := 'inet://' + ServerName + '/'+ DatabaseName; {do not localize}
    wnet:       Result := 'wnet://' + ServerName + '/'+ DatabaseName; {do not localize}
    xnet:       Result := 'xnet://' + ServerName + '/'+ DatabaseName;  {do not localize}
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

end.
