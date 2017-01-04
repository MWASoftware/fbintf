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
unit FBSQLData;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

{ This Unit was hacked out of the IBSQL unit and defines a class used as the
  base for interfaces accessing SQLDAVar data and Array Elements. The abstract
  methods are used to customise for an SQLDAVar or Array Element. The empty
  methods are needed for SQL parameters only. The string getters and setters
  are virtual as SQLVar and Array encodings of string data is different.}

{ $define ALLOWDIALECT3PARAMNAMES}

{$ifndef ALLOWDIALECT3PARAMNAMES}

{ Note on SQL Dialects and SQL Parameter Names
  --------------------------------------------

  Even when dialect 3 quoted format parameter names are not supported, IBX still processes
  parameter names case insensitive. This does result in some additional overhead
  due to a call to "AnsiUpperCase". This can be avoided by undefining
  "UseCaseInSensitiveParamName" below.

  Note: do not define "UseCaseSensitiveParamName" when "ALLOWDIALECT3PARAMNAMES"
  is defined. This will not give a useful result.
}
{$define UseCaseInSensitiveParamName}
{$endif}

interface

uses
  Classes, SysUtils, IBExternals, IBHeader, IB,  FBActivityMonitor;

type

  { TSQLDataItem }

  TSQLDataItem = class(TFBInterfacedObject)
  private
     function AdjustScale(Value: Int64; aScale: Integer): Double;
     function AdjustScaleToInt64(Value: Int64; aScale: Integer): Int64;
     function AdjustScaleToCurrency(Value: Int64; aScale: Integer): Currency;
     procedure SetAsInteger(AValue: Integer);
  protected
     function AdjustScaleFromCurrency(Value: Currency; aScale: Integer): Int64;
     function AdjustScaleFromDouble(Value: Double; aScale: Integer): Int64;
     procedure CheckActive; virtual;
     function GetSQLDialect: integer; virtual; abstract;
     procedure Changed; virtual;
     procedure Changing; virtual;
     procedure InternalSetAsString(Value: String); virtual;
     function SQLData: PChar; virtual; abstract;
     function GetDataLength: cardinal; virtual; abstract;
     function GetCodePage: TSystemCodePage; virtual; abstract;
     function getCharSetID: cardinal; virtual; abstract;
     function Transliterate(s: string; CodePage: TSystemCodePage): RawByteString;
     procedure SetScale(aValue: integer); virtual;
     procedure SetDataLength(len: cardinal); virtual;
     procedure SetSQLType(aValue: cardinal); virtual;
     property DataLength: cardinal read GetDataLength write SetDataLength;

  public
     function GetSQLType: cardinal; virtual; abstract;
     function GetSQLTypeName: string; overload;
     class function GetSQLTypeName(SQLType: short): string; overload;
     function GetName: string; virtual; abstract;
     function GetScale: integer; virtual; abstract;
     function GetAsBoolean: boolean;
     function GetAsCurrency: Currency;
     function GetAsInt64: Int64;
     function GetAsDateTime: TDateTime;
     function GetAsDouble: Double;
     function GetAsFloat: Float;
     function GetAsLong: Long;
     function GetAsPointer: Pointer;
     function GetAsQuad: TISC_QUAD;
     function GetAsShort: short;
     function GetAsString: String; virtual;
     function GetIsNull: Boolean; virtual;
     function getIsNullable: boolean; virtual;
     function GetAsVariant: Variant;
     function GetModified: boolean; virtual;
     procedure SetAsBoolean(AValue: boolean); virtual;
     procedure SetAsCurrency(Value: Currency); virtual;
     procedure SetAsInt64(Value: Int64); virtual;
     procedure SetAsDate(Value: TDateTime); virtual;
     procedure SetAsLong(Value: Long); virtual;
     procedure SetAsTime(Value: TDateTime); virtual;
     procedure SetAsDateTime(Value: TDateTime);
     procedure SetAsDouble(Value: Double); virtual;
     procedure SetAsFloat(Value: Float); virtual;
     procedure SetAsPointer(Value: Pointer);
     procedure SetAsQuad(Value: TISC_QUAD);
     procedure SetAsShort(Value: short); virtual;
     procedure SetAsString(Value: String); virtual;
     procedure SetAsVariant(Value: Variant);
     procedure SetIsNull(Value: Boolean); virtual;
     procedure SetIsNullable(Value: Boolean); virtual;
     procedure SetName(aValue: string); virtual;
     property AsDate: TDateTime read GetAsDateTime write SetAsDate;
     property AsBoolean:boolean read GetAsBoolean write SetAsBoolean;
     property AsTime: TDateTime read GetAsDateTime write SetAsTime;
     property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
     property AsDouble: Double read GetAsDouble write SetAsDouble;
     property AsFloat: Float read GetAsFloat write SetAsFloat;
     property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
     property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
     property AsInteger: Integer read GetAsLong write SetAsInteger;
     property AsLong: Long read GetAsLong write SetAsLong;
     property AsPointer: Pointer read GetAsPointer write SetAsPointer;
     property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
     property AsShort: short read GetAsShort write SetAsShort;
     property AsString: String read GetAsString write SetAsString;
     property AsVariant: Variant read GetAsVariant write SetAsVariant;
     property Modified: Boolean read getModified;
     property IsNull: Boolean read GetIsNull write SetIsNull;
     property IsNullable: Boolean read GetIsNullable write SetIsNullable;
     property Scale: integer read GetScale write SetScale;
     property SQLType: cardinal read GetSQLType write SetSQLType;
  end;

  TSQLVarData = class;

  TStatementStatus = (ssPrepared, ssExecuteResults, ssCursorOpen, ssBOF, ssEOF);

  { TSQLDataArea }

  TSQLDataArea = class
  private
    function GetColumn(index: integer): TSQLVarData;
    function GetCount: integer;
  protected
    FUniqueRelationName: string;
    FColumnList: array of TSQLVarData;
    function GetStatement: IStatement; virtual; abstract;
    function GetPrepareSeqNo: integer; virtual; abstract;
    function GetTransactionSeqNo: integer; virtual; abstract;
    procedure SetCount(aValue: integer); virtual; abstract;
    procedure SetUniqueRelationName;
  public
    procedure Initialize; virtual;
    function IsInputDataArea: boolean; virtual; abstract; {Input to Database}
    procedure PreprocessSQL(sSQL: string; GenerateParamNames: boolean;
      var sProcessedSQL: string);
    function ColumnsInUseCount: integer; virtual;
    function ColumnByName(Idx: string): TSQLVarData;
    function CheckStatementStatus(Request: TStatementStatus): boolean; virtual; abstract;
    procedure GetData(index: integer; var IsNull: boolean; var len: short;
      var data: PChar); virtual;
    procedure RowChange;
    function StateChanged(var ChangeSeqNo: integer): boolean; virtual; abstract;
    property Count: integer read GetCount;
    property Column[index: integer]: TSQLVarData read GetColumn;
    property UniqueRelationName: string read FUniqueRelationName;
    property Statement: IStatement read GetStatement;
    property PrepareSeqNo: integer read GetPrepareSeqNo;
    property TransactionSeqNo: integer read GetTransactionSeqNo;
  end;

  { TSQLVarData }

  TSQLVarData = class
  private
    FParent: TSQLDataArea;
    FName: string;
    FIndex: integer;
    FModified: boolean;
    FUniqueName: boolean;
    FVarString: RawByteString;
    function GetStatement: IStatement;
    procedure SetName(AValue: string);
  protected
    function GetSQLType: cardinal; virtual; abstract;
    function GetSubtype: integer; virtual; abstract;
    function GetAliasName: string;  virtual; abstract;
    function GetFieldName: string; virtual; abstract;
    function GetOwnerName: string;  virtual; abstract;
    function GetRelationName: string;  virtual; abstract;
    function GetScale: integer; virtual; abstract;
    function GetCharSetID: cardinal; virtual; abstract;
    function GetCodePage: TSystemCodePage; virtual; abstract;
    function GetIsNull: Boolean;   virtual; abstract;
    function GetIsNullable: boolean; virtual; abstract;
    function GetSQLData: PChar;  virtual; abstract;
    function GetDataLength: cardinal; virtual; abstract;
    procedure SetIsNull(Value: Boolean); virtual; abstract;
    procedure SetIsNullable(Value: Boolean);  virtual; abstract;
    procedure SetSQLData(AValue: PChar; len: cardinal); virtual; abstract;
    procedure SetScale(aValue: integer); virtual; abstract;
    procedure SetDataLength(len: cardinal); virtual; abstract;
    procedure SetSQLType(aValue: cardinal); virtual; abstract;
    procedure SetCharSetID(aValue: cardinal); virtual; abstract;
  public
    constructor Create(aParent: TSQLDataArea; aIndex: integer);
    procedure SetString(aValue: string);
    procedure Changed; virtual;
    procedure RowChange; virtual;
    function GetAsArray(Array_ID: TISC_QUAD): IArray; virtual; abstract;
    function GetAsBlob(Blob_ID: TISC_QUAD; BPB: IBPB): IBlob; virtual; abstract;
    function CreateBlob: IBlob; virtual; abstract;
    function GetArrayMetaData: IArrayMetaData; virtual; abstract;
    function GetBlobMetaData: IBlobMetaData; virtual; abstract;
    procedure Initialize; virtual;

  public
    property AliasName: string read GetAliasName;
    property FieldName: string read GetFieldName;
    property OwnerName: string read GetOwnerName;
    property RelationName: string read GetRelationName;
    property Parent: TSQLDataArea read FParent;
    property Index: integer read FIndex;
    property Name: string read FName write SetName;
    property CharSetID: cardinal read GetCharSetID write SetCharSetID;
    property SQLType: cardinal read GetSQLType write SetSQLType;
    property SQLSubtype: integer read GetSubtype;
    property SQLData: PChar read GetSQLData;
    property DataLength: cardinal read GetDataLength write SetDataLength;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
    property Scale: integer read GetScale write SetScale;
  public
    property Modified: Boolean read FModified;
    property Statement: IStatement read GetStatement;
    property UniqueName: boolean read FUniqueName write FUniqueName;
  end;

  { TColumnMetaData }

  TColumnMetaData = class(TSQLDataItem,IColumnMetaData)
  private
    FIBXSQLVAR: TSQLVarData;
    FOwner: IUnknown;         {Keep reference to ensure Metadata/statement not discarded}
    FPrepareSeqNo: integer;
    FStatement: IStatement;
    FChangeSeqNo: integer;
  protected
    procedure CheckActive; override;
    function SQLData: PChar; override;
    function GetDataLength: cardinal; override;
    function GetCodePage: TSystemCodePage; override;

  public
    constructor Create(aOwner: IUnknown; aIBXSQLVAR: TSQLVarData);
    destructor Destroy; override;
    function GetSQLDialect: integer; override;
    property Statement: IStatement read FStatement;

  public
    {IColumnMetaData}
    function GetIndex: integer;
    function GetSQLType: cardinal; override;
    function getSubtype: integer;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if not alias}
    function GetName: string; override;      {Disambiguated uppercase Field Name}
    function GetScale: integer; override;
    function getCharSetID: cardinal; override;
    function GetIsNullable: boolean; override;
    function GetSize: cardinal;
    function GetArrayMetaData: IArrayMetaData;
    function GetBlobMetaData: IBlobMetaData;
    property Name: string read GetName;
    property Size: cardinal read GetSize;
    property CharSetID: cardinal read getCharSetID;
    property SQLSubtype: integer read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  end;

  { TIBSQLData }

  TIBSQLData = class(TColumnMetaData,ISQLData)
  protected
    procedure CheckActive; override;
  public
    function GetIsNull: Boolean; override;
    function GetAsArray: IArray;
    function GetAsBlob: IBlob; overload;
    function GetAsBlob(BPB: IBPB): IBlob; overload;
    function GetAsString: String; override;
    property AsBlob: IBlob read GetAsBlob;
 end;

  { TSQLParam }

  TSQLParam = class(TIBSQLData,ISQLParam)
  protected
    procedure CheckActive; override;
    procedure Changed; override;
    procedure InternalSetAsString(Value: String); override;
    procedure SetScale(aValue: integer); override;
    procedure SetDataLength(len: cardinal); override;
    procedure SetSQLType(aValue: cardinal); override;
  public
    procedure Clear;
    function GetModified: boolean; override;
    function GetAsPointer: Pointer;
    procedure SetName(Value: string); override;
    procedure SetIsNull(Value: Boolean);  override;
    procedure SetIsNullable(Value: Boolean); override;
    procedure SetAsArray(anArray: IArray);

    {overrides}
    procedure SetAsBoolean(AValue: boolean);
    procedure SetAsCurrency(AValue: Currency);
    procedure SetAsInt64(AValue: Int64);
    procedure SetAsDate(AValue: TDateTime);
    procedure SetAsLong(AValue: Long);
    procedure SetAsTime(AValue: TDateTime);
    procedure SetAsDateTime(AValue: TDateTime);
    procedure SetAsDouble(AValue: Double);
    procedure SetAsFloat(AValue: Float);
    procedure SetAsPointer(AValue: Pointer);
    procedure SetAsShort(AValue: Short);
    procedure SetAsString(AValue: String); override;
    procedure SetAsVariant(AValue: Variant);
    procedure SetAsBlob(aValue: IBlob);
    procedure SetAsQuad(AValue: TISC_QUAD);
    procedure SetCharSetID(aValue: cardinal);

    property AsBlob: IBlob read GetAsBlob write SetAsBlob;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
  end;

  { TMetaData }

  TMetaData = class(TInterfaceOwner,IMetaData)
  private
    FPrepareSeqNo: integer;
    FMetaData: TSQLDataArea;
    FStatement: IStatement; {ensure FStatement not destroyed until no longer needed}
    procedure CheckActive;
  public
    constructor Create(aMetaData: TSQLDataArea);
    destructor Destroy; override;
  public
    {IMetaData}
    function GetUniqueRelationName: string;
    function getCount: integer;
    function getColumnMetaData(index: integer): IColumnMetaData;
    function ByName(Idx: String): IColumnMetaData;
  end;

  { TSQLParams }

  TSQLParams = class(TInterfaceOwner,ISQLParams)
  private
    FPrepareSeqNo: integer;
    FChangeSeqNo: integer;
    FSQLParams: TSQLDataArea;
    FStatement: IStatement; {ensure FStatement not destroyed until no longer needed}
    procedure CheckActive;
  public
    constructor Create(aSQLParams: TSQLDataArea);
    destructor Destroy; override;
  public
    {ISQLParams}
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ByName(Idx: String): ISQLParam ;
    function GetModified: Boolean;
  end;

  { TResults }

   TResults = class(TInterfaceOwner,IResults)
   private
     FPrepareSeqNo: integer;
     FTransactionSeqNo: integer;
     FChangeSeqNo: integer;
     FResults: TSQLDataArea;
     FStatement: IStatement; {ensure FStatement not destroyed until no longer needed}
     function GetISQLData(aIBXSQLVAR: TSQLVarData): ISQLData;
   protected
     procedure CheckActive;
   public
     constructor Create(aResults: TSQLDataArea);
      {IResults}
     function getCount: integer;
     function ByName(Idx: String): ISQLData;
     function getSQLData(index: integer): ISQLData;
     procedure GetData(index: integer; var IsNull:boolean; var len: short; var data: PChar);
     function GetTransaction: ITransaction; virtual;
     procedure SetRetainInterfaces(aValue: boolean);
 end;

implementation

uses FBMessages, FBClientAPI, variants, IBUtils, FBTransaction;

{ TSQLDataArea }

function TSQLDataArea.GetColumn(index: integer): TSQLVarData;
begin
  if (index < 0) or (index >= Count) then
    IBError(ibxeInvalidColumnIndex,[nil]);
  Result := FColumnList[index];
end;

function TSQLDataArea.GetCount: integer;
begin
  Result := Length(FColumnList);
end;

procedure TSQLDataArea.SetUniqueRelationName;
var
  i: Integer;
  bUnique: Boolean;
  RelationName: string;
begin
  bUnique := True;
  for i := 0 to ColumnsInUseCount - 1 do
  begin
    RelationName := Column[i].RelationName;

    {First get the unique relation name, if any}

    if bUnique and (RelationName <> '') then
    begin
      if FUniqueRelationName = '' then
        FUniqueRelationName := RelationName
      else
      if RelationName <> FUniqueRelationName then
      begin
        FUniqueRelationName := '';
        bUnique := False;
      end;
    end;
  end;
end;

procedure TSQLDataArea.Initialize;
var
  i: Integer;
begin
  for i := 0 to ColumnsInUseCount - 1 do
    Column[i].Initialize;
end;

procedure TSQLDataArea.PreprocessSQL(sSQL: string; GenerateParamNames: boolean;
  var sProcessedSQL: string);
var
  cCurChar, cNextChar, cQuoteChar: Char;
  sParamName: String;
  j, i, iLenSQL, iSQLPos: Integer;
  iCurState {$ifdef ALLOWDIALECT3PARAMNAMES}, iCurParamState {$endif}: Integer;
  iParamSuffix: Integer;
  slNames: TStrings;
  StrBuffer: PChar;
  found: boolean;

const
  DefaultState = 0;
  CommentState = 1;
  QuoteState = 2;
  ParamState = 3;
  ArrayDimState = 4;
 {$ifdef ALLOWDIALECT3PARAMNAMES}
  ParamDefaultState = 0;
  ParamQuoteState = 1;
  {$endif}

  procedure AddToProcessedSQL(cChar: Char);
  begin
    StrBuffer[iSQLPos] := cChar;
    Inc(iSQLPos);
  end;

begin
  if not IsInputDataArea then
    IBError(ibxeNotPermitted,[nil]);

  sParamName := '';
  iLenSQL := Length(sSQL);
  GetMem(StrBuffer,iLenSQL + 1);
  slNames := TStringList.Create;
  try
    { Do some initializations of variables }
    iParamSuffix := 0;
    cQuoteChar := '''';
    i := 1;
    iSQLPos := 0;
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
        DefaultState:
        begin
          case cCurChar of
            '''', '"':
            begin
              cQuoteChar := cCurChar;
              iCurState := QuoteState;
            end;
            '?', ':':
            begin
              iCurState := ParamState;
              AddToProcessedSQL('?');
            end;
            '/': if (cNextChar = '*') then
            begin
              AddToProcessedSQL(cCurChar);
              Inc(i);
              iCurState := CommentState;
            end;
            '[':
            begin
              AddToProcessedSQL(cCurChar);
              Inc(i);
              iCurState := ArrayDimState;
            end;
          end;
        end;

        ArrayDimState:
        begin
          case cCurChar of
          ':',',','0'..'9',' ',#9,#10,#13:
            begin
              AddToProcessedSQL(cCurChar);
              Inc(i);
            end;
          else
            begin
              AddToProcessedSQL(cCurChar);
              Inc(i);
              iCurState := DefaultState;
            end;
          end;
        end;

        CommentState:
        begin
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
            else if GenerateParamNames then
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
      if (iCurState <> ParamState) and (i <= iLenSQL) then
        AddToProcessedSQL(sSQL[i]);
      Inc(i);
    end;
    AddToProcessedSQL(#0);
    sProcessedSQL := strpas(StrBuffer);
    SetCount(slNames.Count);
    for i := 0 to slNames.Count - 1 do
    begin
      Column[i].Name := slNames[i];
      Column[i].UniqueName :=  (slNames.Objects[i] <> nil);
    end;
    for i := 0 to Count - 1 do
    begin
      if not Column[i].UniqueName then
      begin
        found := false;
        for j := i + 1 to Count - 1 do
           if Column[i].Name = Column[j].Name then
           begin
             found := true;
             break;
           end;
        Column[i].UniqueName := not found;
      end;
    end;
  finally
    slNames.Free;
    FreeMem(StrBuffer);
  end;
end;

function TSQLDataArea.ColumnsInUseCount: integer;
begin
  Result := Count;
end;

function TSQLDataArea.ColumnByName(Idx: string): TSQLVarData;
var
  s: String;
  i: Integer;
begin
  {$ifdef UseCaseInSensitiveParamName}
   s := AnsiUpperCase(Idx);
  {$else}
   s := Idx;
  {$endif}
  for i := 0 to Count - 1 do
    if Column[i].Name = s then
    begin
         Result := Column[i];
         Exit;
    end;
  Result := nil;
end;

procedure TSQLDataArea.GetData(index: integer; var IsNull: boolean;
  var len: short; var data: PChar);
begin
  //Do Nothing
end;

procedure TSQLDataArea.RowChange;
var i: integer;
begin
  for i := 0 to Count - 1 do
    Column[i].RowChange;
end;

{TSQLVarData}

function TSQLVarData.GetStatement: IStatement;
begin
  Result := FParent.Statement;
end;

procedure TSQLVarData.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  {$ifdef UseCaseInSensitiveParamName}
  if Parent.IsInputDataArea then
    FName := AnsiUpperCase(AValue)
  else
  {$endif}
    FName := AValue;
end;

constructor TSQLVarData.Create(aParent: TSQLDataArea; aIndex: integer);
begin
  inherited Create;
  FParent := aParent;
  FIndex := aIndex;
  FUniqueName := true;
end;

procedure TSQLVarData.SetString(aValue: string);
begin
  {we take full advantage here of reference counted strings. When setting a string
   value, a reference is kept in FVarString and a pointer to it placed in the
   SQLVar. This avoids string copies. Note that PChar is guaranteed to point to
   a zero byte when the string is empty, neatly avoiding a nil pointer error.}

  FVarString := aValue;
  SQLType := SQL_TEXT;
  SetSQLData(PChar(FVarString),Length(aValue));
end;

procedure TSQLVarData.Changed;
begin
  FModified := true;
end;

procedure TSQLVarData.RowChange;
begin
  FModified := false;
  FVarString := '';
end;

procedure TSQLVarData.Initialize;

  function FindVarByName(idx: string; limit: integer): TSQLVarData;
  var
    k: integer;
  begin
      for k := 0 to limit do
          if Parent.Column[k].Name = idx then
          begin
               Result := Parent.Column[k];
               Exit;
          end;
      Result := nil;
  end;

var
  j, j_len: Integer;
  st: String;
  sBaseName: string;
begin
  RowChange;

  {If an output SQLDA then copy the aliasname to the FName. Ensure
    that they are all upper case only and disambiguated.
   }

   if not Parent.IsInputDataArea then
   begin
     st := Space2Underscore(AnsiUppercase(AliasName));
     if st = '' then
     begin
       sBaseName := 'F_'; {do not localize}
       j := 1; j_len := 1;
       st := sBaseName + IntToStr(j);
     end
     else
     begin
       j := 0; j_len := 0;
       sBaseName := st;
     end;

     {Look for other columns with the same name and make unique}

     while FindVarByName(st,Index-1) <> nil do
     begin
          Inc(j);
          j_len := Length(IntToStr(j));
          if j_len + Length(sBaseName) > 31 then
             st := system.Copy(sBaseName, 1, 31 - j_len) + IntToStr(j)
          else
             st := sBaseName + IntToStr(j);
     end;

     Name := st;
   end;
end;

{TSQLDataItem}

function TSQLDataItem.AdjustScale(Value: Int64; aScale: Integer): Double;
var
  Scaling : Int64;
  i: Integer;
  Val: Double;
begin
  Scaling := 1; Val := Value;
  if aScale > 0 then
  begin
    for i := 1 to aScale do
      Scaling := Scaling * 10;
    result := Val * Scaling;
  end
  else
    if aScale < 0 then
    begin
      for i := -1 downto aScale do
        Scaling := Scaling * 10;
      result := Val / Scaling;
    end
    else
      result := Val;
end;

function TSQLDataItem.AdjustScaleToInt64(Value: Int64; aScale: Integer): Int64;
var
  Scaling : Int64;
  i: Integer;
  Val: Int64;
begin
  Scaling := 1; Val := Value;
  if aScale > 0 then begin
    for i := 1 to aScale do Scaling := Scaling * 10;
    result := Val * Scaling;
  end else if aScale < 0 then begin
    for i := -1 downto aScale do Scaling := Scaling * 10;
    result := Val div Scaling;
  end else
    result := Val;
end;

function TSQLDataItem.AdjustScaleToCurrency(Value: Int64; aScale: Integer
  ): Currency;
var
  Scaling : Int64;
  i : Integer;
  FractionText, PadText, CurrText: string;
begin
  Result := 0;
  Scaling := 1;
  PadText := '';
  if aScale > 0 then
  begin
    for i := 1 to aScale do
      Scaling := Scaling * 10;
    result := Value * Scaling;
  end
  else
    if aScale < 0 then
    begin
      for i := -1 downto aScale do
        Scaling := Scaling * 10;
      FractionText := IntToStr(abs(Value mod Scaling));
      for i := Length(FractionText) to -aScale -1 do
        PadText := '0' + PadText;
      if Value < 0 then
        CurrText := '-' + IntToStr(Abs(Value div Scaling)) + DefaultFormatSettings.DecimalSeparator + PadText + FractionText
      else
        CurrText := IntToStr(Abs(Value div Scaling)) + DefaultFormatSettings.DecimalSeparator + PadText + FractionText;
      try
        result := StrToCurr(CurrText);
      except
        on E: Exception do
          IBError(ibxeInvalidDataConversion, [nil]);
      end;
    end
    else
      result := Value;
end;

procedure TSQLDataItem.SetAsInteger(AValue: Integer);
begin
  SetAsLong(aValue);
end;

function TSQLDataItem.AdjustScaleFromCurrency(Value: Currency; aScale: Integer
  ): Int64;
var
  Scaling : Int64;
  i : Integer;
begin
  Result := 0;
  Scaling := 1;
  if aScale < 0 then
  begin
    for i := -1 downto aScale do
      Scaling := Scaling * 10;
    result := trunc(Value * Scaling);
  end
  else
  if aScale > 0 then
  begin
    for i := 1 to aScale do
       Scaling := Scaling * 10;
    result := trunc(Value / Scaling);
  end
  else
    result := trunc(Value);
end;

function TSQLDataItem.AdjustScaleFromDouble(Value: Double; aScale: Integer
  ): Int64;
var
  Scaling : Int64;
  i : Integer;
begin
  Result := 0;
  Scaling := 1;
  if aScale < 0 then
  begin
    for i := -1 downto aScale do
      Scaling := Scaling * 10;
    result := trunc(Value * Scaling);
  end
  else
  if aScale > 0 then
  begin
    for i := 1 to aScale do
       Scaling := Scaling * 10;
    result := trunc(Value / Scaling);
  end
  else
    result := trunc(Value);
end;

procedure TSQLDataItem.CheckActive;
begin
  //Do nothing by default
end;

procedure TSQLDataItem.Changed;
begin
  //Do nothing by default
end;

procedure TSQLDataItem.Changing;
begin
  //Do nothing by default
end;

procedure TSQLDataItem.InternalSetAsString(Value: String);
begin
  //Do nothing by default
end;

function TSQLDataItem.Transliterate(s: string; CodePage: TSystemCodePage
  ): RawByteString;
begin
  Result := s;
  if StringCodePage(Result) <> CodePage then
    SetCodePage(Result,CodePage,CodePage <> CP_NONE);
end;

procedure TSQLDataItem.SetScale(aValue: integer);
begin
  //Do nothing by default
end;

procedure TSQLDataItem.SetDataLength(len: cardinal);
begin
  //Do nothing by default
end;

procedure TSQLDataItem.SetSQLType(aValue: cardinal);
begin
   //Do nothing by default
end;

function TSQLDataItem.GetSQLTypeName: string;
begin
  Result := GetSQLTypeName(GetSQLType);
end;

class function TSQLDataItem.GetSQLTypeName(SQLType: short): string;
begin
  Result := 'Unknown';
  case SQLType of
  SQL_VARYING:	        Result := 'SQL_VARYING';
  SQL_TEXT:		Result := 'SQL_TEXT';
  SQL_DOUBLE:		Result := 'SQL_DOUBLE';
  SQL_FLOAT:		Result := 'SQL_FLOAT';
  SQL_LONG:		Result := 'SQL_LONG';
  SQL_SHORT:		Result := 'SQL_SHORT';
  SQL_TIMESTAMP:	Result := 'SQL_TIMESTAMP';
  SQL_BLOB:		Result := 'SQL_BLOB';
  SQL_D_FLOAT:          Result := 'SQL_D_FLOAT';
  SQL_ARRAY:		Result := 'SQL_ARRAY';
  SQL_QUAD:		Result := 'SQL_QUAD';
  SQL_TYPE_TIME:	Result := 'SQL_TYPE_TIME';
  SQL_TYPE_DATE:	Result := 'SQL_TYPE_DATE';
  SQL_INT64:		Result := 'SQL_INT64';
  end;
end;

function TSQLDataItem.GetAsBoolean: boolean;
begin
  CheckActive;
  result := false;
  if not IsNull then
  begin
    if SQLType  = SQL_BOOLEAN then
      result := PByte(SQLData)^ = ISC_TRUE
    else
      IBError(ibxeInvalidDataConversion, [nil]);
  end
end;

function TSQLDataItem.GetAsCurrency: Currency;
begin
  CheckActive;
  result := 0;
  if GetSQLDialect < 3 then
    result := GetAsDouble
  else begin
    if not IsNull then
      case SQLType of
        SQL_TEXT, SQL_VARYING: begin
          try
            result := StrtoCurr(AsString);
          except
            on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
          end;
        end;
        SQL_SHORT:
          result := AdjustScaleToCurrency(Int64(PShort(SQLData)^),
                                      Scale);
        SQL_LONG:
          result := AdjustScaleToCurrency(Int64(PLong(SQLData)^),
                                      Scale);
        SQL_INT64:
          result := AdjustScaleToCurrency(PInt64(SQLData)^,
                                      Scale);
        SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
          result := Trunc(AsDouble);
        else
          IBError(ibxeInvalidDataConversion, [nil]);
      end;
    end;
end;

function TSQLDataItem.GetAsInt64: Int64;
begin
  CheckActive;
  result := 0;
  if not IsNull then
    case SQLType  of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToInt64(AsString);
        except
          on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := AdjustScaleToInt64(Int64(PShort(SQLData)^),
                                    Scale);
      SQL_LONG:
        result := AdjustScaleToInt64(Int64(PLong(SQLData)^),
                                    Scale);
      SQL_INT64:
        result := AdjustScaleToInt64(PInt64(SQLData)^,
                                    Scale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := Trunc(AsDouble);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetAsDateTime: TDateTime;
begin
  CheckActive;
  result := 0;
  if not IsNull then
    with FirebirdClientAPI do
    case SQLType of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToDate(AsString);
        except
          on E: EConvertError do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_TYPE_DATE:
        result := SQLDecodeDate(SQLData);
      SQL_TYPE_TIME:
        result := SQLDecodeTime(SQLData);
      SQL_TIMESTAMP:
        result := SQLDecodeDateTime(SQLData);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetAsDouble: Double;
begin
  CheckActive;
  result := 0;
  if not IsNull then begin
    case SQLType of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToFloat(AsString);
        except
          on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := AdjustScale(Int64(PShort(SQLData)^),
                              Scale);
      SQL_LONG:
        result := AdjustScale(Int64(PLong(SQLData)^),
                              Scale);
      SQL_INT64:
        result := AdjustScale(PInt64(SQLData)^, Scale);
      SQL_FLOAT:
        result := PFloat(SQLData)^;
      SQL_DOUBLE, SQL_D_FLOAT:
        result := PDouble(SQLData)^;
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
    if  Scale <> 0 then
      result :=
        StrToFloat(FloatToStrF(result, fffixed, 15,
                  Abs(Scale) ));
  end;
end;

function TSQLDataItem.GetAsFloat: Float;
begin
  CheckActive;
  result := 0;
  try
    result := AsDouble;
  except
    on E: EOverflow do
      IBError(ibxeInvalidDataConversion, [nil]);
  end;
end;

function TSQLDataItem.GetAsLong: Long;
begin
  CheckActive;
  result := 0;
  if not IsNull then
    case SQLType of
      SQL_TEXT, SQL_VARYING: begin
        try
          result := StrToInt(AsString);
        except
          on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
        end;
      end;
      SQL_SHORT:
        result := Trunc(AdjustScale(Int64(PShort(SQLData)^),
                                    Scale));
      SQL_LONG:
        result := Trunc(AdjustScale(Int64(PLong(SQLData)^),
                                    Scale));
      SQL_INT64:
        result := Trunc(AdjustScale(PInt64(SQLData)^, Scale));
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := Trunc(AsDouble);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetAsPointer: Pointer;
begin
  CheckActive;
  if not IsNull then
    result := SQLData
  else
    result := nil;
end;

function TSQLDataItem.GetAsQuad: TISC_QUAD;
begin
  CheckActive;
  result.gds_quad_high := 0;
  result.gds_quad_low := 0;
  if not IsNull then
    case SQLType of
      SQL_BLOB, SQL_ARRAY, SQL_QUAD:
        result := PISC_QUAD(SQLData)^;
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetAsShort: short;
begin
  CheckActive;
  result := 0;
  try
    result := AsLong;
  except
    on E: Exception do IBError(ibxeInvalidDataConversion, [nil]);
  end;
end;


function TSQLDataItem.GetAsString: String;
var
  sz: PChar;
  str_len: Integer;
  rs: RawByteString;
begin
  CheckActive;
  result := '';
  { Check null, if so return a default string }
  if not IsNull then
  with FirebirdClientAPI do
    case SQLType of
      SQL_BOOLEAN:
        if AsBoolean then
          Result := sTrue
        else
          Result := SFalse;

      SQL_TEXT, SQL_VARYING:
      begin
        sz := SQLData;
        if (SQLType = SQL_TEXT) then
          str_len := DataLength
        else begin
          str_len := DecodeInteger(SQLData, 2);
          Inc(sz, 2);
        end;
        SetString(rs, sz, str_len);
        SetCodePage(rs,GetCodePage,false);
        if (SQLType = SQL_TEXT) and (GetCharSetID <> 1) then
          Result := TrimRight(rs)
        else
          Result := rs
      end;
      SQL_TYPE_DATE:
        case GetSQLDialect of
          1 : result := DateTimeToStr(AsDateTime);
          3 : result := DateToStr(AsDateTime);
        end;
      SQL_TYPE_TIME :
        result := TimeToStr(AsDateTime);
      SQL_TIMESTAMP:
        result := FormatDateTime(FormatSettings.ShortDateFormat + ' ' +
                            FormatSettings.LongTimeFormat+'.zzz',AsDateTime);
      SQL_SHORT, SQL_LONG:
        if Scale = 0 then
          result := IntToStr(AsLong)
        else if Scale >= (-4) then
          result := CurrToStr(AsCurrency)
        else
          result := FloatToStr(AsDouble);
      SQL_INT64:
        if Scale = 0 then
          result := IntToStr(AsInt64)
        else if Scale >= (-4) then
          result := CurrToStr(AsCurrency)
        else
          result := FloatToStr(AsDouble);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := FloatToStr(AsDouble);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetIsNull: Boolean;
begin
  CheckActive;
  Result := false;
end;

function TSQLDataItem.getIsNullable: boolean;
begin
  CheckActive;
  Result := false;
end;

function TSQLDataItem.GetAsVariant: Variant;
begin
  CheckActive;
  if IsNull then
    result := NULL
  { Check null, if so return a default string }
  else case SQLType of
      SQL_ARRAY:
        result := '(Array)'; {do not localize}
      SQL_BLOB,
      SQL_TEXT, SQL_VARYING:
        result := AsString;
      SQL_TIMESTAMP, SQL_TYPE_DATE, SQL_TYPE_TIME:
        result := AsDateTime;
      SQL_SHORT, SQL_LONG:
        if Scale = 0 then
          result := AsLong
        else if Scale >= (-4) then
          result := AsCurrency
        else
          result := AsDouble;
      SQL_INT64:
        if Scale = 0 then
          result := AsInt64
        else if Scale >= (-4) then
          result := AsCurrency
        else
          result := AsDouble;
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := AsDouble;
      SQL_BOOLEAN:
        result := AsBoolean;
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetModified: boolean;
begin
  Result := false;
end;


procedure TSQLDataItem.SetIsNull(Value: Boolean);
begin
  //ignore unless overridden
end;

procedure TSQLDataItem.SetIsNullable(Value: Boolean);
begin
  //ignore unless overridden
end;

procedure TSQLDataItem.SetName(aValue: string);
begin
  //ignore unless overridden
end;

procedure TSQLDataItem.SetAsCurrency(Value: Currency);
begin
  CheckActive;
  if GetSQLDialect < 3 then
    AsDouble := Value
  else
  begin
    Changing;
    if IsNullable then
      IsNull := False;
    SQLType := SQL_INT64;
    Scale := -4;
    DataLength := SizeOf(Int64);
    PCurrency(SQLData)^ := Value;
    Changed;
  end;
end;

procedure TSQLDataItem.SetAsInt64(Value: Int64);
begin
  CheckActive;
  Changing;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_INT64;
  Scale := 0;
  DataLength := SizeOf(Int64);
  PInt64(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsDate(Value: TDateTime);
begin
  CheckActive;
  if GetSQLDialect < 3 then
  begin
    AsDateTime := Value;
    exit;
  end;

  Changing;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_TYPE_DATE;
  DataLength := SizeOf(ISC_DATE);
  with FirebirdClientAPI do
    SQLEncodeDate(Value,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsTime(Value: TDateTime);
begin
  CheckActive;
  if GetSQLDialect < 3 then
  begin
    AsDateTime := Value;
    exit;
  end;

  Changing;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_TYPE_TIME;
  DataLength := SizeOf(ISC_TIME);
  with FirebirdClientAPI do
    SQLEncodeTime(Value,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsDateTime(Value: TDateTime);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  Changing;
  SQLType := SQL_TIMESTAMP;
  DataLength := SizeOf(ISC_TIME) + sizeof(ISC_DATE);
  with FirebirdClientAPI do
    SQLEncodeDateTime(Value,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsDouble(Value: Double);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  Changing;
  SQLType := SQL_DOUBLE;
  DataLength := SizeOf(Double);
  Scale := 0;
  PDouble(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsFloat(Value: Float);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  Changing;
  SQLType := SQL_FLOAT;
  DataLength := SizeOf(Float);
  Scale := 0;
  PSingle(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsLong(Value: Long);
begin
  CheckActive;
  if IsNullable then
    IsNull := False;

  Changing;
  SQLType := SQL_LONG;
  DataLength := SizeOf(Long);
  Scale := 0;
  PLong(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsPointer(Value: Pointer);
begin
  CheckActive;
  Changing;
  if IsNullable and (Value = nil) then
    IsNull := True
  else
  begin
    IsNull := False;
    SQLType := SQL_TEXT;
    Move(Value^, SQLData^, DataLength);
  end;
  Changed;
end;

procedure TSQLDataItem.SetAsQuad(Value: TISC_QUAD);
begin
  CheckActive;
  Changing;
  if IsNullable then
      IsNull := False;
  if (SQLType <> SQL_BLOB) and
     (SQLType <> SQL_ARRAY) then
    IBError(ibxeInvalidDataConversion, [nil]);
  DataLength := SizeOf(TISC_QUAD);
  PISC_QUAD(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsShort(Value: short);
begin
  CheckActive;
  Changing;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_SHORT;
  DataLength := SizeOf(Short);
  Scale := 0;
  PShort(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsString(Value: String);
begin
  InternalSetAsString(Value);
end;

procedure TSQLDataItem.SetAsVariant(Value: Variant);
begin
  CheckActive;
  if VarIsNull(Value) then
    IsNull := True
  else case VarType(Value) of
    varEmpty, varNull:
      IsNull := True;
    varSmallint, varInteger, varByte,
      varWord, varShortInt:
      AsLong := Value;
    varInt64:
      AsInt64 := Value;
    varSingle, varDouble:
      AsDouble := Value;
    varCurrency:
      AsCurrency := Value;
    varBoolean:
      AsBoolean := Value;
    varDate:
      AsDateTime := Value;
    varOleStr, varString:
      AsString := Value;
    varArray:
      IBError(ibxeNotSupported, [nil]);
    varByRef, varDispatch, varError, varUnknown, varVariant:
      IBError(ibxeNotPermitted, [nil]);
  end;
end;

procedure TSQLDataItem.SetAsBoolean(AValue: boolean);
begin
  CheckActive;
  Changing;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_BOOLEAN;
  DataLength := 1;
  Scale := 0;
  if AValue then
    PByte(SQLData)^ := ISC_TRUE
  else
    PByte(SQLData)^ := ISC_FALSE;
  Changed;
end;

{TColumnMetaData}

procedure TColumnMetaData.CheckActive;
begin
  if not FIBXSQLVAR.Parent.StateChanged(FChangeSeqNo) then Exit;

  if FPrepareSeqNo < FIBXSQLVAR.Parent.GetPrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FIBXSQLVAR.Parent.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

function TColumnMetaData.SQLData: PChar;
begin
  Result := FIBXSQLVAR.SQLData;
end;

function TColumnMetaData.GetDataLength: cardinal;
begin
  Result := FIBXSQLVAR.DataLength;
end;

function TColumnMetaData.GetCodePage: TSystemCodePage;
begin
   Result := FIBXSQLVAR.GetCodePage;
end;

constructor TColumnMetaData.Create(aOwner: IUnknown; aIBXSQLVAR: TSQLVarData);
begin
  inherited Create;
  FIBXSQLVAR := aIBXSQLVAR;
  FOwner := aOwner;
  FPrepareSeqNo := FIBXSQLVAR.Parent.PrepareSeqNo;
  FIBXSQLVAR.Parent.StateChanged(FChangeSeqNo)
end;

destructor TColumnMetaData.Destroy;
begin
  (FOwner as TInterfaceOwner).Remove(self);
  inherited Destroy;
end;


function TColumnMetaData.GetSQLDialect: integer;
begin
  Result := FIBXSQLVAR.Statement.GetSQLDialect;
end;

function TColumnMetaData.GetIndex: integer;
begin
  Result := FIBXSQLVAR.Index;
end;

function TColumnMetaData.GetSQLType: cardinal;
begin
  CheckActive;
  result := FIBXSQLVAR.SQLType;
end;

function TColumnMetaData.getSubtype: integer;
begin
  CheckActive;
  result := FIBXSQLVAR.SQLSubtype;
end;

function TColumnMetaData.getRelationName: string;
begin
  CheckActive;
   result :=  FIBXSQLVAR.RelationName;
end;

function TColumnMetaData.getOwnerName: string;
begin
  CheckActive;
  result :=  FIBXSQLVAR.OwnerName;
end;

function TColumnMetaData.getSQLName: string;
begin
  CheckActive;
  result :=  FIBXSQLVAR.FieldName;
end;

function TColumnMetaData.getAliasName: string;
begin
  CheckActive;
  result := FIBXSQLVAR.AliasName;
end;

function TColumnMetaData.GetName: string;
begin
  CheckActive;
  Result := FIBXSQLVAR. Name;
end;

function TColumnMetaData.GetScale: integer;
begin
  CheckActive;
  result := FIBXSQLVAR.Scale;
end;

function TColumnMetaData.getCharSetID: cardinal;
begin
  CheckActive;
  Result := FIBXSQLVAR.CharSetID;
end;

function TColumnMetaData.GetIsNullable: boolean;
begin
  CheckActive;
  result := FIBXSQLVAR.IsNullable;
end;

function TColumnMetaData.GetSize: cardinal;
begin
  CheckActive;
  result := FIBXSQLVAR.DataLength;
end;

function TColumnMetaData.GetArrayMetaData: IArrayMetaData;
begin
  CheckActive;
  result := FIBXSQLVAR.GetArrayMetaData;
end;

function TColumnMetaData.GetBlobMetaData: IBlobMetaData;
begin
  CheckActive;
  result := FIBXSQLVAR.GetBlobMetaData;
end;

{ TIBSQLData }

procedure TIBSQLData.CheckActive;
begin
  if not FIBXSQLVAR.Parent.StateChanged(FChangeSeqNo) then Exit;

  inherited CheckActive;

  if not FIBXSQLVAR.Parent.CheckStatementStatus(ssCursorOpen) and
                 not FIBXSQLVAR.Parent.CheckStatementStatus(ssExecuteResults) then
    IBError(ibxeSQLClosed, [nil]);

  if FIBXSQLVAR.Parent.CheckStatementStatus(ssEOF) then
    IBError(ibxeEOF,[nil]);

  if FIBXSQLVAR.Parent.CheckStatementStatus(ssBOF) then
    IBError(ibxeBOF,[nil]);
end;

function TIBSQLData.GetIsNull: Boolean;
begin
  CheckActive;
  result := FIBXSQLVAR.IsNull;
end;

function TIBSQLData.GetAsArray: IArray;
begin
  CheckActive;
  result := FIBXSQLVAR.GetAsArray(AsQuad);
end;

function TIBSQLData.GetAsBlob: IBlob;
begin
  CheckActive;
  result := FIBXSQLVAR.GetAsBlob(AsQuad,nil);
end;

function TIBSQLData.GetAsBlob(BPB: IBPB): IBlob;
begin
  CheckActive;
  result := FIBXSQLVAR.GetAsBlob(AsQuad,BPB);
end;

function TIBSQLData.GetAsString: String;
begin
  CheckActive;
  Result := '';
  { Check null, if so return a default string }
  if not IsNull then
  case SQLType of
    SQL_ARRAY:
      result := SArray;
    SQL_BLOB:
      Result := FIBXSQLVAR.GetAsBlob(AsQuad,nil).GetAsString;
    else
      Result := inherited GetAsString;
  end;
end;

{ TSQLParam }

procedure TSQLParam.InternalSetAsString(Value: String);
var b: IBlob;
begin
  CheckActive;
  if IsNullable then
    IsNull := False;
  case SQLTYPE of
  SQL_BOOLEAN:
    if CompareText(Value,STrue) = 0 then
      AsBoolean := true
    else
    if CompareText(Value,SFalse) = 0 then
      AsBoolean := false
    else
      IBError(ibxeInvalidDataConversion,[nil]);

  SQL_BLOB:
    begin
      Changing;
      b := FIBXSQLVAR.CreateBlob;
      b.SetAsString(Value);
      AsBlob := b;
      Changed;
    end;

  SQL_VARYING,
  SQL_TEXT:
    begin
      Changing;
      FIBXSQLVar.SetString(Transliterate(Value,GetCodePage));
      Changed;
    end;

    SQL_SHORT,
    SQL_LONG,
    SQL_INT64:
      SetAsInt64(StrToInt(Value));

    SQL_D_FLOAT,
    SQL_DOUBLE,
    SQL_FLOAT:
      SetAsDouble(StrToFloat(Value));

    SQL_TIMESTAMP:
      SetAsDateTime(StrToDateTime(Value));

    SQL_TYPE_DATE:
      SetAsDate(StrToDateTime(Value));

    SQL_TYPE_TIME:
      SetAsTime(StrToDateTime(Value));

    else
      IBError(ibxeInvalidDataConversion,[nil]);
  end;
end;

procedure TSQLParam.CheckActive;
begin
  if not FIBXSQLVAR.Parent.StateChanged(FChangeSeqNo) then Exit;

  if FPrepareSeqNo < FIBXSQLVAR.Parent.GetPrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FIBXSQLVAR.Parent.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

procedure TSQLParam.SetScale(aValue: integer);
begin
  CheckActive;
  FIBXSQLVAR.Scale := aValue;
end;

procedure TSQLParam.SetDataLength(len: cardinal);
begin
  CheckActive;
  FIBXSQLVAR.DataLength := len;
end;

procedure TSQLParam.SetSQLType(aValue: cardinal);
begin
  CheckActive;
  FIBXSQLVAR.SQLType := aValue;
end;

procedure TSQLParam.Clear;
begin
  IsNull := true;
end;

function TSQLParam.GetModified: boolean;
begin
  CheckActive;
  Result := FIBXSQLVAR.Modified;
end;

function TSQLParam.GetAsPointer: Pointer;
begin
  IsNull := false; {Assume that we get the pointer in order to set a value}
  Changed;
  Result := inherited GetAsPointer;
end;

procedure TSQLParam.SetName(Value: string);
begin
  CheckActive;
  FIBXSQLVAR.Name := Value;
end;

procedure TSQLParam.SetIsNull(Value: Boolean);
var i: integer;
begin
  CheckActive;
  if FIBXSQLVAR.UniqueName then
    FIBXSQLVAR.IsNull := Value
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
        Column[i].IsNull := Value;
  end
end;

procedure TSQLParam.SetIsNullable(Value: Boolean);
var i: integer;
begin
  CheckActive;
  if FIBXSQLVAR.UniqueName then
    FIBXSQLVAR.IsNullable := Value
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
        Column[i].IsNullable := Value;
  end
end;

procedure TSQLParam.SetAsArray(anArray: IArray);
begin
  CheckActive;
  if GetSQLType <> SQL_ARRAY then
    IBError(ibxeInvalidDataConversion,[nil]);

  if not FIBXSQLVAR.UniqueName then
    IBError(ibxeDuplicateParamName,[FIBXSQLVAR.Name]);

  SetAsQuad(AnArray.GetArrayID);
end;

procedure TSQLParam.Changed;
begin
  FIBXSQLVAR.Changed;
end;

procedure TSQLParam.SetAsBoolean(AValue: boolean);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsBoolean(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsBoolean(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsCurrency(AValue: Currency);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsCurrency(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsCurrency(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsInt64(AValue: Int64);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsInt64(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsInt64(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsDate(AValue: TDateTime);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsDate(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsDate(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsLong(AValue: Long);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsLong(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsLong(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsTime(AValue: TDateTime);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsTime(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsTime(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsDateTime(AValue: TDateTime);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsDateTime(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsDateTime(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsDouble(AValue: Double);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsDouble(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsDouble(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsFloat(AValue: Float);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsFloat(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsFloat(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsPointer(AValue: Pointer);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsPointer(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsPointer(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsShort(AValue: Short);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsShort(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsShort(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsString(AValue: String);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    InternalSetAsString(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          InternalSetAsString(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsVariant(AValue: Variant);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsVariant(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsVariant(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsBlob(aValue: IBlob);
begin
  with FIBXSQLVAR do
  if not UniqueName then
    IBError(ibxeDuplicateParamName,[Name]);
  CheckActive;
  Changing;
  aValue.Close;
  if aValue.GetSubType <> GetSubType then
    IBError(ibxeIncompatibleBlob,[GetSubType,aValue.GetSubType]);
  AsQuad := aValue.GetBlobID;
  Changed;
end;

procedure TSQLParam.SetAsQuad(AValue: TISC_QUAD);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsQuad(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsQuad(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetCharSetID(aValue: cardinal);
begin
  FIBXSQLVAR.SetCharSetID(aValue);
end;

{ TMetaData }

procedure TMetaData.CheckActive;
begin
  if FPrepareSeqNo < FMetaData.PrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FMetaData.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

constructor TMetaData.Create(aMetaData: TSQLDataArea);
begin
  inherited Create(aMetaData.Count);
  FMetaData := aMetaData;
  FStatement := aMetaData.Statement;
  FPrepareSeqNo := aMetaData.PrepareSeqNo;
end;

destructor TMetaData.Destroy;
begin
  (FStatement as TInterfaceOwner).Remove(self);
  inherited Destroy;
end;

function TMetaData.GetUniqueRelationName: string;
begin
  CheckActive;
  Result := FMetaData.UniqueRelationName;
end;

function TMetaData.getCount: integer;
begin
  CheckActive;
  Result := FMetaData.ColumnsInUseCount;
end;

function TMetaData.getColumnMetaData(index: integer): IColumnMetaData;
begin
  CheckActive;
  if (index < 0) or (index >= getCount) then
    IBError(ibxeInvalidColumnIndex,[nil]);

  if FMetaData.Count = 0 then
    Result := nil
  else
  begin
    if not HasInterface(index) then
      AddInterface(index,TColumnMetaData.Create(self,FMetaData.Column[index]));
    Result := TColumnMetaData(GetInterface(index));
  end;
end;

function TMetaData.ByName(Idx: String): IColumnMetaData;
var aIBXSQLVAR: TSQLVarData;
begin
  CheckActive;
  aIBXSQLVAR := FMetaData.ColumnByName(Idx);
  if aIBXSQLVAR = nil then
    IBError(ibxeFieldNotFound,[Idx]);
  Result := getColumnMetaData(aIBXSQLVAR.index);
end;

{ TSQLParams }

procedure TSQLParams.CheckActive;
begin
  if not FSQLParams.StateChanged(FChangeSeqNo) then Exit;

  if FPrepareSeqNo < FSQLParams.PrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FSQLParams.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);
end;

constructor TSQLParams.Create(aSQLParams: TSQLDataArea);
begin
  inherited Create(aSQLParams.Count);
  FSQLParams := aSQLParams;
  FStatement := aSQLParams.Statement;
  FPrepareSeqNo := aSQLParams.PrepareSeqNo;
  FSQLParams.StateChanged(FChangeSeqNo);
end;

destructor TSQLParams.Destroy;
begin
  (FStatement as TInterfaceOwner).Remove(self);
  inherited Destroy;
end;

function TSQLParams.getCount: integer;
begin
  CheckActive;
  Result := FSQLParams.ColumnsInUseCount;
end;

function TSQLParams.getSQLParam(index: integer): ISQLParam;
begin
  CheckActive;
  if (index < 0) or (index >= getCount) then
    IBError(ibxeInvalidColumnIndex,[nil]);

  if getCount = 0 then
    Result := nil
  else
  begin
    if not HasInterface(index) then
      AddInterface(index, TSQLParam.Create(self,FSQLParams.Column[index]));
    Result := TSQLParam(GetInterface(index));
  end;
end;

function TSQLParams.ByName(Idx: String): ISQLParam;
var aIBXSQLVAR: TSQLVarData;
begin
  CheckActive;
  aIBXSQLVAR := FSQLParams.ColumnByName(Idx);
  if aIBXSQLVAR = nil then
    IBError(ibxeFieldNotFound,[Idx]);
  Result := getSQLParam(aIBXSQLVAR.index);
end;

function TSQLParams.GetModified: Boolean;
var
  i: Integer;
begin
  CheckActive;
  result := False;
  with FSQLParams do
  for i := 0 to Count - 1 do
    if Column[i].Modified then
    begin
      result := True;
      exit;
    end;
end;

{ TResults }

procedure TResults.CheckActive;
begin
  if not FResults.StateChanged(FChangeSeqNo) then Exit;

  if FPrepareSeqNo < FResults.PrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FResults.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);

  with GetTransaction as TFBTransaction do
  if not InTransaction or (FResults.TransactionSeqNo <> FTransactionSeqNo) then
    IBError(ibxeInterfaceOutofDate,[nil]);
end;

function TResults.GetISQLData(aIBXSQLVAR: TSQLVarData): ISQLData;
begin
  if (aIBXSQLVAR.Index < 0) or (aIBXSQLVAR.Index >= getCount) then
    IBError(ibxeInvalidColumnIndex,[nil]);

  if not HasInterface(aIBXSQLVAR.Index) then
    AddInterface(aIBXSQLVAR.Index, TIBSQLData.Create(self,aIBXSQLVAR));
  Result := TIBSQLData(GetInterface(aIBXSQLVAR.Index));
end;

constructor TResults.Create(aResults: TSQLDataArea);
begin
  inherited Create(aResults.Count);
  FResults := aResults;
  FStatement := aResults.Statement;
  FPrepareSeqNo := aResults.PrepareSeqNo;
  FTransactionSeqNo := aResults.TransactionSeqNo;
  FResults.StateChanged(FChangeSeqNo);
end;

function TResults.getCount: integer;
begin
  CheckActive;
  Result := FResults.Count;
end;

function TResults.ByName(Idx: String): ISQLData;
var col: TSQLVarData;
begin
  Result := nil;
  CheckActive;
  if FResults.CheckStatementStatus(ssBOF) then
    IBError(ibxeBOF,[nil]);
  if FResults.CheckStatementStatus(ssEOF) then
    IBError(ibxeEOF,[nil]);

  if FResults.Count > 0 then
  begin
    col := FResults.ColumnByName(Idx);
    if col <> nil then
      Result := GetISQLData(col);
  end;
end;

function TResults.getSQLData(index: integer): ISQLData;
begin
  CheckActive;
  if FResults.CheckStatementStatus(ssBOF) then
    IBError(ibxeBOF,[nil]);
  if FResults.CheckStatementStatus(ssEOF) then
    IBError(ibxeEOF,[nil]);
  if (index < 0) or (index >= FResults.Count) then
    IBError(ibxeInvalidColumnIndex,[nil]);

  Result := GetISQLData(FResults.Column[index]);
end;

procedure TResults.GetData(index: integer; var IsNull: boolean; var len: short;
  var data: PChar);
begin
  CheckActive;
  FResults.GetData(index,IsNull, len,data);
end;

function TResults.GetTransaction: ITransaction;
begin
  Result := FStatement.GetTransaction;
end;

procedure TResults.SetRetainInterfaces(aValue: boolean);
begin
  RetainInterfaces := aValue;
end;


end.

