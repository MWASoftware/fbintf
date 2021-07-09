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
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

{ This Unit was hacked out of the IBSQL unit and defines a class used as the
  base for interfaces accessing SQLDAVar data and Array Elements. The abstract
  methods are used to customise for an SQLDAVar or Array Element. The empty
  methods are needed for SQL parameters only. The string getters and setters
  are virtual as SQLVar and Array encodings of string data is different.}


interface

uses
  Classes, SysUtils, IBExternals, {$IFDEF WINDOWS} Windows, {$ENDIF} IB,  FBActivityMonitor, FBClientAPI,
  FmtBCD;

const
  MaxInlineBlobString = 8192;

type

   {The IExTimeZoneServices is only available in FB4 and onwards}

   IExTimeZoneServices = interface(ITimeZoneServices)
   ['{789c2eeb-c4a7-4fed-837e-0cbdef775904}']
   {encode/decode - used to encode/decode the wire protocol}
   procedure EncodeTimestampTZ(timestamp: TDateTime; timezoneID: TFBTimeZoneID;
     bufptr: PByte); overload;
   procedure EncodeTimestampTZ(timestamp: TDateTime; timezone: AnsiString;
       bufptr: PByte); overload;
   procedure EncodeTimeTZ(time: TDateTime; timezoneID: TFBTimeZoneID; OnDate: TDateTime;
     bufptr: PByte); overload;
   procedure EncodeTimeTZ(time: TDateTime; timezone: AnsiString; OnDate: TDateTime;
     bufptr: PByte); overload;
   procedure DecodeTimestampTZ(bufptr: PByte; var timestamp: TDateTime;
     var dstOffset: smallint; var timezoneID: TFBTimeZoneID); overload;
   procedure DecodeTimestampTZ(bufptr: PByte; var timestamp: TDateTime;
     var dstOffset: smallint; var timezone: AnsiString); overload;
   procedure DecodeTimestampTZEx(bufptr: PByte; var timestamp: TDateTime;
     var dstOffset: smallint; var timezoneID: TFBTimeZoneID); overload;
   procedure DecodeTimestampTZEx(bufptr: PByte; var timestamp: TDateTime;
     var dstOffset: smallint; var timezone: AnsiString); overload;
   procedure DecodeTimeTZ(bufptr: PByte; OnDate: TDateTime; var time: TDateTime;
     var dstOffset: smallint; var timezoneID: TFBTimeZoneID); overload;
   procedure DecodeTimeTZ(bufptr: PByte; OnDate: TDateTime; var time: TDateTime;
     var dstOffset: smallint; var timezone: AnsiString); overload;
   procedure DecodeTimeTZEx(bufptr: PByte; OnDate: TDateTime; var time: TDateTime;
     var dstOffset: smallint; var timezoneID: TFBTimeZoneID); overload;
   procedure DecodeTimeTZEx(bufptr: PByte; OnDate: TDateTime; var time: TDateTime;
     var dstOffset: smallint; var timezone: AnsiString); overload;
   end;

   { TSQLDataItem }

  TSQLDataItem = class(TFBInterfacedObject)
  private
     FFirebirdClientAPI: TFBClientAPI;
     FTimeZoneServices: IExTimeZoneServices;
     function AdjustScale(Value: Int64; aScale: Integer): Double;
     function AdjustScaleToInt64(Value: Int64; aScale: Integer): Int64;
     function AdjustScaleToCurrency(Value: Int64; aScale: Integer): Currency;
     function GetDateFormatStr(IncludeTime: boolean): AnsiString;
     function GetTimeFormatStr: AnsiString;
     function GetTimestampFormatStr: AnsiString;
     procedure SetAsInteger(AValue: Integer);
     procedure InternalGetAsDateTime(var aDateTime: TDateTime; var dstOffset: smallint;
       var aTimezone: AnsiString; var aTimeZoneID: TFBTimeZoneID);
  protected
     function AdjustScaleFromCurrency(Value: Currency; aScale: Integer): Int64;
     function AdjustScaleFromDouble(Value: Double; aScale: Integer): Int64;
     procedure CheckActive; virtual;
     procedure CheckTZSupport;
     function GetAttachment: IAttachment; virtual; abstract;
     function GetSQLDialect: integer; virtual; abstract;
     function GetTimeZoneServices: IExTimeZoneServices; virtual;
     procedure Changed; virtual;
     procedure Changing; virtual;
     procedure InternalSetAsString(Value: AnsiString); virtual;
     function SQLData: PByte; virtual; abstract;
     function GetDataLength: cardinal; virtual; abstract;
     function GetCodePage: TSystemCodePage; virtual; abstract;
     function getCharSetID: cardinal; virtual; abstract;
     function Transliterate(s: AnsiString; CodePage: TSystemCodePage): RawByteString;
     procedure SetScale(aValue: integer); virtual;
     procedure SetDataLength(len: cardinal); virtual;
     procedure SetSQLType(aValue: cardinal); virtual;
     property DataLength: cardinal read GetDataLength write SetDataLength;
     property FirebirdClientAPI: TFBClientAPI read FFirebirdClientAPI;
  public
     constructor Create(api: TFBClientAPI);
     function GetSQLType: cardinal; virtual; abstract;
     function GetSQLTypeName: AnsiString; overload;
     class function GetSQLTypeName(SQLType: short): AnsiString; overload;
     function GetStrDataLength: short;
     function GetName: AnsiString; virtual; abstract;
     function GetScale: integer; virtual; abstract;
     function GetAsBoolean: boolean;
     function GetAsCurrency: Currency;
     function GetAsInt64: Int64;
     function GetAsDateTime: TDateTime; overload;
     procedure GetAsDateTime(var aDateTime: TDateTime; var dstOffset: smallint; var aTimezone: AnsiString); overload;
     procedure GetAsDateTime(var aDateTime: TDateTime; var dstOffset: smallint; var aTimezoneID: TFBTimeZoneID); overload;
     procedure GetAsTime(var aTime: TDateTime; var dstOffset: smallint; var aTimezoneID: TFBTimeZoneID; OnDate: TDateTime); overload;
     procedure GetAsTime(var aTime: TDateTime; var dstOffset: smallint; var aTimezone: AnsiString; OnDate: TDateTime); overload;
     procedure GetAsTime(var aTime: TDateTime; var dstOffset: smallint; var aTimezoneID: TFBTimeZoneID); overload;
     procedure GetAsTime(var aTime: TDateTime; var dstOffset: smallint; var aTimezone: AnsiString); overload;
     function GetAsUTCDateTime: TDateTime;
     function GetAsDouble: Double;
     function GetAsFloat: Float;
     function GetAsLong: Long;
     function GetAsPointer: Pointer;
     function GetAsQuad: TISC_QUAD;
     function GetAsShort: short;
     function GetAsString: AnsiString; virtual;
     function GetIsNull: Boolean; virtual;
     function GetIsNullable: boolean; virtual;
     function GetAsVariant: Variant;
     function GetModified: boolean; virtual;
     function GetDateTimeStrLength(DateTimeFormat: TIBDateTimeFormats): integer;
     function GetAsBCD: tBCD;
     function GetSize: cardinal; virtual; abstract;
     function GetCharSetWidth: integer; virtual; abstract;
     procedure SetAsBoolean(AValue: boolean); virtual;
     procedure SetAsCurrency(Value: Currency); virtual;
     procedure SetAsInt64(Value: Int64); virtual;
     procedure SetAsDate(Value: TDateTime); virtual;
     procedure SetAsLong(Value: Long); virtual;
     procedure SetAsTime(Value: TDateTime); overload;
     procedure SetAsTime(aValue: TDateTime; OnDate: TDateTime;aTimeZoneID: TFBTimeZoneID); overload;
     procedure SetAsTime(aValue: TDateTime; OnDate: TDateTime; aTimeZone: AnsiString); overload;
     procedure SetAsDateTime(Value: TDateTime); overload;
     procedure SetAsDateTime(aValue: TDateTime; aTimeZoneID: TFBTimeZoneID); overload;
     procedure SetAsDateTime(aValue: TDateTime; aTimeZone: AnsiString); overload;
     procedure SetAsUTCDateTime(aUTCTime: TDateTime);
     procedure SetAsDouble(Value: Double); virtual;
     procedure SetAsFloat(Value: Float); virtual;
     procedure SetAsPointer(Value: Pointer);
     procedure SetAsQuad(Value: TISC_QUAD);
     procedure SetAsShort(Value: short); virtual;
     procedure SetAsString(Value: AnsiString); virtual;
     procedure SetAsVariant(Value: Variant);
     procedure SetAsNumeric(Value: Int64; aScale: integer);
     procedure SetAsBcd(aValue: tBCD); virtual;
     procedure SetIsNull(Value: Boolean); virtual;
     procedure SetIsNullable(Value: Boolean); virtual;
     procedure SetName(aValue: AnsiString); virtual;
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
     property AsString: AnsiString read GetAsString write SetAsString;
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
    FCaseSensitiveParams: boolean;
    function GetColumn(index: integer): TSQLVarData;
    function GetCount: integer;
  protected
    FUniqueRelationName: AnsiString;
    FColumnList: array of TSQLVarData;
    function GetStatement: IStatement; virtual; abstract;
    function GetPrepareSeqNo: integer; virtual; abstract;
    function GetTransactionSeqNo: integer; virtual; abstract;
    procedure SetCount(aValue: integer); virtual; abstract;
    procedure SetUniqueRelationName;
  public
    procedure Initialize; virtual;
    function IsInputDataArea: boolean; virtual; abstract; {Input to Database}
    procedure PreprocessSQL(sSQL: AnsiString; GenerateParamNames: boolean;
      var sProcessedSQL: AnsiString);
    function ColumnsInUseCount: integer; virtual;
    function ColumnByName(Idx: AnsiString): TSQLVarData;
    function CheckStatementStatus(Request: TStatementStatus): boolean; virtual; abstract;
    procedure GetData(index: integer; var IsNull: boolean; var len: short;
      var data: PByte); virtual;
    procedure RowChange;
    function StateChanged(var ChangeSeqNo: integer): boolean; virtual; abstract;
    property CaseSensitiveParams: boolean read FCaseSensitiveParams
                                            write FCaseSensitiveParams; {Only used when IsInputDataArea true}
    property Count: integer read GetCount;
    property Column[index: integer]: TSQLVarData read GetColumn;
    property UniqueRelationName: AnsiString read FUniqueRelationName;
    property Statement: IStatement read GetStatement;
    property PrepareSeqNo: integer read GetPrepareSeqNo;
    property TransactionSeqNo: integer read GetTransactionSeqNo;
  end;

  { TSQLVarData }

  TSQLVarData = class
  private
    FParent: TSQLDataArea;
    FName: AnsiString;
    FIndex: integer;
    FModified: boolean;
    FUniqueName: boolean;
    FVarString: RawByteString;
    function GetStatement: IStatement;
    procedure SetName(AValue: AnsiString);
  protected
    function GetSQLType: cardinal; virtual; abstract;
    function GetSubtype: integer; virtual; abstract;
    function GetAliasName: AnsiString;  virtual; abstract;
    function GetFieldName: AnsiString; virtual; abstract;
    function GetOwnerName: AnsiString;  virtual; abstract;
    function GetRelationName: AnsiString;  virtual; abstract;
    function GetScale: integer; virtual; abstract;
    function GetCharSetID: cardinal; virtual; abstract;
    function GetCharSetWidth: integer; virtual; abstract;
    function GetCodePage: TSystemCodePage; virtual; abstract;
    function GetIsNull: Boolean;   virtual; abstract;
    function GetIsNullable: boolean; virtual; abstract;
    function GetSQLData: PByte;  virtual; abstract;
    function GetDataLength: cardinal; virtual; abstract; {current field length}
    function GetSize: cardinal; virtual; abstract; {field length as given by metadata}
    procedure SetIsNull(Value: Boolean); virtual; abstract;
    procedure SetIsNullable(Value: Boolean);  virtual; abstract;
    procedure SetSQLData(AValue: PByte; len: cardinal); virtual; abstract;
    procedure SetScale(aValue: integer); virtual; abstract;
    procedure SetDataLength(len: cardinal); virtual; abstract;
    procedure SetSQLType(aValue: cardinal); virtual; abstract;
    procedure SetCharSetID(aValue: cardinal); virtual; abstract;
  public
    constructor Create(aParent: TSQLDataArea; aIndex: integer);
    procedure SetString(aValue: AnsiString);
    procedure Changed; virtual;
    procedure RowChange; virtual;
    function GetAsArray(Array_ID: TISC_QUAD): IArray; virtual; abstract;
    function GetAsBlob(Blob_ID: TISC_QUAD; BPB: IBPB): IBlob; virtual; abstract;
    function CreateBlob: IBlob; virtual; abstract;
    function GetArrayMetaData: IArrayMetaData; virtual; abstract;
    function GetBlobMetaData: IBlobMetaData; virtual; abstract;
    procedure Initialize; virtual;

  public
    property AliasName: AnsiString read GetAliasName;
    property FieldName: AnsiString read GetFieldName;
    property OwnerName: AnsiString read GetOwnerName;
    property RelationName: AnsiString read GetRelationName;
    property Parent: TSQLDataArea read FParent;
    property Index: integer read FIndex;
    property Name: AnsiString read FName write SetName;
    property CharSetID: cardinal read GetCharSetID write SetCharSetID;
    property SQLType: cardinal read GetSQLType write SetSQLType;
    property SQLSubtype: integer read GetSubtype;
    property SQLData: PByte read GetSQLData;
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
    FChangeSeqNo: integer;
  protected
    procedure CheckActive; override;
    function GetAttachment: IAttachment; override;
    function SQLData: PByte; override;
    function GetDataLength: cardinal; override;
    function GetCodePage: TSystemCodePage; override;

  public
    constructor Create(aOwner: IUnknown; aIBXSQLVAR: TSQLVarData);
    destructor Destroy; override;
    function GetSQLDialect: integer; override;

  public
    {IColumnMetaData}
    function GetIndex: integer;
    function GetSQLType: cardinal; override;
    function getSubtype: integer;
    function getRelationName: AnsiString;
    function getOwnerName: AnsiString;
    function getSQLName: AnsiString;    {Name of the column}
    function getAliasName: AnsiString;  {Alias Name of column or Column Name if not alias}
    function GetName: AnsiString; override;      {Disambiguated uppercase Field Name}
    function GetScale: integer; override;
    function getCharSetID: cardinal; override;
    function GetIsNullable: boolean; override;
    function GetSize: cardinal; override;
    function GetCharSetWidth: integer; override;
    function GetArrayMetaData: IArrayMetaData;
    function GetBlobMetaData: IBlobMetaData;
    function GetStatement: IStatement;
    function GetTransaction: ITransaction; virtual;
    property Name: AnsiString read GetName;
    property Size: cardinal read GetSize;
    property CharSetID: cardinal read getCharSetID;
    property SQLSubtype: integer read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  public
    property Statement: IStatement read GetStatement;
  end;

  { TIBSQLData }

  TIBSQLData = class(TColumnMetaData,ISQLData)
  private
    FTransaction: ITransaction;
  protected
    procedure CheckActive; override;
  public
    function GetTransaction: ITransaction; override;
    function GetIsNull: Boolean; override;
    function GetAsArray: IArray;
    function GetAsBlob: IBlob; overload;
    function GetAsBlob(BPB: IBPB): IBlob; overload;
    function GetAsString: AnsiString; override;
    property AsBlob: IBlob read GetAsBlob;
 end;

  { TSQLParam }

  TSQLParam = class(TIBSQLData,ISQLParam,ISQLData)
  protected
    procedure CheckActive; override;
    procedure Changed; override;
    procedure InternalSetAsString(Value: AnsiString); override;
    procedure SetScale(aValue: integer); override;
    procedure SetDataLength(len: cardinal); override;
    procedure SetSQLType(aValue: cardinal); override;
  public
    procedure Clear;
    function GetModified: boolean; override;
    function GetAsPointer: Pointer;
    procedure SetName(Value: AnsiString); override;
    procedure SetIsNull(Value: Boolean);  override;
    procedure SetIsNullable(Value: Boolean); override;
    procedure SetAsArray(anArray: IArray);

    {overrides}
    procedure SetAsBoolean(AValue: boolean);
    procedure SetAsCurrency(AValue: Currency);
    procedure SetAsInt64(AValue: Int64);
    procedure SetAsDate(AValue: TDateTime);
    procedure SetAsLong(AValue: Long);
    procedure SetAsTime(AValue: TDateTime); overload;
    procedure SetAsTime(aValue: TDateTime; OnDate: TDateTime; aTimeZoneID: TFBTimeZoneID); overload;
    procedure SetAsTime(aValue: TDateTime; OnDate: TDateTime; aTimeZone: AnsiString); overload;
    procedure SetAsTime(aValue: TDateTime; aTimeZoneID: TFBTimeZoneID); overload;
    procedure SetAsTime(aValue: TDateTime; aTimeZone: AnsiString); overload;
    procedure SetAsDateTime(AValue: TDateTime); overload;
    procedure SetAsDateTime(aValue: TDateTime; aTimeZoneID: TFBTimeZoneID); overload;
    procedure SetAsDateTime(aValue: TDateTime; aTimeZone: AnsiString); overload;
    procedure SetAsDouble(AValue: Double);
    procedure SetAsFloat(AValue: Float);
    procedure SetAsPointer(AValue: Pointer);
    procedure SetAsShort(AValue: Short);
    procedure SetAsString(AValue: AnsiString); override;
    procedure SetAsVariant(AValue: Variant);
    procedure SetAsBlob(aValue: IBlob);
    procedure SetAsQuad(AValue: TISC_QUAD);
    procedure SetCharSetID(aValue: cardinal);
    procedure SetAsBcd(aValue: tBCD);

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
    function GetUniqueRelationName: AnsiString;
    function getCount: integer;
    function getColumnMetaData(index: integer): IColumnMetaData;
    function ByName(Idx: AnsiString): IColumnMetaData;
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
    function ByName(Idx: AnsiString): ISQLParam ;
    function GetModified: Boolean;
    function GetHasCaseSensitiveParams: Boolean;
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
     function ByName(Idx: AnsiString): ISQLData;
     function getSQLData(index: integer): ISQLData;
     procedure GetData(index: integer; var IsNull:boolean; var len: short; var data: PByte);
     function GetStatement: IStatement;
     function GetTransaction: ITransaction; virtual;
     procedure SetRetainInterfaces(aValue: boolean);
 end;

implementation

uses FBMessages, variants, IBUtils, FBTransaction, DateUtils;

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
  RelationName: AnsiString;
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

procedure TSQLDataArea.PreprocessSQL(sSQL: AnsiString; GenerateParamNames: boolean;
  var sProcessedSQL: AnsiString);

var slNames: TStrings;

  procedure SetColumnNames(slNames: TStrings);
  var i, j: integer;
      found: boolean;
  begin
    found := false;
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
  end;

begin
  if not IsInputDataArea then
    IBError(ibxeNotPermitted,[nil]);

  slNames := TStringList.Create;
  try
    sProcessedSQL := TSQLParamProcessor.Execute(sSQL,GenerateParamNames,slNames);
    SetColumnNames(slNames);
  finally
    slNames.Free;
  end;
end;

function TSQLDataArea.ColumnsInUseCount: integer;
begin
  Result := Count;
end;

function TSQLDataArea.ColumnByName(Idx: AnsiString): TSQLVarData;
var
  s: AnsiString;
  i: Integer;
begin
  if not IsInputDataArea or not CaseSensitiveParams then
   s := AnsiUpperCase(Idx)
  else
   s := Idx;

  for i := 0 to Count - 1 do
    if Column[i].Name = s then
    begin
         Result := Column[i];
         Exit;
    end;
  Result := nil;
end;

procedure TSQLDataArea.GetData(index: integer; var IsNull: boolean;
  var len: short; var data: PByte);
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

procedure TSQLVarData.SetName(AValue: AnsiString);
begin
  if not Parent.IsInputDataArea or not Parent.CaseSensitiveParams then
    FName := AnsiUpperCase(AValue)
  else
    FName := AValue;
end;

constructor TSQLVarData.Create(aParent: TSQLDataArea; aIndex: integer);
begin
  inherited Create;
  FParent := aParent;
  FIndex := aIndex;
  FUniqueName := true;
end;

procedure TSQLVarData.SetString(aValue: AnsiString);
begin
  {we take full advantage here of reference counted strings. When setting a string
   value, a reference is kept in FVarString and a pointer to it placed in the
   SQLVar. This avoids string copies. Note that PAnsiChar is guaranteed to point to
   a zero byte when the string is empty, neatly avoiding a nil pointer error.}

  FVarString := aValue;
  SQLType := SQL_TEXT;
  Scale := 0;
  SetSQLData(PByte(PAnsiChar(FVarString)),Length(aValue));
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

  function FindVarByName(idx: AnsiString; limit: integer): TSQLVarData;
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
  st: AnsiString;
  sBaseName: AnsiString;
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
  FractionText, PadText, CurrText: AnsiString;
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
      {$IF declared(DefaultFormatSettings)}
      with DefaultFormatSettings do
      {$ELSE}
      {$IF declared(FormatSettings)}
      with FormatSettings do
      {$IFEND}
      {$IFEND}
      if Value < 0 then
        CurrText := '-' + IntToStr(Abs(Value div Scaling)) + DecimalSeparator + PadText + FractionText
      else
        CurrText := IntToStr(Abs(Value div Scaling)) + DecimalSeparator + PadText + FractionText;
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

function TSQLDataItem.GetDateFormatStr(IncludeTime: boolean): AnsiString;
begin
  {$IF declared(DefaultFormatSettings)}
  with DefaultFormatSettings do
  {$ELSE}
  {$IF declared(FormatSettings)}
  with FormatSettings do
  {$IFEND}
  {$IFEND}
  case GetSQLDialect of
    1:
      if IncludeTime then
        result := ShortDateFormat + ' ' + LongTimeFormat
      else
        result := ShortDateFormat;
    3:
      result := ShortDateFormat;
  end;
end;

function TSQLDataItem.GetTimeFormatStr: AnsiString;
begin
  {$IF declared(DefaultFormatSettings)}
  with DefaultFormatSettings do
  {$ELSE}
  {$IF declared(FormatSettings)}
  with FormatSettings do
  {$IFEND}
  {$IFEND}
    Result := 'hh' + TimeSeparator + 'nn' + TimeSeparator + 'ss' + '.zzzz';;
end;

function TSQLDataItem.GetTimestampFormatStr: AnsiString;
begin
  {$IF declared(DefaultFormatSettings)}
  with DefaultFormatSettings do
  {$ELSE}
  {$IF declared(FormatSettings)}
  with FormatSettings do
  {$IFEND}
  {$IFEND}
    Result := ShortDateFormat + ' ' +  'hh' + TimeSeparator + 'nn' + TimeSeparator + 'ss' + '.zzzz';
end;

procedure TSQLDataItem.SetAsInteger(AValue: Integer);
begin
  SetAsLong(aValue);
end;

procedure TSQLDataItem.InternalGetAsDateTime(var aDateTime: TDateTime;
  var dstOffset: smallint; var aTimezone: AnsiString;
  var aTimeZoneID: TFBTimeZoneID);
begin
  CheckActive;
  aDateTime := 0;
  dstOffset := 0;
  aTimezone := '';
  aTimeZoneID := TimeZoneID_GMT;
  if not IsNull then
    with FFirebirdClientAPI do
    case SQLType of
      SQL_TEXT, SQL_VARYING:
        if not ParseDateTimeTZString(AsString,aDateTime,aTimeZone) then
          IBError(ibxeInvalidDataConversion, [nil]);
      SQL_TYPE_DATE:
        aDateTime := SQLDecodeDate(SQLData);
      SQL_TYPE_TIME:
        aDateTime := SQLDecodeTime(SQLData);
      SQL_TIMESTAMP:
        aDateTime := SQLDecodeDateTime(SQLData);
      SQL_TIMESTAMP_TZ:
        begin
          GetTimeZoneServices.DecodeTimestampTZ(SQLData,aDateTime,dstOffset,aTimeZone);
          aTimeZoneID := PISC_TIMESTAMP_TZ(SQLData)^.time_zone;
        end;
      SQL_TIMESTAMP_TZ_EX:
      begin
        GetTimeZoneServices.DecodeTimestampTZEx(SQLData,aDateTime,dstOffset,aTimeZone);
        aTimeZoneID := PISC_TIMESTAMP_TZ_EX(SQLData)^.time_zone;
      end;
      SQL_TIME_TZ:
        with GetTimeZoneServices do
        begin
          DecodeTimeTZ(SQLData,GetTimeTZDate,aDateTime,dstOffset,aTimeZone);
          aTimeZoneID := PISC_TIME_TZ(SQLData)^.time_zone;
        end;
      SQL_TIME_TZ_EX:
        with GetTimeZoneServices do
        begin
          DecodeTimeTZEx(SQLData,GetTimeTZDate,aDateTime,dstOffset,aTimeZone);
          aTimeZoneID := PISC_TIME_TZ_EX(SQLData)^.time_zone;
        end;
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
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

procedure TSQLDataItem.CheckTZSupport;
begin
  if not FFirebirdClientAPI.HasTimeZoneSupport then
    IBError(ibxeNoTimezoneSupport,[]);
end;

function TSQLDataItem.GetTimeZoneServices: IExTimeZoneServices;
begin
  if FTimeZoneServices = nil then
  begin
    if not GetAttachment.HasTimeZoneSupport then
      IBError(ibxeNoTimezoneSupport,[]);
    GetAttachment.GetTimeZoneServices.QueryInterface(IExTimeZoneServices,FTimeZoneServices);
  end;
  Result := FTimeZoneServices;
end;

procedure TSQLDataItem.Changed;
begin
  //Do nothing by default
end;

procedure TSQLDataItem.Changing;
begin
  //Do nothing by default
end;

procedure TSQLDataItem.InternalSetAsString(Value: AnsiString);
begin
  //Do nothing by default
end;

function TSQLDataItem.Transliterate(s: AnsiString; CodePage: TSystemCodePage
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

constructor TSQLDataItem.Create(api: TFBClientAPI);
begin
  inherited Create;
  FFirebirdClientAPI := api;
end;

function TSQLDataItem.GetSQLTypeName: AnsiString;
begin
  Result := GetSQLTypeName(GetSQLType);
end;

class function TSQLDataItem.GetSQLTypeName(SQLType: short): AnsiString;
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
  SQL_TIMESTAMP_TZ:     Result := 'SQL_TIMESTAMP_TZ';
  SQL_TIMESTAMP_TZ_EX:  Result := 'SQL_TIMESTAMP_TZ_EX';
  SQL_BLOB:		Result := 'SQL_BLOB';
  SQL_D_FLOAT:          Result := 'SQL_D_FLOAT';
  SQL_ARRAY:		Result := 'SQL_ARRAY';
  SQL_QUAD:		Result := 'SQL_QUAD';
  SQL_TYPE_TIME:	Result := 'SQL_TYPE_TIME';
  SQL_TYPE_DATE:	Result := 'SQL_TYPE_DATE';
  SQL_INT64:		Result := 'SQL_INT64';
  SQL_TIME_TZ:          Result := 'SQL_TIME_TZ';
  SQL_TIME_TZ_EX:       Result := 'SQL_TIME_TZ_EX';
  SQL_DEC_FIXED:        Result := 'SQL_DEC_FIXED';
  SQL_DEC16:            Result := 'SQL_DEC16';
  SQL_DEC34:            Result := 'SQL_DEC34';
  SQL_INT128:           Result := 'SQL_INT128';
  SQL_NULL:             Result := 'SQL_NULL';
  SQL_BOOLEAN:          Result := 'SQL_BOOLEAN';
  end;
end;

function TSQLDataItem.GetStrDataLength: short;
begin
  with FFirebirdClientAPI do
  if SQLType = SQL_VARYING then
    Result := DecodeInteger(SQLData, 2)
  else
    Result := DataLength;
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

        SQL_DEC_FIXED,
        SQL_DEC16,
        SQL_DEC34,
        SQL_INT128:
          if not BCDToCurr(GetAsBCD,Result) then
            IBError(ibxeInvalidDataConversion, [nil]);

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
var aTimezone: AnsiString;
    aTimeZoneID: TFBTimeZoneID;
    dstOffset: smallint;
begin
  InternalGetAsDateTime(Result,dstOffset,aTimeZone,aTimeZoneID);
end;

procedure TSQLDataItem.GetAsDateTime(var aDateTime: TDateTime;
  var dstOffset: smallint; var aTimezone: AnsiString);
var aTimeZoneID: TFBTimeZoneID;
begin
  InternalGetAsDateTime(aDateTime,dstOffset,aTimeZone,aTimeZoneID);
end;

procedure TSQLDataItem.GetAsDateTime(var aDateTime: TDateTime; var dstOffset: smallint;
  var aTimezoneID: TFBTimeZoneID);
var aTimezone: AnsiString;
begin
  InternalGetAsDateTime(aDateTime,dstOffset,aTimeZone,aTimeZoneID);
end;

procedure TSQLDataItem.GetAsTime(var aTime: TDateTime; var dstOffset: smallint;
  var aTimezoneID: TFBTimeZoneID; OnDate: TDateTime);
var aTimeZone: AnsiString;
begin
  CheckActive;
  aTime := 0;
  dstOffset := 0;
  if not IsNull then
    with FFirebirdClientAPI do
    case SQLType of
      SQL_TIME_TZ:
        begin
          GetTimeZoneServices.DecodeTimeTZ(SQLData,OnDate,aTime,dstOffset,aTimeZone);
          aTimeZoneID := PISC_TIME_TZ(SQLData)^.time_zone;
        end;
      SQL_TIME_TZ_EX:
        begin
          GetTimeZoneServices.DecodeTimeTZEx(SQLData,OnDate,aTime,dstOffset,aTimeZone);
          aTimeZoneID := PISC_TIME_TZ_EX(SQLData)^.time_zone;
        end;
    else
      IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

procedure TSQLDataItem.GetAsTime(var aTime: TDateTime; var dstOffset: smallint;
  var aTimezone: AnsiString; OnDate: TDateTime);
begin
  CheckActive;
  aTime := 0;
  dstOffset := 0;
  if not IsNull then
    with FFirebirdClientAPI do
    case SQLType of
      SQL_TIME_TZ:
        GetTimeZoneServices.DecodeTimeTZ(SQLData,OnDate,aTime,dstOffset,aTimeZone);
      SQL_TIME_TZ_EX:
        GetTimeZoneServices.DecodeTimeTZEx(SQLData,OnDate,aTime,dstOffset,aTimeZone);
    else
      IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

procedure TSQLDataItem.GetAsTime(var aTime: TDateTime; var dstOffset: smallint;
  var aTimezoneID: TFBTimeZoneID);
begin
  GetAsTime(aTime,dstOffset,aTimeZoneID,GetTimeZoneServices.GetTimeTZDate);
end;

procedure TSQLDataItem.GetAsTime(var aTime: TDateTime; var dstOffset: smallint;
  var aTimezone: AnsiString);
begin
  GetAsTime(aTime,dstOffset,aTimeZone,GetTimeZoneServices.GetTimeTZDate);
end;

function TSQLDataItem.GetAsUTCDateTime: TDateTime;
var aTimezone: AnsiString;
begin
  CheckActive;
  result := 0;
  aTimezone := '';
  if not IsNull then
    with FFirebirdClientAPI do
    case SQLType of
      SQL_TEXT, SQL_VARYING:
      begin
        if not ParseDateTimeTZString(AsString,Result,aTimeZone) then
          IBError(ibxeInvalidDataConversion, [nil]);
        Result := GetTimeZoneServices.LocalTimeToGMT(Result,aTimeZone);
      end;
      SQL_TYPE_DATE:
        result := SQLDecodeDate(SQLData);
      SQL_TYPE_TIME,
      SQL_TIME_TZ,
      SQL_TIME_TZ_EX:
        result := SQLDecodeTime(SQLData);
      SQL_TIMESTAMP,
      SQL_TIMESTAMP_TZ,
      SQL_TIMESTAMP_TZ_EX:
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
      SQL_DEC_FIXED,
      SQL_DEC16,
      SQL_DEC34,
      SQL_INT128:
        Result := BCDToDouble(GetAsBCD);
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
      SQL_DEC_FIXED,
      SQL_DEC16,
      SQL_DEC34,
      SQL_INT128:
        Result := BCDToInteger(GetAsBCD);
      else
        IBError(ibxeInvalidDataConversion, [GetSQLTypeName]);
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

{Copied from LazUTF8}

function UTF8CodepointSizeFull(p: PAnsiChar): integer;
const TopBitSetMask   = $80; {%10000000}
      Top2BitsSetMask = $C0; {%11000000}
      Top3BitsSetMask = $E0; {%11100000}
      Top4BitsSetMask = $F0; {%11110000}
      Top5BitsSetMask = $F8; {%11111000}
begin
  case p^ of
  #0..#191: // %11000000
    // regular single byte character (#0 is a character, this is Pascal ;)
    Result:=1;
  #192..#223: // p^ and %11100000 = %11000000
    begin
      // could be 2 byte character
      if (ord(p[1]) and Top2BitsSetMask) = TopBitSetMask then
        Result:=2
      else
        Result:=1;
    end;
  #224..#239: // p^ and %11110000 = %11100000
    begin
      // could be 3 byte character
      if ((ord(p[1]) and Top2BitsSetMask) = TopBitSetMask)
      and ((ord(p[2]) and Top2BitsSetMask) = TopBitSetMask) then
        Result:=3
      else
        Result:=1;
    end;
  #240..#247: // p^ and %11111000 = %11110000
    begin
      // could be 4 byte character
      if ((ord(p[1]) and Top2BitsSetMask) = TopBitSetMask)
      and ((ord(p[2]) and Top2BitsSetMask) = TopBitSetMask)
      and ((ord(p[3]) and Top2BitsSetMask) = TopBitSetMask) then
        Result:=4
      else
        Result:=1;
    end;
  else
    Result:=1;
  end;
end;

{Returns the byte length of a UTF8 string with a fixed charwidth}

function GetStrLen(p: PAnsiChar; FieldWidth, MaxDataLength: cardinal): integer;
var i: integer;
    cplen: integer;
    s: AnsiString;
begin
  Result := 0;
  s := strpas(p);
  for i := 1 to FieldWidth do
  begin
    cplen := UTF8CodepointSizeFull(p);
    Inc(p,cplen);
    Inc(Result,cplen);
    if Result >= MaxDataLength then
    begin
      Result := MaxDataLength;
      Exit;
    end;
  end;
end;

function TSQLDataItem.GetAsString: AnsiString;
var
  sz: PByte;
  str_len: Integer;
  rs: RawByteString;
  aTimeZone: AnsiString;
  aDateTime: TDateTime;
  dstOffset: smallint;
begin
  CheckActive;
  result := '';
  { Check null, if so return a default string }
  if not IsNull then
  with FFirebirdClientAPI do
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
        begin
          if GetCodePage = cp_utf8 then
            str_len := GetStrLen(PAnsiChar(sz),GetSize div GetCharSetWidth,DataLength)
          else
            str_len := DataLength
        end
        else begin
          str_len := DecodeInteger(sz, 2);
          Inc(sz, 2);
        end;
        SetString(rs, PAnsiChar(sz), str_len);
        SetCodePage(rs,GetCodePage,false);
        Result := rs;
      end;

      SQL_TYPE_DATE:
        Result := DateToStr(GetAsDateTime);
      SQL_TIMESTAMP:
        Result := FBFormatDateTime(GetTimestampFormatStr,GetAsDateTime);
      SQL_TYPE_TIME:
        Result := FBFormatDateTime(GetTimeFormatStr,GetAsDateTime);
      SQL_TIMESTAMP_TZ,
      SQL_TIMESTAMP_TZ_EX:
        with GetAttachment.GetTimeZoneServices do
        begin
          if GetTZTextOption = tzGMT then
            Result := FBFormatDateTime(GetTimestampFormatStr,GetAsUTCDateTime)
          else
          begin
            GetAsDateTime(aDateTime,dstOffset,aTimeZone);
            if GetTZTextOption = tzOffset then
              Result := FBFormatDateTime(GetTimestampFormatStr,aDateTime) + ' ' + FormatTimeZoneOffset(dstOffset)
            else
              Result := FBFormatDateTime(GetTimestampFormatStr,aDateTime) + ' ' + aTimeZone;
          end;
        end;
      SQL_TIME_TZ,
      SQL_TIME_TZ_EX:
        with GetAttachment.GetTimeZoneServices do
        begin
          if GetTZTextOption = tzGMT then
             Result := FBFormatDateTime(GetTimeFormatStr,GetAsUTCDateTime)
          else
          begin
            GetAsTime(aDateTime,dstOffset,aTimeZone,GetTimeTZDate);
            if GetTZTextOption = tzOffset then
              Result := FBFormatDateTime(GetTimeFormatStr,aDateTime) + ' ' + FormatTimeZoneOffset(dstOffset)
            else
              Result := FBFormatDateTime(GetTimeFormatStr,aDateTime) + ' ' + aTimeZone;
          end;
        end;

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

      SQL_DEC16,
      SQL_DEC34:
        result := BCDToStr(GetAsBCD);

      SQL_DEC_FIXED,
      SQL_INT128:
        result := Int128ToStr(SQLData,scale);

      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetIsNull: Boolean;
begin
  CheckActive;
  Result := false;
end;

function TSQLDataItem.GetIsNullable: boolean;
begin
  CheckActive;
  Result := false;
end;

function TSQLDataItem.GetAsVariant: Variant;
var ts: TDateTime;
  dstOffset: smallint;
    timezone: AnsiString;
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
      SQL_TIMESTAMP_TZ,
      SQL_TIME_TZ,
      SQL_TIMESTAMP_TZ_EX,
      SQL_TIME_TZ_EX:
        begin
          GetAsDateTime(ts,dstOffset,timezone);
          result := VarArrayOf([ts,dstOffset,timezone]);
        end;
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
      SQL_DEC_FIXED,
      SQL_DEC16,
      SQL_DEC34,
      SQL_INT128:
        result := VarFmtBCDCreate(GetAsBcd);
      else
        IBError(ibxeInvalidDataConversion, [nil]);
    end;
end;

function TSQLDataItem.GetModified: boolean;
begin
  Result := false;
end;

function TSQLDataItem.GetDateTimeStrLength(DateTimeFormat: TIBDateTimeFormats
  ): integer;
begin
  case DateTimeFormat of
  dfTimestamp:
    Result := Length(GetTimestampFormatStr);
  dfDateTime:
    Result := Length(GetDateFormatStr(true));
  dfTime:
    Result := Length(GetTimeFormatStr);
  dfTimestampTZ:
    Result := Length(GetTimestampFormatStr) + 6; {assume time offset format}
  dfTimeTZ:
    Result := Length(GetTimeFormatStr)+ 6;
  else
    Result := 0;
  end;end;

function TSQLDataItem.GetAsBCD: tBCD;

begin
  CheckActive;
  if IsNull then
   with Result do
   begin
     FillChar(Result,sizeof(Result),0);
     Precision := 1;
     exit;
   end;

  case SQLType of
  SQL_DEC16,
  SQL_DEC34:
    with FFirebirdClientAPI do
      Result := SQLDecFloatDecode(SQLType,  SQLData);

  SQL_DEC_FIXED,
  SQL_INT128:
    with FFirebirdClientAPI do
      Result := StrToBCD(Int128ToStr(SQLData,scale));
  else
    if not CurrToBCD(GetAsCurrency,Result) then
      IBError(ibxeBadBCDConversion,[]);
  end;
end;


procedure TSQLDataItem.SetIsNull(Value: Boolean);
begin
  //ignore unless overridden
end;

procedure TSQLDataItem.SetIsNullable(Value: Boolean);
begin
  //ignore unless overridden
end;

procedure TSQLDataItem.SetName(aValue: AnsiString);
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
  with FFirebirdClientAPI do
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
  with FFirebirdClientAPI do
    SQLEncodeTime(Value,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsTime(aValue: TDateTime; OnDate: TDateTime; aTimeZoneID: TFBTimeZoneID);
begin
  CheckActive;
  CheckTZSupport;
  if GetSQLDialect < 3 then
  begin
    AsDateTime := aValue;
    exit;
  end;

  Changing;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_TIME_TZ;
  DataLength := SizeOf(ISC_TIME_TZ);
  GetTimeZoneServices.EncodeTimeTZ(aValue, aTimeZoneID,OnDate,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsTime(aValue: TDateTime; OnDate: TDateTime; aTimeZone: AnsiString);
begin
  CheckActive;
  CheckTZSupport;
  if GetSQLDialect < 3 then
  begin
    AsDateTime := aValue;
    exit;
  end;

  Changing;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_TIME_TZ;
  DataLength := SizeOf(ISC_TIME_TZ);
  GetTimeZoneServices.EncodeTimeTZ(aValue, aTimeZone,OnDate,SQLData);
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
  with FFirebirdClientAPI do
    SQLEncodeDateTime(Value,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsDateTime(aValue: TDateTime;
  aTimeZoneID: TFBTimeZoneID);
begin
  CheckActive;
  CheckTZSupport;
  if IsNullable then
    IsNull := False;

  Changing;
  SQLType := SQL_TIMESTAMP_TZ;
  DataLength := SizeOf(ISC_TIMESTAMP_TZ);
  GetTimeZoneServices.EncodeTimestampTZ(aValue,aTimeZoneID,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsDateTime(aValue: TDateTime; aTimeZone: AnsiString
  );
begin
  CheckActive;
  CheckTZSupport;
  if IsNullable then
    IsNull := False;

  Changing;
  SQLType := SQL_TIMESTAMP_TZ;
  DataLength := SizeOf(ISC_TIMESTAMP_TZ);
  GetTimeZoneServices.EncodeTimestampTZ(aValue,aTimeZone,SQLData);
  Changed;
end;

procedure TSQLDataItem.SetAsUTCDateTime(aUTCTime: TDateTime);
begin
  SetAsDateTime(aUTCTime,TimeZoneID_GMT);
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

procedure TSQLDataItem.SetAsString(Value: AnsiString);
begin
  InternalSetAsString(Value);
end;

procedure TSQLDataItem.SetAsVariant(Value: Variant);
begin
  CheckActive;
  if VarIsNull(Value) then
    IsNull := True
  else
  if VarIsArray(Value) then {must be datetime plus timezone}
    SetAsDateTime(Value[0],AnsiString(Value[1]))
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
    else
      if VarIsFmtBCD(Value) then
        SetAsBCD(VarToBCD(Value))
      else
        IBError(ibxeNotSupported, [nil]);
  end;
end;

procedure TSQLDataItem.SetAsNumeric(Value: Int64; aScale: integer);
begin
  CheckActive;
  Changing;
  if IsNullable then
    IsNull := False;

  SQLType := SQL_INT64;
  Scale := aScale;
  DataLength := SizeOf(Int64);
  PInt64(SQLData)^ := Value;
  Changed;
end;

procedure TSQLDataItem.SetAsBcd(aValue: tBCD);
var C: Currency;
begin
  CheckActive;
  Changing;
  if IsNullable then
    IsNull := False;


  with FFirebirdClientAPI do
  if aValue.Precision <= 16 then
  begin
    if not HasDecFloatSupport then
      IBError(ibxeDecFloatNotSupported,[]);

    SQLType := SQL_DEC16;
    DataLength := 8;
    SQLDecFloatEncode(aValue,SQLType,SQLData);
  end
  else
  if aValue.Precision <= 34 then
  begin
    if not HasDecFloatSupport then
      IBError(ibxeDecFloatNotSupported,[]);

    SQLType := SQL_DEC34;
    DataLength := 16;
    SQLDecFloatEncode(aValue,SQLType,SQLData);
  end
  else
  if aValue.Precision <= 38 then
  begin
    if not HasInt128Support then
      IBError(ibxeInt128NotSupported,[]);

    SQLType := SQL_INT128;
    DataLength := 16;
    StrToInt128(scale,BcdToStr(aValue),SQLData);
  end
  else
    IBError(ibxeBCDOverflow,[BCDToStr(aValue)]);

  Changed;
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

function TColumnMetaData.GetAttachment: IAttachment;
begin
  Result := GetStatement.GetAttachment;
end;

function TColumnMetaData.SQLData: PByte;
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
  inherited Create(aIBXSQLVAR.GetStatement.GetAttachment.getFirebirdAPI as TFBClientAPI);
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

function TColumnMetaData.getRelationName: AnsiString;
begin
  CheckActive;
   result :=  FIBXSQLVAR.RelationName;
end;

function TColumnMetaData.getOwnerName: AnsiString;
begin
  CheckActive;
  result :=  FIBXSQLVAR.OwnerName;
end;

function TColumnMetaData.getSQLName: AnsiString;
begin
  CheckActive;
  result :=  FIBXSQLVAR.FieldName;
end;

function TColumnMetaData.getAliasName: AnsiString;
begin
  CheckActive;
  result := FIBXSQLVAR.AliasName;
end;

function TColumnMetaData.GetName: AnsiString;
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
  result := FIBXSQLVAR.GetSize;
end;

function TColumnMetaData.GetCharSetWidth: integer;
begin
  CheckActive;
  result := FIBXSQLVAR.GetCharSetWidth;
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

function TColumnMetaData.GetStatement: IStatement;
begin
  Result := FIBXSQLVAR.GetStatement;
end;

function TColumnMetaData.GetTransaction: ITransaction;
begin
  Result := GetStatement.GetTransaction;
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

function TIBSQLData.GetTransaction: ITransaction;
begin
  if FTransaction = nil then
    Result := inherited GetTransaction
  else
    Result := FTransaction;
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

function TIBSQLData.GetAsString: AnsiString;
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

procedure TSQLParam.InternalSetAsString(Value: AnsiString);

procedure DoSetString;
begin
  Changing;
  FIBXSQLVar.SetString(Transliterate(Value,GetCodePage));
  Changed;
end;

var b: IBlob;
    dt: TDateTime;
    CurrValue: Currency;
    FloatValue: single;
    timezone: AnsiString;
begin
  CheckActive;
  if IsNullable then
    IsNull := False;
  with FFirebirdClientAPI do
  case SQLTYPE of
  SQL_BOOLEAN:
    if AnsiCompareText(Value,STrue) = 0 then
      AsBoolean := true
    else
    if AnsiCompareText(Value,SFalse) = 0 then
      AsBoolean := false
    else
      IBError(ibxeInvalidDataConversion,[nil]);

  SQL_BLOB:
    if Length(Value) < MaxInlineBlobString then
      DoSetString
    else
    begin
      Changing;
      b := FIBXSQLVAR.CreateBlob;
      b.SetAsString(Value);
      AsBlob := b;
      Changed;
    end;

  SQL_VARYING,
  SQL_TEXT:
    DoSetString;

    SQL_SHORT,
    SQL_LONG,
    SQL_INT64:
      if TryStrToCurr(Value,CurrValue) then
        SetAsNumeric(AdjustScaleFromCurrency(CurrValue,GetScale),GetScale)
      else
        DoSetString;

    SQL_D_FLOAT,
    SQL_DOUBLE,
    SQL_FLOAT:
      if TryStrToFloat(Value,FloatValue) then
        SetAsDouble(FloatValue)
      else
        DoSetString;

    SQL_TIMESTAMP:
      if TryStrToDateTime(Value,dt) then
        SetAsDateTime(dt)
      else
        DoSetString;

    SQL_TYPE_DATE:
      if TryStrToDateTime(Value,dt) then
        SetAsDate(dt)
      else
        DoSetString;

    SQL_TYPE_TIME:
      if TryStrToDateTime(Value,dt) then
        SetAsTime(dt)
      else
        DoSetString;

    SQL_TIMESTAMP_TZ:
      if ParseDateTimeTZString(value,dt,timezone) then
        SetAsDateTime(dt,timezone)
      else
        DoSetString;

    SQL_TIME_TZ:
      if ParseDateTimeTZString(value,dt,timezone,true) then
        SetAsTime(dt,GetAttachment.GetTimeZoneServices.GetTimeTZDate,timezone)
      else
        DoSetString;

    SQL_DEC_FIXED,
    SQL_DEC16,
    SQL_DEC34,
    SQL_INT128:
      SetAsBCD(StrToBCD(Value));

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

procedure TSQLParam.SetName(Value: AnsiString);
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

procedure TSQLParam.SetAsTime(aValue: TDateTime; OnDate: TDateTime; aTimeZoneID: TFBTimeZoneID);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsTime(AValue,OnDate, aTimeZoneID)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsTime(AValue,OnDate, aTimeZoneID);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsTime(aValue: TDateTime; OnDate: TDateTime; aTimeZone: AnsiString);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsTime(AValue,OnDate,aTimeZone)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsTime(AValue,OnDate,aTimeZone);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsTime(aValue: TDateTime; aTimeZoneID: TFBTimeZoneID);
begin
  SetAsTime(aValue,GetTimeZoneServices.GetTimeTZDate,aTimeZoneID);
end;

procedure TSQLParam.SetAsTime(aValue: TDateTime; aTimeZone: AnsiString);
begin
  SetAsTime(aValue,GetTimeZoneServices.GetTimeTZDate,aTimeZone);
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

procedure TSQLParam.SetAsDateTime(aValue: TDateTime; aTimeZoneID: TFBTimeZoneID
  );
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsDateTime(AValue,aTimeZoneID)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsDateTime(AValue,aTimeZoneID);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
end;

procedure TSQLParam.SetAsDateTime(aValue: TDateTime; aTimeZone: AnsiString);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsDateTime(AValue,aTimeZone)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsDateTime(AValue,aTimeZone);
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

procedure TSQLParam.SetAsString(AValue: AnsiString);
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

procedure TSQLParam.SetAsBcd(aValue: tBCD);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsBcd(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsBcd(AValue);
        finally
          FIBXSQLVAR := OldSQLVar;
        end;
      end;
  end;
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

function TMetaData.GetUniqueRelationName: AnsiString;
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

function TMetaData.ByName(Idx: AnsiString): IColumnMetaData;
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

function TSQLParams.ByName(Idx: AnsiString): ISQLParam;
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

function TSQLParams.GetHasCaseSensitiveParams: Boolean;
begin
  Result := FSQLParams.CaseSensitiveParams;
end;

{ TResults }

procedure TResults.CheckActive;
begin
  if not FResults.StateChanged(FChangeSeqNo) then Exit;

  if FPrepareSeqNo < FResults.PrepareSeqNo then
    IBError(ibxeInterfaceOutofDate,[nil]);

  if not FResults.CheckStatementStatus(ssPrepared)  then
    IBError(ibxeStatementNotPrepared, [nil]);

  with GetTransaction do
  if not InTransaction or (FResults.TransactionSeqNo <> FTransactionSeqNo) then
    IBError(ibxeInterfaceOutofDate,[nil]);
end;

function TResults.GetISQLData(aIBXSQLVAR: TSQLVarData): ISQLData;
var col: TIBSQLData;
begin
  if (aIBXSQLVAR.Index < 0) or (aIBXSQLVAR.Index >= getCount) then
    IBError(ibxeInvalidColumnIndex,[nil]);

  if not HasInterface(aIBXSQLVAR.Index) then
    AddInterface(aIBXSQLVAR.Index, TIBSQLData.Create(self,aIBXSQLVAR));
  col := TIBSQLData(GetInterface(aIBXSQLVAR.Index));
  col.FTransaction := GetTransaction;
  Result := col;
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

function TResults.ByName(Idx: AnsiString): ISQLData;
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
  var data: PByte);
begin
  CheckActive;
  FResults.GetData(index,IsNull, len,data);
end;

function TResults.GetStatement: IStatement;
begin
  Result := FStatement;
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

