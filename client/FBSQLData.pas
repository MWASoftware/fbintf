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
     function GetDateFormatStr(IncludeTime: boolean): AnsiString;
     function GetTimeFormatStr: AnsiString;
     function GetTimestampFormatStr: AnsiString;
     procedure SetAsInteger(AValue: Integer);
     procedure InternalGetAsDateTime(var aDateTime: TDateTime; var dstOffset: smallint;
       var aTimezone: AnsiString; var aTimeZoneID: TFBTimeZoneID);
  protected
     procedure CheckActive; virtual;
     procedure CheckTZSupport;
     function GetAttachment: IAttachment; virtual; abstract;
     function GetTransaction: ITransaction; virtual; abstract;
     function GetSQLDialect: integer; virtual; abstract;
     function GetTimeZoneServices: IExTimeZoneServices; virtual;
     procedure Changed; virtual;
     procedure Changing; virtual;
     procedure InternalSetAsString(Value: AnsiString); virtual;
     function SQLData: PByte; virtual; abstract;
     function GetDataLength: cardinal; virtual; abstract;
     function GetCodePage: TSystemCodePage; virtual; abstract;
     function getCharSetID: cardinal; virtual; abstract;
     procedure SetScale(aValue: integer); virtual;
     procedure SetDataLength(len: cardinal); virtual;
     procedure SetSQLType(aValue: cardinal); virtual;
     property DataLength: cardinal read GetDataLength write SetDataLength;
     property FirebirdClientAPI: TFBClientAPI read FFirebirdClientAPI;
  public
     constructor Create(api: TFBClientAPI);
     function CanChangeMetaData: boolean; virtual;
     function GetSQLType: cardinal; virtual; abstract; {Current Field Data SQL Type}
     function GetSQLTypeName: AnsiString; overload;
     class function GetSQLTypeName(SQLType: cardinal): AnsiString; overload;
     function GetStrDataLength: short;
     function getColMetadata: IParamMetaData; virtual; abstract;
     function GetName: AnsiString; virtual; abstract;
     function GetScale: integer; virtual; abstract; {Current Field Data scale}
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
     function GetAsNumeric: IFBNumeric;
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
     procedure SetAsNumeric(Value: IFBNumeric); virtual;
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
    function GetAttachment: IAttachment; virtual;
    function GetTransaction: ITransaction; virtual;
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
    function CanChangeMetaData: boolean; virtual; abstract;
    property Count: integer read GetCount;
    property Column[index: integer]: TSQLVarData read GetColumn; default;
    property UniqueRelationName: AnsiString read FUniqueRelationName;
    property Statement: IStatement read GetStatement;
    property Attachment: IAttachment read GetAttachment;
    property PrepareSeqNo: integer read GetPrepareSeqNo;
    property Transaction: ITransaction read GetTransaction;
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
    FColMetaData: IParamMetaData;
    function GetStatement: IStatement;
    procedure SetName(AValue: AnsiString);
  protected
    FArrayIntf: IArray;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    function GetSQLType: cardinal; virtual; abstract;
    function GetSubtype: integer; virtual; abstract;
    function GetAliasName: AnsiString;  virtual; abstract;
    function GetFieldName: AnsiString; virtual; abstract;
    function GetOwnerName: AnsiString;  virtual; abstract;
    function GetRelationName: AnsiString;  virtual; abstract;
    function GetScale: integer; virtual; abstract;
    function GetCharSetID: cardinal; virtual; abstract;
    function GetCharSetWidth: integer;
    function GetCodePage: TSystemCodePage;
    function GetIsNull: Boolean;   virtual; abstract;
    function GetIsNullable: boolean; virtual; abstract;
    function GetSQLData: PByte;  virtual; abstract;
    function GetDataLength: cardinal; virtual; abstract; {current field length}
    function GetSize: cardinal; virtual; abstract; {field length as given by metadata}
    function GetDefaultTextSQLType: cardinal; virtual; abstract;
    procedure InternalSetSQLType(aValue: cardinal; aSubType: integer); virtual; abstract;
    procedure InternalSetScale(aValue: integer); virtual; abstract;
    procedure InternalSetDataLength(len: cardinal); virtual; abstract;
    procedure SetIsNull(Value: Boolean); virtual; abstract;
    procedure SetIsNullable(Value: Boolean);  virtual; abstract;
    procedure SetSQLData(AValue: PByte; len: cardinal); virtual; abstract;
    procedure SetScale(aValue: integer);
    procedure SetDataLength(len: cardinal);
    procedure SetSQLType(aValue: cardinal; aSubType: integer);
    procedure SetCharSetID(aValue: cardinal); virtual; abstract;
    procedure SetMetaSize(aValue: cardinal); virtual;
  public
    constructor Create(aParent: TSQLDataArea; aIndex: integer);
    function CanChangeMetaData: boolean;
    procedure SetString(aValue: AnsiString);
    procedure Changed; virtual;
    procedure RowChange; virtual;
    function GetAsArray: IArray; virtual; abstract;
    function GetAsBlob(Blob_ID: TISC_QUAD; BPB: IBPB): IBlob; virtual; abstract;
    function CreateBlob: IBlob; virtual; abstract;
    function GetArrayMetaData: IArrayMetaData; virtual; abstract;
    function GetBlobMetaData: IBlobMetaData; virtual; abstract;
    function getColMetadata: IParamMetaData;
    procedure Initialize; virtual;
    procedure SaveMetaData;
    procedure SetArray(AValue: IArray);

  public
    property AliasName: AnsiString read GetAliasName;
    property FieldName: AnsiString read GetFieldName;
    property OwnerName: AnsiString read GetOwnerName;
    property RelationName: AnsiString read GetRelationName;
    property Parent: TSQLDataArea read FParent;
    property Index: integer read FIndex;
    property Name: AnsiString read FName write SetName;
    property CharSetID: cardinal read GetCharSetID write SetCharSetID;
    property CodePage: TSystemCodePage read GetCodePage;
    property SQLType: cardinal read GetSQLType;
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

  TColumnMetaData = class(TSQLDataItem,IColumnMetaData,IParamMetaData)
  private
    FIBXSQLVAR: TSQLVarData;
    FOwner: IUnknown;         {Keep reference to ensure Metadata/statement not discarded}
    FPrepareSeqNo: integer;
    FChangeSeqNo: integer;
  protected
    procedure CheckActive; override;
    function SQLData: PByte; override;
    function GetDataLength: cardinal; override;
    function GetCodePage: TSystemCodePage; override;

  public
    constructor Create(aOwner: IUnknown; aIBXSQLVAR: TSQLVarData);
    destructor Destroy; override;
    function GetSQLDialect: integer; override;
    function getColMetadata: IParamMetaData; override;

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
    function GetTransaction: ITransaction; override;
    function GetAttachment: IAttachment; override;
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
  protected
    procedure CheckActive; override;
  public
    function GetIsNull: Boolean; override;
    function GetAsArray: IArray;
    function GetAsBlob: IBlob; overload;
    function GetAsBlob(BPB: IBPB): IBlob; overload;
    function GetAsString: AnsiString; override;
    property AsBlob: IBlob read GetAsBlob;
 end;

  { TSQLParamMetaData }

  TSQLParamMetaData = class(TFBInterfacedObject,IParamMetaData)
  private
    FSQLType: cardinal;
    FSQLSubType: integer;
    FScale: integer;
    FCharSetID: cardinal;
    FNullable: boolean;
    FSize: cardinal;
    FCodePage: TSystemCodePage;
  public
    constructor Create(src: TSQLVarData);
    {IParamMetaData}
    function GetSQLType: cardinal;
    function GetSQLTypeName: AnsiString;
    function getSubtype: integer;
    function getScale: integer;
    function getCharSetID: cardinal;
    function getCodePage: TSystemCodePage;
    function getIsNullable: boolean;
    function GetSize: cardinal;
    property SQLType: cardinal read GetSQLType;
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
    function CanChangeMetaData: boolean; override;
    function getColMetadata: IParamMetaData; override;
    function GetModified: boolean; override;
    function GetAsPointer: Pointer;
    function GetAsString: AnsiString; override;
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
    procedure SetAsNumeric(aValue: IFBNumeric);

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
    function ParamExists(Idx: AnsiString): boolean;
    function ByName(Idx: AnsiString): ISQLParam ; virtual;
    function GetModified: Boolean;
    function GetHasCaseSensitiveParams: Boolean;
    function GetStatement: IStatement;
    function GetTransaction: ITransaction;
    function GetAttachment: IAttachment;
    procedure Clear;
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
     function ByName(Idx: AnsiString): ISQLData; virtual;
     function FieldExists(Idx: AnsiString): boolean;
     function getSQLData(index: integer): ISQLData;
     procedure GetData(index: integer; var IsNull:boolean; var len: short; var data: PByte);
     function GetStatement: IStatement;
     function GetTransaction: ITransaction;
     function GetAttachment: IAttachment;
     procedure SetRetainInterfaces(aValue: boolean);
 end;

implementation

uses FBMessages, variants, IBUtils, FBTransaction, FBNumeric, DateUtils;

{ TSQLParamMetaData }

constructor TSQLParamMetaData.Create(src: TSQLVarData);
begin
  inherited Create;
  FSQLType := src.GetSQLType;
  FSQLSubType := src.getSubtype;
  FScale := src.GetScale;
  FCharSetID := src.getCharSetID;
  FNullable := src.GetIsNullable;
  FSize := src.GetSize;
  FCodePage := src.GetCodePage;
end;

function TSQLParamMetaData.GetSQLType: cardinal;
begin
  Result := FSQLType;
end;

function TSQLParamMetaData.GetSQLTypeName: AnsiString;
begin
  Result := TSQLDataItem.GetSQLTypeName(FSQLType);
end;

function TSQLParamMetaData.getSubtype: integer;
begin
  Result := FSQLSubType;
end;

function TSQLParamMetaData.getScale: integer;
begin
  Result := FScale;
end;

function TSQLParamMetaData.getCharSetID: cardinal;
begin
  Result := FCharSetID;
end;

function TSQLParamMetaData.getCodePage: TSystemCodePage;
begin
  Result :=  FCodePage;
end;

function TSQLParamMetaData.getIsNullable: boolean;
begin
  Result :=  FNullable;
end;

function TSQLParamMetaData.GetSize: cardinal;
begin
  Result := FSize;
end;

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

function TSQLDataArea.GetTransaction: ITransaction;
begin
  Result := GetStatement.GetTransaction;
end;

function TSQLDataArea.GetAttachment: IAttachment;
begin
  Result := GetStatement.GetAttachment;
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
   s := SafeAnsiUpperCase(Idx)
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
    FName := SafeAnsiUpperCase(AValue)
  else
    FName := AValue;
end;

function TSQLVarData.GetAttachment: IAttachment;
begin
  Result := Parent.Attachment;
end;

function TSQLVarData.GetTransaction: ITransaction;
begin
  Result := Parent.Transaction;
end;

function TSQLVarData.GetCharSetWidth: integer;
begin
  result := 1;
  GetAttachment.CharSetWidth(GetCharSetID,result);
end;

function TSQLVarData.GetCodePage: TSystemCodePage;
begin
  result := CP_NONE;
  GetAttachment.CharSetID2CodePage(GetCharSetID,result);
end;

procedure TSQLVarData.SetScale(aValue: integer);
begin
  if aValue = Scale then
    Exit;
  if not CanChangeMetaData  then
    IBError(ibxeScaleCannotBeChanged,[]);
  InternalSetScale(aValue);
end;

procedure TSQLVarData.SetDataLength(len: cardinal);
begin
  if len = DataLength then
    Exit;
  InternalSetDataLength(len);
end;

procedure TSQLVarData.SetSQLType(aValue: cardinal; aSubType: integer);
begin
  if aValue = SQLType then
    Exit;
  if not CanChangeMetaData then
    IBError(ibxeSQLTypeUnchangeable,[TSQLDataItem.GetSQLTypeName(SQLType),
                                          TSQLDataItem.GetSQLTypeName(aValue)]);
  InternalSetSQLType(aValue,aSubType);
end;

procedure TSQLVarData.SetMetaSize(aValue: cardinal);
begin
  //Ignore
end;

procedure TSQLVarData.SaveMetaData;
begin
  FColMetaData := TSQLParamMetaData.Create(self);
end;

procedure TSQLVarData.SetArray(AValue: IArray);
begin
  FArrayIntf := AValue;
end;

constructor TSQLVarData.Create(aParent: TSQLDataArea; aIndex: integer);
begin
  inherited Create;
  FParent := aParent;
  FIndex := aIndex;
  FUniqueName := true;
end;

function TSQLVarData.CanChangeMetaData: boolean;
begin
  Result := Parent.CanChangeMetaData;
end;

procedure TSQLVarData.SetString(aValue: AnsiString);
begin
  {we take full advantage here of reference counted strings. When setting a string
   value, a reference is kept in FVarString and a pointer to it placed in the
   SQLVar. This avoids string copies. Note that PAnsiChar is guaranteed to point to
   a zero byte when the string is empty, neatly avoiding a nil pointer error.}

  FVarString := aValue;
  if SQLType = SQL_BLOB then
  begin
    if (GetDefaultTextSQLType = SQL_TEXT) and not GetStatement.HasBatchMode then
      SetMetaSize(Length(aValue)*GetCharSetWidth)
    else
      SetMetaSize(GetAttachment.GetInlineBlobLimit) {Otherwise batch mode can be limited by first string assigned to var}
  end;
  if CanChangeMetaData then
    SetSQLType(GetDefaultTextSQLType,0);
  Scale := 0;
  if  (SQLType <> SQL_VARYING) and (SQLType <> SQL_TEXT) then
    IBError(ibxeUnableTosetaTextType,[Index,Name,TSQLDataItem.GetSQLTypeName(SQLType)]);
  if not CanChangeMetaData and (Length(aValue) > GetSize) then
    IBError(ibxeStringOverflow,[Length(aValue),DataLength]);
  SetSQLData(PByte(PAnsiChar(FVarString)),Length(aValue));
end;

procedure TSQLVarData.Changed;
begin
  FModified := true;
end;

procedure TSQLVarData.RowChange;
begin
  FArrayIntf := nil;
  FModified := false;
  FVarString := '';
end;

function TSQLVarData.getColMetadata: IParamMetaData;
begin
  Result := FColMetaData;
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
     st := Space2Underscore(SafeAnsiUpperCase(AliasName));
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

function TSQLDataItem.CanChangeMetaData: boolean;
begin
  Result := false;
end;

function TSQLDataItem.GetSQLTypeName: AnsiString;
begin
  Result := GetSQLTypeName(GetSQLType);
end;

class function TSQLDataItem.GetSQLTypeName(SQLType: cardinal): AnsiString;
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
          result := NumericFromRawValues(Int64(PShort(SQLData)^),
                                      Scale).getAsCurrency;
        SQL_LONG:
          result := NumericFromRawValues(Int64(PLong(SQLData)^),
                                      Scale).getAsCurrency;
        SQL_INT64:
          result := NumericFromRawValues(PInt64(SQLData)^,
                                      Scale).getAsCurrency;
        SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
          result := Round(AsDouble);

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
        result := NumericFromRawValues(Int64(PShort(SQLData)^),
                                    Scale).getAsInt64;
      SQL_LONG:
        result := NumericFromRawValues(Int64(PLong(SQLData)^),
                                    Scale).getAsInt64;
      SQL_INT64:
        result := NumericFromRawValues(PInt64(SQLData)^,
                                    Scale).getAsInt64;
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := Round(AsDouble);
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
        result := NumericFromRawValues(Int64(PShort(SQLData)^),
                              Scale).getAsDouble;
      SQL_LONG:
        result := NumericFromRawValues(Int64(PLong(SQLData)^),
                              Scale).getAsDouble;
      SQL_INT64:
        result := NumericFromRawValues(PInt64(SQLData)^, Scale).getAsDouble;
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
        result := NumericFromRawValues(Int64(PShort(SQLData)^),
                                    Scale).getAsInteger;
      SQL_LONG:
        result := NumericFromRawValues(Int64(PLong(SQLData)^),
                                    Scale).getAsInteger;
      SQL_INT64:
        result := NumericFromRawValues(PInt64(SQLData)^, Scale).getAsInteger;

      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        result := Round(AsDouble);
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
begin
  Result := 0;
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
        IBError(ibxeInvalidDataConversion, [GetSQLTypeName]);
    end;
end;

function TSQLDataItem.GetAsNumeric: IFBNumeric;
var aValue: Int64;
begin
  case SQLType of
   SQL_TEXT, SQL_VARYING:
     Result := StrToNumeric(GetAsString);

   SQL_SHORT:
     Result := NumericFromRawValues(PShort(SQLData)^, Scale);

   SQL_LONG:
     Result := NumericFromRawValues(PLong(SQLData)^, Scale);

   SQL_INT64:
     Result := NumericFromRawValues(PInt64(SQLData)^, Scale);

   SQL_DEC16,
   SQL_DEC34,
   SQL_DEC_FIXED,
   SQL_INT128:
     Result := BCDToNumeric(GetAsBCD);

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
  if not CanChangeMetaData and ((SQLType <> SQL_INT64) or (Scale <> -4)) then
    SetAsNumeric(CurrToNumeric(Value))
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
  if not CanChangeMetaData and ((SQLType <> SQL_INT64) or (Scale <> 0)) then
    SetAsNumeric(IntToNumeric(Value))
  else
  begin
    Changing;
    if IsNullable then
      IsNull := False;

    SQLType := SQL_INT64;
    Scale := 0;
    DataLength := SizeOf(Int64);
    PInt64(SQLData)^ := Value;
    Changed;
  end;
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
  if not CanChangeMetaData and (SQLType <> SQL_DOUBLE) then
    SetAsNumeric(DoubleToNumeric(Value))
  else
  begin
    if IsNullable then
      IsNull := False;

    Changing;
    SQLType := SQL_DOUBLE;
    DataLength := SizeOf(Double);
    Scale := 0;
    PDouble(SQLData)^ := Value;
    Changed;
  end;
end;

procedure TSQLDataItem.SetAsFloat(Value: Float);
begin
  CheckActive;
  if not CanChangeMetaData and (SQLType <> SQL_FLOAT) then
    SetAsNumeric(DoubleToNumeric(Value))
  else
  begin
    if IsNullable then
      IsNull := False;

    Changing;
    SQLType := SQL_FLOAT;
    DataLength := SizeOf(Float);
    Scale := 0;
    PSingle(SQLData)^ := Value;
    Changed;
  end;
end;

procedure TSQLDataItem.SetAsLong(Value: Long);
begin
  CheckActive;
  if not CanChangeMetaData and ((SQLType <> SQL_LONG) or (Scale <> 0)) then
    SetAsNumeric(IntToNumeric(Value))
  else
  begin
    if IsNullable then
      IsNull := False;

    Changing;
    SQLType := SQL_LONG;
    DataLength := SizeOf(Long);
    Scale := 0;
    PLong(SQLData)^ := Value;
    Changed;
  end;
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
  if not CanChangeMetaData and ((SQLType <> SQL_SHORT) or (Scale <> 0)) then
    SetAsNumeric(IntToNumeric(Value))
  else
  begin
    Changing;
    if IsNullable then
      IsNull := False;

    SQLType := SQL_SHORT;
    DataLength := SizeOf(Short);
    Scale := 0;
    PShort(SQLData)^ := Value;
    Changed;
  end;
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
  begin
    if VarType(Value[0]) in [varEmpty, varNull] then
      IsNull := true
    else
      SetAsDateTime(Value[0],AnsiString(Value[1]))
  end
  else case VarType(Value) of
    varEmpty, varNull:
      IsNull := True;
    varSmallint, varInteger, varByte, varLongWord,
      varWord, varShortInt, varInt64:
        SetAsNumeric(IntToNumeric(Int64(Value)));
    varSingle, varDouble:
      AsDouble := Value;
    varCurrency:
      SetAsNumeric(CurrToNumeric(Currency(Value)));
    varBoolean:
      AsBoolean := Value;
    varDate:
      AsDateTime := Value;
    varOleStr, varString {$if declared(varUString)}, varUString {$ifend}:
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

procedure TSQLDataItem.SetAsNumeric(Value: IFBNumeric);
begin
  CheckActive;
  Changing;
  if IsNullable then
    IsNull := False;

  if CanChangeMetadata then
  begin
    {Restore original values}
    SQLType := getColMetadata.GetSQLType;
    Scale := getColMetadata.getScale;
    SetDataLength(getColMetadata.GetSize);
  end;

  with FFirebirdClientAPI do
  case GetSQLType of
  SQL_LONG:
      PLong(SQLData)^ := SafeInteger(Value.AdjustScaleTo(Scale).getRawValue);
  SQL_SHORT:
    PShort(SQLData)^ := SafeSmallInt(Value.AdjustScaleTo(Scale).getRawValue);
  SQL_INT64:
    PInt64(SQLData)^ := Value.AdjustScaleTo(Scale).getRawValue;
  SQL_TEXT, SQL_VARYING:
   SetAsString(Value.getAsString);
  SQL_D_FLOAT,
  SQL_DOUBLE:
    PDouble(SQLData)^ := Value.getAsDouble;
  SQL_FLOAT:
    PSingle(SQLData)^ := Value.getAsDouble;
  SQL_DEC_FIXED,
  SQL_DEC16,
  SQL_DEC34:
     SQLDecFloatEncode(Value.getAsBCD,SQLType,SQLData);
  SQL_INT128:
    StrToInt128(Scale,Value.getAsString,SQLData);
  else
    IBError(ibxeInvalidDataConversion, [nil]);
  end;
  Changed;
end;

procedure TSQLDataItem.SetAsBcd(aValue: tBCD);
begin
  CheckActive;
  Changing;
  if IsNullable then
    IsNull := False;

  if not CanChangeMetaData then
  begin
    SetAsNumeric(BCDToNumeric(aValue));
    Exit;
  end;

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
  Result := FIBXSQLVAR.GetAttachment;
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
  inherited Create(aIBXSQLVAR.GetAttachment.getFirebirdAPI as TFBClientAPI);
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
  Result := FIBXSQLVAR.GetAttachment.GetSQLDialect;
end;

function TColumnMetaData.getColMetadata: IParamMetaData;
begin
  Result := self;
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
  Result := FIBXSQLVAR.Name;
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
  Result := FIBXSQLVAR.GetTransaction;
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
  result := FIBXSQLVAR.GetAsArray;
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
  FIBXSQLVar.SetString(TransliterateToCodePage(Value,GetCodePage));
  Changed;
end;

var b: IBlob;
    dt: TDateTime;
    timezone: AnsiString;
    Int64Value: Int64;
    BCDValue: TBCD;
    aScale: integer;
begin
  CheckActive;
  Clear;
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
    if (FIBXSQLVar.GetDefaultTextSQLType = SQL_TEXT) and
         (Length(Value) * GetCharSetWidth < GetAttachment.GetInlineBlobLimit)  then
      DoSetString
    else
    if Length(Value) < GetAttachment.GetInlineBlobLimit then
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
    if TryStrToNumeric(Value,Int64Value,aScale) then
      SetAsNumeric(NumericFromRawValues(Int64Value,aScale))
    else
      DoSetString;

  SQL_DEC_FIXED,
  SQL_DEC16,
  SQL_DEC34,
  SQL_INT128:
    if TryStrToBCD(Value,BCDValue) then
      SetAsNumeric(BCDToNumeric(BCDValue))
    else
      DoSetString;

  SQL_D_FLOAT,
  SQL_DOUBLE,
  SQL_FLOAT:
    if TryStrToNumeric(Value,Int64Value,aScale) then
      SetAsNumeric(NumericFromRawValues(Int64Value,AScale))
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

  SQL_TIMESTAMP_TZ,
  SQL_TIMESTAMP_TZ_EX:
      if ParseDateTimeTZString(value,dt,timezone) then
        SetAsDateTime(dt,timezone)
      else
        DoSetString;

  SQL_TIME_TZ,
  SQL_TIME_TZ_EX:
      if ParseDateTimeTZString(value,dt,timezone,true) then
        SetAsTime(dt,GetAttachment.GetTimeZoneServices.GetTimeTZDate,timezone)
      else
        DoSetString;

  else
    IBError(ibxeInvalidDataConversion,[GetSQLTypeName(getColMetaData.SQLTYPE)]);
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
  FIBXSQLVAR.SetSQLType(aValue,0);
end;

procedure TSQLParam.Clear;
const
      EmptyQuad: TISC_QUAD = (gds_quad_high:0;gds_quad_low:0);
begin
  {Restores the original SQL Type - if it was changed}
  if CanChangeMetaData then
  begin
    FIBXSQLVar.SetSQLType(getColMetadata.GetSQLType,getColMetadata.getSubtype);
    case SQLType of
    SQL_BLOB,
    SQL_ARRAY,
    SQL_QUAD:
      FIBXSQLVar.SetMetaSize(sizeof(TISC_QUAD));
    end;
  end;
  if IsNullable then
    IsNull := true
  else
  case SQLTYPE of
  SQL_BOOLEAN:
      AsBoolean := false;

  SQL_BLOB,
  SQL_ARRAY,
  SQL_QUAD:
    AsQuad := EmptyQuad;

  SQL_VARYING,
  SQL_TEXT:
    FIBXSQLVar.SetString('');

  SQL_SHORT,
  SQL_LONG,
  SQL_INT64,
  SQL_DEC_FIXED,
  SQL_DEC16,
  SQL_DEC34,
  SQL_INT128,
  SQL_D_FLOAT,
  SQL_DOUBLE,
  SQL_FLOAT:
    SetAsNumeric(IntToNumeric(0));

  SQL_TIMESTAMP:
        SetAsDateTime(0);

  SQL_TYPE_DATE:
        SetAsDate(0);

  SQL_TYPE_TIME:
        SetAsTime(0);

  SQL_TIMESTAMP_TZ,
  SQL_TIMESTAMP_TZ_EX:
        SetAsDateTime(0,'');

  SQL_TIME_TZ,
  SQL_TIME_TZ_EX:
        SetAsTime(0,0,'');
  end;
end;

function TSQLParam.CanChangeMetaData: boolean;
begin
  Result := FIBXSQLVAR.CanChangeMetaData;
end;

function TSQLParam.getColMetadata: IParamMetaData;
begin
  Result := FIBXSQLVAR.getColMetadata;
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

function TSQLParam.GetAsString: AnsiString;
var rs: RawByteString;
begin
  Result := '';
  if (SQLType = SQL_VARYING) and not IsNull then
  {SQLData points to start of string - default is to length word}
  begin
    CheckActive;
    SetString(rs,PAnsiChar(SQLData),DataLength);
    SetCodePage(rs,GetCodePage,false);
    Result := rs;
  end
  else
    Result := inherited GetAsString;
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

  FIBXSQLVAR.SetArray(anArray); {save array interface}
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

procedure TSQLParam.SetAsNumeric(aValue: IFBNumeric);
var i: integer;
    OldSQLVar: TSQLVarData;
begin
  if FIBXSQLVAR.UniqueName then
    inherited SetAsNumeric(AValue)
  else
  with FIBXSQLVAR.Parent do
  begin
    for i := 0 to Count - 1 do
      if Column[i].Name = Name then
      begin
        OldSQLVar := FIBXSQLVAR;
        FIBXSQLVAR := Column[i];
        try
          inherited SetAsNumeric(AValue);
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
  if FStatement <> nil then
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
  if FStatement <> nil then
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

function TSQLParams.ParamExists(Idx: AnsiString): boolean;
begin
  CheckActive;
  Result := FSQLParams.ColumnByName(Idx) <> nil;
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

function TSQLParams.GetStatement: IStatement;
begin
  Result := FSQLParams.GetStatement;
end;

function TSQLParams.GetTransaction: ITransaction;
begin
  Result := FSQLParams.GetTransaction;
end;

function TSQLParams.GetAttachment: IAttachment;
begin
  Result := FSQLParams.GetAttachment;
end;

procedure TSQLParams.Clear;
var i: integer;
begin
  for i := 0 to getCount - 1 do
    getSQLParam(i).Clear;
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
  begin
    col := TIBSQLData.Create(self,aIBXSQLVAR);
    AddInterface(aIBXSQLVAR.Index, col);
  end
  else
    col := TIBSQLData(GetInterface(aIBXSQLVAR.Index));
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

function TResults.FieldExists(Idx: AnsiString): boolean;
begin
  Result :=  FResults.ColumnByName(Idx) <> nil;
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
  if FResults.CheckStatementStatus(ssBOF) then
    IBError(ibxeBOF,[nil]);
  if FResults.CheckStatementStatus(ssEOF) then
    IBError(ibxeEOF,[nil]);
  if (index < 0) or (index >= FResults.Count) then
    IBError(ibxeInvalidColumnIndex,[nil]);

  FResults.GetData(index,IsNull, len,data);
end;

function TResults.GetStatement: IStatement;
begin
  Result := FStatement;
end;

function TResults.GetTransaction: ITransaction;
begin
  Result := FResults.GetTransaction;
end;

function TResults.GetAttachment: IAttachment;
begin
  Result := FResults.GetAttachment;
end;

procedure TResults.SetRetainInterfaces(aValue: boolean);
begin
  RetainInterfaces := aValue;
end;

end.

