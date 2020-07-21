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
unit IB;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$IF defined(FPC) and (FPC_FULLVERSION < 30000) }
{$ERROR FPC Version 3.0.0 or later is required}
{$IFEND}
{$ENDIF}

{$IFNDEF LEGACYFIREBIRDAPIONLY}
{$DEFINE USEFIREBIRD3API}
{$ENDIF}
{$IFNDEF FIREBIRD3APIONLY}
{$DEFINE USELEGACYFIREBIRDAPI}
{$ENDIF}

{
  This unit defines the interfaces used to provide the Pascal Language
  bindings for the Firebird API. These are COM style references counted interfaces
  and are automatically freed when they go out of scope.

  The interface definition is independent of the Firebird API version and two
  implementations are provided. One is for the legacy API (2.5 and earlier) and the
  other is for the new object orientated API (3.0 and later). By default, both are
  available with the 3.0 API used if it is available. Otherwise the 2.5 API is used.
  The above two defines can be used to force only one implementation by undefining
  the symbol for the unwanted API.

  Note that the FirebirdAPI function defined below is used for initial access to
  the language bindings.

  The goals of these Pascal Langauge bindings are to provide:

  1. A set of reference counted interfaces providing complete access to the Firebird API.

  2. Application Independence from the Firebird API version.

  3. All data access through strongly typed variables and functions with no need for
     the end user to manipulate untyped data in buffers such as the legacy API SQLDA
     or the Firebird 3.0 message buffer.

  4. A stable platform for LCL Packages (e.g. IBX) that implement the TDataSet model
     with independence from the Firebird API version.

  5. Straightforward progammatic access to the Firebird API from Pascal programs.

  6. FPC and Delphi Support.

  String Types
  ============

  From FPC 3.0 onwards, ANSISTRINGs include the codepage in their definition. All
  strings used by the interface are sensitive to the codepage in that the codepage
  for all strings returned by an interface is consistent with the SQL Character set
  used for the database connection. Input strings will be transliterated, where possible
  and if necessary, to the codepage consistent with the character set used for
  the database connection.
}

interface

uses
  Classes,
  {$IFDEF WINDOWS}Windows, {$ENDIF}
  {$IFDEF FPC} Dynlibs, {$ENDIF}
  SysUtils, DB, FBMessages, IBExternals;

const
  {Interface version information}
  FBIntf_Major = 1;
  FBIntf_Minor = 1;
  FBIntf_Release = 6;
  FBIntf_Version = '1.1.6';

{These include files are converted from the 'C' originals in the Firebird API
 and define the various constants used by the API}

{$I 'include/consts_pub.inc'}
{$I 'include/inf_pub.inc'}
{$I 'include/configkeys.inc'}

{The following constants define the values return by calls to the GetSQLType
 methods provided by several of the interfaces defined below.}

(*********************)
(** SQL definitions **)
(*********************)
  SQL_VARYING                    =        448;
  SQL_TEXT                       =        452;
  SQL_DOUBLE                     =        480;
  SQL_FLOAT                      =        482;
  SQL_LONG                       =        496;
  SQL_SHORT                      =        500;
  SQL_TIMESTAMP                  =        510;
  SQL_BLOB                       =        520;
  SQL_D_FLOAT                    =        530;
  SQL_ARRAY                      =        540;
  SQL_QUAD                       =        550;
  SQL_TYPE_TIME                  =        560;
  SQL_TYPE_DATE                  =        570;
  SQL_INT64                      =        580;
  SQL_BOOLEAN                    =        32764;
  SQL_DATE                       =        SQL_TIMESTAMP;

type
   TGDS_QUAD = record
     gds_quad_high      : ISC_LONG;
     gds_quad_low       : UISC_LONG;
   end;
   TGDS__QUAD           = TGDS_QUAD;
   TISC_QUAD            = TGDS_QUAD;
   PGDS_QUAD            = ^TGDS_QUAD;
   PGDS__QUAD           = ^TGDS__QUAD;
   PISC_QUAD            = ^TISC_QUAD;

{$IFNDEF FPC}
{Delphi missing definitions}
type
  TLibHandle = THandle;

const
  NilHandle = 0;
  DirectorySeparator = '\';

{Delphi only seems to define CP_UTF8 and CP_UTF16}
const
  CP_ACP     = 0;     // default to ANSI code page
  CP_OEMCP   = 1;     // default to OEM (console) code page
  CP_UTF16BE = 1201;  // unicodeFFFE
  CP_UTF7    = 65000; // utf-7
  CP_ASCII   = 20127; // us-ascii
  CP_NONE    = $FFFF; // rawbytestring encoding

{$ENDIF}

type
{$IF not declared(TSystemCodePage)}
  TSystemCodePage = word; {not defined in Delphi}
{$IFEND}

  TIBSQLStatementTypes =
                 (SQLUnknown, SQLSelect, SQLInsert,
                  SQLUpdate, SQLDelete, SQLDDL,
                  SQLGetSegment, SQLPutSegment,
                  SQLExecProcedure, SQLStartTransaction,
                  SQLCommit, SQLRollback,
                  SQLSelectForUpdate, SQLSetGenerator,
                  SQLSavePoint);

  TFBStatusCode = cardinal;
  TByteArray = array of byte;

  IFirebirdAPI = interface;
  IAttachment = interface;
  ITransaction = interface;
  IStatement = interface;

  {The IParameterBlock interface provides the template for all parameter
   block interfaces}

  IParameterBlock<_IItem> = interface
    function getCount: integer;
    function Add(ParamType: byte): _IItem;
    function getItems(index: integer): _IItem;
    function Find(ParamType: byte): _IItem;
    procedure PrintBuf; {can be used to print buffer in hex for debugging}
    property Count: integer read getCount;
    property Items[index: integer]: _IItem read getItems; default;
  end;

  {IParameterBlockItem is not used on its own but instead provides a base type for
   different parameter block items }

  IParameterBlockItem = interface
    ['{53b23f7b-abda-46a5-9aa5-07bd5e723266}']
    function getParamType: byte;
    function getAsInteger: integer;
    function getAsString: AnsiString;
    function getAsByte: byte;
    procedure setAsString(aValue: AnsiString);
    procedure setAsByte(aValue: byte);
    procedure SetAsInteger(aValue: integer);
    property AsString: AnsiString read getAsString write setAsString;
    property AsByte: byte read getAsByte write setAsByte;
    property AsInteger: integer read getAsInteger write SetAsInteger;
  end;


  {The IStatus interface provides access to error information, if any, returned
   by the last API call. It can also be used to customise the error message
   returned by a database engine exception - see EIBInterbaseError.

   This interface can be accessed from IFirebirdAPI.
   }

   TIBDataBaseErrorMessage    = (ShowSQLCode,
                                   ShowIBMessage,
                                   ShowSQLMessage);

   TIBDataBaseErrorMessages   = set of TIBDataBaseErrorMessage;

  IStatus = interface
    ['{34167722-af38-4831-b08a-93162d58ede3}']
    function GetIBErrorCode: Long;
    function Getsqlcode: Long;
    function GetMessage: AnsiString;
    function CheckStatusVector(ErrorCodes: array of TFBStatusCode): Boolean;
    function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
  end;

  { The array metadata interface provides access to the metadata used to describe
    an array column in a Firebird table.
  }

  TArrayBound = record
    UpperBound: short;
    LowerBound: short;
  end;
  TArrayBounds = array of TArrayBound;

  IArrayMetaData = interface
    ['{7dd0aea4-59af-4c2a-b958-565d5025c489}']
    function GetSQLType: cardinal;
    function GetSQLTypeName: AnsiString;
    function GetScale: integer;
    function GetSize: cardinal;
    function GetCharSetWidth: integer;
    function GetCharSetID: cardinal;
    function GetTableName: AnsiString;
    function GetColumnName: AnsiString;
    function GetDimensions: integer;
    function GetBounds: TArrayBounds;
  end;

  {The array interface provides access to and modification of the array data
   contained in an array field of a Firebird Table. The array element is
   selected by specifying its co-ordinates using an integer array. The
   getter and setter methods used should be appropriate for the type of data
   contained in the array. Automatic conversion is provided to and from strings.
   That is GetAsString and SetAsString are safe to use for sql types other than
   boolean.

   The interface is returned by a GetAsArray getter method (see ISQLData). A new array
   can be obtained from the IAttachment interface. The SetAsArray setter method
   (See ISQLParam) is used to apply an updated or new array to the database using
   an UPDATE or INSERT statement.

  }

  TArrayEventReason = (arChanging,arChanged);
  IArray = interface;
  TArrayEventHandler = procedure(Sender: IArray; Reason: TArrayEventReason) of object;

  IArray = interface(IArrayMetaData)
    ['{631c6bb1-fb49-44fb-a64a-c49859632b88}']
    function GetArrayID: TISC_QUAD;
    procedure Clear;
    function IsEmpty: boolean;
    procedure PreLoad;
    procedure CancelChanges;
    procedure SaveChanges;
    function GetAsInteger(index: array of integer): integer;
    function GetAsBoolean(index: array of integer): boolean;
    function GetAsCurrency(index: array of integer): Currency;
    function GetAsInt64(index: array of integer): Int64;
    function GetAsDateTime(index: array of integer): TDateTime;
    function GetAsDouble(index: array of integer): Double;
    function GetAsFloat(index: array of integer): Float;
    function GetAsLong(index: array of integer): Long;
    function GetAsShort(index: array of integer): Short;
    function GetAsString(index: array of integer): AnsiString;
    function GetAsVariant(index: array of integer): Variant;
    procedure SetAsInteger(index: array of integer; AValue: integer);
    procedure SetAsBoolean(index: array of integer; AValue: boolean);
    procedure SetAsCurrency(index: array of integer; Value: Currency);
    procedure SetAsInt64(index: array of integer; Value: Int64);
    procedure SetAsDate(index: array of integer; Value: TDateTime);
    procedure SetAsLong(index: array of integer; Value: Long);
    procedure SetAsTime(index: array of integer; Value: TDateTime);
    procedure SetAsDateTime(index: array of integer; Value: TDateTime);
    procedure SetAsDouble(index: array of integer; Value: Double);
    procedure SetAsFloat(index: array of integer; Value: Float);
    procedure SetAsShort(index: array of integer; Value: Short);
    procedure SetAsString(index: array of integer; Value: AnsiString);
    procedure SetAsVariant(index: array of integer; Value: Variant);
    procedure SetBounds(dim, UpperBound, LowerBound: integer);
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    procedure AddEventHandler(Handler: TArrayEventHandler);
    procedure RemoveEventHandler(Handler: TArrayEventHandler);
  end;

  { The Blob metadata interface provides access to the metadata used to describe
    a blob column in a Firebird table.
  }

  IBlobMetaData = interface
    ['{575f3c61-bb33-46a5-8975-bb7d1b6e37cc}']
    function GetSubType: integer;
    function GetCharSetID: cardinal;
    function GetCodePage: TSystemCodePage;
    function GetSegmentSize: cardinal;
    function GetRelationName: AnsiString;
    function GetColumnName: AnsiString;
  end;

  {The Blob Parameter block is used to select a Blob Filter}

  IBPBItem = interface (IParameterBlockItem)
    ['{660822a5-3114-4c16-b6cb-c1a7b2aba70d}']
  end;

  IBPB = interface (IParameterBlock<IBPBItem>)
    ['{e0cb9eb5-17f7-4416-b7d1-3cddd1dfca76}']
  end;

  { The Blob Interface provides access to a blob data item.

  The interface is returned by a GetAsBlob getter method (see ISQLData). A new Blob
  can be obtained from the IAttachment interface. The SetAsBlob setter method
  (See ISQLParam) is used to apply an updated or new array to the database using
  an UPDATE or INSERT statement.
  }

  TFBBlobMode = (fbmRead,fbmWrite);
  TBlobType = (btSegmented,btStream);

  IBlob = interface(IBlobMetaData)
    ['{3090a145-7780-442b-b15b-efd4568b8611}']
    function GetBPB: IBPB;
    procedure Cancel;
    procedure Close;
    function GetBlobID: TISC_QUAD;
    function GetBlobMode: TFBBlobMode;
    function GetBlobSize: Int64;
    procedure GetInfo(var NumSegments: Int64; var MaxSegmentSize,
                      TotalSize: Int64; var BlobType: TBlobType);
    function Read(var Buffer; Count: Longint): Longint;
    function Write(const Buffer; Count: Longint): Longint;
    function LoadFromFile(Filename: AnsiString): IBlob;
    function LoadFromStream(S: TStream) : IBlob;
    function SaveToFile(Filename: AnsiString): IBlob;
    function SaveToStream(S: TStream): IBlob;
    function GetAsString: rawbytestring;
    procedure SetAsString(aValue: rawbytestring);
    function SetString(aValue: rawbytestring): IBlob;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    property AsString: rawbytestring read GetAsString write SetAsString;
 end;

  { The IColumnMetaData interface provides access to the per column metadata for
    the output of an SQL Statement.
  }

  TIBDateTimeFormats = (dfTimestamp, {SQL TIMESTAMP}
                        dfDateTime,   {SQL DATETIME}
                        dfTime);      {SQL TIME}

  { IColumnMetaData }

  IColumnMetaData = interface
    ['{c222e6c3-53c1-469f-9e05-0a5c3ef232d8}']
    function GetIndex: integer;
    function GetSQLType: cardinal;
    function GetSQLTypeName: AnsiString;
    function getSubtype: integer;
    function getRelationName: AnsiString;
    function getOwnerName: AnsiString;
    function getSQLName: AnsiString;    {Name of the column}
    function getAliasName: AnsiString;  {Alias Name of column or Column Name if no alias}
    function getName: AnsiString;       {Disambiguated uppercase Field Name}
    function getScale: integer;
    function getCharSetID: cardinal;
    function getCodePage: TSystemCodePage;
    function getIsNullable: boolean;
    function GetSize: cardinal;
    function GetArrayMetaData: IArrayMetaData; {Valid only for Array SQL Type}
    function GetBlobMetaData: IBlobMetaData; {Valid only for Blob SQL Type}
    function GetDateTimeStrLength(DateTimeFormat: TIBDateTimeFormats): integer;
    function GetStatement: IStatement;
    function GetTransaction: ITransaction;
    property Name: AnsiString read GetName;
    property Size: cardinal read GetSize;
    property SQLType: cardinal read GetSQLType;
    property Scale: integer read getScale;
    property SQLSubtype: integer read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  end;

  {
   The IMetaData interface provides access to the set of column metadata
   for the output of an SQL Statement
  }

  { IMetaData }

  IMetaData = interface
    ['{4dafdbb6-0d36-4f1f-9c95-8b132804b965}']
    function getCount: integer;
    function getColumnMetaData(index: integer): IColumnMetaData;
    function GetUniqueRelationName: AnsiString; {Non empty if all columns come from the same table}
    function ByName(Idx: AnsiString): IColumnMetaData;
    property ColMetaData[index: integer]: IColumnMetaData read getColumnMetaData; default;
    property Count: integer read getCount;
  end;

  {
    The ISQLData interface provides access to the data returned in a field in the
    current row returned from a query or the result of an SQL Execute statement.

    It subclasses IColumnMetaData and so also provides access to the metadata
    associated with the column.

    The getter and setter methods, and the corresponding properties, provide typed
    access to the field data. The method/property used should be consistent
    with the SQL Type. Automatic conversion is provided from strings.
    That is GetAsString is safe to use for sql types other than  boolean.
  }


  ISQLData = interface(IColumnMetaData)
    ['{3f493e31-7e3f-4606-a07c-b210b9e3619d}']
    function GetStrDataLength: short;
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
    function GetAsString: AnsiString;
    function GetIsNull: Boolean;
    function GetAsVariant: Variant;
    function GetAsBlob: IBlob; overload;
    function GetAsBlob(BPB: IBPB): IBlob; overload;
    function GetAsArray: IArray;
    property AsDate: TDateTime read GetAsDateTime;
    property AsBoolean:boolean read GetAsBoolean;
    property AsTime: TDateTime read GetAsDateTime;
    property AsDateTime: TDateTime read GetAsDateTime ;
    property AsDouble: Double read GetAsDouble;
    property AsFloat: Float read GetAsFloat;
    property AsCurrency: Currency read GetAsCurrency;
    property AsInt64: Int64 read GetAsInt64 ;
    property AsInteger: Integer read GetAsLong;
    property AsLong: Long read GetAsLong;
    property AsPointer: Pointer read GetAsPointer;
    property AsQuad: TISC_QUAD read GetAsQuad;
    property AsShort: short read GetAsShort;
    property AsString: AnsiString read GetAsString;
    property AsVariant: Variant read GetAsVariant ;
    property AsBlob: IBlob read GetAsBlob;
    property AsArray: IArray read GetAsArray;
    property IsNull: Boolean read GetIsNull;
    property Value: Variant read GetAsVariant;
  end;

  { An IResults interface is returned as the result of an SQL Execute statement
    and provides access to the fields returned, if any. It is a collection of
    ISQLData interfaces which are, in turn, used to access the data returned by
    each field of the result set.
  }

  IResults = interface
    ['{e836b2bb-93d1-4bbf-a8eb-7ce535de3bb5}']
   function getCount: integer;
   function GetStatement: IStatement;
   function GetTransaction: ITransaction;
   function ByName(Idx: AnsiString): ISQLData;
   function getSQLData(index: integer): ISQLData;
   procedure GetData(index: integer; var IsNull:boolean; var len: short; var data: PByte);
   procedure SetRetainInterfaces(aValue: boolean);
   property Data[index: integer]: ISQLData read getSQLData; default;
   property Count: integer read getCount;
  end;

  { An IResultSet interface is returned as the result of an SQL Open Cursor statement
    (e.g. Select Statement)  and provides access to the fields returned, if any
    for the current row. It is a collection of ISQLData interfaces which are,
    in turn, used to access the data returned by each field of the current row.
  }
  IResultSet = interface(IResults)
    ['{0ae4979b-7857-4e8c-8918-ec6f155b51a0}']
    function FetchNext: boolean;
    function GetCursorName: AnsiString;
    function IsEof: boolean;
    procedure Close;
  end;

  {The ISQLParam interface is used to provide access to each parameter in a
   parametised SQL Statement. It subclasses IColumnMetaData and this part of
   the interface may be used to access information on the expected SQL Type, etc.

   It also subclasses ISQLData and this part of the interface may be used to access
   current values for each parameter.

   Otherwise, the interface comprises the Setter Methods and properties used to
   set the value of each parameter.

   Automatic conversion is provided to and from strings. That is GetAsString and
   SetAsString are safe to use for sql types other than boolean - provided automatic
   conversion is possible.
  }

  ISQLParam = interface
    ['{b22b4578-6d41-4807-a9a9-d2ec8d1d5a14}']
    function GetIndex: integer;
    function GetSQLType: cardinal;
    function GetSQLTypeName: AnsiString;
    function getSubtype: integer;
    function getName: AnsiString;
    function getScale: integer;
    function getCharSetID: cardinal;
    function getCodePage: TSystemCodePage;
    function getIsNullable: boolean;
    function GetSize: cardinal;
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
    function GetAsString: AnsiString;
    function GetIsNull: boolean;
    function GetAsVariant: Variant;
    function GetAsBlob: IBlob;
    function GetAsArray: IArray;
    procedure Clear;
    function GetModified: boolean;
    procedure SetAsBoolean(AValue: boolean);
    procedure SetAsCurrency(aValue: Currency);
    procedure SetAsInt64(aValue: Int64);
    procedure SetAsDate(aValue: TDateTime);
    procedure SetAsLong(aValue: Long);
    procedure SetAsTime(aValue: TDateTime);
    procedure SetAsDateTime(aValue: TDateTime);
    procedure SetAsDouble(aValue: Double);
    procedure SetAsFloat(aValue: Float);
    procedure SetAsPointer(aValue: Pointer);
    procedure SetAsShort(aValue: Short);
    procedure SetAsString(aValue: AnsiString);
    procedure SetAsVariant(aValue: Variant);
    procedure SetIsNull(aValue: Boolean);
    procedure SetAsBlob(aValue: IBlob);
    procedure SetAsArray(anArray: IArray);
    procedure SetAsQuad(aValue: TISC_QUAD);
    procedure SetCharSetID(aValue: cardinal);
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsBoolean:boolean read GetAsBoolean write SetAsBoolean;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsFloat: Float read GetAsFloat write SetAsFloat;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsInteger: Integer read GetAsLong write SetAsLong;
    property AsLong: Long read GetAsLong write SetAsLong;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsShort: Short read GetAsShort write SetAsShort;
    property AsString: AnsiString read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsBlob: IBlob read GetAsBlob write SetAsBlob;
    property AsArray: IArray read GetAsArray write SetAsArray;
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property Value: Variant read GetAsVariant write SetAsVariant;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsNullable: Boolean read GetIsNullable;
    property Modified: Boolean read getModified;
    property Name: AnsiString read GetName;
    property SQLType: cardinal read GetSQLType;
  end;

   {
   The ISQLParams interface provides access to the collection of parameters used
   for the input to an SQL Statement
  }

  ISQLParams = interface
    ['{c6d95ac7-b2b7-461b-b890-afef0acbb077}']
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ByName(Idx: AnsiString): ISQLParam ;
    function GetModified: Boolean;
    function GetHasCaseSensitiveParams: Boolean;
    property Modified: Boolean read GetModified;
    property Params[index: integer]: ISQLParam read getSQLParam; default;
    property Count: integer read getCount;
  end;


  TPerfStats = (psCurrentMemory, psMaxMemory,
                psRealTime, psUserTime, psBuffers,
                psReads, psWrites, psFetches,psDeltaMemory);

  TPerfCounters = array[TPerfStats] of Int64;

  {The IStatement interface provides access to an SQL Statement once it has been
   initially prepared. The interface is returned from the IAttachment interface.
   }

  IStatement = interface
    ['{a260576d-a07d-4a66-b02d-1b72543fd7cf}']
    function GetMetaData: IMetaData;  {Output Metadata}
    function GetSQLParams: ISQLParams;{Statement Parameters}
    function GetPlan: AnsiString;
    function GetRowsAffected(var SelectCount, InsertCount, UpdateCount, DeleteCount: integer): boolean;
    function GetSQLStatementType: TIBSQLStatementTypes;
    function GetSQLText: AnsiString;
    function GetProcessedSQLText: AnsiString;
    function GetSQLDialect: integer;
    function IsPrepared: boolean;
    procedure Prepare(aTransaction: ITransaction=nil);
    function Execute(aTransaction: ITransaction=nil): IResults;
    function OpenCursor(aTransaction: ITransaction=nil): IResultSet;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    procedure SetRetainInterfaces(aValue: boolean);
    procedure EnableStatistics(aValue: boolean);
    function GetPerfStatistics(var stats: TPerfCounters): boolean;
    property MetaData: IMetaData read GetMetaData;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLStatementType: TIBSQLStatementTypes read GetSQLStatementType;
  end;

  {Transaction Parameter Block: (TPB)

   The TPB provides the parameters used when starting a transaction. It is allocated
   empty by the FirebirdAPI and the parameters are then added to it. Each individual
   parameter may be accessed by the ITPBItem interface which can be used to set the
   value, if any, of the parameter.

   The TPB parameters, and the associated symbolic codes and parameter values may be
   found in the Interbase 6.0 API Guide.
  }

  ITPBItem = interface(IParameterBlockItem)
    ['{544c1f2b-7c12-4a87-a4a5-face7ea72671}']
  end;

  ITPB = interface(IParameterBlock<ITPBItem>)
    ['{7369b0ff-defe-437b-81fe-19b211d42d25}']
  end;

  {The ITransactionAction interface provides access to a Transaction once it
   has been initially started. After a Commit or Rollback, a transaction
   may be restarted, optinally with a new TPB.

   A multi-database transaction is started from the FirebirdAPI. A single database
   transaction is started from the IAttachment interface.
  }

  TTransactionAction  = (TARollback, TACommit, TACommitRetaining, TARollbackRetaining);
  TTransactionCompletion = TARollback.. TACommit;

  ITransaction = interface
    ['{30928d0e-a9d7-4c61-b7cf-14f4f38abe2a}']
    function getTPB: ITPB;
    procedure Start(DefaultCompletion: TTransactionCompletion=taCommit);
    function GetInTransaction: boolean;
    procedure PrepareForCommit; {Two phase commit - stage 1}
    procedure Commit(Force: boolean=false);
    procedure CommitRetaining;
    function HasActivity: boolean;
    procedure Rollback(Force: boolean=false);
    procedure RollbackRetaining;
    function GetAttachmentCount: integer;
    function GetAttachment(index: integer): IAttachment;
    property InTransaction: boolean read GetInTransaction;
  end;

  { The IEvents Interface is used to handle events from a single database. The
    interface is allocated from the IAttachment Interface.

    Note that the EventHandler called when an event occurs following AsynWaitForEvent
    is called in a different thread to the calling program and TThread.Synchronize
    may be needed to pass the event back to the main thread.

    Neither AsyncWaitForEvent nor WaitForEvent is intended to be thread safe
    in a multi-threaded environment and should always be called from the main
    thread.
  }

  TEventInfo = record
    EventName: AnsiString;
    Count: integer;
  end;

  TEventCounts = array of TEventInfo;
  IEvents = interface;
  TEventHandler = procedure(Sender: IEvents) of object;

  { IEvents }

  IEvents = interface
    ['{6a0be233-ed08-4524-889c-2e45d0c20e5f}']
    procedure GetEvents(EventNames: TStrings);
    procedure SetEvents(EventNames: TStrings); overload;
    procedure SetEvents(EventName: AnsiString); overload;
    procedure Cancel;
    function ExtractEventCounts: TEventCounts;
    procedure WaitForEvent;
    procedure AsyncWaitForEvent(EventHandler: TEventHandler);
    function GetAttachment: IAttachment;
  end;

  {The IDBInformation Interface.

   An IDBInformation interface is returned by the  IAttachment GetDBInformation
   method. The interface provides access to the information requested and
   returned by the method.

   IDBInformation itself gives access to a collection of IDBInfoItems. Each one
   provides information requested, as indicated by the ItemType and the actual
   value of the information. In some cases, the returned item is itself a
   colletion of IDBInfoItems.

   The IDBInformation items, and the associated symbolic codes and parameter values may be
   found in the Interbase 6.0 API Guide.
  }

  TDBOperationCount = record
    TableID: UShort;
    Count: cardinal;
  end;

  TDBOperationCounts = array of TDBOperationCount;

  IDBInfoItem = interface
    ['{eeb97b51-ec0f-473f-9f75-c1721f055fcb}']
    function getItemType: byte;
    function getSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: AnsiString;
    function getAsInteger: integer;
    procedure DecodeIDCluster(var ConnectionType: integer; var DBFileName, DBSiteName: AnsiString);
    function getAsBytes: TByteArray;
    function getAsDateTime: TDateTime;
    procedure DecodeVersionString(var Version: byte; var VersionString: AnsiString);
    function getOperationCounts: TDBOperationCounts;
    procedure DecodeUserNames(UserNames: TStrings);

    {user names only}
    function GetCount: integer;
    function GetItem(index: integer): IDBInfoItem;
    function Find(ItemType: byte): IDBInfoItem;
    property AsInteger: integer read getAsInteger;
    property AsString: AnsiString read GetAsString;
    property Count: integer read GetCount;
    property Items[index: integer]: IDBInfoItem read getItem; default;
  end;

  { IDBInformation }

  IDBInformation = interface
    ['{7ac6777f-f0a9-498a-9f5c-4a57a554df81}']
    function GetCount: integer;
    function GetItem(index: integer): IDBInfoItem;
    function Find(ItemType: byte): IDBInfoItem;
    procedure PrintBuf; {can be used to print buffer in hex for debugging}
    property Count: integer read GetCount;
    property Items[index: integer]: IDBInfoItem read getItem; default;
  end;

  {The Database Information Request Block is used to pass requests for
   database information where at least one item requested has a parameter.
   At present, this is only fb_info_page_contents which has a single
   integer parameter.}

  IDIRBItem = interface(IParameterBlockItem)
    ['{d34a7511-8435-4a24-81a7-5103d218d234}']
  end;

  IDIRB = interface(IParameterBlock<IDIRBItem>)
    ['{1010e5ac-0a8f-403b-a302-91625e9d9579}']
  end;


  {The Database Parameter Block (DPB).

   The DPB provides the parameters used when connecting to a database. It is allocated
   empty by the FirebirdAPI and the parameters are then added to it. Each individual
   parameter may be accessed by the IDPBItem interface which can be used to set the
   value, if any, of the parameter.

   The DPB parameters, and the associated symbolic codes and parameter values may be
   found in the Interbase 6.0 API Guide.
   }

  IDPBItem = interface(IParameterBlockItem)
    ['{123d4ad0-087a-4cd1-a344-1b3d03b30673}']
  end;

  IDPB = interface(IParameterBlock<IDPBItem>)
    ['{e676067b-1cf4-4eba-9256-9724f57e0d16}']
  end;

  {The IAttachment interface provides access to a Database Connection. It may be
   used to:

   a. Disconnect and reconnect to the database.

   b. Start a Transaction on the database

   c. Execute directly SQL DDL Statements and others that return no information.

   d. OpenCursors (i.e. execute SQL Select statements and return the results)

   e. Prepare SQL Statements, returning an IStatement interface for further processing.

   f. Provide access to an SQL Event Handler.

   g. Access Database Information.

   h. Support the handling of Array and Blob data.

   Note that SQL statements can be prepared with named parameters (PSQL style).
   This then allows the parameters to be accessed by name. The same name can
   be used for more than one parameter, allowing a single operation to be used
   to set all parameters with the same name.
  }

  { IAttachment }

  IAttachment = interface
    ['{466e9b67-9def-4807-b3e7-e08a35e7185c}']
    function getFirebirdAPI: IFirebirdAPI;
    function getDPB: IDPB;
    function AllocateBPB: IBPB;
    function AllocateDIRB: IDIRB;
    procedure Connect;
    procedure Disconnect(Force: boolean=false);
    function IsConnected: boolean;
    procedure DropDatabase;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionCompletion=taCommit): ITransaction; overload;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionCompletion=taCommit): ITransaction; overload;
    procedure ExecImmediate(transaction: ITransaction; sql: AnsiString; SQLDialect: integer); overload;
    procedure ExecImmediate(TPB: array of byte; sql: AnsiString; SQLDialect: integer); overload;
    procedure ExecImmediate(transaction: ITransaction; sql: AnsiString); overload;
    procedure ExecImmediate(TPB: array of byte; sql: AnsiString); overload;
    function ExecuteSQL(TPB: array of byte; sql: AnsiString; SQLDialect: integer; params: array of const): IResults; overload;
    function ExecuteSQL(transaction: ITransaction; sql: AnsiString; SQLDialect: integer; params: array of const): IResults; overload;
    function ExecuteSQL(TPB: array of byte; sql: AnsiString; params: array of const): IResults; overload;
    function ExecuteSQL(transaction: ITransaction; sql: AnsiString; params: array of const): IResults; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer;
                             params: array of const): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(sql: AnsiString): IResultSet; overload;
    function OpenCursorAtStart(sql: AnsiString;
                             params: array of const): IResultSet; overload;
    function Prepare(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer): IStatement; overload;
    function Prepare(transaction: ITransaction; sql: AnsiString): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: AnsiString;
                       aSQLDialect: integer; GenerateParamNames: boolean=false;
                       CaseSensitiveParams: boolean = false): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: AnsiString;
                       GenerateParamNames: boolean=false;
                       CaseSensitiveParams: boolean = false): IStatement; overload;

    {Events}
    function GetEventHandler(Events: TStrings): IEvents; overload;
    function GetEventHandler(Event: AnsiString): IEvents; overload;

    {Blob - may use to open existing Blobs. However, ISQLData.AsBlob is preferred}

    function CreateBlob(transaction: ITransaction; RelationName, ColumnName: AnsiString; BPB: IBPB=nil): IBlob; overload;
    function CreateBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BPB: IBPB=nil): IBlob; overload;
    function CreateBlob(transaction: ITransaction; SubType: integer; CharSetID: cardinal=0; BPB: IBPB=nil): IBlob; overload;
    function OpenBlob(transaction: ITransaction; RelationName, ColumnName: AnsiString; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob; overload;
    function OpenBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob;  overload;

    {Array - may use to open existing arrays. However, ISQLData.AsArray is preferred}

    function OpenArray(transaction: ITransaction; RelationName, ColumnName: AnsiString; ArrayID: TISC_QUAD): IArray; overload;
    function OpenArray(transaction: ITransaction; ArrayMetaData: IArrayMetaData; ArrayID: TISC_QUAD): IArray; overload;
    function CreateArray(transaction: ITransaction; RelationName, ColumnName: AnsiString): IArray; overload;
    function CreateArray(transaction: ITransaction; ArrayMetaData: IArrayMetaData): IArray; overload;
    function CreateArrayMetaData(SQLType: cardinal; tableName: AnsiString; columnName: AnsiString;
                  Scale: integer; size: cardinal; charSetID: cardinal; dimensions: cardinal;
                  bounds: TArrayBounds): IArrayMetaData;

    {Database Information}
    function GetSQLDialect: integer;
    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: AnsiString): IBlobMetaData;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: AnsiString): IArrayMetaData;
    function GetDBInformation(Requests: array of byte): IDBInformation; overload;
    function GetDBInformation(Request: byte): IDBInformation; overload;
    function GetDBInformation(Requests: IDIRB): IDBInformation; overload;
    function GetConnectString: AnsiString;
    function GetRemoteProtocol: AnsiString;
    function GetAuthenticationMethod: AnsiString;
    function GetSecurityDatabase: AnsiString;
    function GetODSMajorVersion: integer;
    function GetODSMinorVersion: integer;
    procedure getFBVersion(version: TStrings);
    function HasActivity: boolean;

    {Character Sets}
    function HasDefaultCharSet: boolean;
    function GetDefaultCharSetID: integer;
    function GetCharsetName(CharSetID: integer): AnsiString;
    function CharSetID2CodePage(CharSetID: integer; var CodePage: TSystemCodePage): boolean;
    function CodePage2CharSetID(CodePage: TSystemCodePage; var CharSetID: integer): boolean;
    function CharSetName2CharSetID(CharSetName: AnsiString; var CharSetID: integer): boolean;
    function CharSetWidth(CharSetID: integer; var Width: integer): boolean;
    procedure RegisterCharSet(CharSetName: AnsiString; CodePage: TSystemCodePage;
      AllowReverseLookup:boolean; out CharSetID: integer);
  end;

  TProtocolAll = (TCP, SPX, NamedPipe, Local, inet, inet4, inet6, wnet, xnet, unknownProtocol);
  TProtocol = TCP..xnet;

  {Service Parameter Block (SPB).

  The SPB provides the parameters used when connecting to a Service Manager. It is
  allocated empty by the FirebirdAPI and the parameters are then added to it. Each
  individual parameter may be accessed by the ISPBItem interface which can be used
  to set the value, if any, of the parameter.

  The SPB parameters, and the associated symbolic codes and parameter values may be
  found in the Interbase 6.0 API Guide.

  }

  ISPBItem = interface(IParameterBlockItem)
    ['{5d08ae2b-4519-41bd-8b40-97cd451c3f6a}']
  end;

  ISPB = interface(IParameterBlock<ISPBItem>)
    ['{2c5836fd-41ed-4426-9b7d-5af580ec2659}']
  end;

  {Service Query Parameter Block (SQPB).

   This is a specialised parameter block used to send data to a service manager
   in a Query Request.
  }

  ISQPBItem = interface(IParameterBlockItem)
    ['{b07841a6-33b3-47f0-b5a2-028cbc86dc97}']
    function CopyFrom(source: TStream; count: integer): integer;
  end;

  ISQPB = interface(IParameterBlock<ISQPBItem>)
    ['{8553e66b-ee62-498b-8431-dff030211447}']
  end;

  {Service Request Block (SRB).

   The SRB specifies what is requested from the Service Manager when starting a
   service or querying a service. It is allocated  empty by the ServiceManager API and
   the parameters are then added to it. Each individual parameter may be accessed
   by the ISRBItem interface which can be used to set the  value, if any, of the parameter.

   The SRB parameters, and the associated symbolic codes and parameter values may be
   found in the Interbase 6.0 API Guide.

  }

  ISRBItem = interface(IParameterBlockItem)
    ['{47ec790e-f265-4b30-9dcd-261e51677245}']
   end;

  ISRB = interface(IParameterBlock<ISRBItem>)
    ['{9f2e204f-3c33-4e44-90f9-9135e95dafb9}']
  end;

  {The Service Query Results Interface.

  An IServiceQueryResults interface is returned by the IServiceManager Query
  method. The interface provides access to the information requested and
  returned by the method.

  IServiceQueryResults itself gives access to a collection of IServiceQueryResultItem.
  Each one provides information requested, as indicated by the ItemType and the actual
  value of the information. In some cases, the returned item is itself a
  collection of IServiceQueryResultSubItem.

  The IServiceQueryResultItem items, and the associated symbolic codes and parameter values may be
  found in the Interbase 6.0 API Guide.
  }

  IServiceQueryResultSubItem = interface
    ['{8a4c381e-9923-4cc9-a96b-553729248640}']
    function getItemType: byte;
    function getSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: AnsiString;
    function getAsInteger: integer;
    function getAsByte: byte;
    function CopyTo(stream: TStream; count: integer): integer;
    property AsString: AnsiString read getAsString;
    property AsInteger: integer read getAsInteger;
    property AsByte: byte read getAsByte;
  end;

  IServiceQueryResultItem = interface(IServiceQueryResultSubItem)
    ['{b2806886-206c-4024-8df9-5fe0a7630a5e}']
    function getCount: integer;
    function getItem(index: integer): IServiceQueryResultSubItem;
    function find(ItemType: byte): IServiceQueryResultSubItem;
    property Items[index: integer]: IServiceQueryResultSubItem read getItem; default;
    property Count: integer read getCount;
  end;

  IServiceQueryResults = interface
    ['{8fbbef7d-fe03-4409-828a-a787d34ef531}']
    function getCount: integer;
    function getItem(index: integer): IServiceQueryResultItem;
    function find(ItemType: byte): IServiceQueryResultItem;
    procedure PrintBuf; {can be used to print buffer in hex for debugging}
    property Items[index: integer]: IServiceQueryResultItem read getItem; default;
    property Count: integer read getCount;
  end;

  IFirebirdLibrary = interface;

  {The IServiceManager interface provides access to a service manager. It can
   used to Detach and re-attach to Service Manager, to start services and to
   query the service manager.

   The interface is returned by the FirebirdAPI GetService Manager method.
  }

  { IServiceManager }

  IServiceManager = interface
    ['{905b587d-1e1f-4e40-a3f8-a3519f852e48}']
    function getFirebirdAPI: IFirebirdAPI;
    function getSPB: ISPB;
    function getServerName: AnsiString;
    function getProtocol: TProtocol;
    function getPortNo: AnsiString;
    procedure Attach;
    procedure Detach(Force: boolean=false);
    function IsAttached: boolean;
    function AllocateSRB: ISRB;
    function AllocateSQPB: ISQPB;
    function Start(Request: ISRB; RaiseExceptionOnError: boolean=true): boolean;
    function Query(SQPB: ISQPB; Request: ISRB; RaiseExceptionOnError: boolean=true) :IServiceQueryResults; overload;
    function Query(Request: ISRB; RaiseExceptionOnError: boolean=true) :IServiceQueryResults; overload;
  end;

  {Tbe Firebird Library API used to get information about the Firebird library}


  IFirebirdLibrary = interface
    ['{3c04e0a1-12e0-428a-b2e1-bc6fcd97b79b}']
    function GetHandle: TLibHandle;
    function GetLibraryName: string;
    function GetLibraryFilePath: string;
    function GetFirebirdAPI: IFirebirdAPI;
  end;

  {The Firebird API.

   This is the base interface and is used to create/open a database connection, to
   start a transaction on multiple databases and the access the service manager.

   The interface is returned by the FirebirdAPI function.
  }

  IFirebirdAPI = interface
    ['{edeee691-c8d3-4dcf-a780-cd7e432821d5}']
    {Database connections}
    function AllocateDPB: IDPB;
    function OpenDatabase(DatabaseName: AnsiString; DPB: IDPB; RaiseExceptionOnConnectError: boolean=true): IAttachment;
    function CreateDatabase(DatabaseName: AnsiString; DPB: IDPB; RaiseExceptionOnError: boolean=true): IAttachment; overload;
    function CreateDatabase(sql: AnsiString; aSQLDialect: integer; RaiseExceptionOnError: boolean=true): IAttachment; overload;

    {Start Transaction against multiple databases}
    function AllocateTPB: ITPB;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: array of byte; DefaultCompletion: TTransactionCompletion=taCommit): ITransaction; overload;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: ITPB; DefaultCompletion: TTransactionCompletion=taCommit): ITransaction; overload;

    {Service Manager}
    function HasServiceAPI: boolean;
    function AllocateSPB: ISPB;
    function GetServiceManager(ServerName: AnsiString; Protocol: TProtocol; SPB: ISPB): IServiceManager; overload;
    function GetServiceManager(ServerName: AnsiString; Port: AnsiString; Protocol: TProtocol; SPB: ISPB): IServiceManager; overload;

    {Information}
    function GetStatus: IStatus;
    function HasRollbackRetaining: boolean;
    function IsEmbeddedServer: boolean;
    function GetImplementationVersion: AnsiString;
    function GetClientMajor: integer;
    function GetClientMinor: integer;

    {Firebird 3 API}
    function HasMasterIntf: boolean;
    function GetIMaster: TObject;
    function GetFBLibrary: IFirebirdLibrary;
end;

type
  TOnGetLibraryName = procedure(var libname: string);

const
  OnGetLibraryName: TOnGetLibraryName = nil;
  AllowUseOfFBLIB: boolean = false;

type
   { EIBError }

   EIBError = class(EDatabaseError)
   private
     FSQLCode: Long;
   public
     constructor Create(ASQLCode: Long; Msg: AnsiString);
     property SQLCode: Long read FSQLCode;
   end;

   { EIBInterBaseError - Firebird Engine errors}

   EIBInterBaseError = class(EIBError)
   private
     FIBErrorCode: Long;
   public
     constructor Create(Status: IStatus); overload;
     constructor Create(ASQLCode: Long; AIBErrorCode: Long; Msg: AnsiString); overload;
     property IBErrorCode: Long read FIBErrorCode;
   end;

   {IB Client Exceptions}
   EIBClientError = class(EIBError);

{The Firebird API function is used to access the IFirebirdAPI interface.

 It will load the Firebird Client Library if this is not already loaded and
 select an implementation of the Firebird API (legacy 2.5 or 3.0.
}

function FirebirdAPI: IFirebirdAPI;

{IBX support functions. Probably best ignored i.e. always used the FirebirdAPI
 functino to load the library and check if it's loaded.}

function TryIBLoad: Boolean;
procedure CheckIBLoaded;

{If you want to explicitly load the Firebird library from a
 non-default location then use this function and its GetFirebirdAPI function
 to get the API.}

function LoadFBLibrary(aLibPathName: string): IFirebirdLibrary;

implementation

uses FBClientAPI
  {$IFDEF USELEGACYFIREBIRDAPI}, FB25ClientAPI {$ENDIF}
  {$IFDEF USEFIREBIRD3API}, FB30ClientAPI {$ENDIF};

var FDefaultFBLibrary: IFirebirdLibrary;

type

  { TFBLibrary }

  TFBLibraryImpl = class(TFBLibrary)
  protected
    function GetFirebird3API: IFirebirdAPI; override;
    function GetLegacyFirebirdAPI: IFirebirdAPI; override;
  end;

function TFBLibraryImpl.GetFirebird3API: IFirebirdAPI;
begin
 {$IFDEF USEFIREBIRD3API}
 Result := TFB30ClientAPI.Create(self);
 {$ELSE}
 Result := nil;
 {$ENDIF}
end;

function TFBLibraryImpl.GetLegacyFirebirdAPI: IFirebirdAPI;
begin
  {$IFDEF USELEGACYFIREBIRDAPI}
  Result := TFB25ClientAPI.Create(self);
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

function FirebirdAPI: IFirebirdAPI;
begin
  if FDefaultFBLibrary = nil then
    CheckIBLoaded;
  Result := FDefaultFBLibrary.GetFirebirdAPI;
end;

function TryIBLoad: Boolean;
var fblib: IFirebirdLibrary;
begin
 Result := FDefaultFBLibrary <> nil;
 try
  if not Result then
  begin
    fblib := TFBLibraryImpl.Create;
    if (fblib <> nil) and (fblib.GetFirebirdAPI <> nil) then
      FDefaultFBLibrary := fblib;
    Result := FDefaultFBLibrary <> nil;
  end;
 except
   SysUtils.showexception(ExceptObject,ExceptAddr);
   Result := false;
 end;
end;

procedure CheckIBLoaded;
begin
  if not TryIBLoad then
    IBError(ibxeInterBaseMissing, [nil]);
end;

function LoadFBLibrary(aLibPathName: string): IFirebirdLibrary;
var fblib: IFirebirdLibrary;
begin
  if trim(aLibPathName) = '' then
  begin
    CheckIBLoaded;
    Result := FDefaultFBLibrary;
  end
  else
  begin
    fblib := TFBLibraryImpl.GetFBLibrary(aLibPathName);
    if (fblib = nil) or (fblib.GetFirebirdAPI = nil) then
      IBError(ibxeInterBaseMissing, [nil]);
    Result := fblib;
  end;
end;

{ EIBError }

constructor EIBError.Create(ASQLCode: Long; Msg: AnsiString);
begin
  inherited Create(Msg);
  FSQLCode := ASQLCode;
end;

{ EIBInterBaseError }

constructor EIBInterBaseError.Create(Status: IStatus);
begin
  inherited Create(Status.Getsqlcode,Status.GetMessage);
  FIBErrorCode := Status.GetIBErrorCode;
end;

constructor EIBInterBaseError.Create(ASQLCode: Long; AIBErrorCode: Long;
  Msg: AnsiString);
begin
  inherited Create(ASQLCode,Msg);
  FIBErrorCode := AIBErrorCode;
end;


initialization
  FDefaultFBLibrary := nil;

finalization
  FDefaultFBLibrary := nil;

end.

