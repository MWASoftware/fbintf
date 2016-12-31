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

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage UTF8}
{$interfaces COM}
{$IF FPC_FULLVERSION < 30000 }
{$ERROR FPC Version 3.0.0 or later is required}
{$ENDIF}
{$ENDIF}

{$DEFINE USEFIREBIRD3API}
{$DEFINE USELEGACYFIREBIRDAPI}

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
  Classes, SysUtils, DB, FBMessages, IBExternals;

const
  {Interface version information}
  FBIntf_Major = 1;
  FBIntf_Minor = 0;
  FBIntf_Release = 1;
  FBIntf_Version = '1.0.1';

{These include files are converted from the 'C' originals in the Firebird API
 and define the various constants used by the API}

{$I consts_pub.inc}
{$I inf_pub.inc}
{$I configkeys.inc}

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

  TIBSQLStatementTypes =
                 (SQLUnknown, SQLSelect, SQLInsert,
                  SQLUpdate, SQLDelete, SQLDDL,
                  SQLGetSegment, SQLPutSegment,
                  SQLExecProcedure, SQLStartTransaction,
                  SQLCommit, SQLRollback,
                  SQLSelectForUpdate, SQLSetGenerator);

  TFBStatusCode = cardinal;
  TByteArray = array of byte;

  IAttachment = interface;
  ITransaction = interface;

  {The IParameterBlock generic interface provides the template for all parameter
   block interfaces}

  generic IParameterBlock<_IItem> = interface
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
    function getParamType: byte;
    function getAsInteger: integer;
    function getAsString: string;
    function getAsByte: byte;
    procedure setAsString(aValue: string);
    procedure setAsByte(aValue: byte);
    procedure SetAsInteger(aValue: integer);
    property AsString: string read getAsString write setAsString;
    property AsByte: byte read getAsByte write setAsByte;
    property AsInteger: integer read getAsInteger write SetAsInteger;
  end;


  {The IStatus interface provides access to error information, if any, returned
   by the last API call. It can also be used to customise the error message
   returned by a database engine exception - see EIBInterbaseError.

   This interface can be accessed from IFirebirdAPI.
   }

  IStatus = interface
    function GetIBErrorCode: Long;
    function Getsqlcode: Long;
    function GetMessage: string;
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
    function GetSQLType: cardinal;
    function GetSQLTypeName: string;
    function GetScale: integer;
    function GetSize: cardinal;
    function GetCharSetID: cardinal;
    function GetTableName: string;
    function GetColumnName: string;
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
    function GetAsString(index: array of integer): String;
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
    procedure SetAsString(index: array of integer; Value: String);
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
    function GetSubType: integer;
    function GetCharSetID: cardinal;
    function GetCodePage: TSystemCodePage;
    function GetSegmentSize: cardinal;
    function GetRelationName: string;
    function GetColumnName: string;
  end;

  {The Blob Parameter block is used to select a Blob Filter}

  IBPBItem = interface (IParameterBlockItem) end;

  IBPB = specialize IParameterBlock<IBPBItem>;

  { The Blob Interface provides access to a blob data item.

  The interface is returned by a GetAsBlob getter method (see ISQLData). A new Blob
  can be obtained from the IAttachment interface. The SetAsBlob setter method
  (See ISQLParam) is used to apply an updated or new array to the database using
  an UPDATE or INSERT statement.
  }

  TFBBlobMode = (fbmRead,fbmWrite);
  TBlobType = (btSegmented,btStream);

  IBlob = interface(IBlobMetaData)
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
    function LoadFromFile(Filename: string): IBlob;
    function LoadFromStream(S: TStream) : IBlob;
    function SaveToFile(Filename: string): IBlob;
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

  { IColumnMetaData }

  IColumnMetaData = interface
    function GetIndex: integer;
    function GetSQLType: cardinal;
    function GetSQLTypeName: string;
    function getSubtype: integer;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if no alias}
    function getName: string;       {Disambiguated uppercase Field Name}
    function getScale: integer;
    function getCharSetID: cardinal;
    function getCodePage: TSystemCodePage;
    function getIsNullable: boolean;
    function GetSize: cardinal;
    function GetArrayMetaData: IArrayMetaData; {Valid only for Array SQL Type}
    function GetBlobMetaData: IBlobMetaData; {Valid only for Blob SQL Type}
    property Name: string read GetName;
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
    function getCount: integer;
    function getColumnMetaData(index: integer): IColumnMetaData;
    function GetUniqueRelationName: string; {Non empty if all columns come from the same table}
    function ByName(Idx: String): IColumnMetaData;
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
    function GetAsString: String;
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
    property AsString: String read GetAsString;
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
   function getCount: integer;
   function GetTransaction: ITransaction;
   function ByName(Idx: String): ISQLData;
   function getSQLData(index: integer): ISQLData;
   procedure GetData(index: integer; var IsNull:boolean; var len: short; var data: PChar);
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
    function FetchNext: boolean;
    function GetCursorName: string;
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
    function GetIndex: integer;
    function GetSQLType: cardinal;
    function GetSQLTypeName: string;
    function getSubtype: integer;
    function getName: string;
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
    function GetAsString: String;
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
    procedure SetAsString(aValue: String);
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
    property AsString: String read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsBlob: IBlob read GetAsBlob write SetAsBlob;
    property AsArray: IArray read GetAsArray write SetAsArray;
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property Value: Variant read GetAsVariant write SetAsVariant;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsNullable: Boolean read GetIsNullable;
    property Modified: Boolean read getModified;
    property Name: string read GetName;
    property SQLType: cardinal read GetSQLType;
  end;

   {
   The ISQLParams interface provides access to the collection of parameters used
   for the input to an SQL Statement
  }

  ISQLParams = interface
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ByName(Idx: String): ISQLParam ;
    function GetModified: Boolean;
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
    function GetMetaData: IMetaData;  {Output Metadata}
    function GetSQLParams: ISQLParams;{Statement Parameters}
    function GetPlan: String;
    function GetRowsAffected(var SelectCount, InsertCount, UpdateCount, DeleteCount: integer): boolean;
    function GetSQLStatementType: TIBSQLStatementTypes;
    function GetSQLText: string;
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

  ITPBItem = interface(IParameterBlockItem) end;

  ITPB = specialize IParameterBlock<ITPBItem>;

  {The ITransactionAction interface provides access to a Transaction once it
   has been initially started. After a Commit or Rollback, a transaction
   may be restarted, optinally with a new TPB.

   A multi-database transaction is started from the FirebirdAPI. A single database
   transaction is started from the IAttachment interface.
  }

  TTransactionAction  = (TARollback, TACommit, TACommitRetaining, TARollbackRetaining);
  TTransactionCompletion = TARollback.. TACommit;

  ITransaction = interface
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
    EventName: string;
    Count: integer;
  end;

  TEventCounts = array of TEventInfo;
  IEvents = interface;
  TEventHandler = procedure(Sender: IEvents) of object;

  { IEvents }

  IEvents = interface
    procedure GetEvents(EventNames: TStrings);
    procedure SetEvents(EventNames: TStrings); overload;
    procedure SetEvents(EventName: string); overload;
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
    function getItemType: byte;
    function getSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: string;
    function getAsInteger: integer;
    procedure DecodeIDCluster(var ConnectionType: integer; var DBFileName, DBSiteName: string);
    function getAsBytes: TByteArray;
    procedure DecodeVersionString(var Version: byte; var VersionString: string);
    function getOperationCounts: TDBOperationCounts;
    procedure DecodeUserNames(UserNames: TStrings);

    {user names only}
    function GetCount: integer;
    function GetItem(index: integer): IDBInfoItem;
    function Find(ItemType: byte): IDBInfoItem;
    property AsInteger: integer read getAsInteger;
    property AsString: string read GetAsString;
    property Count: integer read GetCount;
    property Items[index: integer]: IDBInfoItem read getItem; default;
  end;

  { IDBInformation }

  IDBInformation = interface
    function GetCount: integer;
    function GetItem(index: integer): IDBInfoItem;
    function Find(ItemType: byte): IDBInfoItem;
    procedure PrintBuf; {can be used to print buffer in hex for debugging}
    property Count: integer read GetCount;
    property Items[index: integer]: IDBInfoItem read getItem; default;
  end;

  {The Database Parameter Block (DPB).

   The DPB provides the parameters used when connecting to a database. It is allocated
   empty by the FirebirdAPI and the parameters are then added to it. Each individual
   parameter may be accessed by the IDPBItem interface which can be used to set the
   value, if any, of the parameter.

   The DPB parameters, and the associated symbolic codes and parameter values may be
   found in the Interbase 6.0 API Guide.
   }

  IDPBItem = interface(IParameterBlockItem) end;

  IDPB = specialize IParameterBlock<IDPBItem>;

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
    function getDPB: IDPB;
    function AllocateBPB: IBPB;
    procedure Connect;
    procedure Disconnect(Force: boolean=false);
    function IsConnected: boolean;
    procedure DropDatabase;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionCompletion=taCommit): ITransaction; overload;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionCompletion=taCommit): ITransaction; overload;
    procedure ExecImmediate(transaction: ITransaction; sql: string; SQLDialect: integer); overload;
    procedure ExecImmediate(TPB: array of byte; sql: string; SQLDialect: integer); overload;
    procedure ExecImmediate(transaction: ITransaction; sql: string); overload;
    procedure ExecImmediate(TPB: array of byte; sql: string); overload;
    function ExecuteSQL(TPB: array of byte; sql: string; SQLDialect: integer; params: array of const): IResults; overload;
    function ExecuteSQL(transaction: ITransaction; sql: string; SQLDialect: integer; params: array of const): IResults; overload;
    function ExecuteSQL(TPB: array of byte; sql: string; params: array of const): IResults; overload;
    function ExecuteSQL(transaction: ITransaction; sql: string; params: array of const): IResults; overload;
    function OpenCursor(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: string; aSQLDialect: integer;
                             params: array of const): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: string): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: string;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string; aSQLDialect: integer;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(sql: string): IResultSet; overload;
    function OpenCursorAtStart(sql: string;
                             params: array of const): IResultSet; overload;
    function Prepare(transaction: ITransaction; sql: string; aSQLDialect: integer): IStatement; overload;
    function Prepare(transaction: ITransaction; sql: string): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       aSQLDialect: integer; GenerateParamNames: boolean=false): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       GenerateParamNames: boolean=false): IStatement; overload;

    {Events}
    function GetEventHandler(Events: TStrings): IEvents; overload;
    function GetEventHandler(Event: string): IEvents; overload;

    {Blob - may use to open existing Blobs. However, ISQLData.AsBlob is preferred}

    function CreateBlob(transaction: ITransaction; RelationName, ColumnName: string; BPB: IBPB=nil): IBlob; overload;
    function CreateBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BPB: IBPB=nil): IBlob; overload;
    function CreateBlob(transaction: ITransaction; SubType: integer; CharSetID: cardinal=0; BPB: IBPB=nil): IBlob; overload;
    function OpenBlob(transaction: ITransaction; RelationName, ColumnName: string; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob;

    {Array - may use to open existing arrays. However, ISQLData.AsArray is preferred}

    function OpenArray(transaction: ITransaction; RelationName, ColumnName: string; ArrayID: TISC_QUAD): IArray;
    function CreateArray(transaction: ITransaction; RelationName, ColumnName: string): IArray; overload;
    function CreateArray(transaction: ITransaction; ArrayMetaData: IArrayMetaData): IArray; overload;
    function CreateArrayMetaData(SQLType: cardinal; tableName: string; columnName: string;
                  Scale: integer; size: cardinal; charSetID: cardinal; dimensions: cardinal;
                  bounds: TArrayBounds): IArrayMetaData;

    {Database Information}
    function GetSQLDialect: integer;
    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: string): IBlobMetaData;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: string): IArrayMetaData;
    function GetDBInformation(Requests: array of byte): IDBInformation; overload;
    function GetDBInformation(Request: byte): IDBInformation; overload;
    function HasActivity: boolean;
  end;

  TProtocol = (TCP, SPX, NamedPipe, Local);

  {Service Parameter Block (SPB).

  The SPB provides the parameters used when connecting to a Service Manager. It is
  allocated empty by the FirebirdAPI and the parameters are then added to it. Each
  individual parameter may be accessed by the ISPBItem interface which can be used
  to set the value, if any, of the parameter.

  The SPB parameters, and the associated symbolic codes and parameter values may be
  found in the Interbase 6.0 API Guide.

  }

  ISPBItem = interface(IParameterBlockItem) end;

  ISPB = specialize IParameterBlock<ISPBItem>;

  {Service Query Parameter Block (SQPB).

   This is a specialised parameter block used to send data to a service manager
   in a Query Request.
  }

  ISQPBItem = interface(IParameterBlockItem)
    function CopyFrom(source: TStream; count: integer): integer;
  end;

  ISQPB = specialize IParameterBlock<ISQPBItem>;

  {Service Request Block (SRB).

   The SRB specifies what is requested from the Service Manager when starting a
   service or querying a service. It is allocated  empty by the ServiceManager API and
   the parameters are then added to it. Each individual parameter may be accessed
   by the ISRBItem interface which can be used to set the  value, if any, of the parameter.

   The SRB parameters, and the associated symbolic codes and parameter values may be
   found in the Interbase 6.0 API Guide.

  }

  ISRBItem = interface(IParameterBlockItem) end;

  ISRB = specialize IParameterBlock<ISRBItem>;

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
    function getItemType: byte;
    function getSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: string;
    function getAsInteger: integer;
    function getAsByte: byte;
    function CopyTo(stream: TStream; count: integer): integer;
    property AsString: string read getAsString;
    property AsInteger: integer read getAsInteger;
    property AsByte: byte read getAsByte;
  end;

  IServiceQueryResultItem = interface(IServiceQueryResultSubItem)
    function getCount: integer;
    function getItem(index: integer): IServiceQueryResultSubItem;
    function find(ItemType: byte): IServiceQueryResultSubItem;
    property Items[index: integer]: IServiceQueryResultSubItem read getItem; default;
    property Count: integer read getCount;
  end;

  IServiceQueryResults = interface
    function getCount: integer;
    function getItem(index: integer): IServiceQueryResultItem;
    function find(ItemType: byte): IServiceQueryResultItem;
    procedure PrintBuf; {can be used to print buffer in hex for debugging}
    property Items[index: integer]: IServiceQueryResultItem read getItem; default;
    property Count: integer read getCount;
  end;

  {The IServiceManager interface provides access to a service manager. It can
   used to Detach and re-attach to Service Manager, to start services and to
   query the service manager.

   The interface is returned by the FirebirdAPI GetService Manager method.
  }

  { IServiceManager }

  IServiceManager = interface
    function getSPB: ISPB;
    function getServerName: string;
    procedure Attach;
    procedure Detach(Force: boolean=false);
    function IsAttached: boolean;
    function AllocateSRB: ISRB;
    function AllocateSQPB: ISQPB;
    procedure Start(Request: ISRB);
    function Query(SQPB: ISQPB; Request: ISRB) :IServiceQueryResults; overload;
    function Query(Request: ISRB) :IServiceQueryResults; overload;
  end;

  {The Firebird API.

   This is the base interface and is used to create/open a database connection, to
   start a transaction on multiple databases and the access the service manager.

   The interface is returned by the FirebirdAPI function.
  }

  IFirebirdAPI = interface
    {Database connections}
    function AllocateDPB: IDPB;
    function OpenDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnConnectError: boolean=true): IAttachment;
    function CreateDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnError: boolean=true): IAttachment; overload;
    function CreateDatabase(sql: string; aSQLDialect: integer; RaiseExceptionOnError: boolean=true): IAttachment; overload;

    {Start Transaction against multiple databases}
    function AllocateTPB: ITPB;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: array of byte; DefaultCompletion: TTransactionCompletion=taCommit): ITransaction; overload;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: ITPB; DefaultCompletion: TTransactionCompletion=taCommit): ITransaction; overload;

    {Service Manager}
    function HasServiceAPI: boolean;
    function AllocateSPB: ISPB;
    function GetServiceManager(ServerName: string; Protocol: TProtocol; SPB: ISPB): IServiceManager;

    {Information}
    function GetStatus: IStatus;
    function GetLibraryName: string;
    function HasRollbackRetaining: boolean;
    function IsEmbeddedServer: boolean;
    function GetImplementationVersion: string;

    {Firebird 3 API}
    function HasMasterIntf: boolean;
    function GetIMaster: TObject;

    {utility}
    function GetCharsetName(CharSetID: integer): string;
    function CharSetID2CodePage(CharSetID: integer; var CodePage: TSystemCodePage): boolean;
    function CodePage2CharSetID(CodePage: TSystemCodePage; var CharSetID: integer): boolean;
    function CharSetName2CharSetID(CharSetName: string; var CharSetID: integer): boolean;
    function CharSetWidth(CharSetID: integer; var Width: integer): boolean;
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
     constructor Create(ASQLCode: Long; Msg: string);
     property SQLCode: Long read FSQLCode;
   end;

   { EIBInterBaseError - Firebird Engine errors}

   EIBInterBaseError = class(EIBError)
   private
     FIBErrorCode: Long;
   public
     constructor Create(Status: IStatus); overload;
     constructor Create(ASQLCode: Long; AIBErrorCode: Long; Msg: string); overload;
     property IBErrorCode: Long read FIBErrorCode;
   end;

   {IB Client Exceptions}
   EIBClientError = class(EIBError);

{IBError is used internally and by IBX to throw an EIBClientError}

procedure IBError(ErrMess: TIBClientError; const Args: array of const);

{The Firebird API function is used to access the IFirebirdAPI interface.

 It will load the Firebird Client Library if this is not already loaded and
 select an implementation of the Firebird API (legacy 2.5 or 3.0.
}

function FirebirdAPI: IFirebirdAPI;

{IBX support functions. Probably best ignored i.e. always used the FirebirdAPI
 functino to load the library and check if it's loaded.}

function TryIBLoad: Boolean;
procedure CheckIBLoaded;

implementation

uses FBClientAPI
  {$IFDEF USELEGACYFIREBIRDAPI}, FB25ClientAPI {$ENDIF}
  {$IFDEF USEFIREBIRD3API}, FB30ClientAPI {$ENDIF};

var FFirebirdAPI: IFirebirdAPI;

function FirebirdAPI: IFirebirdAPI;
begin
  if FFirebirdAPI = nil then
    CheckIBLoaded;
  Result := FFirebirdAPI;
end;

function TryIBLoad: Boolean;
begin
 Result := FFirebirdAPI <> nil;
 try
  {$IFDEF USEFIREBIRD3API}
  if not Result then
  begin
    FFirebirdAPI := TFB30ClientAPI.Create;
    Result := FFirebirdAPI.HasMasterIntf;
  end;
  {$ENDIF}
  {$IFDEF USELEGACYFIREBIRDAPI}
  if not Result then
  begin
    FFirebirdAPI := nil;
    FFirebirdAPI := TFB25ClientAPI.Create;
    Result := true;
  end;
  {$ENDIF}
  if Result and not (FFirebirdAPI as TFBClientAPI).IsLibraryLoaded then
  begin
    Result := false;
    FFirebirdAPI := nil;
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

{ EIBError }

constructor EIBError.Create(ASQLCode: Long; Msg: string);
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
  Msg: string);
begin
  inherited Create(ASQLCode,Msg);
  FIBErrorCode := AIBErrorCode;
end;

procedure IBError(ErrMess: TIBClientError; const Args: array of const);
begin
  raise EIBClientError.Create(Ord(ErrMess),
                              Format(GetErrorMessage(ErrMess), Args));
end;

initialization
  FFirebirdAPI := nil;


end.

