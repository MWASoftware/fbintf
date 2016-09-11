unit IB;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
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
  for all strings returned by an interface are consistent with the SQL Character set
  used for the database connection. Input strings will be transliterated, where possible
  and if necessary, to the codepage consistent with the character set used for
  the database connection.
}

interface

uses
  Classes, SysUtils, DB, FBMessages, IBExternals;

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

  {The IStatus interface provides access to error information, if any, returned
   by the last API call. It can also be used to customise the error message
   returned by a database engine exception - see EIBInterbaseError.
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
  TArrayCoords = array of integer;

  IArrayMetaData = interface
    function GetSQLType: cardinal;
    function GetSQLTypeName: string;
    function GetScale: integer;
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

  IArray = interface
    function GetArrayID: TISC_QUAD;
    function GetMetaData: IArrayMetaData;
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
  end;

  { The Blob metadata interface provides access to the metadata used to describe
    a blob column in a Firebird table.
  }

  IBlobMetaData = interface
    function GetSubType: cardinal;
    function GetCharSetID: cardinal;
    function GetSegmentSize: cardinal;
    function GetTableName: string;
    function GetColumnName: string;
  end;

  { The Blob Interface provides access to a blob data item.

  The interface is returned by a GetAsBlob getter method (see ISQLData). A new Blob
  can be obtained from the IAttachment interface. The SetAsBlob setter method
  (See ISQLParam) is used to apply an updated or new array to the database using
  an UPDATE or INSERT statement.
  }

  TFBBlobMode = (fbmRead,fbmWrite);
  TBlobType = (btSegmented,btStream);

  IBlob = interface
    procedure Cancel;
    procedure Close;
    function GetBlobID: TISC_QUAD;
    function GetBlobMode: TFBBlobMode;
    function GetInfo(var NumSegments: Int64; var MaxSegmentSize,
                      TotalSize: Int64; var BlobType: TBlobType) :boolean;
    function Read(var Buffer; Count: Longint): Longint;
    function Write(const Buffer; Count: Longint): Longint;
    function LoadFromFile(Filename: string): IBlob;
    function LoadFromStream(S: TStream) : IBlob;
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(S: TStream);
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
 end;

  { The IColumnMetaData interface provides access to the per column metadata for
    the output of an SQL Statement.
  }

  { IColumnMetaData }

  IColumnMetaData = interface
    function GetIndex: integer;
    function GetSQLType: cardinal;
    function GetSQLTypeName: string;
    function getSubtype: cardinal;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if not alias}
    function getName: string;       {Disambiguated uppercase Field Name}
    function getScale: cardinal;
    function getCharSetID: cardinal;
    function getIsNullable: boolean;
    function GetSize: integer;
    function GetArrayMetaData: IArrayMetaData; {Valid only for Array SQL Type}
    function GetBlobMetaData: IBlobMetaData; {Valid only for Blob SQL Type}
    property Name: string read GetName;
    property Size: Integer read GetSize;
    property SQLType: cardinal read GetSQLType;
    property Scale: cardinal read getScale;
    property SQLSubtype: cardinal read getSubtype;
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
    function GetUniqueRelationName: string; {True if all columns come from the same table}
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
    function GetAsBlob: IBlob;
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
   function ByName(Idx: String): ISQLData;
   function getSQLData(index: integer): ISQLData;
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
    function GetTransaction: ITransaction;
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
   SetAsString are safe to use for sql types other than boolean.
  }

  ISQLParam = interface(ISQLData)
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
    property CharSetID: cardinal read GetCharSetID write SetCharSetID;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property Modified: Boolean read getModified;
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
    function CreateBlob: IBlob;   {Returns a new IBlob in the context of the statement transaction}
      {Return a new Array in the context of the statement transaction using either the metadata or column name}
    function CreateArray(column: IColumnMetaData): IArray; overload;
    function CreateArray(columnName: string): IArray;  overload;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    property MetaData: IMetaData read GetMetaData;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLStatementType: TIBSQLStatementTypes read GetSQLStatementType;
  end;

  ITPBItem = interface
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

  ITPB = interface
    function getCount: integer;
    function Add(ParamType: byte): ITPBItem;
    function getItems(index: integer): ITPBItem;
    function Find(ParamType: byte): ITPBItem;
    property Items[index: integer]: ITPBItem read getItems; default;
  end;

  TTransactionAction  = (TARollback, TACommit, TACommitRetaining, TARollbackRetaining);

  ITransaction = interface
    function getTPB: ITPB;
    procedure Start(DefaultCompletion: TTransactionAction=taCommit); overload;
    procedure Start(TPB: ITPB; DefaultCompletion: TTransactionAction=taCommit); overload;
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
    procedure SetEvents(Event: string); overload;
    procedure Cancel;
    function ExtractEventCounts: TEventCounts;
    procedure WaitForEvent;
    procedure AsyncWaitForEvent(EventHandler: TEventHandler);
    function GetAttachment: IAttachment;
  end;

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
    property Count: integer read GetCount;
    property Items[index: integer]: IDBInfoItem read getItem; default;
  end;

  IDPBItem = interface
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

  IDPB = interface
    function getCount: integer;
    function Add(ParamType: byte): IDPBItem;
    function getItems(index: integer): IDPBItem;
    function Find(ParamType: byte): IDPBItem;
    property Items[index: integer]: IDPBItem read getItems; default;
  end;

  { IAttachment }

  IAttachment = interface
    function getDPB: IDPB;
    procedure Connect;
    procedure Disconnect(Force: boolean=false);
    function IsConnected: boolean;
    procedure DropDatabase;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionAction): ITransaction; overload;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionAction): ITransaction; overload;
    procedure ExecImmediate(transaction: ITransaction; sql: string; SQLDialect: integer); overload;
    procedure ExecImmediate(TPB: array of byte; sql: string; SQLDialect: integer); overload;
    procedure ExecImmediate(transaction: ITransaction; sql: string); overload;
    procedure ExecImmediate(TPB: array of byte; sql: string); overload;
    function OpenCursor(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: string): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string; aSQLDialect: integer): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: string): IResultSet; overload;
    function Prepare(transaction: ITransaction; sql: string; aSQLDialect: integer): IStatement; overload;
    function Prepare(transaction: ITransaction; sql: string): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       aSQLDialect: integer; GenerateParamNames: boolean=false;
                       UniqueParamNames: boolean=false): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: string;
                       GenerateParamNames: boolean=false;
                       UniqueParamNames: boolean=false): IStatement; overload;

    {Events}
    function GetEventHandler(Events: TStrings): IEvents; overload;
    function GetEventHandler(Event: string): IEvents; overload;

    {Blob - may use to open existing Blobs. However, ISQLData.AsBlob is preferred}

    function CreateBlob(transaction: ITransaction): IBlob;
    function OpenBlob(Transaction: ITransaction; BlobID: TISC_QUAD): IBlob;

    {Array}
    function OpenArray(transaction: ITransaction; RelationName, ColumnName: string; ArrayID: TISC_QUAD): IArray;
    function CreateArray(transaction: ITransaction; RelationName, ColumnName: string): IArray;

    {Database Information}
    function GetSQLDialect: integer;
    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: string): IBlobMetaData;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: string): IArrayMetaData;
    function GetDBInformation(Requests: array of byte): IDBInformation; overload;
    function GetDBInformation(Request: byte): IDBInformation; overload;
    function HasActivity: boolean;
  end;

  TProtocol = (TCP, SPX, NamedPipe, Local);

  IServiceQueryResultSubItem = interface
    function getItemType: byte;
    function getSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: string;
    function getAsInteger: integer;
    function getAsByte: byte;
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
    property Items[index: integer]: IServiceQueryResultItem read getItem; default;
    property Count: integer read getCount;
  end;

  ISRBItem = interface
    function getParamType: byte;
    function getAsString: string;
    function getAsInteger: integer;
    procedure setAsString(aValue: string);
    procedure SetAsInteger(aValue: integer);
    property AsString: string read getAsString write setAsString;
    property AsInteger: integer read getAsInteger write SetAsInteger;
  end;

  ISRB = interface
    function getCount: integer;
    function Add(ParamType: byte): ISRBItem;
    function getItems(index: integer): ISRBItem;
    function Find(ParamType: byte): ISRBItem;
    property Items[index: integer]: ISRBItem read getItems; default;
  end;

  ISPBItem = interface
    function getParamType: byte;
    function getAsString: string;
    function getAsByte: byte;
    procedure setAsString(aValue: string);
    procedure setAsByte(aValue: byte);
    property AsString: string read getAsString write setAsString;
    property AsByte: byte read getAsByte write setAsByte;
  end;

  ISPB = interface
    function getCount: integer;
    function Add(ParamType: byte): ISPBItem;
    function getItems(index: integer): ISPBItem;
    function Find(ParamType: byte): ISPBItem;
    property Items[index: integer]: ISPBItem read getItems; default;
  end;

  { IServiceManager }

  IServiceManager = interface
    function getSPB: ISPB;
    procedure Attach;
    procedure Detach(Force: boolean=false);
    function IsAttached: boolean;
    function AllocateRequestBuffer: ISRB;
    procedure Start(Request: ISRB);
    function Query(Request: ISRB) :IServiceQueryResults;
  end;

  IFirebirdAPI = interface
    function GetStatus: IStatus;
    function AllocateDPB: IDPB;
    function AllocateTPB: ITPB;

    {Database connections}
    function OpenDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnConnectError: boolean=true): IAttachment;
    function CreateDatabase(DatabaseName: string; DPB: IDPB; RaiseExceptionOnError: boolean=true): IAttachment;

    {Start Transaction against multiple databases}
    function StartTransaction(Attachments: array of IAttachment;
             TPB: array of byte; DefaultCompletion: TTransactionAction): ITransaction; overload;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: ITPB; DefaultCompletion: TTransactionAction): ITransaction; overload;

    {Service Manager}
    function AllocateSPB: ISPB;
    function GetServiceManager(ServerName: string; Protocol: TProtocol; SPB: ISPB): IServiceManager;

    {Information}
    function IsEmbeddedServer: boolean;
    function GetLibraryName: string;
    function HasServiceAPI: boolean;
    function HasMasterIntf: boolean;
    function HasRollbackRetaining: boolean;
    function HasSynchronousEventWait: boolean;
    function GetImplementationVersion: string;
    function GetCharsetName(CharSetID: integer): string;
    function CharSetID2CodePage(CharSetID: integer; var CodePage: TSystemCodePage): boolean;
    function CodePage2CharSetID(CodePage: TSystemCodePage; var CharSetID: integer): boolean;
    function CharSetName2CharSetID(CharSetName: string; var CharSetID: integer): boolean;
end;

type
  TOnGetLibraryName = procedure(var libname: string);

const
  OnGetLibraryName: TOnGetLibraryName = nil;

type
   { EIBError }

   EIBError = class(EDatabaseError)
   private
     FSQLCode: Long;
   public
     constructor Create(ASQLCode: Long; Msg: string);
     property SQLCode: Long read FSQLCode;
   end;

   { EIBInterBaseError }

   EIBInterBaseError = class(EIBError)
   private
     FIBErrorCode: Long;
   public
     constructor Create(Status: IStatus); overload;
     constructor Create(ASQLCode: Long; AIBErrorCode: Long; Msg: string); overload;
     property IBErrorCode: Long read FIBErrorCode;
   end;

   EIBClientError = class(EIBError);

procedure IBError(ErrMess: TIBClientError; const Args: array of const);

function FirebirdAPI: IFirebirdAPI;

function TryIBLoad: Boolean;
procedure CheckIBLoaded;

implementation

uses FBClientAPI, FB25ClientAPI, FB30ClientAPI;

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

