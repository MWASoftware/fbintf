unit IB;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, FBErrorMessages, IBExternals, FBTypes;

{$I consts_pub.inc}
{$I inf_pub.inc}
{$I configkeys.inc}

type

  {$IF FPC_FULLVERSION < 20700 }
  RawByteString = AnsiString; {Needed for backwards compatibility}
  {$ENDIF}

  TFBStatusCode = cardinal;
  TByteArray = array of byte;

  IAttachment = interface;
  ITransaction = interface;

  IStatus = interface
    function GetIBErrorCode: Long;
    function Getsqlcode: Long;
    function GetMessage: string;
    function CheckStatusVector(ErrorCodes: array of TFBStatusCode): Boolean;
    function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
  end;

  TArrayBound = record
    UpperBound: short;
    LowerBound: short;
  end;
  TArrayBounds = array of TArrayBound;
  TArrayCoords = array of integer;

  IArrayMetaData = interface
    function GetSQLType: short;
    function GetTableName: string;
    function GetColumnName: string;
    function GetDimensions: integer;
    function GetBounds: TArrayBounds;
  end;

  ISQLElement = interface;

  IArray = interface(IArrayMetaData)
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

  IBlobMetaData = interface
    function GetSubType: short;
    function GetCharSetID: short;
    function GetSegmentSize: short;
    function GetTableName: string;
    function GetColumnName: string;
  end;

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

  { IColumnMetaData }

  IColumnMetaData = interface
    function GetSQLType: short;
    function getSubtype: short;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if not alias}
    function getName: string;       {Disambiguated uppercase Field Name}
    function getScale: short;
    function getCharSetID: cardinal;
    function getIsNullable: boolean;
    function GetSize: integer;
    function GetArrayMetaData: IArrayMetaData; {Valid only for Array SQL Type}
    function GetBlobMetaData: IBlobMetaData; {Valid only for Blob SQL Type}
    property Name: string read GetName;
    property Size: Integer read GetSize;
    property CharSetID: cardinal read getCharSetID;
    property SQLType: short read GetSQLType;
    property SQLSubtype: short read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  end;

  { IMetaData }

  IMetaData = interface
    function getCount: integer;
    function getColumnMetaData(index: integer): IColumnMetaData;
    function GetUniqueRelationName: string;
    function ByName(Idx: String): IColumnMetaData;
    property ColMetaData[index: integer]: IColumnMetaData read getColumnMetaData; default;
  end;

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
    function GetAsShort: Short;
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
    property AsShort: Short read GetAsShort;
    property AsString: String read GetAsString;
    property AsVariant: Variant read GetAsVariant ;
    property AsBlob: IBlob read GetAsBlob;
    property AsArray: IArray read GetAsArray;
    property IsNull: Boolean read GetIsNull;
  end;

  IResults = interface
   function getCount: integer;
   function ByName(Idx: String): ISQLData;
   function getSQLData(index: integer): ISQLData;
   property Data[index: integer]: ISQLData read getSQLData; default;
  end;

  IResultSet = interface(IResults)
    function FetchNext: boolean;
    function GetCursorName: string;
    procedure Close;
  end;

  ISQLElement = interface
    function getScale: short;
    function GetSize: integer;
    function GetAsBoolean: boolean;
    function GetAsCurrency: Currency;
    function GetAsInt64: Int64;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsFloat: Float;
    function GetAsLong: Long;
    function GetAsPointer: Pointer;
    function GetAsShort: Short;
    function GetAsString: String;
    function GetAsVariant: Variant;
    procedure SetAsBoolean(AValue: boolean);
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsInt64(Value: Int64);
    procedure SetAsDate(Value: TDateTime);
    procedure SetAsLong(Value: Long);
    procedure SetAsTime(Value: TDateTime);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsDouble(Value: Double);
    procedure SetAsFloat(Value: Float);
    procedure SetAsPointer(Value: Pointer);
    procedure SetAsShort(Value: Short);
    procedure SetAsString(Value: String);
    procedure SetAsVariant(Value: Variant);
    function GetModified: boolean;
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
  end;


  ISQLParam = interface(ISQLElement)
    procedure Clear;
    function getCharSetID: cardinal;
    function GetIsNull: Boolean;
    function GetAsBlob: IBlob;
    function GetAsArray: IArray;
    function GetName: string;
    function GetSQLType: short;
    function GetSubtype: short;
    function GetAsQuad: TISC_QUAD;
    procedure SetIsNull(Value: Boolean);
    procedure SetAsBlob(Value: IBlob);
    procedure SetAsArray(anArray: IArray);
    procedure SetAsQuad(Value: TISC_QUAD);
    property AsBlob: IBlob read GetAsBlob write SetAsBlob;
    property AsArray: IArray read GetAsArray write SetAsArray;
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property Modified: Boolean read getModified;
    property Name: string read GetName;
 end;

  ISQLParams = interface
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ByName(Idx: String): ISQLParam ;
    function GetModified: Boolean;
    property Modified: Boolean read GetModified;
    property Params[index: integer]: ISQLParam read getSQLParam; default;
  end;


  TIBSQLTypes = (SQLUnknown, SQLSelect, SQLInsert,
                  SQLUpdate, SQLDelete, SQLDDL,
                  SQLGetSegment, SQLPutSegment,
                  SQLExecProcedure, SQLStartTransaction,
                  SQLCommit, SQLRollback,
                  SQLSelectForUpdate, SQLSetGenerator);

  IDBInformation = interface;

  IStatement = interface
    function GetSQLParams: ISQLParams;
    function GetMetaData: IMetaData;
    function GetPlan: String;
    function GetRowsAffected(var InsertCount, UpdateCount, DeleteCount: integer): boolean;
    function GetSQLType: TIBSQLTypes;
    function GetSQLText: string;
    function IsPrepared: boolean;
    procedure Prepare(aTransaction: ITransaction=nil);
    function Execute(aTransaction: ITransaction=nil): IResults;
    function OpenCursor(aTransaction: ITransaction=nil): IResultSet;
    function CreateBlob: IBlob;
    function CreateArray(column: IColumnMetaData): IArray; overload;
    function CreateArray(columnName: string): IArray;  overload;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    property MetaData: IMetaData read GetMetaData;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLType: TIBSQLTypes read GetSQLType;
  end;

  TTransactionCompletion = (tcCommit,tcRollback);

  ITransaction = interface
    function Start(DefaultCompletion: TTransactionCompletion=tcCommit): ITransaction;
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
    procedure DecodeUserNames(UserNames: TStrings);
    function getOperationCounts: TDBOperationCounts;
  end;

  { IDBInformation }

  IDBInformation = interface
    function GetCount: integer;
    function GetItem(index: integer): IDBInfoItem;
    function Find(ItemType: byte): IDBInfoItem;
    property Items[index: integer]: IDBInfoItem read getItem; default;
  end;

  IDPBItem = interface
    function getParamType: byte;
    function getAsString: string;
    function getAsByte: byte;
    procedure setAsString(aValue: string);
    procedure setAsByte(aValue: byte);
    property AsString: string read getAsString write setAsString;
    property AsByte: byte read getAsByte write setAsByte;
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
    procedure DropDatabase;
    function StartTransaction(TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction;
    function CreateBlob(transaction: ITransaction): IBlob;
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

    function OpenBlob(Transaction: ITransaction; BlobID: TISC_QUAD): IBlob;

    {Database Information}
    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: string): IBlobMetaData;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: string): IArrayMetaData;
    function GetDBInformation(Requests: array of byte): IDBInformation;
    function HasActivity: boolean;
  end;

  TProtocol = (TCP, SPX, NamedPipe, Local);

  IServiceQueryResultSubItem = interface
    function getItemType: byte;
    function getDataSize: integer;
    procedure getRawBytes(var Buffer);
    function getAsString: string;
    function getAsInteger: integer;
    function getIsTruncated: boolean;
  end;

  IServiceQueryResultItem = interface(IServiceQueryResultSubItem)
    function getCount: integer;
    function getItem(index: integer): IServiceQueryResultSubItem;
    function find(ItemType: byte): IServiceQueryResultSubItem;
    property Items[index: integer]: IServiceQueryResultSubItem read getItem; default;
  end;

  IServiceQueryResults = interface
    function getCount: integer;
    function getItem(index: integer): IServiceQueryResultItem;
    function find(ItemType: byte): IServiceQueryResultItem;
    property Items[index: integer]: IServiceQueryResultItem read getItem; default;
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

    {Database connections}
    function OpenDatabase(DatabaseName: string; DPB: IDPB): IAttachment;
    function CreateDatabase(DatabaseName: string; SQLDialect: integer;
      CreateParams: string; DPB: IDPB): IAttachment;
    function StartTransaction(Attachments: array of IAttachment;
             TPB: array of byte; DefaultCompletion: TTransactionCompletion): ITransaction; {Start Transaction against multiple databases}

    {Service Manager}
    function AllocateSPB: ISPB;
    function GetServiceManager(ServerName: string; Protocol: TProtocol; SPB: ISPB): IServiceManager;

    {Information}
    function IsEmbeddedServer: boolean;
    function GetLibraryName: string;
    function HasServiceAPI: boolean;
    function GetImplementationVersion: string;
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

uses FBLibrary, FB25ClientAPI;

var FFirebirdAPI: IFirebirdAPI;

function FirebirdAPI: IFirebirdAPI;
begin
  if FFirebirdAPI = nil then
    CheckIBLoaded;
  Result := FFirebirdAPI;
end;

function TryIBLoad: Boolean;
var FBLibraryObj: TFBLibrary;
begin
  Result := FFirebirdAPI <> nil;
  if not Result then
  begin
    FBLibraryObj := TFB25ClientAPI.Create;
    FFirebirdAPI := TFB25ClientAPI(FBLibraryObj);
    Result := true;
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

