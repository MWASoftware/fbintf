unit IB;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, FBErrorMessages, IBExternals;

type
   (*********************************************************************)
   (** Blob id structure                                               **)
   (*********************************************************************)
   TGDS_QUAD = record
     gds_quad_high      : ISC_LONG;
     gds_quad_low       : UISC_LONG;
   end;
   TGDS__QUAD           = TGDS_QUAD;
   TISC_QUAD            = TGDS_QUAD;
   PGDS_QUAD            = ^TGDS_QUAD;
   PGDS__QUAD           = ^TGDS__QUAD;
   PISC_QUAD            = ^TISC_QUAD;

   TFBStatusCode = cardinal;

  IStatus = interface
    function GetIBErrorCode: Long;
    function Getsqlcode: Long;
    function GetMessage: string;
    function CheckStatusVector(ErrorCodes: array of TFBStatusCode): Boolean;
    function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
    procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
  end;

  ITransaction = interface
    function GetStatus: IStatus;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;
  end;

  IBlob = interface
    function GetStatus: IStatus;
    procedure Cancel;
    procedure Close;
    function GetBlobID: TISC_QUAD;
    function GetInfo(var NumSegments: Int64; var MaxSegmentSize,
                      TotalSize: Int64; var BlobType: Short) :boolean;
    function Read(var Buffer; Count: Longint): Longint;
    function Write(const Buffer; Count: Longint): Longint;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(S: TStream);
 end;

  IFieldMetaData = interface
    function GetSQLType: cardinal;
    function getSubtype: integer;
    function getRelationName: string;
    function getOwnerName: string;
    function getSQLName: string;    {Name of the column}
    function getAliasName: string;  {Alias Name of column or Column Name if not alias}
    function getName: string;       {Disambiguated uppercase Field Name}
    function getScale: integer;
    function getCharSetID: cardinal;
    function getIsNullable: boolean;
    function GetSize: integer;
    property Name: string read GetName;
    property Size: Integer read GetSize;
    property CharSetID: cardinal read getCharSetID;
    property SQLType: cardinal read GetSQLType;
    property SQLSubtype: integer read getSubtype;
    property IsNullable: Boolean read GetIsNullable;
  end;

  IMetaData = interface
    function getCount: integer;
    function getFieldMetaData(index: integer): IFieldMetaData;
    function ByName(Idx: String): IFieldMetaData;
  end;

  ISQLData = interface(IFieldMetaData)
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
    property IsNull: Boolean read GetIsNull;
  end;

  IResults = interface(IMetaData)
   function ByName(Idx: String): ISQLData;
   function getSQLData(index: integer): ISQLData;
  end;

  IResultSet = interface(IResults)
    function FetchNext: boolean;
    procedure Close;
  end;

  ISQLParam = interface
    function GetSQLType: cardinal;
    function getSubtype: integer;
    function getScale: integer;
    function getCharSetID: cardinal;
    function GetSize: integer;
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
    function getName: string;
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
    procedure SetAsQuad(Value: TISC_QUAD);
    procedure SetAsShort(Value: Short);
    procedure SetAsString(Value: String);
    procedure SetAsVariant(Value: Variant);
    procedure SetIsNull(Value: Boolean);
    function getModified: boolean;
    procedure Clear;
    procedure SetName(Value: string);
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
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property AsShort: Short read GetAsShort write SetAsShort;
    property AsString: String read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property Modified: Boolean read getModified;
    property Name: string read GetName write SetName;
 end;

  ISQLParams = interface
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ByName(Idx: String): ISQLParam ;
  end;


  TIBSQLTypes = (SQLUnknown, SQLSelect, SQLInsert,
                  SQLUpdate, SQLDelete, SQLDDL,
                  SQLGetSegment, SQLPutSegment,
                  SQLExecProcedure, SQLStartTransaction,
                  SQLCommit, SQLRollback,
                  SQLSelectForUpdate, SQLSetGenerator);

  IStatement = interface
    function GetStatus: IStatus;
    function GetSQLParams: ISQLParams;
    function GetOutMetaData: IMetaData;
    function GetPlan: String;
    function GetRowsAffected: Integer;
    function GetSQLType: TIBSQLTypes;
    function Execute: IResults;
    function OpenCursor: IResultSet;
    property SQLParams: ISQLParams read GetSQLParams;
    property SQLType: TIBSQLTypes read GetSQLType;
  end;

type
  TEventInfo = record
    EventName: string;
    Count: integer;
  end;

  TEventCounts = array of TEventInfo;

  TEventHandler = procedure(EventCounts: TEventCounts) of object;

  IEvents = interface
    function GetStatus: IStatus;
    procedure Cancel;
    procedure WaitForEvent(var EventCounts: TEventCounts);
    procedure AsyncWaitForEvent(EventHandler: TEventHandler);
  end;

  IAttachment = interface
    function GetStatus: IStatus;
    procedure Disconnect(Force: boolean);
    procedure DropDatabase;
    function StartTransaction(Params: TStrings): ITransaction;
    function CreateBlob(transaction: ITransaction): IBlob;
    function OpenBlob(transaction: ITransaction; BlobID: TISC_QUAD): IBlob;
    procedure ExecImmediate(transaction: ITransaction; sql: string; SQLDialect: integer);
    function Prepare(transaction: ITransaction; sql: string; SQLDialect: integer): IStatement;

    {Events}
    function GetEventHandler(Events: TStrings): IEvents;

    {Database Information}
    function GetBlobCharSetID(transaction: ITransaction; tableName, columnName: string): short;
    function GetAllocation: Long;
    function GetBaseLevel: Long;
    function GetDBFileName: String;
    function GetDBSiteName: String;
    function GetDBImplementationNo: Long;
    function GetDBImplementationClass: Long;
    function GetNoReserve: Long;
    function GetODSMinorVersion: Long;
    function GetODSMajorVersion: Long;
    function GetPageSize: Long;
    function GetVersion: String;
    function GetCurrentMemory: Long;
    function GetForcedWrites: Long;
    function GetMaxMemory: Long;
    function GetNumBuffers: Long;
    function GetSweepInterval: Long;
    function GetUserNames: TStringList;
    function GetFetches: Long;
    function GetMarks: Long;
    function GetReads: Long;
    function GetWrites: Long;
    function GetBackoutCount: TStringList;
    function GetDeleteCount: TStringList;
    function GetExpungeCount: TStringList;
    function GetInsertCount: TStringList;
    function GetPurgeCount: TStringList;
    function GetReadIdxCount: TStringList;
    function GetReadSeqCount: TStringList;
    function GetUpdateCount: TStringList;
    function GetReadOnly: Long;
    function GetDBSQLDialect: Long;
  end;

  IService = interface
  end;

  IFirebirdAPI = interface
    function GetStatus: IStatus;
    function OpenDatabase(DatabaseName: string; Params: TStrings): IAttachment;
    procedure CreateDatabase(DatabaseName: string; SQLDialect: integer;
                                          Params: TStrings);
    function GetServiceManager(Service: string; Params: TStrings): IService;
    {Start Transaction against multiple databases}
    function StartTransaction(Databases: array of IAttachment; Params: TStrings): ITransaction;
    function GetIsEmbeddedServer: boolean;
    function GetLibraryName: string;
    function HasServiceAPI: boolean;
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

var FirebirdAPI: IFirebirdAPI;

function TryIBLoad: Boolean;
procedure CheckIBLoaded;

implementation

uses FBLibrary, FB25ClientAPI;

const FBLibraryObj: TFBLibrary = nil;

function TryIBLoad: Boolean;
begin
  Result := false;
  if FBLibraryObj = nil then
  begin
    FBLibraryObj := TFBClientAPI.Create;
    FirebirdAPI := TFBClientAPI(FBLibraryObj);
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
  FirebirdAPI := nil;

finalization
  if FBLibraryObj <> nil then
    FBLibraryObj.Free;

end.

