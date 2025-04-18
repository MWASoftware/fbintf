(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
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
 *  The Original Code is (C) 2016-2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBAttachment;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$interfaces COM}
{$define HASREQEX}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF WINDOWS} windows, {$ENDIF} IB,  FBParamBlock,
  FBActivityMonitor, FBClientAPI, IBUtils, SyncObjs;

const
  DefaultMaxInlineBlobLimit = 8192;

type
  TCharsetMap = record
    CharsetID: integer;
    CharSetName: AnsiString;
    CharSetWidth: integer;
    CodePage: TSystemCodePage;
    AllowReverseLookup: boolean; {used to ensure that lookup of CP_UTF* does not return UNICODE_FSS}
  end;

  { Database Journalling.

    This class is intended to support a client side journal of all database
    updates, inserts and deletes made by the client during a session. It also records
    the transaction each update was made under.

    The database schema is required to include a control table "IBX$JOURNALS" and
    an SQL Sequence IBX$SESSIONS. These are created by the class when the
    database is opened, if they are not already present. However, it is recommended
    that they are created as an orginal part of the database schema in order to
    unnecessarily avoid each user being given sufficient priviledge to create tables
    and Sequences.

    Syntax:

    Transaction Start:
    *S:<date/time>,<attachmentid>,<session id>,<transaction no.>,<string length>:<transaction Name>,<string length>:<TPB>,<default Completion>

    Transaction Commit:
    *C:<date/time>,<attachmentid>,<session id>,<transaction no.>

    Transaction Commit retaining :
    *c:<date/time>,<attachmentid>,<session id>,<transaction no.><old transaction no.>

    Transaction Rollback:
    *R:<date/time>,<attachmentid>,<session id>,<transaction no.>

    Transaction Rollback retaining:
    *r:<date/time>,<attachmentid>,<session id>,<transaction no.><old transaction no.>

    Update/Insert/Delete
    *Q:<date/time>,<attachmentid>,<session id>,<transaction no.>,<length of query text in bytes>:<query text>

  }

  { TFBJournaling }

  TFBJournaling = class(TActivityHandler, IJournallingHook)
  private
    {Logfile}
    const sQueryJournal          = '*Q:''%s'',%d,%d,%d,%d:%s' + LineEnding;
    const sTransStartJnl         = '*S:''%s'',%d,%d,%d,%d:%s,%d:%s,%d' + LineEnding;
    const sTransCommitJnl        = '*C:''%s'',%d,%d,%d' + LineEnding;
    const sTransCommitFailJnl    = '*F:''%s'',%d,%d,%d' + LineEnding;
    const sTransCommitRetJnl     = '*c:''%s'',%d,%d,%d,%d' + LineEnding;
    const sTransRollBackJnl      = '*R:''%s'',%d,%d,%d' + LineEnding;
    const sTransRollBackFailJnl  = '*f:''%s'',%d,%d,%d' + LineEnding;
    const sTransRollBackRetJnl   = '*r:''%s'',%d,%d,%d,%d' + LineEnding;
  private
    FOptions: TJournalOptions;
    FJournalFilePath: string;
    FJournalFileStream: TStream;
    FSessionID: integer;
    FDoNotJournal: boolean;
    FOwnsJournal: boolean;
    FCriticalSection: TCriticalSection;
    function GetDateTimeFmt: AnsiString;
    procedure WriteJnlEntry(LogEntry: AnsiString);
  protected
    procedure EndSession(RetainJournal: boolean);
    function GetAttachment: IAttachment; virtual; abstract;
  public
    {IAttachment}
    procedure Disconnect(Force: boolean=false); virtual;
  public
    {IJournallingHook}
    procedure TransactionStart(Tr: ITransaction);
    function TransactionEnd( TransactionID: integer; Completion: TTrCompletionState): boolean;
    procedure TransactionRetained(Tr: ITransaction; OldTransactionID: integer;
      Action: TTransactionAction);
    procedure ExecQuery(Stmt: IStatement);
    procedure ExecImmediateJnl(sql: AnsiString; tr: ITransaction);
  public
    constructor Create;
    destructor Destroy; override;
    {Client side Journaling}
    function JournalingActive: boolean;
    function GetJournalOptions: TJournalOptions;
    function StartJournaling(aJournalLogFile: AnsiString): integer; overload;
    function StartJournaling(aJournalLogFile: AnsiString; Options: TJournalOptions): integer; overload;
    function StartJournaling(S: TStream; Options: TJournalOptions): integer; overload;
    procedure StopJournaling(RetainJournal: boolean);
  end;

  { TFBAttachment }

  TFBAttachment = class(TFBJournaling)
  private
    FDPB: IDPB;
    FFirebirdAPI: IFirebirdAPI;
    FODSMajorVersion: integer;
    FODSMinorVersion: integer;
    FUserCharSetMap: array of TCharSetMap;
    FSecDatabase: AnsiString;
    FInlineBlobLimit: integer;
    FAttachmentID: integer;
    FSQLDialect: integer;
    FHasDefaultCharSet: boolean;
    FCharSetID: integer;
    FCodePage: TSystemCodePage;
    FRemoteProtocol: AnsiString;
    FAuthMethod: AnsiString;
    FHasConnectionInfo: boolean;
    FInNeedConnectionInfo: boolean;
    procedure NeedDBInfo;
    procedure NeedConnectionInfo;
  protected
    FDatabaseName: AnsiString;
    FRaiseExceptionOnConnectError: boolean;
    constructor Create(api: TFBClientAPI; DatabaseName: AnsiString; DPB: IDPB;
      RaiseExceptionOnConnectError: boolean);
    procedure CheckHandle; virtual; abstract;
    procedure ClearCachedInfo; virtual;
    function GenerateCreateDatabaseSQL(DatabaseName: AnsiString; aDPB: IDPB): AnsiString;
    function GetDBInfo(ReqBuffer: PByte; ReqBufLen: integer): IDBInformation; virtual; abstract;
    function IsConnected: boolean; virtual; abstract;
    procedure EndAllTransactions;
    procedure DPBFromCreateSQL(CreateSQL: AnsiString);
    procedure SetParameters(SQLParams: ISQLParams; params: array of const);
    procedure SetSQLDialect(aValue: integer);
    procedure UseServerICUChanged; virtual;
  public
    destructor Destroy; override;
    procedure Disconnect(Force: boolean); override;
    procedure DropDatabase; virtual;
    procedure IBDataBaseError;
    function getFirebirdAPI: IFirebirdAPI;
    function getDPB: IDPB;
    function AllocateBPB: IBPB;
    function AllocateDIRB: IDIRB;
    function StartTransaction(TPB: array of byte;
      DefaultCompletion: TTransactionCompletion;
      aName: AnsiString=''): ITransaction; overload; virtual; abstract;
    function StartTransaction(TPB: ITPB; DefaultCompletion: TTransactionCompletion;
      aName: AnsiString=''): ITransaction; overload; virtual; abstract;
    procedure ExecImmediate(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer); overload; virtual; abstract;
    procedure ExecImmediate(TPB: array of byte; sql: AnsiString; aSQLDialect: integer); overload;
    procedure ExecImmediate(transaction: ITransaction; sql: AnsiString); overload;
    procedure ExecImmediate(TPB: array of byte; sql: AnsiString); overload;
    function ExecuteSQL(TPB: array of byte; sql: AnsiString; SQLDialect: integer; params: array of const): IResults; overload;
    function ExecuteSQL(transaction: ITransaction; sql: AnsiString; SQLDialect: integer; params: array of const): IResults; overload;
    function ExecuteSQL(TPB: array of byte; sql: AnsiString; params: array of const): IResults; overload;
    function ExecuteSQL(transaction: ITransaction; sql: AnsiString; params: array of const): IResults; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer;
                             Scrollable: boolean=false): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer;
                             params: array of const): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString; Scrollable: boolean=false): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString;
                             params: array of const): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString; Scrollable: boolean;
                             params: array of const): IResultSet; overload;
    function OpenCursor(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer; Scrollable: boolean;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer;
                             Scrollable: boolean=false): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer; Scrollable: boolean;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString; Scrollable: boolean=false): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(transaction: ITransaction; sql: AnsiString; Scrollable: boolean;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(sql: AnsiString;Scrollable: boolean=false): IResultSet; overload;
    function OpenCursorAtStart(sql: AnsiString; Scrollable: boolean;
                             params: array of const): IResultSet; overload;
    function OpenCursorAtStart(sql: AnsiString;
                             params: array of const): IResultSet; overload;
    function Prepare(transaction: ITransaction; sql: AnsiString; aSQLDialect: integer; CursorName: AnsiString=''): IStatement; overload; virtual; abstract;
    function Prepare(transaction: ITransaction; sql: AnsiString; CursorName: AnsiString=''): IStatement; overload;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: AnsiString;
                       aSQLDialect: integer; GenerateParamNames: boolean=false;
                       CaseSensitiveParams: boolean = false; CursorName: AnsiString=''): IStatement; overload; virtual; abstract;
    function PrepareWithNamedParameters(transaction: ITransaction; sql: AnsiString;
                       GenerateParamNames: boolean=false;
                       CaseSensitiveParams: boolean = false; CursorName: AnsiString=''): IStatement; overload;
    function GetEventHandler(Events: TStrings): IEvents; overload; virtual; abstract;
    function GetEventHandler(Event: AnsiString): IEvents; overload;

    function GetSQLDialect: integer;
    function GetAttachmentID: integer;
    function CreateBlob(transaction: ITransaction; RelationName, ColumnName: AnsiString; BPB: IBPB=nil): IBlob; overload;
    function CreateBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BPB: IBPB=nil): IBlob; overload; virtual; abstract;
    function OpenBlob(transaction: ITransaction; BlobMetaData: IBlobMetaData; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob; overload; virtual; abstract;
    function OpenBlob(transaction: ITransaction; RelationName, ColumnName: AnsiString; BlobID: TISC_QUAD; BPB: IBPB=nil): IBlob; overload;
    function OpenBlob(transaction: ITransaction; Field: ISQLData; BPB: IBPB=nil): IBlob; overload;
    function CreateArray(transaction: ITransaction; RelationName, ColumnName: AnsiString
      ): IArray; overload;
    function CreateArray(transaction: ITransaction; ArrayMetaData: IArrayMetaData): IArray; overload; virtual; abstract;
    function OpenArray(transaction: ITransaction; RelationName, ColumnName: AnsiString; ArrayID: TISC_QUAD): IArray; overload;
    function OpenArray(transaction: ITransaction; ArrayMetaData: IArrayMetaData; ArrayID: TISC_QUAD): IArray; overload; virtual; abstract;
    property SQLDialect: integer read GetSQLDialect;
    property DPB: IDPB read FDPB;
  public
    function GetDBInformation(Requests: array of byte): IDBInformation; overload;
    function GetDBInformation(Request: byte): IDBInformation; overload;
    function GetDBInformation(Requests: IDIRB): IDBInformation; overload;
    function GetConnectString: AnsiString;
    function GetRemoteProtocol: AnsiString;
    function GetAuthenticationMethod: AnsiString;
    function GetSecurityDatabase: AnsiString;
    function GetODSMajorVersion: integer;
    function GetODSMinorVersion: integer;
    function GetCharSetID: integer;
    function GetCodePage: TSystemCodePage;
    function HasDecFloatSupport: boolean; virtual;
    function GetInlineBlobLimit: integer;
    procedure SetInlineBlobLimit(limit: integer);
    function HasBatchMode: boolean; virtual;
    function HasTable(aTableName: AnsiString): boolean;
    function HasFunction(aFunctionName: AnsiString): boolean;
    function HasProcedure(aProcName: AnsiString): boolean;

  public
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
    function GetBlobMetaData(Transaction: ITransaction; tableName, columnName: AnsiString): IBlobMetaData; virtual; abstract;
    function GetArrayMetaData(Transaction: ITransaction; tableName, columnName: AnsiString): IArrayMetaData; virtual; abstract;
    property CharSetID: integer read GetCharSetID;
    property CodePage: TSystemCodePage read GetCodePage;

  public
    {Time Zone Support}
    function GetTimeZoneServices: ITimeZoneServices; virtual;
    function HasTimeZoneSupport: boolean; virtual;

  end;

  { TDPBItem }

  TDPBItem = class(TParamBlockItem,IDPBItem)
  public
   function getParamTypeName: AnsiString; override;
  end;

  { TDPB }

  TDPB = class (TCustomParamBlock<TDPBItem,IDPBItem>, IDPB)
  protected
   function LookupItemType(ParamTypeName: AnsiString): byte; override;
  public
    constructor Create(api: TFBClientAPI);
    function GetParamTypeName(ParamType: byte): Ansistring;
    {$IFDEF FPC}
    function IDPB.GetDPBParamTypeName = GetParamTypeName;
    {$ELSE}
    function GetDPBParamTypeName(ParamType: byte): Ansistring;
    {$ENDIF}
  end;

implementation

uses FBMessages, IBErrorCodes, FBTransaction {$IFDEF HASREQEX}, RegExpr{$ENDIF};

const
  {Journaling}
  sJournalTableName = 'IBX$JOURNALS';
  sSequenceName = 'IBX$SESSIONS';

  sqlCreateJournalTable =
    'Create Table ' + sJournalTableName + '(' +
    '  IBX$SessionID Integer not null, '+
    '  IBX$TransactionID Integer not null, '+
    '  IBX$OldTransactionID Integer, '+
    '  IBX$USER VarChar(32) Default CURRENT_USER, '+
    '  IBX$CREATED TIMESTAMP Default CURRENT_TIMESTAMP, '+
    '  Primary Key(IBX$SessionID,IBX$TransactionID)' +
    ')';

  sqlCreateSequence = 'CREATE SEQUENCE ' + sSequenceName;

  sqlGetNextSessionID = 'Select Gen_ID(' + sSequenceName + ',1) as SessionID From RDB$DATABASE';

  sqlRecordJournalEntry = 'Insert into ' + sJournalTableName + '(IBX$SessionID,IBX$TransactionID,IBX$OldTransactionID) '+
                        'Values(?,?,?)';

  sqlCleanUpSession = 'Delete From ' + sJournalTableName + ' Where IBX$SessionID = ?';

const
  CharSetMap: array [0..69] of TCharsetMap = (
  (CharsetID: 0; CharSetName: 'NONE'; CharSetWidth: 1; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 1; CharSetName: 'OCTETS'; CharSetWidth: 1; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 2; CharSetName: 'ASCII'; CharSetWidth: 1; CodePage: CP_ASCII; AllowReverseLookup: true),
  (CharsetID: 3; CharSetName: 'UNICODE_FSS'; CharSetWidth: 3; CodePage: CP_UTF8; AllowReverseLookup: false),
  (CharsetID: 4; CharSetName: 'UTF8'; CharSetWidth: 4; CodePage: CP_UTF8; AllowReverseLookup: true),
  (CharsetID: 5; CharSetName: 'SJIS_0208'; CharSetWidth: 2; CodePage: 20932; AllowReverseLookup: true),
  (CharsetID: 6; CharSetName: 'EUCJ_0208'; CharSetWidth: 2; CodePage: 20932; AllowReverseLookup: true),
  (CharsetID: 7; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 8; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 9; CharSetName: 'DOS737'; CharSetWidth: 1; CodePage: 737; AllowReverseLookup: true),
  (CharsetID: 10; CharSetName: 'DOS437'; CharSetWidth: 1; CodePage: 437; AllowReverseLookup: true),
  (CharsetID: 11; CharSetName: 'DOS850'; CharSetWidth: 1; CodePage: 850; AllowReverseLookup: true),
  (CharsetID: 12; CharSetName: 'DOS865'; CharSetWidth: 1; CodePage: 865; AllowReverseLookup: true),
  (CharsetID: 13; CharSetName: 'DOS860'; CharSetWidth: 1; CodePage: 860; AllowReverseLookup: true),
  (CharsetID: 14; CharSetName: 'DOS863'; CharSetWidth: 1; CodePage: 863; AllowReverseLookup: true),
  (CharsetID: 15; CharSetName: 'DOS775'; CharSetWidth: 1; CodePage: 775; AllowReverseLookup: true),
  (CharsetID: 16; CharSetName: 'DOS858'; CharSetWidth: 1; CodePage: 858; AllowReverseLookup: true),
  (CharsetID: 17; CharSetName: 'DOS862'; CharSetWidth: 1; CodePage: 862; AllowReverseLookup: true),
  (CharsetID: 18; CharSetName: 'DOS864'; CharSetWidth: 1; CodePage: 864; AllowReverseLookup: true),
  (CharsetID: 19; CharSetName: 'NEXT'; CharSetWidth: 1; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 20; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: true),
  (CharsetID: 21; CharSetName: 'ISO8859_1'; CharSetWidth: 1; CodePage: 28591; AllowReverseLookup: true),
  (CharsetID: 22; CharSetName: 'ISO8859_2'; CharSetWidth: 1; CodePage: 28592; AllowReverseLookup: true),
  (CharsetID: 23; CharSetName: 'ISO8859_3'; CharSetWidth: 1; CodePage: 28593; AllowReverseLookup: true),
  (CharsetID: 24; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 25; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 26; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 27; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 28; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 29; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 30; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 31; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 32; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 33; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 34; CharSetName: 'ISO8859_4'; CharSetWidth: 1; CodePage: 28594; AllowReverseLookup: true),
  (CharsetID: 35; CharSetName: 'ISO8859_5'; CharSetWidth: 1; CodePage: 28595; AllowReverseLookup: true),
  (CharsetID: 36; CharSetName: 'ISO8859_6'; CharSetWidth: 1; CodePage: 28596; AllowReverseLookup: true),
  (CharsetID: 37; CharSetName: 'ISO8859_7'; CharSetWidth: 1; CodePage: 28597; AllowReverseLookup: true),
  (CharsetID: 38; CharSetName: 'ISO8859_8'; CharSetWidth: 1; CodePage: 28598; AllowReverseLookup: true),
  (CharsetID: 39; CharSetName: 'ISO8859_9'; CharSetWidth: 1; CodePage: 28599; AllowReverseLookup: true),
  (CharsetID: 40; CharSetName: 'ISO8859_13'; CharSetWidth: 1; CodePage: 28603; AllowReverseLookup: true),
  (CharsetID: 41; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 42; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 43; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 44; CharSetName: 'KSC_5601'; CharSetWidth: 2; CodePage: 949; AllowReverseLookup: true),
  (CharsetID: 45; CharSetName: 'DOS852'; CharSetWidth: 1; CodePage: 852; AllowReverseLookup: true),
  (CharsetID: 46; CharSetName: 'DOS857'; CharSetWidth: 1; CodePage: 857; AllowReverseLookup: true),
  (CharsetID: 47; CharSetName: 'DOS861'; CharSetWidth: 1; CodePage: 861; AllowReverseLookup: true),
  (CharsetID: 48; CharSetName: 'DOS866'; CharSetWidth: 1; CodePage: 866; AllowReverseLookup: true),
  (CharsetID: 49; CharSetName: 'DOS869'; CharSetWidth: 1; CodePage: 869; AllowReverseLookup: true),
  (CharsetID: 50; CharSetName: 'CYRL'; CharSetWidth: 1; CodePage: 1251; AllowReverseLookup: true),
  (CharsetID: 51; CharSetName: 'WIN1250'; CharSetWidth: 1; CodePage: 1250; AllowReverseLookup: true),
  (CharsetID: 52; CharSetName: 'WIN1251'; CharSetWidth: 1; CodePage: 1251; AllowReverseLookup: true),
  (CharsetID: 53; CharSetName: 'WIN1252'; CharSetWidth: 1; CodePage: 1252; AllowReverseLookup: true),
  (CharsetID: 54; CharSetName: 'WIN1253'; CharSetWidth: 1; CodePage: 1253; AllowReverseLookup: true),
  (CharsetID: 55; CharSetName: 'WIN1254'; CharSetWidth: 1; CodePage: 1254; AllowReverseLookup: true),
  (CharsetID: 56; CharSetName: 'BIG_5'; CharSetWidth: 2; CodePage: 950; AllowReverseLookup: true),
  (CharsetID: 57; CharSetName: 'GB_2312'; CharSetWidth: 2; CodePage: 936; AllowReverseLookup: true),
  (CharsetID: 58; CharSetName: 'WIN1255'; CharSetWidth: 1; CodePage: 1255; AllowReverseLookup: true),
  (CharsetID: 59; CharSetName: 'WIN1256'; CharSetWidth: 1; CodePage: 1256; AllowReverseLookup: true),
  (CharsetID: 60; CharSetName: 'WIN1257'; CharSetWidth: 1; CodePage: 1257; AllowReverseLookup: true),
  (CharsetID: 61; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 62; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE; AllowReverseLookup: false),
  (CharsetID: 63; CharSetName: 'KOI8R'; CharSetWidth: 1; CodePage: 20866; AllowReverseLookup: true),
  (CharsetID: 64; CharSetName: 'KOI8U'; CharSetWidth: 1; CodePage: 21866; AllowReverseLookup: true),
  (CharsetID: 65; CharSetName: 'WIN1258'; CharSetWidth: 1; CodePage: 1258; AllowReverseLookup: true),
  (CharsetID: 66; CharSetName: 'TIS620'; CharSetWidth: 1; CodePage: 874; AllowReverseLookup: true),
  (CharsetID: 67; CharSetName: 'GBK'; CharSetWidth: 2; CodePage: 936; AllowReverseLookup: true),
  (CharsetID: 68; CharSetName: 'CP943C'; CharSetWidth: 2; CodePage: 943; AllowReverseLookup: true),
  (CharsetID: 69; CharSetName: 'GB18030'; CharSetWidth: 4; CodePage: 54936; AllowReverseLookup: true)
);

const
  isc_dpb_last_dpb_constant = isc_dpb_decfloat_traps;

  DPBConstantNames: array[1..isc_dpb_last_dpb_constant] of string = (
    'cdd_pathname',
    'allocation',
    'journal',
    'page_size',
    'num_buffers',
    'buffer_length',
    'debug',
    'garbage_collect',
    'verify',
    'sweep',
    'enable_journal',
    'disable_journal',
    'dbkey_scope',
    'number_of_users',
    'trace',
    'no_garbage_collect',
    'damaged',
    'license',
    'sys_user_name',
    'encrypt_key',
    'activate_shadow',
    'sweep_interval',
    'delete_shadow',
    'force_write',
    'begin_log',
    'quit_log',
    'no_reserve',
    'user_name',
    'password',
    'password_enc',
    'sys_user_name_enc',
    'interp',
    'online_dump',
    'old_file_size',
    'old_num_files',
    'old_file',
    'old_start_page',
    'old_start_seqno',
    'old_start_file',
    'drop_walfile',
    'old_dump_id',
    'wal_backup_dir',
    'wal_chkptlen',
    'wal_numbufs',
    'wal_bufsize',
    'wal_grp_cmt_wait',
    'lc_messages',
    'lc_ctype',
    'cache_manager',
    'shutdown',
    'online',
    'shutdown_delay',
    'reserved',
    'overwrite',
    'sec_attach',
    'disable_wal',
    'connect_timeout',
    'dummy_packet_interval',
    'gbak_attach',
    'sql_role_name',
    'set_page_buffers',
    'working_directory',
    'sql_dialect',
    'set_db_readonly',
    'set_db_sql_dialect',
    'gfix_attach',
    'gstat_attach',
    'set_db_charset',
    'gsec_attach',
    'address_path' ,
    'process_id',
    'no_db_triggers',
    'trusted_auth',
    'process_name',
    'trusted_role',
    'org_filename',
    'utf8_filename',
    'ext_call_depth',
    'auth_block',
    'client_version',
    'remote_protocol',
    'host_name',
    'os_user',
    'specific_auth_data',
    'auth_plugin_list',
    'auth_plugin_name',
    'config',
    'nolinger',
    'reset_icu',
    'map_attach',
    'session_time_zone',
    'set_db_replica',
    'set_bind',
    'decfloat_round',
    'decfloat_traps'
    );

type

  { TQueryProcessor }

  TQueryProcessor=class(TSQLTokeniser)
  private
    FInString: AnsiString;
    FIndex: integer;
    FStmt: IStatement;
    function DoExecute: AnsiString;
    function GetParamValue(ParamIndex: integer): AnsiString;
  protected
    function GetChar: AnsiChar; override;
  public
    class function Execute(Stmt: IStatement): AnsiString;
  end;

  { TQueryProcessor }

function TQueryProcessor.DoExecute: AnsiString;
var token: TSQLTokens;
    ParamIndex: integer;
begin
  Result := '';
  ParamIndex := 0;

  while not EOF do
  begin
    token := GetNextToken;
    case token of
    sqltPlaceHolder:
      begin
        Result := Result + GetParamValue(ParamIndex);
        Inc(ParamIndex);
      end;

    sqltIdentifierInDoubleQuotes:
      Result := Result + '"' + TokenText + '"';
    else
      Result := Result + TokenText;
    end;
  end;
end;

function TQueryProcessor.GetParamValue(ParamIndex: integer): AnsiString;

  function formatWithTZ(fmt: AnsiString): AnsiString;
  var aDateTime: TDateTime;
      aTimeZone: AnsiString;
      dstOffset: smallint;
  begin
    with FStmt.GetAttachment.GetTimeZoneServices, FStmt.SQLParams[ParamIndex] do
    begin
      if GetTZTextOption = tzGMT then
        Result := FBFormatDateTime(fmt,GetAsUTCDateTime)
      else
      begin
        GetAsDateTime(aDateTime,dstOffset,aTimeZone);
        if GetTZTextOption = tzOffset then
          Result := FBFormatDateTime(fmt,aDateTime) + ' ' + FormatTimeZoneOffset(dstOffset)
        else
          Result := FBFormatDateTime(fmt,aDateTime) + ' ' + aTimeZone;
      end;
    end;
  end;

begin
  with FStmt.SQLParams[ParamIndex] do
  begin
    if IsNull then
      Result := 'NULL'
    else
    case getColMetadata.GetSQLType of
    SQL_BLOB:
      if  GetSQLType = SQL_BLOB then
      begin
        if getSubType = 1 then {string}
          Result := '''' + SQLSafeString(GetAsString) + ''''
        else
          Result := TSQLXMLReader.FormatBlob(GetAsString,getSubType);
      end
      else
        Result := '''' + SQLSafeString(GetAsString) + '''';

    SQL_ARRAY:
        Result := TSQLXMLReader.FormatArray(getAsArray);

    SQL_VARYING,
    SQL_TEXT:
      Result := '''' + SQLSafeString(GetAsString) + '''';

    SQL_TYPE_DATE:
      Result := '''' + SQLSafeString(FormatDateTime('yyyy-mm-dd',GetAsDateTime)) + '''';

    SQL_TIMESTAMP:
      Result := '''' + SQLSafeString(FBFormatDateTime('yyyy-mm-dd hh:mm:ss.zzzz',GetAsDateTime)) + '''';

    SQL_TYPE_TIME:
      Result := '''' + SQLSafeString(FBFormatDateTime('hh:mm:ss.zzzz',GetAsDateTime)) + '''';

    SQL_TIMESTAMP_TZ_EX,
    SQL_TIMESTAMP_TZ:
        Result := '''' + SQLSafeString(formatWithTZ('yyyy-mm-dd hh:mm:ss.zzzz')) + '''';

    SQL_TIME_TZ_EX,
    SQL_TIME_TZ:
      Result := '''' + SQLSafeString(formatWithTZ('hh:mm:ss.zzzz')) + '''';

    else
      Result := GetAsString;
    end;
  end;
end;

function TQueryProcessor.GetChar: AnsiChar;
begin
  if FIndex <= Length(FInString) then
  begin
    Result := FInString[FIndex];
    Inc(FIndex);
  end
  else
    Result := #0;
end;

class function TQueryProcessor.Execute(Stmt: IStatement): AnsiString;
begin
  if not Stmt.IsPrepared then
    IBError(ibxeSQLClosed,[]);
  with self.Create do
  try
    FStmt := Stmt;
    FInString := Stmt.GetProcessedSQLText;
    FIndex := 1;
    Result := Trim(DoExecute);
  finally
    Free;
  end;
end;

{ TFBJournaling }

function TFBJournaling.GetDateTimeFmt: AnsiString;
begin
  {$IF declared(DefaultFormatSettings)}
  with DefaultFormatSettings do
  {$ELSE}
  {$IF declared(FormatSettings)}
  with FormatSettings do
  {$IFEND}
  {$IFEND}
  Result := ShortDateFormat + ' ' + LongTimeFormat + '.zzzz'
end;

procedure TFBJournaling.WriteJnlEntry(LogEntry: AnsiString);
begin
  if assigned(FJournalFileStream) then
  begin
    FCriticalSection.Acquire;
    try
        FJournalFileStream.Write(LogEntry[1],Length(LogEntry));
    finally
      FCriticalSection.Release;
    end;
  end;

end;

procedure TFBJournaling.EndSession(RetainJournal: boolean);
begin
  if JournalingActive and (FJournalFileStream <> nil) then
  begin
    if FOwnsJournal then
      FJournalFileStream.Free;
    FJournalFileStream := nil;

    if not (joNoServerTable in FOptions) and not RetainJournal then
    try
        GetAttachment.ExecuteSQL([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],
             sqlCleanUpSession,[FSessionID]);
        if FileExists(FJournalFilePath) then
          sysutils.DeleteFile(FJournalFilePath);
    except On E: EIBInterBaseError do
      if E.IBErrorCode <> isc_lost_db_connection then
        raise;
        {ignore - do not delete journal if database gone away}
    end;
    FSessionID := -1;
    FJournalFilePath := '';
  end;
end;

procedure TFBJournaling.Disconnect(Force: boolean);
begin
  if JournalingActive then
    EndSession(Force);
end;

procedure TFBJournaling.TransactionStart(Tr: ITransaction);
var LogEntry: AnsiString;
    TPBText: AnsiString;
begin
  if not (joNoServerTable in FOptions) then
  try
    FDoNotJournal := true;
    GetAttachment.ExecuteSQL(Tr,sqlRecordJournalEntry,[FSessionID,Tr.GetTransactionID,NULL]);
  finally
    FDoNotJournal := false;
  end;
  TPBText := Tr.getTPB.AsText;
  LogEntry := Format(sTransStartJnl,[FBFormatDateTime(GetDateTimeFmt,Now),
                                     GetAttachment.GetAttachmentID,
                                     FSessionID,
                                     Tr.GetTransactionID,
                                     Length(Tr.TransactionName),
                                     Tr.TransactionName,
                                     Length(TPBText),TPBText,
                                     ord(tr.GetDefaultCompletion)]);
  WriteJnlEntry(LogEntry);
end;

function TFBJournaling.TransactionEnd(TransactionID: integer;
  Completion: TTrCompletionState): boolean;

var LogEntry: AnsiString;
begin
  Result := false;
    case Completion of
    trRolledback:
      begin
        LogEntry := Format(sTransRollbackJnl,[FBFormatDateTime(GetDateTimeFmt,Now),
                                              GetAttachment.GetAttachmentID,
                                              FSessionID,TransactionID]);
        Result := true;
      end;

    trRollbackFailed:
      begin
        LogEntry := Format(sTransRollbackFailJnl,[FBFormatDateTime(GetDateTimeFmt,Now),
                                              GetAttachment.GetAttachmentID,
                                              FSessionID,TransactionID]);
        Result := true;
      end;

    trCommitted:
      begin
        LogEntry := Format(sTransCommitJnl,[FBFormatDateTime(GetDateTimeFmt,Now),
                                            GetAttachment.GetAttachmentID,
                                            FSessionID,TransactionID]);
        Result := true;
      end;

    trCommitFailed:
      begin
        LogEntry := Format(sTransCommitFailJnl,[FBFormatDateTime(GetDateTimeFmt,Now),
                                            GetAttachment.GetAttachmentID,
                                            FSessionID,TransactionID]);
        Result := true;
      end;
    end;
    WriteJnlEntry(LogEntry);
end;

procedure TFBJournaling.TransactionRetained(Tr: ITransaction;
  OldTransactionID: integer; Action: TTransactionAction);
var LogEntry: AnsiString;
begin
    case Action of
      TACommitRetaining:
          LogEntry := Format(sTransCommitRetJnl,[FBFormatDateTime(GetDateTimeFmt,Now),
                                  GetAttachment.GetAttachmentID,
                                  FSessionID,Tr.GetTransactionID,OldTransactionID]);
      TARollbackRetaining:
          LogEntry := Format(sTransRollbackRetJnl,[FBFormatDateTime(GetDateTimeFmt,Now),
                                      GetAttachment.GetAttachmentID,
                                      FSessionID,Tr.GetTransactionID,OldTransactionID]);
    end;
    WriteJnlEntry(LogEntry);

    if not (joNoServerTable in FOptions) then
    try
      FDoNotJournal := true;
      GetAttachment.ExecuteSQL(Tr,sqlRecordJournalEntry,[FSessionID,Tr.GetTransactionID,OldTransactionID]);
    finally
      FDoNotJournal := false;
   end;
end;

procedure TFBJournaling.ExecQuery(Stmt: IStatement);
var SQL: AnsiString;
    LogEntry: AnsiString;
begin
  SQL := TQueryProcessor.Execute(Stmt);
  LogEntry := Format(sQueryJournal,[FBFormatDateTime(GetDateTimeFmt,Now),
                                      GetAttachment.GetAttachmentID,
                                      FSessionID,
                                      Stmt.GetTransaction.GetTransactionID,
                                      Length(SQL),SQL]);
  WriteJnlEntry(LogEntry);
end;

procedure TFBJournaling.ExecImmediateJnl(sql: AnsiString; tr: ITransaction);
var LogEntry: AnsiString;
begin
  LogEntry := Format(sQueryJournal,[FBFormatDateTime(GetDateTimeFmt,Now),
                                      GetAttachment.GetAttachmentID,
                                      FSessionID,
                                      tr.GetTransactionID,
                                      Length(sql),sql]);
  WriteJnlEntry(LogEntry);
end;

constructor TFBJournaling.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TFBJournaling.Destroy;
begin
  if FCriticalSection <> nil then
    FCriticalSection.Free;
  inherited Destroy;
end;

function TFBJournaling.JournalingActive: boolean;
begin
  Result := (FJournalFileStream <> nil) and not FDoNotJournal;
end;

function TFBJournaling.GetJournalOptions: TJournalOptions;
begin
  Result := FOptions;
end;

function TFBJournaling.StartJournaling(aJournalLogFile: AnsiString): integer;
begin
  Result := StartJournaling(aJournalLogFile,[joReadWriteTransactions,joModifyQueries]);
end;

function TFBJournaling.StartJournaling(aJournalLogFile: AnsiString;
  Options: TJournalOptions): integer;
begin
  try
    StartJournaling(TFileStream.Create(aJournalLogFile,fmCreate),Options);
    FOwnsJournal := true;
  finally
    FJournalFilePath := aJournalLogFile;
  end;
end;

function TFBJournaling.StartJournaling(S: TStream; Options: TJournalOptions
  ): integer;
begin
  FOptions := Options;
  if not (joNoServerTable in FOptions) then
  with GetAttachment do
  begin
    if  not HasTable(sJournalTableName) then
    begin
      ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateJournalTable);
      ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateSequence);
    end;
    FSessionID := OpenCursorAtStart(sqlGetNextSessionID)[0].AsInteger;
  end;
  FJournalFileStream := S;
  FOwnsJournal := false;
  Result := FSessionID;
end;

procedure TFBJournaling.StopJournaling(RetainJournal: boolean);
begin
  EndSession(RetainJournal);
end;

{ TFBAttachment }

procedure TFBAttachment.NeedConnectionInfo;

  function AsCP_NONE(s: RawByteString): RawByteString;
  begin
    Result := s;
    SetCodePage(Result,CP_NONE,false);
  end;

var Stmt: IStatement;
    ResultSet: IResultSet;
    Param: IDPBItem;
begin
  if not IsConnected or FHasConnectionInfo or FInNeedConnectionInfo then Exit;
  FInNeedConnectionInfo := true;
  try
    NeedDBInfo;
    FCharSetID := 0;
    FRemoteProtocol := '';
    FAuthMethod := 'Legacy_Auth';
    FSecDatabase := 'Default';
    if FODSMajorVersion > 11 then
    begin
      Stmt := Prepare(StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit),
                      AsCP_NONE('Select MON$CHARACTER_SET_ID, MON$REMOTE_PROTOCOL, MON$AUTH_METHOD, MON$SEC_DATABASE From MON$ATTACHMENTS, MON$DATABASE '+
                      'Where MON$ATTACHMENT_ID = CURRENT_CONNECTION '));
      ResultSet := Stmt.OpenCursor;
      if ResultSet.FetchNext then
      begin
        FCharSetID := ResultSet[0].AsInteger;
        FRemoteProtocol := Trim(ResultSet[1].AsString);
        FAuthMethod := Trim(ResultSet[2].AsString);
        FSecDatabase := Trim(ResultSet[3].AsString);
      end
    end
    else
    if (FODSMajorVersion = 11) and (FODSMinorVersion >= 1) then
    begin
      Stmt := Prepare(StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit),
                      AsCP_NONE('Select MON$CHARACTER_SET_ID, MON$REMOTE_PROTOCOL From MON$ATTACHMENTS '+
                      'Where MON$ATTACHMENT_ID = CURRENT_CONNECTION'));
      ResultSet := Stmt.OpenCursor;
      if ResultSet.FetchNext then
      begin
        FCharSetID := ResultSet[0].AsInteger;
        FRemoteProtocol := Trim(ResultSet[1].AsString);
      end
    end
    else
    if DPB <> nil then
    begin
      Param :=  DPB.Find(isc_dpb_lc_ctype);
      if (Param = nil) or not CharSetName2CharSetID(Param.AsString,FCharSetID) then
        FCharSetID := 0;
      case GetProtocol(FDatabaseName) of
      TCP:       FRemoteProtocol := 'TCPv4';
      Local:     FRemoteProtocol := '';
      NamedPipe: FRemoteProtocol := 'Netbui';
      SPX:       FRemoteProtocol := 'SPX'
      end;
    end;
    FHasDefaultCharSet := CharSetID2CodePage(FCharSetID,FCodePage) and (FCharSetID > 1);
    FHasConnectionInfo := true;
  finally
    FInNeedConnectionInfo := false;
  end;
end;

procedure TFBAttachment.NeedDBInfo;
var DBInfo: IDBInformation;
    i: integer;
begin
  if not IsConnected or (FAttachmentID > 0) then Exit;
  DBInfo := GetDBInformation([isc_info_db_id,isc_info_ods_version,isc_info_ods_minor_version,
                               isc_info_db_SQL_Dialect, isc_info_attachment_id]);
  for i := 0 to DBInfo.GetCount - 1 do
    with DBInfo[i] do
      case getItemType of
      isc_info_ods_minor_version:
        FODSMinorVersion := getAsInteger;
      isc_info_ods_version:
        FODSMajorVersion := getAsInteger;
      isc_info_db_SQL_Dialect:
        FSQLDialect := getAsInteger;
      isc_info_attachment_id:
        FAttachmentID := getAsInteger;
      end;
end;

function TFBAttachment.GetCodePage: TSystemCodePage;
begin
  NeedConnectionInfo;
  Result := FCodePage;
end;

constructor TFBAttachment.Create(api: TFBClientAPI; DatabaseName: AnsiString;
  DPB: IDPB; RaiseExceptionOnConnectError: boolean);
begin
  inherited Create;
  FFirebirdAPI := api.GetAPI; {Keep reference to interface}
  FSQLDialect := 3;
  FDatabaseName := DatabaseName;
  SetLength(FUserCharSetMap,0);
  ClearCachedInfo;
  FInlineBlobLimit := DefaultMaxInlineBlobLimit;
  FDPB := DPB;
  FCodePage := CP_NONE;
  FRaiseExceptionOnConnectError := RaiseExceptionOnConnectError;
end;

procedure TFBAttachment.ClearCachedInfo;
begin
  FHasDefaultCharSet := false;
  FAttachmentID := 0;
  FODSMajorVersion := 0;
  FODSMinorVersion := 0;
  FCodePage := CP_NONE;
  FCharSetID := 0;
  FRemoteProtocol := '';
  FAuthMethod := '';
  FSecDatabase := '';
  FHasConnectionInfo := false;
end;

function TFBAttachment.GenerateCreateDatabaseSQL(DatabaseName: AnsiString;  aDPB: IDPB): AnsiString;
var CreateParams: AnsiString;
    DPBItem: IDPBItem;
begin
  CreateParams := '';

  if aDPB <> nil then
  begin
    DPBItem :=  aDPB.Find(isc_dpb_user_name);
    if DPBItem <> nil then
      CreateParams := CreateParams + ' USER ''' + DPBItem.AsString + '''';

    DPBItem :=  aDPB.Find(isc_dpb_password);
    if DPBItem <> nil then
      CreateParams := CreateParams + ' Password ''' + DPBItem.AsString + '''';

    DPBItem :=  aDPB.Find(isc_dpb_page_size);
    if DPBItem <> nil then
      CreateParams := CreateParams + ' PAGE_SIZE ' + DPBItem.AsString;

    DPBItem :=  aDPB.Find(isc_dpb_lc_ctype);
    if DPBItem <> nil then
      CreateParams := CreateParams + ' DEFAULT CHARACTER SET ' + DPBItem.AsString;

    DPBItem :=  aDPB.Find(isc_dpb_sql_dialect);
    if DPBItem <> nil then
      FSQLDialect := DPBItem.AsInteger;
  end;

  Result := 'CREATE DATABASE ''' + DatabaseName + ''' ' + CreateParams; {do not localize}
end;

procedure TFBAttachment.EndAllTransactions;
var i: integer;
    intf: TInterfacedObject;
begin
  for i := 0 to InterfaceCount - 1 do
  begin
    intf := GetInterface(i);
    if (intf <> nil) and  (intf is TFBTransaction) then
      TFBTransaction(intf).DoDefaultTransactionEnd(true);
  end;
end;

{$IFDEF HASREQEX}
procedure TFBAttachment.DPBFromCreateSQL(CreateSQL: AnsiString);
var RegexObj: TRegExpr;
begin
  FDPB := FFirebirdAPI.AllocateDPB;
  RegexObj := TRegExpr.Create;
  try
    {extact database file spec}
    RegexObj.ModifierG := false; {turn off greedy matches}
    RegexObj.ModifierI := true; {case insensitive match}
    RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +''.*'' +USER +''(.+)'' PASSWORD +''(.+)''';
    if RegexObj.Exec(CreateSQL) then
    begin
      DPB.Add(isc_dpb_user_name).AsString := system.copy(CreateSQL,RegexObj.MatchPos[2],RegexObj.MatchLen[2]);
      DPB.Add(isc_dpb_password).AsString := system.copy(CreateSQL,RegexObj.MatchPos[3],RegexObj.MatchLen[3]);
    end
    else
    begin
      RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +(''.*'') +USER +''(.+)''';
      if RegexObj.Exec(CreateSQL) then
        DPB.Add(isc_dpb_user_name).AsString := system.copy(CreateSQL,RegexObj.MatchPos[2],RegexObj.MatchLen[2]);
    end;
  finally
    RegexObj.Free;
  end;
  if FCharSetID > 0 then
    DPB.Add(isc_dpb_lc_ctype).AsString := GetCharSetName(FCharSetID);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(FSQLDialect);
end;
{$ELSE}
procedure TFBAttachment.DPBFromCreateSQL(CreateSQL: AnsiString);
begin
  FDPB := FFirebirdAPI.AllocateDPB;
  if FCharSetID > 0 then
    DPB.Add(isc_dpb_lc_ctype).AsString := GetCharSetName(FCharSetID);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(FSQLDialect);
end;
{$ENDIF}

procedure TFBAttachment.SetParameters(SQLParams: ISQLParams;
  params: array of const);
var i: integer;
begin
  if SQLParams.Count <> Length(params) then
    IBError(ibxeInvalidParamCount,[SQLParams.Count,Length(params)]);

  for i := 0 to High(params) do
  begin
    case params[i].vtype of
      vtinteger    :
        SQLParams[i].AsInteger := params[i].vinteger;
      vtInt64:
        SQLParams[i].AsInt64 := params[i].VInt64^;
      {$IF declared (vtQWord)}
      vtQWord:
        SQLParams[i].AsInt64 := params[i].VQWord^;
      {$IFEND}
      vtboolean    :
        SQLParams[i].AsBoolean :=  params[i].vboolean;
      vtchar       :
        SQLParams[i].AsString := params[i].vchar;
      vtextended   :
        case SQLParams[i].getColMetadata.SQLType of
          SQL_TYPE_DATE, SQL_TYPE_TIME,SQL_TIMESTAMP,
          SQL_TIMESTAMP_TZ_EX,SQL_TIME_TZ_EX,
          SQL_TIMESTAMP_TZ,SQL_TIME_TZ:
            SQLParams[i].AsDateTime := params[i].VExtended^;
        else
          SQLParams[i].AsDouble := params[i].VExtended^;
        end;
      vtCurrency:
        SQLParams[i].AsDouble := params[i].VCurrency^;
      vtString     :
        SQLParams[i].AsString := strpas(PChar(params[i].VString));
      vtPChar      :
        SQLParams[i].AsString := strpas(params[i].VPChar);
      vtAnsiString :
        SQLParams[i].AsString := strpas(PAnsiChar(params[i].VAnsiString));
      vtVariant:
        SQLParams[i].AsVariant := params[i].VVariant^;
      vtWideChar:
        SQLParams[i].AsString := UTF8Encode(WideCharLenToString(@params[i].VWideChar,1));
      vtPWideChar:
        SQLParams[i].AsString := UTF8Encode(strpas(PWideChar(params[i].VPWideChar)));
      vtWideString:
        SQLParams[i].AsString := UTF8Encode(strpas(PWideChar(params[i].VWideString)));
      vtUnicodeString:
        SQLParams[i].AsString := UTF8Encode(strpas(PWideChar(params[i].VUnicodeString)));
    else
        IBError(ibxeInvalidVariantType,[nil]);
    end;
  end;
end;

procedure TFBAttachment.SetSQLDialect(aValue: integer);
begin
  FSQLDialect := aValue;
end;

procedure TFBAttachment.UseServerICUChanged;
begin
  // Do nothing by default
end;

procedure TFBAttachment.IBDataBaseError;
begin
  raise EIBInterBaseError.Create(getFirebirdAPI.GetStatus,CodePage);
end;

destructor TFBAttachment.Destroy;
begin
  Disconnect(true);
  inherited Destroy;
end;

procedure TFBAttachment.Disconnect(Force: boolean);
begin
  inherited Disconnect(Force);
  ClearCachedInfo;
end;

procedure TFBAttachment.DropDatabase;
begin
  ClearCachedInfo;
end;

function TFBAttachment.getFirebirdAPI: IFirebirdAPI;
begin
  Result := FFirebirdAPI;
end;

function TFBAttachment.getDPB: IDPB;
begin
  Result := FDPB;
end;

function TFBAttachment.AllocateBPB: IBPB;
begin
  Result := TBPB.Create(FFirebirdAPI as TFBClientAPI);
end;

function TFBAttachment.AllocateDIRB: IDIRB;
begin
  Result := TDIRB.Create(FFirebirdAPI as TFBClientAPI);
end;

procedure TFBAttachment.ExecImmediate(TPB: array of byte; sql: AnsiString;
  aSQLDialect: integer);
var tr: ITransaction;
begin
  tr := StartTransaction(TPB,taCommit);
  try
    ExecImmediate(tr,sql,aSQLDialect);
    tr.Commit;
  except
    tr.Rollback(true);
    raise;
  end;
end;

procedure TFBAttachment.ExecImmediate(transaction: ITransaction; sql: AnsiString);
begin
  ExecImmediate(transaction,sql,FSQLDialect);
end;

procedure TFBAttachment.ExecImmediate(TPB: array of byte; sql: AnsiString);
begin
  ExecImmediate(TPB,sql,FSQLDialect);
end;

function TFBAttachment.ExecuteSQL(TPB: array of byte; sql: AnsiString;
  SQLDialect: integer; params: array of const): IResults;
var tr: ITransaction;
begin
  tr := StartTransaction(TPB,taCommit);
  try
    Result := ExecuteSQL(tr,sql,SQLDialect,params);
    tr.CommitRetaining;
  except
    tr.Rollback(true);
    raise;
  end;
end;

function TFBAttachment.ExecuteSQL(transaction: ITransaction; sql: AnsiString;
  SQLDialect: integer; params: array of const): IResults;
begin
  with Prepare(transaction,sql,SQLDialect) do
  begin
    SetParameters(SQLParams,params);
    Result := Execute;
  end;
end;

function TFBAttachment.ExecuteSQL(TPB: array of byte; sql: AnsiString;
  params: array of const): IResults;
begin
   Result := ExecuteSQL(TPB,sql,FSQLDialect,params);
end;

function TFBAttachment.ExecuteSQL(transaction: ITransaction; sql: AnsiString;
  params: array of const): IResults;
begin
  Result := ExecuteSQL(transaction,sql,FSQLDialect,params);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer; Scrollable: boolean): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect,Scrollable,[]);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer; params: array of const): IResultSet;

begin
  Result := OpenCursor(transaction,sql,FSQLDialect,false,params);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: AnsiString;
  Scrollable: boolean): IResultSet;
begin
  Result := OpenCursor(transaction,sql,FSQLDialect,Scrollable,[]);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: AnsiString;
  params: array of const): IResultSet;
begin
  Result := OpenCursor(transaction,sql,FSQLDialect,false,params);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: AnsiString;
  Scrollable: boolean; params: array of const): IResultSet;
begin
  Result := OpenCursor(transaction,sql,FSQLDialect,Scrollable,params);
end;

function TFBAttachment.OpenCursor(transaction: ITransaction; sql: AnsiString;
  aSQLDialect: integer; Scrollable: boolean;
  params: array of const): IResultSet;
var Statement: IStatement;
begin
  CheckHandle;
  Statement := Prepare(transaction,sql,aSQLDialect);
  SetParameters(Statement.SQLParams,params);
  Result := Statement.OpenCursor(Scrollable);
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: AnsiString; aSQLDialect: integer; Scrollable: boolean): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect,Scrollable,[]);
  Result.FetchNext;
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: AnsiString; aSQLDialect: integer; params: array of const): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect,params);
  Result.FetchNext;
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: AnsiString; aSQLDialect: integer; Scrollable: boolean;
  params: array of const): IResultSet;
begin
  Result := OpenCursor(transaction,sql,aSQLDialect,Scrollable,params);
  Result.FetchNext;
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: AnsiString; Scrollable: boolean): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect,Scrollable,[]);
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: AnsiString; params: array of const): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect,params);
end;

function TFBAttachment.OpenCursorAtStart(transaction: ITransaction;
  sql: AnsiString; Scrollable: boolean; params: array of const): IResultSet;
begin
  Result := OpenCursorAtStart(transaction,sql,FSQLDialect,Scrollable,params);
end;

function TFBAttachment.OpenCursorAtStart(sql: AnsiString; Scrollable: boolean
  ): IResultSet;
begin
  Result := OpenCursorAtStart(sql,Scrollable,[]);
end;

function TFBAttachment.OpenCursorAtStart(sql: AnsiString; Scrollable: boolean;
  params: array of const): IResultSet;
var tr: ITransaction;
begin
  tr := StartTransaction([isc_tpb_read,isc_tpb_wait,isc_tpb_concurrency],taCommit);
  try
    Result := OpenCursorAtStart(tr,sql,FSQLDialect,Scrollable,params);
    tr.CommitRetaining;
  except
    tr.Rollback(true);
    raise;
  end;
end;

function TFBAttachment.OpenCursorAtStart(sql: AnsiString;
  params: array of const): IResultSet;
begin
  Result := OpenCursorAtStart(sql,false,params);
end;

function TFBAttachment.Prepare(transaction: ITransaction; sql: AnsiString;
  CursorName: AnsiString): IStatement;
begin
  Result := Prepare(transaction,sql,FSQLDialect,CursorName);
end;

function TFBAttachment.PrepareWithNamedParameters(transaction: ITransaction;
  sql: AnsiString; GenerateParamNames: boolean; CaseSensitiveParams: boolean;
  CursorName: AnsiString): IStatement;
begin
  Result := PrepareWithNamedParameters(transaction,sql,FSQLDialect,GenerateParamNames,CaseSensitiveParams,CursorName);
end;

function TFBAttachment.GetEventHandler(Event: AnsiString): IEvents;
var S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Add(Event);
    Result := GetEventHandler(S);
  finally
    S.Free;
  end;
end;

function TFBAttachment.GetSQLDialect: integer;
begin
  NeedDBInfo;
  Result := FSQLDialect;
end;

function TFBAttachment.GetAttachmentID: integer;
begin
  NeedDBInfo;
  Result := FAttachmentID;
end;

function TFBAttachment.CreateBlob(transaction: ITransaction; RelationName,
  ColumnName: AnsiString; BPB: IBPB): IBlob;
begin
  Result := CreateBlob(transaction,GetBlobMetaData(Transaction,RelationName,ColumnName),BPB);
end;

function TFBAttachment.OpenBlob(transaction: ITransaction; RelationName,
  ColumnName: AnsiString; BlobID: TISC_QUAD; BPB: IBPB): IBlob;
begin
  Result := OpenBlob(Transaction,
                GetBlobMetaData(Transaction,RelationName,ColumnName),
                BlobID,BPB);
end;

function TFBAttachment.OpenBlob(transaction: ITransaction; Field: ISQLData;
  BPB: IBPB): IBlob;
begin
  Result := OpenBlob(Transaction,Field.GetBlobMetadata, Field.AsQuad,BPB);
end;

function TFBAttachment.CreateArray(transaction: ITransaction; RelationName,
  ColumnName: AnsiString): IArray;
begin
  Result := CreateArray(transaction,GetArrayMetaData(transaction,RelationName,ColumnName));
end;

function TFBAttachment.OpenArray(transaction: ITransaction; RelationName,
  ColumnName: AnsiString; ArrayID: TISC_QUAD): IArray;
begin
  Result := OpenArray(transaction,
    GetArrayMetaData(transaction,RelationName,ColumnName),ArrayID);
end;

function TFBAttachment.GetDBInformation(Requests: array of byte
  ): IDBInformation;
var ReqBuffer: PByte;
    i: integer;
begin
  CheckHandle;
  if Length(Requests) = 1 then
    Result := GetDBInformation(Requests[0])
  else
  begin
    GetMem(ReqBuffer,Length(Requests));
    try
      for i := 0 to Length(Requests) - 1 do
        ReqBuffer[i] := Requests[i];

      Result := GetDBInfo(ReqBuffer,Length(Requests));

    finally
      FreeMem(ReqBuffer);
    end;
  end;
end;

function TFBAttachment.GetDBInformation(Request: byte): IDBInformation;
begin
  CheckHandle;
  Result := GetDBInfo(@Request,1);
end;

function TFBAttachment.GetDBInformation(Requests: IDIRB): IDBInformation;
begin
  CheckHandle;
  with Requests as TDIRB do
    Result := GetDBInfo(getBuffer,getDataLength);
end;

function TFBAttachment.GetConnectString: AnsiString;
begin
  Result := FDatabaseName;
end;

function TFBAttachment.GetRemoteProtocol: AnsiString;
begin
  NeedConnectionInfo;
  Result := FRemoteProtocol;
end;

function TFBAttachment.GetAuthenticationMethod: AnsiString;
begin
  NeedConnectionInfo;
  Result := FAuthMethod;
end;

function TFBAttachment.GetSecurityDatabase: AnsiString;
begin
  NeedConnectionInfo;
  Result := FSecDatabase;
end;

function TFBAttachment.GetODSMajorVersion: integer;
begin
  NeedDBInfo;
  Result := FODSMajorVersion;
end;

function TFBAttachment.GetODSMinorVersion: integer;
begin
  NeedDBInfo;
  Result := FODSMinorVersion;
end;

function TFBAttachment.GetCharSetID: integer;
begin
  NeedConnectionInfo;
  Result := FCharSetID;
end;

function TFBAttachment.HasDecFloatSupport: boolean;
begin
  Result := false;
end;

function TFBAttachment.GetInlineBlobLimit: integer;
begin
  Result := FInlineBlobLimit;
end;

procedure TFBAttachment.SetInlineBlobLimit(limit: integer);
begin
  if limit > 32*1024 then
     FInlineBlobLimit := 32*1024
  else
    FInlineBlobLimit := limit;
end;

function TFBAttachment.HasBatchMode: boolean;
begin
  Result := false;
end;

function TFBAttachment.HasTable(aTableName: AnsiString): boolean;
begin
  Result := OpenCursorAtStart(
       'Select count(*) From RDB$RELATIONS Where RDB$RELATION_NAME = ?',
          [aTableName])[0].AsInteger > 0;
end;

function TFBAttachment.HasFunction(aFunctionName: AnsiString): boolean;
begin
  Result := OpenCursorAtStart(
       'Select count(*) From RDB$FUNCTIONS Where RDB$FUNCTION_NAME = ?',
          [aFunctionName])[0].AsInteger > 0;
end;

function TFBAttachment.HasProcedure(aProcName: AnsiString): boolean;
begin
  Result := OpenCursorAtStart(
       'Select count(*) From RDB$PROCEDURES Where RDB$PROCEDURE_NAME = ?',
          [aProcName])[0].AsInteger > 0;
end;

function TFBAttachment.HasDefaultCharSet: boolean;
begin
  NeedConnectionInfo;
  Result := FHasDefaultCharSet
end;

function TFBAttachment.GetDefaultCharSetID: integer;
begin
  NeedConnectionInfo;
  Result := FCharsetID;
end;

function TFBAttachment.GetCharsetName(CharSetID: integer): AnsiString;
var i: integer;
begin
  Result := '';
  if (CharSetID >= Low(CharSetMap)) and (CharSetID <= High(CharSetMap)) and
                                  (CharSetMap[CharSetID].CharSetID = CharSetID) then
    begin
      Result := CharSetMap[CharSetID].CharSetName;
      Exit;
    end;

  for i := 0 to Length(FUserCharSetMap) - 1 do
    if FUserCharSetMap[i].CharSetID = CharSetID then
    begin
      Result := FUserCharSetMap[i].CharSetName;
      Exit;
    end;
end;

function TFBAttachment.CharSetID2CodePage(CharSetID: integer;
  var CodePage: TSystemCodePage): boolean;
var i: integer;
begin
  Result := (CharSetID >= Low(CharSetMap)) and (CharSetID <= High(CharSetMap))
               and (CharSetMap[CharSetID].CharSetID = CharSetID);
  if Result then
    begin
      CodePage := CharSetMap[CharSetID].CodePage;
      Result := true;
      Exit;
    end;

  for i := 0 to Length(FUserCharSetMap) - 1 do
    if FUserCharSetMap[i].CharSetID = CharSetID then
    begin
      CodePage := FUserCharSetMap[i].CodePage;
      Result := true;
      Exit;
    end;
end;

function TFBAttachment.CodePage2CharSetID(CodePage: TSystemCodePage;
  var CharSetID: integer): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(CharSetMap) to High(CharSetMap) do
    if (CharSetMap[i].AllowReverseLookup) and (CharSetMap[i].CodePage = CodePage) then
    begin
      CharSetID := CharSetMap[i].CharSetID;
      Result := true;
      Exit;
    end;

  for i := 0 to Length(FUserCharSetMap) - 1 do
    if (FUserCharSetMap[i].AllowReverseLookup) and (FUserCharSetMap[i].CodePage = CodePage) then
    begin
      CharSetID := FUserCharSetMap[i].CharSetID;
      Result := true;
      Exit;
    end;
end;

function TFBAttachment.CharSetName2CharSetID(CharSetName: AnsiString;
  var CharSetID: integer): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(CharSetMap) to High(CharSetMap) do
    if AnsiCompareText(CharSetMap[i].CharSetName, CharSetName) = 0 then
    begin
      CharSetID := CharSetMap[i].CharSetID;
      Result := true;
      Exit;
    end;

    for i := 0 to Length(FUserCharSetMap) - 1 do
      if AnsiCompareText(FUserCharSetMap[i].CharSetName, CharSetName) = 0 then
      begin
        CharSetID := FUserCharSetMap[i].CharSetID;
        Result := true;
        Exit;
      end;
end;

function TFBAttachment.CharSetWidth(CharSetID: integer; var Width: integer
  ): boolean;
var i: integer;
begin
  Result := (CharSetID >= Low(CharSetMap)) and (CharSetID <= High(CharSetMap))
               and (CharSetMap[CharSetID].CharSetID = CharSetID);
  if Result then
    begin
      Width := CharSetMap[CharSetID].CharSetWidth;
      Result := true;
      Exit;
    end;

  for i := 0 to Length(FUserCharSetMap) - 1 do
    if FUserCharSetMap[i].CharSetID = CharSetID then
    begin
      Width := FUserCharSetMap[i].CharSetWidth;
      Result := true;
      Exit;
    end;
end;

const
  sqlLookupCharSet = 'Select RDB$CHARACTER_SET_ID, RDB$BYTES_PER_CHARACTER From RDB$CHARACTER_SETS '+
                     'Where RDB$SYSTEM_FLAG = 0 and RDB$CHARACTER_SET_NAME = UPPER(?)';

procedure TFBAttachment.RegisterCharSet(CharSetName: AnsiString;
  CodePage: TSystemCodePage; AllowReverseLookup: boolean; out CharSetID: integer
  );
var CharSets: IResultSet;
    idx: integer;
begin
  if CharSetName2CharSetID(CharSetName,CharSetID) then
    IBError(ibxeCharacterSetExists,[CharSetName]);

  CharSets := OpenCursorAtStart(sqlLookupCharSet,[CharSetName]);
  if CharSets.IsEof then
    IBError(ibxeUnknownUserCharSet,[CharSetName]);

  idx := Length(FUserCharSetMap);
  SetLength(FUserCharSetMap,idx+1);
  FUserCharSetMap[idx].AllowReverseLookup := AllowReverseLookup;
  FUserCharSetMap[idx].CharSetID := CharSets[0].AsInteger;
  FUserCharSetMap[idx].CharSetName := SafeAnsiUpperCase(CharSetName);
  FUserCharSetMap[idx].CharSetWidth := CharSets[1].AsInteger;
  FUserCharSetMap[idx].CodePage := CodePage;
  CharSetID := CharSets[0].AsInteger;
end;

function TFBAttachment.GetTimeZoneServices: ITimeZoneServices;
begin
  IBError(ibxeNotSupported,[]);
end;

function TFBAttachment.HasTimeZoneSupport: boolean;
begin
  Result := false;
end;

{ TDPBItem }

function TDPBItem.getParamTypeName: AnsiString;
begin
  Result := DPBPrefix + DPBConstantNames[getParamType];
end;

{ TDPB }

constructor TDPB.Create(api: TFBClientAPI);
begin
  inherited Create(api);
  FDataLength := 1;
  FBuffer^ := isc_dpb_version1;
end;

function TDPB.GetParamTypeName(ParamType: byte): Ansistring;
begin
  if ParamType <= isc_dpb_last_dpb_constant then
    Result := DPBConstantNames[ParamType]
  else
    Result := '';
end;

{$IFNDEF FPC}
function TDPB.GetDPBParamTypeName(ParamType: byte): Ansistring;
begin
  Result := GetParamTypeName(ParamType);
end;
{$ENDIF}

function TDPB.LookupItemType(ParamTypeName: AnsiString): byte;
var i: byte;
begin
  Result := 0;
  ParamTypeName := LowerCase(ParamTypeName);
  if (Pos(DPBPrefix, ParamTypeName) = 1) then
    Delete(ParamTypeName, 1, Length(DPBPrefix));

  for i := 1 to isc_dpb_last_dpb_constant do
    if (ParamTypeName = DPBConstantNames[i]) then
    begin
      Result := i;
      break;
    end;
end;

end.

