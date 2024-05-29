(*
 *  Firebird UDR Support (fbudr). The fbudr components provide a set of
 *  Pascal language bindings for the Firebird API in support of server
 *  side User Defined Routines (UDRs). The fbudr package is an extension
 *  to the Firebird Pascal API.
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
 *  The Original Code is (C) 2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBUDRController;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$WRITEABLECONST ON}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,FirebirdOOAPI, IB, FBUDRIntf, FBSQLData,
  FB30Statement, IniFiles;

type
    {The Controller is based on the UnloadDetector class in <firebird>/src/include/firebird/UdrCppEngine.h
     It provides the entry point for the library and is also responsible for cleaning
     up on library unload.

     It order to operate similar to a C++ static class such as UnloadDetector, when
     instantiated, it saves a reference to itself as a COM interface in a private
     class var. This is a managed variable and hence automatically initialised to
     nil, and automatically freed on exit.

     The controller also handles the registration of UDRs, access to a configuration
     file and logging.
    }

  TFBUDRControllerLogOption = (loLogFunctions, loLogProcedures, loLogTriggers, loLogFetches,
                            loModifyQueries, loReadOnlyQueries, loDetails);
  TFBUDRControllerLogOptions = set of TFBUDRControllerLogOption;

  { In order to customise log handling, the logging options may be
     statically set by overridding the values of the writable constant
     FBControllerLogOptions, or through configuration file overrides.
  }

  TFBUDRControllerOptions = record
    ModuleName: AnsiString;
    AllowConfigFileOverrides: boolean;
    LogFileNameTemplate: AnsiString;
    ConfigFileNameTemplate: AnsiString;
    ForceWriteLogEntries: boolean;
    LogOptions: TFBUDRControllerLogOptions;
    ThreadSafeLogging: boolean;
  end;

  {LogFileNameTemplate and ConfigFileName macros:
    $LOGDIR = Firebird log directory
    $UDRDIR = Firebird UDR directory
    $TEMP = System temp directory
    $MODULE = Module Name
    $TIMESTAMP = date/time in "yyyymmddhhnnss format
  }

const FBUDRControllerOptions: TFBUDRControllerOptions = (
          ModuleName: 'untitled';
          AllowConfigFileOverrides: false;
          LogFileNameTemplate:'$LOGDIR$TIMESTAMP$MODULE.log';
          ConfigFileNameTemplate: '$UDRDIR$MODULE.conf';
          ForceWriteLogEntries: false;
          LogOptions: [];
          ThreadSafeLogging: false);

{$if declared(TStringArray)}
 FalseStrings: TStringArray = ['false','no'];
 TrueStrings: TStringArray = ['true','yes'];
{$ifend}
type

  { TFBUDRController }

  TFBUDRController = class(TInterfacedObject)
  private
    class var FFBController: IUnknown; {A Managed object and hence should be initialised
                                             by the compiler to nil. Also, guaranteed to be
                                             destoyed on exit. No need for finalization clause.}
    class var FUDRFactoryList: TStringList;
    class var FMyUnloadFlag: boolean;
    function CharSetIDToText(att: IAttachment; id: integer): AnsiString;
    function GetStrValue(item: TColumnMetaData): Ansistring;
    function LogOptionsToStr(aLogOptions: TFBUDRControllerLogOptions
      ): AnsiString;
  private
    const sLogFormat = '@%s:%s';
  private
    FTheirUnloadFlag: FirebirdOOAPI.BooleanPtr;
    FLogStream: TStream;
    FCriticalSection: TCriticalSection;
    FMaster: IMaster;
    FMessageBuffer: AnsiString;
    FConfigFile: TIniFile;
    FJnlOpenAppend: boolean;
    function GetDateTimeFmt: AnsiString;
    procedure RegisterUDRFactories(status: FirebirdOOAPI.IStatus; udrPlugin: FirebirdOOAPI.IUdrPlugin);
    procedure RegisterUDRFactory(status: FirebirdOOAPI.IStatus; udrPlugin: FirebirdOOAPI.IUdrPlugin;
                                         aName: AnsiString; factory: TObject);
    procedure FreeFactoryList;
    procedure LoadConfig;
    function NeedLogStream: boolean;
  public
    constructor Create(status: FirebirdOOAPI.IStatus; udrPlugin: FirebirdOOAPI.IUdrPlugin;
                                         aTheirUnloadFlag: booleanPtr; var aMyUnloadFlag: booleanPtr);
    destructor Destroy; override;
    procedure FBSetStatusFromException(E: Exception; aStatus: FirebirdOOAPI.IStatus);
    function ProcessTemplateMacros(aTemplate: AnsiString): AnsiString;
    procedure WriteToLog(Msg: AnsiString); overload;
    procedure WriteToLog(aTitle: AnsiString; Params: IFBUDRInputParams); overload;
    procedure WriteToLog(aTitle: AnsiString; OutputData: IFBUDROutputData); overload;
    procedure StartJournaling(context: IFBUDRExternalContext);
    function HasConfigFile: boolean;
    function ReadConfigString(Section, Ident, DefaultValue: AnsiString): AnsiString;
    function ReadConfigInteger(Section, Ident: AnsiString; DefaultValue: integer): integer;
    function ReadConfigBool(Section, Ident: AnsiString; DefaultValue: boolean): boolean;
  end;

   { TFBUDRInputParams }

   TFBUDRInputParams = class(TResults,IFBUDRInputParams)
   public
    function ParamExists(Idx: AnsiString): boolean;
    function ByName(Idx: AnsiString): ISQLData ; override;
   end;

   { TFBUDROutputParams }

   TFBUDROutputParams = class(TSQLParams,IFBUDROutputData)
   public
     function FieldExists(Idx: AnsiString): boolean;
     function ByName(Idx: AnsiString): ISQLParam ; override;
   end;

   {TFBUDROutParamsSQLDA subclasses a TIBXINPUTSQLDA. TIBXINPUTSQLDA is defined
    in support of executable statements and is usually used to prepare the
    input parameters to a query. Here, a TFBUDROutParamsSQLDA is used to record the
    values returned by a function or procedure in the "outMsg" buffer provided
    when the UDR is invoked.
   }

   { TFBUDROutParamsSQLDA }

   TFBUDROutParamsSQLDA = class(TIBXINPUTSQLDA) {note semantics reversed}
   private
     FBuffer: PByte;
     FAttachment: IAttachment;
     FTransaction: ITransaction;
   protected
     procedure AllocMessageBuffer(len: integer); override;
     procedure FreeMessageBuffer; override;
     function GetAttachment: IAttachment; override;
     function GetTransaction: ITransaction; override;
     function IsInputDataArea: boolean; override;
   public
     {created with the UDR output metadata and a pointer to the outMsg buffer.}
     constructor Create(context: IFBUDRExternalContext; aMetadata: FirebirdOOAPI.IMessageMetaData; aBuffer: PByte);

     {We override CanChangeMetaData to stop a UDR writer trying to change the output
     metadata and hence invalidate the outMsg buffer.}
     function CanChangeMetaData: boolean; override;

     {Finalise is called after the UDR completes and copies the output variable values
     into the outMsg buffer}
     procedure Finalise;
   end;

     {TFBUDRTriggerNewValuesSQLDA additionally initialises the field values to
      those found in the messagebuffer. This is appropriate for "before" triggers
      only}

   TFBUDRTriggerNewValuesSQLDA = class(TFBUDROutParamsSQLDA)
   public
     {created with the UDR output metadata and a pointer to the outMsg buffer.}
     constructor Create(context: IFBUDRExternalContext; aMetadata: FirebirdOOAPI.IMessageMetaData; aBuffer: PByte);
   end;

   {TFBUDRInParamsSQLDA subclasses TIBXOUTPUTSQLDA. TIBXOUTPUTSQLDA is defined
    in support of executable statements and is usually used to return either
    a singleton or multiple rows from a query. Here, a TFBUDRInParamsSQLDA is used to
    hold the input parameter values to a function, procedure or trigger, as provided
    in the "inMsg" buffer.
   }

   { TFBUDRInParamsSQLDA }

   TFBUDRInParamsSQLDA = class(TIBXOUTPUTSQLDA) {note semantics reversed}
   private
     FBuffer: PByte;
     FAttachment: IAttachment;
     FTransaction: ITransaction;
   protected
     procedure AllocMessageBuffer(len: integer); override;
     procedure FreeMessageBuffer; override;
     function GetAttachment: IAttachment; override;
     function GetTransaction: ITransaction; override;
     function IsInputDataArea: boolean; override;
   public
     {created with the input messge metadata and a pointer to the inMsg buffer}
     constructor Create(context: IFBUDRExternalContext;
       aMetadata: FirebirdOOAPI.IMessageMetaData; aBuffer: PByte);
   end;

  {A TFBUDRFunction object is instantiated by a TUDRFunctionFactory when a
   "newItem" is requested. This is an abstract class and is subclassed by a
   UDR writer for each UDR function required. TFBUDRFunction subclasses and their
   "Routine Name" are registered at initialisation time by the RegisterUDRFunction
   procedure.

  { TFBUDRFunction }

  { TFBUDRFunction }

  TFBUDRFunction = class(FirebirdOOAPI.IExternalFunctionImpl)
  private
    FController: TFBUDRController;
    FName: AnsiString;
    FRoutineMetadata: IFBUDRRoutineMetadata;
    FFirebirdAPI: IFirebirdAPI;
  public
    constructor Create(aController: TFBUDRController;
                       aName: AnsiString;
                       routineMetadata: IFBUDRRoutineMetadata);
  public
    {External Function Implementation}

    {Override getCharSet when the function returns strings in a different charset
     to that used by the database connection.}
    function getCharSet(context: IFBUDRExternalContext): AnsiString; overload; virtual;

   {Execute must be overridden by each subclass in order to define a new UDR
    function. The function returns its output value in a variant. The value
    contained in the variant must be type compatible with the result type in
    the SQL function declaration.}
   function Execute(context: IFBUDRExternalContext;
                    ProcMetadata: IFBUDRProcMetadata;
                    InputParams: IFBUDRInputParams;
                    ResultSQLType: cardinal): variant; overload; virtual;

   {alternatively, you can override this version to return any value that ISQLParam
    can take. The Result is returned as a procedure parameter rather than
    as a function result}
    procedure Execute(context: IFBUDRExternalContext;
                     ProcMetadata: IFBUDRProcMetadata;
                     InputParams: IFBUDRInputParams;
                     ReturnValue: ISQLParam); overload; virtual;

   {setup is a class procedure and called by the UDR's Function factory when its
    setup procedure is called. This is typically called the first time a
    function factory is first used and may be used to initialise any class vars
    used by the Execute function. This is normally only required with stateful
    functions.}
     class procedure setup(context: IFBUDRExternalContext;
                   metadata: IFBUDRRoutineMetadata;
                   inBuilder: IFBUDRMetadataBuilder;
                   outBuilder: IFBUDRMetadataBuilder); virtual;

     procedure InitFunction; virtual;
     property Name: AnsiString read FName;
     property Controller: TFBUDRController read FController;
     property FirebirdAPI: IFirebirdAPI read FFirebirdAPI;
   public
    {IExternalFunction}
    procedure dispose(); override;
    procedure getCharSet(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                                                   name: PAnsiChar; nameSize: Cardinal); overload; override;
    procedure execute(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                                                inMsg: Pointer; outMsg: Pointer); overload; override;
  end;

  TFBUDRFunctionClass = class of TFBUDRFunction;

   {A new instance of TFBUDRFunctionFactory is instantiated for each UDR function
    registered with the controller. It is called by the Firebird engine to "setup"
    each UDR function and to create a new instance of each UDR function.
    TFBUDRFunctionFactory is used internally by the UDR Controller and may be ignored
    by a UDR writer.}

  {see <firebird>/src/plugins/udr_engine/UdrEngine.cpp for how Firebird UDR Engine
   uses the factory classes}

  { TFBUDRFunctionFactory }

  TFBUDRFunctionFactory = class(FirebirdOOAPI.IUdrFunctionFactoryImpl)
  private
    FController: TFBUDRController;
    FName: AnsiString;
    FFunction: TFBUDRFunctionClass;
    procedure SetController(AValue: TFBUDRController);
  public
    constructor Create(aName: AnsiString; aFunction: TFBUDRFunctionClass);
    property Controller: TFBUDRController read FController write SetController;
  public
    {IUdrFunctionFactory}
    procedure dispose(); override;
    procedure setup(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                    metadata: FirebirdOOAPI.IRoutineMetadata;
                    inBuilder: FirebirdOOAPI.IMetadataBuilder;
                    outBuilder: FirebirdOOAPI.IMetadataBuilder); override;
    function newItem(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext; metadata:
                                               FirebirdOOAPI.IRoutineMetadata): FirebirdOOAPI.IExternalFunction; override;
  end;

  {A TFBUDRProcedure object is instantiated by a TUDRPRocedureFactory when a
   "newItem" is requested. This is an abstract class and is the base class for
   UDR Execute Procedures (TUDRExecuteProcedure) and UDR Select Procedures (TUDRSelectProcedure).
   }

  { TFBUDRProcedure }

  TFBUDRProcedure = class(FirebirdOOAPI.IExternalProcedureImpl)
    private
      FController: TFBUDRController;
      FName: AnsiString;
      FRoutineMetadata: IFBUDRRoutineMetadata;
      FRefCount: integer;
      FFirebirdAPI: IFirebirdAPI;
   public
      constructor Create(aController: TFBUDRController;
                       aName: AnsiString;
                       routineMetadata: IFBUDRRoutineMetadata);
      property FirebirdAPI: IFirebirdAPI read FFirebirdAPI;
    public
     {External Procedure Implementation}

     {Override getCharSet when the procedure returns strings in a different charset
     to that used by the database connection.}
    function getCharSet(context: IFBUDRExternalContext): AnsiString; overload; virtual;

    {setup is a class procedure and called by the UDR's Procedure factory when its
     setup procedure is called. This is typically called the first time a
     procedure factory is first used and may be used to initialise any class vars
     used by the procedure. This is normally only required with stateful
     procedures.}
     class procedure setup(context: IFBUDRExternalContext;
                     metadata: IFBUDRRoutineMetadata;
                     inBuilder: IFBUDRMetadataBuilder;
                     outBuilder: IFBUDRMetadataBuilder); virtual;
     procedure InitProcedure; virtual;
     property Name: AnsiString read FName;
     property Controller: TFBUDRController read FController;
   public
      {IExternalProcedure}
      procedure dispose(); override;
      procedure getCharSet(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                                                     name: PAnsiChar; nameSize: Cardinal); overload; override;
    end;

  TFBUDRProcedureClass = class of TFBUDRProcedure;

  {The Firebird UDR model requires that a procedure's "open" function returns a
   FirebirdOOAPI.IExternalResultSet object. The TFBUDRExternalResultsSet is used to implement
   this object and is subclassed separately to provide the results set for
   Execute and Select Procedures.
  }

  { TFBUDRExternalResultsSet }

  TFBUDRExternalResultsSet = class(FirebirdOOAPI.IExternalResultSetImpl)
  private
    FUDRProcedure: TFBUDRProcedure;
    FOutputDataSQLDA: TFBUDROutParamsSQLDA;
    FOutputData: IFBUDROutputData;
  protected
    procedure Close; virtual;
  public
    constructor Create(UDRProcedure: TFBUDRProcedure; context: IFBUDRExternalContext;
                       metadata: FirebirdOOAPI.IMessageMetadata;
                       outMsg: pointer);
    destructor Destroy; override;
    property OutputData: IFBUDROutputData read FOutputData;
  public
    {IExternalResultSetImpl}
    procedure dispose(); override;
  end;

  {TFBUDRSingletonRow subclasses TFBUDRExternalResultsSet in order to provide the
   FirebirdOOAPI.IExternalResultSet object returned by an Execute procedure. In a
   TFBUDRExecuteProcedure, the output parameters are set in the main body of
   the UDR writer's Execute procedure and are returned to the Firebird engine
   via the "fetch" function implemented by this object.
  }

  { TFBUDRSingletonRow }

  TFBUDRSingletonRow = class(TFBUDRExternalResultsSet)
  private
    FFetchCalled: boolean;
  public
    {IExternalResultSetImpl}
    function fetch(status: FirebirdOOAPI.IStatus): Boolean; override;
  end;

  {TFBUDRResultsCursor subclasses TFBUDRExternalResultsSet in order to provide the
   FirebirdOOAPI.IExternalResultSet object returned by s Select procedure. In a
   TUDRSelectProcedure, the work is divided into two methods "open" and "fetch".
   The former initialises procedure, while "fetch" is called to return each row
   in the output dataset. A "close" nethod may also be provided to perform any
   tidy up following the last call to "fetch".

  }
  { TFBUDRResultsCursor }

  TFBUDRResultsCursor = class(TFBUDRExternalResultsSet)
  private
    FDone: boolean;
  protected
     procedure Close; override;
  public
    {IExternalResultSetImpl}
    function fetch(status: FirebirdOOAPI.IStatus): Boolean; override;
  end;

  {TFBUDRExecuteProcedure subclasses a TFBUDRProcedure for a UDR Execute Procedure.
   A TFBUDRExecuteProcedure object is instantiated by a TUDRProcedureFactory when a
   "newItem" is requested. This is an abstract class and is subclassed by a
   UDR writer for each UDR Execute procedure required. TFBUDRExecuteProcedure
   subclasses and their "Routine Name" are registered at initialisation time by
   the RegisterUDRProcedure procedure.
  }

  { TFBUDRExecuteProcedure }

  TFBUDRExecuteProcedure = class(TFBUDRProcedure)
  public
  {Execute must be overridden by each subclass in order to define a new UDR
   Execute Procedure. The procedure may read its input parameter values from the
   InputParams interface, and return the output parameter values using the
   outputData interface.}
    procedure Execute(context: IFBUDRExternalContext;
                      ProcMetadata: IFBUDRProcMetadata;
                      InputParams: IFBUDRInputParams;
                      OutputData: IFBUDROutputData); virtual; abstract;
  public
    {IExternalProcedure}
    function open(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                 inMsg: Pointer; outMsg: Pointer): FirebirdOOAPI.IExternalResultSet; override;
  end;

  {TFBUDRSelectProcedure subclasses a TFBUDRProcedure for a UDR Select Procedure.
    A TFBUDRSelectProcedure object is instantiated by a TUDRProcedureFactory when a
    "newItem" is requested. This is an abstract class and is subclassed by a
    UDR writer for each UDR Select procedure required. TFBUDRSelectProcedure
    subclasses and their "Routine Name" are registered at initialisation time by
    the RegisterUDRProcedure procedure.
   }

   { TFBUDRSelectProcedure }

  TFBUDRSelectProcedure = class(TFBUDRProcedure)
  public
    {open must be overridden by a subclass in order to process the input parameter
    values and to set up the select procedure. }
    procedure open(context: IFBUDRExternalContext;
                     ProcMetadata: IFBUDRProcMetadata;
                     InputParams: IFBUDRInputParams); overload; virtual; abstract;

    {After "open" returns the "fetch" method is called by the Firebird engine
    to return each row in the output dataset. The values of the row's columns
    are retuned in the Outputdata. Fetch returns false to indicate End of data.}
    function fetch(OutputData: IFBUDROutputData): boolean; virtual;

    {The "close" method is called after "fetch" returns false and may be overridden
     in order to tidy up if necessary.}
    procedure close; virtual;
  public
    {IExternalProcedure}
    function open(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                 inMsg: Pointer; outMsg: Pointer): FirebirdOOAPI.IExternalResultSet; overload; override;
  end;

    {A new instance of TFBUDRProcedureFactory is instantiated for each UDR procedure
    registered with the controller. It is called by the Firebird engine to "setup"
    each UDR procedure and to create a new instance of each UDR procedure.
    TFBUDRProcedureFactory is used internally by the UDR Controller and may be ignored
    by a UDR writer.}

  { TFBUDRProcedureFactory }

  TFBUDRProcedureFactory = class(FirebirdOOAPI.IUdrProcedureFactoryImpl)
  private
    FController: TFBUDRController;
    FName: AnsiString;
    FProcedure: TFBUDRProcedureClass;
    procedure SetController(AValue: TFBUDRController);
  public
    constructor Create(aName: AnsiString; aProcedure: TFBUDRProcedureClass);
    property Controller: TFBUDRController read FController write SetController;
  public
    {IUdrProcedureFactory}
    procedure dispose(); override;
    procedure setup(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                    metadata:FirebirdOOAPI.IRoutineMetadata; inBuilder: FirebirdOOAPI.IMetadataBuilder;
                    outBuilder: FirebirdOOAPI.IMetadataBuilder); override;
    function newItem(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                     metadata: FirebirdOOAPI.IRoutineMetadata): IExternalProcedure; override;
  end;

  TFBUDRTriggerAction = (taInsert, taUpdate, taDelete, taConnect, taDisconnect,
                          taTransactionStart, taTransactionCommit, taTransactionRollback,
                          taDDL);
  {A TFBUDRTrigger object is instantiated by a TUDRTriggerFactory when a
   "newItem" is requested. This is an abstract class and is subclassed by a
   UDR writer for each UDR Trigger required. TFBUDRTrigger subclasses and their
   "Routine Name" are registered at initialisation time by the RegisterUDRTrigger
   procedure.}

  { TFBUDRTrigger }

  TFBUDRTrigger = class(FirebirdOOAPI.IExternalTriggerImpl)
  private
    FController: TFBUDRController;
    FName: AnsiString;
    FRoutineMetadata: IFBUDRRoutineMetadata;
    FFirebirdAPI: IFirebirdAPI;
  public
    constructor Create(aController: TFBUDRController;
                       aName: AnsiString;
                       routineMetadata: IFBUDRRoutineMetadata);
  public
    {External Trigger Implementation}

    {Override getCharSet when the Trigger returns (?) strings in a different charset
     to that used by the database connection.}
    function getCharSet(context: IFBUDRExternalContext): AnsiString; overload; virtual;

    {Override AfterTrigger in order to carry out an after trigger's function.
     Separate interfaces  are used to provide the "old" and "new" values of each
     of the parent dataset's columns. Note that these are both read only for an
     after trigger}

    procedure AfterTrigger(context: IFBUDRExternalContext;
                           TriggerMetaData: IFBUDRTriggerMetaData;
                           action: TFBUDRTriggerAction;
                           OldParams: IFBUDRInputParams;
                           NewParams: IFBUDRInputParams); virtual;


    {Override BeforeTrigger in order to carry out a before trigger's function.
     Separate interfaces  are used to provide the "old" and "new" values of each
     of the parent dataset's columns. Note that the new values are writeable for an
     after trigger}

    procedure BeforeTrigger(context: IFBUDRExternalContext;
                           TriggerMetaData: IFBUDRTriggerMetaData;
                           action: TFBUDRTriggerAction;
                           OldParams: IFBUDRInputParams;
                           NewParams: IFBUDROutputData); virtual;


    {Override DatabaseTrigger in order to carry out a before trigger's function.
     Note that a database trigger does not have any input values}

    procedure DatabaseTrigger(context: IFBUDRExternalContext;
                           TriggerMetaData: IFBUDRTriggerMetaData); virtual;

    {setup is a class procedure and called by the UDR's Trigger factory when its
     setup procedure is called. This is typically called the first time a
     trigger factory is first used and may be used to initialise any class vars
     used by the Execute function. This is normally only required with stateful
     triggers.}

    class procedure setup(context: IFBUDRExternalContext;
                   metadata: IFBUDRRoutineMetadata;
                   fieldsBuilder: IFBUDRMetadataBuilder); virtual;

    procedure InitTrigger; virtual;

    property Name: AnsiString read FName;
    property Controller: TFBUDRController read FController;
    property FirebirdAPI: IFirebirdAPI read FFirebirdAPI;
 public
    {IExternalTrigger}
    procedure dispose(); override;
    procedure getCharSet(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                   name: PAnsiChar; nameSize: Cardinal); overload; override;
    procedure execute(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                   action: Cardinal; oldMsg: Pointer; newMsg: Pointer);  overload; override;
  end;

  TFBUDRTriggerClass = class of TFBUDRTrigger;

  {A new instance of TFBUDRTriggerFactory is instantiated for each UDR trigger
   registered with the controller. It is called by the Firebird engine to "setup"
   each UDR trigger and to create a new instance of each UDR trigger.
   TFBUDRTriggerFactory is used internally by the UDR Controller and may be ignored
   by a UDR writer.}

  { TFBUDRTriggerFactory }

  TFBUDRTriggerFactory = class(FirebirdOOAPI.IUdrTriggerFactoryImpl)
  private
    FController: TFBUDRController;
    FName: AnsiString;
    FTrigger: TFBUDRTriggerClass;
    procedure SetController(AValue: TFBUDRController);
  public
    constructor Create(aName: AnsiString; aTrigger: TFBUDRTriggerClass);
    property Controller: TFBUDRController read FController write SetController;
  public
    procedure dispose(); override;
    procedure setup(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                                              metadata: FirebirdOOAPI.IRoutineMetadata;
                                              fieldsBuilder: FirebirdOOAPI.IMetadataBuilder); override;
    function newItem(status: FirebirdOOAPI.IStatus; context: FirebirdOOAPI.IExternalContext;
                                              metadata: FirebirdOOAPI.IRoutineMetadata): FirebirdOOAPI.IExternalTrigger; override;
  end;

  {firebird_udr_plugin is the UDR library entry point and must be exported by the
   library.}

function firebird_udr_plugin(status: FirebirdOOAPI.IStatus; aTheirUnloadFlag: FirebirdOOAPI.BooleanPtr;
                                              udrPlugin: IUdrPlugin): FirebirdOOAPI.BooleanPtr; cdecl;

{The register functions are called at initialisation time to register each function,
 procedure and trigger defined by the library. Note: "aName" is the routine name
 in the corresponding SQL declaration for the function,  procedure or trigger.}

procedure FBRegisterUDRFunction(aName: AnsiString; aFunction: TFBUDRFunctionClass);
procedure FBRegisterUDRProcedure (aName: AnsiString; aProcedure: TFBUDRProcedureClass);
procedure FBRegisterUDRTrigger(aName: AnsiString; aTrigger: TFBUDRTriggerClass);

implementation

uses FBUDRUtils, FBUDRMessage, IBUtils, FBClientAPI, FB30ClientAPI, IBErrorCodes,
  IBExternals;

resourcestring
  SFetchCalled = 'Fetch called for ';
  SOpenExecuteProc = 'Open Execute Procedure ';
  SOpenSelectProc = 'Open Select Procedure ';
  SSetupTrigger = 'Setup Trigger ';
  STriggerDisposed = 'Trigger %s: dispose called';
  STriggerCharset = 'GetCharSet for Trigger %s charset name = "%s"';
  STriggerExecute = 'Execute Trigger ';
  STriggerNew =  'New Field Values:';
  STriggerOld = 'Old Field Values:';
  SProcSetup = 'Setup Procedure ';
  SProcDispose = 'Procedure %s: dispose called with refcount = %d';
  SProcCharset = 'GetCharSet for Procedure %s charset name = "%s"';
  sFuncCreated = 'Function %s created';
  sProcCreated = 'Procedure %s created';
  sTriggerCreated = 'Trigger %s created';
  SInputParams = 'Input Parameters';
  SOutputParams = 'Output Parameters';
  SOutputData = 'Output Parameters with data';
  SFuncDispose = 'Function %s: dispose called';
  SFuncCharset = 'GetCharSet for Function %s charset name = "%s"';
  SFuncExecute =  'Execute Function ';
  SFuncSetup = 'Setup Function ';
  SRoutineMetadata = 'Routine Metadata';
  SExceptionRaised = 'Exception raised';
  SFuncRegister = 'Registering Function ';
  SProcRegister = 'Registering Procedure ';
  STriggerRegister = 'Registering Trigger ';
  SBadLogOptionsStr = 'Malformed LogOptions Config string "%s" at position %d';
  SNoConfigFile = 'Unable to find/load configuration file';
  SReadingConfigFile = 'Reading Configuration File: %s';
  SFuncNotOverridden = 'UDR Function %s is undefined';
  SNoReturnValue = 'Function %s does not have a return value!';
  STriggerIsNotImplemented = 'Trigger %s is not implemented';
  STriggerNewAfter = 'New Field Values after trigger execution';
  SUnknownFieldName = 'Unknown Field Name - %s';
  SEof = 'No More Rows';

function firebird_udr_plugin(status : FirebirdOOAPI.IStatus;
  aTheirUnloadFlag : FirebirdOOAPI.BooleanPtr; udrPlugin : IUdrPlugin
  ) : FirebirdOOAPI.BooleanPtr; cdecl;
begin
  if TFBUDRController.FFBController = nil then
    TFBUDRController.Create(status,udrPlugin,aTheirUnloadFlag,Result); {create a default instance}
end;

procedure RegisterUDRFactory(aName: AnsiString; aFactory: TObject);
begin
  if TFBUDRController.FUDRFactoryList = nil then
    TFBUDRController.FUDRFactoryList := TStringList.Create;

  TFBUDRController.FUDRFactoryList.AddObject(aName,aFactory);
end;

procedure FBRegisterUDRFunction(aName: AnsiString; aFunction: TFBUDRFunctionClass);
begin
  RegisterUDRFactory(aName,TFBUDRFunctionFactory.Create(aName,aFunction));
end;

procedure FBRegisterUDRProcedure(aName: AnsiString;
  aProcedure: TFBUDRProcedureClass);
begin
  RegisterUDRFactory(aName,TFBUDRProcedureFactory.Create(aName,aProcedure));
end;

procedure FBRegisterUDRTrigger(aName: AnsiString; aTrigger: TFBUDRTriggerClass);
begin
  RegisterUDRFactory(aName,TFBUDRTriggerFactory.Create(aName,aTrigger));
end;

{ TFBUDRInputParams }

function TFBUDRInputParams.ParamExists(Idx: AnsiString): boolean;
begin
  Result := FieldExists(Idx);
end;

function TFBUDRInputParams.ByName(Idx: AnsiString): ISQLData;
begin
  Result := inherited ByName(Idx);
  if Result = nil then
    raise Exception.CreateFmt(SUnknownFieldName,[idx]);
end;

{ TFBUDROutputParams }

function TFBUDROutputParams.FieldExists(Idx: AnsiString): boolean;
begin
  Result := ParamExists(Idx);
end;

function TFBUDROutputParams.ByName(Idx: AnsiString): ISQLParam;
begin
  Result := inherited ByName(Idx);
  if Result = nil then
    raise Exception.CreateFmt(SUnknownFieldName,[idx]);
end;

{ TFBUDRTriggerNewValuesSQLDA }

constructor TFBUDRTriggerNewValuesSQLDA.Create(context: IFBUDRExternalContext;
  aMetadata: FirebirdOOAPI.IMessageMetaData; aBuffer: PByte);
var i: integer;
    SQLNullIndicator: PShort;
    data: PByte;
    strlen: integer;
    rs: rawbytestring;
begin
  inherited Create(context,aMetadata,aBuffer);
  for i := 0 to Count - 1 do
  with TIBXSQLVar(Column[i]) do
  begin
    SQLNullIndicator := PShort(aBuffer + aMetaData.getNullOffset(context.getStatus,i));
    context.CheckStatus;
    if SQLNullIndicator^ = -1 then
      IsNull := true
    else
    begin
      IsNull := false;
      data := aBuffer + aMetaData.getOffset(context.getStatus,i);
      context.CheckStatus;
      if SQLType = SQL_VARYING then
      begin
        strlen := (context.GetFirebirdAPI as TFBClientAPI).DecodeInteger(data,2);
        DataLength := strlen;
        setLength(rs,strlen);
        Move((data+2)^,rs[1],strlen);
        SetCodePage(rs,CodePage,false);
        SetString(rs);
      end
      else
        Move(data^,SQLData^,DataLength);
    end;
  end;
end;

{ TFBUDRResultsCursor }

procedure TFBUDRResultsCursor.Close;
begin
  inherited Close;
  if not FDone then
    (FUDRProcedure as TFBUDRSelectProcedure).Close;
    FDone := true;
end;

function TFBUDRResultsCursor.fetch(status: FirebirdOOAPI.IStatus): Boolean;
begin
  Result := false;
  try
    if loLogFetches in FBUDRControllerOptions.LogOptions then
      FUDRProcedure.FController.WriteToLog(SFetchCalled + FUDRProcedure.FName);

    if FOutputDataSQLDA <> nil then
    begin
      Result := (FUDRProcedure as TFBUDRSelectProcedure).fetch(FOutputData);
      if [loLogFetches,loDetails] <= FBUDRControllerOptions.LogOptions then
        if Result then
          FUDRProcedure.FController.WriteToLog(SOutputData,FOutputData)
        else
          FUDRProcedure.FController.WriteToLog(SEof);
      if Result then
        FOutputDataSQLDA.Finalise;
    end
    else
      Result := false;

    if not Result then
    begin
      if not FDone then
        (FUDRProcedure as TFBUDRSelectProcedure).Close;
      FDone := true;
    end;
  except on E: Exception do
    FUDRProcedure.FController.FBSetStatusFromException(E,status);
  end;
end;

{ TFBUDRSingletonRow }

function TFBUDRSingletonRow.fetch(status: FirebirdOOAPI.IStatus): Boolean;
begin
  try
  Result := (FOutputDataSQLDA <> nil) and not FFetchCalled;
  if Result then
  begin
    if [loLogProcedures,loDetails] <= FBUDRControllerOptions.LogOptions then
        FUDRProcedure.FController.WriteToLog(SOutputData,FOutputData);
    FOutputDataSQLDA.Finalise; {copy output row to outMsg}
    FFetchCalled := true;
  end;
  except on E: Exception do
    FUDRProcedure.FController.FBSetStatusFromException(E,status);
  end;
end;

{ TFBUDRExecuteProcedure }

function TFBUDRExecuteProcedure.open(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; inMsg: Pointer; outMsg: Pointer
  ): FirebirdOOAPI.IExternalResultSet;
var aProcMetadata: IFBUDRProcMetadata;
    InputParamsSQLDA: TFBUDRInParamsSQLDA;
    InputParams: IFBUDRInputParams;
    metadata: FirebirdOOAPI.IMessageMetadata;
    FBContext: IFBUDRExternalContext;
    singletonRow: TFBUDRSingletonRow;
begin
  InputParams := nil;
  InputParamsSQLDA := nil;
  Result := nil;
  singletonRow := nil;
  try
    if loLogProcedures in FBUDRControllerOptions.LogOptions then
      FController.WriteToLog(SOpenExecuteProc + FName);

    if FRoutineMetadata.QueryInterface(IFBUDRProcMetadata,aProcMetadata) <> S_OK then
      FBUDRError(ibxeNoProcMetadata,[nil])
    else
    begin
      FBContext := TFBUDRExternalContext.Create(Controller,context);
      FFirebirdAPI := FBContext.GetFirebirdAPI;
      try
        if [loLogProcedures,loDetails] <= FBUDRControllerOptions.LogOptions  then
             FController.WriteToLog((FBContext as TFBUDRExternalContext).AsText);

        if FRoutineMetadata.HasInputMetadata then
        begin
          metadata := (FRoutineMetadata as TFBUDRRoutineMetadata).getInputMetadata;
          try
            InputParamsSQLDA := TFBUDRInParamsSQLDA.Create(FBContext,
                                               metadata,
                                               inMsg);
          finally
            metadata.release;
          end;
        end;

        try
          if InputParamsSQLDA <> nil then
          begin
            InputParams := TFBUDRInputParams.Create(InputParamsSQLDA);
            if [loLogProcedures,loDetails] <= FBUDRControllerOptions.LogOptions then
              FController.WriteToLog(SInputParams,InputParams);
          end;

          metadata := nil;
          if FRoutineMetadata.HasOutputMetadata then
            metadata := (FRoutineMetadata as TFBUDRRoutineMetadata).getOutputMetadata;

          try
            singletonRow := TFBUDRSingletonRow.Create(self, FBContext,
                                                 metadata,
                                                 outMsg);

          finally
            if metadata <> nil then
              metadata.release;
          end;

          Execute(FBContext,aProcMetadata,InputParams,
                                    singletonRow.OutputData);
        finally
          InputParams := nil;
          if InputParamsSQLDA <> nil then
            InputParamsSQLDA.Free;
        end;

      finally
        FFirebirdAPI := nil;
      end;

    end;
    except on E: Exception do
      begin
        if singletonRow <> nil then
          singletonRow.dispose;
        singletonRow := nil;
        FController.FBSetStatusFromException(E,status);
      end;
    end;
    Result := singletonRow.asIExternalResultSet;
end;

{ TFBUDRSelectProcedure }

function TFBUDRSelectProcedure.fetch(OutputData: IFBUDROutputData): boolean;
begin
  Result := false;
end;

procedure TFBUDRSelectProcedure.close;
begin
  //override this method to tidy up once all rows have been returned
end;

function TFBUDRSelectProcedure.open(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; inMsg: Pointer; outMsg: Pointer
  ): FirebirdOOAPI.IExternalResultSet;
var aProcMetadata: IFBUDRProcMetadata;
    InputParamsSQLDA: TFBUDRInParamsSQLDA;
    InputParams: IFBUDRInputParams;
    metadata: FirebirdOOAPI.IMessageMetadata;
    FBContext: IFBUDRExternalContext;
begin
  Result := nil;
  try
    if loLogProcedures in FBUDRControllerOptions.LogOptions then
      FController.WriteToLog(SOpenSelectProc + FName);

    InputParamsSQLDA := nil;
    InputParams := nil;
    if FRoutineMetadata.QueryInterface(IFBUDRProcMetadata,aProcMetadata) <> S_OK then
      FBUDRError(ibxeNoProcMetadata,[nil])
    else
    begin
      FBContext := TFBUDRExternalContext.Create(Controller,context);
      FFirebirdAPI := FBContext.GetFirebirdAPI;
      try
        if [loLogProcedures,loDetails] <= FBUDRControllerOptions.LogOptions then
             FController.WriteToLog((FBContext as TFBUDRExternalContext).AsText);

        if FRoutineMetadata.HasInputMetadata then
        begin
          metadata := (FRoutineMetadata as TFBUDRRoutineMetadata).getInputMetadata;
          try
            InputParamsSQLDA := TFBUDRInParamsSQLDA.Create(FBContext,
                                               metadata,
                                               inMsg);
         finally
            metadata.release;
          end;
        end;

        try
          if InputParamsSQLDA <> nil then
          begin
            InputParams := TFBUDRInputParams.Create(InputParamsSQLDA);
            if [loLogProcedures,loDetails] <= FBUDRControllerOptions.LogOptions then
              FController.WriteToLog(SInputParams,InputParams);
          end;

          metadata := nil;
          if FRoutineMetadata.HasOutputMetadata then
            metadata := (FRoutineMetadata as TFBUDRRoutineMetadata).getOutputMetadata;

          try
            open(FBContext,aProcMetadata,InputParams);
            Result := TFBUDRResultsCursor.Create(self, FBContext,
                                                   metadata,
                                                   outMsg).asIExternalResultSet;
          finally
            if metadata <> nil then
              metadata.release;
          end;
        finally
          InputParams := nil;
          if InputParamsSQLDA <> nil then
            InputParamsSQLDA.Free;
        end;
      finally
        FFirebirdAPI := nil;
      end;
    end;

    except on E: Exception do
      begin
        if Result <> nil then
          Result.dispose;
        Result := nil;
        FController.FBSetStatusFromException(E,status);
      end;
    end;
end;

  { TFBUDRTriggerFactory }

procedure TFBUDRTriggerFactory.SetController(AValue: TFBUDRController);
begin
  if FController = AValue then Exit;
  FController := AValue;
end;

constructor TFBUDRTriggerFactory.Create(aName: AnsiString;
  aTrigger: TFBUDRTriggerClass);
begin
  inherited Create;
  FName := aName;
  FTrigger := aTrigger;
end;

procedure TFBUDRTriggerFactory.dispose();
begin
  Free;
end;

procedure TFBUDRTriggerFactory.setup(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; metadata: FirebirdOOAPI.IRoutineMetadata;
  fieldsBuilder: FirebirdOOAPI.IMetadataBuilder);
var FBRoutineMetadata: IFBUDRRoutineMetadata;
    FBFieldsBuilder: IFBUDRMetadataBuilder;
    FBContext: IFBUDRExternalContext;
begin
  FBFieldsBuilder := nil;
  try
    if loLogTriggers in FBUDRControllerOptions.LogOptions then
      FController.WriteToLog(SSetupTrigger + FName);

    FBContext := TFBUDRExternalContext.Create(Controller,context);

    FBRoutineMetadata := TFBUDRRoutineMetadata.Create(FBContext,metadata);
    if fieldsBuilder <> nil then
      FBFieldsBuilder := TFBUDRMetadataBuilder.Create(FBContext,fieldsBuilder);
    if [loLogTriggers, loDetails] <= FBUDRControllerOptions.LogOptions then
        FController.WriteToLog(SRoutineMetadata + LineEnding + (FBRoutineMetadata as TFBUDRRoutineMetadata).AsText);
    TFBUDRTrigger.setup(FBContext,FBRoutineMetadata,FBFieldsBuilder);
  except on E: Exception do
    FController.FBSetStatusFromException(E,status);
  end;
end;

function TFBUDRTriggerFactory.newItem(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; metadata: FirebirdOOAPI.IRoutineMetadata
  ): FirebirdOOAPI.IExternalTrigger;
var FBRoutineMetadata: IFBUDRRoutineMetadata;
    FBContext: IFBUDRExternalContext;
begin
  try
    FBContext := TFBUDRExternalContext.Create(Controller,context);
    FBRoutineMetadata := TFBUDRRoutineMetadata.Create(FBContext,metadata);
    Result := FTrigger.Create(FController,FName,FBRoutineMetadata).asIExternalTrigger;
  except on E: Exception do
    FController.FBSetStatusFromException(E,status);
  end;
end;

{ TFBUDRTrigger }

constructor TFBUDRTrigger.Create(aController: TFBUDRController;
  aName: AnsiString; routineMetadata: IFBUDRRoutineMetadata);
begin
  inherited Create;
  FName := aName;
  FController := aController;
  FRoutineMetaData := routineMetadata;
  InitTrigger;
  if loLogTriggers in FBUDRControllerOptions.LogOptions then
  begin
    FController.WriteToLog(Format(sTriggerCreated,[aName]));
    if loDetails in FBUDRControllerOptions.LogOptions then
       FController.WriteToLog((FRoutineMetaData as TFBUDRRoutineMetadata).AsText);
  end;
end;

function TFBUDRTrigger.getCharSet(context: IFBUDRExternalContext): AnsiString;
begin
   Result := '';
end;

procedure TFBUDRTrigger.AfterTrigger(context: IFBUDRExternalContext;
  TriggerMetaData: IFBUDRTriggerMetaData; action: TFBUDRTriggerAction;
  OldParams: IFBUDRInputParams; NewParams: IFBUDRInputParams);
begin
  raise Exception.CreateFmt(STriggerIsNotImplemented,[FName]);
end;

procedure TFBUDRTrigger.BeforeTrigger(context: IFBUDRExternalContext;
  TriggerMetaData: IFBUDRTriggerMetaData; action: TFBUDRTriggerAction;
  OldParams: IFBUDRInputParams; NewParams: IFBUDROutputData);
begin
  raise Exception.CreateFmt(STriggerIsNotImplemented,[FName]);
end;

procedure TFBUDRTrigger.DatabaseTrigger(context: IFBUDRExternalContext;
  TriggerMetaData: IFBUDRTriggerMetaData);
begin
  raise Exception.CreateFmt(STriggerIsNotImplemented,[FName]);
end;

class procedure TFBUDRTrigger.setup(context: IFBUDRExternalContext;
  metadata: IFBUDRRoutineMetadata; fieldsBuilder: IFBUDRMetadataBuilder);
begin
  //Override in subclass
end;

procedure TFBUDRTrigger.InitTrigger;
begin
  //override in subclass if necessary
end;

procedure TFBUDRTrigger.dispose();
begin
  if loLogTriggers in FBUDRControllerOptions.LogOptions then
    FController.WriteToLog(Format(STriggerDisposed,[FName]));

  Free;
end;

procedure TFBUDRTrigger.getCharSet(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; name: PAnsiChar; nameSize: Cardinal);
var charset: AnsiString;
    FBContext: IFBUDRExternalContext;
begin
  try
    FBContext := TFBUDRExternalContext.Create(Controller,context);
    FFirebirdAPI := FBContext.GetFirebirdAPI;
    try
      charset := getCharSet(FBContext);
      if charset <> '' then
      begin
        StrPLCopy(name,charset,nameSize);
        if loLogTriggers in FBUDRControllerOptions.LogOptions then
          FController.WriteToLog(Format(STriggerCharset,[FName,charset]));
      end;
    finally
      FFirebirdAPI := nil;
    end;
  except on E: Exception do
    FController.FBSetStatusFromException(E,status);
  end;
end;

procedure TFBUDRTrigger.execute(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; action: Cardinal; oldMsg: Pointer;
  newMsg: Pointer);
var aTriggerMetadata: IFBUDRTriggerMetaData;
    OldParamsSQLDA: TFBUDRInParamsSQLDA;
    NewParamsSQLDA: TFBUDRInParamsSQLDA;
    WritableParamsSQLDA: TFBUDRTriggerNewValuesSQLDA;
    OldParams: IFBUDRInputParams;
    NewParams: IFBUDRInputParams;
    NewWritableParams: IFBUDROutputData;
    TriggerAction: TFBUDRTriggerAction;
    FBContext: IFBUDRExternalContext;

  procedure SetUpOldParams;
  var metadata: FirebirdOOAPI.IMessageMetadata;
  begin
    metadata := (FRoutineMetadata as TFBUDRRoutineMetadata).getTriggerMetadata;
    try
      OldParamsSQLDA := TFBUDRInParamsSQLDA.Create(FBContext,
                                            metadata,
                                            oldMsg);
      OldParams := TFBUDRInputParams.Create(OldParamsSQLDA);
      if [loLogTriggers,loDetails] <= FBUDRControllerOptions.LogOptions then
      begin
        FController.WriteToLog(STriggerOld,OldParams);
      end;
    finally
      metadata.release;
    end;
  end;

  procedure SetupNewParams;
  var metadata: FirebirdOOAPI.IMessageMetadata;
  begin
    metadata := (FRoutineMetadata as TFBUDRRoutineMetadata).getTriggerMetadata;
    try
      NewParamsSQLDA := TFBUDRInParamsSQLDA.Create(FBContext,
                                              metadata,
                                              newMsg);
      NewParams := TFBUDRInputParams.Create(NewParamsSQLDA);
      if [loLogTriggers,loDetails] <= FBUDRControllerOptions.LogOptions then
      begin
        FController.WriteToLog(STriggerNew,NewParams);
      end;
   finally
      metadata.release;
    end;
  end;

  procedure SetupWritableNewParams;
  var metadata: FirebirdOOAPI.IMessageMetadata;
  begin
    metadata := (FRoutineMetadata as TFBUDRRoutineMetadata).getTriggerMetadata;
    try
      WritableParamsSQLDA := TFBUDRTriggerNewValuesSQLDA.Create(FBContext,
                                          metadata,
                                          newMsg);
      NewWritableParams := TFBUDROutputParams.Create(WritableParamsSQLDA);
      if [loLogTriggers,loDetails] <= FBUDRControllerOptions.LogOptions then
      begin
        FController.WriteToLog(STriggerNew,NewWritableParams);
      end;
    finally
      metadata.release;
    end;
  end;

begin
  try
    if loLogTriggers in FBUDRControllerOptions.LogOptions then
      FController.WriteToLog(STriggerExecute + FName);
    OldParamsSQLDA := nil;
    NewParamsSQLDA := nil;
    WritableParamsSQLDA := nil;
    OldParams := nil;
    NewParams := nil;
    NewWritableParams := nil;

    if FRoutineMetadata.QueryInterface(IFBUDRTriggerMetaData,aTriggerMetadata) <> S_OK then
      FBUDRError(ibxeNoTriggerMetadata,[nil])
    else
    begin
      FBContext := TFBUDRExternalContext.Create(Controller,context);
      FFirebirdAPI := FBContext.GetFirebirdAPI;
      try
        with FirebirdOOAPI.IExternalTriggerImpl do
        case action of
        ACTION_INSERT:
          TriggerAction := taInsert;
        ACTION_UPDATE:
          TriggerAction := taUpdate;
        ACTION_DELETE:
          TriggerAction := taDelete;
        ACTION_CONNECT:
          TriggerAction := taConnect;
        ACTION_DISCONNECT:
          TriggerAction := taDisconnect;
        ACTION_TRANS_START:
          TriggerAction := taTransactionStart;
        ACTION_TRANS_COMMIT:
          TriggerAction := taTransactionCommit;
        ACTION_TRANS_ROLLBACK:
          TriggerAction := taTransactionRollback;
        ACTION_DDL:
          TriggerAction := taDDL;
        else
          FBUDRError(ibxeUnknownTransactionAction,[action]);
        end;

        case FRoutineMetadata.getTriggerType of
        ttBefore:
          begin
            if FRoutineMetadata.HasTriggerMetadata then
            begin
              if TriggerAction in [taUpdate, taDelete] then
                SetUpOldParams;
              if TriggerAction in [taInsert,taUpdate] then
              SetupWritableNewParams;
            end;
            BeforeTrigger(FBContext,aTriggerMetadata,TriggerAction,OldParams,NewWritableParams);
            WritableParamsSQLDA.Finalise;
            if [loLogTriggers,loDetails] <= FBUDRControllerOptions.LogOptions then
              FController.WriteToLog(STriggerNewAfter,NewWritableParams);
          end;
        ttAfter:
          begin
            if FRoutineMetadata.HasTriggerMetadata then
            begin
              if TriggerAction in [taUpdate, taDelete] then
                SetUpOldParams;
              if TriggerAction in [taInsert,taUpdate] then
                SetupNewParams;
              AfterTrigger(FBContext,aTriggerMetadata,TriggerAction,OldParams,NewParams);
            end;
          end;
        ttDatabase:
          begin
            DatabaseTrigger(FBContext,aTriggerMetadata);
          end;
        end;
      finally
        NewParams := nil;
        OldParams := nil;
        NewWritableParams := nil;
        if OldParamsSQLDA <> nil then
          OldParamsSQLDA.Free;
        if NewParamsSQLDA <> nil then
          NewParamsSQLDA.Free;
        if WritableParamsSQLDA <> nil then
          WritableParamsSQLDA.Free;
        FFirebirdAPI := nil;
        end;
      end;
    except on E: Exception do
      FController.FBSetStatusFromException(E,status);
    end;
end;

{ TFBUDRProcedureFactory }

procedure TFBUDRProcedureFactory.SetController(AValue: TFBUDRController);
begin
  if FController = AValue then Exit;
  FController := AValue;
end;

constructor TFBUDRProcedureFactory.Create(aName: AnsiString;
  aProcedure: TFBUDRProcedureClass);
begin
  inherited Create;
  FName := aName;
  FProcedure := aProcedure;
end;

procedure TFBUDRProcedureFactory.dispose();
begin
  Free;
end;

procedure TFBUDRProcedureFactory.setup(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; metadata: FirebirdOOAPI.IRoutineMetadata;
  inBuilder: FirebirdOOAPI.IMetadataBuilder; outBuilder: FirebirdOOAPI.IMetadataBuilder);
var FBRoutineMetadata: IFBUDRRoutineMetadata;
    FBInBuilder: IFBUDRMetadataBuilder;
    FBOutBuilder: IFBUDRMetadataBuilder;
    FBContext: IFBUDRExternalContext;
begin
  try
    if loLogProcedures in FBUDRControllerOptions.LogOptions then
      FController.WriteToLog(SProcSetup + FName);

    FBInBuilder := nil;
    FBOutBuilder := nil;
    FBContext := TFBUDRExternalContext.Create(Controller,context);

    FBRoutineMetadata := TFBUDRRoutineMetadata.Create(FBContext,metadata);
    if inBuilder <> nil then
      FBInBuilder := TFBUDRMetadataBuilder.Create(FBContext,inBuilder);
    if outBuilder <> nil then
      FBOutBuilder := TFBUDRMetadataBuilder.Create(FBContext,outBuilder);
      if [loLogProcedures, loDetails] <= FBUDRControllerOptions.LogOptions then
        FController.WriteToLog(SRoutineMetadata + LineEnding + (FBRoutineMetadata as TFBUDRRoutineMetadata).AsText);
    TFBUDRProcedure.setup(FBContext,FBRoutineMetadata,FBInBuilder,FBOutBuilder);
  except on E: Exception do
    FController.FBSetStatusFromException(E,status);
  end;
end;

function TFBUDRProcedureFactory.newItem(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; metadata: FirebirdOOAPI.IRoutineMetadata
  ): IExternalProcedure;
var FBRoutineMetadata: IFBUDRRoutineMetadata;
    FBContext: IFBUDRExternalContext;
begin
  try
    FBContext := TFBUDRExternalContext.Create(Controller,context);
    FBRoutineMetadata := TFBUDRRoutineMetadata.Create(FBContext,metadata);
    Result := FProcedure.Create(FController,FName,FBRoutineMetadata).asIExternalProcedure;
  except on E: Exception do
    FController.FBSetStatusFromException(E,status);
  end;
end;

{ TFBUDRExternalResultsSet }

procedure TFBUDRExternalResultsSet.Close;
begin
  //do nothing by default
end;

constructor TFBUDRExternalResultsSet.Create(UDRProcedure: TFBUDRProcedure;
  context: IFBUDRExternalContext; metadata: FirebirdOOAPI.IMessageMetadata;
  outMsg: pointer);
begin
  inherited Create;
  FUDRProcedure := UDRProcedure;
  Inc(FUDRProcedure.FRefCount);
  if metadata <> nil then
  begin
    FOutputDataSQLDA := TFBUDROutParamsSQLDA.Create(context,metadata,outMsg);
    FOutputData :=  TFBUDROutputParams.Create(FOutputDataSQLDA);
    if [loLogProcedures,loDetails] <= FBUDRControllerOptions.LogOptions then
      FUDRProcedure.FController.WriteToLog(SOutputParams,FOutputData);
  end;
end;

destructor TFBUDRExternalResultsSet.Destroy;
begin
  FOutputData := nil;
  if FOutputDataSQLDA <> nil then
    FOutputDataSQLDA.Free;
  inherited Destroy;
end;

procedure TFBUDRExternalResultsSet.dispose();
begin
  Close;
  FUDRProcedure.dispose;
  Free;
end;

{ TFBUDRProcedure }

constructor TFBUDRProcedure.Create(aController: TFBUDRController;
  aName: AnsiString; routineMetadata: IFBUDRRoutineMetadata);
begin
  inherited Create;
  FController := aController;
  FName := aName;
  FRefCount := 1;
  FRoutineMetaData := routineMetadata;
  InitProcedure;
  if loLogProcedures in FBUDRControllerOptions.LogOptions then
  begin
    FController.WriteToLog(Format(sProcCreated,[aName]));
    if loDetails in FBUDRControllerOptions.LogOptions then
       FController.WriteToLog((FRoutineMetaData as TFBUDRRoutineMetadata).AsText);
  end;
end;

function TFBUDRProcedure.getCharSet(context: IFBUDRExternalContext): AnsiString;
begin
  Result := '';
end;

class procedure TFBUDRProcedure.setup(context: IFBUDRExternalContext;
  metadata: IFBUDRRoutineMetadata; inBuilder: IFBUDRMetadataBuilder;
  outBuilder: IFBUDRMetadataBuilder);
begin
  //Override in subclass
end;

procedure TFBUDRProcedure.InitProcedure;
begin
  //override in sublass if necessary
end;

procedure TFBUDRProcedure.dispose();
begin
  if loLogProcedures in FBUDRControllerOptions.LogOptions then
    FController.WriteToLog(Format(SProcDispose,[FName,FRefCount]));

  Dec(FRefCount);
  if FRefCount = 0 then Free;
end;

procedure TFBUDRProcedure.getCharSet(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; name: PAnsiChar; nameSize: Cardinal);
var charset: AnsiString;
    FBContext: IFBUDRExternalContext;
begin
  try
    FBContext := TFBUDRExternalContext.Create(Controller,context);
    FFirebirdAPI := FBContext.GetFirebirdAPI;
    try
      charset := getCharSet(FBContext);
      if charset <> '' then
      begin
        StrPLCopy(name,charset,nameSize);
        if loLogProcedures in FBUDRControllerOptions.LogOptions then
          FController.WriteToLog(Format(SProcCharset,[FName,charset]));
      end;
    finally
      FFirebirdAPI := nil;
    end;
  except on E: Exception do
    FController.FBSetStatusFromException(E,status);
  end;
end;

{ TFBUDRInParamsSQLDA }

procedure TFBUDRInParamsSQLDA.AllocMessageBuffer(len: integer);
begin
  FMessageBuffer := FBuffer;
  FMsgLength := len;
end;

procedure TFBUDRInParamsSQLDA.FreeMessageBuffer;
begin
  FBuffer := nil;
  FMsgLength := 0;
end;

function TFBUDRInParamsSQLDA.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TFBUDRInParamsSQLDA.GetTransaction: ITransaction;
begin
  Result := FTransaction;
end;

function TFBUDRInParamsSQLDA.IsInputDataArea: boolean;
begin
  Result := true;
end;

constructor TFBUDRInParamsSQLDA.Create(context: IFBUDRExternalContext;
  aMetadata: FirebirdOOAPI.IMessageMetaData; aBuffer: PByte);
begin
  inherited Create(context.GetFirebirdAPI);
  FAttachment := context.GetAttachment;
  FTransaction := context.GetTransaction;
  FBuffer := aBuffer;
  Bind(aMetaData);
end;

{ TFBUDROutParamsSQLDA }

procedure TFBUDROutParamsSQLDA.AllocMessageBuffer(len: integer);
begin
  FillChar(FBuffer^,len,0);
  FMessageBuffer := FBuffer;
  FMsgLength := len;
end;

procedure TFBUDROutParamsSQLDA.FreeMessageBuffer;
begin
  FMessageBuffer := nil;
  FMsgLength := 0;
end;

function TFBUDROutParamsSQLDA.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TFBUDROutParamsSQLDA.GetTransaction: ITransaction;
begin
  Result := FTransaction;
end;

function TFBUDROutParamsSQLDA.IsInputDataArea: boolean;
begin
  Result := true;
end;

constructor TFBUDROutParamsSQLDA.Create(context: IFBUDRExternalContext;
  aMetadata: FirebirdOOAPI.IMessageMetaData; aBuffer: PByte);
begin
  inherited Create(context.GetFirebirdAPI);
  FAttachment := context.GetAttachment;
  FTransaction := context.GetTransaction;
  FBuffer := aBuffer;
  Bind(aMetadata);
end;

function TFBUDROutParamsSQLDA.CanChangeMetaData: boolean;
begin
  Result := false;
end;

procedure TFBUDROutParamsSQLDA.Finalise;
begin
  PackBuffer;
end;

{ TFBUDRFunction }

constructor TFBUDRFunction.Create(aController: TFBUDRController;
  aName: AnsiString; routineMetadata: IFBUDRRoutineMetadata);
begin
  inherited Create;
  FController := aController;
  FName := aName;
  FRoutineMetaData := routineMetadata;
  InitFunction;
  if loLogFunctions in FBUDRControllerOptions.LogOptions then
  begin
    FController.WriteToLog(Format(sFuncCreated,[aName]));
    if loDetails in FBUDRControllerOptions.LogOptions then
       FController.WriteToLog((FRoutineMetaData as TFBUDRRoutineMetadata).AsText);
  end;
end;

function TFBUDRFunction.getCharSet(context: IFBUDRExternalContext): AnsiString;
begin
  Result := '';
end;

function TFBUDRFunction.Execute(context: IFBUDRExternalContext;
  ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams;
  ResultSQLType: cardinal): variant;
begin
  raise Exception.CreateFmt(SFuncNotOverridden,[FName]);
end;

procedure TFBUDRFunction.Execute(context: IFBUDRExternalContext;
  ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams;
  ReturnValue: ISQLParam);
begin
  ReturnValue.AsVariant := Execute(context,ProcMetadata,InputParams,ReturnValue.GetSQLType);
end;

class procedure TFBUDRFunction.setup(context: IFBUDRExternalContext;
  metadata: IFBUDRRoutineMetadata; inBuilder: IFBUDRMetadataBuilder;
  outBuilder: IFBUDRMetadataBuilder);
begin
  //Do nothing be default
end;

procedure TFBUDRFunction.InitFunction;
begin
  //override if necessary
end;

procedure TFBUDRFunction.dispose();
begin
  if loLogFunctions in FBUDRControllerOptions.LogOptions then
    FController.WriteToLog(Format(SFuncDispose,[FName]));

  Free;
end;

procedure TFBUDRFunction.getCharSet(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; name: PAnsiChar; nameSize: Cardinal);
var charset: AnsiString;
    FBContext: IFBUDRExternalContext;
begin
  try
    FBContext := TFBUDRExternalContext.Create(Controller,context);
    FFirebirdAPI := FBContext.GetFirebirdAPI;
    try
      charset := getCharSet(FBContext);
      if charset <> '' then
      begin
        StrPLCopy(name,charset,nameSize);
        if loLogFunctions in FBUDRControllerOptions.LogOptions then
          FController.WriteToLog(Format(SFuncCharset,[FName,charset]));
      end;
    finally
      FFirebirdAPI := nil;
    end;
  except on E: Exception do
    FController.FBSetStatusFromException(E,status);
  end;
end;

procedure TFBUDRFunction.execute(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; inMsg: Pointer; outMsg: Pointer);
var aProcMetadata: IFBUDRProcMetadata;
    OutParamsSQLDA: TFBUDROutParamsSQLDA;
    InParamsSQLDA: TFBUDRInParamsSQLDA;
    InputParams: IFBUDRInputParams;
    OutputData: IFBUDROutputData;
    metadata: FirebirdOOAPI.IMessageMetadata;
    FBContext: IFBUDRExternalContext;
begin
  try
    if loLogFunctions in FBUDRControllerOptions.LogOptions then
      FController.WriteToLog(SFuncExecute + FName);

    OutParamsSQLDA := nil;
    InParamsSQLDA := nil;
    InputParams := nil;
    OutputData := nil;
    if FRoutineMetadata.QueryInterface(IFBUDRProcMetadata,aProcMetadata) <> S_OK then
      FBUDRError(ibxeNoProcMetadata,[nil])
    else
    begin
      FBContext := TFBUDRExternalContext.Create(Controller,context);
      FFirebirdAPI := FBContext.GetFirebirdAPI;
      if [loLogFunctions,loDetails] <= FBUDRControllerOptions.LogOptions  then
         FController.WriteToLog((FBContext as TFBUDRExternalContext).AsText);

      try
        if FRoutineMetadata.HasInputMetadata then
        begin
          metadata := (FRoutineMetadata as TFBUDRRoutineMetadata).getInputMetadata;
          try
            InParamsSQLDA := TFBUDRInParamsSQLDA.Create(FBContext,
                                 metadata,
                                 inMsg);
          finally
            metadata.release;
          end;

          InputParams := TFBUDRInputParams.Create(InParamsSQLDA);
          if [loLogFunctions,loDetails] <= FBUDRControllerOptions.LogOptions then
            FController.WriteToLog(SInputParams,InputParams);
        end;

        if FRoutineMetadata.HasOutputMetadata then
        begin
          metadata := (FRoutineMetadata as TFBUDRRoutineMetadata).getOutputMetadata;
          try
            OutParamsSQLDA := TFBUDROutParamsSQLDA.Create(FBContext,
                                       metadata,
                                       outMsg);
          finally
            metadata.release;
          end;
          OutputData := TFBUDROutputParams.Create(OutParamsSQLDA);
        end
        else
          raise Exception.CreateFmt(SNoReturnValue,[FName]);

        Execute(FBContext,aProcMetadata,InputParams,OutputData[0]);

        if [loLogFunctions,loDetails] <= FBUDRControllerOptions.LogOptions then
          FController.WriteToLog(SOutputData,OutputData);

        OutParamsSQLDA.Finalise; {copy result to OutMsg buffer}
      finally
        OutputData := nil;
        InputParams := nil;
        if OutParamsSQLDA <> nil then
          OutParamsSQLDA.Free;
        if InParamsSQLDA <> nil then
          InParamsSQLDA.Free;
        FFirebirdAPI := nil;
      end;
    end;
    except on E: Exception do
      FController.FBSetStatusFromException(E,status);
    end;
end;

{ TFBUDRFunctionFactory }

procedure TFBUDRFunctionFactory.SetController(AValue: TFBUDRController);
begin
  if FController = AValue then Exit;
  FController := AValue;
end;

constructor TFBUDRFunctionFactory.Create(aName: AnsiString;
  aFunction: TFBUDRFunctionClass);
begin
  inherited Create;
  FName := aName;
  FFunction := aFunction;
end;

procedure TFBUDRFunctionFactory.dispose();
begin
  Free;
end;

procedure TFBUDRFunctionFactory.setup(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; metadata: FirebirdOOAPI.IRoutineMetadata;
  inBuilder: FirebirdOOAPI.IMetadataBuilder; outBuilder: FirebirdOOAPI.IMetadataBuilder);
var FBRoutineMetadata: IFBUDRRoutineMetadata;
    FBInBuilder: IFBUDRMetadataBuilder;
    FBOutBuilder: IFBUDRMetadataBuilder;
    FBContext: IFBUDRExternalContext;
begin
  FBInBuilder := nil;
  FBOutBuilder := nil;
  try
    if loLogFunctions in FBUDRControllerOptions.LogOptions then
      FController.WriteToLog(SFuncSetup + FName);

    FBContext := TFBUDRExternalContext.Create(Controller,context);

    FBRoutineMetadata := TFBUDRRoutineMetadata.Create(FBContext,metadata);

    if inBuilder <> nil then
      FBInBuilder := TFBUDRMetadataBuilder.Create(FBContext,inBuilder);
    if outBuilder <> nil then
      FBOutBuilder := TFBUDRMetadataBuilder.Create(FBContext,outBuilder);
    if [loLogFunctions, loDetails] <= FBUDRControllerOptions.LogOptions then
      FController.WriteToLog(SRoutineMetadata + LineEnding + (FBRoutineMetadata as TFBUDRRoutineMetadata).AsText);

    TFBUDRFunction.setup(FBContext,FBRoutineMetadata,FBInBuilder,FBOutBuilder);
  except on E: Exception do
    FController.FBSetStatusFromException(E,status);
  end;
end;

function TFBUDRFunctionFactory.newItem(status: FirebirdOOAPI.IStatus;
  context: FirebirdOOAPI.IExternalContext; metadata: FirebirdOOAPI.IRoutineMetadata
  ): FirebirdOOAPI.IExternalFunction;
var FBRoutineMetadata: IFBUDRRoutineMetadata;
    FBContext: IFBUDRExternalContext;
begin
  try
    FBContext := TFBUDRExternalContext.Create(Controller,context);
    FBRoutineMetadata := TFBUDRRoutineMetadata.Create(FBContext,metadata);
    Result := FFunction.Create(FController,FName,FBRoutineMetadata).asIExternalFunction;
  except on E: Exception do
    FController.FBSetStatusFromException(E,status);
  end;
end;

{ TFBUDRController }

function TFBUDRController.GetDateTimeFmt: AnsiString;
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

function TFBUDRController.ProcessTemplateMacros(aTemplate: AnsiString
  ): AnsiString;

  function CleanDirName(aDirName: PAnsiChar): AnsiString;
  begin
    Result := Trim(strpas(aDirName));
    {$IFDEF WINDOWS}
    Result := StringReplace(Result,'/',DirectorySeparator,[rfReplaceAll]);
    {$ELSE}
    Result := StringReplace(Result,'\',DirectorySeparator,[rfReplaceAll]);
    {$ENDIF}
    if (Length(Result) > 0) and (Result[Length(aDirName)] <> DirectorySeparator) then
      Result := Result + DirectorySeparator;
  end;

var udr_config: FirebirdOOAPI.IConfig;
    config_entry: FirebirdOOAPI.IConfigEntry;
    aStatus: FirebirdOOAPI.IStatus;
begin
  if FMaster <> nil then
  with FMaster.getConfigManager^ do
  begin
    Result := StringReplace(aTemplate,'$LOGDIR',CleanDirName(getDirectory(FirebirdOOAPI.IConfigManagerImpl.DIR_LOG)),[rfReplaceAll, rfIgnoreCase]);
    udr_config := getPluginConfig('UDR');
    if udr_config <> nil then
    try
      aStatus := FMaster.getStatus;
      try
        config_entry := udr_config.find(aStatus,'path');
        with aStatus^ do
          if (getState and FirebirdOOAPI.IStatusImpl.STATE_ERRORS) <> 0 then
            raise EFBUDRException.Create(aStatus);
      finally
        aStatus.dispose;
      end;

      if config_entry <> nil then
      try
        with config_entry^ do
          Result := StringReplace(Result,'$UDRDIR',CleanDirName(config_entry.getValue),[rfReplaceAll, rfIgnoreCase]);
      finally
        config_entry.release;
      end;
    finally
      udr_config.release();
    end;
  end;
  Result := StringReplace(Result,'$TEMP',GetTempDir,[rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result,'$MODULE',FBUDRControllerOptions.ModuleName,[rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result,'$TIMESTAMP',FormatDateTime('yyyymmddhhnnss',Now),[rfReplaceAll, rfIgnoreCase]);
end;

procedure TFBUDRController.RegisterUDRFactories(status: FirebirdOOAPI.IStatus;
  udrPlugin: FirebirdOOAPI.IUdrPlugin);
var i: integer;
begin
  if FUDRFactoryList <> nil then
  for i := 0 to FUDRFactoryList.Count - 1 do
  try
    RegisterUDRFactory(status,udrPlugin,FUDRFactoryList[i], FUDRFactoryList.Objects[i]);
    with status^ do
      if (getState and FirebirdOOAPI.IStatusImpl.STATE_ERRORS) <> 0 then break;
  except on E: Exception do
    FBSetStatusFromException(E,status);
  end;
end;

procedure TFBUDRController.RegisterUDRFactory(status: FirebirdOOAPI.IStatus;
  udrPlugin: FirebirdOOAPI.IUdrPlugin; aName: AnsiString; factory: TObject);
begin
    if factory is TFBUDRFunctionFactory then
    begin
      if loLogFunctions in FBUDRControllerOptions.LogOptions then
        WriteToLog(SFuncRegister + aName);
      udrPlugin.registerFunction(status,PAnsiChar(aName),FirebirdOOAPI.IUdrFunctionFactoryImpl(factory).asIUdrFunctionFactory);
      TFBUDRFunctionFactory(factory).Controller := self;
    end
    else
    if factory is TFBUDRProcedureFactory then
    begin
      if loLogProcedures in FBUDRControllerOptions.LogOptions then
        WriteToLog(SProcRegister + aName);
      udrPlugin.registerProcedure(status,PAnsiChar(aName),FirebirdOOAPI.IUdrProcedureFactoryImpl(factory).asIUdrProcedureFactory);
      TFBUDRProcedureFactory(factory).Controller := self;
    end
    else
    if factory is TFBUDRTriggerFactory then
    begin
      if loLogTriggers in FBUDRControllerOptions.LogOptions then
        WriteToLog(STriggerRegister + aName);
      udrPlugin.registerTrigger(status,PAnsiChar(aName),FirebirdOOAPI.IUdrTriggerFactoryImpl(factory).asIUdrTriggerFactory);
      TFBUDRTriggerFactory(factory).Controller := self;
    end
    else
      FBUDRError(ibxeInvalidFactoryObject,[factory.ClassName,aName]);
end;

procedure TFBUDRController.FreeFactoryList;
//var i: integer;
begin
  {if FUDRFactoryList <> nil then
    for i := 0 to FUDRFactoryList.Count - 1 do
      if FUDRFactoryList.Objects[i] <> nil then
        FUDRFactoryList.Objects[i].Free;} {disposed of by Firebird Engine}
  FreeAndNil(FUDRFactoryList);
end;

const
  LogOptionsTable: array [TFBUDRControllerLogOption] of AnsiString = (
                   'loLogFunctions',
                   'loLogProcedures',
                   'loLogTriggers',
                   'loLogFetches',
                   'loModifyQueries',
                   'loReadOnlyQueries',
                   'loDetails'
                   );

procedure TFBUDRController.LoadConfig;

  function GetLogOptions(LogOptionsStr: AnsiString; var aLogOptions: TFBUDRControllerLogOptions): boolean;
  var s: AnsiString;
      p1, p2, len: integer;
      i: TFBUDRControllerLogOption;
      found: boolean;
  begin
    Result := LogOptionsStr <> '';
    if Result then
    begin
      aLogOptions := [];
      p2 := 1;
      {skip past opening square bracket}
      while (p2 <= length(LogOptionsStr)) and (LogOptionsStr[p2] <> '[') do
        Inc(p2);

      {parse into words separated by commas}
      Inc(p2);
      p1 := p2;
      while p2 <= length(LogOptionsStr) do
      begin
        if LogOptionsStr[p2] in [',',']']  then
        begin
          s := Trim(system.copy(LogOptionsStr,p1,p2-p1));

          {Now convert string to LogOption}
          found := false;
          for i := low(TFBUDRControllerLogOption) to high(TFBUDRControllerLogOption) do
            if CompareText(s,LogOptionsTable[i]) = 0 then
            begin
              aLogOptions := aLogOptions + [i];
              found := true;
              break;
            end;
          if not found then
            WriteToLog(Format(SBadLogOptionsStr,[LogOptionsStr,p2]));
          if LogOptionsStr[p2] = ']' then
            break;
          p1 := p2 + 1;
        end;
        Inc(p2);
      end;
      if p2 > length(LogOptionsStr) then
        WriteToLog(Format(SBadLogOptionsStr,[LogOptionsStr,p2]));
      Result := true;
    end;
  end;

var aLogOptions: TFBUDRControllerLogOptions;
    aConfigFileName: Ansistring;
begin
  aConfigFileName := ProcessTemplateMacros(FBUDRControllerOptions.ConfigFileNameTemplate);
  if (FConfigFile = nil) and (aConfigFileName <> '') and FileExists(aConfigFileName) then
  begin
    FConfigFile := TIniFile.Create(aConfigFileName);
    {$if declared(TStringArray)}
    FConfigFile.BoolFalseStrings := FalseStrings;
    FConfigFile.BoolTrueStrings := TrueStrings;
    {$ifend}
    WriteToLog(Format(SReadingConfigFile,[aConfigFileName]));
    with FBUDRControllerOptions do
    if AllowConfigFileOverrides then
    begin
      LogFileNameTemplate := FConfigFile.ReadString('Controller','LogFileNameTemplate',LogFileNameTemplate);
      WriteToLog('LogFileNameTemplate = ' + LogFileNameTemplate);
      ForceWriteLogEntries := FConfigFile.ReadBool('Controller','ForceWriteLogEntries',ForceWriteLogEntries);
      WriteToLog('ForceWriteLogEntries = ' + BooleanToStr(ForceWriteLogEntries ,'true','false'));
      ThreadSafeLogging := FConfigFile.ReadBool('Controller','ThreadSafeLogging',ThreadSafeLogging);
      WriteToLog('ThreadSafeLogging = ' + BooleanToStr(ThreadSafeLogging,'true','false'));
      if GetLogOptions( FConfigFile.ReadString('Controller','LogOptions',''),aLogOptions) then
        LogOptions := aLogOptions;
      WriteToLog('LogOptions = ' + LogOptionsToStr(LogOptions));
    end;
  end;
end;

function TFBUDRController.NeedLogStream: boolean;
var FilePathName: AnsiString;
begin
  Result := false;
  if FLogStream = nil then
  begin
    FilePathName := ProcessTemplateMacros(FBUDRControllerOptions.LogFileNameTemplate);
    if FilePathName = '' then
      Exit;
    if FJnlOpenAppend then
    begin
      FLogStream := TFileStream.Create(FilePathName,fmOpenWrite or fmShareDenyNone);
      FLogStream.Seek(0, soFromEnd);
    end
    else
    begin
      FLogStream := TFileStream.Create(FilePathName,fmCreate or fmShareDenyNone);
      FJnlOpenAppend := true;
    end;
  end;
  Result := true;
end;

function TFBUDRController.LogOptionsToStr(aLogOptions: TFBUDRControllerLogOptions): AnsiString;
var i: TFBUDRControllerLogOption;
    separator: AnsiString;
begin
  Result := '[';
  separator := '';
  for i := low(TFBUDRControllerLogOption) to high(TFBUDRControllerLogOption) do
    if i in aLogOptions then
    begin
      Result := Result + separator + LogOptionsTable[i];
      separator := ',';
    end;
  Result := Result + ']';
end;

constructor TFBUDRController.Create(status: FirebirdOOAPI.IStatus;
  udrPlugin: FirebirdOOAPI.IUdrPlugin; aTheirUnloadFlag: booleanPtr;
  var aMyUnloadFlag: booleanPtr);
begin
  try
    inherited Create;
    FFBController := self;
    FTheirUnloadFlag := aTheirUnloadFlag;
    FMyUnloadFlag := false;
    aMyUnloadFlag := @FMyUnloadFlag;
    FMaster := udrPlugin.getMaster;
    FCriticalSection := TCriticalSection.Create;
    RegisterUDRFactories(status,udrPlugin);
    LoadConfig;
  except on E: Exception do
    FBSetStatusFromException(E,status);
  end;
end;

destructor TFBUDRController.Destroy;
begin
  if FConfigFile <> nil then
    FConfigFile.Free;
  if FLogStream <> nil then
    FLogStream.Free;
  FreeFactoryList;
  if FCriticalSection <> nil then
    FCriticalSection.Free;
  if (FTheirUnloadFlag <> nil) and not FMyUnloadFlag then
    FTheirUnloadFlag^ := true; {notify unload of module}
  inherited Destroy;
end;

procedure TFBUDRController.FBSetStatusFromException(E: Exception; aStatus: FirebirdOOAPI.IStatus);
var StatusVector: TStatusVector;
    ErrorVector: NativeIntPtr;
begin
  if E is EFBUDRException then
    aStatus.setErrors((E as EFBUDRException).Status.getErrors())
  else
  if E is EIBInterBaseError then
  begin
    ErrorVector := ((E as EIBInterBaseError).Status as TFB30Status).GetStatus.getErrors();
    astatus.setErrors(ErrorVector);
  end
  else
  begin
    FMessageBuffer := E.Message;
    StatusVector[0] := isc_arg_gds;
    StatusVector[1] := NativeInt(isc_random);
    StatusVector[2] := isc_arg_string;
    StatusVector[3] := NativeInt(PAnsiChar(FMessageBuffer));
    StatusVector[4] := isc_arg_end;
    astatus.setErrors(@StatusVector);
  end;
  try
    WriteToLog(SExceptionRaised + LineEnding + E.Message);
  except
    //ignore
  end;
end;

procedure TFBUDRController.WriteToLog(Msg: AnsiString);
var LogEntry: AnsiString;
begin
  if not NeedLogStream then
    Exit; {no log file available}
  LogEntry := Format(sLogFormat,[FBFormatDateTime(GetDateTimeFmt,Now),Msg]) + LineEnding;
  if FBUDRControllerOptions.ThreadSafeLogging then
  begin
    FCriticalSection.Enter;
    try
      FLogStream.Write(LogEntry[1],Length(LogEntry));
      if FBUDRControllerOptions.ForceWriteLogEntries then
        FreeAndNil(FLogStream);
    finally
      FCriticalSection.Leave;
    end;
  end
  else
    FLogStream.Write(LogEntry[1],Length(LogEntry));
end;

function TFBUDRController.CharSetIDToText(att: IAttachment; id: integer): AnsiString;
begin
  if att = nil then
    Result := IntToStr(id)
  else
    Result := att.GetCharsetName(id);
end;

procedure TFBUDRController.WriteToLog(aTitle: AnsiString; Params: IFBUDRInputParams
  );
var i: integer;
    Msg: AnsiString;
begin
  Msg := aTitle + LineEnding;
  for i := 0 to Params.getCount - 1 do
  with Params[i] do
  begin
    Msg := Msg +
           'Parameter ' + IntToStr(i) + ':' + NewLineTAB +
           'Field Name = ' + getName + NewLineTab +
           'Alias Name = ' + getAliasName + NewLineTab +
           'SQLType = ' + GetSQLTypeName + NewLineTAB +
           'sub type = ' + IntToStr(getSubType) + NewLineTAB +
           'Scale = ' + IntToStr(getScale) + NewLineTAB +
           'Charset = '  + CharSetIDToText((Params as TFBUDRInputParams).GetAttachment,getCharSetID) +  NewLineTAB +
           BooleanToStr(getIsNullable,'Nullable','Not Nullable') + NewLineTAB +
           'Size = ' + IntToStr(GetSize) + NewLineTAB +
           'Value = ' + BooleanToStr(IsNull,'NULL',GetStrValue(Params[i] as TColumnMetaData)) + LineEnding;
  end;
  WriteToLog(Msg);
end;

function TFBUDRController.GetStrValue(item: TColumnMetaData): Ansistring;

  function HexString(s: AnsiString): AnsiString;
  var i: integer;
  begin
    Result := '';
    for i := 1 to length(s) do
      Result := Result + Format('%x ',[byte(s[i])]);
  end;

begin
  with Item do
  case SQLType of
    SQL_ARRAY:
      Result := '(array)';
    SQL_BLOB:
      if getSubtype = 1 then
      begin
        if GetCharSetID = 1 then
          Result := HexString(AsString)
        else
          Result := AsString;
      end
      else
        Result := '(Blob)';
    SQL_TEXT,SQL_VARYING:
      if GetCharSetID = 1 then
        Result := HexString(AsString)
      else
        Result := TrimRight(AsString);
    else
      Result := AsString;
  end;
end;

procedure TFBUDRController.WriteToLog(aTitle: AnsiString; OutputData: IFBUDROutputData);
var i: integer;
    Msg: AnsiString;
begin
  Msg := aTitle + LineEnding;
  for i := 0 to OutputData.getCount - 1 do
  with OutputData[i] do
  begin
    Msg := Msg + 'Column ' + IntToStr(i) + NewLineTAB +
                 'Field Name = ' + getName + NewLineTab +
                 'SQLType = ' + GetSQLTypeName + NewLineTAB +
                 'sub type = ' + IntToStr(getSubType) + NewLineTAB +
                 'Scale = ' + IntToStr(getScale) + NewLineTAB +
                 'Charset = '  + CharSetIDToText((OutputData as TFBUDROutputParams).GetAttachment,getCharSetID) +  NewLineTAB +
                 BooleanToStr(getIsNullable,'Nullable','Not Nullable') + NewLineTAB +
                 'Size = ' + IntToStr(GetSize) + NewLineTAB +
                 'Value = ' + BooleanToStr(IsNull,'NULL', GetStrValue(OutputData[i] as TColumnMetaData)) + LineEnding;
  end;
  WriteToLog(Msg);
end;

procedure TFBUDRController.StartJournaling(context: IFBUDRExternalContext);
var JnlOptions: TJournalOptions;
begin
  JnlOptions := [];
  if loModifyQueries in FBUDRControllerOptions.LogOptions then
    JnlOptions := JnlOptions + [joModifyQueries];
  if loReadOnlyQueries in FBUDRControllerOptions.LogOptions then
    JnlOptions := JnlOptions + [joReadOnlyQueries];
  if JnlOptions <> [] then
  begin
    JnlOptions := JnlOptions + [joNoServerTable,joReadWriteTransactions];
    if NeedLogStream then
      context.GetAttachment.StartJournaling(FLogStream,JnlOptions);
  end;
end;

function TFBUDRController.HasConfigFile: boolean;
begin
  Result := FConfigFile <> nil;
end;

function TFBUDRController.ReadConfigString(Section, Ident,
  DefaultValue: AnsiString): AnsiString;
begin
  if HasConfigFile then
    Result := FConfigFile.ReadString(Section, Ident, DefaultValue)
  else
    raise Exception.Create(SNoConfigFile);
end;

function TFBUDRController.ReadConfigInteger(Section, Ident: AnsiString;
  DefaultValue: integer): integer;
begin
  if HasConfigFile then
    Result := FConfigFile.ReadInteger(Section, Ident, DefaultValue)
  else
    raise Exception.Create(SNoConfigFile);
end;

function TFBUDRController.ReadConfigBool(Section, Ident: AnsiString;
  DefaultValue: boolean): boolean;
begin
  if HasConfigFile then
    Result := FConfigFile.ReadBool(Section, Ident, DefaultValue)
  else
    raise Exception.Create(SNoConfigFile);
end;

end.

