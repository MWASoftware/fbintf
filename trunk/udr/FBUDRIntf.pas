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
unit FBUDRIntf;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, Firebird, IB, IBExternals;

type
    {The IFBUDRExternalContext interface provides the Firebird IExternalContext
     interface as a native pascal interface}

  IFBUDRExternalContext = interface
    ['{00b2616d-12e0-436a-8c2c-58670a2be805}']
    function GetFirebirdAPI: IFirebirdAPI;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    function GetUserName: AnsiString;
    function GetDatabaseName: AnsiString;
    function GetClientCharSet: AnsiString;
    function obtainInfoCode: Integer;
    function getInfo(code: Integer): Pointer;
    function setInfo(code: Integer; value: Pointer): Pointer;
    function getStatus: Firebird.IStatus;
    procedure CheckStatus;
    function HasConfigFile: boolean;
    function ReadConfigString(Section, Ident, DefaultValue: AnsiString): AnsiString;
    function ReadConfigInteger(Section, Ident: AnsiString; DefaultValue: integer): integer;
    function ReadConfigBool(Section, Ident: AnsiString; DefaultValue: boolean): boolean;
    procedure WriteToLog(Msg: AnsiString);
  end;

  TFBUDRTriggerType = (ttAfter, ttBefore, ttDatabase);
  IFBUDRMessageMetadata = interface;

  {The IFBUDRRoutineMetadata interface provides the Firebird IRoutineMetadata
   interface as a native Pascal interface}

  IFBUDRRoutineMetadata = interface
    ['{28a03226-e8df-40e8-b67f-d3dc27886e9f}']
    function getPackage: AnsiString;
    function getName: AnsiString;
    function getEntryPoint: AnsiString; {response is parsed into the following three components}
    function getModuleName: AnsiString;
    function getRoutineName: AnsiString;
    function getInfo: AnsiString;
    function getBody: AnsiString;
    function HasInputMetadata: boolean;
    function HasOutputMetadata: boolean;
    function HasTriggerMetadata: boolean;
    function getFBInputMetadata: IFBUDRMessageMetadata;
    function getFBOutputMetadata: IFBUDRMessageMetadata;
    function getFBTriggerMetadata: IFBUDRMessageMetadata;
    function getTriggerTable: AnsiString;
    function getTriggerType: TFBUDRTriggerType;
  end;

  {IFBUDRProcMetadata is a subset of IFBUDRRoutineMetadata and is used to provide
   additional information for procedure and function calls}

  IFBUDRProcMetadata = interface
    ['{d20fc3ae-635e-4841-ad79-b4cd88be75d8}']
    function getPackage: AnsiString;
    function getName: AnsiString;
    function getEntryPoint: AnsiString;
    function getModuleName: AnsiString;
    function getRoutineName: AnsiString;
    function getInfo: AnsiString;
    function getBody: AnsiString;
  end;

  {IFBUDRTriggerMetaData is a subset of IFBUDRRoutineMetadata and is used to provide
   additional information for triggers}

  IFBUDRTriggerMetaData = interface
    ['{9458bad8-809a-469a-b13f-3a3ab95f8d94}']
    function getName: AnsiString;
    function getModuleName: AnsiString;
    function getRoutineName: AnsiString;
    function getInfo: AnsiString;
    function getBody: AnsiString;
    function getTriggerTable: AnsiString;
    function getTriggerType: TFBUDRTriggerType;
  end;


  {IFBUDRInputParams is a subset of the IResults interface and is used to provide
   the input parameters to an external function, procedure or trigger as a
   Pascal interface using Pascal native types.}

  IFBUDRInputParams = interface
    ['{e49d096e-3a9c-4f75-bb39-db32b1897312}']
    function getCount: integer;
    function getSQLData(index: integer): ISQLData;
    procedure GetData(index: integer; var IsNull:boolean; var len: short; var data: PByte);
    function GetTransaction: ITransaction;
    function GetAttachment: IAttachment;
    function ByName(Idx: AnsiString): ISQLData;
    property Data[index: integer]: ISQLData read getSQLData; default;
    property Count: integer read getCount;
  end;

  {IFBUDROutputData is a subset of the ISQLParams interface and is used to return the
   each output row for an external procedure}

  IFBUDROutputData = interface
    ['{8a7d7890-e9a4-430b-8cbc-3874b5f66b31}']
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function GetModified: Boolean;
    function GetHasCaseSensitiveParams: Boolean;
    function GetTransaction: ITransaction;
    function GetAttachment: IAttachment;
    function ByName(Idx: AnsiString): ISQLParam ;
    procedure Clear;
    property Modified: Boolean read GetModified;
    property Params[index: integer]: ISQLParam read getSQLParam; default;
    property Count: integer read getCount;
  end;

  {IFBUDRMetadataBuilder provides the FIrebird IMetadataBuilder interface
   as a native Pascal Interface}

  IFBUDRMetadataBuilder = interface
    ['{a6876fed-fd70-40f0-b965-6c43b8c5c00d}']
    procedure setType(index: Cardinal; type_: Cardinal);
    procedure setSubType(index: Cardinal; subType: Integer);
    procedure setLength(index: Cardinal; length: Cardinal);
    procedure setCharSet(index: Cardinal; charSet: Cardinal);
    procedure setScale(index: Cardinal; scale: Integer);
    procedure truncate(count: Cardinal);
    procedure moveNameToIndex(name: AnsiString; index: Cardinal);
    procedure remove(index: Cardinal);
    function addField:Cardinal;
    procedure setField(index: Cardinal; field: AnsiString);
    procedure setRelation(index: Cardinal; relation: AnsiString);
    procedure setOwner(index: Cardinal; owner: AnsiString);
    procedure setAlias(index: Cardinal; alias: AnsiString);
  end;

  IFBUDRMessageMetadata = interface
    ['{da84190f-91a3-40ae-9fab-bbfd98a49dcb}']
    function getCount: Cardinal;
    function getField(index: Cardinal): AnsiString;
    function getRelation(index: Cardinal): AnsiString;
    function getOwner(index: Cardinal): AnsiString;
    function getAlias(index: Cardinal): AnsiString;
    function getType(index: Cardinal): Cardinal;
    function isNullable(index: Cardinal): Boolean;
    function getSubType(index: Cardinal): Integer;
    function getLength(index: Cardinal): Cardinal;
    function getScale(index: Cardinal): Integer;
    function getCharSet(index: Cardinal): Cardinal;
    function getOffset(index: Cardinal): Cardinal;
    function getNullOffset(index: Cardinal): Cardinal;
    function getBuilder: IFBUDRMetadataBuilder;
    function getMessageLength: Cardinal;
    function getAlignment: Cardinal;
    function getAlignedLength: Cardinal;
  end;



implementation

end.

