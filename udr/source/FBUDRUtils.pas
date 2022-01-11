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
unit FBUDRUtils;

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
  Classes, SysUtils, Firebird, FBActivityMonitor, IB, FBUDRController, FBUDRIntf;

const
  {$IFDEF WINDOWS}
  NewLineTAB = #$0D#$0A'  ';
  {$ELSE}
  NewLineTAB = #$0A'  ';
  {$ENDIF}
  {$if not declared(LineEnding)}
  LineEnding = #$0D#$0A;
  {$ifend}

type

   { TFBUDRObject }

   TFBUDRObject = class(TFBInterfacedObject)
   private
     FFirebirdAPI: IFirebirdAPI;
     FController: TFBUDRController;
     procedure SetFirebirdAPI(AValue: IFirebirdAPI);
   protected
     FStatus: Firebird.IStatus;
   public
     constructor Create(aController: TFBUDRController);
     destructor Destroy; override;
     procedure Clear; {IStatus}
     procedure CheckStatus;
     function getStatus: Firebird.IStatus;
     property FirebirdAPI: IFirebirdAPI read FFirebirdAPI write SetFirebirdAPI;
     property Controller: TFBUDRController read FController;
   end;

  {An External Context is provided when a factory object or an instance of an
   external function, procedure or trigger is called}

  { TFBUDRExternalContext }

  TFBUDRExternalContext = class(TFBUDRObject, IFBUDRExternalContext)
  private
    FContext: Firebird.IExternalContext;
    FAttachment: IAttachment;
    FTransaction: ITransaction;
  public
    constructor Create(aController: TFBUDRController; context: Firebird.IExternalContext);
    function AsText: AnsiString;
  public
    {IFBUDRExternalContext}
    function GetFirebirdAPI: IFirebirdAPI;
    function GetAttachment: IAttachment;
    function GetTransaction: ITransaction;
    function GetUserName: AnsiString;
    function GetDatabaseName: AnsiString;
    function GetClientCharSet: AnsiString;
    function obtainInfoCode: Integer;
    function getInfo(code: Integer): Pointer;
    function setInfo(code: Integer; value: Pointer): Pointer;
    function HasConfigFile: boolean;
    function ReadConfigString(Section, Ident, DefaultValue: AnsiString): AnsiString;
    function ReadConfigInteger(Section, Ident: AnsiString; DefaultValue: integer): integer;
    function ReadConfigBool(Section, Ident: AnsiString; DefaultValue: boolean): boolean;
    procedure WriteToLog(Msg: AnsiString);
  end;

  { The Routine metadata is provided when a factory object or an instance of an
   external function, procedure or trigger is called and provides the input
   and output metadata}

   { TFBUDRRoutineMetadata }

   TFBUDRRoutineMetadata = class(TFBUDRObject,IFBUDRRoutineMetadata,IFBUDRProcMetadata,IFBUDRTriggerMetaData)
   private
     FRoutineMetadata: firebird.IRoutineMetadata;
     FInputMetadata: firebird.IMessageMetadata;
     FOutputMetadata: firebird.IMessageMetadata;
     FTriggerMetadata: firebird.IMessageMetadata;
     FContext: IFBUDRExternalContext;
     FFBInputMetadata: IFBUDRMessageMetadata;
     FFBOutputMetadata: IFBUDRMessageMetadata;
     FFBTriggerMetadata: IFBUDRMessageMetadata;
     FModuleName: AnsiString;
     FRoutineName: AnsiString;
     FInfo: AnsiString;
   public
     constructor Create(context: IFBUDRExternalContext; routineMetadata: firebird.IRoutineMetadata);
     destructor Destroy; override;
     function AsText: AnsiString;
     function getInputMetadata: firebird.IMessageMetadata;
     function getOutputMetadata: firebird.IMessageMetadata;
     function getTriggerMetadata: firebird.IMessageMetadata;
     class procedure ParseEntryPoint(aEntryPoint: AnsiString; var aModuleName, aRoutineName, aInfo: AnsiString);
   public
     {IFBUDRRoutineMetadata}
     function getPackage: AnsiString;
     function getName: AnsiString;
     function getEntryPoint: AnsiString;
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

   { TFBUDRMetadataBuilder }

   TFBUDRMetadataBuilder = class(TFBUDRObject,IFBUDRMetadataBuilder)
   private
     FMetadataBuilder: Firebird.IMetadataBuilder;
   public
     constructor Create(context: IFBUDRExternalContext;
                        metadataBuilder: Firebird.IMetadataBuilder);
     destructor Destroy; override;
     property Builder: Firebird.IMetadataBuilder read FMetadataBuilder;
   public
     {IFBUDRMetadataBuilder}
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

   { TFBUDRMessageMetadata }

   TFBUDRMessageMetadata = class(TFBUDRObject,IFBUDRMessageMetadata)
   private
     FMetadata: Firebird.IMessageMetadata;
     FContext: IFBUDRExternalContext;
   public
     constructor Create(context: IFBUDRExternalContext;
                        metadata: Firebird.IMessageMetadata);
     destructor Destroy; override;
     function AsText: AnsiString;
  public
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

   { EFBUDRException }

   EFBUDRException = class(Exception)
   private
     FStatus: Firebird.IStatus;
   public
     constructor Create(aStatus: Firebird.IStatus);
     destructor Destroy; override;
     property Status: Firebird.IStatus read FStatus;
   end;

{$IFDEF MSWINDOWS}
function GetTempDir: AnsiString;
{$ENDIF}

function BooleanToStr(boolValue: boolean; ValTrue, ValFalse: AnsiString): AnsiString;

implementation

uses FBClientLib, FBClientAPI, FB30ClientAPI, FB30Attachment, FB30Transaction,
     FBUDRMessage,  FBSQLData {$IFDEF MSWINDOWS}, Windows{$ENDIF};

{$IFDEF MSWINDOWS}
function GetTempDir: AnsiString;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
end;
{$ENDIF}

function BooleanToStr(boolValue: boolean; ValTrue, ValFalse: AnsiString): AnsiString;
begin
  if boolValue then
    Result := ValTrue
  else
    Result := ValFalse;
end;

{ TFBUDRMessageMetadata }

constructor TFBUDRMessageMetadata.Create(context: IFBUDRExternalContext;
  metadata: Firebird.IMessageMetadata);
begin
  inherited Create((context as TFBUDRExternalContext).Controller);
  FirebirdAPI := context.GetFirebirdAPI;
  FContext := context;
  FMetadata := metadata;
  FMetadata.addRef;
end;

destructor TFBUDRMessageMetadata.Destroy;
begin
  if FMetadata <> nil then
    FMetadata.release;
  inherited Destroy;
end;

function TFBUDRMessageMetadata.AsText: AnsiString;

  function CharsetIDToText(id: integer): AnsiString;
  begin
    if FContext.GetAttachment <> nil then
      Result := FContext.GetAttachment.GetCharsetName(id)
    else
      Result := IntToStr(id);
  end;

var i: integer;
begin
  Result := Format('Field Count = %d' + NewLineTAB,[getCount]) +
            Format('Alignment = %d' + NewLineTAB,[getAlignment]) +
            Format('Aligned Length = %d' + NewLineTAB,[getAlignedLength]);
  for i := 0 to getCount - 1 do
  begin
    Result := Result +
            Format('Field No. %d' + NewLineTAB,[i]) +
            Format('Field Name = %s' + NewLineTAB,[getField(i)]) +
            Format('Relation Name = %s' + NewLineTAB,[getRelation(i)]) +
            Format('Alias Name = %s' + NewLineTAB,[getAlias(i)]) +
            Format('SQLType = %s' + NewLineTAB,[TSQLDataItem.GetSQLTypeName(getType(i))]) +
            Format('IsNullable = %s' + NewLineTAB,[BooleanToStr(isNullable(i),'yes','no')]) +
            Format('SubType = %d' + NewLineTAB,[getSubType(i)]) +
            Format('Length = %d' + NewLineTAB,[getLength(i)]) +
            Format('Scale = %d' + NewLineTAB,[getScale(i)]) +
            Format('Offset = %d' + NewLineTAB,[getOffset(i)]) +
            Format('Null Offset = %d' + NewLineTAB,[getNullOffset(i)]) +
            Format('Message Length = %d' + NewLineTAB,[getLength(i)]);
  end;
end;

function TFBUDRMessageMetadata.getCount: Cardinal;
begin
  Result := FMetadata.getCount(FStatus);
  CheckStatus;
end;

function TFBUDRMessageMetadata.getField(index: Cardinal): AnsiString;
begin
  Result := strpas(FMetadata.getField(FStatus,index));
  CheckStatus;
end;

function TFBUDRMessageMetadata.getRelation(index: Cardinal): AnsiString;
begin
  Result := strpas(FMetadata.getRelation(FStatus,index));
  CheckStatus;
end;

function TFBUDRMessageMetadata.getOwner(index: Cardinal): AnsiString;
begin
  Result := strpas(FMetadata.getOwner(FStatus,index));
  CheckStatus;
end;

function TFBUDRMessageMetadata.getAlias(index: Cardinal): AnsiString;
begin
  Result := strpas(FMetadata.getAlias(FStatus,index));
  CheckStatus;
end;

function TFBUDRMessageMetadata.getType(index: Cardinal): Cardinal;
begin
  Result := FMetadata.getType(FStatus,index);
  CheckStatus;
end;

function TFBUDRMessageMetadata.isNullable(index: Cardinal): Boolean;
begin
  Result := FMetadata.isNullable(FStatus,index);
  CheckStatus;
end;

function TFBUDRMessageMetadata.getSubType(index: Cardinal): Integer;
begin
  Result := FMetadata.getSubType(FStatus,index);
  CheckStatus;
end;

function TFBUDRMessageMetadata.getLength(index: Cardinal): Cardinal;
begin
  Result := FMetadata.getLength(FStatus,index);
  CheckStatus;
end;

function TFBUDRMessageMetadata.getScale(index: Cardinal): Integer;
begin
  Result := FMetadata.getScale(FStatus,index);
  CheckStatus;
end;

function TFBUDRMessageMetadata.getCharSet(index: Cardinal): Cardinal;
begin
  Result := FMetadata.getCharSet(FStatus,index);
  CheckStatus;
end;

function TFBUDRMessageMetadata.getOffset(index: Cardinal): Cardinal;
begin
  Result := FMetadata.getOffset(FStatus,index);
  CheckStatus;
end;

function TFBUDRMessageMetadata.getNullOffset(index: Cardinal): Cardinal;
begin
  Result := FMetadata.getNullOffset(FStatus,index);
  CheckStatus;
end;

function TFBUDRMessageMetadata.getBuilder: IFBUDRMetadataBuilder;
var builder: Firebird.IMetadataBuilder;
begin
  builder := FMetadata.getBuilder(FStatus);
  try
    CheckStatus;
    Result := TFBUDRMetadataBuilder.Create(FContext,builder);
  finally
    builder.release;
  end;
end;

function TFBUDRMessageMetadata.getMessageLength: Cardinal;
begin
  Result := FMetadata.getMessageLength(FStatus);
  CheckStatus;
end;

function TFBUDRMessageMetadata.getAlignment: Cardinal;
begin
  Result := FMetadata.getAlignment(FStatus);
  CheckStatus;
end;

function TFBUDRMessageMetadata.getAlignedLength: Cardinal;
begin
  Result := FMetadata.getAlignedLength(FStatus);
  CheckStatus;
end;

{ TFBUDRMetadataBuilder }

constructor TFBUDRMetadataBuilder.Create(context: IFBUDRExternalContext;
  metadataBuilder: Firebird.IMetadataBuilder);
begin
  inherited Create((context as TFBUDRExternalContext).Controller);
  FirebirdAPI := context.GetFirebirdAPI;
  FMetadataBuilder := metadataBuilder;
  FMetadataBuilder.addRef;
end;

destructor TFBUDRMetadataBuilder.Destroy;
begin
  if FMetadataBuilder <> nil then
    FMetadataBuilder.release;
  inherited Destroy;
end;

procedure TFBUDRMetadataBuilder.setType(index: Cardinal; type_: Cardinal);
begin
  FMetadataBuilder.setType(FStatus,index,type_);
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.setSubType(index: Cardinal; subType: Integer);
begin
  FMetadataBuilder.setSubType(FStatus,index,subType);
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.setLength(index: Cardinal; length: Cardinal);
begin
  FMetadataBuilder.setLength(FStatus,index,Length);
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.setCharSet(index: Cardinal; charSet: Cardinal);
begin
  FMetadataBuilder.setCharSet(FStatus,index,charSet);
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.setScale(index: Cardinal; scale: Integer);
begin
  FMetadataBuilder.SetScale(FStatus,index,scale);
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.truncate(count: Cardinal);
begin
  FMetadataBuilder.truncate(FStatus,count);
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.moveNameToIndex(name: AnsiString; index: Cardinal);
begin
  FMetadataBuilder.moveNameToIndex(FStatus,PAnsiChar(name),index);
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.remove(index: Cardinal);
begin
  FMetadataBuilder.remove(FStatus,index);
  CheckStatus;
end;

function TFBUDRMetadataBuilder.addField: Cardinal;
begin
  Result := FMetadataBuilder.addField(FStatus);
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.setField(index: Cardinal; field: AnsiString);
begin
  FMetadataBuilder.setField(FStatus,index,PAnsiChar(field));
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.setRelation(index: Cardinal; relation: AnsiString);
begin
  FMetadataBuilder.setRelation(FStatus,index,PAnsiChar(relation));
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.setOwner(index: Cardinal; owner: AnsiString);
begin
  FMetadataBuilder.setOwner(FStatus,index,PAnsiChar(owner));
  CheckStatus;
end;

procedure TFBUDRMetadataBuilder.setAlias(index: Cardinal; alias: AnsiString);
begin
  FMetadataBuilder.setAlias(FStatus,index,PAnsiChar(alias));
end;

{ TFBUDRObject }

procedure TFBUDRObject.SetFirebirdAPI(AValue: IFirebirdAPI);
var MasterProvider: IFBIMasterProvider;
begin
  if FFirebirdAPI = AValue then Exit;
  FFirebirdAPI := AValue;
  if (FStatus = nil) and
      FirebirdAPI.HasMasterIntf and (FirebirdAPI.QueryInterface(IFBIMasterProvider,MasterProvider) = S_OK) then
    FStatus := MasterProvider.GetIMasterIntf.getStatus;
end;

procedure TFBUDRObject.CheckStatus;
begin
  with FStatus do
  if (getState and STATE_ERRORS) <> 0 then
    raise EFBUDRException.Create(FStatus);
end;

function TFBUDRObject.getStatus: Firebird.IStatus;
begin
  Result := FStatus;
end;

constructor TFBUDRObject.Create(aController: TFBUDRController);
begin
  inherited Create;
  FController := aController;
end;

destructor TFBUDRObject.Destroy;
begin
  if FStatus <> nil then
    FStatus.dispose;
  inherited Destroy;
end;

procedure TFBUDRObject.Clear;
begin
  if FStatus <> nil then
    FStatus.Init;
end;

{ TFBUDRExternalContext }

constructor TFBUDRExternalContext.Create(aController: TFBUDRController;
  context: Firebird.IExternalContext);
begin
  inherited Create(aController);
  FContext := context;
  FirebirdAPI := TFB30ClientAPI.Create(context.getMaster);
end;

function TFBUDRExternalContext.AsText: AnsiString;
begin
  Result := 'External Context: ' + NewLineTAB +
            Format('Attachment ID = %d' + NewLineTAB,[GetAttachment.GetAttachmentID]) +
            Format('Transaction ID = %d' + NewLineTAB,[GetTransaction.GetTransactionID]) +
            Format('User Name = %s'  + NewLineTAB,[GetUserName]) +
            Format('Database Name = %s' + NewLineTAB,[GetDatabaseName]) +
            Format('Client Character Set = %s' + NewLineTAB,[GetClientCharSet]);
end;

function TFBUDRExternalContext.GetFirebirdAPI: IFirebirdAPI;
begin
  Result := FirebirdAPI;
end;

function TFBUDRExternalContext.GetAttachment: IAttachment;
var att: Firebird.IAttachment;
begin
  if FAttachment = nil then
  begin
    att := FContext.getAttachment(FStatus);
    CheckStatus;
    FAttachment := TFB30Attachment.Create(FirebirdAPI as TFB30ClientAPI,
                                                  att,
                                                  GetDatabaseName);
  end;
  Result := FAttachment;
end;

function TFBUDRExternalContext.GetTransaction: ITransaction;
var tr: Firebird.ITransaction;
begin
  Result := nil;
  if FTransaction = nil then
  begin
    tr := FContext.getTransaction(FStatus);
    CheckStatus;
    FTransaction := TFB30Transaction.Create(FirebirdAPI as TFB30ClientAPI,GetAttachment,tr);
  end;
  Result := FTransaction;
end;

function TFBUDRExternalContext.GetUserName: AnsiString;
begin
  Result := strpas(FContext.getUserName);
end;

function TFBUDRExternalContext.GetDatabaseName: AnsiString;
begin
  Result := strpas(FContext.getDatabaseName);
end;

function TFBUDRExternalContext.GetClientCharSet: AnsiString;
begin
  Result := strpas(FContext.getClientCharSet);
end;

function TFBUDRExternalContext.obtainInfoCode: Integer;
begin
  Result := FContext.obtainInfoCode;
end;

function TFBUDRExternalContext.getInfo(code: Integer): Pointer;
begin
  Result := FContext.getInfo(code);
end;

function TFBUDRExternalContext.setInfo(code: Integer; value: Pointer): Pointer;
begin
  Result := FContext.setInfo(code,value);
end;

function TFBUDRExternalContext.HasConfigFile: boolean;
begin
  Result := Controller.HasConfigFile;
end;

function TFBUDRExternalContext.ReadConfigString(Section, Ident,
  DefaultValue: AnsiString): AnsiString;
begin
  Result := Controller.ReadConfigString(Section, Ident, DefaultValue);
end;

function TFBUDRExternalContext.ReadConfigInteger(Section, Ident: AnsiString;
  DefaultValue: integer): integer;
begin
  Result := Controller.ReadConfigInteger(Section, Ident, DefaultValue);
end;

function TFBUDRExternalContext.ReadConfigBool(Section, Ident: AnsiString;
  DefaultValue: boolean): boolean;
begin
  Result := Controller.ReadConfigBool(Section, Ident, DefaultValue);
end;

procedure TFBUDRExternalContext.WriteToLog(Msg: AnsiString);
begin
  Controller.WriteToLog(Msg);
end;


{ TFBUDRRoutineMetadata }

class procedure TFBUDRRoutineMetadata.ParseEntryPoint(aEntryPoint: AnsiString;
  var aModuleName, aRoutineName, aInfo: AnsiString);
var p1,p2: integer;
begin
  aModuleName := '';
  aRoutineName := '';
  aInfo := '';
  p1 := 1;
  P2 := 1;
  while (p2 < length(aEntryPoint)) and (aEntryPoint[p2] <> '!') do
    Inc(p2);
  if p2 = length(aEntryPoint) then
  begin
    aModuleName := aEntryPoint;
    Exit;
  end;
  aModuleName := system.copy(aEntryPoint,1,p2-1);
  Inc(p2);
  p1 := p2;
  while (p2 < length(aEntryPoint)) and (aEntryPoint[p2] <> '!') do
    Inc(p2);
  if p2 = length(aEntryPoint) then
  begin
    aRoutineName := system.copy(aEntryPoint,p1,maxint);
    Exit;
  end;
  aRoutineName := system.copy(aEntryPoint,p1,p2-p1);
  aInfo := system.copy(aEntryPoint,p2+1,maxint);
end;

constructor TFBUDRRoutineMetadata.Create(context: IFBUDRExternalContext;
  routineMetadata: firebird.IRoutineMetadata);
var TriggerType: cardinal;
begin
  inherited Create((context as TFBUDRExternalContext).Controller);
  FirebirdAPI := context.GetFirebirdAPI;
  FContext := context;
  FRoutineMetadata := routineMetadata;

  TriggerType := FRoutineMetadata.getTriggerType(FStatus);
  CheckStatus;

  if TriggerType = 0 then
  begin
    FInputMetadata := FRoutineMetadata.getInputMetadata(FStatus);
    CheckStatus;
    if FInputMetadata <> nil then
      FInputMetadata.addRef;

    FOutputMetadata := FRoutineMetadata.getOutputMetadata(FStatus);
    CheckStatus;
    if FOutputMetadata <> nil then
      FOutputMetadata.addRef;
  end
  else
  begin
    FTriggerMetadata := FRoutineMetadata.getTriggerMetadata(FStatus);
    CheckStatus;
    if FTriggerMetadata <> nil then
      FTriggerMetadata.addRef;
  end;
  ParseEntryPoint(getEntryPoint,FModuleName,FRoutineName,FInfo);
end;

destructor TFBUDRRoutineMetadata.Destroy;
begin
  if FInputMetadata <> nil then
    FInputMetadata.release;
  if FOutputMetadata <> nil then
    FOutputMetadata.release;
  if FTriggerMetadata <> nil then
    FTriggerMetadata.release;
  inherited Destroy;
end;

function TFBUDRRoutineMetadata.AsText: AnsiString;

  function MetadataToText(metadata: Firebird.IMessageMetadata): AnsiString;
  var fbMetadata: TFBUDRMessageMetadata;
  begin
    if metadata = nil then
      Result := '(nil)'
    else
    begin
      fbMetadata := TFBUDRMessageMetadata.Create(FContext,metadata);
      try
        Result := fbMetadata.AsText;
      finally
        fbMetadata.Free;
      end;
    end;
  end;

  function TriggerTypeToText(TriggerType: TFBUDRTriggerType): AnsiString;
  begin
    case TriggerType of
    ttAfter:
      Result := 'After';
    ttBefore:
      Result := 'Before';
    ttDatabase:
      Result := 'Database';
    end;
  end;

begin
  Result := Format('Package Name = %s' + NewLineTAB,[getPackage]) +
            Format('Name = %s' + NewLineTAB,[getName]) +
            Format('Entry Point = %s (%s,%s,%s)' + NewLineTAB,[getEntryPoint,getModuleName,getRoutineName,getInfo]) +
            Format('Body = %s' + NewLineTAB,[getBody]) +
            Format('Input Metadata:' + NewLineTAB + '%s',[MetadataToText(FInputMetaData)]) + LineEnding +
            Format('Output Metadata:' + NewLineTAB + '%s',[MetadataToText(FOutputMetaData)]);
  if FRoutineMetadata.getTriggerType(FStatus) > 0 then
  begin
    Result := Result +
    Format('Trigger Metadata:' + NewLineTAB + '%s',[MetadataToText(FTriggerMetaData)]) +
    Format('Trigger Table = %s' + NewLineTAB,[getTriggerTable]) +
    Format('Trigger Type = %s' + NewLineTAB,[TriggerTypeToText(getTriggerType)]);
  end;
  CheckStatus;
end;

function TFBUDRRoutineMetadata.getPackage: AnsiString;
begin
  Result := strpas(FRoutineMetadata.getPackage(FStatus));
  CheckStatus;
end;

function TFBUDRRoutineMetadata.getName: AnsiString;
begin
  Result := strpas(FRoutineMetadata.getName(FStatus));
  CheckStatus;
end;

function TFBUDRRoutineMetadata.getModuleName: AnsiString;
begin
  Result := FModuleName;
end;

function TFBUDRRoutineMetadata.getRoutineName: AnsiString;
begin
  Result := FRoutineName;
end;

function TFBUDRRoutineMetadata.getInfo: AnsiString;
begin
  Result := FInfo;
end;

function TFBUDRRoutineMetadata.getEntryPoint: AnsiString;
begin
  Result := strpas(FRoutineMetadata.getEntryPoint(FStatus));
  CheckStatus;
end;

function TFBUDRRoutineMetadata.getBody: AnsiString;
begin
  Result := strpas(FRoutineMetadata.getBody(FStatus));
  CheckStatus;
end;

function TFBUDRRoutineMetadata.HasInputMetadata: boolean;
begin
  Result := FInputMetadata <> nil;
end;

function TFBUDRRoutineMetadata.HasOutputMetadata: boolean;
begin
  Result := FOutputMetadata <> nil;
end;

function TFBUDRRoutineMetadata.HasTriggerMetadata: boolean;
begin
  Result := FTriggerMetadata <> nil;
end;

function TFBUDRRoutineMetadata.getFBInputMetadata: IFBUDRMessageMetadata;
begin
  Result := nil;
  if (FFBInputMetadata = nil) and (FInputMetadata <> nil) then
      FFBInputMetadata := TFBUDRMessageMetadata.Create(FContext,FInputMetadata);
  Result := FFBInputMetadata;
end;

function TFBUDRRoutineMetadata.getFBOutputMetadata: IFBUDRMessageMetadata;
begin
  Result := nil;
  if (FFBOutputMetadata = nil) and (FOutputMetadata <> nil) then
    FFBOutputMetadata := TFBUDRMessageMetadata.Create(FContext,FOutputMetadata);
  Result := FFBOutputMetadata;
end;

function TFBUDRRoutineMetadata.getFBTriggerMetadata: IFBUDRMessageMetadata;
begin
  Result := nil;
  if (FFBTriggerMetadata = nil) and (FTriggerMetadata <> nil) then
    FFBTriggerMetadata := TFBUDRMessageMetadata.Create(FContext,FTriggerMetadata);
  Result := FFBTriggerMetadata;
end;

function TFBUDRRoutineMetadata.getInputMetadata: firebird.IMessageMetadata;
begin
  Result := FInputMetaData;
  if Result <> nil then
    Result.addRef;
end;

function TFBUDRRoutineMetadata.getOutputMetadata: firebird.IMessageMetadata;
begin
  Result := FOutputMetadata;
  if Result <> nil then
    Result.addRef;
end;

function TFBUDRRoutineMetadata.getTriggerMetadata: firebird.IMessageMetadata;
begin
  Result := FTriggerMetadata;
  if Result <> nil then
    Result.addRef;
end;

function TFBUDRRoutineMetadata.getTriggerTable: AnsiString;
begin
  Result := strpas(FRoutineMetadata.getTriggerTable(FStatus));
  CheckStatus;
end;

function TFBUDRRoutineMetadata.getTriggerType: TFBUDRTriggerType;
var TriggerType: cardinal;
begin
  TriggerType := FRoutineMetadata.getTriggerType(FStatus);
  CheckStatus;
  with Firebird.IExternalTrigger do
  case TriggerType of
  TYPE_BEFORE:
    Result := ttBefore;
  TYPE_AFTER:
    Result := ttAfter;
  TYPE_DATABASE:
    Result := ttDatabase;
  else
    FBUDRError(ibxeUnknownTriggerType,[TriggerType]);
  end;
end;

{ EFBUDRException }

constructor EFBUDRException.Create(aStatus: Firebird.IStatus);
begin
  inherited Create(SFirebirdStatusError);
  FStatus := aStatus.clone;
end;

destructor EFBUDRException.Destroy;
begin
  FStatus.dispose;
  inherited Destroy;
end;

end.

