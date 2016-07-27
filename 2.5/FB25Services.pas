unit FB25Services;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FB25ClientAPI, IBExternals, IBHeader;

type

  { TFBServiceManager }

  TFBServiceManager = class(TInterfacedObject,IServiceManager)
  private
    FServerName: string;
    FSPB: PChar;
    FSPBLength: Short;
    FHandle: TISC_SVC_HANDLE;
    FProtocol: TProtocol;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckServerName;
    procedure GenerateSPB(sl: TStrings; var SPB: String; var SPBLength: Short);
  public
    constructor Create(ServerName: string; Protocol: TProtocol; Params: TStrings);
    destructor Destroy; override;
    property Handle: TISC_SVC_HANDLE read FHandle;

  public
    {IServiceManager}
    function GetStatus: IStatus;
    procedure Attach;
    procedure Detach;
    function IsAttached: boolean;
    procedure Start(Command: char; Params: TServiceCommandParams);
    function Query(Command: char; Params: TServiceQueryParams): TServiceQueryResponse; overload;
    function Query(Commands: array of char):TServiceQueryResponse; overload;
    procedure Release;
  end;

implementation

uses FBErrorMessages;

const
  DefaultBufferSize = 32000;

  SPBPrefix = 'isc_spb_';
  SPBConstantNames: array[1..isc_spb_last_spb_constant] of String = (
    'user_name',
    'sys_user_name',
    'sys_user_name_enc',
    'password',
    'password_enc',
    'command_line',
    'db_name',
    'verbose',
    'options',
    'connect_timeout',
    'dummy_packet_interval',
    'sql_role_name'
  );

  SPBConstantValues: array[1..isc_spb_last_spb_constant] of Integer = (
    isc_spb_user_name_mapped_to_server,
    isc_spb_sys_user_name_mapped_to_server,
    isc_spb_sys_user_name_enc_mapped_to_server,
    isc_spb_password_mapped_to_server,
    isc_spb_password_enc_mapped_to_server,
    isc_spb_command_line_mapped_to_server,
    isc_spb_dbname_mapped_to_server,
    isc_spb_verbose_mapped_to_server,
    isc_spb_options_mapped_to_server,
    isc_spb_connect_timeout_mapped_to_server,
    isc_spb_dummy_packet_interval_mapped_to_server,
    isc_spb_sql_role_name_mapped_to_server
  );

type

  { TServiceBuffer }

  TServiceBuffer = class
  private
    FBuffer: PChar;
    FSize: integer;
    FBufPtr: PChar;
    FDataSize: integer;
    procedure CheckBuffer(BytesNeeded: integer);
  public
    constructor Create(command:char; aSize: integer = 1024);
    destructor Destroy; override;
    procedure AddVar(ServiceVar: TServiceParam);
    function Buffer: PChar;
    property Size: integer read FSize;
    property DataSize: integer read FDataSize;
  end;

  { TServiceResponseBuffer }

  TServiceResponseBuffer = class
  private
    FBuffer: PChar;
    FSize: integer;
  public
    constructor Create(aSize: integer = DefaultBufferSize);
    destructor Destroy; override;
    function Buffer: PChar;
    function ParseBuffer: TServiceQueryResponse;
    property Size: integer read FSize;
  end;

{ TServiceResponseBuffer }

constructor TServiceResponseBuffer.Create(aSize: integer);
begin
  inherited Create;
  FSize := aSize;
  GetMem(FBuffer,aSize);
  FillChar(FBuffer^,aSize,255);
end;

destructor TServiceResponseBuffer.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TServiceResponseBuffer.Buffer: PChar;
begin
  Result := Buffer;
end;

function TServiceResponseBuffer.ParseBuffer: TServiceQueryResponse;
var P: PChar;
    ResCount: integer;

  procedure ParseString(var resp: TServiceResponse);
  var len: ISC_LONG;
  begin
    with Firebird25ClientAPI do
      len := isc_vax_integer(P + 1, 2);
    resp.dr := drString;
    SetString(resp.StringValue,P+3,len);
    Inc(P,len+3);
  end;

  procedure ParseInteger(var resp: TServiceResponse);
  begin
    resp.dr := drInteger;
    with Firebird25ClientAPI do
      resp.IntValue := isc_vax_integer(P+1,4);
    Inc(P,5);
  end;

  procedure ParseByte(var resp: TServiceResponse);
  begin
    resp.dr := drInteger;
    resp.IntValue := integer((P+1)^);
    Inc(P,2);
  end;

begin
  P := FBuffer;
  ResCount := 0;

  SetLength(Result,0);
  while (P < Buffer + Size) and (P^ <> char(isc_info_end)) do
  begin
    Inc(ResCount);
    SetLength(Result,ResCount);
    Result[ResCount-1].Identifier := P^;
    case integer(P^) of
    isc_info_svc_line,
    isc_info_svc_get_env,
    isc_info_svc_get_env_lock,
    isc_info_svc_get_env_msg,
    isc_info_svc_user_dbpath,
    isc_info_svc_server_version,
    isc_info_svc_implementation:
      ParseString(Result[ResCount-1]);

    isc_info_svc_get_license_mask,
    isc_info_svc_capabilities,
    isc_info_svc_version,
    isc_info_svc_running:
      ParseInteger(Result[ResCount-1]);

    isc_info_svc_to_eof:
      begin
        ParseString(Result[ResCount-1]);
        if P^ = char(isc_info_truncated) then
        begin
          Result[ResCount-1].Identifier := char(isc_info_truncated);
          Exit;
        end;
      end;
    isc_info_svc_get_config:
      begin
        Result[ResCount-1].dr := drNone;
        Inc(P);
        while (P < Buffer + Size) and (P^ <> char(isc_info_end)) do
        begin
          Inc(ResCount);
          SetLength(Result,ResCount);
          Result[ResCount-1].Identifier := P^;
          ParseInteger(Result[ResCount-1]);
        end;
        Inc(ResCount);
        SetLength(Result,ResCount);
        Result[ResCount-1].Identifier := char(isc_info_flag_end);
        Result[ResCount-1].dr := drNone;
      end;

    isc_info_svc_svr_db_info:
      begin
        Result[ResCount-1].dr := drNone;
        Inc(P);
        while (P < Buffer + Size) and (P^ <> char(isc_info_end)) do
        begin
          Inc(ResCount);
          SetLength(Result,ResCount);
          Result[ResCount-1].Identifier := P^;
          case integer(P^) of
          isc_spb_num_att,
          isc_spb_num_db:
            ParseInteger(Result[ResCount-1]);

          isc_spb_dbname:
            ParseString(Result[ResCount-1]);

          else
            IBError(ibxeOutputParsingError, [nil]);
          end;
        end;
        Inc(ResCount);
        SetLength(Result,ResCount);
        Result[ResCount-1].Identifier := char(isc_info_flag_end);
        Result[ResCount-1].dr := drNone;
     end;

    isc_info_svc_get_license:
      begin
        Result[ResCount-1].dr := drNone;
        Inc(P);
        while (P < Buffer + Size) and (P^ <> char(isc_info_end)) do
        begin
          Inc(ResCount);
          SetLength(Result,ResCount);
          Result[ResCount-1].Identifier := P^;
          case integer(P^) of
          isc_spb_lic_id,
          isc_spb_lic_key:
            ParseString(Result[ResCount-1]);

          else
            IBError(ibxeOutputParsingError, [nil]);
          end;
        end;
        Inc(ResCount);
        SetLength(Result,ResCount);
        Result[ResCount-1].Identifier := char(isc_info_end);
        Result[ResCount-1].dr := drNone;
      end;

      isc_info_svc_limbo_trans:
        begin
          Result[ResCount-1].dr := drNone;
          Inc(P);
          while (P < Buffer + Size) and (P^ <> char(isc_info_end)) do
          begin
            Inc(ResCount);
            SetLength(Result,ResCount);
            Result[ResCount-1].Identifier := P^;
            case integer(P^) of
            isc_spb_single_tra_id,
            isc_spb_multi_tra_id:
              ParseInteger(Result[ResCount-1]);

            isc_spb_tra_host_site,
            isc_spb_tra_remote_site,
            isc_spb_tra_db_path:
              ParseString(Result[ResCount-1]);

            isc_spb_tra_advise,
            isc_spb_tra_state:
              ParseByte(Result[ResCount-1]);

            else
              IBError(ibxeOutputParsingError, [nil]);
            end;
          end;
          Inc(ResCount);
          SetLength(Result,ResCount);
          Result[ResCount-1].Identifier := char(isc_info_end);
          Result[ResCount-1].dr := drNone;
        end;

      isc_info_svc_get_users:
        begin
          Result[ResCount-1].dr := drNone;
          Inc(P);
          while (P < Buffer + Size) and (P^ <> char(isc_info_end)) do
          begin
            Inc(ResCount);
            SetLength(Result,ResCount);
            Result[ResCount-1].Identifier := P^;
            case integer(P^) of
            isc_spb_sec_userid,
            isc_spb_sec_groupid:
              ParseInteger(Result[ResCount-1]);

            isc_spb_sec_username,
            isc_spb_sec_password,
            isc_spb_sec_firstname,
            isc_spb_sec_middlename,
            isc_spb_sec_lastname:
              ParseString(Result[ResCount-1]);

            else
              IBError(ibxeOutputParsingError, [nil]);
            end;
          end;
          Inc(ResCount);
          SetLength(Result,ResCount);
          Result[ResCount-1].Identifier := char(isc_info_end);
          Result[ResCount-1].dr := drNone;
        end;

    else
      IBError(ibxeOutputParsingError, [nil]);

    end;
    Inc(P);
  end;
  if P >= Buffer + Size then
    IBError(ibxeOutputParsingError, [nil]);
end;

{ TServiceBuffer }

procedure TServiceBuffer.CheckBuffer(BytesNeeded: integer);
begin
  if FDataSize + BytesNeeded > FSize then
  begin
    FSize += 1024;
    ReAllocMem(FBuffer,FSize);
  end;
end;

constructor TServiceBuffer.Create(command: char; aSize: integer);
begin
  inherited Create;
  FSize := aSize;
  GetMem(FBuffer,aSize);
  FillChar(FBuffer^,aSize,255);
  FBufPtr := FBuffer;
  FBufPtr^ := command;
  Inc(FBufPtr);
  FDataSize := 1;
end;

destructor TServiceBuffer.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

procedure TServiceBuffer.AddVar(ServiceVar: TServiceParam);
var Len: UShort;
begin
  with Firebird25ClientAPI do
  case ServiceVar.dt of
    dtInteger:
      begin
        CheckBuffer(5); {Is there enough space for an integer}
        FBufPtr^ := ServiceVar.Identifier;
        Inc(FBufPtr);
        EncodeLsbf(ServiceVar.IntValue,4,FBufPtr);
        Inc(FBufPtr,4);
      end;

    dtString:
      begin
        Len := Length(ServiceVar.StringValue);
        CheckBuffer(Len+3); {Is there enough space for the string}
        FBufPtr^ := ServiceVar.Identifier;
        Inc(FBufPtr);
        EncodeLsbf(Len,2,FBufPtr);
        Inc(FBufPtr,2);
        Move(ServiceVar.StringValue[1],FBufPtr^,Len);
        Inc(FBufPtr,Len);
      end;
  end;
end;

function TServiceBuffer.Buffer: PChar;
begin
  Result := FBuffer;
end;

{ TFBServiceManager }

{
 * GenerateSPB -
 *  Given a string containing a textual representation
 *  of the Service parameters, generate a service
 *  parameter buffer, and return it and its length
 *  in SPB and SPBLength, respectively.
}

procedure TFBServiceManager.CheckActive;
begin
  if FHandle = nil then
    IBError(ibxeServiceActive, [nil]);
end;

procedure TFBServiceManager.CheckInactive;
begin
  if FHandle <> nil then
    IBError(ibxeServiceInActive, [nil]);
end;

procedure TFBServiceManager.CheckServerName;
begin
  if (FServerName = '') and (FProtocol <> Local) then
    IBError(ibxeServerNameMissing, [nil]);
end;

procedure TFBServiceManager.GenerateSPB(sl: TStrings; var SPB: String;
  var SPBLength: Short);
var
  i, j, SPBVal, SPBServerVal: UShort;
  param_name, param_value: String;
begin
  { The SPB is initially empty, with the exception that
   the SPB version must be the first byte of the string.
  }
  SPBLength := 2;
  SPB := Char(isc_spb_version);
  SPB := SPB + Char(isc_spb_current_version);
  { Iterate through the textual service parameters, constructing
   a SPB on-the-fly }
  if sl.Count > 0 then
  for i := 0 to sl.Count - 1 do
  begin
   { Get the parameter's name and value from the list,
     and make sure that the name is all lowercase with
     no leading 'isc_spb_' prefix }
    if (Trim(sl.Names[i]) = '') then continue;
    param_name := LowerCase(sl.Names[i]); {mbcs ok}
    param_value := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i])); {mbcs ok}
    if (Pos(SPBPrefix, param_name) = 1) then {mbcs ok}
      Delete(param_name, 1, Length(SPBPrefix));
    { We want to translate the parameter name to some integer
      value. We do this by scanning through a list of known
      service parameter names (SPBConstantNames, defined above). }
    SPBVal := 0;
    SPBServerVal := 0;
    { Find the parameter }
    for j := 1 to isc_spb_last_spb_constant do
      if (param_name = SPBConstantNames[j]) then
      begin
        SPBVal := j;
        SPBServerVal := SPBConstantValues[j];
        break;
      end;
    case SPBVal of
      isc_spb_user_name, isc_spb_password:
      begin
        SPB := SPB +
               Char(SPBServerVal) +
               Char(Length(param_value)) +
               param_value;
        Inc(SPBLength, 2 + Length(param_value));
      end;
      else
      begin
        if (SPBVal > 0) and
           (SPBVal <= isc_dpb_last_dpb_constant) then
          IBError(ibxeSPBConstantNotSupported,
                   [SPBConstantNames[SPBVal]])
        else
          IBError(ibxeSPBConstantUnknown, [SPBVal]);
      end;
    end;
  end;
end;

constructor TFBServiceManager.Create(ServerName: string; Protocol: TProtocol;
  Params: TStrings);
var SPB: String;
begin
  inherited Create;
  FProtocol := Protocol;
  GenerateSPB(Params, SPB, FSPBLength);
  with Firebird25ClientAPI do
    IBAlloc(FSPB, 0, FsPBLength);
  Move(SPB[1], FSPB[0], FSPBLength);
  FServerName := ServerName;
  Attach;
end;

destructor TFBServiceManager.Destroy;
begin
  inherited Destroy;
end;

function TFBServiceManager.GetStatus: IStatus;
begin
  Result := Firebird25ClientAPI.Status;
end;

procedure TFBServiceManager.Attach;
var ConnectString: String;
begin
  case FProtocol of
    TCP: ConnectString := FServerName + ':service_mgr'; {do not localize}
    SPX: ConnectString := FServerName + '@service_mgr'; {do not localize}
    NamedPipe: ConnectString := '\\' + FServerName + '\service_mgr'; {do not localize}
    Local: ConnectString := 'service_mgr'; {do not localize}
  end;
  with Firebird25ClientAPI do
    Call(isc_service_attach(StatusVector, Length(ConnectString),
                           PChar(ConnectString), @FHandle,
                           FSPBLength, FSPB));
end;

procedure TFBServiceManager.Detach;
begin
  CheckActive;
  with Firebird25ClientAPI do
  if (Call(isc_service_detach(StatusVector, @FHandle), False) > 0) then
  begin
    FHandle := nil;
    IBDataBaseError;
  end
  else
    FHandle := nil;
end;

function TFBServiceManager.IsAttached: boolean;
begin
  Result := FHandle <> nil;
end;

procedure TFBServiceManager.Start(Command: char; Params: TServiceCommandParams);
var i: integer;
begin
  with TServiceBuffer.Create(Command) do
  try
    for i := 0 to Length(Params) - 1 do
      AddVar(Params[i]);

    with Firebird25ClientAPI do
      Call(isc_service_start(StatusVector, @FHandle, nil,
                           DataSize, Buffer));
  finally
    Free;
  end;
end;

function TFBServiceManager.Query(Command: char; Params: TServiceQueryParams): TServiceQueryResponse;
var i: integer;
    response: TServiceResponseBuffer;
begin
  with TServiceBuffer.Create(Command) do
  try
    for i := 0 to Length(Params) - 1 do
      AddVar(Params[i]);

    response := TServiceResponseBuffer.Create;
    try
      with Firebird25ClientAPI do
        Call(isc_service_query(StatusVector, @FHandle, nil, 0, nil,
                           DataSize, Buffer,
                           response.Size, response.Buffer));

      Result := response.ParseBuffer;

    finally
      response.Free;
    end;
  finally
    Free;
  end;
end;

function TFBServiceManager.Query(Commands: array of char
  ): TServiceQueryResponse;
var CommandBuffer: string;
    i: integer;
    response: TServiceResponseBuffer;
begin
  CommandBuffer := '';
  for i := 0 to Length(Commands) - 1 do
    CommandBuffer += Commands[i];
  response := TServiceResponseBuffer.Create;
  try
    with Firebird25ClientAPI do
      Call(isc_service_query(StatusVector, @FHandle, nil, 0, nil,
                         Length(CommandBuffer), PChar(CommandBuffer),
                         response.Size, response.Buffer));

    Result := response.ParseBuffer;

  finally
    response.Free;
  end;
end;

procedure TFBServiceManager.Release;
begin
  if IsAttached then Detach;
  Free;
end;

end.

