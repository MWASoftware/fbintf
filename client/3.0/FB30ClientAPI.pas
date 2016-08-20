unit FB30ClientAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FBClientAPI, Firebird, IB;

type

  { TFB30ClientAPI }

  TFB30ClientAPI = class(TFBClientAPI,IFirebirdAPI)
  protected
    procedure LoadInterface; override;
  public
    constructor Create;

  public
    {IFirebirdAPI}
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
    function HasRollbackRetaining: boolean;
    function GetImplementationVersion: string;
  end;

var Firebird30ClientAPI: TFB30ClientAPI;

implementation

uses FBParamBlock;

{ TFB30ClientAPI }

procedure TFB30ClientAPI.LoadInterface;
begin
  inherited LoadInterface;
  if FMaster <> nil then
  begin
    MasterIntf := FMaster; {make global}
    PluginManager := MasterIntf.getPluginManager;
    Provider := MasterIntf.getDispatcher;
  end;
end;

constructor TFB30ClientAPI.Create;
begin
  inherited;
  FStatus := TFBStatus.Create;
  FStatusIntf := FStatus;
  Firebird30ClientAPI := self;
end;

function TFB30ClientAPI.GetStatus: IStatus;
begin

end;

function TFB30ClientAPI.AllocateDPB: IDPB;
begin
  Result := TDPB.Create;
end;

function TFB30ClientAPI.AllocateTPB: ITPB;
begin
  Result := TTPB.Create;
end;

function TFB30ClientAPI.OpenDatabase(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnConnectError: boolean): IAttachment;
begin
  Result := TFBAttachment.Create(DatabaseName, DPB, RaiseExceptionOnConnectError);
end;

function TFB30ClientAPI.CreateDatabase(DatabaseName: string; DPB: IDPB;
  RaiseExceptionOnError: boolean): IAttachment;
begin

end;

function TFB30ClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: array of byte; DefaultCompletion: TTransactionAction): ITransaction;
begin

end;

function TFB30ClientAPI.StartTransaction(Attachments: array of IAttachment;
  TPB: ITPB; DefaultCompletion: TTransactionAction): ITransaction;
begin

end;

function TFB30ClientAPI.AllocateSPB: ISPB;
begin

end;

function TFB30ClientAPI.GetServiceManager(ServerName: string;
  Protocol: TProtocol; SPB: ISPB): IServiceManager;
begin

end;

function TFB30ClientAPI.IsEmbeddedServer: boolean;
begin

end;

function TFB30ClientAPI.GetLibraryName: string;
begin

end;

function TFB30ClientAPI.HasServiceAPI: boolean;
begin

end;

function TFB30ClientAPI.HasRollbackRetaining: boolean;
begin

end;

function TFB30ClientAPI.GetImplementationVersion: string;
begin

end;

end.

