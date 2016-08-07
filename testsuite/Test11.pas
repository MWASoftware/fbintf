unit Test11;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest11 }

  TTest11 = class(TTestBase)
  private
    procedure GetStatistics(Service: IServiceManager; DBName: string);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;

implementation

{ TTest11 }

procedure TTest11.GetStatistics(Service: IServiceManager; DBName: string);
var Req: ISRB;
    Results: IServiceQueryResults;
begin
  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_info_svc_version);
  Req.Add(isc_info_svc_server_version);
  Req.Add(isc_info_svc_implementation);
  Results := Service.Query(Req);
  WriteServiceQueryResult(Results);

  Req := Service.AllocateRequestBuffer;
  Req.Add(isc_action_svc_db_stats);
  Req.Add(isc_spb_dbname).SetAsString(DBName);
  Req.Add(isc_spb_options).SetAsInteger(isc_spb_sts_data_pages or isc_spb_sts_db_log or
                                        isc_spb_sts_hdr_pages or isc_spb_sts_idx_pages or
                                        isc_spb_sts_sys_relations);

  Service.Start(Req);
{  Results := Service.Query(Req);
  WriteServiceQueryResult(Results);}
end;

function TTest11.TestTitle: string;
begin
  Result := 'Test 11: Services API';
end;

procedure TTest11.RunTest(CharSet: string; SQLDialect: integer);
var SPB: ISPB;
    Service: IServiceManager;
    I: integer;
    ServerName: string;
    DBName: string;
begin
  if not FirebirdAPI.HasServiceAPI then Exit;

  ServerName := Owner.GetEmployeeDatabaseName;
  I := Pos(':',ServerName);
  if i > 0 then
    DBName := system.copy(ServerName,i+1,length(ServerName) - 2);
  system.Delete(ServerName,i,Length(ServerName)-i+1);

  SPB := FirebirdAPI.AllocateSPB;
  SPB.Add(isc_spb_user_name_mapped_to_server).setAsString(Owner.GetUserName);
  SPB.Add(isc_spb_password_mapped_to_server).setAsString(Owner.GetPassword);
  Service := FirebirdAPI.GetServiceManager(ServerName,TCP,SPB);

  GetStatistics(Service,DBName);

end;

initialization
  RegisterTest(TTest11);

end.

