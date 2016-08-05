unit Test1;

{Create and Drop a Database}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestManager, IB;

type

  { TTest1 }

  TTest1 = class(TTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
  public
    function TestTitle: string; override;
    procedure RunTest(CharSet: string; SQLDialect: integer); override;
  end;

implementation

{ TTest1 }

procedure TTest1.DoQuery(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
    i: integer;
begin
    Transaction := Attachment.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],tcCommit);
    Statement := Attachment.Prepare(Transaction,'Select * from RDB$Database',3);
    ResultSet := Statement.OpenCursor;
    try
      while ResultSet.FetchNext do
      begin
        for i := 0 to ResultSet.getCount - 1 do
          writeln(ResultSet[i].Name,' = ',ResultSet[i].AsString);
      end;
    finally
      ResultSet.Close;
    end;
end;

function TTest1.TestTitle: string;
begin
  Result := 'Test 1: Create and Drop a Database';
end;

procedure TTest1.RunTest(CharSet: string; SQLDialect: integer);
var DPB: IDPB;
    CreateParams: string;
    Attachment: IAttachment;
    DBInfo: IDBInformation;
    ConType: integer;
    DBFileName: string;
    DBSiteName: string;
begin
  writeln('Creating a Database with empty parameters');
  try
    Attachment := FirebirdAPI.CreateDatabase('',SQLDialect,'',nil);
  except on e: Exception do
    writeln('Create Database fails: ',E.Message);
  end;

  writeln('Last Status Message: ' + FirebirdAPI.GetStatus.Getmessage);

  writeln('Creating a Database with no DPD');
  CreateParams := 'USER ''' + Owner.GetUserName + ''' PASSWORD ''' + Owner.GetPassword + ''' ' +
      'DEFAULT CHARACTER SET ' + CharSet;
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,SQLDialect,CreateParams,DPB);

  writeln('Dropping Database');
  Attachment.DropDatabase;

  {Open Database}
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString(CharSet);
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);

  PrintDPB(DPB);
  writeln('Creating a Database with a DPD');
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,SQLDialect,CreateParams,DPB);
  DBInfo := Attachment.GetDBInformation(byte(isc_info_db_id));
  DBInfo[0].DecodeIDCluster(ConType,DBFileName,DBSiteName);
  writeln('Database ID = ', ConType,' FB = ', DBFileName, ' SN = ',DBSiteName);
  DBInfo := Attachment.GetDBInformation(byte(isc_info_ods_version));
  write('ODS major = ',DBInfo[0].getAsInteger);
  DBInfo := Attachment.GetDBInformation(byte(isc_info_ods_minor_version));
  writeln(' minor = ', DBInfo[0].getAsInteger );

  {Querying Database}
  DoQuery(Attachment);

  writeln('Dropping Database');
  Attachment.DropDatabase;
end;


initialization
  RegisterTest(TTest1);

end.
