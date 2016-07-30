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
    Params: TStringList;
    ResultSet: IResultSet;
    i: integer;
begin
  Params := TStringList.Create;
  try
    Params.Add('read');
    Params.Add('nowait');
    Params.Add('concurrency');
    Transaction := Attachment.StartTransaction(Params,tcCommit);
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
  finally
    Params.Free;
  end;
end;

function TTest1.TestTitle: string;
begin
  Result := 'Create and Drop a Database';
end;

procedure TTest1.RunTest(CharSet: string; SQLDialect: integer);
var Params: TStringList;
    CreateParams: string;
    Attachment: IAttachment;
    DBInfo: IDBInformation;
    ConType: integer;
    DBFileName: string;
    DBSiteName: string;
begin
  writeln('Creating a Database');
  CreateParams := 'USER ''' + Owner.GetUserName + ''' PASSWORD ''' + Owner.GetPassword + ''' ' +
      'DEFAULT CHARACTER SET ' + CharSet;
  {Open Database}
  Params := TStringList.Create;
  try
    Params.Add('user_name='+ Owner.GetUserName);
    Params.Add('password='+ Owner.GetPassword);
    Params.Add('lc_ctype='+ CharSet);
    Params.Add('sql_dialect='+IntToStr(SQLDialect));
    Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,SQLDialect,CreateParams,Params);
    writeln('Database Created without error');
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
  finally
    Params.Free;
  end;
end;


(*procedure TTest1.RunTest;
var Params: TStringList;
    CreateParams: string;
    Attachment: IAttachment;
begin
  writeln('Creating a Database');
  CreateParams := 'USER ''' + FUser + ''' PASSWORD ''' + FPassword + ''' ' +
    'DEFAULT CHARACTER SET ' + FCharSet;
  Attachment := FirebirdAPI.CreateDatabase(FDatabaseName,FSQLDialect,CreateParams,nil);
  writeln('Database Created without error');
  writeln('Dropping Database');
  Attachment.DropDatabase;


{  Params := TStringList.Create;
  try
    {Open Database}
    Params.Add('user_name='+ FUser);
    Params.Add('password='+ FPassword);
    Params.Add('lc_ctype='+ FCharSet);
    Params.Add('sql_dialect='+IntToStr(FSQLDialect));
    writeln('Opening Database ' + FDatabaseName);
    Attachment := FirebirdAPI.OpenDatabase(FDatabaseName,Params);
    writeln('Database Opened');
    writeln('Dropping Database');
    Attachment.DropDatabase;
  finally
    Params.Free;
  end; }
end; *)

initialization
  RegisterTest(TTest1);

end.

