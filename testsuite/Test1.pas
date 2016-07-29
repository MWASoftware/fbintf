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
    FDatabaseName: string;
    FUser: string;
    FPassword: string;
    FCharSet: string;
    FSQLDialect: integer;
  private
    procedure DoQuery(Attachment: IAttachment);
  public
    constructor Create; override;
    function TestTitle: string; override;
    procedure RunTest; override;
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

constructor TTest1.Create;
begin
  inherited Create;
  FDatabaseName := 'localhost:/home/firebird/test1.fdb';
  FUser := 'SYSDBA';
  FPassword := 'celebr8';
  FCharSet := 'UTF8';
end;

function TTest1.TestTitle: string;
begin
  Result := 'Create and Drop a Database';
end;

procedure TTest1.RunTest;
var Params: TStringList;
    CreateParams: string;
    Attachment: IAttachment;
begin
  writeln('Creating a Database');
  CreateParams := 'USER ''' + FUser + ''' PASSWORD ''' + FPassword + ''' ' +
      'DEFAULT CHARACTER SET ' + FCharSet;
  {Open Database}
  Params := TStringList.Create;
  try
    Params.Add('user_name='+ FUser);
    Params.Add('password='+ FPassword);
    Params.Add('lc_ctype='+ FCharSet);
    Params.Add('sql_dialect='+IntToStr(FSQLDialect));
    Attachment := FirebirdAPI.CreateDatabase(FDatabaseName,FSQLDialect,CreateParams,Params);
    writeln('Database Created without error');
    writeln('Database ID = ',Attachment.GetInfoString(Char(isc_info_db_id)));
    writeln('ODS major = ',Attachment.GetInfoValue(char(isc_info_ods_version)),' minor = ',
       Attachment.GetInfoValue(char(isc_info_ods_minor_version)));

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

