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
  public
    constructor Create; override;
    function TestTitle: string; override;
    procedure RunTest; override;
  end;

implementation

{ TTest1 }

constructor TTest1.Create;
begin
  inherited Create;
  FDatabaseName := 'localhost:/tmp/test.fdb';
  FUser := 'SYSDBA';
  FPassword := 'Pears';
  FCharSet := 'UTF8';
end;

function TTest1.TestTitle: string;
begin
  Result := 'Create and Drop a Database';
end;

procedure TTest1.RunTest;
var Params: TStringList;
begin
  writeln('Creating a Database');
  Params := TStringList.Create;
  try
    Params.Add('USER ''' + FUser + ''' ');
    Params.Add('PASSWORD ''' + FPassword + ''' ');
    Params.Add('DEFAULT CHARACTER SET ' + FCharSet + '');
    FirebirdAPI.CreateDatabase(FDatabaseName,FSQLDialect,Params);
  finally
    Params.Free;
  end;
  writeln('Database Created without error');
end;

initialization
  RegisterTest(TTest1);

end.

