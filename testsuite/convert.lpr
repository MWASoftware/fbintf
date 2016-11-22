program convert;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, LazUTF8
  { you can add units after this };

type

  { TUTF8ToConsole }

  TUTF8ToConsole = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TUTF8ToConsole }

procedure TUTF8ToConsole.DoRun;
var
  ErrorMsg: String;
  InFile,OutFile: TFileStream;
  InFileName, OutFileName: string;
  s: string;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hio', 'help infile outfile');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  InFileName := '';
  OutFileName := '';

  if HasOption('i','infile') then
    InFileName := GetOptionValue('i','infile');

  if HasOption('o','outfile') then
    OutFileName := GetOptionValue('o','outfile');

  if (InfileName = '') or (OutFileName = '') then
    raise Exception.Create('Both input and output files must be specified');

  InFile := TFileStream.Create(InFileName,fmOpenRead);
  try
    OutFile := TFileStream.Create(OutFileName,fmCreate);
    try
      SetLength(s,InFile.Size);
      InFile.Read(s[1],length(s));
      s := UTF8ToConsole(s);
      OutFile.Write(s[1],Length(s));
    finally
      OutFile.Free;
    end;
  finally
    InFile.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TUTF8ToConsole.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TUTF8ToConsole.Destroy;
begin
  inherited Destroy;
end;

procedure TUTF8ToConsole.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TUTF8ToConsole;
begin
  Application := TUTF8ToConsole.Create(nil);
  Application.Title := 'UTF8 to Console';
  Application.Run;
  Application.Free;
end.

