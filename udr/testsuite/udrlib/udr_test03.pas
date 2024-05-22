(*
 *  Firebird UDR Support (fbudrtested). The fbudr components provide a set of
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
unit udr_test03;

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
  Classes, SysUtils, IB, FBUDRController, FBUDRIntf{$IFDEF FPC}, Streamex{$ENDIF};

  {This unit provides the implementation of selected number of UDR Select
   procedures used to test out various aspects of the TFBUDRSelectProcedure class.
   Note that each class is registered with the FBUDRController at initialization time.}

type
  {TMySelectProcedure implements a simple select procedure to return the
   list of employee salaries plus a line by line accumulator. The employee
   database is assumed.

   create or alter procedure MySelectProc ()
    returns (FullName VarChar(36), Salary Numeric(10,2), AccSalary Numeric(10,2) )
    external name 'fbudrtests!select_proc'
    engine udr;
   }

  TMySelectProcedure  = class(TFBUDRSelectProcedure)
  private
    FAccSalary: currency;
    FResults: IResultset;
  public
    procedure open(context: IFBUDRExternalContext;
                     ProcMetadata: IFBUDRProcMetadata;
                     InputParams: IFBUDRInputParams); override;
    function fetch(OutputData: IFBUDROutputData): boolean; override;
    procedure close; override;
  end;

  {TReadTextFile is a select procedure that reads lines from a text file and
   returns each line as successive rows.

   create or alter procedure MyReadText (
        path varchar(200) not null /*relative to udr directory */
        ) returns (
       text varchar(100) not null
   )
   external name 'fbudrtests!read_txt'
   engine udr;
}

  TReadTextFile  = class(TFBUDRSelectProcedure)
    private
      FTextFile: TStreamReader;
      FDefaultDir: string;
      {$ifndef unix}
      function GetSpecialFolder(const CSIDL: integer) : string;
      {$endif}
    public
      procedure InitProcedure; override;
      procedure open(context: IFBUDRExternalContext;
                       ProcMetadata: IFBUDRProcMetadata;
                       InputParams: IFBUDRInputParams);  override;
      function fetch(OutputData: IFBUDROutputData): boolean;  override;
      procedure close; override;
    end;

implementation

{$IFDEF FPC}
{$IFDEF WINDOWS }
uses WinDirs;
{$ENDIF}
{$ELSE}
uses ShlObj;
{$ENDIF}


{ TMySelectProcedure }

{open is called first and opens the cursor. The IResultset returned is saved
 as a private property of the class, and the accumulator is initialized to zero.}

procedure TMySelectProcedure.open(context: IFBUDRExternalContext;
  ProcMetadata: IFBUDRProcMetadata; InputParams: IFBUDRInputParams);
begin
  with context do
   FResults := GetAttachment.OpenCursor(GetTransaction,'Select Full_Name,Salary from EMPLOYEE order by EMP_NO');
  FAccSalary := 0;
end;

{fetch is called to return each row in the OutputData. The function returns
 false when at EOF.}

function TMySelectProcedure.fetch(OutputData: IFBUDROutputData): boolean;
begin
  Result := (FResults <> nil) and FResults.FetchNext;
  if Result then
  begin
    FAccSalary := FAccSalary + FResults.ByName('Salary').AsCurrency;
    OutputData.ByName('FullName').AsString := FResults.ByName('Full_Name').AsString;
    OutputData.ByName('Salary').AsCurrency := FResults.ByName('Salary').AsCurrency;
    OutputData.ByName('AccSalary').AsCurrency := FAccSalary;
  end;
end;

{close is called after fetch returned EOF. Here it is used to explicitly close
 the cursor. Although this will be closed automatically when the class is
 freed, or open called again.}

procedure TMySelectProcedure.close;
begin
  FResults := nil;
end;

procedure TReadTextFile.InitProcedure;
begin
  inherited InitProcedure;
  {$ifdef unix}
  FDefaultDir := GetTempDir;
  {$else}
  if FDefaultDir = '' then
    FDefaultDir := GetSpecialFolder(CSIDL_COMMON_APPDATA);
  {$endif}
end;

{$ifndef unix}
function TReadTextFile.GetSpecialFolder(const CSIDL: integer): string;
{$IFDEF FPC}
begin
  Result := GetWindowsSpecialDir(CSIDL);
end;
{$ELSE}
{$if not declared(MAX_PATH)}
const MAX_PATH=25;
{$ifend}
var
  RecPath : PChar;
begin
  RecPath := StrAlloc(MAX_PATH);
  try
  FillChar(RecPath^, MAX_PATH, 0);
  if SHGetSpecialFolderPath(0, RecPath, CSIDL, false)
    then result := RecPath
    else result := '';
  finally
    StrDispose(RecPath);
  end;
end;
{$ENDIF}
{$endif}

procedure TReadTextFile.open(context: IFBUDRExternalContext;
                 ProcMetadata: IFBUDRProcMetadata;
                 InputParams: IFBUDRInputParams);
var aFileName: AnsiString;
    {$IFDEF FPC}F: TFileStream;{$ENDIF}

begin
  context.WriteToLog('Read Text called in directory '+ GetCurrentDir);
  aFileName := FDefaultDir + DirectorySeparator + InputParams.ByName('path').AsString;
  if not FileExists(aFileName) then
    raise Exception.CreateFmt('Unable to find file "%s"',[aFileName]);
  context.WriteToLog('Reading from ' + aFileName);
  {$IFDEF FPC}
  F := TFileStream.Create(aFileName,fmOpenRead);
  FTextFile := TStreamReader.Create(F,8192,true);
  {$ELSE}
  FTextFile := TStreamReader.Create(aFileName, TEncoding.ANSI);
  {$ENDIF}
end;

function TReadTextFile.fetch(OutputData: IFBUDROutputData): boolean;
begin
  Result := not FTextFile.{$IFDEF FPC}EOF{$ELSE}EndOfStream{$ENDIF};
  if Result  then
    OutputData.ByName('text').AsString := FTextFile.ReadLine;
end;

procedure TReadTextFile.close;
begin
  if FTextFile <> nil then
    FTextFile.Free;
  FTextFile := nil;
end;

Initialization
  FBRegisterUDRProcedure('select_proc',TMySelectProcedure);
  FBRegisterUDRProcedure('read_txt',TReadTextFile);

end.

