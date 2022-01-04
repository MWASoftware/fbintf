(*
 *  Firebird Interface (fbintf) Test suite. This program is used to
 *  test the Firebird Pascal Interface and provide a semi-automated
 *  pass/fail check for each test.
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
 *  The Original Code is (C) 2016-2020 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBUDRTestApp;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

interface

uses
  Classes, SysUtils, TestApplication, IB, FBUDRController, FBUdrPlugin;

type

  { TFBUDRTestSuite }

  { TFBUDRTestApp }

  TFBUDRTestApp = class(TTestApplication)
  public
    constructor Create(TheOwner: TComponent); override;
    function getModuleName: AnsiString; virtual;
  end;

  { TFBUDRTestBase }

  TFBUDRTestBase = class(TTestBase)
  private
    FUDRPlugin: TFBUdrPlugin;
    function getModuleName: AnsiString;
  protected
    procedure CreateObjects(Application: TTestApplication); override;
  public
    destructor Destroy; override;
    procedure ApplyDDL(attachment: IAttachment; sql: array of AnsiString);
    property UDRPlugin: TFBUdrPlugin read FUDRPlugin;
  end;


implementation

{ TFBUDRTestBase }

function TFBUDRTestBase.getModuleName: AnsiString;
begin
  Result := (Owner as TFBUDRTestApp).getModuleName;
end;

procedure TFBUDRTestBase.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  if FUDRPlugin = nil then
    FUDRPlugin := TFBUdrPlugin.Create(getModuleName);
end;

destructor TFBUDRTestBase.Destroy;
begin
  if FUDRPlugin <> nil then
    FUDRPlugin.Free;
  inherited Destroy;
end;

procedure TFBUDRTestBase.ApplyDDL(attachment: IAttachment;
  sql: array of AnsiString);
var i: integer;
begin
  for i := 0 to length(sql) - 1 do
  try
    attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sql[i]);
  except on E:Exception do
    writeln(OutFile,'ApplyDDL problem: ',E.Message);
  end;
end;


{ TFBUDRTestSuite }

constructor TFBUDRTestApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

function TFBUDRTestApp.getModuleName: AnsiString;
begin
  Result := 'udrfbtestsuite';
end;

end.

