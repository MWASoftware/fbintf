program testsuite;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage utf8}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Test1, Test2, Test3, Test4, Test5,
  Test6, Test7, Test8, Test9, Test10, Test11, Test12, Test13, Test14, Test15,
  Test16, IB, Test17, Test18, TestApplication, FBTestApp;

type

  { TFBIntTestbed }

  TFBIntTestbed = class(TTestApplication)
    constructor Create(TheOwner: TComponent); override;
  end;

{ TFBIntTestbed }

constructor TFBIntTestbed.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

var
  Application: TFBIntTestbed;
begin
  Application := TFBIntTestbed.Create(nil);
  Application.Title:='Firebird API Test Suite';
  Application.Run;
  Application.Free;
end.

