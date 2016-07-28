unit FB25APIObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, FBLibrary, IBExternals;

type

  { TAPIObject }

  TAPIObject = class(TObjectOwner)
  private
    FOwners: TList;
    FActivity: boolean;
  protected
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean = true): ISC_STATUS;
    procedure AddOwner(aOwner: TObject);
    procedure ResetActivity;
    procedure SignalActivity;
    property Owners: TList read FOwners;
  public
    constructor Create;
    destructor Destroy; override;
    function HasActivity: boolean;  {One shot - reset on read}
  end;

implementation

uses FB25ClientAPI;

{ TAPIObject }

function TAPIObject.Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
begin
  result := ErrCode;
  SignalActivity;
  if RaiseError and (ErrCode > 0) then
    Firebird25ClientAPI.IBDataBaseError;
end;

procedure TAPIObject.AddOwner(aOwner: TObject);
begin
  if (aOwner is TObjectOwner) and (FOwners.IndexOf(aOwner) = -1) then
  begin
    FOwners.Add(aOwner);
    TObjectOwner(aOwner).RegisterObj(self);
  end;
end;

procedure TAPIObject.ResetActivity;
var i: integer;
begin
  FActivity := false;
end;

procedure TAPIObject.SignalActivity;
var i: integer;
begin
  for i := 0 to FOwners.Count - 1 do
    if TObject(FOwners[i]) is TAPIObject then
      TAPIObject(FOwners[i]).SignalActivity;
  FActivity := true;
end;

constructor TAPIObject.Create;
begin
  inherited Create;
  FOwners := TList.Create;
end;

destructor TAPIObject.Destroy;
var i: integer;
begin
  if assigned(FOwners) then
  begin
    for i := 0 to FOwners.Count - 1 do
      TObjectOwner(FOwners[i]).UnRegisterObj(self);
    FOwners.Free;
  end;
  inherited Destroy;
end;

function TAPIObject.HasActivity: boolean;
begin
  Result := FActivity;
  ResetActivity;
end;

end.

