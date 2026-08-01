(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API.
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
 *  The Original Code is (C) 2016 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit FBSDL;
{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$interfaces COM}
{$ENDIF}

{ The SDL (slice description language) program that describes an array
  slice to the server. The generator is shared by every provider that
  builds its own SDL: the 3.0 provider hands it to getSlice/putSlice and
  the wire protocol provider to op_get_slice/op_put_slice. }

interface

uses
  Classes, SysUtils, IB, IBHeader, FBClientAPI, FBParamBlock;

type
  TSDLItem = class(TParamBlockItem,ISDLItem);

  { TSDLBlock }

  TSDLBlock = class (TCustomParamBlock<TSDLItem,ISDLItem>, ISDL)
  public
    constructor Create(api: TFBClientAPI);
  end;

{Generates the SDL describing the full slice of the array - based on
 gen_SDL from Firebird src/dsql/array.cpp}
function GenerateSDL(api: TFBClientAPI; aArrayDesc: PISC_ARRAY_DESC): ISDL;

implementation

function GenerateSDL(api: TFBClientAPI; aArrayDesc: PISC_ARRAY_DESC): ISDL;
var FSDL: ISDL;

  procedure AddVarInteger(aValue: integer);
  begin
    if (aValue >= -128) and (aValue <= 127) then
      FSDL.Add(isc_sdl_tiny_integer).SetAsTinyInteger(aValue)
    else
    if (aValue >= -32768) and (aValue <= 32767) then
      FSDL.Add(isc_sdl_short_integer).SetAsShortInteger(aValue)
    else
      FSDL.Add(isc_sdl_long_integer).SetAsInteger(aValue);
  end;

var i: integer;
    SDLItem: ISDLItem;
begin
  FSDL := TSDLBlock.Create(api);
  with aArrayDesc^ do
  begin
    SDLItem := FSDL.Add(isc_sdl_struct);
    SDLItem.SetAsByte(array_desc_dtype);

    case array_desc_dtype of
    blr_short,blr_long,
    blr_int64,blr_quad,
    blr_int128:
        SDLItem.AddShortInt(array_desc_scale);

    blr_text,blr_cstring, blr_varying:
        SDLItem.addShortInteger(array_desc_length);
    end;

    FSDL.Add(isc_sdl_relation).SetAsString(array_desc_relation_name);
    FSDL.Add(isc_sdl_field).SetAsString(array_desc_field_name);

    for i := 0 to array_desc_dimensions - 1 do
    begin
      if array_desc_bounds[i].array_bound_lower = 1 then
        FSDL.Add(isc_sdl_do1).SetAsTinyInteger(i)
      else
      begin
        FSDL.Add(isc_sdl_do2).SetAsTinyInteger(i);
        AddVarInteger(array_desc_bounds[i].array_bound_lower);
      end;
      AddVarInteger(array_desc_bounds[i].array_bound_upper);
    end;

    SDLItem := FSDL.Add(isc_sdl_element);
    SDLItem.AddByte(1);
    SDLItem := FSDL.Add(isc_sdl_scalar);
    SDLItem.AddByte(0);
    SDLItem.AddByte(array_desc_dimensions);
    for i := 0 to array_desc_dimensions - 1 do
    begin
      SDLItem := FSDL.Add(isc_sdl_variable);
      SDLItem.AddByte(i);
    end;
    FSDL.Add(isc_sdl_eoc);
  end;
  Result := FSDL;
end;

{ TSDLBlock }

constructor TSDLBlock.Create(api: TFBClientAPI);
begin
  inherited Create(api);
  FDataLength := 1;
  FBuffer^ := isc_sdl_version1;
end;

end.
