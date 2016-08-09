unit FBTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBExternals;

type
  (*********************************************************************)
  (** Blob id structure                                               **)
  (*********************************************************************)
  TGDS_QUAD = record
    gds_quad_high      : ISC_LONG;
    gds_quad_low       : UISC_LONG;
  end;
  TGDS__QUAD           = TGDS_QUAD;
  TISC_QUAD            = TGDS_QUAD;
  PGDS_QUAD            = ^TGDS_QUAD;
  PGDS__QUAD           = ^TGDS__QUAD;
  PISC_QUAD            = ^TISC_QUAD;


implementation

end.

