(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API. Although predominantly
 *  a new development they include source code taken from IBX and may be
 *  considered a derived product. This software thus also includes the copyright
 *  notice and license conditions from IBX.
 *
 *  Except for those parts dervied from IBX, contents of this file are subject
 *  to the Initial Developer's Public License Version 1.0 (the "License"); you
 *  may not use this file except in compliance with the License. You may obtain a
 *  copy of the License here:
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
unit FBClientLib;

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
  Classes, SysUtils, Firebird;

  {IFBIMasterProvider may be coerced from an IFirebirdAPI when the Firebird 3 API
   is provided and itself provides type safe access to the underlying Firebird
   IMaster interface - see Firebird.pas}

type
  IFBIMasterProvider = interface
    ['{c0202c92-5d2d-4130-b4df-427469aa42cf}']
    {Firebird 3 API}
    function GetIMasterIntf: Firebird.IMaster;
  end;

  {Example Code:
  var MasterProvider: IFBIMasterProvider;
  begin
   ..
     if FirebirdAPI.HasMasterIntf and (FirebirdAPI.QueryInterface(IFBIMasterProvider,MasterProvider) = S_OK) then
     with MasterProvider.GetIMasterIntf.getConfigManager do
     begin
       writeln('Firebird Bin Directory = ', getDirectory(DIR_BIN));
       writeln('Firebird Conf Directory = ', getDirectory(DIR_CONF));
       ..
  }



implementation

end.

