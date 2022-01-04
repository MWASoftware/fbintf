(*
 *  Firebird UDR Support (fbudr). The fbudr components provide a set of
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
unit FBUDRMessage;

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
  Classes, SysUtils;

type
  TFBUDRError = (ibxeUnknownTriggerType,
                 ibxeUnknownTransactionAction,
                 ibxeNoTriggerMetadata,
                 ibxeInvalidFactoryObject,
                 ibxeNoProcMetadata
                );

  procedure FBUDRError(ErrMess: TFBUDRError; const Args: array of const);

resourcestring
  {Messages used by other units}
  SFirebirdStatusError  = 'Firebird API Status returned Error Code';

implementation

uses IB;

resourcestring
  SUnknownTriggerType           = 'Unknown Trigger Type (%d)';
  SUnknownTransactionAction     = 'Unknown Trigger Action (%d)';
  SNoTriggerMetadata            = 'Unable to get interface IFBTriggerMetaData';
  SInvalidFactoryObject         = 'Entry Point %s: %s is not recognised factory class';
  SNoProcMetadata               = 'Unable to get interface IFBProcMetadata';

const
  FBUDRErrorMessages: array[TFBUDRError] of string = (
    SUnknownTriggerType,
    SUnknownTransactionAction,
    SNoTriggerMetadata,
    SInvalidFactoryObject,
    SNoProcMetadata
    );

function GetErrorMessage(ErrMess: TFBUDRError): AnsiString;
begin
  Result := FBUDRErrorMessages[ErrMess];
end;


procedure FBUDRError(ErrMess: TFBUDRError; const Args: array of const);
begin
  raise EIBClientError.Create(Ord(ErrMess),
                              Format(GetErrorMessage(ErrMess), Args));
end;

end.

