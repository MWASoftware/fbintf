(*
 *  Firebird UDR Support (fbudrtestbed). The fbudr components provide a set of
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
unit udr_test04;

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
  Classes, SysUtils, IB, FBUDRController, FBUDRIntf;

  {This unit provides the implementation of a UDR trigger and is used
   to test out various aspects of the TFBUDRTrigger class. Note that each class is
   registered with the FBUDRController at initialization time.}

type
  {This test adds an extra column to EMPLOYEE table to work with with an update before trigger.
   This is OLD_PHONE_EXT. The trigger checks to see if the Phone_Ext has been updated and, if so, the old
   value is saved in OLD_PHONE_EXT.

   Alter Table EMPLOYEE Add PREVIOUS_PHONE_EXT VarChar(4);

   Create or Alter Trigger MyEmployeeUpdate Active Before Update On  EMPLOYEE
   external name 'fbudrtests!my_employee_update'
    engine udr;
  }

  { TMyEmployeeUpdateTrigger }

  TMyEmployeeUpdateTrigger = class(TFBUDRTrigger)
  public
     procedure BeforeTrigger(context: IFBUDRExternalContext;
                           TriggerMetaData: IFBUDRTriggerMetaData;
                           action: TFBUDRTriggerAction;
                           OldParams: IFBUDRInputParams;
                           NewParams: IFBUDROutputData); override;
  end;

implementation

{ TMyEmployeeUpdateTrigger }

procedure TMyEmployeeUpdateTrigger.BeforeTrigger(
  context: IFBUDRExternalContext; TriggerMetaData: IFBUDRTriggerMetaData;
  action: TFBUDRTriggerAction; OldParams: IFBUDRInputParams;
  NewParams: IFBUDROutputData);
begin
  if (TriggerMetaData.getTriggerType <> ttBefore) or (action <> taUpdate) then
    raise Exception.CreateFmt('%s should be an update before trigger',[Name]);

  if OldParams.ByName('PHONE_EXT').AsString <> NewParams.ByName('PHONE_EXT').AsString then
    NewParams.ByName('PREVIOUS_PHONE_EXT').AsString := OldParams.ByName('PHONE_EXT').AsString;
end;

initialization
  FBRegisterUDRTrigger('my_employee_update', TMyEmployeeUpdateTrigger);

end.

