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
 *  The Original Code is (C) 2020 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)

unit Test20;

{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

{ $DEFINE USELOCALDATABASE} //Remote fails - see https://github.com/FirebirdSQL/firebird/issues/6900

{Test 20: stress test IBatch interface}

interface

uses
  Classes, SysUtils, TestApplication, FBTestApp, IB {$IFDEF WINDOWS},Windows{$ENDIF};

type

  { TTest20 }

  TTest20 = class(TFBTestBase)
  private
    procedure DoTest(Attachment: IAttachment);
    procedure WriteBatchCompletion(bc: IBatchCompletion);
  public
    function TestTitle: AnsiString; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses IBUtils;

const
   sqlCreateTable = 'Create Table LotsOfData ('+
    'RowID integer not null,'+
    'theDate TimeStamp,'+
    'MyText VarChar(1024),'+
    'Primary Key (RowID)'+
  ');';

{ TTest20 }

procedure TTest20.WriteBatchCompletion(bc: IBatchCompletion);
var updated: integer;
begin
if bc <> nil then
  with bc do
  begin
    writeln(OutFile,'Batch Completion Info');
    writeln(OutFile,'Total rows processed = ',getTotalProcessed);
    updated := getUpdated;
    writeln(Outfile,'Updated Rows = ',updated);
    if updated > 0 then
    {$IFDEF FPC}
      writeln(Outfile,'Row ',updated,' State = ',getState(updated-1),' Msg = ',getStatusMessage(updated-1));
    {$ELSE}
      writeln(Outfile,'Row ',updated,' State = ',ord(getState(updated-1)),' Msg = ',getStatusMessage(updated-1));
    {$ENDIF}
  end;
end;

const
   RecordCount = 100000;
   RowLimit    = 50000;

procedure TTest20.DoTest(Attachment: IAttachment);
var Transaction: ITransaction;
    Statement: IStatement;
    i: integer;
    rows: integer;
    BC: IBatchCompletion;
    InMsgHash, OutMsgHash: TMsgHash;
    HashString: AnsiString;
    Results: IResultSet;
begin
  Attachment.getFirebirdAPI.getStatus.SetIBDataBaseErrorMessages([ShowSQLCode,
                                   ShowIBMessage,
                                   ShowSQLMessage]);
  Transaction := Attachment.StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  Statement := Attachment.Prepare(Transaction,'insert into LotsOfData values(?, current_timestamp, ?)');
  InMsgHash := TMsgHash.CreateMsgHash;
  rows := 0;
  Statement.SetBatchRowLimit(RowLimit);
  try
    for i := 1 to RecordCount do
    begin
       Statement.SQLParams[0].AsInteger := i;
       HashString := Format('asdbfkwfwf83274kjdfj0usd0uj329j9rfh38fvhuhsijf9u28rf4329jf-j9rghvvsw89rgf8yh%d', [i * 2]);
       Statement.SQLParams[1].AsString := HashString;
       InMsgHash.AddText(HashString);
       Inc(rows);
       if rows mod 10000 = 0 then
         writeln(Outfile,rows,' rows added');
       try
         Statement.AddToBatch;
       except
         on E: EIBBatchBufferOverflow do
           begin
             writeln(outfile,'Batch Execute');
             BC := Statement.ExecuteBatch;
             writeln(Outfile,'Intermediate Apply Batch on row ', i);
             WriteBatchCompletion(BC);
             Statement.AddToBatch;
           end
         else
           begin
             writeln(Outfile,'Exception raised on row ',i);
             raise;
           end;
       end;
    end;
    writeln(outfile,'Batch Execute');
    BC := Statement.ExecuteBatch;
    WriteBatchCompletion(BC);
    rows :=  Attachment.OpenCursorAtStart(Transaction,'Select count(*) From LOTSOFData')[0].AsInteger;
    writeln(Outfile,'Rows in Dataset = ',rows);
    InMsgHash.Finalise;
    writeln(Outfile,' Message Hash = ',InMsgHash.Digest);
    if rows <> RecordCount then
      writeln(Outfile,'Test Fails - expecting ',RecordCount,' rows - found ',rows);
    {Now check the table checksum}
    OutMsgHash := TMsgHash.CreateMsgHash;
    Results := Attachment.OpenCursor(Transaction,'Select MyText From LotsOfData Order by RowID');
    try
      while Results.FetchNext do
        OutMsgHash.AddText(Results[0].AsString);
      OutMsgHash.Finalise;
      writeln(Outfile,' Message Hash = ',OutMsgHash.Digest);
      if OutMsgHash.SameHash(InMsgHash) then
        writeln(Outfile,'Test Completed Successfully')
      else
        writeln(Outfile,'Test Failed - MD5 checksum error');
    finally
      OutMsgHash.Free;
    end;
  finally
    InMsgHash.Free;
  end;
end;


function TTest20.TestTitle: AnsiString;
begin
   Result := 'Test 20: Stress Test IBatch interface';
end;

procedure TTest20.RunTest(CharSet: AnsiString; SQLDialect: integer);
var DPB: IDPB;
    Attachment: IAttachment;
    VerStrings: TStringList;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(Owner.GetUserName);
  DPB.Add(isc_dpb_password).setAsString(Owner.GetPassword);
  DPB.Add(isc_dpb_lc_ctype).setAsString('UTF8');
  DPB.Add(isc_dpb_set_db_SQL_dialect).setAsByte(SQLDialect);
  {$IFDEF USELOCALDATABASE}
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetTempDatabaseName,DPB);
  {$ELSE}
  Attachment := FirebirdAPI.CreateDatabase(Owner.GetNewDatabaseName,DPB);
  {$ENDIF}
  VerStrings := TStringList.Create;
  try
    Attachment.getFBVersion(VerStrings);
    writeln(OutFile,' FBVersion = ',VerStrings[0]);
  finally
    VerStrings.Free;
  end;

  try
    if (FirebirdAPI.GetClientMajor < 4) or (Attachment.GetODSMajorVersion < 13) then
      writeln(OutFile,'Skipping test for Firebird 4 and later')
    else
    begin
      Attachment.ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable);
      try
        DoTest(Attachment);
      except on E:Exception do
        begin
          writeln(OutFile,'Exception writing data batch');
          writeln(Outfile,E.Message);
          raise;
        end;
      end;
    end;
  finally
    Attachment.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest20);
end.

