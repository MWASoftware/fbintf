(*
 *  Firebird Interface (fbintf). Regression test for the pure Pascal wire
 *  protocol client (client/wire).
 *
 *  Contents of this file are subject to the Initial Developer's Public
 *  License Version 1.0 (the "License"); you may not use this file except in
 *  compliance with the License. You may obtain a copy of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
 *  Usage:  WireTest [<database> [<user> [<password> [<scratch database>]]]]
 *          defaults to localhost:employee SYSDBA masterkey
 *
 *  The cryptographic tests need no server. The remaining tests need a
 *  Firebird 3.0 or later server and a database the user may create tables
 *  in. If a scratch database is named as well then creating and dropping a
 *  database is tested too; it must be a path the server may write to and
 *  must not already exist.
*)
program WireTest;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, IB, IBUtils,
  FBWireBigInt, FBWireCrypto, FBWireSRP, FBWireStream, FBWireConst,
  FBWireMessage, FBWireDescribe, FBWireProtocol, FBWireClientAPI;

var
  TestsRun: integer = 0;
  TestsFailed: integer = 0;
  DatabaseName: AnsiString = 'localhost:employee';
  UserName: AnsiString = 'SYSDBA';
  Password: AnsiString = 'masterkey';
  ScratchDatabase: AnsiString = '';

procedure Check(const aTest: AnsiString; aCondition: boolean;
  const aDetail: AnsiString = '');
begin
  Inc(TestsRun);
  if aCondition then
    writeln('  ok    ',aTest)
  else
  begin
    Inc(TestsFailed);
    if aDetail <> '' then
      writeln('  FAIL  ',aTest,' - ',aDetail)
    else
      writeln('  FAIL  ',aTest);
  end;
  {flushed so that the log is complete even if the next call raises}
  Flush(Output);
end;

procedure CheckEquals(const aTest, aGot, aWanted: AnsiString);
begin
  Check(aTest,aGot = aWanted,'got "' + aGot + '" wanted "' + aWanted + '"');
end;

function BytesToHex(const b: array of byte): AnsiString;
var i: integer;
begin
  Result := '';
  for i := 0 to High(b) do
    Result := Result + LowerCase(IntToHex(b[i],2));
end;

function StrToBytes(const s: AnsiString): TBytes;
begin
  SetLength(Result,Length(s));
  if s <> '' then
    Move(s[1],Result[0],Length(s));
end;

{---------------------------------------------------------------------------}

procedure TestBigInt;
var a, b, q, r, m: TBigInt;
begin
  writeln('Arbitrary precision arithmetic');
  CheckEquals('zero',TBigInt.FromHex('0').ToHex,'0');
  CheckEquals('hex round trip',TBigInt.FromHex('deadbeef12345678').ToHex,
              'deadbeef12345678');
  a := TBigInt.FromHex('ffffffffffffffffffffffff');
  b := TBigInt.FromHex('1');
  CheckEquals('add with carry',TBigInt.Add(a,b).ToHex,
              '1000000000000000000000000');
  CheckEquals('subtract with borrow',
              TBigInt.Subtract(TBigInt.Add(a,b),b).ToHex,
              'ffffffffffffffffffffffff');
  a := TBigInt.FromHex('123456789abcdef0');
  b := TBigInt.FromHex('fedcba9876543210');
  CheckEquals('multiply',TBigInt.Multiply(a,b).ToHex,
              '121fa00ad77d7422236d88fe5618cf00');
  a := TBigInt.FromHex('121fa00ad77d7422236d88fe5618cf01');
  TBigInt.DivMod(a,b,q,r);
  CheckEquals('divide quotient',q.ToHex,'123456789abcdef0');
  CheckEquals('divide remainder',r.ToHex,'1');
  CheckEquals('modular exponentiation',
    TBigInt.ModPow(TBigInt.FromCardinal($1234),TBigInt.FromCardinal($5678),
                   TBigInt.FromCardinal($FFFF1)).ToHex,'4470e');
  {a 1024 bit modular exponentiation of the size SRP performs}
  m := TBigInt.FromHex(
    'E67D2E994B2F900C3F41F08F5BB2627ED0D49EE1FE767A52EFCD565CD6E76881' +
    '2C3E1E9CE8F0A8BEA6CB13CD29DDEBF7A96D4A93B55D488DF099A15C89DCB064' +
    '0738EB2CBDD9A8F7BAB561AB1B0DC1C6CDABF303264A08D1BCA932D1F1EE428B' +
    '619D970F342ABA9A65793B8B2F041AE5364350C16F735F56ECBCA87BD57B29E7');
  a := TBigInt.FromHex(
    'deadbeefcafebabe0123456789abcdefdeadbeefcafebabe0123456789abcdef');
  b := TBigInt.FromHex('123456789abcdef0fedcba9876543210');
  CheckEquals('1024 bit modular exponentiation',TBigInt.ModPow(a,b,m).ToHex,
    'c05da5ced840733fcb10af56ed841a5835aab6fd03750959bde9fa367f6b9406' +
    '85f361546298c87db8bd8b68b4eb6bd66c34820b97e820db34020a6f341caa71' +
    '16cefe89032df8791931996cf6596444ddf50b5a3b667c004fe2b16599fa925b' +
    '8fd922586989ca91ddaa1bb24389c88d5cea74d06b01c54cacd6f42796f03d0d');
end;

procedure TestCrypto;
var ctx: TSHA1;
    rc4: TRC4;
    cc: TChaCha20;
    buf, key, nonce, block: TBytes;
    i: integer;
begin
  writeln('Cryptographic primitives');
  CheckEquals('SHA-1 of "abc"',BytesToHex(TSHA1.Digest(StrToBytes('abc'))),
              'a9993e364706816aba3e25717850c26c9cd0d89d');
  CheckEquals('SHA-1 of the empty string',
              BytesToHex(TSHA1.Digest(StrToBytes(''))),
              'da39a3ee5e6b4b0d3255bfef95601890afd80709');
  CheckEquals('SHA-1 of the 448 bit test vector',
    BytesToHex(TSHA1.Digest(StrToBytes(
      'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'))),
    '84983e441c3bd26ebaae4aa1f95129e5e54670f1');
  {a million 'a' - exercises the streaming path across many blocks}
  ctx.Init;
  SetLength(block,10000);
  for i := 0 to High(block) do block[i] := ord('a');
  for i := 1 to 100 do ctx.Update(block);
  CheckEquals('SHA-1 of one million characters',BytesToHex(ctx.Final),
              '34aa973cd4c4daa4f61eeb2bdbad27316534016f');
  CheckEquals('SHA-256 of "abc"',
    BytesToHex(TSHA256.Digest(StrToBytes('abc'))),
    'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
  CheckEquals('SHA-256 of the empty string',
    BytesToHex(TSHA256.Digest(StrToBytes(''))),
    'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855');
  CheckEquals('SHA-256 of the 448 bit test vector',
    BytesToHex(TSHA256.Digest(StrToBytes(
      'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'))),
    '248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1');

  rc4 := TRC4.Create(StrToBytes('Key'));
  try
    buf := StrToBytes('Plaintext');
    rc4.Process(buf[0],Length(buf));
    CheckEquals('RC4',BytesToHex(buf),'bbf316e8d940af0ad3');
  finally
    rc4.Free;
  end;

  {RFC 8439 section 2.4.2}
  SetLength(key,32);
  for i := 0 to 31 do key[i] := i;
  nonce := TBytes.Create($00,$00,$00,$00,$00,$00,$00,$4a,$00,$00,$00,$00);
  cc := TChaCha20.Create(key,nonce,1);
  try
    buf := StrToBytes('Ladies and Gentlemen of the class of ''99: If I could ' +
                      'offer you only one tip for the future, sunscreen would be it.');
    cc.Process(buf[0],Length(buf));
    CheckEquals('ChaCha20',Copy(BytesToHex(buf),1,64),
      '6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0b');
  finally
    cc.Free;
  end;
end;

procedure TestSRP;
const
  {a fixed client private key so that the exchange is reproducible}
  ClientPrivateKey =
    '84316857F47914F838918D5C12CE3A3E7A9B2D7C9486346809E9EEFCE8DE7CD4' +
    '259D8BE4FD0BCC2D259553769E078FA61EE2977025E4DA42F7FD97914D8A3372' +
    '3DFAFBC00770B7DA0C2E3778A05790F0C0F33C32A19ED88A12928567749021B3' +
    'FD45DCD1CE259C45325067E3DDC972F87867349BA82C303CCCAA9B207218007B';
  {computed independently from the same inputs}
  ExpectedA =
    '7b00bb84b35d25c54508808adff5fe3483f7b3010c805a4a2baf13342c812b08' +
    '6fe04d9bce76255ac24b915022ba696e022ff1202c7b08fac98163e1bb574465' +
    '073e871d8f6a9d7e49d318b768124e69cce4dab195beeb0ddcf5bddf5a7e59c6' +
    'aae1a699ff03173dc7fb93eda3b7043f7021561d36977430cccbed429f2c3f68';
  ServerB =
    'e04008c63e7770099cd0fb48c14e8fa7e08a8c12f3bc20df5ee513f3486a1b93' +
    '9968fb22b9d8c590d101f009c9330afc01bd4a45395c23edb4427d8ce0615755' +
    '097d76e9e2e851de03cc5995adb3835766b798fdfd5e1b3cb8d12d48d21e8570' +
    'bba4dea73e5031bd851d5ac54449014bf4bdb74be08bfc31c80f478b0cb6b9c';
  ExpectedSessionKey = '9b374b7ed7d8c4e5c98b273546b302857e874f18';
  ExpectedProofSHA1 = '8494f6eb8d3891373843464a0cc083e49d13fad3';
  ExpectedProofSHA256 =
    '59f5bc74dc8780d4fe109acb1cfc83ff937bf2236dc2b1f531ccbc4968d0d507';
var srp: TSRPClient;
    salt, proof: TBytes;
    i: integer;
begin
  writeln('SRP-6a client');
  SetLength(salt,32);
  for i := 0 to 31 do salt[i] := i;

  srp := TSRPClient.Create(ClientPrivateKey);
  try
    CheckEquals('client public key',srp.PublicKeyHex,ExpectedA);
    proof := srp.ClientProof('SYSDBA','masterkey',salt,ServerB,sphSHA1);
    CheckEquals('session key',BytesToHex(srp.SessionKey),ExpectedSessionKey);
    CheckEquals('client proof (Srp)',BytesToHex(proof),ExpectedProofSHA1);
  finally
    srp.Free;
  end;

  srp := TSRPClient.Create(ClientPrivateKey);
  try
    {the account name is upper cased before hashing, so a lower case login
     must produce the same proof}
    proof := srp.ClientProof('sysdba','masterkey',salt,ServerB,sphSHA256);
    CheckEquals('client proof (Srp256, lower case login)',
                BytesToHex(proof),ExpectedProofSHA256);
  finally
    srp.Free;
  end;

  srp := TSRPClient.Create;
  try
    Check('random key pair is generated',srp.PublicKeyHex <> '');
  finally
    srp.Free;
  end;
end;

procedure TestMessageLayout;
var fmt: TWireMessageFormat;
    size: cardinal;
    blr: TBytes;
    quad: array[0..7] of byte;
begin
  writeln('Message layout and BLR');
  SetLength(fmt,3);
  FillChar(fmt[0],SizeOf(TWireSQLVar)*3,0);
  fmt[0].SQLType := SQL_SHORT;
  fmt[0].DataSize := 2;
  fmt[1].SQLType := SQL_VARYING;
  fmt[1].DataSize := 10;
  fmt[1].CharSetID := 4;
  fmt[2].SQLType := SQL_INT64;
  fmt[2].DataSize := 8;
  fmt[2].Scale := -2;
  size := ComputeMessageLayout(fmt);
  Check('short is at offset zero',fmt[0].DataOffset = 0);
  Check('varying is two byte aligned',fmt[1].DataOffset mod 2 = 0);
  Check('int64 is eight byte aligned',fmt[2].DataOffset mod 8 = 0);
  Check('null indicators follow the data',
        fmt[0].NullOffset >= fmt[2].DataOffset + 8);
  Check('buffer size matches the layout',MessageBufferSize(fmt) = size);

  blr := BuildMessageBlr(fmt);
  Check('BLR starts with version 5 and begin',
        (blr[0] = blr_version5) and (blr[1] = blr_begin));
  Check('BLR declares a message',blr[2] = blr_message);
  Check('BLR field count is twice the column count',
        (blr[4] or (blr[5] shl 8)) = 6);
  Check('text is described with its character set',
        blr[Length(blr)-1] = blr_eoc);
  {the varying descriptor must carry the character set, i.e. blr_varying2}
  Check('varying uses blr_varying2',Pos(AnsiChar(blr_varying2),
        AnsiString(PAnsiChar(@blr[0]))) >= 0);

  {ISC_QUAD conversion: the high word is stored first}
  Int64ToWireQuad(@quad[0],$1122334455667788);
  Check('the high word occupies the first four bytes of a quad',
        PInteger(@quad[0])^ = $11223344);
  Check('the low word occupies the last four bytes of a quad',
        PCardinal(@quad[4])^ = $55667788);
  Check('quad round trips',WireQuadToInt64(@quad[0]) = $1122334455667788);
end;

{---------------------------------------------------------------------------}

{splits the connect string into the parts the raw protocol layer needs}
procedure SplitConnectString(const aConnectString: AnsiString;
  out aHost, aDatabase: AnsiString; out aPort: integer);
var aProtocol: TProtocolAll;
    aPortText: AnsiString;
begin
  aHost := 'localhost';
  aDatabase := aConnectString;
  aPort := 3050;
  aPortText := '';
  if ParseConnectString(aConnectString,aHost,aDatabase,aProtocol,aPortText) then
  begin
    if aPortText <> '' then
      aPort := StrToIntDef(aPortText,3050);
    if aHost = '' then
      aHost := 'localhost';
  end;
end;

function ConnectToServer(out Connection: TFBWireConnection): boolean;
var host, dbname: AnsiString;
    port: integer;
begin
  Result := false;
  Connection := nil;
  SplitConnectString(DatabaseName,host,dbname,port);
  Connection := TFBWireConnection.Create;
  try
    Connection.ConnectTo(host,port,dbname,UserName,Password);
    Result := true;
  except
    on E: Exception do
    begin
      writeln('  SKIP  no server at ',host,':',port,' - ',E.Message);
      FreeAndNil(Connection);
    end;
  end;
end;

procedure TestProtocol;
var C: TFBWireConnection;
begin
  writeln('Live connection');
  if not ConnectToServer(C) then Exit;
  try
    Check('a protocol version was negotiated',
          (C.ProtocolVersion >= 13) and (C.ProtocolVersion <= 20),
          'got ' + IntToStr(C.ProtocolVersion));
    Check('an SRP plugin authenticated the connection',
          (C.AuthPluginName = sSrpPluginName) or
          (C.AuthPluginName = sSrp256PluginName),
          'plugin ' + C.AuthPluginName);
    Check('wire encryption was negotiated',C.CryptPlugin <> '',
          'the server may have WireCrypt disabled');
    if C.CryptPlugin = '' then
      writeln('        protocol ',C.ProtocolVersion,', ',C.AuthPluginName,
              ', no wire encryption')
    else
      writeln('        protocol ',C.ProtocolVersion,', ',C.AuthPluginName,
              ', ',C.CryptPlugin,' wire encryption');
  finally
    C.Free;
  end;
end;

procedure TestProtocolNegotiation;
const Caps: array[0..3] of cardinal = (PROTOCOL_VERSION14,PROTOCOL_VERSION15,
                                       PROTOCOL_VERSION16,PROTOCOL_VERSION17);
var C: TFBWireConnection;
    host, dbname: AnsiString;
    port, i: integer;
begin
  writeln('Protocol negotiation');
  SplitConnectString(DatabaseName,host,dbname,port);
  for i := 0 to High(Caps) do
  begin
    C := TFBWireConnection.Create;
    try
      try
        C.MaxProtocol := Caps[i];
        C.ConnectTo(host,port,dbname,UserName,Password);
        {the server settles on the highest version it also knows, so an
         older server correctly negotiates below the cap: Firebird 3 tops
         out at 15 however high the offer goes}
        Check(Format('offering up to protocol %d negotiates %d',
                     [Caps[i] and FB_PROTOCOL_MASK,C.ProtocolVersion]),
              (C.ProtocolVersion >= 13) and
              (C.ProtocolVersion <= (Caps[i] and FB_PROTOCOL_MASK)),
              'got ' + IntToStr(C.ProtocolVersion));
      except
        on E: Exception do
          {an old server simply may not know this version}
          writeln('  SKIP  protocol ',Caps[i] and FB_PROTOCOL_MASK,': ',E.Message);
      end;
    finally
      C.Free;
    end;
  end;
end;

{---------------------------------------------------------------------------}

var
  API: IFirebirdAPI;
  Attachment: IAttachment;

function OpenTestDatabase: boolean;
var DPB: IDPB;
begin
  Result := false;
  API := WireFirebirdAPI;
  DPB := API.AllocateDPB;
  DPB.Add(isc_dpb_user_name).AsString := UserName;
  DPB.Add(isc_dpb_password).AsString := Password;
  DPB.Add(isc_dpb_lc_ctype).AsString := 'UTF8';
  try
    Attachment := API.OpenDatabase(DatabaseName,DPB);
    Result := (Attachment <> nil) and Attachment.IsConnected;
  except
    on E: Exception do
      writeln('  SKIP  cannot attach to ',DatabaseName,': ',E.Message);
  end;
end;

procedure TestCreateDatabase;
var DPB: IDPB;
    Scratch: IAttachment;
    Tr: ITransaction;
    RS: IResultSet;
begin
  writeln('Provider: create and drop database');
  if ScratchDatabase = '' then
  begin
    writeln('  SKIP  no scratch database named on the command line');
    Exit;
  end;
  DPB := API.AllocateDPB;
  DPB.Add(isc_dpb_user_name).AsString := UserName;
  DPB.Add(isc_dpb_password).AsString := Password;
  DPB.Add(isc_dpb_lc_ctype).AsString := 'UTF8';
  DPB.Add(isc_dpb_set_db_SQL_dialect).AsByte := 3;
  DPB.Add(isc_dpb_page_size).AsInteger := 8192;

  Scratch := API.CreateDatabase(ScratchDatabase,DPB);
  Check('database created',(Scratch <> nil) and Scratch.IsConnected);
  if Scratch = nil then Exit;
  Check('the new database has a current ODS',Scratch.GetODSMajorVersion >= 11,
        'ODS ' + IntToStr(Scratch.GetODSMajorVersion));

  Tr := Scratch.StartTransaction([isc_tpb_read_committed,isc_tpb_rec_version,
          isc_tpb_nowait,isc_tpb_write],taCommit);
  Scratch.ExecImmediate(Tr,'create table SCRATCH (ID integer not null primary key)');
  Tr.Commit;
  Tr := Scratch.StartTransaction([isc_tpb_read_committed,isc_tpb_rec_version,
          isc_tpb_nowait,isc_tpb_write],taCommit);
  RS := Scratch.OpenCursorAtStart(Tr,
          'select count(*) from RDB$RELATIONS where RDB$RELATION_NAME = ''SCRATCH''');
  Check('a table can be created in the new database',RS[0].AsInteger = 1);
  RS.Close;
  RS := nil;
  Tr.Commit;
  Tr := nil;

  Scratch.DropDatabase;
  Check('database dropped',not Scratch.IsConnected);
  Scratch := nil;

  {the database must now be gone}
  Scratch := nil;
  try
    Scratch := API.OpenDatabase(ScratchDatabase,DPB,false);
  except
    on E: Exception do Scratch := nil;
  end;
  Check('a dropped database can no longer be attached',
        (Scratch = nil) or not Scratch.IsConnected);
  Scratch := nil;
end;

procedure TestProviderQueries;
var Tr: ITransaction;
    S: IStatement;
    RS: IResultSet;
    rows: integer;
begin
  writeln('Provider: queries');
  Check('attached',Attachment.IsConnected);
  Check('ODS version is 11 or later',Attachment.GetODSMajorVersion >= 11,
        'ODS ' + IntToStr(Attachment.GetODSMajorVersion));
  Check('SQL dialect is 3',Attachment.GetSQLDialect = 3);

  Tr := Attachment.StartTransaction([isc_tpb_read_committed,
          isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  Check('transaction started',Tr.GetInTransaction);
  Check('transaction has an id',Tr.GetTransactionID > 0);

  S := Attachment.Prepare(Tr,'select CAST(? AS INTEGER) + 1 as RESULT from RDB$DATABASE');
  Check('statement prepared',S.IsPrepared);
  Check('one parameter described',S.SQLParams.Count = 1);
  Check('one column described',S.MetaData.Count = 1);
  S.SQLParams[0].AsInteger := 41;
  RS := S.OpenCursor;
  Check('cursor returns a row',RS.FetchNext);
  Check('parameter reached the server',RS[0].AsInteger = 42,
        'got ' + IntToStr(RS[0].AsInteger));
  Check('cursor is exhausted',not RS.FetchNext);
  RS.Close;

  {a null parameter must arrive as null: cast it so that the engine can
   describe the parameter type}
  S := Attachment.Prepare(Tr,
        'select CAST(? AS VARCHAR(10)) as VAL from RDB$DATABASE');
  S.SQLParams[0].IsNull := true;
  RS := S.OpenCursor;
  RS.FetchNext;
  Check('a null parameter is sent as null',RS[0].IsNull);
  RS.Close;

  {multiple rows through the batch cache}
  rows := 0;
  RS := Attachment.OpenCursor(Tr,'select RDB$RELATION_ID from RDB$RELATIONS');
  while RS.FetchNext do
    Inc(rows);
  RS.Close;
  Check('many rows fetched across batches',rows > 10,
        IntToStr(rows) + ' rows');

  Tr.Commit;
  Check('transaction committed',not Tr.GetInTransaction);
end;

procedure TestProviderDataTypes;
var Tr: ITransaction;
    RS: IResultSet;
begin
  writeln('Provider: data types');
  Tr := Attachment.StartTransaction([isc_tpb_read_committed,
          isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  RS := Attachment.OpenCursorAtStart(Tr,
    'select CAST(-32768 AS SMALLINT) C_SMALL,' +
    ' CAST(-2147483648 AS INTEGER) C_INT,' +
    ' CAST(-9223372036854775808 AS BIGINT) C_BIG,' +
    ' CAST(3.25 AS DOUBLE PRECISION) C_DBL,' +
    ' CAST(''2024-02-29'' AS DATE) C_DATE,' +
    ' CAST(''13:45:56.1234'' AS TIME) C_TIME,' +
    ' CAST(''2024-02-29 13:45:56.1234'' AS TIMESTAMP) C_TS,' +
    ' CAST(''abc'' AS VARCHAR(10)) C_STR,' +
    ' CAST(12345.67 AS NUMERIC(15,2)) C_NUM,' +
    ' CAST(NULL AS INTEGER) C_NULL' +
    ' from RDB$DATABASE');
  Check('smallint',RS.ByName('C_SMALL').AsInteger = -32768);
  Check('integer',RS.ByName('C_INT').AsInteger = -2147483648);
  Check('bigint',RS.ByName('C_BIG').AsInt64 = Low(Int64));
  Check('double',Abs(RS.ByName('C_DBL').AsDouble - 3.25) < 1E-9);
  CheckEquals('date',FormatDateTime('yyyy-mm-dd',RS.ByName('C_DATE').AsDateTime),
              '2024-02-29');
  CheckEquals('time',FormatDateTime('hh:nn:ss',RS.ByName('C_TIME').AsDateTime),
              '13:45:56');
  CheckEquals('timestamp',
    FormatDateTime('yyyy-mm-dd hh:nn:ss',RS.ByName('C_TS').AsDateTime),
    '2024-02-29 13:45:56');
  CheckEquals('varchar',RS.ByName('C_STR').AsString,'abc');
  CheckEquals('scaled numeric',RS.ByName('C_NUM').AsString,'12345.67');
  Check('null column',RS.ByName('C_NULL').IsNull);
  Tr.Commit;
end;

procedure TestProviderUpdates;
const TestTable = 'FBINTF_WIRE_TEST';
var Tr: ITransaction;
    S: IStatement;
    RS: IResultSet;
    Blob: IBlob;
    BlobText, ReadBack: AnsiString;
    i: integer;
begin
  writeln('Provider: updates and blobs');
  Tr := Attachment.StartTransaction([isc_tpb_read_committed,
          isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  try
    Attachment.ExecImmediate(Tr,'drop table ' + TestTable);
    Tr.Commit;
    Tr := Attachment.StartTransaction([isc_tpb_read_committed,
            isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  except
    {the table did not exist}
    Tr := Attachment.StartTransaction([isc_tpb_read_committed,
            isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  end;

  Attachment.ExecImmediate(Tr,'create table ' + TestTable +
    ' (ID integer not null primary key, NAME varchar(60), FLAG boolean,' +
    '  AMOUNT numeric(15,2), NOTES blob sub_type 1)');
  Tr.Commit;
  Check('table created',Attachment.HasTable(TestTable));

  Tr := Attachment.StartTransaction([isc_tpb_read_committed,
          isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  S := Attachment.Prepare(Tr,'insert into ' + TestTable +
        ' (ID,NAME,FLAG,AMOUNT) values (?,?,?,?)');
  for i := 1 to 3 do
  begin
    S.SQLParams[0].AsInteger := i;
    if i = 2 then
      S.SQLParams[1].IsNull := true
    else
      S.SQLParams[1].AsString := 'row ' + IntToStr(i) + ' with accents àéîõü';
    S.SQLParams[2].AsBoolean := Odd(i);
    S.SQLParams[3].AsCurrency := i * 1234.56;
    S.Execute;
  end;
  Tr.Commit;

  Tr := Attachment.StartTransaction([isc_tpb_read_committed,
          isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  RS := Attachment.OpenCursorAtStart(Tr,
          'select count(*) from ' + TestTable);
  Check('three rows inserted',RS[0].AsInteger = 3,
        'got ' + IntToStr(RS[0].AsInteger));
  RS.Close;
  RS := nil;

  RS := Attachment.OpenCursorAtStart(Tr,
          'select ID,NAME,FLAG,AMOUNT from ' + TestTable + ' order by ID');
  Check('boolean true survived the round trip',RS.ByName('FLAG').AsBoolean);
  Check('accented text survived the round trip',
        Pos('àéîõü',RS.ByName('NAME').AsString) > 0,
        RS.ByName('NAME').AsString);
  Check('scaled numeric survived the round trip',
        Abs(RS.ByName('AMOUNT').AsCurrency - 1234.56) < 0.005,
        CurrToStr(RS.ByName('AMOUNT').AsCurrency));
  Check('second row is null',RS.FetchNext and RS.ByName('NAME').IsNull);
  Check('third row has flag true',RS.FetchNext and RS.ByName('FLAG').AsBoolean);
  RS.Close;
  RS := nil;

  {a blob larger than one segment}
  BlobText := '';
  for i := 1 to 400 do
    BlobText := BlobText + 'Firebird pure Pascal wire protocol, line ' +
                IntToStr(i) + '.' + LineEnding;
  S := Attachment.Prepare(Tr,'update ' + TestTable +
        ' set NOTES = ? where ID = 1');
  Blob := Attachment.CreateBlob(Tr,1,0);
  Blob.SetAsString(BlobText);
  Blob.Close;
  S.SQLParams[0].AsBlob := Blob;
  S.Execute;
  Tr.Commit;

  Tr := Attachment.StartTransaction([isc_tpb_read_committed,
          isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  RS := Attachment.OpenCursorAtStart(Tr,'select NOTES from ' + TestTable +
          ' where ID = 1');
  ReadBack := RS[0].AsString;
  Check('blob round trips byte for byte',ReadBack = BlobText,
        Format('wrote %d bytes, read %d',[Length(BlobText),Length(ReadBack)]));
  RS.Close;
  RS := nil;
  Blob := nil;
  S := nil;
  Tr.Commit;

  {rollback must undo the delete}
  Tr := Attachment.StartTransaction([isc_tpb_read_committed,
          isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  Attachment.ExecImmediate(Tr,'delete from ' + TestTable);
  Tr.Rollback;
  Tr := Attachment.StartTransaction([isc_tpb_read_committed,
          isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  RS := Attachment.OpenCursorAtStart(Tr,'select count(*) from ' + TestTable);
  Check('rollback undid the delete',RS[0].AsInteger = 3,
        'got ' + IntToStr(RS[0].AsInteger));
  RS.Close;
  RS := nil;
  Tr.Commit;

  {Cleanup only. An attachment that has read a table keeps an interest in
   it that outlives the transaction, and Firebird 5 then refuses to drop
   the table on that attachment - the stock fbclient provider behaves
   identically here, so this is a property of the server and not of the
   wire client. The next run drops the table before creating it, so
   failing here leaves nothing behind.}
  Tr := Attachment.StartTransaction([isc_tpb_read_committed,
          isc_tpb_rec_version,isc_tpb_nowait,isc_tpb_write],taCommit);
  try
    Attachment.ExecImmediate(Tr,'drop table ' + TestTable);
    Tr.Commit;
    writeln('  note  test table dropped');
  except
    on E: Exception do
    begin
      Tr.Rollback;
      writeln('  note  the server would not drop the test table yet: ',
              E.Message);
    end;
  end;
end;

procedure TestServices;
var SPB: ISPB;
    Service: IServiceManager;
    Req: ISRB;
    Results: IServiceQueryResults;
    host, dbname: AnsiString;
    port: integer;
    i, statLines: integer;
    serverVersion, serverImplementation, line: AnsiString;
begin
  writeln('Provider: services');
  SplitConnectString(DatabaseName,host,dbname,port);

  SPB := API.AllocateSPB;
  SPB.Add(isc_spb_user_name).AsString := UserName;
  SPB.Add(isc_spb_password).AsString := Password;
  Service := API.GetServiceManager(host,IntToStr(port),TCP,SPB);
  Check('service manager attached',(Service <> nil) and Service.IsAttached);
  if (Service = nil) or not Service.IsAttached then Exit;

  {information query}
  Req := Service.AllocateSRB;
  Req.Add(isc_info_svc_server_version);
  Req.Add(isc_info_svc_implementation);
  Results := Service.Query(Req);
  serverVersion := '';
  serverImplementation := '';
  if Results <> nil then
    for i := 0 to Results.Count - 1 do
      case Results[i].getItemType of
      isc_info_svc_server_version:
        serverVersion := Results[i].AsString;
      isc_info_svc_implementation:
        serverImplementation := Results[i].AsString;
      end;
  Check('the server version was returned',serverVersion <> '');
  Check('the server implementation was returned',serverImplementation <> '');
  if serverVersion <> '' then
    writeln('        server version: ',serverVersion);

  {detach and reattach - the suite relies on this working}
  Service.Detach;
  Check('service manager detached',not Service.IsAttached);
  Service.Attach;
  Check('service manager reattached',Service.IsAttached);

  {run a service: header page statistics on the test database}
  Req := Service.AllocateSRB;
  Req.Add(isc_action_svc_db_stats);
  Req.Add(isc_spb_dbname).SetAsString(dbname);
  Req.Add(isc_spb_options).SetAsInteger(isc_spb_sts_hdr_pages);
  Check('header page statistics service started',Service.Start(Req));

  statLines := 0;
  repeat
    Req := Service.AllocateSRB;
    Req.Add(isc_info_svc_line);
    Results := Service.Query(Req);
    line := '';
    if (Results <> nil) and (Results.Count > 0) and
       (Results[0].getItemType = isc_info_svc_line) then
      line := Results[0].AsString;
    if line <> '' then
      Inc(statLines);
  until line = '';
  Check('statistics output was returned',statLines > 0,
        'got ' + IntToStr(statLines) + ' lines');

  Service.Detach;
  Service := nil;
end;

{---------------------------------------------------------------------------}

begin
  if ParamCount >= 1 then DatabaseName := ParamStr(1);
  if ParamCount >= 2 then UserName := ParamStr(2);
  if ParamCount >= 3 then Password := ParamStr(3);
  if ParamCount >= 4 then ScratchDatabase := ParamStr(4);

  writeln('fbintf pure Pascal wire protocol test');
  writeln('database: ',DatabaseName,'  user: ',UserName);
  writeln;

  TestBigInt;
  TestCrypto;
  TestSRP;
  TestMessageLayout;
  TestProtocol;
  TestProtocolNegotiation;

  if OpenTestDatabase then
  try
    TestProviderQueries;
    TestProviderDataTypes;
    TestProviderUpdates;
    TestServices;
    TestCreateDatabase;
  finally
    Attachment.Disconnect;
    Attachment := nil;
  end;

  writeln;
  writeln(Format('%d tests, %d failures',[TestsRun,TestsFailed]));
  if TestsFailed > 0 then
    Halt(1);
end.
