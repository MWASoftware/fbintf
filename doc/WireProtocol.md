# The pure Pascal wire protocol client

`client/wire` implements a Firebird client that speaks the remote (wire)
protocol directly over TCP. It needs no `fbclient` library on the client
machine: the compiled program is the whole dependency.

It is a third provider alongside the existing two. `client/2.5` binds the
legacy ISC API, `client/3.0` binds the Firebird 3 object oriented API, and
both dynamically load `fbclient`. `client/wire` instead implements the
protocol those libraries speak, so the same fbintf interfaces are available
where installing a client library is impractical: containers, single file
deployments, cross compiled targets, or a machine whose installed client is
older than the server.

## Contents

1. [Using it](#using-it)
2. [What is implemented](#what-is-implemented)
3. [Architecture](#architecture)
4. [The protocol, as actually implemented](#the-protocol-as-actually-implemented)
5. [Authentication](#authentication)
6. [Wire encryption](#wire-encryption)
7. [Messages, BLR and data types](#messages-blr-and-data-types)
8. [Error reporting](#error-reporting)
9. [Testing](#testing)
10. [Continuous integration](#continuous-integration)
11. [Roadmap](#roadmap)

---

## Using it

The only difference from ordinary fbintf code is where the API comes from:
`WireFirebirdAPI` instead of `IB.FirebirdAPI`. Everything below that is the
same object model.

```pascal
uses IB, FBWireClientAPI;

var
  API: IFirebirdAPI;
  DPB: IDPB;
  Attachment: IAttachment;
  Transaction: ITransaction;
  Results: IResultSet;
begin
  API := WireFirebirdAPI;               {loads no library}

  DPB := API.AllocateDPB;
  DPB.Add(isc_dpb_user_name).AsString := 'SYSDBA';
  DPB.Add(isc_dpb_password).AsString := 'masterkey';
  DPB.Add(isc_dpb_lc_ctype).AsString := 'UTF8';

  Attachment := API.OpenDatabase('localhost:employee',DPB);
  Transaction := Attachment.StartTransaction(
                   [isc_tpb_read_committed,isc_tpb_rec_version,
                    isc_tpb_wait,isc_tpb_write],taCommit);

  Results := Attachment.OpenCursor(Transaction,
               'select EMP_NO, FULL_NAME from EMPLOYEE where EMP_NO < ?',[20]);
  while Results.FetchNext do
    writeln(Results[0].AsInteger:5,'  ',Results[1].AsString);

  Transaction.Commit;
  Attachment.Disconnect;
end;
```

Connect strings take the usual forms, including an explicit port:

```
localhost:employee
localhost:/var/lib/firebird/data/employee.fdb
db.example.com/3051:payroll
inet://db.example.com/payroll
```

The path after the host is resolved **by the server**, so it is a path (or
alias) on the server machine, not on the client.

### Passwords

The password is used for the SRP exchange and then removed from the DPB
before the attach is sent. It never travels over the network in any form,
encrypted or otherwise, which is the point of SRP: the server stores a
verifier, not the password, and both sides end up agreeing a session key
without transmitting the secret.

### Choosing the protocol version

`TFBWireConnection.MaxProtocol` caps the highest version offered in
`op_connect`. It defaults to the newest the client implements. Lowering it
is useful for reproducing a problem against an older dialect of the
protocol, and is how the version matrix below was measured.

---

## What is implemented

Working and covered by the test suite:

* the connection handshake with protocol negotiation, versions 13 to 18
* `Srp256` and `Srp` authentication, in both the `op_cond_accept` flow
  (authentication must finish before attaching) and the `op_accept_data`
  flow (the proof travels in the DPB with the attach)
* wire encryption with `ChaCha64`, `ChaCha` and `Arc4`
* attach, create and drop database, detach
* transactions: start, commit, rollback, the retaining variants, and the
  two phase commit prepare
* DSQL: allocate, prepare with a full describe, execute, execute with a
  singleton result (`op_execute2`), fetch, close, drop, set cursor name
* every Firebird data type, including `INT128`, `DECFLOAT(16)`,
  `DECFLOAT(34)`, `BOOLEAN` and the time zone types
* blobs: create, open, segmented read and write, close, cancel
* information calls for the database, transaction, statement and blob
* events: `IEvents` with asynchronous and synchronous waits, delivered on
  the `op_connect_request` auxiliary connection by a listener thread
* array columns: `IArray` and `IArrayMetaData` over `op_get_slice` and
  `op_put_slice`, with the SDL generator shared with the 3.0 provider
* operation cancellation (`IAttachment.CancelOperation` - `op_cancel`
  sent out of band from another thread) and statement timeouts
  (`IStatement.SetStatementTimeout` - the protocol 16 timeout field)
* scrollable cursors on protocol 18 servers: the five positioned fetches
  of `IResultSet` over `op_fetch_scroll`

Deliberately not implemented yet. These raise `ibxeNotSupported` rather
than failing in a confusing way:

| Feature | What it needs |
|---|---|
| Services | `op_service_*` exist in `FBWireProtocol`; the `IServiceManager` wrapper does not |
| Batches | the protocol 16 `op_batch_*` family |
| Multi database transactions | a two phase commit coordinator |

Delphi is not supported yet either: the transport is written against the
FPC socket units. Everything above the transport is compiler neutral, so a
Delphi transport is the only missing piece.

---

## Architecture

```
    your code
        │  IAttachment, ITransaction, IStatement, IResultSet, IBlob
        ▼
    ┌─────────────────────────────────────────────────────────┐
    │ FBWireClientAPI  IFirebirdAPI, status, date/time codecs │
    │ FBWireAttachment FBWireTransaction FBWireStatement      │
    │ FBWireBlob                                              │  provider
    └─────────────────────────────────────────────────────────┘
        │  handles, message buffers
        ▼
    ┌─────────────────────────────────────────────────────────┐
    │ FBWireProtocol   handshake, one method per packet       │
    │ FBWireDescribe   isc_info_sql describe parser           │
    │ FBWireMessage    buffer layout, BLR, row encoding       │  protocol
    └─────────────────────────────────────────────────────────┘
        │  XDR primitives
        ▼
    ┌─────────────────────────────────────────────────────────┐
    │ FBWireStream     buffered TCP, per direction ciphers    │
    │ FBWireSRP  FBWireCrypto  FBWireBigInt  FBWireConst      │  transport
    └─────────────────────────────────────────────────────────┘
        │  TCP
        ▼
    Firebird server
```

| Unit | Contents |
|---|---|
| `FBWireBigInt` | unsigned arbitrary precision arithmetic: Knuth algorithm D division, square and multiply modular exponentiation |
| `FBWireCrypto` | SHA-1, SHA-256, RC4, ChaCha20 |
| `FBWireSRP` | the client half of SRP-6a as the engine implements it |
| `FBWireStream` | buffered TCP transport, independent send and receive ciphers, the XDR codec |
| `FBWireConst` | operation codes and protocol constants from `protocol.h` |
| `FBWireMessage` | message buffer layout, BLR message descriptions, row encode and decode |
| `FBWireDescribe` | parser for the `isc_info_sql_*` describe response |
| `FBWireProtocol` | `TFBWireConnection`: the handshake and one method per packet exchange |
| `FBWireClientAPI` | `IFirebirdAPI`, the status object, the date and time codecs |
| `FBWireAttachment` `FBWireTransaction` `FBWireStatement` `FBWireBlob` | the provider proper |

`FBWireProtocol` is usable on its own if you want to speak the protocol
without the fbintf object model:

```pascal
Connection := TFBWireConnection.Create;
Connection.ConnectTo('localhost',3050,'employee','SYSDBA','masterkey');
writeln(Connection.ProtocolVersion, ' ', Connection.CryptPlugin);
DbHandle := Connection.AttachDatabase('employee',DPBBytes);
```

### Reused from fbintf

The provider deliberately reuses the existing machinery rather than
duplicating it:

* `FBParamBlock` builds the DPB, TPB, SPB and BPB clumplet buffers. Those
  buffers are exactly what the protocol carries, so they are sent verbatim.
* `FBOutputBlock` parses the information responses.
* `TSQLDataItem` supplies every data conversion. The provider only has to
  place a pointer at the right offset in the message buffer and report the
  type, scale and character set.
* `IBUtils` supplies connect string parsing and the named parameter
  preprocessor.

---

## The protocol, as actually implemented

The reference is `src/remote/protocol.h`, `protocol.cpp` and
`interface.cpp` in the Firebird source tree. The points below are the ones
where a plausible reading of the packet layouts produces a client that does
not work; each cost a debugging cycle.

### Framing

There is none. The connection is one continuous XDR stream and packet
boundaries are found by decoding field by field. Every 16 bit field
occupies 4 bytes on the wire (`xdr_short` widens), opaque data is zero
padded to a multiple of 4, and 64 bit values travel as two 32 bit words,
high word first.

### The version list

`op_connect` carries a list of `(version, architecture, min type, max type,
weight)` entries and the server picks the highest weight it understands.
Always send `arch_generic`: if the entry's architecture matches the
server's own, the server sets `PORT_symmetric` and starts transmitting
messages as raw memory images instead of XDR.

### Wire encryption key type

`op_crypt` carries the plugin name and a **key type**. That key type is the
one the server advertised in its key clumplets, in practice `Symmetric`. It
is not the authentication plugin name; sending that gets the connection
closed with *"Client attempted to start wire encryption using unknown key"*.

### Text columns need their character set

Text fields must be described with `blr_text2` / `blr_varying2` naming the
column's character set. With plain `blr_text` / `blr_varying` the engine
assumes the connection character set and reinterprets the declared byte
length as `length div max bytes per character`. A `VARCHAR(37)` fetched
over a UTF8 connection silently becomes 9 characters and the fetch fails
with a string truncation error.

### Booleans

XDR sends a boolean as a single value byte padded to four bytes, value
first. Sending it as a big endian integer puts the value in the last byte
and every boolean reads back as false.

### Fetches arrive as a stream

The server answers one `op_fetch` with a sequence of `op_fetch_response`
packets, one row each, terminated by a packet whose message count is zero
(or whose status is 100 at end of cursor). The whole batch must be drained
before anything else is sent, otherwise the queued row packets are mistaken
for the next request's response. Rows are decoded into a cache as they
arrive and handed out one at a time.

### Blob and array identifiers

`ISC_QUAD` puts the high word first, which is not the memory layout of an
`Int64` on a little endian machine. `WireQuadToInt64` and `Int64ToWireQuad`
convert.

---

## Authentication

`FBWireSRP` implements SRP-6a as the engine implements it, which is not
quite as the specification describes it. Each deviation is required for
interoperability and is commented where it is implemented:

* the exponent `a + u*x` is reduced modulo `N`;
* `H(N) xor H(g)` from RFC 5054 is a modular exponentiation, `H(N)^H(g) mod N`;
* the salt is hashed as its hexadecimal **text**, not as raw bytes;
* `k = SHA1(pad128(N), pad128(g))` pads its arguments to 128 bytes, while
  `A`, `B`, `S` and the proof components are hashed in minimal form with no
  padding;
* the account name is upper cased before hashing;
* the session key is always `SHA1(S)`, 20 bytes, even for `Srp256`. The
  plugin's hash is used only for the client proof.

The group is the 1024 bit prime from `srp.cpp` with generator 2. The client
public key `A` travels in `CNCT_specific_data`, chunked into pieces of at
most 254 bytes each prefixed with a part number; the proof travels either
in `op_cont_auth` or as `isc_dpb_specific_auth_data` in the DPB, depending
on which accept the server sent.

`Legacy_Auth` is not implemented. It offers no session key, so it cannot
satisfy a server that requires wire encryption, and modern servers reject
it unless explicitly enabled.

---

## Wire encryption

After authentication the client holds a session key. The server advertises,
per key type, the wire encryption plugins it supports, and from protocol 16
also each plugin's initialisation vector. The client picks the first of
`ChaCha64`, `ChaCha`, `Arc4` that the server offers and sends `op_crypt`.

The changeover is asymmetric and easy to get wrong:

* `op_crypt` itself is sent in clear;
* the server encrypts everything it sends **from the moment it receives
  `op_crypt`**, so the receive cipher is installed before the response is
  read;
* the client encrypts only after that response has been validated.

`Arc4` uses the session key directly. The ChaCha plugins stretch it with
SHA-256 to 32 bytes and use the server supplied IV: 8 bytes of nonce for
`ChaCha64`, 12 bytes of nonce plus a 32 bit big endian counter for
`ChaCha`. Both directions use the same key and the same IV, which is what
the engine does.

Protocol 14 and 15 settle on `Arc4` because the IV needed by the ChaCha
plugins is only advertised from protocol 16.

---

## Messages, BLR and data types

A prepared statement's parameters and columns are described by
`isc_info_sql_*` clumplets, parsed by `FBWireDescribe` into a
`TWireMessageFormat`. For text types the reported subtype is the character
set id and the reported length is already the byte length in that
character set.

`FBWireMessage` then computes a flat buffer layout with the same storage
conventions the client library uses: `SQL_VARYING` is a two byte length
followed by the characters, integers are native endian and naturally
aligned, and the null indicators are four byte words placed after the data.
`TSQLDataItem` points straight into that buffer, so the whole fbintf
conversion layer works unchanged.

On the wire, from protocol 13, a message is a null bitmap (one bit per
field, padded to four bytes) followed by the values of the fields that are
not null.

Verified against the server for every type, with the exact byte patterns
checked by hand:

| Type | Wire form |
|---|---|
| `SMALLINT`, `INTEGER`, `BIGINT` | 4 or 8 byte two's complement, scale applied by the caller |
| `FLOAT`, `DOUBLE PRECISION` | IEEE 754 bit pattern, high word first |
| `DATE` | days from 17 November 1858 |
| `TIME` | decimilliseconds since midnight |
| `TIMESTAMP` | the two above, in that order |
| `CHAR` | blank padded to the full field width |
| `VARCHAR` | counted string |
| `BOOLEAN` | one value byte, padded to four |
| `INT128` | two 64 bit words, high first |
| `DECFLOAT(16)`, `DECFLOAT(34)` | IEEE 754 decimal, word swapped |
| `TIME`/`TIMESTAMP WITH TIME ZONE` | the plain value followed by the zone id |
| `BLOB`, `ARRAY` | an `ISC_QUAD` identifier |

---

## Error reporting

The server sends its status vector with each error, and `FBWireProtocol`
decodes it into `isc_arg_*` items. `FBWireClientAPI` rebuilds a standard
ISC status vector from that, so `EIBInterBaseError`, `GetIBErrorCode` and
`CheckStatusVector` all behave as they do with the other providers.

Message **text** is the one visible difference. `firebird.msg` is a client
library resource, so it is not available here. Most engine errors carry
interpreted text in the status vector and that text is used; the rest are
reported as `Firebird Error Code: n`. Where the stock provider says
*"unsuccessful metadata update"*, this one says
`Firebird Error Code: 335544351`. The numeric code, the SQLCODE path and
any strings the server supplies are all still there.

---

## Testing

`testsuite/WireTest.pas` is self contained and needs no fbclient:

```bash
fpc -Fuclient -Fuclient/2.5 -Fuclient/3.0 -Fuclient/3.0/firebird \
    -Fuclient/wire -Ficlient/include -FEbuild testsuite/WireTest.pas

./build/WireTest [<database> [<user> [<password> [<scratch database>]]]]
```

Defaults are `localhost:employee SYSDBA masterkey`. Naming a scratch
database, which must not already exist, adds create and drop database
coverage:

```bash
./build/WireTest localhost:employee SYSDBA masterkey localhost:/tmp/scratch.fdb
```

It runs in four layers, and the offline ones run even with no server:

1. **Arithmetic** — the big integer operations, including a 1024 bit
   modular exponentiation of the size SRP performs, against values computed
   independently.
2. **Cryptography** — SHA-1, SHA-256, RC4 and ChaCha20 against the
   published FIPS and RFC 8439 vectors.
3. **SRP** — a complete exchange against a fixed reference exchange:
   client public key, session key and proof for both `Srp` and `Srp256`,
   including the upper casing of the account name.
4. **Message layout** — alignment, buffer size, the generated BLR and the
   quad conversions.

Then, if a server answers:

5. **Live connection and negotiation** — the handshake, and the version
   actually negotiated when the offer is capped at 14, 15, 16 and 17.
6. **Provider** — queries and parameters, every data type, insert, update,
   null handling, accented text, blob round trips, transaction rollback,
   and create and drop database.

With no server reachable the live sections report `SKIP` and the process
still exits 0, which is what the offline CI job relies on.

### The full fbintf test suite

`WireTest` is the unit layer. The integration layer is the ordinary
fbintf test suite - all twenty two programs - run over this provider:

```bash
testsuite/runtest.sh -a wire
```

The `-a wire` (`--api wire`) switch makes `TTestApplication` obtain the
API from `WireFirebirdAPI` instead of `IB.FirebirdAPI`; nothing else in
the suite changes. The output is compared against
`testsuite/FBWirereference.log` after normalising the run dependent
values (transaction ids, page counters, journal timestamps) on both
sides of the diff. Tests for events, arrays and other unimplemented
features skip with a fixed message so the comparison stays exact.

The reference log is the CI environment's own output: Firebird 6
(ODS 14, protocol 17) in a container, the employee example database
restored from `testsuite/employee.gbk`, an x86_64 runner. Float to text
rendering differs in the last digit between CPU architectures, so a log
produced elsewhere (for example on ARM) shows a handful of known
differences. To regenerate the reference after an intended output
change, download the `wire-suite-testout` artifact from the CI run,
apply `runtest.sh`'s normalisation, and commit it.

### Measured results

| Server | WireCrypt | Negotiated | Encryption | Result |
|---|---|---|---|---|
| 6.0 (CI container) | Enabled, Required | 18 | `ChaCha64` | 137 tests, 0 failures |
| 6.0.0 (local, LI-T6.0.0.2076) | Required | 18 | `ChaCha64` | 137 tests, 0 failures |
| 5.0 (CI container) | Enabled, Required | 18 | `ChaCha64` | 137 tests, 0 failures |
| 5.0.4 (local container) | Enabled, Required | 18 | `ChaCha64` | 137 tests, 0 failures |
| 4.0 (CI container) | Enabled, Required | 17 | `ChaCha64` | 137 tests, 0 failures |
| 3.0 (CI container) | Enabled | 15 | `Arc4` | 137 tests, 0 failures |
| no server | — | — | — | 36 tests, live sections skipped |

Firebird 3 settles on protocol 15 with Arc4: it is the newest protocol that
server knows, and it predates the ChaCha plugins. Everything else in the
suite behaves identically there.

The container rows come from the CI matrix; the rest were run locally.

### Dropping a table you have just read

An attachment that has read a table keeps an interest in it that outlives
the transaction, and Firebird 5 then refuses `drop table` on that same
attachment with `isc_no_meta_update`. This is a property of the server, not
of this client: the stock fbclient provider fails in exactly the same place
with exactly the same code, and a second attachment issuing the drop blocks
instead of succeeding. `WireTest` therefore treats dropping its test table
as best effort and drops it at the start of the next run.

---

## Continuous integration

`.github/workflows/wire-protocol.yml` runs on every push and pull request
that touches `client/**` or the test.

The **server** job runs a matrix of Firebird 3, 4, 5 and 6 containers
against `WireCrypt = Enabled` and `WireCrypt = Required`, seven
combinations in all. Firebird 3 with `Required` is excluded: protocol 13 is
the only one it offers without encryption support, so the combination
cannot succeed by definition. Each job starts the container, creates a test
database with `isql` inside it, builds the package and the test with the
distribution's FPC, and runs the test over TCP. Nothing Firebird related is
installed on the runner: the client only needs a socket.

The **offline** job builds and runs the same binary on Linux and Windows
with no server at all, so a regression in the arithmetic, the hashes or the
message layout cannot be masked by a server problem, and the code keeps
compiling on Windows.

Each job writes the negotiated protocol and the test totals to the step
summary, so the matrix view shows at a glance which protocol each server
version settled on.

---

## Roadmap

The provider is complete enough for ordinary work: connect, transactions,
DSQL, every data type, blobs. What follows is what is left, in the order it
is worth doing, with what each piece actually needs. Nothing here is
speculative; each item names the operations involved and the fbintf
machinery that already exists to support it.

| # | Milestone | Needs | Protocol |
|---|---|---|---|
| 1 | Run the existing test suite against this provider — **done** | `testsuite -a wire` runs all twenty two programs | — |
| 2 | Events — **done** | `FBWireEvents` implements `IEvents` over the auxiliary connection | 13 |
| 3 | Services | the `IServiceManager` wrapper over exchanges that already exist | 13 |
| 4 | A Delphi transport | a `TFBWireTransport` over Winsock and Posix sockets | — |
| 5 | Array columns — **done** | `FBWireArray` over `op_get_slice`/`op_put_slice`, shared SDL generator | 13 |
| 6 | Statement timeouts and cancellation — **done** | `CancelOperation`/`SetStatementTimeout` on all providers | 12, 16 |
| 7 | Scrollable cursors — **done** | `op_fetch_scroll`, protocol raised to 18 | 18 |
| 8 | The batch API | the `op_batch_*` family | 16 |
| 9 | Inline blobs | `op_inline_blob` | 19 |
| 10 | Firebird 6 protocol 20 | schema search path, named arguments | 20 |
| 11 | Wire compression | zlib either side of the cipher | 13 |
| 12 | Engine message text | a `firebird.msg` reader or a generated table | — |

### 1. Run the existing test suite against this provider — done

`testsuite -a wire` (or `runtest.sh -a wire`) runs all twenty two test
programs over this provider, and the output is compared line by line
against `testsuite/FBWirereference.log` with the run dependent values
(transaction ids, page counts, journal timestamps) normalised on both
sides. The CI workflow runs the suite against a Firebird 6 container and
fails on any difference from the reference log. Tests for features the
provider does not implement skip with a fixed message, guarded by the new
`IAttachment.HasArraySupport` and `HasEventSupport` capability checks
alongside the existing ones; those skip lines shrinking is how milestones
2 and 5 showed up in the log.

The prediction that this was the cheapest large win was right for the
wrong reasons: the suite immediately found seven real defects that
`WireTest` could not see, several in code paths that had simply never
been executed. The fixes that came out of the first run:

* **SRP broke inside the suite binary only** — the account name upper
  casing used `AnsiUpperCase`, and with `fpwidestring` installed (which
  the suite loads) that returns the string with a trailing `#0` included
  in its length, poisoning the proof hashes. ASCII `UpperCase` is both
  safe and what the engine does.
* **`op_execute2` had its statement and transaction handles swapped** at
  the call site, killing the connection on any `execute procedure` or
  `insert ... returning` — the exchange had never been exercised.
* **`Execute` returned nil for `update/insert ... returning`**: Firebird
  5 and later describe those as select statements (cursor + one row),
  older servers as `SQLExecProcedure` (`op_execute2` singleton); both
  paths are now implemented.
* **Named parameters lost their names**: the bind after prepare
  overwrote the preprocessor's `:name` assignments with the describe
  response's empty names.
* **Parameter metadata was immutable**: assigning a value of a different
  type to a parameter (`AsInteger` on a `SMALLINT`, a string to a blob)
  now changes the message format and relays out the buffer, as the other
  providers allow — the client owns the BLR it sends.
* **`DECFLOAT` values decoded to garbage**: the provider inherited the
  base class codec, which has no implementation. It now carries its own
  IEEE 754 densely packed decimal encoder and decoder, verified against
  the server in both directions.
* **Blob metadata from table and column names** (`GetBlobMetaData`) never
  looked the column up, so blob subtypes were wrong; it now runs the same
  system table query as the 3.0 provider.

The suite also drove smaller fixes: create database from a SQL statement
now extracts the file spec locally (the stock providers delegate that
preparse to `fbclient`), a nil DPB no longer crashes, the DPB sent with
the attach is now a verbatim clumplet copy rather than a re-encoded one,
and the transport discards its session ciphers on disconnect so that the
same connection object can reconnect (also found independently by the
services milestone).

### 2. Events — done

Implemented by `client/wire/FBWireEvents.pas`. One auxiliary connection
and one listener thread per attachment, created on the first
`GetEventHandler` call, serve all its `IEvents` instances; interest is
registered with `op_que_events` on the main connection and notifications
are dispatched by event id. `TFBEvents` supplied the event block, the
count diffing and the callback dispatch exactly as anticipated; the wire
side is the three exchanges plus the second transport.

Findings from the implementation, beyond the NAT point the plan already
flagged (only the port of the returned address is usable - the client
reuses the host it connected to):

* **Each `op_que_events` must carry a fresh event id.** An interest is
  one shot, and re-arming under the same id is accepted by the server
  but not honoured immediately: counts accumulated while nobody waited
  were only delivered when the *next* event fired, one delivery late.
  The stock client increments its id on every queue, and doing the same
  made deferred delivery immediate.
* The auxiliary connection carries no handshake, no authentication and
  no encryption - the server associates it with the session by the
  accept, and it only ever delivers `op_event` (and `op_dummy`) packets.
* **The auxiliary port must be reachable.** By default the server opens
  a random port for it, which a container that only publishes 3050, or a
  firewall, silently blocks - the CI containers demonstrated this by
  hanging in the connect. Set `RemoteAuxPort` in `firebird.conf` to pin
  it and publish or allow that port; the CI workflow pins it to 3051.
  This applies to any Firebird client, not just this one.
* The event handler is called from the listener thread, exactly as the
  2.5 provider calls its handler from an AST thread, so a handler must
  not call back into the same attachment from that thread; `Synchronize`
  or `Queue` the work first. The test suite's Test 10 shows the pattern.
* One deliberate difference from the stock providers: events posted
  while interest was cancelled are included in the counts of the next
  wait (the stock bookkeeping can drop them). The wire reference log
  records this in Test 10's final count.

### 3. Services

`op_service_attach`, `op_service_detach`, `op_service_start` and
`op_service_info` are already implemented in `FBWireProtocol` and unused.
What is missing is the `IServiceManager` layer: a `TFBWireServiceManager`
over `TFBServiceManager`, which needs `InternalAttach`, `Detach` and
`Query`, plus `AllocateSPB` returning the existing `TSPB`. `FBOutputBlock`
already parses the responses. This is a small, well bounded piece of work
that would bring backup, restore, statistics and user management to a
client with no library installed — arguably the most useful thing this
provider could offer an operations team.

### 4. A Delphi transport

Everything above `FBWireStream` is compiler neutral and already compiles
under `{$mode delphi}`. Only the transport binds FPC's `sockets` and
`ssockets`. A Delphi implementation needs `TFBWireTransport` reimplemented
over `Winapi.WinSock2` and `Posix.SysSocket` behind the same four methods
(`ConnectTo`, `Disconnect`, `ReadBytes`, `WriteBytes`), after which the
units can be added to `fbintf.dpk`. The cipher and XDR layers do not change.

### 5. Array columns — done

Implemented by `client/wire/FBWireArray.pas`. `TFBWireArray` subclasses
`FBArray`'s element addressing and conversion layer and implements the two
provider methods over `TFBWireConnection.GetSlice`/`PutSlice`
(`op_get_slice`/`op_put_slice`); `TFBWireArrayMetaData` fills the array
descriptor with the same system table query the 3.0 provider uses, run
over the wire like any other statement. The SDL generator moved from
`FB30Array` into the shared, compiler neutral `FBSDL` unit, so both
providers emit identical SDL.

The slice data on the wire is XDR, element by element, following
`xdr_slice`/`xdr_datum` driven by the SDL element descriptor
(`FBWireMessage.XDREncodeSlice`/`XDRDecodeSlice`). Two things the sources
reveal that the isc API hides: the slice length fields count in the
*descriptor's* element length units (`sdl_desc` in `src/common/sdl.cpp`),
which for a `CHAR(n)` element is `n` while fbintf's client buffer spaces
elements at `n+1`; and `blr_varying` maps to a **`dtype_cstring`**
element - a count followed by the bytes - which is exactly the zero
terminated layout `FBArray` keeps in its buffer, so the "curious" varchar
array format the IBPP comment in `FBArray.pas` describes is simply the
SDL's view of the column.

Finding the arrays also flushed out a provider wide leak: the wire
statement's `TWireSQLDataArea` never freed its column variables (the 3.0
provider frees them in `FreeXSQLDA`), which pinned every blob, array and
SDL block a statement had touched - and, through the blobs' transaction
references, kept transactions alive so that their `taCommit` default
completion never ran. Test 6's execute procedure result had recorded the
symptom in the reference log as a NULL blob. Fixed with a destructor and
a `SetCount` that frees on shrink; the suite output now matches the
fbclient providers on that line.

### 6. Statement timeouts and cancellation — done

Both halves were interface additions to fbintf, not just wire changes:
`IAttachment.CancelOperation(aKind)` and
`IStatement.SetStatementTimeout`/`GetStatementTimeout` are new, and all
three providers implement them - 2.5 through `fb_cancel_operation`
(when the loaded library exports it), 3.0 through
`IAttachment::cancelOperation` and `IStatement::setTimeout` (timeouts
need a Firebird 4 client), and the wire provider natively.

On the wire, `op_cancel` has no response packet and is sent from a
different thread while the owner is blocked reading: it bypasses the
shared send buffer through `TFBWireTransport.SendDirect`, whose lock
serialises the cipher and socket write against `Flush` - the stream
cipher stays consistent because bytes are enciphered in wire order. The
timeout travels in the `p_sqldata_timeout` field of
`op_execute`/`op_execute2` (protocol 16); below protocol 16 a non zero
timeout raises `ibxeNotSupported` rather than being silently dropped.

One correction to the plan: an expired timeout does not arrive as its
own error code. The server cancels the request, so the primary status
is `isc_cancelled` with `isc_req_stmt_timeout` as the secondary code
(`thread_db::checkCancelState`). `op_cancel` also does not unblock a
client whose server has gone away - that is the socket timeout's job
(`ConnectTo` accepts one).

### 7. Scrollable cursors — done

The protocol offer now goes up to 18, which Firebird 5 and 6 accept
(Firebird 4 stays on 17, Firebird 3 on 15). At 18 every
`op_execute`/`op_execute2` carries a cursor flags word after the timeout
field; a cursor opened with `IStatement.OpenCursor(true)` sets
`CURSOR_TYPE_SCROLLABLE` in it, and the five positioned fetches of
`IResultSet` then travel as `op_fetch_scroll` - `op_fetch` plus a
direction and a position, answered by the same `op_fetch_response`
sequence.

A positioned fetch requests a single row and first discards the client's
read ahead cache: those rows describe a cursor position the scroll
abandons (the server, symmetrically, discards its own prefetch and
repositions when the fetch direction changes - `rem_port::fetch` in
`src/remote/server/server.cpp`). Sequential fetches keep their batched
read ahead, and one that follows a scroll simply starts a fresh batch
from the new position. BOF/EOF bookkeeping follows
`TFB30Statement.Fetch`: success clears both, `FetchPrior` off the top
sets BOF, and a failed positioned fetch leaves the flags alone.
`HasScollableCursors` answers protocol >= 18, so the suite's Test 2
scrollable section runs against Firebird 5 and 6 and skips on older
servers.

### 8. The batch API

`op_batch_create`, `op_batch_msg`, `op_batch_exec`, `op_batch_rls` and
`op_batch_cs` (protocol 16) support `IBatch`. Messages are packed into a
stream, each padded to an eight byte boundary, and the completion state
comes back as update counts plus per row status vectors. `TFBStatement`
already has the batch entry points defaulting to unsupported, and
`TBatchCompletion` in the 3.0 provider shows the shape of the result.

### 9. Inline blobs

Protocol 19 lets the server push small blobs with the row that references
them, in `op_inline_blob` packets, saving a round trip each. The client
declares the size it will accept in `op_execute`. `ReadOperation` already
has to skip unsolicited packets, so this fits naturally: cache the blob
against its identifier and have `TFBWireBlob` check the cache before
opening.

### 10. Firebird 6 and protocol 20

Protocol 20 adds SQL schemas and named arguments. It needs a schema search
path in the DPB (`isc_dpb_search_path`), a different describe item list, and
an extra flags field in `op_prepare_statement`. The client currently offers
up to 17 and Firebird 6 negotiates down happily, so this is about gaining
the new features rather than about compatibility.

### 11. Wire compression

`pflag_compress` is understood in the accept but never requested. Turning it
on means running zlib over the byte stream underneath the cipher, which the
transport is already structured for: compression would be another filter in
the same position as `TWireCipher`, applied in the opposite order.

### 12. Engine message text

The one visible difference from the stock providers. Two ways to close it:
read `firebird.msg` when a copy happens to be available, or generate a
Pascal table of the common `isc_*` codes and their format strings from
`msgs.sql` at build time. The second keeps the no dependencies property and
is a few hundred kilobytes of generated source.

### Not planned

* **`Legacy_Auth`.** It produces no session key, so it cannot satisfy a
  server requiring encryption, and modern servers disable it by default.
  The DES `crypt(3)` implementation it needs is not worth carrying.
* **`Win_SSPI` / trusted authentication.** Windows specific and awkward to
  test in CI.
* **Protocols below 13.** Firebird 2.5 and earlier need the pre
  authentication plugin handshake and a different message encoding, and are
  out of support.

