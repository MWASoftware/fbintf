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
11. [Limitations and future work](#limitations-and-future-work)

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

* the connection handshake with protocol negotiation, versions 13 to 17
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

Deliberately not implemented yet. These raise `ibxeNotSupported` rather
than failing in a confusing way:

| Feature | What it needs |
|---|---|
| Events | the auxiliary connection from `op_connect_request` plus a listener thread |
| Services | `op_service_*` exist in `FBWireProtocol`; the `IServiceManager` wrapper does not |
| Array columns | `op_get_slice` / `op_put_slice` and SDL descriptions |
| Batches | the protocol 16 `op_batch_*` family |
| Scrollable cursors | `op_fetch_scroll`, protocol 18 |
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

### Measured results

| Server | WireCrypt | Negotiated | Encryption | Result |
|---|---|---|---|---|
| 6.0.0 (LI-T6.0.0.2076) | Required | 17 | `ChaCha64` | 81 tests, 0 failures |
| 5.0.4 (container) | Enabled | 17 | `ChaCha64` | 81 tests, 0 failures |
| 5.0.4 (container) | Required | 17 | `ChaCha64` | 81 tests, 0 failures |
| no server | — | — | — | 36 tests, 0 failures, live sections skipped |

Firebird 3 and 4 are covered by CI: their container images are published
for amd64 only and could not run on the arm64 machine this was developed
on.

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

## Limitations and future work

In rough order of usefulness:

1. **Events.** The largest missing piece. Needs `op_connect_request`, a
   second socket and a listener thread. `FBEvents.CreateEventBlock` already
   builds the event block in exactly the form `op_que_events` carries.
2. **Services.** The packet exchanges are implemented in `FBWireProtocol`
   already; what is missing is the `IServiceManager` wrapper and the SPB
   plumbing.
3. **A Delphi transport.** Everything above `FBWireStream` is compiler
   neutral.
4. **Arrays**, **batches** and **scrollable cursors**, in that order.
5. **Compression.** `pflag_compress` is understood but never requested; it
   would need zlib on the client side.
6. **Message text.** A reader for `firebird.msg`, or a generated table of
   the common codes, would close the one visible gap against the stock
   providers.
