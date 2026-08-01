# Pure Pascal Firebird wire protocol client

This directory implements a Firebird client that speaks the remote (wire)
protocol directly over TCP. It needs no `fbclient` library: there is nothing
to install on the client machine beyond the compiled program itself.

It supports Firebird 3.0, 4.0, 5.0 and 6.0 servers by negotiating protocol
versions 13 to 17, with SRP authentication and optional wire encryption.

## Status

Working and tested against a live server (see *Testing* below):

* connection handshake with protocol negotiation (13, 14, 15, 16, 17)
* `Srp256` and `Srp` authentication, both the `op_cond_accept` flow (where
  authentication must complete before attaching) and the `op_accept_data`
  flow (where the proof travels in the DPB)
* wire encryption with `ChaCha64`, `ChaCha` and `Arc4`
* attach, create and drop database, detach
* transactions: start, commit, rollback, the retaining variants and the
  two phase commit prepare
* DSQL: allocate, prepare with full describe, execute, execute with a
  singleton result (`op_execute2`), fetch, close and drop, set cursor name
* all Firebird data types including `INT128`, `DECFLOAT(16)`, `DECFLOAT(34)`,
  `BOOLEAN` and the time zone types
* blobs: create, open, read and write segments, close, cancel
* information calls for the database, transaction, statement and blob
* events: `IEvents` with asynchronous and synchronous waits, delivered on
  the `op_connect_request` auxiliary connection by a listener thread
* `DECFLOAT(16)`/`DECFLOAT(34)` conversions through the provider's own
  IEEE 754 densely packed decimal codec
* `execute procedure` and `insert/update ... returning` singleton results
* named parameters, parameter type coercion, create database from a SQL
  statement, reconnection of a disconnected attachment

The whole fbintf test suite (twenty two test programs) runs over this
provider with `testsuite/runtest.sh -a wire` and is compared against
`testsuite/FBWirereference.log`; CI runs it against Firebird 6 on every
change.

Not implemented yet, and reported as `ibxeNotSupported` rather than
failing obscurely (see the roadmap in doc/WireProtocol.md for what each
would take):

* array columns (`op_get_slice` / `op_put_slice` with SDL descriptions)
* the batch API of protocol 16
* scrollable cursors (`op_fetch_scroll`, protocol 18)
* transactions spanning several attachments (needs a two phase commit
  coordinator)

## Verified servers

Every row below was measured with `testsuite/WireTest.pas`, in CI for the
container rows and locally for the others.

| Server | WireCrypt | Negotiated | Encryption | Result |
|---|---|---|---|---|
| 6.0 (CI container) | Enabled, Required | 20 | `ChaCha64` | 178 tests, 0 failures |
| 6.0.0 (local, LI-T6.0.0.2076) | Required | 20 | `ChaCha64` | 178 tests, 0 failures |
| 5.0 (CI container) | Enabled, Required | 19 | `ChaCha64` | 178 tests, 0 failures |
| 5.0.4 (local container) | Enabled, Required | 19 | `ChaCha64` | 178 tests, 0 failures |
| 4.0 (CI container) | Enabled, Required | 17 | `ChaCha64` | 178 tests, 0 failures |
| 3.0 (CI container) | Enabled | 15 | `Arc4` | 178 tests, 0 failures |
| no server | — | — | — | 36 tests, live sections skipped |

Firebird 3 settles on protocol 15 with Arc4: it is the newest protocol that
server knows, and it predates the ChaCha plugins. Everything else in the
suite behaves identically there.

Capping `TFBWireConnection.MaxProtocol` in turn, against both servers:

| Offered up to | Negotiated | Encryption |
|---|---|---|
| 13 | refused with `isc_miss_wirecrypt` when the server requires encryption | — |
| 14 | 14 | `Arc4` |
| 15 | 15 | `Arc4` |
| 16 | 16 | `ChaCha64` |
| 17 | 17 | `ChaCha64` |

Protocol 13 has no wire encryption, so a server configured with
`WireCrypt = Required` (the default from Firebird 4 on) refuses it. It is
still offered because a Firebird 3 server, or one configured with
`WireCrypt = Enabled`, accepts it. `ChaCha` and `ChaCha64` need the
initialisation vector the server only sends from protocol 16, which is why
14 and 15 fall back to `Arc4`.

## A note on dropping a table you have read

An attachment that has read a table keeps an interest in it that outlives
the transaction, and Firebird 5 then refuses `drop table` on that same
attachment with `isc_no_meta_update`. This is not a property of this
client: the stock fbclient provider fails in exactly the same place with
exactly the same code, and a second attachment issuing the drop blocks
rather than succeeding. `WireTest` therefore treats dropping its test
table as best effort and drops it at the start of the next run instead.

## Layout

| Unit | Contents |
|---|---|
| `FBWireBigInt` | arbitrary precision unsigned arithmetic for SRP |
| `FBWireCrypto` | SHA-1, SHA-256, RC4 and ChaCha20 |
| `FBWireSRP` | the client half of SRP-6a as the engine implements it |
| `FBWireStream` | buffered TCP transport, per direction ciphers, XDR codec |
| `FBWireConst` | operation codes and protocol constants from `protocol.h` |
| `FBWireMessage` | message buffer layout, BLR descriptions, row encoding |
| `FBWireDescribe` | `isc_info_sql` describe response parser |
| `FBWireProtocol` | the connection: handshake and all packet exchanges |
| `FBWireClientAPI` | `IFirebirdAPI` for the provider |
| `FBWireAttachment`, `FBWireTransaction`, `FBWireStatement`, `FBWireBlob` | the rest of the fbintf provider |

`FBWireProtocol` is usable on its own if you want to speak the protocol
without the fbintf object model; the remaining units adapt it to
`IFirebirdAPI`.

## Use

```pascal
uses IB, FBWireClientAPI;

var API: IFirebirdAPI;
    DPB: IDPB;
    Attachment: IAttachment;
begin
  API := WireFirebirdAPI;          {never loads a client library}
  DPB := API.AllocateDPB;
  DPB.Add(isc_dpb_user_name).AsString := 'SYSDBA';
  DPB.Add(isc_dpb_password).AsString := 'masterkey';
  DPB.Add(isc_dpb_lc_ctype).AsString := 'UTF8';
  Attachment := API.OpenDatabase('localhost:employee',DPB);
  ...
```

Everything after that is the ordinary fbintf API, so code written against
`IAttachment`, `ITransaction`, `IStatement` and `IResultSet` works
unchanged. The password is used for the SRP exchange and is removed from
the DPB before the attach: it never travels over the network.

Wire encryption is negotiated automatically and is on whenever the server
offers it. A server configured with `WireCrypt = Required` (the Firebird 4
and later default) works out of the box; there is no way to reach such a
server with an unencrypted client.

## Notes on the protocol

These points are all places where a naive reading of the packet layouts
produces a client that does not work, so they are worth repeating here.

* The key type sent in `op_crypt` is the type the server advertised in its
  key clumplets, normally `Symmetric`. It is *not* the authentication
  plugin name, although some documentation says so.
* Text columns must be described with `blr_text2` / `blr_varying2` naming
  the column's character set. With plain `blr_text` / `blr_varying` the
  engine assumes the connection character set and divides the declared
  byte length by the maximum character size, so a `VARCHAR(37)` fetched
  over a UTF8 connection silently becomes 9 characters and the fetch fails
  with a string truncation error.
* XDR transmits a boolean as a single value byte padded to four bytes. It
  is not a big endian integer, and sending it as one makes every boolean
  read as false.
* A batched `op_fetch` must be drained completely before anything else is
  sent on the connection: the server streams one `op_fetch_response` per
  row and terminates the batch with a message count of zero. Rows are
  therefore cached as they arrive.
* `ISC_QUAD` values (blob and array ids) put the high word first, which is
  not the memory layout of an `Int64` on a little endian machine. Use
  `WireQuadToInt64` and `Int64ToWireQuad`.
* SRP as implemented by the engine deviates from the specification in
  several ways: the exponent is reduced modulo N, `H(N) xor H(g)` is a
  modular exponentiation rather than an exclusive or, the salt is hashed
  as its hexadecimal text, and the session key is always SHA-1 even for
  `Srp256`. `FBWireSRP` documents each of these where it implements them.

Error message text comes from the status vector strings that the server
sends, because `firebird.msg` is a client library resource. Most engine
errors carry their text; those that do not are reported as
`Firebird Error Code: n` with the numeric code.

## Testing

`testsuite/WireTest.pas` is a self contained regression test. It needs a
Firebird server and the `employee` example database:

```
cd testsuite
fpc -Fu../client/wire -Fu../client -Fu../client/2.5 -Fu../client/3.0 \
    -Fu../client/3.0/firebird -Fi../client/include WireTest.pas
./WireTest localhost:employee SYSDBA masterkey
```

It exercises the cryptographic primitives against their published test
vectors, then runs the protocol against the server: connect, attach, DDL,
parameterised DML, every data type, blob round trips, transaction control
and the `IFirebirdAPI` provider layer.
