# Design: Statement timeouts and operation cancellation

Roadmap milestone 6 (`doc/WireProtocol.md`). This document is the
implementation plan; no code changes are included.

## Goal

Two related but separable features:

1. **`op_cancel`** (protocol 12, so available on every connection this
   client makes): abort an operation in flight from another thread, the
   wire form of `fb_cancel_operation`. The victim fails with
   `isc_cancelled` through the normal status path.
2. **Statement timeouts** (protocol 16): the `p_sqldata_timeout` field of
   `op_execute` / `op_execute2`, which `TFBWireConnection` already writes —
   currently hardcoded to zero in both `ExecuteStatement` and
   `ExecuteStatement2`.

## Current state

* `op_cancel = 91` and the four kinds (`fb_cancel_disable`,
  `fb_cancel_enable`, `fb_cancel_raise`, `fb_cancel_abort`) are defined in
  `FBWireConst.pas` and unused.
* The timeout field is written as a literal 0 at the two `op_execute`
  sites in `FBWireProtocol.pas`, gated on `FProtocolVersion >= 16`.
* There is no timeout surface anywhere in fbintf's `IStatement` — neither
  the wire provider nor the 2.5/3.0 providers expose one. So the timeout
  half of this milestone is an **interface addition**, not just a wire
  change.

## Design: timeouts

Interface: add to `IStatement` (and `TFBStatement` as virtual with a
stored field):

```pascal
procedure SetStatementTimeout(aMilliseconds: cardinal);
function GetStatementTimeout: cardinal;   {0 = no timeout}
```

* Wire provider: `TFBWireStatement` passes the value into
  `ExecuteStatement`/`ExecuteStatement2`, which gain a timeout parameter
  replacing the literal 0. On connections below protocol 16 a non-zero
  timeout raises `ibxeNotSupported` rather than being silently dropped.
* 3.0 provider: `IStatement` maps onto Firebird 4+'s
  `IStatement::setTimeout` when available (client major ≥ 4), else
  `ibxeNotSupported`. The 2.5 provider always raises. This keeps the
  interface honest across providers.

Server behaviour when the timeout fires: the statement fails with
`isc_sql_timeout` (Firebird 4+) arriving as an ordinary error response —
no new packet handling needed.

## Design: cancellation

Surface: `IAttachment.CancelOperation(Kind)` exists conceptually in the
ISC API as `fb_cancel_operation`; fbintf does not expose it today, so add
`procedure CancelOperation(aKind: integer = fb_cancel_raise)` to
`IAttachment`, implemented by 2.5 (via `fb_cancel_operation` when the
loaded library exports it), 3.0 (`IAttachment::cancelOperation`) and wire.

Wire mechanics — the two constraints that shape everything:

* `op_cancel` has **no response**. Nothing must be read after sending it;
  the cancelled operation's own error response is what comes back, on the
  main read path.
* It must be written **while another thread is blocked** in `ReadBytes`
  on the same socket. The send and receive sides of `TFBWireTransport`
  are already independent (separate buffers, separate ciphers), so a
  concurrent write is structurally sound. What is missing is a lock: two
  threads writing concurrently would interleave packets.

Plan:

1. Add a send-side critical section to `TFBWireTransport`, taken by
   `WriteBytes`/`Flush` callers at packet granularity: the normal request
   path already serialises (one request at a time per connection), so the
   only new contender is `SendCancel`.
2. `TFBWireConnection.SendCancel(aKind)` — takes the send lock, writes
   `op_cancel` + kind, flushes. Never touches the receive side.
3. The blocked reader then receives the pending operation's response,
   which carries `isc_cancelled` (`fb_cancel_raise`) — already handled by
   the existing status vector path.
4. `fb_cancel_abort` closes the connection server side; the reader's
   `EFBWireError('Connection lost...')` → `isc_network_error` path already
   copes.

One caveat to document: with the send **cipher** active, `Process` mutates
cipher state, so the send lock must also cover cipher application — it
already will, because encryption happens inside `Flush`.

### What op_cancel does not do

It does not unblock a client stuck in `ReadBytes` because the *server* is
gone — that is the socket timeout's job (`ConnectTo` already accepts one).
The doc should say so to head off misuse.

## Acceptance

* `WireTest` gains: start `select ... from big generator loop` (e.g.
  `rdb$types` cross joins) on a worker thread, cancel from the main
  thread, assert `isc_cancelled` arrives within a bound; execute a
  statement with a 1ms timeout against a deliberately slow query on
  protocol ≥ 16 and assert `isc_sql_timeout`.
* Timeout of 0 (default) produces byte-identical packets to today —
  verified by the existing suite passing unchanged.
* Threaded test guarded the same way `FBEvents.SetEvents` guards
  (`IsMultiThread`), and skipped in the offline CI job.
