# Design: Events over the wire protocol

Roadmap milestone 2 (`doc/WireProtocol.md`). This document is the
implementation plan; no code changes are included.

## Goal

`IAttachment.GetEventHandler` works on the wire provider exactly as it does
on the other two: register interest in named events, receive counts
asynchronously through `TEventHandler`, or block in `WaitForEvent`.

Today `TFBWireAttachment.GetEventHandler` (`FBWireAttachment.pas`) raises
`ibxeNotSupported` with the comment that events need the auxiliary
connection established with `op_connect_request`.

## What already exists

Nearly all of the event machinery in fbintf is provider neutral and lives in
`client/FBEvents.pas`:

* `TFBEvents.CreateEventBlock` builds the event parameter block in exactly
  the form `op_que_events` carries: version byte `EPB_version1`, then per
  event a length byte, the name, and a little endian count initialised to 1.
  `FResultBuffer` has the same layout and length.
* `TFBEvents.ProcessEventCounts` diffs `FResultBuffer` against
  `FEventBuffer`, fills `TEventCounts`, and copies the result buffer over
  the event buffer so it becomes the new baseline.
* `TFBEvents.EventSignaled` performs the callback dispatch under
  `FCriticalSection`, honouring `FInWaitState`, and calls the handler
  outside the lock.

A subclass supplies exactly four overrides — `GetIEvents`, `CancelEvents`,
`WaitForEvent`, `AsyncWaitForEvent` — plus a way of filling `FResultBuffer`
and calling `EventSignaled` when a notification arrives. `TFB25Events` and
`TFB30Events` are the two existing models; the 2.5 one is the closer match
because it also deals in raw EPB buffers rather than the OO API's callback
object.

On the wire side, `FBWireConst.pas` already defines `op_connect_request`,
`op_aux_connect`, `op_que_events`, `op_cancel_events`, `op_event` and
`P_REQ_async`. None of them is used yet. `TFBWireConnection` exposes its
`XDR: TXDRStream` and `Transport: TFBWireTransport` publicly, and
`TXDRStream` already has the string/opaque codecs the packets need.

## The protocol

Three exchanges on the **main** connection, one packet type on a **second**
connection.

1. **`op_connect_request`** (main connection): int32 type = `P_REQ_async`,
   the database object id, int32 partner id (0). The `op_response` carries,
   in its data bytes, a `sockaddr_in` naming the port the server is
   listening on for the auxiliary connection. Only the port is trustworthy:
   behind NAT the address is the server's own view of itself, so the client
   reuses the host it originally connected to and takes just the port. This
   is what the stock remote client does.

2. **Auxiliary connection**: a plain TCP connect to that host:port. No
   handshake, no authentication, no encryption — the server associates it
   with the session by the accept. The socket then only ever *receives*.

3. **`op_que_events`** (main connection): database handle, the EPB as a
   string (byte-counted), an AST address, an argument (both legacy fields,
   sent as 0), and an event id chosen by the client. The response's
   `ObjectHandle` is the server side id used for cancellation.

4. **`op_event`** (auxiliary connection): database handle, the updated
   event buffer as a string, the 8 legacy AST bytes, and the event id. The
   buffer has the same layout as the EPB, with new counts.

5. **`op_cancel_events`** (main connection): database handle, event id.

## Design

New unit `client/wire/FBWireEvents.pas`:

* `TFBWireEvents = class(TFBEvents, IEvents)` — the four overrides.
  `AsyncWaitForEvent` queues via `op_que_events`; `WaitForEvent` does the
  same and then blocks on an OS event signalled by `EventSignaled`, the
  same shape as `TFB25Events.WaitForEvent`.
* `TFBWireEventListener = class(TThread)` — owns the auxiliary socket. Its
  loop decodes `op_event` packets with a private `TXDRStream` over a second
  `TFBWireTransport`, copies the event buffer into the owning
  `TFBWireEvents.FResultBuffer` under `FCriticalSection`, and calls
  `EventSignaled`. Socket close is the shutdown signal.

`TFBWireConnection` gains:

* `ConnectRequest(aDbHandle: integer): TAuxAddress` — sends
  `op_connect_request`, parses the `sockaddr` out of the response data,
  substitutes the original host.
* `QueEvents(aDbHandle: integer; const EPB: TBytes; aEventID: integer): integer`
  and `CancelEvents(aDbHandle, aEventID: integer)`.

The auxiliary connection is created lazily on the first
`GetEventHandler` call and shared by all `IEvents` instances of the
attachment; one listener thread serves them all, dispatching on event id.
It is torn down in `TFBWireAttachment.InternalDisconnect`.

### Concurrency

The listener thread never touches the main connection, so no locking is
added to the request path. The only shared state is `FResultBuffer` and the
wait flags, which `TFBEvents` already guards with `FCriticalSection`.
`op_que_events` after a notification (re-arming) happens on the caller's
thread, from `EventSignaled`'s handler, as the other providers do.

### Failure modes

* Server refuses `op_connect_request` (aux port disabled): surface the
  status vector as usual; `GetEventHandler` fails cleanly.
* Aux socket drops: the listener marks the events object dead; the next
  `WaitForEvent`/`AsyncWaitForEvent` raises through the normal status path.
* `SetEvents` while queued: `TFBEvents.SetEvents` already cancels first.

## Acceptance

* `Test16` (events) in the test suite passes against the wire provider
  (milestone 1 provides the provider switch).
* A new `WireTest` section: queue two events, post from a second
  attachment via `execute block ... post_event`, assert both counts; then
  cancel and assert no further delivery.
* CI matrix: events must work on Firebird 3, 4, 5, 6 — the packets are
  protocol 13 level and unchanged since.

## Out of scope

Encrypted auxiliary connections (the server sends `op_event` in clear even
when the main line is encrypted — as does the stock client), and
`isc_wait_for_event`-style synchronous multiplexing beyond what
`TFBEvents` already offers.
