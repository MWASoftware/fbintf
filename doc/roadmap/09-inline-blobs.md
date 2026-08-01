# Design: Inline blobs (protocol 19)

Roadmap milestone 9 (`doc/WireProtocol.md`). This document is the
implementation plan; no code changes are included.

## Goal

On protocol 19+ connections the server pushes small blobs alongside the
rows that reference them as `op_inline_blob` packets; the client caches
them and `IBlob` opens served from the cache, saving one round trip per
blob. This is transparent to callers — it is purely a latency
optimisation.

## Current state

* `op_inline_blob = 114` is defined in `FBWireConst.pas`, unused.
* `MaxSupportedProtocol` is 17; this milestone (after the scroll
  milestone takes it to 18) raises it to `PROTOCOL_VERSION19`.
* `ReadOperation` already skips unsolicited traffic (`op_dummy`,
  `op_response_piggyback`) in a loop — the natural place to intercept
  `op_inline_blob`, exactly as the roadmap anticipates.
* Blob opens go through `TFBWireBlob` over the connection's blob
  methods; blob ids travel as the quad/`Int64` handled by
  `WireQuadToInt64`.

## The protocol

* The client announces the maximum inline blob size it accepts in
  `op_execute` / `op_execute2`: protocol 19 appends two fields after
  `p_sqldata_timeout` — the cursor flags and the **inline blob size
  limit** (and Firebird's default is 64KB; 0 disables). Note the current
  code writes nothing after the timeout field, which is correct for ≤18
  and must grow the extra field(s) once 19 can be negotiated.
* `op_inline_blob` packets arrive interleaved with `op_fetch_response`
  packets inside a fetch batch (and after execute for singleton results).
  Each carries: transaction handle, the blob id (quad), the blob's total
  info (length/segmented flag) and the whole blob data as one opaque
  segment stream.
* An inline blob is a *copy* pushed at fetch time: it is only valid for
  that blob id under that transaction, and the server sends each id once.

## Design

1. **Cache.** A per-attachment `TWireInlineBlobCache`: map from
   (transaction handle, blob id) to the received bytes + info. Owned by
   `TFBWireAttachment`, cleared on transaction end (commit/rollback drop
   that transaction's entries) and capped: entries are evicted once
   consumed (a blob is normally opened at most once per fetch), and the
   whole cache is bounded by, say, 16MB to keep a scan over a
   blob-heavy table from ballooning memory — beyond the cap new arrivals
   are simply dropped (falling back to `op_open_blob2` costs a round
   trip, never correctness).
2. **Receive path.** `ReadOperation` gains a case: on `op_inline_blob`,
   decode the packet and hand it to a callback the attachment installs on
   the connection (`OnInlineBlob: procedure(...) of object`), then
   continue the loop. This keeps `FBWireProtocol` free of provider
   types, consistent with the unit layering.
3. **Open path.** `TFBWireBlob` (open-for-read constructor) consults the
   cache before sending `op_open_blob2`. On a hit it becomes a purely
   local object: `Read` serves from the buffer, `GetInfo` answers from
   the stored info, `Close` just releases. On a miss, unchanged
   behaviour.
4. **Announcing the limit.** `ExecuteStatement`/`ExecuteStatement2` write
   the P19 fields when `ProtocolVersion >= 19`; the limit is a property
   on the attachment (default the server default; 0 lets users opt out
   and also gives tests a control case). No `IAttachment` interface
   change needed — a wire-specific property, like `MaxProtocol`.

## Risks

The interleave handling is the only sharp edge: the fetch drain loop in
`FetchRow` treats any operation other than `op_fetch_response` /
`op_response` as fatal. Routing everything through `ReadOperation` (which
it already uses) makes the interception automatic, but the drain loop's
error case must still fire for genuinely unexpected ops — the case added
in step 2 keeps that property.

## Acceptance

* All existing blob tests (WireTest blob round trips, suite Tests 6/15)
  pass unchanged on servers negotiating 19 (Firebird 5.0.3+/6) **and**
  the results are byte-identical with the limit set to 0 versus default —
  proving transparency.
* `WireTest` gains: fetch a row set with small blobs, assert (via a
  packet/round-trip counter on the connection, which `WireTest` can read)
  that no `op_open_blob2` was sent for inline-served blobs; and a
  larger-than-limit blob still opens the classic way.
* Firebird 3/4 negotiate ≤17 and are unaffected — CI matrix proves it.
