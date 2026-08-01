# Design: Wire compression

Roadmap milestone 11 (`doc/WireProtocol.md`). This document is the
implementation plan; no code changes are included.

## Goal

Request `pflag_compress` in the connect, and when the server accepts, run
zlib over the byte stream **beneath** the cipher (compress, then encrypt —
compressing ciphertext is useless), matching `WireCompression = true` in
`firebird.conf`.

## Current state

* `pflag_compress = $100` is defined in `FBWireConst.pas` and never set;
  the accept handler masks the type with `ptype_mask = $FF`, so the flag
  bit is currently discarded unseen — it must be captured before masking.
* The transport (`TFBWireTransport`) has exactly one filter slot per
  direction (`FSendCipher`/`FRecvCipher: TWireCipher`), applied
  wholesale: encrypt in `Flush` over the pending send buffer, decrypt in
  `FillRecvBuffer` over the freshly read chunk. That model fits a stream
  cipher (length-preserving) and does **not** fit zlib, which changes
  byte counts.
* FPC ships zlib bindings (`zbase`/`zinflate`/`zdeflate` in the `paszlib`
  package — pure Pascal, so the no-external-dependencies property
  survives); Delphi has `System.ZLib`. The transport remains the only
  compiler-sensitive unit.

## Design

### Negotiation

`ConnectTo` sets `pflag_compress` on each offered entry's type field when
a new `Compression` property is true (default **false** initially;
flipping the default can follow once soak-tested). In the accept,
capture `aType and pflag_compress` before the existing `ptype_mask`
masking; compression is on only if both sides asked.

### Transport restructure

The send and receive paths get a two-stage pipeline with an explicit
buffer between the stages, replacing the in-place single-buffer trick
only when compression is active:

* **Send**: `Flush` runs deflate (`Z_SYNC_FLUSH` — packet boundaries must
  reach the server promptly; a plain full-flush-never deflate deadlocks
  request/response protocols) over the pending plaintext into a staging
  buffer, then the cipher over the staging buffer, then the socket
  write. With compression off, today's path is untouched.
* **Receive**: `FillRecvBuffer` reads ciphertext, decrypts in place
  (unchanged), then feeds the result into the inflate stream, and
  `ReadBytes` consumes inflate output. Because inflate output length is
  unpredictable, the receive side becomes: raw buffer → inflate →
  plaintext buffer, with `FRecvPos/FRecvLimit` moving to the plaintext
  buffer. `HasBufferedData` must account for bytes held inside the
  inflate state (pending output *and* unconsumed input) — it guards
  cipher installation and event polling, so getting it wrong corrupts
  the stream.

One zlib stream per direction for the lifetime of the connection (the
protocol compresses the stream, not packets), created lazily when the
accept confirms compression.

### Ordering with encryption

`op_crypt` changeover happens after compression is already active
(compression starts with the accept; encryption starts later). The
existing asymmetric changeover logic is unchanged because the cipher
still sits directly against the socket on both paths — the pipeline
order compress→encrypt on send and decrypt→inflate on receive keeps the
cipher position identical to today. The `EnableRecvCipher` "unread data"
guard now also requires the inflate stage to be drained.

## Risks

* The receive rework touches the hottest path in the client; it must be
  refactored so the compression-off path is provably identical (same
  code, staging disabled) — the CI matrix and reference logs are the
  guard.
* `Z_SYNC_FLUSH` overhead makes small packets slightly larger; the
  feature only pays on bulk fetches over slow links, which is why the
  default stays off and the property is per-connection.

## Acceptance

* `WireTest` offline: a loopback deflate/inflate round trip through the
  transport pipeline with the cipher also enabled, asserting byte
  fidelity across chunk-boundary-straddling reads.
* Live: the full `WireTest` run against a server with
  `WireCompression = true`, in all three cases: compression only,
  encryption only, both. CI matrix gains a `WireCompression = true`
  dimension row for Firebird 5 (one row, not the cross product — the
  compression code is version independent, protocol ≥ 13 all the same).
* With the property false, packets are byte-identical to today.
