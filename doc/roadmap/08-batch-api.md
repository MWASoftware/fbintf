# Design: The batch API over the wire protocol

Roadmap milestone 8 (`doc/WireProtocol.md`).

**Status: implemented** — `TFBWireConnection.BatchCreate/Msg/RegBlob/
Exec/Release/Cancel`, `TWireBatchCompletion`, and the `TFBWireStatement`
overrides, with Tests 19 and 20 passing over the wire and a WireTest
batch section (151 tests). The design held in outline; the findings
that reshaped its details:

* Batch messages do **not** travel as an eight byte padded counted
  blob: `xdr_packed_message` is byte for byte the ordinary row message
  encoding (null bitmap + values), so `XDREncodeMessage` served
  unchanged. The eight byte alignment governs the server's buffer
  spacing and the client's buffer accounting only.
* `op_batch_create`'s message length field must be computed with the
  server's `PARSE_msg_format` rules from our BLR (`EngineMessageLength`)
  - the client's private buffer layout is laid out differently and its
  size is rejected. The `IBatch` parameter block is a *wide* tagged
  clumplet buffer: four byte lengths, not the DPB's one byte form.
* Phase 2 was partially needed after all: the engine translates every
  non null, non zero blob id in a batch message through a registration
  map and consumes the entry, so pre-existing blob ids (Test 19's
  pattern) must be registered with `op_batch_regblob` once per row -
  exactly what `TFB30Statement`'s message packer does with
  `registerBlob`. Array ids pass through untouched, so
  `op_batch_set_bpb`/`op_batch_blob_stream` remain unimplemented (no
  fbintf surface reaches them).
* `TAG_MULTIERROR` is not set, matching the 3.0 provider: a batch stops
  at its first failing row and the completion reports it.
* The deepest change was not in the batch code: the wire statement's
  default text type moved from `SQL_TEXT` to `SQL_VARYING`, as the
  fbclient providers answer. The `SQL_TEXT` default re-sized the
  parameter format on every string assignment - fatal once
  `op_batch_create` has frozen the BLR - and its blank padding leaked
  into stored values. The switch exposed and fixed two latent accessor
  bugs (a varying *parameter's* `SQLData` must point at the characters
  while a *column's* points at the length prefix - `TSQLParam` relies
  on the distinction), and `CanChangeMetaData` now answers false while
  a batch is open, refusing mid batch type changes with the same error
  the 3.0 provider raises. The suite's parameter metadata and value
  output now matches the fbclient reference logs where it previously
  diverged (blank padded CHAR echoes, padded `INCLEAR` hex dumps).

## Goal

`IStatement.AddToBatch` / `ExecuteBatch` / `CancelBatch` /
`GetBatchCompletion` work over the wire on protocol 16+ connections, with
`IBatchCompletion` reporting per-row status, matching the 3.0 provider's
behaviour on Firebird 4+.

## Current state

* `TFBStatement` already carries the whole public surface with defaults:
  the four methods raise `ibxeBatchModeNotSupported`,
  `Get/SetBatchRowLimit` are concrete (`DefaultBatchRowLimit = 1000`),
  `IsInBatchMode`/`HasBatchMode` return false.
  `TFBWireAttachment.HasBatchMode` returns false. The wire statement has
  no batch code at all.
* The full opcode family is defined and unused in `FBWireConst.pas`:
  `op_batch_create = 99`, `op_batch_msg`, `op_batch_exec`,
  `op_batch_rls`, `op_batch_cs`, `op_batch_regblob`,
  `op_batch_blob_stream`, `op_batch_set_bpb`, `op_batch_cancel`,
  `op_batch_sync`, `op_info_batch`.
* `TBatchCompletion` in `client/3.0/FB30Statement.pas` shows the result
  shape (`getTotalProcessed`, `getState` mapping to
  `bcExecuteFailed`/`bcSuccessNoInfo`/`bcNoMoreErrors`, `getErrorStatus`,
  `getUpdated`), but wraps the OO API's completion object — the wire
  version parses the same information out of `op_batch_cs` directly, so
  it is a sibling, not a reuse.

## The protocol

* **`op_batch_create`**: statement handle, the BLR describing the message
  (the same `BuildMessageBlr` output used for `op_execute`), the message
  length, and a parameter block (`TAG_RECORD_COUNTS`,
  `TAG_BUFFER_BYTES_SIZE`, `TAG_MULTIERROR`...) — the same tags
  `TFB30Statement.AddToBatch` writes with `IXpbBuilder.BATCH`, built here
  with `FBParamBlock` machinery.
* **`op_batch_msg`**: statement handle, message count, then the messages
  as one counted blob, **each message padded to an 8 byte boundary** —
  note `FBWireMessage` aligns to 4 today, so the batch encoder pads
  explicitly.
* **`op_batch_exec`**: statement handle, transaction handle.
* **`op_batch_cs`** (the response to exec): per-row completion — total
  count, then vectors of update counts and status vectors for failed
  rows. Parsed into the wire `TBatchCompletion`.
* **`op_batch_rls`** releases the batch; `op_batch_cancel` abandons it.
* Blob-carrying batches additionally use `op_batch_regblob` /
  `op_batch_blob_stream` / `op_batch_set_bpb`.

## Design

Phase 1 — no blobs (matches most real batch use: bulk insert of scalars):

1. `TFBWireConnection` gains `BatchCreate`, `BatchMsg`, `BatchExec` (→
   parsed completion record), `BatchRelease`, `BatchCancel`, each gated on
   `ProtocolVersion >= 16`.
2. `TWireBatchCompletion = class(TInterfaceOwner, IBatchCompletion)` in
   `FBWireStatement.pas`, built from the parsed `op_batch_cs` body;
   semantics copied from the 3.0 class (RowNo is 1-based first failure,
   `getUpdated` counts until first failure).
3. `TFBWireStatement` overrides the four methods plus
   `IsInBatchMode`/`CheckChangeBatchRowLimit`, with the same guard
   behaviour as 3.0 (`ibxeInvalidBatchQuery` unless the statement is an
   insert/update/delete/exec-procedure; `ibxeInBatchMode` on row-limit
   change mid-batch; `EIBBatchBufferOverflow` past
   `FBatchRowLimit * aligned row size` clamped to [16MB, 256MB] — same
   sizing rule as 3.0 so behaviour matches across providers).
4. Messages accumulate client-side in a growable buffer (8-byte padded);
   `ExecuteBatch` sends create + msg + exec in one flush and parses the
   completion; failures raise `EIBInterBaseError` via the same
   `Check4BatchCompletionError` logic 3.0 uses.
5. `TFBWireAttachment.HasBatchMode` returns
   `Connection.ProtocolVersion >= 16`.

Phase 2 — blobs in batch (`op_batch_regblob` for existing blob ids,
`op_batch_set_bpb` + `op_batch_blob_stream` for inline creation), only
after phase 1 is green; Test 19 includes blob columns, so phase 1 alone
keeps its skip line for the blob subtest or the test is split.

## Acceptance

* Test 19 (batch update/insert) and Test 20 (batch stress) pass over the
  wire provider against Firebird 4, 5 and 6; against Firebird 3
  `HasBatchMode` is false (protocol 15) and both keep their skip paths.
* `WireTest` gains an offline check of the 8-byte message padding and a
  live section: 1000-row batch insert, one deliberate constraint
  violation mid-batch with `TAG_MULTIERROR`, asserting
  `getTotalProcessed`, the failing row number and `isc` code from
  `getErrorStatus`, and rollback behaviour.
* Wire and 3.0 providers produce the same `IBatchCompletion` answers for
  the same input — asserted by running the same batch through both in the
  suite.
