# Design: Scrollable cursors over the wire protocol

Roadmap milestone 7 (`doc/WireProtocol.md`). This document is the
implementation plan; no code changes are included.

## Goal

`FetchPrior`, `FetchFirst`, `FetchLast`, `FetchAbsolute` and
`FetchRelative` work on the wire provider when the negotiated protocol is
18 or later, via `op_fetch_scroll`.

## Current state

* `TWireResultSet.FetchPrior/First/Last/Absolute/Relative` each raise
  `ibxeNotSupported`; `TFBWireStatement.InternalOpenCursor` raises when
  asked for a scrollable cursor; `TFBWireAttachment.HasScollableCursors`
  hardcodes false (note the interface spelling, one `r` — keep it).
* `op_fetch_scroll = 112` and `op_info_cursor = 113` are defined in
  `FBWireConst.pas`, unused.
* `MaxSupportedProtocol` is 17, so **this milestone requires raising the
  offer to protocol 18** and verifying nothing else changes at 18 (the
  P18 additions are `op_fetch_scroll`/`op_info_cursor`; the accept
  handling itself is version agnostic).
* The forward-only row cache is `TWireCursorState`
  (`Rows: array of TBytes; NextRow: integer; EndOfCursor: boolean`),
  filled by `TFBWireConnection.FetchRow`, which drains the whole
  `op_fetch_response` batch before returning — a hard protocol
  requirement that applies equally to scroll fetches.

## The protocol

`op_fetch_scroll` is `op_fetch` plus two fields: statement handle, BLR,
message number, fetch count, then **direction** and **position**.
Directions follow `IResultSet`'s semantics: next, prior, first, last,
absolute, relative; position is the absolute row number or relative
offset (ignored for the others). The response stream is the same
`op_fetch_response` sequence, and end-of-window is still status 100 /
zero message count. A scrollable cursor must be requested at execute
time: `op_execute`'s cursor flags word gets the scrollable bit when the
statement was opened scrollable (mirrors `IStatement::CURSOR_TYPE_SCROLLABLE`).

## Design

1. **Protocol layer.** `TFBWireConnection.FetchRowScroll(aHandle, aFormat,
   aOutBuffer, aDirection, aPosition, var aState)` — same drain loop as
   `FetchRow`, plus the two extra fields. Gate: raise `ibxeNotSupported`
   if `ProtocolVersion < 18`. Raise `MaxSupportedProtocol` to
   `PROTOCOL_VERSION18` and extend `OfferedProtocols` (the negotiation
   code already caps by `MaxProtocol`, and the CI matrix measures what
   each server settles on).
2. **Cache semantics.** The existing cache assumes the server-side cursor
   position equals "last row the client fetched". Any non-sequential
   fetch invalidates that: on `FetchPrior/First/Last/Absolute/Relative`,
   discard `aState.Rows` (with a proper `SetLength(...,0)` — note the
   current `FillChar` reset of a record containing a managed dynamic
   array leaks the array; fix that while touching it) and fetch a batch
   of **one** for positioned directions. Batched read-ahead stays for
   `next` (and can be added for `prior` later; correctness first).
3. **Statement layer.** `InternalOpenCursor` stops raising for
   `Scrollable` when the attachment reports support; it records the flag
   and sets the scrollable bit at execute. `TWireResultSet`'s five
   methods delegate to a new `TFBWireStatement.FetchScroll(direction,
   position)`, symmetric with `FetchNextRow`, updating `FBOF`/`FEOF` per
   direction (prior before row 1 sets BOF, etc. — copy the semantics from
   `TFB30Statement`/`TResults`, which the suite's Test 2 pins down).
4. **Capability.** `HasScollableCursors` returns
   `Connection.ProtocolVersion >= 18`, and `TFBWireStatement.GetFlags`
   is overridden to include `stScrollable` truthfully (the base returns
   `[]`; `FB30Statement.GetPerfStatistics`'s neighbour `GetFlags` is the
   model). Test 2's scrollable section then runs automatically once
   milestone 1's suite run exists.

## Interaction with inline blobs

None yet — but the drain loop is the same one milestone 9 teaches to
accept `op_inline_blob` packets interleaved with rows, so both changes
land in the same few lines of `FetchRow`/`FetchRowScroll`. Sequence the
two milestones to avoid a merge knot (this one first, per roadmap order).

## Acceptance

* Test 2's `DoScrollableQuery` passes over the wire provider against
  Firebird 5 and 6 (protocol 18 servers); against Firebird 3/4 the
  capability stays false and the section skips as it does today.
* `WireTest` gains a scroll section: open scrollable, `FetchLast`,
  `FetchAbsolute(3)`, `FetchPrior`, `FetchRelative(-1)`, `FetchFirst`,
  asserting row identity each step against a known ordered result.
* The negotiation test in `WireTest` (caps at 14–17 today) extends to 18,
  and the CI step summary shows which servers settle on 18.
