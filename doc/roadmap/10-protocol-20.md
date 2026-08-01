# Design: Firebird 6 and protocol 20

Roadmap milestone 10 (`doc/WireProtocol.md`).

**Status: implemented** — the offer goes to protocol 20, WireTest gains
a schema section (164 tests) and the negotiation test runs the full
14..20 ladder. Findings against the plan:

* The field audit found exactly one unconditional packet change at 20:
  `p_sqlst_flags`, a flags word at the end of both
  `op_prepare_statement` **and** `op_exec_immediate` (they share the
  XDR block; the plan only named the prepare). Written as zero.
* Named arguments are a SQL level feature (`name => value` in calls),
  not a describe item - there is nothing to request or parse for them
  on the wire.
* The describe gains `isc_info_sql_relation_schema` (33), requested
  only from protocol 20 connections and parsed into the column format.
  `ParsePrepareResponse` already skipped unknown items with their
  length, so no hardening was needed - the defensive property the plan
  asked to verify was there.
* The 3.0 provider surfaces no schema name (fbintf's `IColumnMetaData`
  has no such member and the bundled `FirebirdOOAPI` predates one), so
  there was nothing to match: the wire stores the name and exposes it
  through `TFBWireStatement.ColumnSchemaName` for the tests, leaving
  the interface question to fbintf as a whole.
* `isc_dpb_search_path` went in alongside the other Firebird 6 DPB
  tags (97..107), with the DPB name table extended to match, and works
  through the ordinary `DPB.Add(...).AsString` route - proven by the
  WireTest section attaching with a search path and reading the
  expected table.

## Goal

Negotiate protocol 20 with Firebird 6 and gain its two features: SQL
schema support (search path, schema-qualified metadata) and named
arguments in the describe information. Firebird 6 already works with this
client today by negotiating down to 17; this milestone is about the new
capabilities, not compatibility.

## Current state

* `PROTOCOL_VERSION20` is defined in `FBWireConst.pas`;
  `MaxSupportedProtocol` is 17 (rising to 19 through milestones 7 and 9 —
  this milestone should land after them, per roadmap order, so each
  version bump is tested in isolation).
* The describe parser is `FBWireDescribe.ParsePrepareResponse` over
  `isc_info_sql_*` clumplets; the DPB builder is the shared
  `FBParamBlock`.
* The CI matrix already runs Firebird 6 (`WireCrypt` Enabled and
  Required), so acceptance infrastructure exists.

## What protocol 20 changes

Three things touch this client; each is independently small:

1. **Schema search path in the attach.** `isc_dpb_search_path` carries
   the schema search path. This is a DPB tag, so `FBParamBlock` needs
   only the constant — users add it like any other DPB item. The
   session-level `SET SEARCH_PATH` statement works regardless; the DPB
   route makes it declarative at connect.
2. **Schema-aware describe.** The describe response gains schema name
   items per column (`isc_info_sql_relation_schema` and friends).
   `ParsePrepareResponse` must skip unknown items robustly today (verify
   — a parser that raises on unknown clumplets breaks the moment the
   server is asked for new items) and `TWireSQLVar` gains the schema
   name, surfaced through the existing `IColumnMetaData` where the 3.0
   provider surfaces it for Firebird 6.
3. **`op_prepare_statement` flags.** Protocol 20 adds a flags word to the
   prepare packet (named-argument describe among them). Written when
   `ProtocolVersion >= 20`, zero by default.

Named arguments themselves (`:name` binding by name at the API level) are
an fbintf-wide feature question — the wire work is only to request and
parse the argument names; exposing them follows whatever the 3.0 provider
does for Firebird 6.

## Design

1. Raise `MaxSupportedProtocol` to `PROTOCOL_VERSION20`, extend
   `OfferedProtocols` (weights continue the existing `(i+1)*2` series).
   Audit every `FProtocolVersion >=` site for fields protocol 20 appends
   — the known ones are the prepare flags and any execute-packet growth;
   the audit method is a field-by-field read of Firebird 6's
   `protocol.cpp` send/receive for the DSQL ops, recorded in
   `doc/WireProtocol.md` the way the 13–17 differences already are.
2. Add the new `isc_dpb_search_path` and `isc_info_sql_*` schema
   constants to `client/include` alongside their peers.
3. Harden `ParsePrepareResponse` to skip-with-length any unknown item
   (defensive regardless of this milestone), then teach it the schema
   items.
4. Plumb schema name into `TWireSQLVar` → `TWireSQLVarData` →
   `IColumnMetaData.GetRelationSchema` (or whatever name the 3.0
   provider settles on for FB6 — match it, don't invent).

## Acceptance

* Firebird 6 CI rows negotiate 20 and the whole `WireTest` suite still
  passes — the step summary makes the negotiated version visible.
* The negotiation cap test extends to 20, and capping at 17 against
  Firebird 6 still works (regression guard for mixed-version fleets).
* New `WireTest` section, gated on protocol ≥ 20: create two schemas,
  same-named table in each, attach with a search path and assert the
  right table answers; describe a query and assert the schema name
  arrives per column.
* Firebird 3/4/5 rows are byte-identical in behaviour (they never see
  the new fields).
