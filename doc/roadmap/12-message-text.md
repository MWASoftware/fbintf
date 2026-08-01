# Design: Engine message text without firebird.msg

Roadmap milestone 12 (`doc/WireProtocol.md`). This document is the
implementation plan; no code changes are included.

## Goal

Close the one visible difference from the stock providers: errors whose
text the server does not interpret currently read
`Firebird Error Code: 335544351` instead of
`unsuccessful metadata update`. After this milestone the wire provider
formats the same text `fbclient` would, with no file dependency.

## Current state

* `TFBWireStatus.GetIBMessage` (`FBWireClientAPI.pas`) returns the
  concatenated server-supplied strings (`isc_arg_string` /
  `isc_arg_interpreted` / `isc_arg_sql_state` items are folded into
  `FMessage` by `SetFromWireStatus`), falling back to
  `Firebird Error Code: %d` when the server sent none.
* A second, independent fallback lives in
  `EFBWireProtocolError.CreateFromStatus` (`FBWireProtocol.pas`):
  `Engine Code: %d`. The two should converge on one formatter.
* Most engine errors *do* arrive with interpreted text; the gap is the
  subset where the server expects the client to format the message from
  its own `firebird.msg`, substituting the status vector's string/number
  arguments into `@1`/`@2`/`@3` placeholders.

## The two options, decided

The roadmap names two routes: read `firebird.msg` when present, or
generate a Pascal table at build time. **The generated table is the
plan**: it keeps the provider's defining property (the binary is the
whole dependency), works on machines that have never seen a Firebird
install, and is testable offline. A `firebird.msg` reader can be added
later as an override; it is not needed for parity in practice.

## Design

### Generation

A build-time script (`client/wire/generate_messages.sh` +
`mkmsgs.pas`-style formatter, checked in with its output) consumes
Firebird's message source (`src/msgs/messages2.sql` / the `msg.fdb`
facility numbers in the Firebird tree — pinned to a tagged Firebird
release, recorded in the generated header) and emits
`client/wire/FBWireMessages.pas`:

```pascal
function FindEngineMessage(aCode: cardinal; out aFormat: AnsiString): boolean;
```

* Table restricted to facility 0 (JRD — the `isc_` engine codes) plus
  DSQL and DYN facilities: the ones a server round trip can produce.
  That is a few thousand entries; as flat `const` arrays (sorted codes +
  one string table) it compiles to a few hundred KB, which the roadmap
  already accepts. Binary search at runtime.
* The generated file is committed, so builders never run the generator;
  regenerating is a maintenance task per Firebird release.

### Formatting

`TFBWireStatus` currently throws away the *positional* relationship
between a `isc_arg_gds` item and its following string/number arguments
(strings go straight into `FMessage`). To format like `fbclient`:

* `SetFromWireStatus` keeps, per `isc_arg_gds` item, its trailing
  argument list (strings and numbers, in order) in a parallel structure
  (the C-style vector still cannot hold the string pointers — that
  constraint stands).
* `GetIBMessage` walks the gds items: look up the format string,
  substitute `@n` parameters (`@1` = first argument...; number arguments
  rendered decimal), join multiple gds items with the existing
  `LineEnding + '-'` convention. Server-interpreted items
  (`isc_arg_interpreted`) are used verbatim as today. Unknown code and
  no server text → the current numeric fallback, which also becomes the
  single shared formatter used by `EFBWireProtocolError`.

This reproduces `fb_interpret`'s output shape, which is what the
reference logs contain — making this milestone a prerequisite for a
clean suite diff (milestone 1's wire reference log shrinks once this
lands; sequence flexibly, the two only touch at the log).

## Acceptance

* Offline `WireTest` section (no server needed): assert
  `FindEngineMessage(335544351)` yields `unsuccessful metadata update`,
  and a parameterised case, e.g. 335544343 formatting `@1` substitution,
  matches `fbclient` output captured in the test as a literal.
* Live: provoke `isc_no_meta_update` (the suite's drop-table case
  already does) and a syntax error, and diff the `EIBInterBaseError`
  message text against the 3.0 provider on the same server — identical
  modulo the client-library-path banner.
* Binary size delta of `WireTest` recorded in the PR (expected: a few
  hundred KB), confirming the roadmap's estimate.
