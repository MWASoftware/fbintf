# Design: Running the existing test suite against the wire provider

Roadmap milestone 1 (`doc/WireProtocol.md`). This document is the
implementation plan; no code changes are included.

## Goal

`./testsuite -a wire ...` (option name to taste) runs all twenty two test
programs over `client/wire`, and that run becomes the acceptance criterion
for the events, services and array milestones.

## Where the provider is chosen

Exactly one place. `TTestApplication.GetFirebirdAPI`
(`testsuite/testApp/TestApplication.pas`) lazily assigns
`FFirebirdAPI := IB.FirebirdAPI`, and every test reaches the API through
`Owner.FirebirdAPI` — no test calls `IB.FirebirdAPI` directly. The only
other writer is `SetClientLibraryPath` (the `-l` option). So the switch is:

* a new option (`-a wire` / `--api wire`, added to `GetShortOptions`,
  `GetLongOptions` and both the FPC and Delphi `GetParams` variants);
* when given, `GetFirebirdAPI` assigns `WireFirebirdAPI` from
  `FBWireClientAPI` instead of `IB.FirebirdAPI`.

Build plumbing: `testsuite/Makefile.fpc` (`unitdir`) and
`testsuite/testsuite.lpi` (`OtherUnitFiles`) do not list `../client/wire`
today; both need it. The top level `Makefile.fpc` and `fbintf.lpk` already
include it.

## What will break, known in advance

The research below is from reading the harness, not speculation; each item
names the line it comes from.

**Harness faults (must fix first):**

* `DoRun` prints
  `FirebirdAPI.GetFBLibrary.GetLibraryFilePath` in the banner, and
  `WriteAttachmentInfo` does the same per attachment. The wire API has no
  library, so `GetFBLibrary` returns nil and both dereference it. Guard
  both: print `Firebird Client Library Path = none (wire protocol)` when
  `GetFBLibrary` is nil. This also documents itself in the output.
* `HasMasterIntf` is false for the wire provider, so the Bin/Conf
  directory banner lines are omitted — fine, already guarded.

**Feature gaps (expected, become milestone acceptance criteria):**

| Test | Feature | Current wire behaviour |
|---|---|---|
| 2 | scrollable cursors | `HasScollableCursors` false → section self-skips |
| 7, 8, 18 | arrays | `ibxeNotSupported` → "Test Completed with Error" |
| 10 | events | `GetEventHandler` raises → error |
| 11, 16 | services | `HasServiceAPI` false → sections self-skip |
| 13 | multi-database transaction | `StartTransaction` with two attachments raises |
| 19 | batch | `HasBatchMode` false → skip path |
| 20 | batch stress | guards on client major/ODS, not `HasBatchMode` — will error |

Tests 19, 2, 11 and 16 already degrade gracefully because they test a
capability flag first. Tests 7, 8, 10, 13, 18 and 20 do not, and the
harness's generic handler turns the raise into a single
`Test Completed with Error` line, aborting the rest of that test. That is
survivable but noisy, and it aborts unrelated later checks in the same
test.

**Environment differences (diff noise, not failures):**

* the banner's `Client API Version` will read `5.0`
  (`WireClientMajorVersion`), and connect strings take the `inet://` form
  since the reported client major is ≥ 3;
* reference logs embed absolute library paths that cannot match.

## The skip story

Pass/fail today is a `diff` against `FB<n>reference.log` chosen by ODS
version in `runtest.sh` — the per-version reference logs are how
"unsupported" is already encoded (e.g. the FB2/FB3 logs contain the
"Skipping test for Firebird 4 and later" lines). Two mechanisms exist in
the harness and are currently unused: `TTestBase.SkipTest` (virtual,
consulted by `DoTest`) and `ESkipException` (caught by `DoTest`).

Plan, in keeping with the existing convention:

1. In tests 7, 8, 10, 13, 18, 20: guard the unsupported feature on the
   capability the provider actually reports (`GetEventHandler` needs a new
   `HasEventHandler`-style check, or `try...on E: EIBClientError` with the
   `ibxeNotSupported` code → write a fixed `Skipping: ...` line). Prefer
   extending the capability surface (`IFirebirdAPI`/`IAttachment` already
   have `HasServiceAPI`, `HasBatchMode`, `HasScollableCursors`; events and
   arrays deserve the same) over exception sniffing.
2. Add `testsuite/FBWirereference.log` produced the same way the others
   were, with the skip lines in place, and teach `runtest.sh` to select it
   when the output banner says the wire provider is in use (a new banner
   line `Provider = wire` makes that trivial).
3. As milestones 2, 3 and 5 land, the skip lines shrink and the reference
   log is regenerated — the diff *is* the progress report.

## CI

Extend `.github/workflows/wire-protocol.yml` with a job that builds the
full suite (FPC, `unitdir` fix above) and runs
`./testsuite -a wire -u SYSDBA -p masterkey -e inet://localhost/employee ...`
against the same container matrix, diffing against
`FBWirereference.log`. Keep `WireTest` as-is: it is the offline/unit layer,
the suite is the integration layer.

## Acceptance

* The full suite runs to `Test Suite Ends` with no crash on Firebird 3–6.
* The diff against the wire reference log is empty.
* No change in the suite's behaviour for the two existing providers: the
  default path through `GetFirebirdAPI` is untouched and the existing
  reference logs still match.
