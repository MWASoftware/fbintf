# Design: A Delphi transport for the wire provider

Roadmap milestone 4 (`doc/WireProtocol.md`).

**Status: written, unverified** — the Delphi branches exist in
`FBWireStream.pas` (`Winapi.Winsock2` on Windows behind a one time
`WSAStartup`, the `Posix.*` units elsewhere; `getaddrinfo` with an IPv4
result preferred, `TCP_NODELAY`, `SO_RCVTIMEO`/`SO_SNDTIMEO` from
`aTimeout`, `recv`/`send`, graceful `shutdown` in `Disconnect`) and the
wire units are listed in `fbintf.dpk`/`fbintf.dproj`. Deviations from
the plan below: wire compression (milestone 11, paszlib) stays FPC only
— `EnableCompression` raises a clear error under Delphi — and the
`WireTest` Delphi project file has not been created. **No Delphi
toolchain exists on this host or in CI, so none of it has been compiled
or run**; the verification section below is still the outstanding work,
including the expected first-compile strictness pass.

## Goal

`client/wire` compiles and runs under Delphi (Windows and the Posix
targets), so the wire provider can be added to `fbintf.dpk` and used from
Delphi programs with no `fbclient` installed.

## How little is actually missing

The FPC dependency is confined to `FBWireStream.pas`, and inside it to one
field and three call sites:

* `FSocket: TInetSocket` (unit `ssockets`), created in `ConnectTo` with
  `IOTimeout` and a `fpsetsockopt` `TCP_NODELAY` call (unit `sockets`);
* `FSocket.Read` in `FillRecvBuffer`;
* `FSocket.Write` in `Flush`.

The non-FPC branches currently raise
`'Wire protocol transport is not implemented for this compiler'`. The XDR
codec (`TXDRStream`), the cipher classes, and everything above them are
already compiler neutral, and the units carry `{$mode delphi}`-compatible
code throughout. No packet logic changes.

## Design

Keep one unit, one class, and branch inside the four transport methods —
the same `{$IFDEF FPC}` structure that exists now grows a Delphi branch
instead of a raise. Rejected alternative: a class hierarchy
(`TFBWireTransport` abstract + per-compiler subclasses) — more moving
parts than three call sites justify, and it would force a factory into
`TFBWireConnection`.

Delphi branch:

* **Windows**: `Winapi.WinSock2` + `Winapi.Windows`. One-time `WSAStartup`
  guarded by a unit variable; `getaddrinfo` for the host (IPv4 first,
  matching current behaviour), `socket`/`connect`, `setsockopt`
  `TCP_NODELAY`, `SO_RCVTIMEO`/`SO_SNDTIMEO` from the `aTimeout`
  parameter; `recv`/`send` in `FillRecvBuffer`/`Flush`; `closesocket` +
  graceful `shutdown` in `Disconnect`.
* **Posix Delphi** (Linux/macOS targets): `Posix.SysSocket`,
  `Posix.NetinetIn`, `Posix.ArpaInet`, `Posix.NetDB`, `Posix.Unistd` —
  the same calls in their Posix spelling.

Error mapping: both branches raise `EFBWireError` with the socket error
code and text, matching the FPC branch's
`'Connection lost to database server'` behaviour on zero-byte reads, so
`WireIBError` keeps converting transport failures to `isc_network_error`.

Everything else in the unit — buffers, cipher hooks, `HasBufferedData` —
is untouched, because it operates on the byte buffers, not the socket.

## Packaging

* Add the ten `client/wire` units to `fbintf.dpk` / `fbintf.dproj`.
* `FBWireBigInt`, `FBWireCrypto`, `FBWireSRP` use no FPC-specific
  language features by design, but have only ever been compiled by FPC —
  expect a first-compile pass for Delphi strictness (typed `@`, `TBytes`
  index base, `AnsiString` code page warnings). These are mechanical.
* `WireTest.pas` gets a Delphi project file alongside the existing
  invocation, and its offline sections become the Delphi smoke test.

## Verification

CI cannot run Delphi. The plan mirrors what the repo already does for the
Delphi test suite (`testsuite/delphitestsuite.groupproj`):

* the **offline** job already builds `WireTest` on a Windows runner with
  FPC — it stays, protecting the shared code on Windows;
* Delphi compilation is verified manually per release, like the rest of
  fbintf's Delphi support; the transport branch is small enough that the
  offline `WireTest` run (arithmetic, crypto, SRP, layout — all
  compiler-sensitive code) plus one live connection is sufficient
  acceptance.

## Acceptance

* `fbintf.dpk` builds in Delphi with the wire units included.
* `WireTest` offline sections pass under Delphi on Windows.
* A live connect + query against a Firebird 5 server from a Delphi-built
  binary, with `WireCrypt = Required`, works — this exercises the timeout,
  NODELAY and cipher paths end to end.
* FPC behaviour is bit-for-bit unchanged (the FPC branch is not edited).
