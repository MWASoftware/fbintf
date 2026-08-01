# Design: Services over the wire protocol

Roadmap milestone 3 (`doc/WireProtocol.md`). This document is the
implementation plan; no code changes are included.

## Goal

`IServiceManager` works on the wire provider: attach to `service_mgr`,
start and query services, so backup, restore, statistics and user
management run from a client with no `fbclient` installed.

This is the smallest milestone by a distance, because both halves already
exist and only the joining layer is missing.

## What already exists

**The wire half is done.** `TFBWireConnection` in
`client/wire/FBWireProtocol.pas` already implements, unused:

* `ServiceAttach(const aServiceName: AnsiString; SPB: TBytes): integer` —
  sends `op_service_attach`, returns the object handle;
* `ServiceDetach(aSvcHandle: integer)` — `op_service_detach`;
* `ServiceStart(aSvcHandle: integer; const aItems: TBytes)` —
  `op_service_start`;
* `ServiceQuery(aSvcHandle; aSendItems, aRecvItems: TBytes; aBufferLength)`
  — `op_service_info`, returning the response data bytes.

**The fbintf half is done.** `TFBServiceManager` in `client/FBServices.pas`
implements most of `IServiceManager` (`Attach`, `AllocateSRB`,
`AllocateSQPB`, the connect string assembly, the single-argument `Query`)
and leaves exactly these to the provider subclass:

* `InternalAttach(ConnectString)` — virtual abstract;
* `Detach(Force)` — virtual abstract;
* `Query(SQPB, Request, RaiseExceptionOnError)` — virtual abstract;
* `IsAttached` and `Start(Request, RaiseExceptionOnError)` — required by
  `IServiceManager` but not present in the base.

`TSPB`, `TSRB` and `TSQPB` build the parameter blocks;
`TServiceQueryResults` (`FBOutputBlock.pas`) parses the reply.
`ParamBlockToBytes` in `FBWireClientAPI.pas` already converts any
`TParamBlock` to the `TBytes` the wire methods want.

Currently `TFBWireClientAPI.HasServiceAPI` returns false and
`AllocateSPB` / both `GetServiceManager` overloads raise
`ibxeNotSupported`.

## Design

New unit `client/wire/FBWireServices.pas`:

```pascal
TFBWireServiceManager = class(TFBServiceManager, IServiceManager)
```

following `TFB25ServiceManager` (the handle-based model), with a
`TFBWireConnection` of its own instead of an `isc` handle:

* `InternalAttach(ConnectString)` — parse host and port with the same
  `IBUtils` machinery the attachment uses, create a `TFBWireConnection`,
  `ConnectTo` it, then `ServiceAttach('service_mgr', ParamBlockToBytes(FSPB))`
  and keep the returned handle. One nuance: the initial `op_connect` for a
  service session should carry `op_service_attach` as its
  `p_cnct_operation` where the current code always sends `op_attach`; the
  operation value becomes a parameter of `ConnectTo`.
* `Detach(Force)` — `ServiceDetach(handle)`, then `Disconnect`; swallow
  errors when `Force`.
* `IsAttached` — connection assigned and handle valid.
* `Start(Request, RaiseExceptionOnError)` —
  `ServiceStart(handle, ParamBlockToBytes(Request))`.
* `Query(SQPB, Request, RaiseExceptionOnError)` — call `ServiceQuery` with
  the two byte blocks and `TServiceQueryResults.DefaultBufferSize`, copy
  the reply into a `TServiceQueryResults` and return it.

`TFBWireClientAPI` then flips over:

* `HasServiceAPI` returns true;
* `AllocateSPB` returns `TSPB.Create(self)`;
* both `GetServiceManager` overloads construct `TFBWireServiceManager`,
  mirroring the shape of `OpenDatabase`.

Authentication and encryption need no new work: the SRP exchange and
`op_crypt` happen in `ConnectTo` before the service attach, exactly as for
a database attach. The password is consumed by SRP and stripped from the
SPB the same way the attachment strips it from the DPB.

### Error handling

Service errors arrive as ordinary status vectors on `op_response`;
`ReceiveAndCheckResponse` already raises through the standard path, so
`EIBInterBaseError` behaves as with the other providers. The
`ibxeServiceActive` / `ibxeServiceInActive` guards from `TFB30ServiceManager`
are copied so misuse fails with the same messages.

## Acceptance

* `Test19` / the service-related programs of the test suite pass against
  the wire provider (via the milestone 1 provider switch).
* A new `WireTest` section: attach to `service_mgr`, query
  `isc_info_svc_server_version` and `isc_info_svc_implementation`, assert
  non-empty strings; run `isc_action_svc_db_stats` header-page stats on the
  test database and assert output arrives.
* Green across the CI matrix (Firebird 3–6, WireCrypt Enabled and
  Required): everything used here is protocol 13 level.

## Out of scope

The `isc_spb_*` version 3 service parameter extensions beyond what `TSPB`
already writes, and trusted authentication (`Win_SSPI`) which the roadmap
excludes globally.
