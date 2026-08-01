# Firebird Pascal API (fbintf)

This package provides MWA Software's Firebird Pascal API (fbintf). It is intended to provide
a standard set of Language Bindings between programs written for the Free Pascal
Compiler (FPC) and the Firebird Client API. Both the new Firebird 3 and the legacy
API are supported. From release 2.0.2 onwards, the Delphi Win32 compiler is also suported.

fbintf is released under the InterBase Public License for the original code and under the
compatible Initial Developers Public License for new software.

See the "doc" directory for more information, installation instructions and a full manual
guide to the API. fbintf also comes with a comprehensive test suite. See doc/TestSuite.pdf
for more information.

## Connecting without a client library

From this release fbintf also includes a pure Pascal implementation of the
Firebird remote (wire) protocol in `client/wire`. It talks to a Firebird
3.0, 4.0, 5.0 or 6.0 server directly over TCP and needs no fbclient library
installed, which suits containers, single file deployments and cross
compiled targets. SRP authentication and wire encryption (ChaCha64, ChaCha
and Arc4) are supported.

Code written against the fbintf interfaces runs unchanged; only the way the
API is obtained differs:

```pascal
uses IB, FBWireClientAPI;

API := WireFirebirdAPI;      {instead of IB.FirebirdAPI}
```

See doc/WireProtocol.md for the full description, and client/wire/README.md
for a summary of what is and is not implemented. The implementation is FPC
only for now: the transport uses the FPC socket units.

See the "changelog" for information on changes from previous releases


