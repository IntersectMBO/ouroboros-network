# Asynchronious IO for Windows

`Win32-network` provides low level api for doing IO / networking on Windows
using Overlapped (or asynchronous) IO.  It supports:

* `File` Api
* `NamedPipes` Api
* `winsock2` - Berkeley sockets api on Windows

`NamedPipes` provide a good alternative for the lack of Unix Sockets on
Windows, and there are ways of providing abstraction for both, though this is
not present in this package.

An application which is using this package should use `-threaded` option, as
the io manager thread runs a blocking ffi call (e.g.
[GetQueuedCompletionStatus](https://docs.microsoft.com/en-us/windows/win32/api/ioapiset/nf-ioapiset-getqueuedcompletionstatus)).

## Acknowledgement

The initial version of this library was based on
[winio](https://hackage.haskell.org/package/winio) by Felix Martini.
