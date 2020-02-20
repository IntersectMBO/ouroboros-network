// Taken from winio library by Felix Martini
#include <Win32-network.h>
#include <HsFFI.h>
#include <stdio.h>

#define DllExport __declspec(dllexport)

#define MAGIC_COMPLETION_KEY 696205568


DllExport
BOOL HsAssociateHandle(HANDLE handle, HANDLE port)
{
  return (CreateIoCompletionPort(handle, port, MAGIC_COMPLETION_KEY, 0)
          == port);
}

// 'CreateIoCompletionPort' can accept any system abstraction that represents
// overlapped I/O endpoint, including sockets.
// https://docs.microsoft.com/en-us/windows/win32/fileio/createiocompletionport#createiocompletionport-function
//
DllExport
BOOL HsAssociateSocket(SOCKET socket, HANDLE port)
{
  return (CreateIoCompletionPort((HANDLE) socket, port, MAGIC_COMPLETION_KEY, 0)
          == port);
}
