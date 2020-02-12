// Taken from winio library by Felix Martini
#include <Win32-network.h>
#include <HsFFI.h>
#include <stdio.h>

#define DllExport __declspec(dllexport)

#define MAGIC_COMPLETION_KEY 696205568

// A wrapper around 'GetQueuedCompletionStatus' system call.  It frees the
// 'IODATA' record which wrappes 'OVERLLAPPED' struct, which is allocated
// by 'HsAsyncRead' or 'HsAsyncWrite'.
//
DllExport
BOOL HsGetQueuedCompletionStatus( HANDLE       port
                                , DWORD       *numBytes
                                , DWORD        timeout
                                , GQCSRESULT  *gqcsResult
                                )
{
    OVERLAPPED *overlappedPtr = NULL;
    LONGLONG    completionKey = 0;
    BOOL result;

    result = GetQueuedCompletionStatus(port,
                                       numBytes,
                                       &completionKey,
                                       &overlappedPtr,
				       timeout);

    gqcsResult->gqcsResult           = result;
    gqcsResult->gqcsOverlappedIsNull = overlappedPtr == NULL;
    gqcsResult->gqcsCompletionKey    = completionKey == MAGIC_COMPLETION_KEY;
    if (overlappedPtr != NULL && completionKey == MAGIC_COMPLETION_KEY) {
      gqcsResult->gqcsIODataPtr =
          CONTAINING_RECORD(overlappedPtr, IODATA, iodOverlapped);
    } else {
      gqcsResult->gqcsIODataPtr = NULL;
    }
}


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
