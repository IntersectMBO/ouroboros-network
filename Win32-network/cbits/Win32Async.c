// Taken from winio library by Felix Martini
#include <windows.h>
#include <HsFFI.h>

#define DllExport __declspec(dllexport)

// each OVERLAPPED struct is wrapped in PER_IO_DATA, we use CONTAINING_RECORD
// windows kernel macro to retrieve the pointer to it from a pointer to
// OVERLAPPED struct.
typedef struct _PER_IO_DATA {
    OVERLAPPED  Overlapped;
    HsStablePtr UserData;
} PER_IO_DATA;

#define MAGIC_COMPLETION_KEY 696205568

// A wrapper around 'GetQueuedCompletionStatus' system call.  It frees the
// 'PER_IO_DATA' record which wrappes 'OVERLLAPPED' struct, which is allocated
// by 'HsAsyncRead' or 'HsAsyncWrite'
DllExport
BOOL HsGetQueuedCompletionStatus(HANDLE port, DWORD *numBytes, HsStablePtr *userData,
                                     DWORD timeout)
{
    OVERLAPPED *overlappedPtr = NULL;
    LONGLONG    completionKey  = 0;
    BOOL res;
    
    res = GetQueuedCompletionStatus(port,
				    numBytes,
				    &completionKey,
				    &overlappedPtr,
				    timeout);
    if (overlappedPtr != NULL && completionKey == MAGIC_COMPLETION_KEY) {
        PER_IO_DATA *perIoDataPtr;
        perIoDataPtr =
            CONTAINING_RECORD(overlappedPtr, PER_IO_DATA, Overlapped);
	*userData = perIoDataPtr->UserData;
        HeapFree(GetProcessHeap(), 0, perIoDataPtr);
    } else {
        *userData = NULL;
    }
    return res;
}


// A wrapper of 'ReadFile' system call.  It allocates 'PER_IO_DATA' which will
// be freed by 'HsGetQueuedCompletionStatus'.
//
// Aruments:
// HANDLE handle    - handle which must be initialised with FILE_FLAG_OVERLAPPED
// void   *buf      - reading buffer
// DWORD  size      - size of the reading buffer
// void   *userData - a stable pointer to an 'MVar' which holds the
//                    result of the read operation
//
// Returns number of bytes written synchronously or -1 on error.
// The error ERROR_IO_PENDING is used on Windows to signal successful
// initialisation of async operation.  In this case on completion, the
// associated completion port will be triggered and the results will be
// obtained via HsGetQueuedCompletionStatus.
DllExport
void HsAsyncRead(HANDLE handle, void *buf, DWORD size,
                 HsStablePtr userData)
{
    PER_IO_DATA *perIoDataPtr;
    DWORD numBytesRead;
    BOOL res;
    
    perIoDataPtr =
        HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(PER_IO_DATA));
    if (perIoDataPtr == NULL) {
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        return;
    } else {
        perIoDataPtr->Overlapped.Offset = 0;
        perIoDataPtr->Overlapped.OffsetHigh = 0;
        perIoDataPtr->UserData = userData;
        res = ReadFile(handle, buf, size, &numBytesRead,
                  &(perIoDataPtr->Overlapped));
        if (!res) {
            DWORD error;
            error = GetLastError();
            if (error != ERROR_IO_PENDING) {
                HeapFree(GetProcessHeap(), 0, perIoDataPtr);
            }
            return;
        } else {
            // We report a pending operation even if it has completed
            // synchronously because Windows still posts a completion event
            // to the port. TODO: Vista and later operating systems support a
            // function named SetFileCompletionNotificationModes which can
            // prevent that.
            SetLastError(ERROR_IO_PENDING);
            return;
        }
    }
}

// Allocates 'PER_IO_DATA' which will be freed by
// 'HsGetQueuedCompletionStatus'.
//
// arguments:
// HANDLE handle    - handle which must be initialised with FILE_FLAG_OVERLAPPED
// void   *buf      - write buffer
// int    size      - size of the write buffer
// void   *userData - a stable pointer to an 'MVar' which holds the
//                    result of the read operation
//
// Negative returned value indicates an error.
DllExport
void HsAsyncWrite(HANDLE handle, void *buf, int size,
                  HsStablePtr userData)
{
    PER_IO_DATA *perIoDataPtr;
    DWORD numBytesWritten;
    BOOL res;
    
    perIoDataPtr =
        HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(PER_IO_DATA));
    if (perIoDataPtr == NULL) {
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        return;
    } else {
        perIoDataPtr->Overlapped.Offset = 0;
        perIoDataPtr->Overlapped.OffsetHigh = 0;
        perIoDataPtr->UserData = userData;
        res = WriteFile(handle, buf, size, &numBytesWritten,
                        &(perIoDataPtr->Overlapped));
        if (!res) {
            DWORD error;
            error = GetLastError();
            if (error != ERROR_IO_PENDING) {
                HeapFree(GetProcessHeap(), 0, perIoDataPtr);
            }
            return;
        } else {
            SetLastError(ERROR_IO_PENDING);
            return;
        }
    }
}

void HsAssociate(HANDLE handle, HANDLE port)
{
  CreateIoCompletionPort(handle, port, MAGIC_COMPLETION_KEY, 0);
  return;
}


DllExport
void HsConnectNamedPipe(HANDLE handle, HsStablePtr userData)
{
  BOOL res;
  PER_IO_DATA *perIoDataPtr;
  perIoDataPtr =
      HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(PER_IO_DATA));
  if (perIoDataPtr == NULL) {
      SetLastError(ERROR_NOT_ENOUGH_MEMORY);
      return;
  } else {
      perIoDataPtr->Overlapped.Offset = 0;
      perIoDataPtr->Overlapped.OffsetHigh = 0;
      perIoDataPtr->UserData = userData;
      res = ConnectNamedPipe(handle, &(perIoDataPtr->Overlapped));
      if (!res) {
	DWORD error;
	error = GetLastError();
	if (error != ERROR_IO_PENDING && error != ERROR_PIPE_CONNECTED) {
	  HeapFree(GetProcessHeap(), 0, perIoDataPtr);
	}
	return;
      } else {
	SetLastError(ERROR_IO_PENDING);
	return;
      }
  }
}
