// Taken from winio library by Felix Martini
#include <winsock2.h>
#include <windows.h>
#include <HsFFI.h>

#define DllExport __declspec(dllexport)

typedef struct _WSA_PER_IO_DATA {
    WSAOVERLAPPED WSAOverlapped;
    HsStablePtr UserData;
} WSA_PER_IO_DATA;

DllExport
void HsSendBuf(SOCKET s, CHAR *buf, ULONG len, HsStablePtr userData)
{
  int res;
  WSA_PER_IO_DATA *perIoDataPtr;

  perIoDataPtr =
      HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(WSA_PER_IO_DATA));
  if (perIoDataPtr == NULL) {
      WSASetLastError(WSA_NOT_ENOUGH_MEMORY);
      return;
  }
  perIoDataPtr->WSAOverlapped.Offset = 0;
  perIoDataPtr->WSAOverlapped.OffsetHigh = 0;
  perIoDataPtr->UserData = userData;

  WSABUF wsaBuf;
  wsaBuf.len = len;
  wsaBuf.buf = buf;

  res = WSASend(s, &wsaBuf, 1, NULL, 0, &(perIoDataPtr->WSAOverlapped), NULL);
  if (!res) {
    DWORD error;
    error = WSAGetLastError();
    if (error == 0) {
      WSASetLastError(WSA_IO_PENDING);
      return;
    }
    if (error != WSA_IO_PENDING) {
      HeapFree(GetProcessHeap(), 0, perIoDataPtr);
    }
  } else {
    // the send operation has completed synchronously, despite that, the
    // result is available in I/O completion port.
    WSASetLastError(WSA_IO_PENDING);
  }
}
