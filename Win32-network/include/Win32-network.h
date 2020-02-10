#include <windows.h>
#include <HsFFI.h>

// each OVERLAPPED struct is wrapped in IO_DATA, we use CONTAINING_RECORD
// windows kernel macro to retrieve the pointer to it from a pointer to
// OVERLAPPED struct.
typedef struct _IODATA {
    OVERLAPPED  iodOverlapped;
    HsStablePtr iodData;
} IODATA;

typedef struct _GQCSRESULT {
    BOOL    gqcsResult;
    BOOL    gqcsOverlappedIsNull;
    BOOL    gqcsCompletionKey;
    IODATA *gqcsIODataPtr;
} GQCSRESULT;
