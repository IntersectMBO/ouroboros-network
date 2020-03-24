#include <winsock2.h>
#include <windows.h>
#include <HsFFI.h>

// each OVERLAPPED struct is wrapped in IO_DATA;  It is important to keep
// OVERLAPPED as the first member of the record.  This allows us to cast
// an OVERLAPPED pointer to _IODATA and recover the second member..
typedef struct _IODATA {
    OVERLAPPED  iodOverlapped;
    HsStablePtr iodData;
} IODATA;


// As for _IODATA: it is vital to keep WSAOVERLAPPED as the first member of
// the struct.
typedef struct _WSAIODATA {
    WSAOVERLAPPED  iodOverlapped;
    HsStablePtr    iodData;
} WSAIODATA;
