/* TS = NONE */
/*
**  OS2TYPES.H  --  Header containing types for Compression/Decompression
**    modules in Setup Toolkits.
*/


#ifdef WIN_VER
#undef min
#undef max
#undef NULL
#include "windows.h"
#endif /* WIN_VER */

#ifdef OS2_VER

#define INCL_NOPM
#define INCL_DOSDEVICES
#include <os2.h>

#else /* not OS2_VER */

/* C 5.1 doesn't have _fmemset--this one is compatible */

void _far * _far lcb_fmemset(void _far *dst, int c, size_t count);
#define _fmemset(a,b,c) lcb_fmemset(a,b,c)

typedef  unsigned char  BYTE;
#define  BOOL  int
#undef TRUE
#define  TRUE  ~0
#undef LONG
typedef  long           LONG;
#define  FALSE  0
#ifndef NULL
#define  NULL   0
#endif
typedef  int            SHORT;
typedef  unsigned int   USHORT;
typedef  unsigned long  ULONG;
typedef  char           CHAR;
typedef  unsigned char  UCHAR;

#endif /* OS2_VER */

#define  NIL   (-1L)
typedef  CHAR *         SZ;

typedef int (far *PFNWFROMW)(int);



#ifdef C700
#define  open          _open
#define  close         _close
#define  read          _read
#define  write         _write
#define  eof           _eof
#define  lseek         _lseek
#define  tell          _tell
#define  filelength    _filelength
#define  stat          _stat
#define  chsize        _chsize
#define  chmod         _chmod
#define  access        _access
#define  mkdir         _mkdir
#define  stricmp       _stricmp
#define  strdup        _strdup
#endif /* C700 */
