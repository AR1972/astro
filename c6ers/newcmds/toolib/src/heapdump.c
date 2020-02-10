#include <ctype.h>
#include "..\h\tools.h"

#define BUFSIZE 40
#define	REG	register

typedef unsigned short	word ;

union	Ptr
{
	word near *	w ;
	char near *	b ;
} ;

extern	unsigned short	_asegds ;
extern	unsigned short	_asizds ;
extern  long            lHeapSize;

/*
**     heapdump - dump  heap info to fp
**
**     output to fp:
**         _asegds      unsigned
**         _asizds      unsigned
**         lHeapSize    long        if == -1 then heap error
**         lHeapFree    long        if lHeapSize == -1 then this is heap
**                                     error code
**         lHeapLargest long        if lHeapSize == -1 then zero
**
**     returns
**         zero    - info written to fp
**         nonzero - error writing to fp
**
**     Note: APPENDS to fp
**
**
*/
int     heapdump ( fp, iFormat )
FILE    *fp;
int     iFormat;
{
REG     union   Ptr     cp ;
        word    size;
        int     iMax;
        int     i;
        long    lSize;
        char    buf [ BUFSIZE + 1 ];
        char    *p, *q;
        char    ch;

        heapinfo ( );
        if ( lHeapSize ) {
            fprintf ( fp, "%ld %d\n", lHeapSize + 6, iFormat);
            /*
            **  skip over signature word
            */
            cp.w = (word near *) _asegds + 1;
            if ( iFormat ) {
                while ( ( size = *cp.w & ~1 ) != 0xFFFE ) {
                    fprintf ( fp, "%6u %6u%s", cp.w, *cp.w,
                      ( *cp.w & 1 ? "*  " : "   " ) );
                    p = buf;
                    q = cp.b + 2;
                    iMax = ( size < BUFSIZE ? size : BUFSIZE );
                    for ( i = 0; i < iMax; i++ )
			if ((ch = *q++) && 0x20 <= ch && ch < 0x7f) *p++ = ch ;
			else *p++ = '.' ;
                    *p = '\0';
                    fprintf ( fp, "%s\n", buf );
                    cp.b += size + 2;
                }
            }
            else {
                /*
                **  lHeapSize is the number of bytes in heap exclusive of the
                **  4 that are the initial dummy node and 2 that are the
                **  end marker.  So we add 6 to write starting with dummy
                **  node through the end marker
                */
                lSize = lHeapSize + 6;
                while ( lSize ) {
                    i = ( (long) 512 < lSize ? 512 : (int) lSize );
                    if ( fwrite ( cp.b, i, 1, fp ) != 1 )
                        return 1;
                    lSize -= i;
                    cp.b += i;
                }
                fprintf ( fp, "\n" );
            }
        }
        else
            fprintf ( fp, "0 %d\n", lHeapSize + 6, iFormat);

        fprintf ( fp, "<EndOfHeap>\n" );
        return 0;
 }
