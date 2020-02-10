#include <string.h>
#include "..\h\tools.h"

#define	REG	register

typedef unsigned short	word ;

union	Ptr
{
	word near *	w ;
	char near *	b ;
} ;

extern	unsigned short	_asegds ;
extern	unsigned short	_asizds ;


/*
 * int heapset ( int ) - set contents of heap free nodes
 *
 *      HEAPOK          - completed okay
 *      HEAPCANTFIND    - can't find heap
 *      HEAPBADNODE     - malformed node somewhere
 */

int     heapset ( fillbyte )
int     fillbyte;
{
REG     union   Ptr     cp ;
REG     word    size ;

        if ( ! _asegds )
            /*
            ** Not initialized yet
            */
            return ( HEAPOK );

	/*
	** Check the beginning of the heap
	** Consisting of a zero-byte unused entry and a two-byte used entry
	*/

	cp.w = (word near *) _asegds ;
	if ( * cp.w ++ != 1 || * cp.w ++ != 2 )
                return ( HEAPBADBEGIN );
	cp.b += 2 ;	/* Skip contents of dummy entry */

	/*
	** Scan through the Heap
	*/
	while ( ( size = * cp.w ++ & ~1 ) != 0xFFFE )
	{
		/*
		** Set unallocated blocks to the value "fillbyte"
		**	This call to memset() even sets the unused
		**	byte in an odd allocation, since _nmalloc()
		**	always allocates an even number of bytes.
		*/
		if ( cp.w [ -1 ] & 1 )
			memset ( cp.b , fillbyte , (size+1) & ~1 ) ;

		if ( _asizds < size || _asizds - size < (word) cp.w )
                        return ( HEAPBADNODE );
		cp.b += size ;
	}

        return ( HEAPOK );
}



/*
 * int heapchk ( void ) - does minimal consistency check on heap
 *
 *      HEAPOK          - okay
 *      HEAPBADBEGIN    - bad beginning, couldn't find initial header info
 *      HEAPBADNODE     - bad node, damaged heap
 */

int     heapchk ( )
{
REG     union   Ptr     cp ;
REG     word            size ;

        if ( ! _asegds )
            /*
            ** Not initialized yet
            */
            return ( HEAPOK );

        /*
        ** Check the beginning of the heap
        ** Consisting of a zero-byte unused entry and a two-byte used entry
        */
        cp.w = (word near *) _asegds ;
        if ( * cp.w ++ != 1 || * cp.w ++ != 2 )
                return HEAPBADBEGIN;
        cp.b += 2 ;     /* Skip contents of dummy entry */

        /*
        ** Scan through the Heap
        */
        while ( ( size = * cp.w ++ & ~1 ) != 0xFFFE )
        {
                if ( _asizds < size || _asizds - size < (word) cp.w )
                        return HEAPBADNODE;

                cp.b += size ;
        }
        return ( HEAPOK );
}


/*
 * int  heapinfo ( void ) - determines heap size and free space
 *      leaves info in lHeapSize lHeapFree
 *
 * long heapsize ( void ) - determines heap size
 *      if return >= 0 then size else error below
 *
 * long heapfree ( void ) - determines heap free
 *      if return >= 0 then size else error below
 *
 *      HEAPOK          - okay
 *      HEAPBADBEGIN    - bad beginning, couldn't find initial header info
 *      HEAPBADNODE     - bad node, damaged heap
 */

long    lHeapSize, lHeapFree;
long    lHeapLargest;

int     heapinfo ( )
{
REG	union	Ptr	cp ;
REG	word		size ;

        lHeapSize = lHeapFree = 0L;
        lHeapLargest = 0;

        if ( ! _asegds )
            /*
            ** Not initialized yet
            */
            return ( HEAPOK );

        /*
        ** Check the beginning of the heap
        ** Consisting of a zero-byte unused entry and a two-byte used entry
        */

        cp.w = (word near *) _asegds ;
        if ( * cp.w ++ != 1 || * cp.w ++ != 2 )
                return ( HEAPBADBEGIN );
        cp.b += 2 ;     /* Skip contents of dummy entry */

        /*
        ** Scan through the Heap
        */
        while ( ( size = * cp.w ++ & ~1 ) != 0xFFFE )
        {
                /*
                ** Add two for the header node
                */
                lHeapSize += size + 2;

		if ( cp.w [ -1 ] & 1 ) {
                        lHeapFree += size + 2;
		    if ( (long) size > lHeapLargest )
			lHeapLargest = (long) size;
		}

                if ( _asizds < size || _asizds - size < (word) cp.w )
                        return ( HEAPBADNODE );
                cp.b += size ;
        }

        return ( HEAPOK );
}


long    heapsize ( )
{
        int i = heapinfo ( );

        return ( ( i == HEAPOK ) ? lHeapSize : i );
}


long    heapfree ( )
{
        int i = heapinfo ( );

        return ( ( i == HEAPOK ) ? lHeapFree : i );
}
