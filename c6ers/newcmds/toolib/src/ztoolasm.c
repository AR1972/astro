/*++

Copyright (c) 1990  Microsoft Corporation

Module Name:

    move.c

Abstract:

    Move and file routines that were previously in asm
    Done to ease porting of utilities (wzmail)

Author:

    Dave Thompson (Daveth) 7 May-1990


Revision History:


--*/

#include    <stdio.h>
#include    <io.h>
#include    <windows.h>
#include    <tools.h>

#include <memory.h>
#include <string.h>

#if 0
/* Move and Fill are implemented in NT as macros to their respective
 *  C runtime library functions.  They are left here for completeness.
 *	ErichS
 */

//
//  Move:  move count bytes src -> dst
//

void
Move (
    void * src,
    void * dst,
    unsigned int count)
    {

    memmove(dst, src, count);
}

//
//  Fill:  fill count bytes of dst with value
//

void
Fill (
    char * dst,
    char value,
    unsigned int count)
    {

    memset(dst, (int) value, count);
}
#endif

//
//  strpre - return -1 if s1 is a prefix of s2 - case insensitive
//

flagType
strpre (
    char * s1,
    char * s2)
    {
    if ( _strnicmp ( s1, s2, strlen(s1)) == 0 )
	return -1;
    else
	return 0;

}

/*** max - maximum function (non-macro to avoid double arg evaluation)
 */
int
max (int x, int y)
{
    return (x > y) ? x : y ;
}

/*** min - minimum function (non-macro to avoid double arg evaluation)
 */
int
min (int x, int y)
{
    return (x < y) ? x : y ;
}

/*** z_handle - gets OS handle from standard RTL handle
 *
 *  The NT runtime libraries hand back library handles, not file handles.
 *  This routine uses the _get_osfhandle() function to get
 *  a Win32 file handle for internal use, and assumes that any handle that
 *  is not accepted by this routine is valid as is.
 *
 *  Primary assumption: that the set of OS handles is disjoint from the set
 *  of CRT file handles.  The reason we assume this is the array of CRT
 *  handles is at most 256, the 64K at each end of user space is not mapped
 *  to catch runaway errors, and the OS handle is a pointer.
 *
 */
HANDLE
z_handle (int crtfh)
{
    HANDLE  ntfh;

    if ((ntfh = (HANDLE) _get_osfhandle(crtfh)) == (HANDLE) -1)
	return (HANDLE) crtfh;
    else
	return ntfh;
}
