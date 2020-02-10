/*
 *  18-Feb-1987  bw Fix off-by-one errors.
 *  22-Dec-1987  bw Fix strncrlf hang, redo buffer handling.
 *		    Pay attention to max argument
 *---------------------------------------------------------------------------
 *
 *  pgetl.c - do fgetl() from a pipe without blocking. In particular, do this:
 *
 *	    If ( Buffer is empty )
 *		Fill Buffer
 *	    If ( 0 length request ) return ( there is more data )?
 *	    { - 1 allows for '\0' byte in target }
 *	    set max to min(max - 1, count of valid bytes);
 *	    If ( "full line" is buffered )
 *		Transfer Full line.
 *		Clean up and go
 *	    Move partial line to base of buffer
 *	    Fill rest of buffer
 *	    If ( we have a "full line" now ) transfer it
 *	    Else
 *		return	FALSE
 *
 *	A "full line" is either LF terminated or len bytes long, whichever
 *	comes first.
 *
 *	The Fill Buffer method is this:
 *
 *	    Set Read Semaphore
 *	    Read Asynch
 *	    Wait on Read Semaphore with Timeout ( ~.1 seconds )
 *
 *  Note that if a 0 length read is requested, the effect is to check for
 *  unread data.
 *
 *  Note that if the destination buffer is NULL, one line is read and flushed.
*/
#include <os2.h>
#include "..\h\tools.h"


static int FilBuf( int, char *, int );


pgetl ( buf, max, fh )
char *buf;
int max;
int fh;
{
    static char buffer[BUFSIZ];
    static int cnt = 0; 	    /* Valid bytes remaining in buffer	     */
    static char * pValid = buffer;  /* First valid byte 		     */
    char * eos;


	if ( ! cnt )
	    if ( ! (cnt = FilBuf( fh, buffer, BUFSIZ )) ) return FALSE;
	    else pValid = buffer;

	/* There is now at least 1 character in buffer */

	if ( ! max-- ) return cnt;    /* max == 0 means ret unread chars    */
				      /* dec max for '\0' byte		    */
	max = min( max , BUFSIZ );

	/* Check for full line already present */
	if ( (eos = strncrlfend(pValid, cnt))  ||  max <= cnt ) goto full_line;

	/* Move partial line down to base of buffer */
	Move ((char far *)pValid, (char far *)buffer, cnt);
	pValid = buffer;

	cnt += FilBuf( fh, buffer + cnt, BUFSIZ - cnt );

	if ( (eos = strncrlfend(buffer, cnt))  ||  max <= cnt ) goto full_line;

	return FALSE;


full_line:
	{
	  int linelen, deadchars;

	    if ((linelen = eos ? eos - pValid : max) > max)
		linelen = max;


	    if ( buf ) /* Give user their line, if they want it */
	    {
		Move( (char far *)pValid, (char far *)buf, linelen );
		buf[linelen] = '\0';
	    }


	    deadchars = linelen + (eos != 0) + (eos && *eos == '\r');
	    cnt -= deadchars;

	    /* Now remove it from my buffer */
	    pValid += deadchars;

	    return TRUE;
	}
}


/*
 *  strncrlfend - Return a pointer to the first of:
 *
 *	    The CR in a CR/LF pair
 *	    A lone LF
 *	    The 0 byte.
 *
 *  or return NULL if none of these can be found in the first 'max' characters.
 *
 *  Note: If the LF in a CR/LF pair is the 'max' + 1st character, return NULL.
*/
char * strncrlfend( str, max )
char * str;
int max;
{
    int i;

	for (i = 0; i < max; i++)
	    if (!str[i] || str[i] == '\n')
		    return str + i - (i && str[i-1] == '\r');

	return NULL;
}


/*
 *  FilBuf - read from handle, assuming that the read may block.  If the block
 *  last more than .1 seconds, return 0 bytes read.  In this case, assume that
 *  the caller is willing to have 'buf' filled at some random later time.
*/
static int FilBuf( fh, buf, amt )
int fh;
char * buf;
int amt;
{
    static unsigned long semRead;
    static unsigned BytesRead, retC;
    static flagType pending = FALSE;

	if ( ! pending )
	{
	    DosSemSet ( (HSEM)&semRead );

	    DosReadAsync (   fh,
			    (unsigned long far *)&semRead,
			    (PUSHORT)&retC,
			    (char far *)buf,
			    amt,
			    (PUSHORT)&BytesRead       );
	}

	if ( DosSemWait ( (HSEM)&semRead, 100L ) )
	{
	    pending = TRUE;
	    return 0;
	}
	else
	{
	    pending = FALSE;
	    return BytesRead;
	}
}
