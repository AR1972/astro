/*  fgetl.c - expand tabs and return lines w/o separators
 *
 *  Modifications
 *	05-Aug-1988 mz	Make exact length lines work correctly
 *
 */

#include "..\h\tools.h"

/* returns line from file (no CRLFs); returns NULL if EOF */
fgetl (buf, len, fh)
char *buf;
int len;
FILE *fh;
{
    register int c;
    register char *p;

    /* remember NUL at end */
    len--;
    p = buf;
    while (TRUE) {
        c = fgetc (fh);
	if (c == EOF || c == '\n')
	    break;
	if (c != '\r')
	    if (len == 0) {
		ungetc (c, fh);
		break;
		}
	    else
	    if (c != '\t') {
		*p++ = (char) c;
		len--;
		}
	    else {
		c = min (8 - ((p-buf) & 0x0007), len);
		Fill (p, ' ', c);
		p += c;
		len -= c;
		}
	}
    *p = 0;
    return ! ( (c == EOF) && (p == buf) );
}
