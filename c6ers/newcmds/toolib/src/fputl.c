/*  fputl.c - write a CRLF line to a file
 */

#include "..\h\tools.h"

/* writes a line to file (with trailing CRLFs) from buf, return <> 0 if
 * writes fail
 */
fputl (buf, len, fh)
char *buf;
int len;
FILE *fh;
{
    return (fwrite (buf, 1, len, fh) != (unsigned) len || fputs ("\r\n", fh) == EOF) ? EOF : 0;
}
