/* error.c - return text of error corresponding to the most recent DOS error
 *
 *  Modifications:
 *
 *	05-Jul-1989 bw	    Use MAXPATHLEN
 *      01-Jan-1993 jh      Localization of error messages
 */


#include <stdlib.h>
#define  UNKNOWN    37

char *sys_errorlist[] =
{
#include "syserr.msg"    /* -> IPG, localize syserr.msg */
};

int system_nerr = sizeof( sys_errorlist ) / sizeof( sys_errorlist[ 0 ] ) - 1;

char *error ()
{
    if (errno < 0 || errno >= system_nerr)
	return sys_errorlist[UNKNOWN];
    else
        return sys_errorlist[errno];
}
