/* snoop through the environment for a name and return the value end of things
 *	29-Oct-1986 mz	Use c-runtime instead of Z-alike
 */

#include <stdlib.h>
#include "..\h\tools.h"
#include <malloc.h>
#include <string.h>

/* Map will scan a buffer for $(name) and attempt to replace it with
 * text from the environment
 */
void mapenv (char *src, char *dst)
{
    char *buf, *env;
    char *p, *p1, *p2, *p3;

    if ((buf = (*tools_alloc) (MAXLINELEN)) != NULL) {
	if ((env = (*tools_alloc) (MAXLINELEN)) != NULL) {
	    p = src;
	    p1 = buf;
	    while (*p != 0) {
		if (strpre ("$(", p) && ((p2 = strchr (p+2, ')')) != NULL)) {
		    Move ((char far *)(p+2), (char far *)env, p2-(p+2));
		    env[p2-(p+2)] = '\0';
		    strupr (env);
		    if ((p3 = getenv (env)) != NULL) {
			strcpy (p1, p3);
			p1 += strlen (p3);
			p = p2+1;
			continue;
			}
		    }
		*p1++ = *p++;
		}
	    *p1++ = '\0';
	    strcpy (dst, buf);
	    free (env);
	    }
	free (buf);
	}
}
