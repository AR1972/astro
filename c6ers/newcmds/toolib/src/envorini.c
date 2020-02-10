#include <stdlib.h>
#include <string.h>
#include "..\h\tools.h"


/* getenvini - get environment or tools.ini,
 * looks for pstrTag_pstrEnv as an environment variable,
 *   if found return copy of its value
 * if not found in env look for pstrEnv in pstrTag sect of $USER:\TOOLS.INI,
 *   if found return copy of its value
 * else return NULL.
 *
 * Returned value should be deallocated
 *
 * N.B. even if there is an entry in TOOLS.INI, non-NULL is returned iff
 * there are non-white space characters following the '=', e.g.
 *
 *  [tag]
 *      entry1=
 *      entry2=nonnull
 *
 *  getenvini ("entry1", "tag");  will return NULL
 *  getenvini ("entry2", "tag");  will return copy of "nonnull"
 *
 */

char *getenvini(pstrEnv, pstrTag)
char *pstrEnv;
char *pstrTag;
{
    char *p;
    char *pstrTmp;

    if (!(pstrTmp = (*tools_alloc) (strlen (pstrEnv) + strlen (pstrTag) + 2)))
        return NULL;
    strcpy (pstrTmp, pstrTag);
    strcat (pstrTmp, "_");
    strcat (pstrTmp, pstrEnv);
    pstrTmp = strupr (pstrTmp); /* getenv requires upper case */
    if ((p = getenv (pstrTmp))) {
        /* found in env so do NOT look into switch file */
        if (*(p = strbskip (p, "\t ")))
            /* found non-white space char so return non-NULL */
            p = strdup (p);
        else
            /* rhs has only white space char */
            p = NULL;
        }
    else
        p = swfind (pstrEnv, NULL, pstrTag); /* swfind does an alloc if found */
    free (pstrTmp);
    return p;
 }
