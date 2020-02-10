/* delnode - removes a node and its descendants */

#include "..\h\tools.h"
#include <string.h>
#include <direct.h>

static void fDoDel (
    char		*name,
    struct findType	*pBuf,
    va_list		ap
    )
{
    char *p;

    /* if it is a file, attempt to delete it */
    if (!TESTFLAG(PFT_FOUNDATTR(pBuf), A_D)) {
	/* if file is read-only, make it writable */
	if (TESTFLAG(PFT_FOUNDATTR(pBuf), A_RO))
	    if (setattr (name, PFT_FOUNDATTR(pBuf) & ~A_RO))
		return;
	unlink (name);
	}
    else if (strcmp (PFT_FOUNDNAME(pBuf), ".") &&
	     strcmp (PFT_FOUNDNAME(pBuf), "..")) {
	/* clear out subdir first */
	p = strend (name);
	pathcat (name, "*.*");
	forfile (name, A_H | A_S | A_D, fDoDel);
	*p = 0;
	rmdir (name);
	}
    return;
    ap;     //to access, as we don't use the vararg list
}

flagType delnode (name)
char *name;
{
    return (flagType) forfile (name, A_H | A_S | A_D, fDoDel);
}
