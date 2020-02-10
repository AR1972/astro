/* fmove.c - fast copy between two file specs
 *
 *   5/10/86  daniel lipkie     Added frenameNO.  fmove uses frenameNO
 */

#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <malloc.h>
#include <dos.h>
#include "..\h\tools.h"
#include <errno.h>

#include "messages.msg"     // Localizable strings

// extern int errno;

#define IBUF    10240

/* frenameNO (newname, oldname) renames a file from the oldname to the
 * newname.  This interface parallels the C rename function in the
 * pre version 4.0 of C.  The rename function changed the order of the
 * params with version 4.0.  This interface isolates the change.
 * pre-4.0: rename (newname, oldname)
 * 4.0:     rename (oldname, newname);
 */
int frenameNO(strNew, strOld)
char *strNew, *strOld;
{
    rename(strOld, strNew);  /* assumes we are compiling with 4.0 lib */
    return(0);
}

/* fmove (source file, destination file) copies the source to the destination
 * preserving attributes and filetimes.  Returns NULL if OK or a char pointer
 * to the corresponding text of the error
 */
char *fmove (src,dst)
char *src, *dst;
{
    char *result;

    /*	Try a simple rename first
     */
    if (rename (src, dst) == 0)
	return NULL;

    /*	Try to fdelete the destination
     */
    if (fdelete (dst) > 2)
        // "Unable to delete destination";
        return szErrUnableToDelete;

    /*	Destination is gone.  See if we can simply rename again
     */
    if (rename (src, dst) == -1) {

	/* Since networks don't imitate DOS error code behavior exactly,
	 * we will assume we have failed because of a external device move.
	 * The fcopy() routine will return the correct errors of access violation
	 * or invalide filaname or directory name.
	 */

	 /*  Try a copy across devices
	  */
	    if ((result = fcopy (src, dst)) != NULL) {
		unlink(dst);
		return result;
	    }

	/*  Cross-device copy worked.  Must delete source
	 */

        /* Mark file *not* read-only, so we can delete it */
	setattr (src, A_NO);

	fdelete (src);
        }

    return NULL;
}
