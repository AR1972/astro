/* fexpunge.c - remove all deleted objects from the index
 *
 *  HISTORY:
 *
 *	??-???-???? ??	 Original Version
 *	06-Sep-1988 bw	 Issue error is directory removal fails
 *	20-Dec-1989 SB	 Change for new Index file format, added NOTES
 *
 * NOTES:
 *  The old-format index file was composed of elements of size RM_RECLEN having
 *  the following syntax :-
 *
 *     <element> := <valid-element> | <deleted-element>
 *     <valid-element> := <8.3filename> <padding>
 *     <deleted-element> := <padding>
 *	   where,
 *	       <padding> is series of (RM_RECLEN - sizeof(8.3filename) 0x00's
 *
 *  If the first RM_RECLEN bytes of the index file match the new index file
 *  header then the index file has new-format.
 *
 *  The new-format index file is composed of elements of size (n * RM_RECLEN)
 *  having the following syntax :-
 *
 *	<header>	  := <0x00> <magic> <version> <0x00> <first-padding>
 *	<valid-element>   := <longfilename> <padding>
 *	<deleted-element> := <padding>
 *	    where,
 *		<padding> is a series is 0x00 to round off to RM_RECLEN length
 *		<magic> is RM_MAGIC (currently IX)
 *		<version> is RM_VERSION (currently 1.01)
 *  When <longfilename> is a multiple of RM_RECLEN then an extra padding record
 *  is added to make it a NULL terminated string.
 *
 */

#if defined (OS2)
#include <os2.h>
#endif

#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#include "..\h\tools.h"
#include "..\h\rm.h"
#include <string.h>
#include <time.h>
#include <direct.h>
#include <malloc.h>

/* we open the index corresponding to the named directory and release all
 * the deleted files present.  At the end, we remove the index and the deleted
 * directory */
long fexpunge (pDir, list)
char *pDir;
FILE *list;
{
    int fhidx;
    char *dir;				/* deleted dir */
    char *szRec;			/* name of file deleted */
    char *idx;				/* name of index */
    char *file;
    long totbytes;
    struct stat statbuf;

    totbytes = 0L;
    dir = idx = file = NULL;
    if ((dir = (*tools_alloc) (MAXPATHLEN)) == NULL ||
	    (idx = (*tools_alloc) (MAXPATHLEN)) == NULL ||
	    (file = (*tools_alloc) (MAXPATHLEN)) == NULL ||
	    (szRec = (*tools_alloc) (MAXPATHLEN)) == NULL) {
	if (list)
	    fprintf (list, "Unable to allocate internal storage\n");
	goto done;
    }

    /* generate deleted directory name from dir */
    strcpy (dir, pDir);
    pathcat (dir, RM_DIR);
    /* generate index name from deleted directory */
    strcpy (idx, dir);
    pathcat (idx, RM_IDX);
    /* try to open index.  If it fails, no problem */
    if ((fhidx = open (idx, O_RDWR | O_BINARY)) != -1) {
	if (list)
	    fprintf (list, "Expunging files in %s\n", pDir);

	readIdxRec (fhidx, szRec);
	if (fIdxHdr (szRec))
	    if (!readNewIdxRec (fhidx, szRec, MAXPATHLEN))
		goto done;
	do {
	    /* For each file that was RMed and not UNDELed */
	    if (szRec[0] != '\0') {
		/* The name starts earlier than current position in the index
		 * file. The deleted file index is derived from the current
		 * offset and the length of the string.
		 */
		sprintf (file, "%s\\deleted.%03x", dir, (lseek (fhidx, 0L, SEEK_CUR)
			 - strlen (szRec)) / RM_RECLEN);
		if (stat (file, &statbuf) == -1) {
		    if (list)
			fprintf (list, " (%s - %s)\n", file, error ());
		}
		else {
		    unlink (file);
		    totbytes += statbuf.st_size;
		    if (list) {
			char *pTime = ctime (&statbuf.st_mtime);

			/* ctime() returns a string which has a \n at
			 * fixed offset of 24. [ANSI draft]. We don't need
			 * it because we put the File Name before \n
			 */
			*(pTime + 24) = '\0';
			upd (dir, szRec, file);
			fprintf (list, "%8ld %s  %s\n", statbuf.st_size, pTime,
				 file);
			fflush (list);
		    }
		}
	    }
	} while (readNewIdxRec (fhidx, szRec, MAXPATHLEN));

	close (fhidx);
	unlink (idx);
	if (rmdir (dir))
	    fprintf (list, "ERROR: Unable to remove directory %s - %s\n", dir, error ());
	if (list)
	    fprintf (list, "%ld bytes freed\n", totbytes);
    }
    else
	if (!stat (dir, &statbuf))
	    fprintf (list, "Warning: Cannot open %s - %s\n", idx, error ());
done:
    if (dir)
	free (dir);
    if (idx)
	free (idx);
    if (file)
	free (file);
    if (szRec)
	free (szRec);
    return totbytes;
}
