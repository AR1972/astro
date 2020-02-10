/*  root.c - generate a path to a file from the root
 *
 *  Modifications:
 *
 *	30-Jul-1986 mz	Add sensitivity for network names
 *	29-Oct-1986 mz	Use c-runtime instead of Z-alike
 *      18-Oct-1990 w-barry  Fixed case where '..' is passed in.  Function
 *                           now returns 'x:\' instead of 'x:' where x is a
 *                           drive letter.
 */

#include "..\h\tools.h"
#include <string.h>
#include <ctype.h>


/*  rootpath - construct a path from the root to the specified file
 *  correctly handling ., .. and current directory/drive references.
 *
 *  src 	source path for input
 *  dst 	destination buffer
 *  returns	TRUE if error detected
 */
rootpath (src, dst)
char *src, *dst;
{
#if defined(NT)

    LPSTR FilePart;

    return (!GetFullPathName( (LPSTR) src,
                              (DWORD) MAX_PATH,
                              (LPSTR) dst,
			      &FilePart ));

#else
    char *beg = dst;
    register char *p, *p1;
    char d;

    p = src;
    if (src[0] && src[1] == ':') {
	*p = (char) tolower (*p);
	p += 2;
	}
    if (!fPathChr (p[0]) || !fPathChr (p[1])) {
	d = 0;
	if (*src && src[1] == ':') {
	    d = (char) (toupper (*src) - 'A' + 1);
	    src += 2;
	    }
	if (curdir (dst, d))
	    /* invalid drive */
	    return TRUE;
	/* dst = cur dir on drive d, so now bump across d: */
	dst += 2;
	if (*src == '.' && (src[1] == '\0' ||
			    (fPathChr (src[1]) && src[2] == '\0')))
	    /* src == "." or ".\", i.e cur dir, we are done */
	    return FALSE;
	}
    else
	src = p;

    if (fPathChr (*src))
	strcpy (dst, src);
    else {
	d = dst[strlen(dst)-1];
	if (!fPathChr (d))
	    strcat (dst, PSEPSTR);
	strcat (dst, src);
	}
    p1 = src = dst;
    while (*src) {
	p1 = strbscan (p=p1, "\\/");
	d = *p1;
	*p1++ = 0;
	if (!strcmp (p, ".")) {
	    do {
		if (--dst < src)
		    return TRUE;
	    } while (!fPathChr (*dst));
	    }
	else
	if (!strcmp (p, "..")) {
	    do {
		if (--dst < src)
		    return TRUE;
	    } while (!fPathChr (*dst));
	    do {
		if (--dst < src)
		    return TRUE;
	    } while (!fPathChr (*dst));
	    }
	else {
	    strcpy (dst, p);
	    dst += strlen (dst);
	    }
	if (fPathChr (d))
	    d = PSEPCHR;
	if (!(*dst++ = d))
	    break;
	}
    // If '..' was passed in one level above the root dir, beg at this point
    // will contain '<drive_letter>:' which will not be treated as a valid
    // directory - To account for this, add a '\' character.
    //
    // Note: This case does not occur if '<drive_letter>:' is passed in.
    //
    if( strlen( beg ) == 2 ) {
        *( beg + 2 ) = PSEPCHR;
        *( beg + 3 ) = '\0';
    }
    pname (beg);
    return FALSE;
#endif
}
