/*  init.c - routines for managing TOOLS.INI-like files
 *
 *  Modifications
 *	15-Jul-87   danl    Start of section is <optionalwhitespace>[...]
 *	05-Aug-1988 mz	    Use buffer equate for swgoto.
 *	05-Jul-1989 bw	    Use MAXPATHLEN
 *
 */

#include <string.h>
#include <limits.h>
#include "..\h\tools.h"
#include "..\h\strpfx.h"

#define BUFLEN 256

static flagType fMatchMark (char *, char *);
static char *space = "\t ";

/*  fMatchMark - see if a tag in in a mark set
 *
 *  We treat the mark set as a collection of whitespace-separated names
 *
 *  pMark	pointer to mark set (contents modified)
 *  pTag	tag to find
 *
 *  returns	TRUE if match was found
 */
static flagType fMatchMark (pMark, pTag)
char *pMark, *pTag;
{
    char *p, c;

    while (*pMark != 0) {
	pMark = strbscan (p = strbskip (pMark, space), space);
	c = *pMark;
	*pMark = 0;
	if (!stricmp (p, pTag))
	    return TRUE;
	*pMark = c;
	}
    return FALSE;
}


/* returns pointer to tag if line is marker; NULL otherwise */
char *ismark (buf)
register char *buf;
{
    register char *p;

    buf = strbskip (buf, space);
    if (*buf++ == '[')
	if (*(p = strchr (buf, ']')) != '\0') {
            *p = 0;
            return buf;
            }
    return NULL;
}

flagType swgoto (fh, tag)
FILE *fh;
char *tag;
{
    char buf[BUFLEN];

    if (fh) {
	while (fgetl (buf, BUFLEN, fh)) {
            register char *p;

            if ((p = ismark (buf)) != NULL) {
		if (fMatchMark (p, tag))
                    return TRUE;
                }
            }
        }
    return FALSE;
}

/* returns fh of file if tag found, else NULL */
FILE *swopen (file, tag)
char *file, *tag;
{
    FILE *fh;
    char buf[MAXPATHLEN];

    if ((fh = pathopen (file, buf, "rb")) == NULL)
        return NULL;

    if (swgoto (fh, tag))
	return fh;

    fclose (fh);
    return NULL;
}

/* close a switch file */
swclose (fh)
FILE *fh;
{
    return fclose (fh);
}

/* read a switch line; return FALSE if end of file.  Skips leading spaces
 * and lines that begin with ; and blank lines
 */
int swread (buf, len, fh)
char *buf;
int len;
FILE *fh;
{
    register char *p;

    while (fgetl (buf, len, fh))
        if (ismark (buf) != NULL)
	    break;
        else {
	    p = strbskip (buf, space);
            if (*p != 0 && *p != ';') {
                strcpy (buf, p);
		return TRUE;
            }
        }
    return FALSE;
}

/* Reads lines from the file fh looking in the section pstrTag for one with
 * "entry=" and if there are non-white space characters following the '='
 * a copy of these characters is returned else NULL is returned.
 *
 * If fh == 0 then the file $INIT:\TOOLS.INI is used as the switch file
 *
 * If a non-NULL value is returned, it should eventually be free'd.
 *
 * N.B. if there are only white space characters, space and tab, following
 * the '=', NULL is returned
 *
 */
char *swfind (pstrEntry, fh, pstrTag)
char *pstrEntry;
FILE *fh;
char *pstrTag;
{
    char *p;
    char *q;
    FILE *fhIn = fh;
    char buf[BUFLEN];

    q = NULL;
    if (fh || (fh = swopen ("$INIT:\\TOOLS.INI", pstrTag))) {
	while (swread (buf, BUFLEN, fh) && !ismark(buf) ) {
	    if ((p = strchr (buf, '=')) != NULL) {
                *p++ = '\0';
		if (!strcmpis (buf, pstrEntry)) {
		    if (*(p = strbskip (p, space)))
                        q = strdup (p);
                    break;
                    }
                }
            }
        }
    if (!fhIn)
        swclose (fh);
    return q;
}

/* Returns a ptr to the separator in buf & fills in sep */
static char *FindSep(char *buf, char **seps, char **sep)
{
    char *szBrk;
    unsigned i, index, len = 0;

    index = UINT_MAX;
    *sep = NULL;

    // find longest leftmost separator from given list in buf
    while (NULL != *seps)
    {
	szBrk = strstr(buf, *seps);
	if (szBrk )
	{
	   i = szBrk - buf;
	   if (i < index || (i == index && len < strlen(*seps)))
	   {
	       index = i;
	       *sep = *seps;
	       len = strlen(*sep);
	   }
	}
	seps++;
    }

    if (*sep)
	return (buf + index);
    else
	return NULL;
}

/*
 * Parse the given buffer. Return the lhs, rhs and the actual separator.
 *
 * buf	switch line contents (!!!contents modified!!!)
 * seps null-terminated list of allowed separators
 * lhs  ptr to parameter (left-hand side value)
 * rhs  ptr to value (value to the right of the separator)
 * sep	actual separator found in buf
 *
 * Returns TRUE if successful else FALSE.
 */

int swparse(char *buf, char *seps[], char **lhs, char **rhs, char **sep)
{
    char *szBrk = NULL;

    *lhs = *rhs = *sep = NULL;
    if (NULL == buf || '\0' == *buf || (NULL != ismark(buf)))
	return FALSE;

    buf += strspn(buf, space);
    if (';' == *buf)
	return FALSE;

    // sep
    szBrk = FindSep(buf, seps, sep);

    // lhs
    *lhs = buf;

    // rhs
    if (szBrk)
    {
	// place 0 at sep 
	*szBrk = '\0';
	szBrk += strspn(szBrk += strlen(*sep), space);
	*rhs = szBrk;
    }

    return TRUE;
}

static flagType fMatchPat (char *pMark, char *pTag)
{
    char *p, c;

    while (*pMark != 0) {
	pMark = strbscan (p = strbskip (pMark, space), space);
	c = *pMark;
	*pMark = 0;
	if (striprefix (pTag, p))
	    return TRUE;
	*pMark = c;
	}
    return FALSE;
}

/*
 * Variation of goto where the tag is of the form <string>*
 *
 * fh  file handle
 * tag tag to match
 * 
 * Retruns ptr to match found if successful else NULL
 */

char *swmatch(FILE *fh, char *tag)
{
    char buf[BUFLEN];

    if (fh) {
	while (fgetl (buf, BUFLEN, fh)) {
            register char *p;

            if ((p = ismark (buf)) != NULL) {
		if (fMatchPat (p, tag))
                    return (strdup(p));
                }
            }
        }
    return NULL;
}
