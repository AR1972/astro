/* forfile (filename, attr, routine) step through file names calling routine
 *	29-Oct-1986 mz	Use c-runtime instead of Z-alike
 *	24-Feb-1987 bw	Do findclose() to make FAPI happy.
 */

#include <dos.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>
#include "..\h\tools.h"

#include "messages.msg" // Localizable strings

/* internal functions */
int AllowXLinkError (void);
int Novell_Installed (void);
int isDecnet(void);

/* internal global variables */
char sz[MAXPATHLEN];

/**********************************************************************
 *
 * Determine the existance of DECNET Pathworks.
 *
 * INPUT : None
 *
 * OUTPUT: return version of Pathworks currently running. return zero
 *    if Decnet is not found
 *
 * DEPENDENCY: Only rung in real mode
 *
 **********************************************************************/
int isDecnet()
{
#define DECNET_DNP        0x6E
#define DECNET_GETVEC     0x35

   static int uResult=-1;
   register int i;
   char rgchMemBuf[10];
   union REGS inregs, outregs;
   struct SREGS segments;
   char far *lpReturnDos;

   if (-1 != uResult)
      return uResult;

   /*
    * this inturrupt only works on real mode.
    * Segment selector have to be determine in protected mode.
    * which I didn't implement (w-johny)
    */ 
   inregs.h.al = DECNET_DNP;
   inregs.h.ah = DECNET_GETVEC;
   segread( &segments );
   intdosx( &inregs, &outregs, &segments );

   FP_SEG (lpReturnDos) = segments.es;
   FP_OFF (lpReturnDos) = outregs.x.bx;

   for(i=0; i < 10; i++)  /* copy the memmoy content */
      rgchMemBuf[i] = *( lpReturnDos + (i-5) );

   for(i=0; i < 10; i++)
      /*
       * Search for the string "DNP"
       */
      if (
         ('D' == toupper( rgchMemBuf[i] ) )   && 
            ('N' == toupper( rgchMemBuf[i+1] ) ) && 
            ('P' == toupper( rgchMemBuf[i+2] ) )
         )
         break;  /* found the DNP signiture */

   if (10 == i)
      return ( uResult = 0 ); /* not found */
   else
      /*
       * version number is two bytes before the DNP signiture
       */
      return ( uResult = (
                ( (unsigned)(rgchMemBuf[i-2]) << 8 ) |
                ( (unsigned)(rgchMemBuf[i-1]) )
               ) );

} /* isDecnet() */


/*******************************************************************
*
* Function Novell_Installed
*
* This function checks for the existence of a Novell Netware
* network. It returns 1 if Netware is detected, 0 if not
*
*
* Local Variables Used
* --------------------
*
* inregs, outregs : Used to read and write the general purpose
*                   registers.
*
*******************************************************************/
int Novell_Installed (void)
{
  union REGS inregs, outregs;

  inregs.x.ax = 0x7A00;
  inregs.x.bx = 0x0000;
  int86(0x2F, &inregs, &outregs);

  if (outregs.h.al == 0xFF)
      /* Novell is installed */
      return (TRUE);
  else
      /* Novell is not installed */
      return (FALSE);
}

/*******************************************************************
*
* Function AllowXLinkError
*
* This function determines whether the cross link error should be allowed or
* skipped. Because we are determining a xlink by the existance of a '.' file
* in a subdir, and some networks do not return a '.' file, we only want to
* allow the XLink error when a '.' file is returned from find first.
*
*******************************************************************/
int AllowXLinkError (void)
{
    static int fAllowXLinkError=-1;

    if (-1 != fAllowXLinkError)
       return fAllowXLinkError;

    /* if novell is installed we don't allow the xlink error */
    if (Novell_Installed() == TRUE) {
       fAllowXLinkError = FALSE;
       return fAllowXLinkError;
    }

    /* if DEC is installed we don't allow the xlink error */
    if (isDecnet() != 0) {
       fAllowXLinkError = FALSE;
       return fAllowXLinkError;
    }

    /* if we get this far, we can allow the xlink error. */
    fAllowXLinkError = TRUE;
    return fAllowXLinkError;
}

/*******************************************************************
*
* Function forfile
*
* This function enumerates a file pattern using findfirst and findnext.
* If a file is found, a user function is called with the found file.
*
* Parameters used:
*      pat:	     file pattern to search for
*      attr:	     file to search for must have this file attribute
*      rtn:	     function to call with enumerated file
*
*******************************************************************/
forfile (pat, attr, rtn, ...)
char *pat;
int attr;
void (*rtn)(char *, struct findType *, va_list);
{
    struct findType *fbuf;
    char	    *buf;
    va_list	    ap;

    if ((fbuf = (struct findType *) (*tools_alloc) (sizeof (*fbuf))) == NULL)
	return FALSE;

    if (ffirst (pat, attr, fbuf)) {
	free ((char *) fbuf);
	return FALSE;
	}

    /* Verify subdir\*.* is a valid directory. First entry from fbuf must be a
     * '.' else we fail because it is a cross link; unless we are at the root.
     * If you call ffirst(*.*), the fbuf->name must be '.' else it is the root
     * or a cross link. A check must be made that we are not at the root before
     * giving the cross link error.
     */

    /* get the file extention from path */
    fileext (pat,sz);

    /* Check if extention is '*.*'. We can only check for a valid subdir if
     * we have called ffirst(path\*.*). This puts a '.' in the fbuf->name field.
     * Caller procs like delnode, append *.* to dir paths in order to recurse.
     * This allows us to check and make sure the sub dir is valid.
     */
    if (strcmp (sz,"*.*") == 0) {
	/* create a canonical path in sz[] */
	rootpath (pat, sz);

	/* strip off '*.*' */
	sz[((strlen(sz))-3)] = '\0';

	/* see if last char is path char ; if so remove it unless x:\ */
	if (fPathChr (sz[strlen(sz)-1])) {
	    /* if not at root */
	    if (strlen(sz) > 3) {
		/* get rid of trailing path char */
		sz[strlen(sz)-1] = '\0';
	    }
	}

	/* if the path we have is a directory and it is not the root directory
	 * then fbuf->name must be '.' otherwise we have a cross link.
	 */
	if ( ( ((char) getattr (sz)) == A_D) && (strlen(sz) > 3) ) {

	    /* The file must be a '.' else it is xlinked - unless
	     * we don't allow the cross link error
	     */
	    if ((fbuf->name[0] != '.') && (AllowXLinkError() == TRUE)) {
		    printf (pszMsgRunChkDsk);
		    exit(1);
	    }
	}

    }

    if ((buf = (*tools_alloc) (MAXPATHLEN)) == NULL) {
	findclose (fbuf);
	free ((char *) fbuf);
	return FALSE;
	}

    drive (pat, buf);
    path (pat, strend (buf));
    pat = strend (buf);

    do {
	/*  Assume the case correct form has been returned by ffirst/fnext
	 */
	strcpy (pat, PFT_FOUNDNAME(fbuf));
	va_start(ap, rtn);
	(*rtn) (buf, fbuf, ap);
	va_end(ap);
    } while (!fnext (fbuf));

    findclose (fbuf);

    free (buf);
    free ((char *) fbuf);

    return TRUE;
}
