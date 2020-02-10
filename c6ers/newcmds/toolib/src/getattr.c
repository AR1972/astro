/* getattr - return attribute for a directory entry
 *
 *	09-Dec-1986 bw	Added DOS 5 support
 *	30-Oct-1987 bw	Change 'DOS5' to 'OS2'
 *
*/

#if defined(OS2)
#include <os2.h>
#include <string.h>
#else
#include <dos.h>
#endif

#include "..\h\tools.h"

int getattr (pname)
char *pname;
{
#if defined(NT) || defined(OS2)
    {	int CurAttr;

#if defined(NT)
	if ((CurAttr = GetFileAttributes(pname)) != -1)
#elif defined(OS2)
	if (DosQFileMode ((char far *)pname, (PUSHORT)&CurAttr , 0L) == 0)
#endif
	    return CurAttr;
    }

    if (*strbscan (pname, "?*") != 0)
	return -1;

    {	FIND fnd;

	if (!ffirst (pname, A_ALL, &fnd)) {
	    findclose (&fnd);
	    return fnd.attr;
	    }
    }

    if (strlen (pname) == 2 && fPathChr (pname[0]) && fPathChr (pname[1]))
	return A_D;

    return -1;
#elif defined(DOS)
    union REGS regs;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
    struct SREGS sregs;
#endif

    // Get File Attributes. ds:dx are correctly loaded depending
    // upon the memory model.

    regs.x.ax = 0x4300;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
    segread( &sregs);
    sregs.ds = FP_SEG(pname);
    regs.x.dx = FP_OFF(pname);
    intdosx(&regs, &regs, &sregs);
#else
    regs.x.dx = (unsigned) pname;
    intdos (&regs, &regs);
#endif
    return regs.x.cflag ? -1 : regs.x.cx;
#endif
}
 
