/* setattr - set attribute for a directory entry */

#if defined(NT)
#include <windows.h>
#elif defined(OS2)
#include <os2.h>
#define RESV_BITS 0x3F	/* Bits not in this mask are documented as "reserved */
#elif defined(DOS)
#include <dos.h>
#endif


int setattr (pname, attr)
char *pname;
int attr;
{
#if defined(NT)

    return SetFileAttributes(pname, attr) ? 0 : GetLastError() ;

#elif defined(OS2)

    return DosSetFileMode( (char far *)pname , attr & RESV_BITS , 0L );

#else

    union REGS regs;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
    struct SREGS sregs;
#endif

    // Set File Attributes. ds:dx are correctly loaded
    // depending upon the memory model.

    regs.x.ax = 0x4301;
    regs.x.cx = attr;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
    segread( &sregs);
    sregs.ds = FP_SEG(pname);
    regs.x.dx = FP_OFF(pname);
    intdosx(&regs, &regs, &sregs);
#else
    regs.x.dx = (unsigned) pname;
    intdos (&regs, &regs);
#endif
    return regs.x.cflag;

#endif

}
