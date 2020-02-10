/* return amount of freespace on a drive
 *
 *  09-Dec-1986 bw - Added DOS 5 support
 *
*/

#if defined(OS2)
#include <os2.h>
#else
#include <dos.h>
#endif

#include "..\h\tools.h"

long freespac (d)
int d;
{
#if defined(NT)

    char root[] = "a:\\";

    DWORD cSecsPerClus, cBytesPerSec, cFreeClus, cTotalClus;

    // Constuct a drive string from the given drive number.
    root[0] = (char)( 'a' + d - 1 );

    return (!GetDiskFreeSpace(root, &cSecsPerClus, &cBytesPerSec,
      &cFreeClus, &cTotalClus)) ?
	-1L :
	cBytesPerSec * cSecsPerClus * cFreeClus;

#elif defined(OS2)

    FSALLOCATE FS;

    if ( DosQFSInfo ( d, 1, (char far *)&FS, sizeof FS ) )
    {
	return -1L;
    }

    return FS.cbSector * FS.cSectorUnit * FS.cUnitAvail;

#elif defined(DOS)

    union REGS regs;

    regs.h.ah = 0x36;
    regs.h.dl = (unsigned char) d;
    intdos (&regs, &regs);
    if (regs.x.cflag)
	return -1L;
    return (long)regs.x.ax * (long)regs.x.bx * (long)regs.x.cx;

#endif
}

long sizeround (l, d)
long l;
int d;
{
#if defined(NT)

    char root[] = "a:\\";
    DWORD cSecsPerClus, cBytesPerSec, cFreeClus, cTotalClus;
    ULONG BytesPerCluster;

    root[0] = (char)( 'a' + d - 1 );

    if( !GetDiskFreeSpace( root, &cSecsPerClus, &cBytesPerSec, &cFreeClus, &cTotalClus ) ) {
        return -1L;
    }

    BytesPerCluster = cSecsPerClus * cBytesPerSec;
    l += BytesPerCluster - 1;
    l /= BytesPerCluster;
    l *= BytesPerCluster;

    return l;


#elif defined(OS2)

    FSALLOCATE FS;
    unsigned long BytesPerCluster;

    if ( DosQFSInfo( d, 1, (char far *)&FS, sizeof FS ) )
    {
	return -1L;
    }

    BytesPerCluster = FS.cbSector * FS.cSectorUnit;
    l += BytesPerCluster - 1;
    l /= BytesPerCluster;
    l *= BytesPerCluster;

    return l;

#elif defined(DOS)

    union REGS regs;

    regs.h.ah = 0x36;
    regs.h.dl = (unsigned char) d;
    intdos (&regs, &regs);
    if (regs.x.cflag == -1)
	return -1L;
    l += regs.x.ax * regs.x.cx - 1;
    l /= regs.x.ax * regs.x.cx;
    l *= regs.x.ax * regs.x.cx;

    return l;

#endif
}
