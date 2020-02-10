/***
* $Workfile:   loadherc.c  $
* $Revision:   1.2  $
*   $Author:   Dave Sewell  $
*     $Date:   11 Sep 1990  8:48:50  $
*
* Load Hercules Graphics InColor palette routine.
***/

#include <dos.h>
#include <fcntl.h>
#include <stdlib.h>
#include "osdep.h"

/* LoadHerculesPalette -- Loads the file specified by the HPAL environment
    variable into the passed in buffer.  This file should contain the users
    favorite color palette for the InColor card.

    This routine is called from init_screen() only.
*/

int _far _pascal LoadHerculesPalette(void far *palette_buffer, unsigned length)
{
    int fd, retval;
    unsigned bytes;
    char *path = getenv("HPAL");

    if (!path) return FALSE;
    if (_dos_open(path, O_RDONLY, &fd)) return FALSE;
    retval = (_dos_read(fd, palette_buffer, length, &bytes) || bytes !=length) ?
        FALSE : TRUE;
    _dos_close(fd);
    return retval;
}
