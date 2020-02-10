/***
* $Workfile:   drawbox.c  $
* $Revision:   1.2  $
*   $Author:   Dave Sewell  $
*     $Date:   24 Apr 1990  7:45:16  $
***/

#include "osdep.h"
#include "umfunc.h"
#include "screen.h"

#define F_H1	 0xC4
#define F_H2	 0xCD
#define F_V1	 0xB3
#define F_V2	 0xBA

#define F_UL1	 0xDA
#define F_UR1	 0xBF
#define F_LL1	 0xC0
#define F_LR1	 0xD9

#define F_UL2	 0xC9
#define F_UR2	 0xBB
#define F_LL2	 0xC8
#define F_LR2	 0xBC

struct Border {
    byte horizontal;            /* Horizontal line */
    byte vertical;              /* Vertical line */
    byte upperLeft;             /* Upper left corner */
    byte upperRight;            /* Upper right corner */
    byte lowerLeft;             /* Lower left corner */
    byte lowerRight;            /* Lower right corner */
};

/***
* If ltype is 1 or -1 a single line border will be drawn, otherwise double.
* If ltype is negative, the interior of the box will not be cleared.
***/

void _fastcall draw_box(int ulpos, int height_width, int ltype, int attrib)
{
    register struct Border *bp;
    int height = height_width >> 8;
    int width	= height_width & 0xFF;
    static struct Border singleBorder = {
        F_H1, F_V1, F_UL1, F_UR1, F_LL1, F_LR1
    };
    static struct Border doubleBorder = {
        F_H2, F_V2, F_UL2, F_UR2, F_LL2, F_LR2
    };

    bp = (ltype == 1 || ltype == -1) ? &singleBorder : &doubleBorder;
    set_attribute(attrib);
    dispchar(ulpos, bp->upperLeft);
    fill(ulpos + 1, boxsize(1, width - 2), bp->horizontal);
    dispchar(ulpos + width - 1, bp->upperRight);
    fill(ulpos + 0x0100, boxsize(height - 2, 1), bp->vertical);
    if (ltype >= 0) fill(ulpos + 0x0101, height_width - 0x0202, ' ');
    fill(ulpos + 0x100 + width - 1, boxsize(height - 2, 1), bp->vertical);
    dispchar(ulpos + (height << 8) - 0x100, bp->lowerLeft);
    fill(ulpos + 1 + (height << 8) - 0x100, boxsize(1, width - 2),
	    bp->horizontal);
    dispchar(ulpos + height_width - 0x0101, bp->lowerRight);
}

