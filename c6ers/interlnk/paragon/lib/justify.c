/***
* $Workfile:   justify.c  $
* $Revision:   1.1  $
*   $Author:   Dave Sewell  $
*     $Date:   24 Apr 1990  7:45:22  $
***/

#include "osdep.h"
#include "screen.h"
#include "umfunc.h"

void _fastcall justify_str(int pos, int len, int attrib, char *str, int justify)
{
    register int str_len;
    int leading_blanks;     /* Number of blanks to output before string.    */
    int trailing_blanks;    /* Number of blanks to output after string.     */

    push_attribute(attrib);
    str_len = pstrlen(str);
    if (justify < 0) {		/* left justify.    */
	leading_blanks	= 0;
	trailing_blanks = len - str_len;
    }
    else if (justify > 0) {	/* right justify    */
	leading_blanks = len - str_len;
	trailing_blanks = 0;
    }
    else {			/* center	    */
	leading_blanks = (len - str_len) / 2;
	trailing_blanks = len - str_len - leading_blanks;
    }
    fill(pos, boxsize(1, leading_blanks), ' ');
    dispstr(pos + leading_blanks, str);
    fill(pos + leading_blanks + str_len, boxsize(1, trailing_blanks), ' ');
    pop_attribute();
}

