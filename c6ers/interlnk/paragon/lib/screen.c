/***
* $Workfile:   screen.c  $
* $Revision:   1.1  $
*   $Author:   Dave Sewell  $
*     $Date:   24 Apr 1990  7:48:44  $
*
* Text mode screen access routines for IBM screen memory compatible machines.
***/

#include "osdep.h"
#include <bios.h>
#include <string.h>
#include "screen.h"
#include "umfunc.h"

#define MONO_SEG    0xB000	    /* Segment of monochrome display memory */
#define COLOR_SEG   0xB800	    /* Segment of color display memory	    */

#define MAX_ATTRIBS 6
#define MAX_CURSORS 4

byte const _near _cdecl scr_rows = 25;
byte const _near _cdecl scr_cols = 80;
byte near cdecl force_mono   = FALSE;
byte near cdecl retrace_wait = FALSE;
unsigned short near cdecl display_segment = MONO_SEG;
unsigned short near cdecl display_offset  = 0;
unsigned short near cdecl on_cursor_value;    /* Value needed to turn cursor on.  */
unsigned short near cdecl off_cursor_value;   /* Value needed to turn cursor off. */

static int attrib_stack[MAX_ATTRIBS];
static int asp = 0;			/* Attribute stack pointer.	    */

static struct {
    unsigned value;
    unsigned location;
} cursor_stack[MAX_CURSORS];

static int csp = 0;

void _fastcall push_cursor(int on_flag, int location)
{
    if (csp < MAX_CURSORS) {
        cursor_stack[csp].value    = cursor_value;
        cursor_stack[csp].location = cursor_location;
        csp++;
    }
    if (on_flag) cursor_on();
    else cursor_off();
    locate(location);
}

void pascal pop_cursor()
{
    if (csp) {
        csp--;
        restore_cursor(cursor_stack[csp].value);
        locate(cursor_stack[csp].location);
    }
}


void _fastcall push_attribute(new_attribute)
int new_attribute;
{
    if (asp < MAX_ATTRIBS) attrib_stack[asp++] = _attrib;
    else new_attribute |= BLINKING;	/* Stack overflow - indicate w/blink*/
    set_attribute(new_attribute);
}

void pascal pop_attribute()
{
    if (asp) set_attribute(attrib_stack[--asp]);
    else set_attribute( color(RED, WHITE) | BLINKING );
}

void _fastcall fill_attr(int ulpos, int height_width, int c, int attrib)
{
    set_attribute(attrib);
    fill(ulpos, height_width, c);
}

void _fastcall dispstr_attr(int pos, byte far *buff, int attr)
{
    set_attribute(attr);
    dispstr(pos, buff);
}

void _fastcall dispmem_attr(int pos, byte far *buff, int cnt, int attrib)
{
    set_attribute(attrib);
    dispmem(pos, buff, cnt);
}

void _fastcall dispstr(int pos, byte far *buff)
{
    dispmem(pos, buff, strlenf(buff));
}

void _fastcall dispchar(int pos, int c)
{
    dispmem(pos, (byte far *) &c, 1);
}
