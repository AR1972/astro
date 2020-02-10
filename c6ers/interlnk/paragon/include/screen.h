/***
* $Workfile:   screen.h  $
* $Revision:   1.0  $
*   $Author:   Dave Sewell  $
*     $Date:   28 Apr 1989 17:00:08  $
***/

#define BLACK		0
#define BLUE		1
#define GREEN		2
#define CYAN		3
#define RED		4
#define MAGENTA 	5
#define BROWN		6
#define WHITE		7

#define INTENSE 	0x08
#define BLINKING	0x80
#define MONO_REVERSE	0x0100
#define MONO_UNDERLINE	0x0200
#define MONO_NORMAL	0x0400
#define MONO_BOLD	0x0800

#define YELLOW		(BROWN | INTENSE)

#define color(fg, bg)		(((bg) << 4) + (fg))
#define coord(row, col)        (((row) << 8) + (col))
#define boxsize(height, width)	(((height) << 8) + (width))

extern byte near cdecl force_mono;
extern int cdecl _attrib;
extern unsigned int cdecl _origin;
extern byte cdecl desqview;
extern const byte cdecl cursor_row;
extern const byte cdecl cursor_column;
extern const word cdecl cursor_location;
extern const word cdecl cursor_value;
extern const byte _cdecl scr_rows;
extern const byte _cdecl scr_cols;
