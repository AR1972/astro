/***
* $Workfile:   cwuser.h  $
* $Revision:   1.5  $
*   $Author:   Dave Sewell  $
*     $Date:   28 Sep 1990  9:54:46  $
***/

#ifndef BYTE
#define BYTE  unsigned char
#endif
#ifndef WORD
#define WORD  unsigned short
#endif
#ifndef O_RAW
#define O_RAW O_BINARY
#endif
#ifndef NULL
#if defined(M_I86LM) || defined(M_I86CM) || defined(M_I86HM)
#define NULL 0L
#else
#define NULL 0
#endif
#endif

#ifndef NOPROC
#define NOPROC ((int (*)())0)
#endif

#ifndef TRUE
#include    "osdep.h"
#endif

/**** The following are a core of functions common to most applications ****/

#include "bioskybd.h"
#include "screen.h"

/*** fatal error numbers ***/
#define CREATE_PORTAL_FATAL_ERROR   1
#define DISPLAYTEXTINPORTAL_FATAL_1 2
#define DISPLAYTEXTINPORTAL_FATAL_2 3
#define HELP_STACK_FULL_FATAL_ERROR 5
#define HELP_MALLOC_FATAL_ERROR     7
#define LIST_STACK_FULL_FATAL_ERROR 8
#define LIST_STACK_EMPTY_FATAL_ERR  9
#define OUT_OF_NEAR_HEAP_FATAL_ERROR	10
#define PORTAL_TABLE_FULL_FATAL_ERROR	11
#define LIST_SEARCH_ERROR		12

/*** color definitions ***/

#define INIT_NORMAL	(color(CYAN,BLUE))
#define INIT_REVERSE	(color(BLUE,CYAN) | MONO_REVERSE)
#define INIT_RBLINK	(color(BLUE,CYAN) | MONO_REVERSE | BLINKING)
#define INIT_INTENSE	(color(CYAN,BLUE) | INTENSE)
#define INIT_IBLINK	(color(CYAN,BLUE) | INTENSE | BLINKING)

#define LIST_NORMAL	(color(WHITE,BLUE) | INTENSE | MONO_BOLD)
#define LIST_DIM	(color(WHITE,BLUE) | MONO_NORMAL)
#define LIST_REVERSE	(color(BLUE,WHITE) | MONO_REVERSE)
#define LIST_RBLINK	(color(BLUE,WHITE) | MONO_REVERSE | BLINKING)
#define LIST_INTENSE	(color(YELLOW,BLUE) | MONO_BOLD)
#define LIST_IBLINK	(color(YELLOW,BLUE) | BLINKING)

#define ALERT_REVERSE	(color(WHITE, MAGENTA) | INTENSE | MONO_REVERSE)

#define NO_CLEAR    0x80

struct portal
{
    BYTE frameLine;	     /* Topmost line of frame on physical screen */
    BYTE frameColumn;	     /* Leftmost column of frame on physical screen */
    BYTE frameHeight;	     /* Height of frame on physical screen */
    BYTE frameWidth;	     /* Width of frame on physical screen */
    BYTE saveFlag;	     /* TRUE = save old screen (may have NO_CLEAR) */
    BYTE borderType;	     /* Type of border to use */
    int  borderAttribute;    /* Attribute to use with border */
    int  borderDeselect;     /* Border attribute when deselected.	*/
    int  normal_attribute;   /* Normal attribute for portal body.	*/
    char *headerText;	     /* Pointer to header text */
};

typedef struct pcb
{
    struct portal p;
    byte headerLen;	     /* Max length of header, including null	*/
    char *saveScreen;	     /* Address of save screen */
    BYTE portalLine;	     /* Topmost line of portal on physical screen */
    BYTE portalColumn;	     /* Rightmost column of portal on physical screen */
    BYTE portalHeight;	     /* Height of portal */
    BYTE portalWidth;	     /* Width of portal */
} PCB;

#define NO_MESSAGE ((char *) 0)
#define NOHEADER   ((char *)NULL) /* no header with the portal */
#define NOBORDER   ((BYTE)0)	  /* no border around portal */
#define SINGLE	   ((BYTE)1)	  /* single line forms border */
#define DOUBLE	   ((BYTE)2)	  /* double line forms border */

#define SAVE	   ((BYTE)1)	  /* save old screen under portal */
#define NOSAVE	   ((BYTE)0)	  /* don't save screen under portal */

#define CURSOR_ON  ((BYTE)1)	  /* turn the cursor on in the portal */
#define CURSOR_OFF ((BYTE)0)	  /* turn the cursor off in the portal */

#define DIRECT	   ((BYTE)1)	  /* no virtual space for portal */
#define VIRTUAL    ((BYTE)0)	  /* create virtual space for portal */

/**** valid key flags for SelectFromList ****/
#define M_ESC	    0x0001
#define M_ESCAPE    0x0001
#define M_INSERT    0x0002
#define M_DELETE    0x0004
#define M_MODIFY    0x0008
#define M_SELECT    0x0010
#define M_MDELETE   0x0020
#define M_CYCLE     0x0040
#define M_MMODIFY   0x0080
#define M_MSELECT   0x0100
	    /* these are both only used in split screen mode, so the
	    *  verification checks should work fine.  M_MARKALL is
	    *  only used for verification, and the value is never
	    *  returned to the caller of SelectFromList, so it
	    *  we can identify this return value as a MKDIR.
	    */
#define M_MARKALL   0x0200
#define M_MKDIR     0x0200
#define M_VIEW	    0x0400
#define M_RENAME    0x0800
#define M_MARKPLUS  0x1000
#define M_LEFT	    0x2000
#define M_RIGHT     0x4000

#define TEXT_OFFSET 9

/**** definition of list marking primitives ****/
/* The following bit in the 1st byte of the record is used for the mark bit. */
#define MARK_BIT    0x40

#define Mark(element)	   (*((char far *)element) |=  MARK_BIT)
#define Unmark(element)    (*((char far *)element) &= ~MARK_BIT)
#define IsMarked(element)  (*((char far *)element) &   MARK_BIT)

/**** definition of list structure ****/
/* NOTE: The 'text' field is declared as an array of 1 character.  However,
 * the list manipulation procedures will allocate a memory buffer longer than
 * the minimum size of the LIST structure.  The 'text' field is treated as a
 * null-terminated string occupying the memory left over from that used by the
 * other fields.
 */
typedef struct LIST_STRUCT
{
    BYTE marked;		     /* whether or not element is marked */
    struct LIST_STRUCT far *prev;    /* previous element in list */
    struct LIST_STRUCT far *next;    /* next element in list */
    char far *otherInfo;	     /* pointer to auxiliary data */
    char text[1];		     /* text (primary data) for element */
} LIST;

typedef struct listptr
{
    LIST far *head;
    LIST far *tail;
    int (pascal *sortProc)();
} LISTPTR;

struct options {
    char *text; 	    /* Description of option.			    */
    int  value; 	    /* Value to associate with the option.	    */
};

struct menu {
    byte center_line;		/* Line (row) to center vertically on.	    */
    byte center_column; 	/* Column to center horizontally on.	    */
    char *header;		/* Header (title) for menu.		    */
    int  help_screen;		/* Help screen number.			    */
    int  (*action)(int);	/* Action routine.			    */
    struct options *options;	/* Pointer to list of options.		    */
};


/* Form definitions */

struct edtstr { 		/* Edit string type data structure	    */
    byte center_line;		/* Line (row) where edit portal is centered */
    byte center_column; 	/* Column where edit portal is centered     */
    byte portal_height; 	/* Edit portal height			    */
    byte portal_width;		/* Edit portal width			    */
    char *header;		/* Header message for edit portal	    */
};

struct fields {
    void *data; 		    /* Pointer to data for field	    */
    int  bufsize;		    /* size allocated for *data 	    */

    byte line;			    /* Line (in portal) of field	    */
    byte column;		    /* Column (in portal) of field	    */
    byte width; 		    /* Width (in portal) of field	    */

    byte type;			    /* Type of field			    */
    int flags;			    /* Control Flags			    */

    int help_screen;		    /* Help screen for field		    */
    char *prompt;		    /* Prompt for field 		    */
    void *info; 		    /* Pointer field specific data	    */
};

struct form {
    byte center_line;		/* Line (row) to center vertically on.	    */
    byte center_column; 	/* Column to center horizontally on.	    */
    byte form_height;
    byte form_width;
    char *header;		/* Header (title) for menu.		    */
    int  help_screen;		/* Help screen number.			    */
    int  exit_help_screen;	/* Help screen number.			    */
    struct fields *fields;	/* Pointer to list of fields.		    */
};

struct form_control {
    int (near _fastcall *input)(struct fields *, int, int *);
    void (near _fastcall *output)(struct fields *);
    int (near _fastcall *verify)(struct fields *);
};

/* Standard form field types */

#define MENU_TYPE      0
#define TIME_TYPE      1
#define DATE_TYPE      2
#define STRING_TYPE    3
#define EDTSTR_TYPE    4
#define PROMPT_TYPE    5
#define NULL_TYPE      6

/**** Field select key types ****/
#define S_NONFUNC 0x00		/* Non function key cause selection */
#define S_SELECT  0x01		/* K_SELECT key caused selection */
#define S_MODIFY  0x02		/* K_MODIFY key caused selection */
#define S_INSERT  0x04		/* K_INSERT key caused selection */
#define S_DELETE  0x08		/* K_DELETE key caused selection */


/**** fieldFlags Type masks ****/
#define NORMAL_FIELD	    0x00	/* normal editable field */
#define LOCKED_FIELD	    0x01	/* non accessable */
#define REQUIRED_FIELD	    0x02	/* verify field on form exit */
#define PROMPT_FIELD	    0x04 | LOCKED_FIELD

#define RIGHT_FORMAT	    0x40	/* right justification format */
#define LEFT_FORMAT	    0x80	/* left justification format */
#define CENTER_FORMAT	    0xC0	/* centering format */

#define LONGEST_MONTH_NAME 9

/**** Definition of edit return codes ****/
#define E_CHANGE     0x01    /* Whether buffer was changed */
#define E_ESCAPE     0x02    /* Whether buffer edit was canceled */
#define E_SELECT     0x04    /* Whether changes confirmed */
#define E_EMPTY      0x08    /* Whether buffer has anything in it */

/* Video Scroll Directions */
#define V_UP	 ((BYTE)6)
#define V_DOWN	 ((BYTE)7)

#define FATAL		      1
#define WARNING 	      2
#define INFORM		      3

#define J_NONE		      0
#define J_LEFT		      1
#define J_RIGHT 	      2

#define HEADER_NONE	      -1
#define HEADER_NORMAL	      0

#define NO_HELP_CONTEXT       -1


extern int (* edit_portal_string_key_handler)(int);

extern int currentPortal;		// Index of current portal.
extern PCB *portal[];         // Table of active portals.

extern unsigned list_rec_size;
extern unsigned list_num_recs;

/*** Macros for doing writes within a portal. ***/

extern unsigned origin;

#define p_fill_attr(ul, size, c, att) fill_attr((ul)+origin, size, c, att)
#define p_dispstr_attr(pos, buf, att) dispstr_attr((pos)+origin, buf, att)
#define p_dispmem_attr(pos,buf,cnt,att) dispmem_attr((pos)+origin,buf,cnt,att)
#define p_dispstr(pos,buf) dispstr((pos)+origin, buf)
#define p_dispchar(pos,c) dispchar((pos)+origin, c)
#define p_locate(row_col) locate((row_col)+origin)
#define p_dispmem(pos, buff, cnt) dispmem((pos)+origin, buff, cnt)
#define p_clear(ulpos, lrpos) clear((ulpos)+origin, (lrpos)+origin)
#define p_scroll_up(ul,lr,att,cnt) scroll((ul)+origin,(lr)+origin,att,cnt,0)
#define p_scroll_down(ul,lr,att,cnt) scroll((ul)+origin,(lr)+origin,att,cnt,1)
#define p_fill(ulpos, size, c) fill((ulpos)+origin, size, c)
#define p_shade(ulpos, height_width) shade((ulpos)+origin, height_width)
#define p_save_zone(ulpos, size, buff) save_zone((ulpos)+origin, size, buff)
#define p_restore_zone(ulpos,size,buff) restore_zone((ulpos)+origin,size,buff)

/* date and time format controls */
#define NORMAL_DATE	       0x00
#define USE_ALPHA_MONTH        0x01
#define INCLUDE_DAY_OF_WEEK    0x02

/* box_printf flags */

#define BOX_HORIZ_FLAGS     0x03
#define HORIZ_CENTER        0x00
#define HORIZ_LEFT          0x02
#define HORIZ_RIGHT         0x01
#define BOX_VERT_FLAGS      0x0C
#define VERT_CENTER         0x00
#define VERT_TOP            0x08
#define VERT_BOTTOM         0x04
#define BOX_DOUBLE          0x00
#define BOX_SINGLE          0x10
#define BOX_SAVE            0x00
#define BOX_NOSAVE          0x20
#define BOX_BELL            0x40

#include <stdarg.h>

#ifndef CWFUNC
#define CWFUNC	1
#include    "cwfunc.h"
#endif
