/* SCCSWHAT( "%W% %E% %U%	%Q%" ) */

/*** ld - line buffer */
typedef struct ld {
	ushort flags;
	ushort cb;	/* count of bytes in buffer */
	ushort cbMax;	/* max buffer length */
	char *prgch;	/* pointer to buffer */
	} ld; 

/*** ip - insert point */
typedef struct ip {	/* insert point */
	short ob;	/* offset to current column position */
	short oln;	/* offset to current line position */
	} ip;

/*** pd - program descripter */
typedef struct pd {
	ushort olntop;	/* offset within progrm display at top of screen */
	short obleft;	/* offset within list buffer of left most character */
	ushort oln;	/* current ld line offset within program */
	} pd;

/*** ef - Edit Field ***/
struct ef {
	ushort	hBuffer;
	pd	pdCur;
	ip	ipCur;
	ip	ipAnchor;
	ld	*pldCur;
	PWND	pwndScrollV;
	PWND	pwndScrollH;
	ushort	attrCur;
	uchar	Style;
	bool	fSelection;
};

struct LineAttr {
	ushort	attr;
	ushort	cb;
};

typedef struct LineAttr LineAttr;

extern bool pascal fPasteOk;
extern bool pascal fInsertMode;
extern ld pascal ldEMScratch;

extern bool pascal emFlags;
#define EMF_IN_EDITMGR 0x01

/* styles */
#define ES_MULTILINE	0x0001
#define ES_NOREDRAW	0x0002
#define ES_NOSELECT	0x0004
#define EF_MOVECURSOR	0x0008

/* control messages: */
#define EM_SELCHARS	WM_USER+0
#define EM_REPLACESEL	WM_USER+1
#define EM_GETWORD	WM_USER+2
#define EM_GETLINESEL	WM_USER+3
#define WM_UNDO 	WM_USER+4
#define WM_CLEAR	WM_USER+5
#define WM_SETTEXT	WM_USER+6
#define WM_GETTEXT	WM_USER+7
#define WM_SEARCHFIND	WM_USER+8
#define WM_SEARCHCHANGE WM_USER+9
#define WM_SETBOOKMARK	WM_USER+10
#define WM_GOTOBOOKMARK WM_USER+11
#define WM_SETREDRAW	WM_USER+12
#define WM_SEARCHNEXT	WM_USER+13
#define WM_MATCHBRACE	WM_USER+14
#define WM_FLUSHFOCUS	WM_USER+15
#define EM_MOVECURSOR	WM_USER+16

/* ldCur.flags values */
#define LD_fDirty			0x0001

#define isaEditWindow		(isaUserMin + 0)
#define isaCurStmt		(isaUserMin + 1)
#define isaBreakpoint		(isaUserMin + 2)
#define isaCurBreakpoint	(isaUserMin + 3)
#define isaStatusLine		(isaUserMin + 4)
#define isaStatusAlert		(isaUserMin + 5)
#define isaStatusLock		(isaUserMin + 6)

#define isaDebugWindow		(isaUserMin + 7)
#define isaHelpWindow		(isaUserMin + 8)
#define isaIncludeFileHilite	(isaUserMin + 9)
#define isaListBoxHilite	(isaUserMin + 10)
#define isaWatchpointHilite	(isaUserMin + 11)

#define isaBold 		(isaUserMin + 12)
#define isaItalic		(isaUserMin + 13)
#define isaUnderline		(isaUserMin + 14)

#define isaHelp 		isaDialogBox
#define isaSyntaxHelp		isaHelpWindow
#define isaWatchWindow		isaDebugWindow

#define pefExtra		rgwExtra[1]
