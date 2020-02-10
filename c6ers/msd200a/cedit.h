/*
	CW : Character Windows
	cedit.h

	Multi-line edit structures
-- Created Mon Dec 16 14:30:57 1991 */ 

#ifndef NOMULTIEDIT

/*** ld - line buffer */
typedef struct ld
	{
	WORD	flags;
	WORD	cb;	/* count of bytes in buffer */
	WORD	cbMax;	/* max buffer length */
	char *	prgch;	/* pointer to buffer */
	} LD; 

/*** ip - insert point */
typedef struct ip
	{	/* insert point */
	short	ob;	/* offset to current column position */
	short	oln;	/* offset to current line position */
	} IP;

/*** pd - program descripter */
typedef struct pd
	{
	WORD	olntop;	/* offset within progrm display at top of screen */
	short	obleft;	/* offset within list buffer of left most character */
	WORD	oln;	/* current ld line offset within program */
	} PD;

/*** ef - Edit Field ***/
typedef struct ef
	{
	WORD	hBuffer;
	PD	pdCur;
	IP	ipCur;
	IP	ipAnchor;
	LD	*pldCur;
	PWND	pwndScrollV;
	PWND	pwndScrollH;
	WORD	attrCur;
	BYTE	Style;
	BYTE	fSelection;
	} EF;

#define hBufferUndefined	0xffff	/* hBuffer is not defined */

typedef struct LineAttr
	{
	WORD	attr;
	WORD	cb;
	} LineAttr;

extern BOOL PASCAL fPasteOk;
extern BOOL PASCAL fInsertMode;
extern LD PASCAL ldEMScratch;

extern WORD PASCAL emFlags;
#define EMF_IN_EDITMGR 0x01

/* styles */
#define ES_MULTILINE	0x0001
#define ES_NOREDRAW	0x0002
#define ES_NOSELECT	0x0004

/* control messages: */
#define EM_SELCHARS	(WM_USER+2+0)
#define EM_REPLACESEL	(WM_USER+2+1)
#define EM_GETWORD	(WM_USER+2+2)
#define EM_GETLINESEL	(WM_USER+2+3)
#define WM_UNDO 	(WM_USER+2+4)
#define WM_CLEAR	(WM_USER+2+5)
#define WM_SETTEXT	(WM_USER+2+6)
#define WM_GETTEXT	(WM_USER+2+7)
#define WM_SEARCHFIND	(WM_USER+2+8)
#define WM_SEARCHCHANGE (WM_USER+2+9)
#define WM_SETBOOKMARK	(WM_USER+2+10)
#define WM_GOTOBOOKMARK (WM_USER+2+11)
#define WM_SEARCHNEXT	(WM_USER+2+13)
#define WM_MATCHBRACE	(WM_USER+2+14)

/* ldCur.flags values */
#define LD_fDirty			0x0001


/* for Basic Text Manager Only */
#ifndef	NOPROCS
ISA		FARPUBLIC SetInverseIsa(ISA);		/* OPTIONAL */
#endif	/* !NOPROCS */

#endif /*!NOMULTIEDIT*/

