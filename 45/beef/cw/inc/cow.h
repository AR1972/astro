/*
	CW : Character Windows

	cow.h : System Wide definitions
*/

/***BEGIN_PUBLIC***/

#ifdef NOALL
#define NOMINMAX
#define NOWND
#define NOWINSTYLES
#define NOCOLOR
#define NORECT
#define NOMSG
#define NOWINMESSAGES
#define NOKEYSTATE
#define NOMENUS
#define NOMEMMGR
#define NODRAW
#define NOSCROLL
#define NOSUBSTYLES
#define NOVIRTUALKEYCODES
#define NOMB
#define NOWNDMACROS
#endif /*NOALL*/


#ifndef CC
/* -- Pcode Specific Definitions -- */
#ifndef EXPORT
#define	EXPORT	export
#endif
#ifndef NATIVE
#define	NATIVE	native
#endif
#else
/* -- Cmerge Compiler -- */
#ifndef CDECL
#define CDECL cdecl
#endif
#endif /*CC*/


#ifndef PASCAL
#define	PASCAL pascal
#endif

#define	FALSE	0
#define	TRUE	1
#define	NULL	0

#define	FAR	far
#define	LONG	long
#define	VOID	void
#ifdef CC
#define	NEAR	near
#else
#define NEAR
#endif

#define FARPUBLIC	FAR PASCAL		/* all interfaces FAR */

typedef unsigned char	BYTE;
typedef unsigned short	WORD;
typedef unsigned long	DWORD;
typedef int		BOOL;
typedef int	(FARPUBLIC *PFN)();		/* General Procedure */
typedef BOOL	(FARPUBLIC *PFFN)();		/* BOOL Procedure */
typedef VOID	(FARPUBLIC *PVFN)();		/* Void Procedure */
typedef WORD	(FARPUBLIC *PWFN)();		/* Word Procedure */
typedef DWORD	(FARPUBLIC *PLFN)();		/* DWORD Procedure */
typedef VOID	(FARPUBLIC *LPFN)();		/* explicit FAR procedure */
typedef WORD		HANDLE;
typedef unsigned	BIT;
typedef unsigned	BITS;

/* special type for WndProc pointers */
typedef DWORD	(FARPUBLIC *PLFN_WNDPROC)(struct _wnd *, WORD, WORD, DWORD);

/* BYTE/WORD types */
#ifndef CC	/* Pcode => WORD */
typedef WORD		AX;
typedef WORD		AY;
typedef WORD		RX;
typedef WORD		RY;
typedef WORD		ISA;
#else
typedef BYTE		AX;
typedef BYTE		AY;
typedef BYTE		RX;
typedef BYTE		RY;
typedef BYTE		ISA;
#endif /*CC*/

typedef char FAR	*LPSTR;
typedef int  FAR	*LPINT;

#ifndef NOMINMAX
#define	max(a,b)	((a) > (b) ? (a) : (b))
#define	min(a,b)	((a) < (b) ? (a) : (b))
#endif

#define	MAKELONG(l, h)	((long)(((unsigned)(l)) | ((unsigned long)((unsigned)(h))) << 16))
#define MAKEWORD(l, h)	((WORD)(((BYTE)(l)) | ((WORD)((BYTE)(h))) << 8))
#define	LOWORD(l)	((WORD)(l))
#define	HIWORD(l)	((WORD)(((DWORD)(l) >> 16) & 0xffff))
#define	LOBYTE(w)	((BYTE) ((w) & 0xff))
#define	HIBYTE(w)	((BYTE)(((WORD)(w) >> 8) & 0xff))

/* DCHAR = Double Byte Character */
typedef	WORD		DCHAR;
/* CHAR = unsigned char */
typedef unsigned char	CHAR;

/* ACHAR = either a DCHAR or a CHAR */
#ifdef KANJI
typedef	DCHAR		ACHAR;
extern BOOL PASCAL fKanaAccel;	/* set by application to choose accelerators */
#else
typedef	CHAR		ACHAR;
#endif

/*****************************************************************************/

#ifndef NORECT
typedef struct _rrc
	{
	BYTE	rxLeft;
	BYTE	ryTop;
	BYTE	rxRight;
	BYTE	ryBottom;
	} RRC;
typedef RRC *PRRC;
typedef struct _arc
	{
	BYTE	axLeft;
	BYTE	ayTop;
	BYTE	axRight;
	BYTE	ayBottom;
	} ARC;
typedef ARC *PARC;
typedef struct _box
	{
	char	chTopLeftCorner;
	char	chTopRightCorner;
	char	chBottomLeftCorner;
	char	chBottomRightCorner;
	char	chTopSide;
	char	chBottomSide;
	char	chLeftSide;
	char	chRightSide;
	} BOX;
extern BOX PASCAL boxSingle, PASCAL boxDouble;
extern BOX PASCAL boxActiveWindowOut;
extern BOX PASCAL boxInactiveWindowOut;
extern BOX PASCAL boxActiveWindowIn;
extern BOX PASCAL boxInactiveWindowIn;
extern BYTE PASCAL axMac;
extern BYTE PASCAL ayMac;
/* all CW applications should work with screens up to 254x254 */
#define axMax 254
#define ayMax 254

#endif /*!NORECT*/

#ifndef NOWND

#ifndef cwExtraWnd
#ifdef CC
/* -- for CC compiler : fixed length */
#define cwExtraWnd 1 
#else
/* -- for CS compiler : variable length */
#define cwExtraWnd
#endif
#endif

typedef struct _wnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	ARC	arcWindow;
	ARC	arcClipping;
	PLFN_WNDPROC pfnWndProc;			/* Medium Model */
	struct _wnd *pwndParent;
	struct _wnd *pwndSibling;
	struct _wnd *pwndChild;
	BYTE	axCursor;
	BYTE	ayCursor;
	WORD	rgwExtra[cwExtraWnd];
	} WND;

typedef WND *PWND;
#endif /*!NOWND*/

/***END_PUBLIC***/

#ifdef COW

#define	DEBPUB			 1	/* always debugging publics */
#include <version.h>

#define PUBLIC		/* global export */
#define PRIVATE		/* local export within module */
#define	BLOCK		/* Nested Block */


#ifdef CC
#ifndef COW_TINY_SWAPPED
#define FARPRIVATE	NEAR PASCAL		/* small model internal */
#else
#define FARPRIVATE	FAR PASCAL
#endif
#else
#define	FARPRIVATE	PASCAL
#endif

typedef int (FARPRIVATE *PFN_PRIVATE)();	/* General Private Procedure */

#ifndef DEBPUB
#define STATIC static	/* static */
#else
#define STATIC		/* make visible for debugging */
#endif

#define REGISTER register			/* for old code */
#define REG register				/* use this */
#define	Unreferenced(x)		((void)x)	/* for unreferenced formals */

#ifdef DEBUG
PRIVATE VOID FAR CowAssertFailedLine(char *, int);
PRIVATE VOID FAR CowAssertFailedSz(char *);
#define Assert(exp)	{ \
	if (!(exp)) CowAssertFailedLine(__FILE__, __LINE__); \
		}
#define AssertSz(exp, sz)	{ \
	if (!(exp)) CowAssertFailedSz(sz); \
		}
#define	Debug(statement)	statement
	
#else
#define Assert(exp)
#define AssertSz(exp, sz)
#define	Debug(statement)
#endif /* !DEBUG */

#ifdef PROFILE
extern int FAR StartNMeas(void);	/* really VOID */
extern VOID FAR StopNMeas(void);
extern DWORD FAR LTrickCall(void);
extern VOID FAR TrickReturn(DWORD);

extern int PASCAL crefCow;

/* note : StartPublic may should be placed after first { of procedure */
#define	StartPublic() int wProfile = crefCow++ ? 0 : StartNMeas()

/* note : StopPublic should be placed before the end and before any returns */
#define	StopPublic() {if (--crefCow == 0) StopNMeas(); }
#define	ReturnPublic(expr, type) {\
	type xRet = expr; StopPublic(); return(xRet); }

/* note : ReturnLeave is like ReturnPublic, but leave COW to evaluate "expr" */
#define	ReturnLeave(expr, type) {\
	type xRet;		/* variable for expression result */\
	int crefT = crefCow;	/* save reference count */	\
	DWORD lTrick = LTrickCall();	/* trick CALL */	\
	StartNMeas();						\
	crefCow = 0;		/* leaving COW for a while */	\
	xRet = expr;		/* evaluate function */		\
	crefCow = crefT;	/* restore state */		\
	TrickReturn(lTrick);	/* trick return */		\
	StopNMeas();						\
	return(xRet); }

#else
#define	StartPublic()	extern int fooBar	/* trick to put at start */
/*? ?? ?? ? ? does it increase frames in optimized code ???? */
#define	StopPublic()
#define	ReturnPublic(expr, type) return(expr)
#define	ReturnLeave(expr, type) return(expr)
#endif


/*	* Things supplied by the application */
BYTE *		FARPUBLIC PbAllocWork(WORD);
VOID		FARPUBLIC FreeWork(BYTE *);
VOID		FARPUBLIC OutOfMemory(void);
VOID		FARPUBLIC Help(WORD, WORD, VOID *, WORD);
VOID		FARPUBLIC UpdateShiftKk(WORD, WORD);
#ifdef KANJI
VOID		FARPUBLIC UpdateShiftKj(WORD, WORD);
#endif
VOID		FARPUBLIC PrepareSwapDisk(void);

/* for far screen saves (only if SCREEN_FAR_SAVE) */
BYTE FAR *	FARPUBLIC LpbAllocWorkFar(WORD);
VOID		FARPUBLIC FreeWorkFar(BYTE FAR *);

#ifdef DEBUG
BOOL		FARPUBLIC FTrapMouseRbutton(void);
#endif

#endif /*COW*/
