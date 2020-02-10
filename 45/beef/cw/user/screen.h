/*
	COW: Character Oriented Windows
	
	screen.h: cow private screen interface
*/

/*****************************************************************************/
/* characters */

#include <inscreen.h>

/* Border width */
#define	daxBorder	1
#define	dayBorder	1

extern BYTE		cHoldUpdate;		/* defer DoneUpdate */

#define BeginDraw()     (cHoldUpdate++)

/*****************************************************************************/

VOID	FARPUBLIC	MoveHardwareCursor(AX, AY, BOOL);
#ifdef KANJI
VOID	FARPRIVATE	FlushDraw(void);
#endif
VOID	FARPRIVATE	EndDraw(void);

/* ABS coordinate routunes */
VOID	FARPRIVATE 	CharOutAbs(AX, AY, ACHAR, WORD);
VOID	FARPRIVATE	TextOutAbs(AX, AY, char *, WORD, WORD);
VOID	FARPRIVATE	FillArc(AX, AY, AX, AY, ACHAR, WORD);
VOID    FARPRIVATE      BltArc(AX, AY, WORD, WORD, AX, AY);

VOID	FARPRIVATE	SaveArc(AX, AY, AX, AY, BYTE FAR *);
VOID	FARPRIVATE	RestoreArc(AX, AY, AX, AY, BYTE FAR *);

/*****************************************************************************/

#ifdef KANJI
extern BOOL		fRestoreDbcs;		/* restore 1/2 characters ? */
#define	daxDbcs		1			/* 1 byte for DBCS 1/2 character
						  padding */
#else
#define	daxDbcs		0
#endif

/*****************************************************************************/
