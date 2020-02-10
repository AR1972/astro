/*
	CW: Character Oriented Windows
	
	inscreen.h: Installable screen driver CW info
*/

#include <indrv.h>				/* installable driver */
#include <inscr.h>				/* installable driver */

/***BEGIN_PUBLIC***/

extern INST PASCAL instCur;
extern INCH PASCAL inch;		/* near buffer for characters */
extern BOOL PASCAL fFontAvailable;	/* extra "ffont" drawing available ? */

/* name aliases for the actual characters */
#define	chTopSide1		(inch._chTopSide1)
#define	chBottomSide1		(inch._chBottomSide1)
#define	chLeftSide1		(inch._chLeftSide1)
#define	chRightSide1		(inch._chRightSide1)
#define	chTopLeftCorner1	(inch._chTopLeftCorner1)
#define	chTopRightCorner1	(inch._chTopRightCorner1)
#define	chBottomLeftCorner1	(inch._chBottomLeftCorner1)
#define	chBottomRightCorner1	(inch._chBottomRightCorner1)
#define chMiddleLeft1		(inch._chMiddleLeft1)
#define chMiddleRight1		(inch._chMiddleRight1)
#define	chTopSide2		(inch._chTopSide2)
#define	chBottomSide2		(inch._chBottomSide2)
#define	chTopLeftCorner2	(inch._chTopLeftCorner2)
#define	chTopRightCorner2	(inch._chTopRightCorner2)
#define	chBottomLeftCorner2	(inch._chBottomLeftCorner2)
#define	chBottomRightCorner2	(inch._chBottomRightCorner2)
#define	chUpArrow		(inch._chUpArrow)
#define	chDownArrow		(inch._chDownArrow)
#define	chRightArrow		(inch._chRightArrow)
#define	chLeftArrow		(inch._chLeftArrow)
#define	chBullet		(inch._chBullet)
#define	chMiddleDot		(inch._chMiddleDot)
#define	chScrollbar		(inch._chScrollbar)
#define	chElevator		(inch._chElevator)
#define	chShadowInit		(inch._chShadowInit)

/* for overlapping windows */
#define	chClose			(inch._chClose)
#define	chZoomIn		(inch._chZoomIn)
#define	chZoomOut		(inch._chZoomOut)
#define	chUpDownArrow		(inch._chUpDownArrow)
#define	chLeftRightArrow	(inch._chLeftRightArrow)

/*****************************************************************************/
/* Screen Procedures */

typedef WORD FAR *	(FAR PASCAL *LPFN_DRV_ALLOC)(WORD, WORD);
typedef VOID		(FAR PASCAL *LPFN_DRV_FREE)(WORD FAR *);

WORD	FARPUBLIC ImodeGuessCurrent(void);			/*OPTIONAL*/
BOOL	FARPUBLIC FQueryInst(INST *, WORD);			/*OPTIONAL*/
BOOL	FARPUBLIC FAllocInstBuffers(INST *, LPFN_DRV_ALLOC, BOOL); /*OPTIONAL*/
VOID	FARPUBLIC FreeInstBuffers(INST *, LPFN_DRV_FREE);	/*OPTIONAL*/

BOOL	FARPUBLIC FAllocOverlapTable(INST *, LPFN_DRV_ALLOC);	/*OPTIONAL*/
VOID	FARPUBLIC FreeOverlapTable(LPFN_DRV_FREE);		/*OPTIONAL*/

BOOL	FARPUBLIC FInitScreen(INST *);				/*OPTIONAL*/
VOID	FARPUBLIC EndScreen(BOOL);				/*OPTIONAL*/

BOOL	FARPUBLIC FGetColorPalette(WORD, WORD *, WORD *);	/*OPTIONAL*/
VOID	FARPUBLIC SetColorPalette(WORD, WORD, WORD *);		/*OPTIONAL*/

VOID	FARPUBLIC MoveHwCursCsd(AX, AY, WORD);			/*OPTIONAL*/

BOOL	FARPUBLIC FQueryInft(INFT *, WORD);			/*OPTIONAL*/

WORD	FARPUBLIC CbSizeVids(void);				/*OPTIONAL*/
BOOL	FARPUBLIC FSaveVids(VIDS *, INST *);			/*OPTIONAL*/
BOOL	FARPUBLIC FRestoreVids(VIDS *);				/*OPTIONAL*/
VOID	FARPUBLIC SaveVidData(VIDS *, WORD FAR *);		/*OPTIONAL*/
VOID	FARPUBLIC RestoreVidData(VIDS *, WORD FAR *);		/*OPTIONAL*/
VOID	FARPUBLIC EnableVidsMonitor(BOOL);			/*OPTIONAL*/

VOID	FARPUBLIC GetCharMap(INFT *, BYTE, BYTE *);

/***END_PUBLIC***/

/*****************************************************************************/
/* COW private info */

extern INSJ insj;			/* jump vectors for screen driver */

/*****************************************************************************/
