/*
	CW : Character Windows

	kkeyboar.h : keyboard exported interface
*/

#include <inkbd.h>				/* installable driver */

/***BEGIN_PUBLIC***/
extern BOOL PASCAL fAbort;			/* normal Abort */
extern BOOL PASCAL fPollKeyboard;		/* Poll the keyboard ? */
extern BYTE PASCAL fKeyIsUp, fKeyWasUp;		/* Key transitions */
extern WORD PASCAL wRateKeyRepeat;		/* repeat rate */
VOID		FARPUBLIC EnableKeyboard(BOOL);
VOID		FARPUBLIC PollKeyboard(void);
VOID		FARPUBLIC SetShiftKk(WORD);
VOID		FARPUBLIC DisableExtendedKeyboard(void);
/***END_PUBLIC***/


/*****************************************************************************/

#ifdef COW
/* COW PRIVATE */

INKJ	inkj;					/* jump vectors */

extern BYTE PASCAL fNonAltKey;			/* set on every non-alt key */
extern WORD PASCAL fNormalKeyboard;		/* FALSE => TSR keyboard input */
extern BYTE PASCAL fAbortSpecial;		/* special abort case */

WORD	FAR PASCAL	MkGetShiftStates(void);
#define	FAltDown()	(MkGetShiftStates() & MK_MENU)


#ifdef KANJI
char	FAR PASCAL	ChAlternateKeytop(char);
#endif /*KANJI*/

#endif /*COW*/

