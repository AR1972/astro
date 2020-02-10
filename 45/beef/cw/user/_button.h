/*
	COW : Character Oriented Windows

	_button.h : button specific stuff
*/


/* bits :
   0..1 => bst (button state)
   2 => fButtonDown
   3 => first in group
*/

/* button states */

#define bstOff		0
#define bstOn		1
#define bstGreyed	2		/*Assert(bstGreyed == wNinchCheck); */
#define bstMax		3


/* button characters */

#define chRadioButtonOff	' '
#ifdef KANJI
#define chRadioButtonOn		chMiddleDot
#else
#define chRadioButtonOn		chBullet
#endif
#define chRadioButtonPrefix	'('
#define chRadioButtonSuffix	')'
#define chCheckBoxOff		' '
#define chCheckBoxOn		'X'
#define chCheckBoxGrey		'-'
#define chCheckBoxPrefix	'['
#define chCheckBoxSuffix	']'


/* structure for rendering */
typedef struct _btr
	{
	char	chOff;
	char	chOn;
	char	chPrefix;
	char	chSuffix;
	char	chGrey;
	} BTR;		/* Button rendering */

#define	rxPrefix	0
#define	rxButton	1
#define	rxSuffix	2
#define	rxButtonText	4		/* one space after suffix */


/* Macros for bit accesses */
#define BstOfWnd(pwnd) ((pwnd)->wButton & 3)

#define SetWndBst(pwnd, bst) {\
	(pwnd)->wButton = (pwnd->wButton & ~(3+8)) | (bst) ;}

#define FButtonDown(pwnd) ((pwnd)->wButton & 4)

#define SetFButtonDown(pwnd, fDown) \
	{if (fDown) (pwnd)->wButton |= 4; \
	  else (pwnd)->wButton &= ~4;}

#define FFirstButton(pwnd) ((pwnd)->wButton & 8)

