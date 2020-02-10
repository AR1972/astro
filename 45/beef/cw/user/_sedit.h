/*
	COW : Character Oriented Windows

	_sedit.h : single line edit specific stuff
*/

/*	* Range for valid wParam for WM_CHAR (9 bits) */
/* NOTE : be careful if you change these */
#define wEditFirst	' '		/* ignore control characters */
#define	wEditLast	0xff		/* ignore untranslated VK_'s */
#define chDelete	0x7f		/* special delete character */

#define	ctickRepEdit	1		/* edit repeat time */

/* flags are:
	0=> fFocus
	1=> fChanged
*/


#define lszNull ((char far *) NULL)

#define FFocus(pwnd) ((pwnd)->wEb & 1)

#define SetFFocus(pwnd, fFocus) \
	{if (fFocus) (pwnd)->wEb |= 1; \
	 else (pwnd)->wEb &= ~1;}
 
#define FChanged(pwnd) ((pwnd)->wEb & 2)

#define SetFChanged(pwnd, fChanged) \
	{if (fChanged) (pwnd)->wEb |= 2; \
	 else (pwnd)->wEb &= ~2;}

#define SzEdit(pwnd) ((char *) pwnd->szDialog)

