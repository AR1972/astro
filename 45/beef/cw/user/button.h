/*
	COW : Character Oriented Windows

	button.h : button cow private interface
*/

DWORD	FARPUBLIC	ButtonWndProc(PWND, WORD, WORD, DWORD);



/* button control styles */
#define BS_AUTO				0x10
#define BS_PUSHBUTTON			0
#define BS_DEFPUSHBUTTON		1
#define BS_CHECKBOX			2
#define BS_AUTOCHECKBOX			(BS_CHECKBOX | BS_AUTO)
#define BS_RADIOBUTTON			3
#define BS_GROUP 			4


#define wButton 	rgwExtra[cwExtraText]
					/* flag word for button control */
#define	pwndButtonNext	rgwExtra[cwExtraText+1]	/* next radio button */

#if cwExtraButton != cwExtraText+2
.....
#endif

#define	wButtonFirstRadio	8	/* Initial value for 1st button */
#define	wButtonInit		0	/* Initial value for other buttons */

/* walking the list of radio buttons */
#define	PwndButtonNext(pwnd) ((PWND) (pwnd)->pwndButtonNext)
	/* note : uses "pbox" extra word */


VOID	FARPRIVATE	CheckDlgButton(PWND, WORD, BOOL);
WORD	FARPRIVATE	WButtonChecked(PWND);
VOID	FARPRIVATE	CheckRadioButton(PWND, WORD, BOOL);
PWND	FARPRIVATE	PwndDefaultPushButton(PWND);

#ifndef BUTTON_LARGE
#define	dayPushButton	1		/* single line button */
#define	daxPushButton	2		/* <> overhead */
#define	wsPushButton	(WS_CHILD | WS_BUTTON | BS_PUSHBUTTON)
#define	wsDefPushButton	(WS_CHILD | WS_BUTTON | BS_DEFPUSHBUTTON)
#else
#define	dayPushButton	3		/* fat button */
#define	daxPushButton	0		/* no <> overhead */
#define	wsPushButton	(WS_CHILD | WS_BUTTON | BS_PUSHBUTTON | WS_BORDER)
#define	wsDefPushButton	(WS_CHILD | WS_BUTTON | BS_DEFPUSHBUTTON | WS_BORDER)
#endif /*BUTTON_LARGE*/


#ifdef BUTTON_LARGE
#ifdef DIALOG_LINE
	.... ERROR : BUTTON_LARGE and DIALOG_LINE are mutually exclusive !!
#endif
#endif
