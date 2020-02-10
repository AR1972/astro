/*    	COW : Character Oriented Windows

	udialog.h: Definitions for user Dialog 
*/

/***BEGIN_PUBLIC***/

WORD		FARPUBLIC MessageBox(char *, char *, char *, WORD);
VOID		FARPUBLIC SetDialogCaption(HANDLE, char *);
VOID		FARPUBLIC HiliteDialogAccel(void);

/* Message Box Definitions */

#ifndef NOMB
#define	IDDEFAULT		1
#define	IDCANCEL		2
#define	IDNO			3
#define	IDOK			IDDEFAULT
#define	IDYES			IDDEFAULT
#define	IDRETRY			IDDEFAULT
#define	IDABORT			IDDEFAULT
#ifdef	HELP_BUTTON
#define IDHELP			4
#endif	// HELP_BUTTON
#define	MB_OK			1
#define	MB_YESNOCANCEL		2
#define	MB_RETRYCANCEL		3
#define	MB_OKCANCEL		4
#define	MB_ABORT		5
#define MB_YESNO		6
#define MB_RETRY		7
#define	MB_TYPE			0x0f		/* message type */
#define MB_BEEP			0x10
#define MB_CAPTION		0x20		/* 1st param is caption */
#ifdef	HELP_BUTTON
#define MB_NOHELP		0x8000
#endif	// HELP_BUTTON
#endif /*!NOMB*/

/* for Special MessageBox */
extern BYTE FAR * PASCAL lpbWorkTemp;	/* App should never use directly */

#define	InitSpecialMessageBox(lpbBuff)	\
	{				\
	Assert(lpbWorkTemp == NULL);	\
	lpbWorkTemp = (lpbBuff);	\
	}

#define	EndSpecialMessageBox()		\
	{				\
	Assert(lpbWorkTemp != NULL);	\
	lpbWorkTemp = NULL;		\
	}


/***END_PUBLIC***/
