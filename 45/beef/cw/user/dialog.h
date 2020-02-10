/*
    	COW : Character Oriented Windows

	dialog.h: Dialog cow private interface
*/

/* special characters in dialog static strings */
#define	chPrefix1	'~'		/* tilde => accelerator */
#define	chPrefix2	((char) 247)	/* double tilde => hidden accelerator */

extern BYTE fShowDlgAccel;	/* show dialog accelerators ? */
extern BYTE fButtonAction;	/* TRUE => mouse button down on button */
extern PWND pwndCapture;

#define	FCaptured(pwnd)	((pwnd) == pwndCapture)

PRIVATE	BYTE	fDrawItem;
PRIVATE	BYTE	fRedrawItem;


/* WndProcs must be the same size as all public entries */
DWORD	FARPUBLIC	DialogWndProc(PWND, WORD, WORD, DWORD);

WORD	FARPRIVATE	DialogBox(PWND, PLFN);
VOID	FARPRIVATE	EndDialog(PWND, WORD);
WORD	FARPRIVATE	GetDlgItemText(PWND, char *, WORD);
VOID	FARPRIVATE	SetDlgItemText(PWND, char *, BOOL);

VOID	FARPRIVATE	DlgDirList(PWND, char *, PWND, BOOL, PWND);
BOOL	FARPRIVATE	DlgDirSelect(PWND, char *, PWND);
BOOL	FARPRIVATE	FMaybeDir(char *);

VOID	FARPRIVATE	AddListString(PWND, char *);
WORD	FARPRIVATE 	GetListText(PWND, char *, WORD);

WORD	FARPRIVATE 	CwSizeDialog(ARC *);
#define	CbSizeDialog(parc) (CwSizeDialog(parc)<<1)


/* Common portion of all dialog windows */
#define	cwExtraMin	1		/* at least 1 field for all items */
					/* not used for dialog box */
#define	aclDialog	rgwExtra[0]	/* dialog accelerator */

/* Text info for : edit, static and button controls */
#define	cwExtraText	(cwExtraMin+2)	/* 2 more for text controls */
#define szDialog	rgwExtra[1]	/* also Dialog Box caption */
#define cchDialog	rgwExtra[2]	/* edit, static & buttons */

/* flag indicating whether EndDialog has been
   called.  Used to prevent multiple calls
   to PostQuitMessage */
#define wParamEnd		rgwExtra[2]


/* sizes of rgwExtra for the various controls */

#define cwExtraDialog	(cwExtraMin+2)	/* szDialog + fEndDb */

#define cwExtraStatic	(cwExtraText)	/* simple text */
#define cwExtraButton	(cwExtraText+2)	/* text + button state + next radio */
#define cwExtraEdit	(cwExtraText+10)	/* see edit.h for details */
#define	cwExtraGeneral	(cwExtraMin+2)	/* see general.h for details */

#ifndef LISTBOX_HORIZ
#define cwExtraListBox	(cwExtraMin+14)	/* see _listbox.h for details */
#else
#define cwExtraListBox	(cwExtraMin+16)	/* see _listbox.h for details */
#endif /*LISTBOX_HORIZ*/


/* Dialog Accelerator control */
#define	aclNil		0	/* no accelerator */
#ifndef KANJI
/* non-Kanji : single character within first 16 characters */
#define	ChAccel(pwnd)	LOBYTE((pwnd)->aclDialog)
#define	IchAccel(pwnd)	HIBYTE((pwnd)->aclDialog)
#else
/* Kanji : either Roman or Kana - first character only */
#define	ChAccel(pwnd)	(fKanaAccel ? HIBYTE(pwnd->aclDialog) : \
			    LOBYTE(pwnd->aclDialog))
#define	IchAccel(pwnd)	0
#endif

