/*
	COW : Character Oriented Windows

	menu.h : menu specific stuff
*/


#define	vkMenuPrefix	VK_MENU

#define	idNil		((WORD) -2)	/* allow -1 for wrap backwards */
#define	imenuNil	idNil
#define	iitemNil	idNil



/* Menu Modes : HIWORD(lParam) for WM_COMMAND / WM_MENUSELECT messages */
/* should be in cwindows.h somewhere .... */
#define	mmdItem		0		/* dropdown item */
#define	mmdAccel	1		/* keyboard accelerator */
#define	mmdMenu		2		/* dropdown not item */



/* Menu processing is locked out by changing the filter proc */
PRIVATE BOOL FARPRIVATE		MenuFilterProc(PMSG);
#define FMenuAllowed() (pfnFilter == MenuFilterProc)


typedef struct _mni
	{
	WORD	imenu;			/* current menu selected */
	WORD	iitem;			/* current item */
	MENU	*pmenuOpen;		/* Open menu, or NULL */
	BITS	fMenuMnem:1;		/* mnemonics on ? */
	BITS	fMouse:1;		/* Mouse mode ? */
	BITS	fMouseOnTitle:1;	/* Mouse stay on title ? */
	} MNI;	/* Menu Information */

PRIVATE MNI mniCur;

#define FMenuActive()		(mniCur.imenu != imenuNil)
#define FMenuOpen()		(mniCur.pmenuOpen != NULL)

extern PMENUBAR pmenubarCur;
