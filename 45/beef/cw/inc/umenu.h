/*
		COW: Character Oriented Windos
		
		umenu.h : Definitions for User Menu 
*/		

/***BEGIN_PUBLIC***/

#ifndef NOMENUS
/* hack for nameless unions in CC */
#ifdef CC
#define CC_USZ u
#define CC_URG u
#else
#define CC_USZ
#define CC_URG
#endif


typedef struct _mpvkeyid
	{
	WORD	vkey;
	WORD	idItem;
	} MPVKEYID;
#define VkeyOfVkKk(vk, kk)	((vk) | (kk))
typedef struct _menuitem
	{
	WORD	idItem;			/* id for menuitem */
	BITS	fEnabled:1;		/* TRUE => enabled, FALSE => greyed */
	BITS	fChecked:1;		/* TRUE => checked */
	BITS	fSeparator:1;		/* TRUE => separator */
	BITS	fHandle:1;		/* TRUE => use pszItem, else szItem */
	BITS	ichHilite:4;		/* index of prefix character */
	BITS	bParamUser:8;		/* available for application use */
	union
		{
		char *szItem;
		char **pszItem;
		} CC_USZ;
	WORD	wParamUser;		/* available for application use */
#ifdef KANJI
	WORD	chKanaAccel;		/* Kana Accelerators */
#endif /*KANJI*/
	} MENUITEM;
typedef struct _menu
	{
	WORD	idMenu;
	BITS	rxTitle:8;
	BITS	ichHilite:4;		/* index of prefix character */
	BITS	fHandle:1;
	BITS	fEnabled:1;		/* is menu 'enabled' (not greyed) */
	BITS	filler:2;
	WORD	cchTitle;
	char	*pchTitle;
	WORD	citem;
	WORD	cchitemMax;
	union
		{
		MENUITEM *rgmenuitem;
		MENUITEM **prgmenuitem;
		} CC_URG;
	WORD	wParamUser;		/* available for application use */
#ifdef KANJI
	WORD	chKanaAccel;		/* Kana Accelerators */
#endif /*KANJI*/
	} MENU;
typedef struct _menubar
	{
	WORD	cmenu;
	MENU	*rgmenu;
	MPVKEYID *rgmpvkeyid;
	} MENUBAR;

typedef MENU *PMENU;
typedef MENUITEM *PMENUITEM;
typedef MENUBAR *PMENUBAR;

/* menu item define macros :
	D => disabled
	H => string handle
	X => with index (otherwise default to 0)
*/

#ifndef KANJI
/* enabled simple item */
#define menuitem(mid, sz, w) {(mid), TRUE, FALSE, FALSE, FALSE,\
	0, 0, {(sz)}, (WORD) (w)},
#define menuitemD(mid, sz, w) {(mid), FALSE, FALSE, FALSE, FALSE,\
	0, 0, {(sz)}, (WORD) (w)},
#define menuitemDH(mid, psz, w) {(mid), FALSE, FALSE, FALSE, TRUE,\
	0, 0, {(char *)(psz)}, (WORD)(w)},
#define menuitemX(mid, sz, ich, w) {(mid), TRUE, FALSE, FALSE, FALSE,\
	(ich), 0, {(sz)}, (WORD) (w)},
#define menuitemDX(mid, sz, ich, w) {(mid), FALSE, FALSE, FALSE, FALSE,\
	(ich), 0, {(sz)}, (WORD) (w)},
/* separator */
#define	menuitemSep	{0, FALSE, FALSE, TRUE, FALSE, 0, 0, {NULL}, 0},

#else	/* KANJI - 1 extra value needed for Kana Accelerator */
	/* Accelerator should always be at start */

#define menuitem(mid, sz, chKana, w) {(mid), TRUE, FALSE, FALSE, FALSE,\
	0, 0, {(sz)}, (WORD) (w), (WORD) (chKana)},
#define menuitemD(mid, sz, chKana, w) {(mid), FALSE, FALSE, FALSE, FALSE,\
	0, 0, {(sz)}, (WORD) (w), (WORD) (chKana)},
#define	menuitemSep	{0, FALSE, FALSE, TRUE, FALSE, 0, 0, {NULL}, 0, 0},

#endif /*KANJI*/

#endif /*!NOMENUS*/

VOID		FARPUBLIC InitMenu(PWND, PMENUBAR);
BOOL		FARPUBLIC FEnableMenuBar(BOOL);
VOID		FARPUBLIC EnableMenu(WORD, BOOL);
VOID		FARPUBLIC EnableMenuItem(WORD, BOOL);
VOID		FARPUBLIC CheckMenuItem(WORD, BOOL);
BOOL		FARPUBLIC FMenuItemChecked(WORD);
PMENUITEM	FARPUBLIC FindMenuItem(WORD);
VOID		FARPUBLIC DrawMenubar(void);
VOID		FARPUBLIC OpenMenu(WORD);		/*OPTIONAL*/

/***END_PUBLIC***/
