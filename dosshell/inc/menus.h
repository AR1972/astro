;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#ifndef OLDCW
typedef HMNU MENUINFO;

#else

#define MAXITEMS 70    /* maximum number of menu items */
#define MAXMENUS 15    /* maximum number of menus      */
#define MENUSPACE 4    /* number of spaces between menu headers */

typedef struct _menuinfo{
	MENUBAR   bar;
	MENU	  themenu[MAXMENUS];
	MENUITEM  theitem[MAXITEMS];
	MPVKEYID  thekey[MAXITEMS];
	int	  (*menuproc[MAXITEMS])();
	WORD	  nummenus;
	WORD	  numitems;
	WORD	  menudx;
	} MENUINFO;

extern MENUINFO *curbar;
#endif
