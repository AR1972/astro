/*
	COW : Character Oriented Windows

	uisa.h : Master table of isa's
*/

/***BEGIN_PUBLIC***/
#ifndef NOCOLOR

/*	* General colors */
#define	isaBackground		0

#define	isaHilite		1		/* hilite / inversion */
#define	isaGreyed		2		/* not currently used */
#define	isaEnabled		3
#define	isaDisabled		4
#define	isaAlert		5

/*	* Dialog elements :	*/
#define	isaDialogBox		6	/* the actual dialog box */
#define isaStatic		isaDialogBox	/* static text */
#define isaButton		isaDialogBox	/* radio/check buttons */
#define isaPushButton		7		/* push buttons */
#define isaButtonDown		8		/* pushed button */
#define isaListBox		9		/* listbox background */
#define isaEdit			isaDialogBox

/*	* Scroll Bars :		*/
#define isaScrollbar		10
#define isaElevator		11

/*	* Menus :		*/
#define isaMenuBox		12		/* box around pull downs */
#define	isaMenu			13		/* non-selected MENU */
#define isaMenuSelected		14		/* selected menu item */
#define	isaMenuHilite		15		/* hilited character */
/* hilited character under selection */
#define	isaMenuHiliteSel	16		/* for menu titles */
#define	isaItemHiliteSel	17		/* for menu items */

#define	isaDialogAccel		18		/* dialog accelerators */

/*	* Shadows :		*/
#define	isaShadow		19

/* User Colors :		*/
#define	isaUserMin		20
#define	isaUserMax		(isaUserMin+16)
#define isaMax			isaUserMax

#endif /*!NOCOLOR*/
/***END_PUBLIC***/


#ifdef COW


/*	* Message boxes :	*/
#define	isaMessageBox		isaAlert

/*	* Buttons :		*/
#define isaButtonDisabled	isaDisabled

/*	* Edit Items :		*/
#define isaHiliteEdit		isaHilite

/*	* Listboxes :		*/
#define isaListBoxSelection 	isaHilite

/*	* Menus :		*/
#define isaEnabledMenuItem	isaEnabled	/* enabled item */
#define isaDisabledMenuItem	isaDisabled	/* disabled item */
#define isaSelectedMenuItem	isaHilite	/* selected item */


#endif /*COW*/

