/*
	COW : Character Oriented Windows

	_menu.h : menu private info
*/

/* Menu Bar */
#define ayMenu		((AY) 0)	/* menu at top of screen */
#define axMenuMin	((AX) 0)	/* start at left of screen */
#define axMenuMac	axMac 		/* use entire width */

/* Menu Items */
#define	rxMenuMark	((RX) daxBorder)   /* where check goes */
#define	rxMenuText	((RX) daxBorder+1) /* where text starts */
#define	daxMenuBox	(-daxBorder*2)	 /* left of menu title to left of box */

#define chMenuMark	chBullet	/* marked (checked) items */
