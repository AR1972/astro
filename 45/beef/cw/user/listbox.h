/*
	COW : Character Oriented Windows

	listbox.h : listbox cow private interface
*/

#define	PwfnCtlLb(pwnd)	(*((PWFN_CTL *) &pwnd->rgwExtra[cwExtraMin]))

/* Listbox Styles */

/*#define LBS_SORT  1 (defined in uwindow.h) */
#define LBS_NOTIFY			0x10

