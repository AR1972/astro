/*
	COW : Character Oriented Windows

	_listbox.h : listbox specific stuff
*/

/* customization */
#define	rxListBoxMin		1	/* 1 extra space to left */


/* special on-demand value */
#define	offEmpty		((WORD) -1)


/* extra word usage */

/* 0,1 are listbox function (for on-demand) */
#define iszTopLb 	rgwExtra[cwExtraMin+2]
				/* first item in Display */
#define cszLb		rgwExtra[cwExtraMin+3]
				/* # of strings in list in list */
#define iszCurLb	rgwExtra[cwExtraMin+4]
				/* currently selected item */
#define hmemMpiszoffLb	rgwExtra[cwExtraMin+5]
				/* array of offsets in string buffer*/
#define hmemGpszLb	rgwExtra[cwExtraMin+6]
				/* string buffer pool */
#define offLb 		rgwExtra[cwExtraMin+7]
				/* next string buffer pointer */
#define offMaxLb 	rgwExtra[cwExtraMin+8]
				/* size of string buffer */
#define iszMacLb 	rgwExtra[cwExtraMin+9]
				/* max isz before index buf is grown*/
#define fSelectedLb	rgwExtra[cwExtraMin+10]
				/* do we have a selection ? */
#define isaColor	rgwExtra[cwExtraMin+11]
				/* colour of the listbox */
#define isaHiliteColor	rgwExtra[cwExtraMin+12]
				/* colour of the listbox hilite*/
#define ctickRepLb	rgwExtra[cwExtraMin+13]
				/* scrolling rate */
#ifndef LISTBOX_HORIZ
#define	cwExtraNeeded	(cwExtraMin+14)
#else
/* Horizontal scrolling extra info */
#define	drxItemLb	rgwExtra[cwExtraMin+14]
				/* width of a single item (add 1 for space) */
#define	citemWidthLb	rgwExtra[cwExtraMin+15]
				/* max # of items wide */
#define	cwExtraNeeded	(cwExtraMin+16)
#endif /*LISTBOX_HORIZ*/

#if cwExtraListBox != cwExtraNeeded
.....
#endif


#define FSelected(pwnd) ((pwnd)->fSelectedLb)

#define SetFSelected(pwnd, fSelected) (pwnd)->fSelectedLb = (fSelected);

/* info for listbox memory allocation */

#define coffInitLb	48	/* initial size of mpiszoff */
#define coffGrowLb 	32	/* amount to grow index buffer by */
#define cbInitLb 	512	/* initial size of string buffer */
#define cbGrowLb 	256	/* amount to grow string buffer by */


#ifndef LISTBOX_HORIZ
/* Vertical listboxes (only 1 column) */
#define	CitemVisible(pwnd, dry)	(dry)
#else
#define	CitemVisible(pwnd, dry)	((dry) * (pwnd)->citemWidthLb)
/* start of item display */
#define RxLeftItem(pwnd, icol)	\
	((RX) ((BYTE)(icol) * (BYTE) (pwnd->drxItemLb+1) + rxListBoxMin))
#endif

