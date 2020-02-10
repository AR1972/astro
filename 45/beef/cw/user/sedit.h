/*
	COW : Character Oriented Windows

	sedit.h : single line edit header
	-- single line edit intimately tied with SDM
*/

/*****************************************************************************/
/* extra word usage (filled by SDM) */

#define	isaEb		rgwExtra[cwExtraText+0]
				/* color */
#define	isaSelEb	rgwExtra[cwExtraText+1]
				/* selected color */
#define chFillDialog	rgwExtra[cwExtraText+2]
				/* fill char for trailing spaces */
#define ichMacEb	rgwExtra[cwExtraText+3]
				/* last character in edit buffer */
#define ichLeftEb	rgwExtra[cwExtraText+4]
				/* leftmost character displayed */
#define ichCursorEb	rgwExtra[cwExtraText+5]
				/* current cursor position, to the left of
				    insertion point */
#define ichSelEb	rgwExtra[cwExtraText+6]
				/* start of selection */
#define	fNoBracketEb	rgwExtra[cwExtraText+7]
				/* don't show brackets ?? */
#define wEb		rgwExtra[cwExtraText+8]
				/* random flags */
#define cchMaxEb	rgwExtra[cwExtraText+9]
				/* for fixed length edit items */

/* option for retaining selection : same as no brackets !!! */
#define	fRetainSelEb	fNoBracketEb

#if cwExtraEdit != cwExtraText+10
.....
#endif

/*****************************************************************************/
