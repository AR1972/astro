/*
	COW : Character Oriented Windows

	scroll.h : scroll bar cow private interface
*/


#define cwExtraScroll	5	/* size of rgwExtra for scroll windows */

/* scroll bar definitions */

#define ctickRepSb 	rgwExtra[0]	/* # of ticks to repeat */
#define ptCurSb 	rgwExtra[1]	/* current position on scroll line */
#define ptMinSb 	rgwExtra[2]	/* minimum position on scroll line */
#define ptMaxSb 	rgwExtra[3]	/* end position on scroll line */
#define ptElevatorSb	rgwExtra[4]	/* elevator position (lower byte) */

/*	Repeat timing values :
 *	ctickRepScrollStart	: time till start scrolling
 *	ctickRepScrollDefault	: repeat rate (default SDM scroll bars)
 *	ctickRepList		: repeat rate (for listbox contents), slow
 *					(fast is 2*)
 *
 *	ctick values are in 1/18 th sec
*/

#define ctickRepScrollStart	9		/* 1/2 second */
#define ctickRepScrollDefault	1		/* as fast as possible */
#define ctickRepList		2

/* private message */
#define SB_UPCLICK	15
