/*
	COW : Character Oriented Windows

	strings.h : string pool (+special message-box stuff)

	-- use these variables if string is duplicated, otherwise use the
		explicit #defines as found in <itl.h>
*/

/* PRIVATE */
extern char szEmptyString[];

extern char szOkString[];
extern char szCancelString[];

extern char szYesString[];
extern char szNoString[];
extern char szRetryString[];
extern char szAbortString[];
#ifdef HELP_BUTTON
extern char szHelpString[];
#endif


/* international info for message box */

typedef struct _mbbi 
	{
	BYTE	cch;		/* Size of the string */
	BYTE	drx;		/* Size of the window */
	BYTE	chAccel;	/* The accelerator to user */
	} MBB;

extern BYTE mpmbcchButton[];
extern MBB rgmbb[];
