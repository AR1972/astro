/*
	COW : Character Oriented Windows

	_msgbox.h : message box private structures
*/


#define	mbMin		1		/* one based request #s */

typedef struct _mbi
	{
	char *	sz;
	WORD	cch;
	RX	rx;
	} MBI;		/* Message Box Info */

#define imbiMax	3		/* 3 lines max */

typedef struct _mbs
	{
	char *  szTitle;		/* dialog title (NULL if none) */
	WORD	mbSimple;		/* MB_CAPTION and MB_BEEP removed */
#ifdef	HELP_BUTTON
	WORD	fNoHelp;		/* MB_NOHELP bit set */
#endif	// HELP_BUTTON
	WORD	imbiMac;		/* # of strings */
	WORD	cchMac;			/* max width of text */
	MBI	rgmbi[imbiMax];		/* string details */
	} MBS;		/* message box state */


/* height overhead of a message box */
#ifdef DIALOG_LINE
#define	dayMsgBox	5
#else
#define	dayMsgBox	4
#endif
