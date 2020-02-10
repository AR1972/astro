/*
	COW : Character Oriented Windows

	uevent.h : Definitions for User Windows
*/

/***BEGIN_PUBLIC***/

#ifndef NOMSG
/* Message structure */
typedef struct _msg
	{
	PWND	pwnd;
	WORD	message;
	WORD	wParam;
	DWORD	lParam;
	DWORD	time;
	} MSG;
typedef MSG *PMSG;
#ifndef DOS5
extern BOOL PASCAL fMessage;
#ifdef DUAL
extern DWORD PASCAL semaMessage;	/* message semaphore */
#define	hsemaMessage	((DWORD) (DWORD FAR *) &semaMessage)
#endif /*DUAL*/
#else
extern DWORD PASCAL semaMessage;	/* message semaphore */
#define	hsemaMessage	((DWORD) (DWORD FAR *) &semaMessage)
#endif
#endif /*!NOMSG*/


#ifndef NOWINMESSAGES
/* Window Messages */
#define	WM_NULL			0x0000
#define	WM_CREATE		0x0001
#define	WM_WANTFOCUS		0x0005
#define	WM_MAKEACTIVE		0x0006
#define	WM_SETFOCUS		0x0007
#define	WM_KILLFOCUS		0x0008
#define	WM_PAINT		0x000f
#define	WM_QUIT			0x0012

#define	WM_KEYFIRST		0x0100
#define WM_KEYLAST		0x0102

#define	WM_KEYDOWN		0x0100
#define	WM_KEYUP		0x0101
#define	WM_CHAR			0x0102

#define	WM_CUT			0x0103
#define	WM_PASTE		0x0104
#define	WM_COPY			0x0105
#define	WM_INSERT		0x0106

#define	WM_MENUIDLE		0x0110		/* Menu Idle */
#define	WM_COMMAND		0x0111
#define	WM_MENUSELECT		0x0112		/* selecting a menu item */
#define	WM_MENUSTART		0x0113		/* starting a menu action */
#define	WM_HSCROLL		0x0114
#define	WM_VSCROLL		0x0115
#define	WM_INITMENUPOPUP	0x0116
#define	WM_ALARM		0x0117

#define	WM_MOUSEFIRST		0x0200
#define	WM_LMOUSELAST		0x0203	/* last of Left mouse actions */
#define	WM_MOUSELAST		0x0206

#define	WM_MOUSEMOVE		0x0200	/* mouse related constants */
#define	WM_LBUTTONDOWN		0x0201
#define	WM_LBUTTONUP		0x0202
#define	WM_LBUTTONDBLCLK	0x0203
#define	WM_RBUTTONDOWN		0x0204
#define	WM_RBUTTONUP		0x0205
#define	WM_RBUTTONDBLCLK	0x0206

/* Edit Wnd Proc Messages */
#define EM_SETSEL		0x0300
#define EM_GETSEL		0x0301

/* Overlap Wnd Proc Messages */
#define WM_ACTIVATE	0x0320
#define WM_ZOOM		0x0321
#define WM_CLOSE	0x0322
#define WM_MOVE		0x0323
#define WM_SIZE		0x0324


/*WM-DIALOG is actually private (and LB_SETWIDTH)*/
#define WM_DIALOG		0x0380
/* Listbox Proc Messages */
#define WM_LISTBOX_COMMAND	WM_DIALOG
#define LB_RESETCONTENT		0x0340
#define LB_ADDSTRING		0x0341
#define LB_DELETESTRING		0x0342
#define LB_SETCURSEL		0x0343
#define LB_GETCURSEL		0x0344
#ifdef LISTBOX_HORIZ
#define LB_SETWIDTH		0x0345
#endif /*LISTBOX_HORIZ*/
#define LB_GETTEXT		0x0346
#define LB_GETCOUNT		0x0347
#define LB_REPLACESTRING	0x0348
#define LB_INSERTSTRING		0x0349

/* private window messages start here */
#define	WM_USER			0x0400
#endif

/* Window's aliases for KK_ states */
#define	KK_MENU			KK_ALT
#define	KK_CAPITAL		KK_CAPLOCK

/* HELP */
#define	VK_HELP_KEY		VK_F1

/* listbox notification codes */
#define LBN_SELCHANGE		0
#define LBN_DBLCLK		1
#define LBN_SELECT_DONE		2


/* List Box Selection Codes */
#define lbrCause		0xf
#define lbrNone			0
#define lbrMouse		1
#define lbrScroll		2
#define lbrKeys			3
#define lbrSpace		4
#define lbrOther		5
#define flbrReselect		0x10


/* help types/contexts */
#define	hemMenu			1
#define	hemMenuItem		2
#define	hemMbox			3
#define	hemDialog		4
#define	hemUserMin		0x10		/* For Application contexts */

/* help id's for message boxes */
#ifndef NOMB
#define	hidMboxOk		MB_OK
#define	hidMboxYesNoCancel	MB_YESNOCANCEL
#define	hidMboxRetryCancel	MB_RETRYCANCEL
#define	hidMboxOkCancel		MB_OKCANCEL
#define	hidMboxAbort		MB_ABORT
#define hidMboxYesNo		MB_YESNO
#define hidMboxRetry		MB_RETRY
#endif /*!NOMB*/

VOID		FARPUBLIC SetAlarm(PWND, WORD);
VOID		FARPUBLIC KillAlarm(void);
VOID		FARPUBLIC UngetMessage(PMSG);
BOOL		FARPUBLIC PeekMessage(PMSG);
PWND		FARPUBLIC GetFocus(void);
PWND		FARPUBLIC SetFocus(PWND);
VOID		FARPUBLIC FlushAbort(void);
PWND		FARPUBLIC SetCapture(PWND);
VOID 		FARPUBLIC ReleaseCapture(void);
DWORD		FARPUBLIC DispatchMessage(PMSG);
BOOL		FARPUBLIC PostMessage(PWND, WORD, WORD, DWORD);
DWORD		FARPUBLIC SendMessage(PWND, WORD, WORD, DWORD);
WORD		FARPUBLIC SetDoubleClickTime(WORD);		/*OPTIONAL*/
VOID		FARPUBLIC SynthesizeShiftKeys(WORD, WORD);	/*OPTIONAL*/

/***END_PUBLIC***/

#ifdef COW

/* window if alarm set (used by timer) */
extern PWND pwndAlarm;

#ifdef KANJI
VOID		FAR PASCAL KeyboardMessage(BYTE, BYTE, WORD, WORD, BOOL);
#else
VOID		FAR PASCAL KeyboardMessage(BYTE, WORD, WORD, BOOL);
#endif
VOID		FAR PASCAL SpecialTsrAbort(void);
BOOL		FAR PASCAL FTestKeyboardEmpty(void);
VOID		FAR PASCAL FlushKeyEvents(VOID);

#endif
