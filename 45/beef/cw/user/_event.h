/*
	COW : Character Oriented Windows

	_event.h : event specific stuff
*/

#ifdef	MSG_LIMIT_SIZE
#define imsgMax 8		/* 8 key type ahead */
#else	// MSG_LIMIT_SIZE
#define imsgMax 16		/* 16 key type ahead */
#endif	// MSG_LIMIT_SIZE

typedef struct _msgq
	{
	WORD	cmsg;			/* # of messages */
	PMSG	pmsgHead;		/* head of list */
	PMSG	pmsgNext;		/* after tail (next free) */
	MSG	rgmsg[imsgMax];
	} MSGQ;

#define timeMsgMax 0x7fffffffl

#define	timeDoubleClickDefault	10	/* >.5 sec */

// defined in eventlow.c
extern MSGQ msgqAppl, msgqKeyboard, msgqMouse;
extern PMSG pmsgLast;
extern PWND pwndCapture;
extern MSP mspPrev;
extern BYTE fUseCache;
extern MSG msgCache;
extern WORD timeDoubleClick;

PRIVATE BOOL FQueueMsg(MSGQ *, PWND, WORD, WORD, DWORD, DWORD);
PRIVATE VOID FindMouseWnd(MSG *);

#ifndef DOS5
extern BOOL PASCAL fMessage;
#define	SetMessage()	{ fMessage = TRUE; }
#define	ClearMessage()	{ fMessage = FALSE; }
#else
/* NOTE : semaphore is CLEARED when a message is ready !! */
extern DWORD PASCAL semaMessage;	/* RAM semaphore */
#define	SetMessage()	DosSemClear(hsemaMessage);	/* message ready */
#define	ClearMessage()	DosSemSet(hsemaMessage);
#endif
