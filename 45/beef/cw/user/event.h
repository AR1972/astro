/*
	COW : Character Oriented Windows

	event.h : event cow private interface
*/

/* internal COW control messages */
#define	WM_INITDIALOG			0x0381
#define	WM_IDLE				0x0382	/* DIALOG IDLE */
#define	WM_HELP				0x0383	/* DIALOG HELP */
#define	WM_ENDDIALOG			0x0384
#define	WM_KEYSTATE			0x0385	/* Change in shift states */
#define	WM_DIALOG_SETFOCUS		0x0386
#define	WM_DIALOG_KILLFOCUS		0x0387

/*PRIVATE*/
WORD		FARPRIVATE SendMessageShort(PWND, WORD);
VOID		FARPRIVATE RePostMouseMessage(WORD, WORD, DWORD);
BOOL		FARPRIVATE DummyFilter(struct _msg *);
/* alarm */
BOOL		FARPRIVATE FCheckAlarm(struct _msg *);
VOID		FARPRIVATE UndoRepeat(WORD, DWORD);
VOID		FARPRIVATE PostMouseUpMessages(void);

VOID		FAR PASCAL MouseMessage(WORD);

typedef union _msp
	{
	struct
		{
		BYTE rx;
		BYTE ry;
		BYTE ax;
		BYTE ay;
		} s;
	DWORD lParam;
	} MSP;	/* Mouse position (maps to lParam) */


typedef BOOL	(FARPRIVATE *PFFN_FILTER)(PMSG);	/* filter */
extern PFFN_FILTER pfnFilter;
extern PWND PASCAL pwndFocus;
#ifdef WINDOW_OVERLAP
extern PFFN_FILTER pfnOverlapFilter;
#endif /*WINDOW_OVERLAP*/

#define	VkOfVw(vw)	((WORD) (vw) + VK_MIN)
