/*
	CW: Character Oriented Windows
	
	inkbd.h: Installable keyboard driver CW info
*/

/***BEGIN_PUBLIC***/
/* Key state masks for keyboard messages (in HIWORD(lParam)) */
#define	KK_EXTENDED		0x8000	/* from extended keypad usually */
/* shifts */
#define	KK_CAPLOCK		0x4000
#define	KK_NUMLOCK		0x2000
#define	KK_SCRLOCK		0x1000
#define	KK_ALT			0x0800
#define	KK_CONTROL		0x0400
#define	KK_SHIFT		0x0200

/* for WM_CHAR */
#define	KK_VK			0x01ff	/* mask to get untranslated VK_ */

/* for WM_KEYUP / WM_KEYDOWN */
#define	KK_SC			0x00ff	/* mask to get scan code */

#ifdef KANJI
#define KJ_SC			0xff00
#define KJ_KANA 		0x0080
#define KJ_OTHER		0x0040
#define KJ_KK			0x00b0
#define KJ_COUNT		0x003f
#endif

/***END_PUBLIC***/

/*****************************************************************************/
/* keyboard information structure (passed to driver) */

typedef VOID		(FAR PASCAL *LPFN_KBD_MSG)(BYTE, WORD, WORD, BOOL);
typedef BOOL		(FAR PASCAL *LPFN_KBD_TEST)(void);
typedef BOOL		(FAR PASCAL *LPFN_KBD_FALTD)(void);
typedef VOID		(FAR PASCAL *LPFN_KBD_UPD)(BOOL, BOOL, BOOL);
typedef VOID		(FAR PASCAL *LPFN_KBD_ABORT)(void);


typedef struct _inkb	/* keyboard info */
	{
	/* call back functions */
	LPFN_KBD_MSG	lpfnKeyboardMessage;
	LPFN_KBD_TEST	lpfnFTestKeyboardEmpty;
	LPFN_KBD_ABORT	lpfnSpecialAbort;

	/* public flags */
	BOOL	fAbort;			/* maps to global flag */
	BOOL	fPollKeyboard;		/* maps to global flag */
	BYTE	fKeyIsUp;		/* maps to global flag */
	BYTE	fKeyWasUp;		/* maps to global flag */
	WORD	wRateKeyRepeat;		/* maps to global flag */

	/* private CW flags */
	WORD	fNormalKeyboard;	/* => no TSR resident */
	BYTE	fNonAltKeyHit;		/* => non alt key hit */

	WORD	fDisableExtended;	/* => don't do extended bios calls */
	} INKB;

extern INKB	inkb;

/*****************************************************************************/
/* CW private info */

typedef VOID		(FAR PASCAL *LPFN_KBD_ENABLE)(INKB *, BOOL, BOOL);
typedef VOID		(FAR PASCAL *LPFN_KBD_POLL)(void);
typedef VOID		(FAR PASCAL *LPFN_KBD_FLUSH)(char *);
typedef WORD		(FAR PASCAL *LPFN_KBD_MKGET)(void);
typedef VOID		(FAR PASCAL *LPFN_KBD_SETKK)(WORD);
typedef char		(FAR PASCAL *LPFN_KBD_CHALT)(char);

typedef struct _inkj	/* keyboard jump table */
	{
	LPFN_KBD_ENABLE	pfnEnableKeyboardKbd;
	LPFN_KBD_POLL	pfnPollKeyboardKbd;
	LPFN_KBD_FLUSH	pfnFlushKeyRgchKbd;
	LPFN_KBD_MKGET	pfnMkGetShiftStatesKbd;
	LPFN_KBD_SETKK	pfnSetShiftKkKbd;
	LPFN_KBD_CHALT	pfnChAlternateKeytopKbd;	/* KANJI only */
	} INKJ;

/* indtKeyboard service */
#define	cpfnKbdMin	6

/*****************************************************************************/
