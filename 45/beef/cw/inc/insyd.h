/*
	CW: Character Windows
	
	insyd.h: SYD (System Feature) Driver specifics
*/

/*****************************************************************************/
/***BEGIN_PUBLIC***/

/* System Specifics */
VOID		FARPUBLIC DoSound(WORD);
DWORD		FARPUBLIC ClockTicks(void);

/***END_PUBLIC***/

/*****************************************************************************/
/* indtSystem : loaded representation */

/*
  -- the INYJ structure contains far pointers to routines in an installable
	driver
*/

/* Prototypes for functions */
typedef	VOID	(FAR PASCAL *LPFN_SY_SOUND)(WORD);
typedef	DWORD	(FAR PASCAL *LPFN_SY_TIME)(VOID);


typedef	struct _inyj
	{
	LPFN_SY_SOUND	lpfnDoSoundSyd;
	LPFN_SY_TIME	lpfnLGetTimeSyd;
	} INYJ; /* Installable sYstem Jump table */

#define	cpfnSydMin	2

/*****************************************************************************/
