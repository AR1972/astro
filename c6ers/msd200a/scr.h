/*
	scr.h : screen / kanji tests
*/

VOID FAR PASCAL InitModeMapping(void);
BOOL FAR	FInitScreenMode(WORD);
VOID FAR	SelectMode(void);
VOID FAR	SaveOriginalScreen(void);
BOOL FAR	FRestoreOriginalScreen(void);

WORD FAR * FAR PASCAL LpwAllocDriverMem(WORD, WORD);
VOID FAR PASCAL FreeDriverMem(WORD FAR *);
