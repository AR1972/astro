
#define F_AUTOSTART 0x10

/*
 *  Exit flags.
 */
#define EF_RESTART  0x01        // User allowed option to start windows.
#define EF_REBOOT   0x02        // User needs to reboot
#define EF_DOS      0x04        // User should be returned to DOS
#define EF_NOEXIT   0x08		  // Can't allow an exit without rebooting

/*
 *   Work specification flags, used in DlgInit() calls.
 */

#define    DLGINIT_REMOVE_CLOSE_MENU    0x01

/* Misc stuff. */

#define MAXSTR             128
#define MAX_RES            256

extern WORD	fExit;
extern HWND hwndParent;
extern HANDLE hInstWS;

/*
 *  **** Publics living in WSEXIT.C ***
 */

void PUBLIC CleanUpDroppings(void);


// need to get these functions

WORD PUBLIC wsInsertDisk(LPSTR sz);
int  PUBLIC atoi(PSTR sz);


// Functions and global variables added for DOS/Windows merge defined in 
// wsdosdlg.c

extern BOOL	bIsQuick;
extern BOOL bIsUpgrade;

BOOL PUBLIC wsUninstallDlg( HWND, unsigned, WORD, LONG );
WORD PUBLIC wsInsertUninstall( char *szDiskLabel );
BOOL PUBLIC dsWelcomeDlg(HWND hDlg, unsigned uiMessage, WORD wParam, long lParam);

WORD PUBLIC wsSelectDskFmt( int iDriveType );

BOOL FAR PASCAL wsDskTypeDlg( HWND, unsigned, WORD, LONG );

void PUBLIC wsFatalError( WORD wType, WORD WillReboot ); 
BOOL FAR PASCAL wsFatalErrorDlg( HWND, unsigned, WORD, LONG );

void PUBLIC wsFmtDlgOpen( void );
void PUBLIC wsFmtDlgClose( void );
void PUBLIC wsFmtDlgUpdate( WORD wPercent );
BOOL FAR PASCAL wsFmtProDlg( HWND, unsigned, WORD, LONG );

void PUBLIC wsDosCopyError( char *szFile, WORD wErrorType, WORD Reboot );
BOOL FAR PASCAL wsDosCpyErrDlg( HWND, unsigned, WORD, LONG );

void PUBLIC wsDskError( WORD wErrorDlg );
BOOL FAR PASCAL wsDskErrorDlg( HWND, unsigned, WORD, LONG );

void PUBLIC wsDskPrepError( WORD wErrorType );
BOOL FAR PASCAL wsDskPrepErrorDlg( HWND, unsigned, WORD, LONG );

BOOL FAR PASCAL wsQueryStartDlg( HWND, unsigned, WORD, LONG );

BOOL FAR PASCAL wsBadDosExitDlg( HWND, unsigned, WORD, LONG );

void PUBLIC wsNonFatalError( WORD wMsgID );
void PUBLIC wsFatalError( WORD wType, WORD WillReboot );

void PUBLIC dsEndWait(void);
void PUBLIC dsStartWait(void);

/* RESOLVE.C */

int	FAR    fDialog(int, HWND, FARPROC);
int   FAR    fDialogWithlParam(int, HWND, FARPROC, DWORD);
void  PUBLIC wsDlgInit(HWND hDlg, WORD wWorkFlags);
BOOL 	PUBLIC QueryExit(HWND);
BOOL 	PUBLIC wsYield(HWND);
PSTR 	PUBLIC wsLoadSz(int ids, PSTR pch, int iBufLen);
void 	PUBLIC EnableExit(BOOL); /*BUGBUG is this required*/
void	PUBLIC AppQuit(void);
BOOL 	PUBLIC wsDiskDlg(HWND,unsigned,WORD,LONG);
BOOL 	PUBLIC wsErrorDlg(HWND,unsigned,WORD,LONG);
int  	PUBLIC atoi(PSTR sz);
BOOL 	PUBLIC wsExitDlg(HWND,unsigned,WORD,LONG);

void GetDPTVec(void);
LONG EXPORT wsDefSetupDlgProc(HWND, WORD, WORD, LONG);
BOOL PUBLIC wsExitInit(void);
VOID PUBLIC wsExitTerm(HWND);



