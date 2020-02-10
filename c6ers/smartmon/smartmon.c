/**************************************************************************/
/***									***/
/***	SMARTMON.C  - SmartDrv 4.0 monitor application main module	***/
/***									***/
/***									***/
/***									***/
/***									***/
/**************************************************************************/

/**************************************************************************/
/***									***/
/***	Global variable declarations and initializations		***/
/***									***/
/**************************************************************************/

#include <windows.h>
#include <shellapi.h>
#include "smartmon.h"

HANDLE	hInst;		    // application instance handle
HWND	hWndMain;	    // application's main window
char	szBuffer[BUFLEN];   // buffer for stringtable stuff
char	szTitle[20];	    // from resource
DWORD	DosCacheSize;	    // Cache size (Kbyte) under DOS
DWORD	WinCacheSize;	    // Cache size (Kbyte) under Windows
WORD	wWinVer;	    // Windows version
BOOL	fIconic;	    // flag to indicate if we're iconized
RECT	rcIconic;	    // window size while iconic
BOOL	fUpdateIconBackground;
BOOL	fAlreadyIdle;	    // cache idle during previous cycle
char	szStartLog[20];     // "Start Log" button label
char	szStopLog[20];	    // "Stop Log" button label
BOOL	fLogging;	    // logging status
HICON	hIcoDrag;	    // icon for dragging
HBITMAP hbmpIdle;	    // idle drive bitmap for icon
HBITMAP hbmpBusy;	    // busy drive bitmap for icon
HCURSOR hCurWait;	    // wait cursor
DWORD	LogStartTime;	    // logging start time
DWORD	LogStopTime;	    // logging auto-stop time
BOOL	fUpdateAuto;	    // change autoexec.bat or not
BOOL	fDriveInfoChanged;  // drive cache setting changed
BOOL	fProfileChanged;    // profile modified since start
BOOL	fTopMost;	    // run SmartMon as topmost window
BOOL	fTopMostChanged;    // save topmost info
char	szBatchFile[MAXFILENAMELEN];
HMENU	hSysMenu;

char	*szProSec = "SmartMon";     // profile section in WIN.INI
char	*szInterval = "Interval";   // profile key
char	*szFreq = "Frequency";	    // profile key
char	*szfStop = "AutoStop";	    // profile key
char	*szLogTime = "LogTime";     // profile key
char	*szUpdateDOS = "UpdateBatch";	 // profile key
char	*szLog = "LogFile";	    // profile key
char	*szBatch = "BatchFile";     // profile key
char	*szTopMost = "TopMost";     // profile key

extern	HBITMAP hbmpIconDrv;// either hbmpIdle or hbmpBusy
extern	WORD Frequency;     // sampling frequency
extern	HFONT hfontStatus;  // font used for text of status bar
extern	char szLogFile[];   // log file name
extern	WORD AutoLogTime;
extern	BOOL fAutoStop;
extern	WORD SpecInterval;
extern	DWORD BaseHits;
extern	DWORD BaseTotal;
extern	WORD AveHitRate;

long FAR PASCAL WambiWndProc( HWND, unsigned, WORD, LONG );
BOOL FAR PASCAL OptionsDlgProc( HWND, WORD, WPARAM, LPARAM );
void DoDriveControl( HWND, WORD, WORD );
void UpdateMemorySize( HWND );
void DoHelp( HWND, UINT, DWORD );
void ForceIconRedraw( HWND );
void CheckAutoStop( HWND );
void GetProfileSettings( void );
void SaveProfileSettings( void );
BOOL CheckForStacker( WORD );
void DoTopMost( HWND );

extern void MeasureItem( HANDLE, LPMEASUREITEMSTRUCT );
extern BOOL wNoRedraw;
extern void DrawItem( HWND, WPARAM, LPDRAWITEMSTRUCT, BOOL );
extern void DrawRateBox( HWND, int );
extern void InitRateBox( HWND );
extern void ForceRateBoxRedraw( HWND );
extern void InitStatusBar( HWND );
extern void UpdateStatusBar( HWND, HDC );
extern int  GetDriveIndex( WORD, WORD );
extern void DrawIconBackground( HDC );
extern void UpdateRateIcon( HWND, HDC );
extern BOOL GetHitRate( void );
extern BOOL StartLog( void );
extern void StopLog( void );
extern BOOL WriteLog( void );
extern void UpdateHelpMessage( int );
extern void CleanupRate( void );
extern void ResetRateChart( HWND );
extern void CheckDriveList( HWND );
extern void ResetDriveBox( HWND );
extern void InitDriveBitmap( void );
extern BOOL MungeBatchFile( void );
extern void ResetRateChartColor( BOOL );

/**************************************************************************/
/***                                                                    ***/
/***	WinMain 							***/
/***                                                                    ***/
/**************************************************************************/

int NEAR PASCAL WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
HANDLE hInstance;	// handle for this instance
HANDLE hPrevInstance;	// handle for possible previous instances
LPSTR lpszCmdLine;	// long pointer to exec command line
int nCmdShow;		// Show code for main window display
{
    MSG      msg;
    WNDCLASS wndclass;
    FARPROC  lpfnWndProc;

    //
    // Initialize state information.
    //
    hInst = hInstance;
    if ( !WambiInit( hPrevInstance ) )
	return FALSE;

    //
    // Register a window class.
    //
    if(!hPrevInstance) {
        wndclass.style          = NULL;
	wndclass.lpfnWndProc	= DefDlgProc;
        wndclass.cbClsExtra     = 0;
        wndclass.cbWndExtra     = DLGWINDOWEXTRA;
        wndclass.hInstance      = hInstance;
	wndclass.hIcon		= NULL;
        wndclass.hCursor        = LoadCursor (NULL, IDC_ARROW);
	wndclass.hbrBackground	= GetStockObject(WHITE_BRUSH);
	wndclass.lpszMenuName	= NULL;
	wndclass.lpszClassName	= "Wambi";

	if (!RegisterClass(&wndclass) )
            return FALSE;
    } else {
	return FALSE;	// Allow only one instance.
    }

    //
    // Create and show main dialog window.
    //
    lpfnWndProc = MakeProcInstance( (FARPROC)WambiWndProc, hInst );
    hWndMain = CreateDialog( hInst, "main", 0, lpfnWndProc );
    SetWindowPos( hWndMain, HWND_TOPMOST, 0, 0, 0, 0,
	SWP_NOMOVE | SWP_NOSIZE | SWP_NOREDRAW |
	(fTopMost ? 0 : SWP_NOZORDER) );
    fIconic = nCmdShow == SW_SHOWMINIMIZED;
    ShowWindow( hWndMain, nCmdShow );

    //
    // Main message loop
    //
    while ( GetMessage( &msg, NULL, 0, 0 ) ) {
	if ( !IsDialogMessage( hWndMain, &msg ) ) {
	    TranslateMessage( &msg );
	    DispatchMessage( &msg );
	}
    }

    UnregisterClass( szTitle, hInst );

    return msg.wParam;
}


/**************************************************************************/
/***                                                                    ***/
/***	WambiWndProc							***/
/***                                                                    ***/
/**************************************************************************/

long FAR PASCAL WambiWndProc( hWnd, Message, wParam, lParam )
HWND hWnd;
unsigned Message;
WORD wParam;
LONG lParam;
{
    extern HBRUSH hbrBkgnd;
    extern HBRUSH hbrFrame;
    extern RECT rcStatusLine;	// Bounding rect for status bar.

    HDC hDC;
    PAINTSTRUCT ps;
    FARPROC lpfnOptionWndProc;

    switch (Message)
    {
    case WM_INITDIALOG:
	EnableWindow( GetDlgItem(hWnd, IDD_STOPLOG), FALSE );
	wsprintf( szBuffer, "%u msec", Frequency );
	SetDlgItemText( hWnd, IDD_SAMPFREQ, szBuffer );

	InitDriveBox( hWnd );
	InitRateBox( hWnd );
	InitStatusBar( hWnd );

	//
	// Insert "About" into system menu.  "About SmartMon" would
	// stretch the system menu box too wide.  Also insert
	// "Always on Top".
	//
	hSysMenu = GetSystemMenu( hWnd, FALSE );
	AppendMenu( hSysMenu, MF_SEPARATOR, 0, (LPSTR)NULL );
	AppendMenu( hSysMenu, MF_STRING, IDD_ABOUT,   "&About...");
	AppendMenu( hSysMenu, MF_STRING, IDD_TOPMOST, "Always on &Top" );
	CheckMenuItem( hSysMenu, IDD_TOPMOST,
	    fTopMost ? MF_CHECKED : MF_UNCHECKED );

	SetTimer( hWnd, 0, Frequency, NULL );
	break;

    case WM_TIMER:
	if ( GetHitRate() ) {
	    hbmpIconDrv = hbmpBusy;
	    ForceRateBoxRedraw( hWnd );
	    if ( fLogging ) {
		WriteLog();
		UpdateHelpMessage( IDS_LOGGING );
	    } else
		UpdateHelpMessage( IDS_CACHEACTIVE );
	    if ( fIconic ) {
		InvalidateRect( hWnd, (LPRECT)NULL, FALSE );
	    }
	    fAlreadyIdle = FALSE;
	} else {
	    UpdateHelpMessage( IDS_CACHEIDLE );
	    hbmpIconDrv = hbmpIdle;
	    if ( fIconic )
		if ( !fAlreadyIdle ) {
		    InvalidateRect( hWnd, (LPRECT)NULL, FALSE );
		    fAlreadyIdle = TRUE;
		}
	}

	CheckAutoStop( hWnd );
	break;

    case WM_ACTIVATE:
	//
	// We never know when a network share could be disconnected,
	// or a CD-ROM dismounted.  So be safe, reset all the drives.
	//
	if ( wParam != WA_INACTIVE )
	    ResetDriveBox( hWnd );
	break;

    case WM_MOVE:
	if ( fIconic )
	    fUpdateIconBackground = TRUE;
	break;

    case WM_SIZE:
	if ( (wParam == SIZE_MINIMIZED) ) {
	    if ( !fIconic ) {
		fIconic = fUpdateIconBackground = TRUE;
		SetRect((LPRECT)&rcIconic, 0, 0, LOWORD(lParam), HIWORD(lParam));
	    }
	} else
	    fIconic = FALSE;
	break;

    case WM_PAINT:
	hDC = BeginPaint( hWnd, &ps );

	if ( fIconic ) {
	    if ( fUpdateIconBackground ) {
		fUpdateIconBackground = FALSE;
		DrawIconBackground( hDC );
	    } else
		ValidateRect( hWnd, (LPRECT)NULL );
	    UpdateRateIcon( hWnd, hDC );
	} else {
	    UpdateMemorySize( hWnd );
	    UpdateStatusBar( hWnd, hDC );
	}

	EndPaint( hWnd, &ps );
	return TRUE;

    case WM_MEASUREITEM:
	MeasureItem( hWnd, (LPMEASUREITEMSTRUCT)lParam );
	return TRUE;

    case WM_DRAWITEM:
	DrawItem( hWnd, wParam, (LPDRAWITEMSTRUCT)lParam, FALSE );
	return TRUE;

    case WM_SYSCOMMAND:
	if ( wParam == IDD_ABOUT )
	    ShellAbout( hWnd, szTitle, NULL, hIcoDrag );
	else if ( wParam == IDD_TOPMOST )
	    DoTopMost( hWnd );
	break;

    case WM_KEYDOWN:
	if ( wParam == VK_F1 ) {
	    PostMessage( hWnd, WM_COMMAND, IDD_HELP, 0L );
	    return TRUE;
	}
	break;

    case WM_COMMAND:
	switch( wParam )
	{
	case IDCANCEL:
	    SendDlgItemMessage( hWnd, IDD_DRIVEID, CB_SHOWDROPDOWN, FALSE, 0 );
	    break;

	case IDD_READONLY:
	case IDD_READWRITE:
	case IDD_NOCACHING:
	    DoDriveControl( hWnd, SET, wParam );
	    break;

	case IDD_FLUSH:
	    commit_cache();
	    UpdateHelpMessage( IDS_FLUSHED );
	    break;

	case IDD_RESET:
	    reset_cache();

	    //
	    // Reset lifetime average hit rate baseline
	    //
	    BaseHits = get_cache_hits();
	    BaseTotal = get_cache_misses() + BaseHits;
	    AveHitRate = 0;

	    UpdateHelpMessage( IDS_RESET );
	    break;

	case IDD_DRIVEID:
	    if ( HIWORD(lParam) == CBN_SELCHANGE )
		DoDriveControl( hWnd, GET, 0 );
	    break;

	case IDD_HELP:
	    DoHelp( hWnd, HELP_INDEX, 0L );
	    break;

	case IDD_STARTLOG:
	    if ( StartLog() ) {
		EnableWindow( GetDlgItem(hWnd, IDD_STARTLOG), FALSE );
		EnableWindow( GetDlgItem(hWnd, IDD_STOPLOG), TRUE );
		fLogging = TRUE;
		UpdateHelpMessage( IDS_LOGSTARTED );

		LogStartTime = GetTickCount();
		if ( fAutoStop )
		    LogStopTime = LogStartTime + AutoLogTime * 60000;
	    } else
		MessageBox( hWnd, (LPSTR)"Can not start logging.",
		    (LPSTR)szTitle, MB_ICONINFORMATION );
	    break;

	case IDD_STOPLOG:
	    StopLog();
	    EnableWindow( GetDlgItem(hWnd, IDD_STOPLOG), FALSE );
	    EnableWindow( GetDlgItem(hWnd, IDD_STARTLOG), TRUE );
	    fLogging = FALSE;
	    UpdateHelpMessage( IDS_LOGSTOPPED );
	    break;

	case IDD_OPTION:
	    lpfnOptionWndProc = MakeProcInstance( OptionsDlgProc, hInst );
	    DialogBox( hInst, "options", hWndMain, lpfnOptionWndProc );
	    FreeProcInstance( lpfnOptionWndProc );
	    break;
	}
	return TRUE;

    case WM_CLOSE:
	if ( fLogging )
	    StopLog();

	if ( fTopMostChanged ) {
	    wsprintf( szBuffer, "%u", fTopMost );
	    WriteProfileString( szProSec, szTopMost, szBuffer );
	}

	if ( fDriveInfoChanged && fUpdateAuto ) {
	    if ( !MungeBatchFile() ) {
		LoadString( hInst, IDS_SAVEAUTO, szBuffer, BUFLEN );
		if ( MessageBox( hWnd, (LPSTR)szBuffer, (LPSTR)szTitle,
		    MB_ICONINFORMATION | MB_OKCANCEL ) == IDCANCEL )
		    return TRUE;
	    }
	}

	CleanupRate();
	DestroyIcon( hIcoDrag );
	DeleteObject( hbmpIdle );
	DeleteObject( hbmpBusy );

	if ( hfontStatus )
	    DeleteObject( hfontStatus );

	DoHelp( hWnd, HELP_QUIT, 0L );

	DestroyWindow( hWnd );
	return TRUE;

    case WM_DESTROY:
	PostQuitMessage(0);
	break;

    case WM_SYSCOLORCHANGE:
	ResetRateChartColor( FALSE );
	ResetRateChart( GetDlgItem( hWndMain, IDD_CHARTBOX ) );
	ResetDriveBox( hWnd );
	break;

    case WM_QUERYDRAGICON:
	return hIcoDrag;
    }

    return FALSE;
}


/**************************************************************************/
/***                                                                    ***/
/***	WambiInit							***/
/***                                                                    ***/
/**************************************************************************/

BOOL WambiInit( HANDLE hPrevInstance )
{
    extern char CurDriveList[];
    extern WORD CurDriveCount;

    WORD BlockSize, DosBlocks, WinBlocks;
    char cBootDrv;
    int i, iDrv, iDrvIndex;

    //
    // Load basic string resources.
    //
    LoadString( hInst, IDS_TITLE, szTitle, 20 );
    LoadString( hInst, IDS_STARTLOG, szStartLog, 20 );
    LoadString( hInst, IDS_STOPLOG, szStopLog, 20 );
    LoadString( hInst, IDS_LOGFILE, szLogFile, MAXFILENAMELEN );
    hIcoDrag = LoadIcon( hInst, MAKEINTRESOURCE(1) );
    hbmpIdle = LoadBitmap( hInst, MAKEINTRESOURCE(2) );
    hbmpBusy = LoadBitmap( hInst, MAKEINTRESOURCE(3) );
    hCurWait = LoadCursor( NULL, IDC_WAIT );

    //
    // Allow single instance to run.
    //
    if ( hPrevInstance ) {
	MessageBox( (HWND)NULL, (LPSTR)"SmartMon is already running.",
	    (LPSTR)szTitle, MB_ICONINFORMATION );
	return FALSE;
    }

    //
    // Don't allow app to run if SmartDrive is not installed.
    //
    if ( get_cache_hits() == -1L ) {
	MessageBox( (HWND)NULL, (LPSTR)"SmartDrive is not installed.",
	    (LPSTR)szTitle, MB_ICONINFORMATION );
	return FALSE;
    }

    //
    // Get cache size
    //
    BlockSize = get_cache_info( &DosBlocks, &WinBlocks );
    DosCacheSize = (DWORD)DosBlocks * (DWORD)BlockSize / 1024;
    WinCacheSize = (DWORD)WinBlocks * (DWORD)BlockSize / 1024;

    //
    // Compose default DOS batch filename (for saving drive settings).
    // The default is AUTOEXEC.BAT on the boot drive.  If we can't
    // identify the boot drive, then assume the first hard disk.
    //
    if ( LOBYTE(get_dos_version()) >= 4 )
	cBootDrv = get_boot_drive() - 1 + 'a';
    else {
	CurDriveCount = count_valid_drives( CurDriveList );
	cBootDrv = 'c';
	for ( i = 0; i < CurDriveCount; i++ ) {
	    iDrv = CurDriveList[i+1];
	    iDrvIndex = GetDriveIndex( iDrv, GetDriveType( iDrv ) );
	    if ( iDrvIndex == HARDDRVBMP ) {
		cBootDrv = iDrv + 'a';
		break;
	    }
	}
    }
    wsprintf( szBatchFile, "%c:\\autoexec.bat", cBootDrv );

    //
    // Initialize all random things here
    //
    wWinVer = GetVersion();
    fUpdateIconBackground = FALSE;
    fTopMostChanged = FALSE;
    fLogging = FALSE;
    UpdateHelpMessage( IDS_TITLE );
    hbmpIconDrv = hbmpIdle;
    fProfileChanged = FALSE;
    fAlreadyIdle = FALSE;
    fDriveInfoChanged = FALSE;
    InitDriveBitmap();
    fTopMost = TRUE;

    GetProfileSettings();

    return TRUE;
}


#ifdef OBSOLETE
/**************************************************************************/
/***                                                                    ***/
/***	UpdateStat							***/
/***                                                                    ***/
/***	This routine displays the hit/miss count and ratio is simple	***/
/***	text format.							***/
/***                                                                    ***/
/***	Note that it might be a good idea to activate this code when	***/
/***	InitRateBox fails for some reason (like low memory).		***/
/***                                                                    ***/
/**************************************************************************/

void UpdateStat( HWND hWnd )
{
    DWORD hits, misses, rate;

    //
    // Get cache status
    //
    hits = get_cache_hits();
    misses = get_cache_misses();
    rate = (hits * 100) / (hits + misses);

    //
    // Display cache status
    //
    wsprintf( szBuffer, "%lu", hits );
    SetDlgItemText( hWnd, IDD_HITS, szBuffer );
    wsprintf( szBuffer, "%lu", misses );
    SetDlgItemText( hWnd, IDD_MISSES, szBuffer );
    wsprintf( szBuffer, "%lu%%", rate );
    SetDlgItemText( hWnd, IDD_RATE, szBuffer );
}
#endif


/**************************************************************************/
/***                                                                    ***/
/***	DoDriveControl							***/
/***                                                                    ***/
/***	This routine changes the caching status of each drive as	***/
/***	the user manipulates the corresponding radio buttons.		***/
/***                                                                    ***/
/***	Since the caching status, as well as drive configuration	***/
/***	can be changed outside of SMARTMON, this routine should 	***/
/***	be called to do a GET whenever we get focus back.		***/
/***                                                                    ***/
/**************************************************************************/

void DoDriveControl( HWND hWnd, WORD cmd, WORD id )
{
    extern WORD cCurDriveSel;

    WORD status;
    WORD iCurDrvIndex;
    HWND hCtl;

    hCtl = GetDlgItem( hWnd, IDD_DRIVEID );
    SendMessage( hCtl, CB_GETLBTEXT,
	(WPARAM)(DWORD)SendMessage(hCtl, CB_GETCURSEL,0,0),
	(LPARAM)(LPSTR)szBuffer );
    cCurDriveSel = szBuffer[0] - 'a';

    if ( cmd == GET ) {

	status = cache_a_drive( GET, cCurDriveSel );

	if ( status & NO_READ )
	    id = IDD_NOCACHING;
	else if ( status & NO_WRITE )
	    id = IDD_READONLY;
	else
	    id = IDD_READWRITE;

	//
	// Disable r/o and r/w buttons if drive cannot be cached.
	//
	iCurDrvIndex = GetDriveIndex( cCurDriveSel, GetDriveType( cCurDriveSel ) );
	if ( (iCurDrvIndex == FLOPPYBMP) ||
	     ((iCurDrvIndex == HARDDRVBMP) && !CheckForStacker(cCurDriveSel))) {
	    EnableWindow( GetDlgItem(hWnd, IDD_READONLY), TRUE );
	    EnableWindow( GetDlgItem(hWnd, IDD_READWRITE), TRUE );
	    EnableWindow( GetDlgItem(hWnd, IDD_NOCACHING), TRUE );
	} else {
	    EnableWindow( GetDlgItem(hWnd, IDD_READONLY), FALSE );
	    EnableWindow( GetDlgItem(hWnd, IDD_READWRITE), FALSE );
	    EnableWindow( GetDlgItem(hWnd, IDD_NOCACHING), FALSE );
	}

    } else {	// Set operation

	//
	// Get into a known state, then disable selectively
	//
	cache_a_drive( ENABLE_READ, cCurDriveSel );
	cache_a_drive( ENABLE_WRITE, cCurDriveSel );

	if ( id != IDD_READWRITE ) {
	    cache_a_drive( DISABLE_WRITE, cCurDriveSel );
	    if ( id == IDD_NOCACHING )
		cache_a_drive( DISABLE_READ, cCurDriveSel );
	}

	fDriveInfoChanged = TRUE;
	UpdateHelpMessage( IDS_STATUS );
    }

    CheckRadioButton( hWnd, IDD_READONLY, IDD_NOCACHING, id );
}


/**************************************************************************/
/***                                                                    ***/
/***	UpdateMemorySize						***/
/***                                                                    ***/
/***	This routine displays the cache memory size under DOS and	***/
/***	under Windows.	Currently the size cannot be changed when	***/
/***	Windows is running.						***/
/***                                                                    ***/
/**************************************************************************/

void UpdateMemorySize( HWND hWnd )
{
    wsprintf( szBuffer, "%uK", DosCacheSize );
    SetDlgItemText( hWnd, IDD_DOSSIZE, szBuffer );
    wsprintf( szBuffer, "%uK", WinCacheSize );
    SetDlgItemText( hWnd, IDD_WINSIZE, szBuffer );
}


/**************************************************************************/
/***                                                                    ***/
/***	DoHelp								***/
/***                                                                    ***/
/***	This routine invokes WINHELP if BOOL is true, or dismisses	***/
/***	it if BOOL is false.						***/
/***									***/
/**************************************************************************/

void DoHelp( HWND hWnd, UINT cmd, DWORD data )
{
    if ( LoadString(hInst, IDS_HELPFILE, szBuffer, sizeof(szBuffer)-1) )
	WinHelp( hWnd, (LPSTR)szBuffer, cmd, data );
}


/**************************************************************************/
/***                                                                    ***/
/***	OptionsDlgProc							***/
/***									***/
/**************************************************************************/

BOOL FAR PASCAL OptionsDlgProc( hDlg, msg, wParam, lParam )
HWND hDlg;
WORD msg;
WPARAM wParam;
LPARAM lParam;
{
    OFSTRUCT of;
    int rc;
    WORD val;
    BOOL bRet;
    HWND hCtl;

    switch ( msg )
    {
    case WM_INITDIALOG:
	SetDlgItemInt( hDlg, IDD_MSEC, Frequency, FALSE );
	SetDlgItemInt( hDlg, IDD_INTERVAL, SpecInterval, FALSE );
	SetDlgItemText( hDlg, IDD_LOGFILE, szLogFile );
	CheckDlgButton( hDlg, IDD_AUTOSTOP, fAutoStop );
	SetDlgItemInt( hDlg, IDD_STOPTIME, AutoLogTime, FALSE );
	CheckDlgButton( hDlg, IDD_SAVESET, fUpdateAuto );
	SetDlgItemText( hDlg, IDD_BATCHFILE, szBatchFile );
	break;

    case WM_COMMAND:
	switch( wParam )
	{
	case IDD_HELP:
	    DoHelp( hDlg, HELP_CONTEXT, 1L );
	    break;

	case IDD_AUTOSTOP:
	    CheckDlgButton( hDlg, IDD_AUTOSTOP,
		!IsDlgButtonChecked(hDlg, IDD_AUTOSTOP) );
	    break;

	case IDD_SAVESET:
	    CheckDlgButton( hDlg, IDD_SAVESET,
		!IsDlgButtonChecked(hDlg, IDD_SAVESET) );
	    break;

	case IDOK:

	    //
	    //	Reset sampling frequency
	    //
	    val = GetDlgItemInt( hDlg, IDD_MSEC, (BOOL FAR*)&bRet, FALSE );
	    if ( bRet ) {
		if ( val > MAX_FREQUENCY )
		    val = MAX_FREQUENCY;
		if ( val < MIN_FREQUENCY )
		    val = MIN_FREQUENCY;
		if ( val != Frequency ) {
		    Frequency = val;
		    KillTimer( hWndMain, 0 );
		    SetTimer( hWndMain, 0, Frequency, NULL );
		    wsprintf( szBuffer, "%u msec", Frequency );
		    SetDlgItemText( hWndMain, IDD_SAMPFREQ, szBuffer );
		}
	    }

	    //
	    //	Reset display interval
	    //
	    val = GetDlgItemInt( hDlg, IDD_INTERVAL, (BOOL FAR*)&bRet, FALSE );
	    if ( bRet ) {
		if ( val > MAX_INTERVAL )
		    val = MAX_INTERVAL;
		if ( val < MIN_INTERVAL )
		    val = MIN_INTERVAL;
		if ( val != SpecInterval ) {
		    SpecInterval = val;
		    hCtl = GetDlgItem( hWndMain, IDD_CHARTBOX );
		    ResetRateChart( hCtl );
		    InvalidateRect( hCtl, (LPRECT)NULL, TRUE );
		}
	    }

	    GetDlgItemText( hDlg, IDD_LOGFILE, szLogFile, MAXFILENAMELEN );

	    fAutoStop = IsDlgButtonChecked( hDlg, IDD_AUTOSTOP );
	    val = GetDlgItemInt( hDlg, IDD_STOPTIME, (BOOL FAR *)&bRet, FALSE );
	    if ( bRet )
		AutoLogTime = val;
	    if ( AutoLogTime > MAX_AUTOLOGTIME )
		AutoLogTime = MAX_AUTOLOGTIME;
	    if ( AutoLogTime < MIN_AUTOLOGTIME )
		AutoLogTime = MIN_AUTOLOGTIME;

	    if ( fLogging )
		LogStopTime = LogStartTime + AutoLogTime * 60000;

	    fUpdateAuto = IsDlgButtonChecked( hDlg, IDD_SAVESET );
	    GetDlgItemText( hDlg, IDD_BATCHFILE, szBuffer, BUFLEN );
	    rc = OpenFile( (LPSTR)szBuffer, (OFSTRUCT FAR *)&of, OF_EXIST );
	    SetDlgItemText( hDlg, IDD_BATCHFILE, of.szPathName );
	    OemToAnsi( of.szPathName, szBatchFile );
	    if ( fUpdateAuto &&  (rc == -1) ) {
		MessageBox( (HWND)NULL, (LPSTR)"Batch file does not exist.",
		(LPSTR)szTitle, MB_ICONINFORMATION );
		return TRUE;
	    }

	    SaveProfileSettings();

	    // fall through

	case IDCANCEL:
	    DoHelp( hDlg, HELP_QUIT, 0L );
	    EndDialog( hDlg, 0 );
	    break;
	}
	break;

    default:
	return FALSE;	// message not processed
    }

    return TRUE;    // message processed
}


/**************************************************************************/
/***                                                                    ***/
/***	CheckAutoStop							***/
/***									***/
/***	This routine stops logging to file.  Note that the logging	***/
/***	auto stop time may be altered through the options box after	***/
/***	logging has been started.					***/
/***									***/
/**************************************************************************/

void CheckAutoStop( HWND hWnd )
{
    if ( fLogging && fAutoStop )
	//
	// Note, this logic will fail if tick counter wraps around
	//
	if ( GetTickCount() > LogStopTime )
	    PostMessage( hWnd, WM_COMMAND, IDD_STOPLOG, 0L );
}


/**************************************************************************/
/***                                                                    ***/
/***	GetProfileSettings						***/
/***	SaveProfileSettings						***/
/***									***/
/***	Get and save settings to WIN.INI.				***/
/***									***/
/**************************************************************************/
void GetProfileSettings()
{
    //
    // Retrieve option settings from WIN.INI
    //
    SpecInterval = GetProfileInt( szProSec, szInterval, DEF_INTERVAL );
    Frequency	 = GetProfileInt( szProSec, szFreq, DEF_FREQUENCY );
    fAutoStop	 = GetProfileInt( szProSec, szfStop, DEF_AUTOSTOP );
    AutoLogTime  = GetProfileInt( szProSec, szLogTime, DEF_LOGTIME );
    fUpdateAuto  = GetProfileInt( szProSec, szUpdateDOS, DEF_UPDATEDOS );
    fTopMost	 = GetProfileInt( szProSec, szTopMost, DEF_TOPMOST );

    if ( GetProfileString( szProSec, szLog, szLogFile, szBuffer, BUFLEN ) )
	lstrcpy( szLogFile, szBuffer );
    if ( GetProfileString( szProSec, szBatch, szBatchFile, szBuffer, BUFLEN ) )
	lstrcpy( szBatchFile, szBuffer );
}


void SaveProfileSettings()
{
    //
    // Save option settings to profile
    //
    wsprintf( szBuffer, "%u", SpecInterval );
    WriteProfileString( szProSec, szInterval, szBuffer );
    wsprintf( szBuffer, "%u", Frequency );
    WriteProfileString( szProSec, szFreq, szBuffer );
    wsprintf( szBuffer, "%u", fAutoStop );
    WriteProfileString( szProSec, szfStop, szBuffer );
    wsprintf( szBuffer, "%u", AutoLogTime );
    WriteProfileString( szProSec, szLogTime, szBuffer );
    wsprintf( szBuffer, "%u", fUpdateAuto );
    WriteProfileString( szProSec, szUpdateDOS, szBuffer );
    wsprintf( szBuffer, "%u", fTopMost );
    WriteProfileString( szProSec, szTopMost, szBuffer );
    WriteProfileString( szProSec, szLog, szLogFile );
    WriteProfileString( szProSec, szBatch, szBatchFile );
}


/**************************************************************************/
/***                                                                    ***/
/***	CheckForStacker 						***/
/***									***/
/***	This routine does an absolute disk read (INT 25) of the 	***/
/***	first sector, and checks the disk label to see if it is 	***/
/***	a Stacker volume.  We allocate an 8K buffer to serve as 	***/
/***	the DTA (same as WINFILE), hope it's big enough for all         ***/
/***	disks.								***/
/***									***/
/**************************************************************************/

BOOL CheckForStacker( WORD cDrv )
{
    HANDLE hBuf;
    LPSTR  pBuf;
    BOOL rc;

    //
    // If can't allocate DTA to do int 25, assume not Stacker
    //
    if (! (hBuf = GlobalAlloc( GHND, 8192L )) )
	return FALSE;
    pBuf = GlobalLock( hBuf );

    rc = is_Stacker_drive( cDrv, pBuf );

    GlobalUnlock( hBuf );
    GlobalFree( hBuf );

    return rc;
}


void DoTopMost( HWND hWnd )
{
    fTopMostChanged = TRUE;

    fTopMost = fTopMost ? FALSE : TRUE;
    CheckMenuItem( hSysMenu, IDD_TOPMOST, fTopMost? MF_CHECKED : MF_UNCHECKED );

    if ( !fTopMost ) {
	SetWindowPos( hWnd, HWND_BOTTOM, 0, 0, 0, 0,
	    SWP_NOMOVE | SWP_NOSIZE | SWP_NOREDRAW );
	SetWindowPos( hWnd, HWND_TOP, 0, 0, 0, 0,
	    SWP_NOMOVE | SWP_NOSIZE );
    } else
	SetWindowPos( hWnd, HWND_TOPMOST, 0, 0, 0, 0,
	    SWP_NOMOVE | SWP_NOSIZE | SWP_SHOWWINDOW );
}
