/**************************************************************************/
/***									***/
/***	STATUS.C    - Status bar update routines			***/
/***									***/
/***	Largely borrowed from CHARMAP.C.				***/
/***									***/
/***									***/
/**************************************************************************/

#include <windows.h>
#include "smartmon.h"

extern HANDLE hInst;
extern char szBuffer[];     // buffer for stringtable stuff
extern WORD AveHitRate;
extern HWND hWndMain;	    // application's main window

void PaintStatusField( HDC, BOOL, BOOL );
int PointsToHeight( int );

RECT rcStatusLine;	    // Bounding rect for status bar.
int dyStatus;		    // Height of status bar.
int dxHelpField;	    // Width of help window.
HFONT hfontStatus;	    // Font used for text of status bar.
int dxHitRateField;	    // Width of HitRate window.
int iHelpMessage = 0;	    // help message resource string id

/**************************************************************************/
/***                                                                    ***/
/***	UpdateStatusBar 						***/
/***                                                                    ***/
/**************************************************************************/

void UpdateStatusBar( HWND hWnd, HDC hDC )
{
    HBRUSH hBrush;
    RECT rcTemp;
    int dyBorder;

    rcTemp = rcStatusLine;

    dyBorder = GetSystemMetrics(SM_CYBORDER);

    //
    // Make the whole thing grey.
    //
    if ( hBrush = CreateSolidBrush(GetSysColor(COLOR_BTNFACE)) ) {
	FillRect( hDC, &rcTemp, hBrush );
	DeleteObject( hBrush );
    }

    if ( hBrush = CreateSolidBrush(GetSysColor(COLOR_BTNSHADOW)) ) {
	// Status line top.
	rcTemp.left   = 8 * dyBorder;
	rcTemp.right  = rcTemp.left + dxHelpField;
	rcTemp.top    = rcStatusLine.top + dyBorder * 2;
	rcTemp.bottom = rcTemp.top + dyBorder;
	FillRect( hDC, &rcTemp, hBrush );

	// HitRate line top.
	rcTemp.right = rcStatusLine.right - 8 * dyBorder;
	rcTemp.left = rcTemp.right - dxHitRateField;
	FillRect( hDC, &rcTemp, hBrush);

	// Status line left side.
	rcTemp = rcStatusLine;
	rcTemp.left = 8 * dyBorder;
	rcTemp.right = rcTemp.left + dyBorder;
	rcTemp.top += dyBorder * 2;
	rcTemp.bottom -= dyBorder * 2;
	FillRect( hDC, &rcTemp, hBrush );

	// HitRate line left side.
	rcTemp.left = rcStatusLine.right - 9 * dyBorder - dxHitRateField;
	rcTemp.right = rcTemp.left + dyBorder;
	FillRect( hDC, &rcTemp, hBrush );

	DeleteObject( hBrush );
    }

    if ( hBrush = CreateSolidBrush(GetSysColor(COLOR_BTNHIGHLIGHT)) ) {
	// Status line bottom.
	rcTemp.left   = 8 * dyBorder;
	rcTemp.right  = rcTemp.left + dxHelpField;
	rcTemp.top    = rcStatusLine.bottom - 3 * dyBorder;
	rcTemp.bottom = rcTemp.top + dyBorder;
	FillRect( hDC, &rcTemp, hBrush );

	// HitRate line bottom.
	rcTemp.right = rcStatusLine.right - 8 * dyBorder;
	rcTemp.left = rcTemp.right - dxHitRateField;
	FillRect( hDC, &rcTemp, hBrush );

	// Status line right side.
	rcTemp = rcStatusLine;
	rcTemp.left = 8 * dyBorder + dxHelpField;
	rcTemp.right = rcTemp.left + dyBorder;
	rcTemp.top += dyBorder * 2;
	rcTemp.bottom -= dyBorder * 2;
	FillRect( hDC, &rcTemp, hBrush );

	// HitRate line right side.
	rcTemp.left = rcStatusLine.right - 8 * dyBorder;
	rcTemp.right = rcTemp.left + dyBorder;
	FillRect( hDC, &rcTemp, hBrush );

	DeleteObject( hBrush );
    }

    //
    // Solid black line across top
    //
    if ( hBrush = CreateSolidBrush(GetSysColor(COLOR_WINDOWFRAME)) ) {
	rcTemp = rcStatusLine;
	rcTemp.bottom = rcTemp.top;
	rcTemp.top -= dyBorder;
	FillRect( hDC, &rcTemp, hBrush );
	DeleteObject( hBrush );
    }

    PaintStatusField( hDC, TRUE, TRUE );
}


/**************************************************************************/
/***                                                                    ***/
/***	UpdateHelpMessage						***/
/***                                                                    ***/
/**************************************************************************/

void UpdateHelpMessage( int id )
{
    extern BOOL fIconic;
    extern int Frequency;
    static int delay = 0;

    if ( iHelpMessage != id ) {

	//
	// Let the user-initiated messages stay for two seconds.
	// During that time it can be pre-empted by another such
	// message, but other low-priority messages are lost.
	//
	if ( (id == IDS_LOGSTARTED) ||
	     (id == IDS_LOGSTOPPED) ||
	     (id == IDS_STATUS)     ||
	     (id == IDS_FLUSHED)    ||
	     (id == IDS_RESET) ) {
	    delay = 2000 / Frequency;
	} else if ( delay ) {
	    delay--;
	    return;
	}

	iHelpMessage = id;
	if ( !fIconic )
	    InvalidateRect( hWndMain, (LPRECT)&rcStatusLine, FALSE );
    }
}


/**************************************************************************/
/***                                                                    ***/
/***	PaintStatusBar							***/
/***                                                                    ***/
/***	Repaints Help field if fHelp is TRUE, repaints the AveHitRate	***/
/***	field if fRate is TRUE. 					***/
/***                                                                    ***/
/**************************************************************************/

void PaintStatusField( HDC hDC, BOOL fHelp, BOOL fRate )
{
    HFONT hfontOld = NULL;
    RECT rect;
    int dyBorder;

    dyBorder = GetSystemMetrics( SM_CYBORDER );

    if ( hfontStatus )
	hfontOld = SelectObject( hDC, hfontStatus );

    // set the text and background colors
    SetTextColor( hDC, GetSysColor(COLOR_BTNTEXT) );
    SetBkColor( hDC, GetSysColor(COLOR_BTNFACE) );

    LoadString( hInst, iHelpMessage, szBuffer, BUFLEN - 1 );

    //
    // Update the help text field
    //
    if ( fHelp ) {
	// now the help text, with a gray background
	rect.top    = rcStatusLine.top + 3 * dyBorder;
	rect.bottom = rcStatusLine.bottom - 3 * dyBorder;
	rect.left   = 9 * dyBorder;
	rect.right  = rect.left + dxHelpField - 2 * dyBorder;

	ExtTextOut( hDC, rect.left + dyBorder * 5, rect.top,
	    ETO_OPAQUE | ETO_CLIPPED, &rect, szBuffer,
	    lstrlen(szBuffer), NULL);
    }

    //
    // Update the average hit rate number
    //
    if ( fRate ) {
        rect.top    = rcStatusLine.top + 3 * dyBorder;
        rect.bottom = rcStatusLine.bottom - 3 * dyBorder;
        rect.right = rcStatusLine.right - 9 * dyBorder;
	rect.left = rect.right - dxHitRateField + 2 * dyBorder;

	wsprintf( szBuffer, "Average Hit Rate: %4u%%", AveHitRate );

	ExtTextOut( hDC, rect.left + dyBorder * 5, rect.top,
	    ETO_OPAQUE | ETO_CLIPPED, &rect, szBuffer,
	    lstrlen(szBuffer), NULL );
    }

    if ( hfontOld )
	SelectObject( hDC, hfontOld );
}


/**************************************************************************/
/***                                                                    ***/
/***	PointsToHeight							***/
/***                                                                    ***/
/**************************************************************************/

int PointsToHeight( int iPoints )
{
    HDC hDC;
    int iHeight;

    hDC = GetDC( HWND_DESKTOP );
    iHeight = MulDiv( iPoints, GetDeviceCaps(hDC, LOGPIXELSY), 72 );
    ReleaseDC( HWND_DESKTOP, hDC );
    return( iHeight );
}


/**************************************************************************/
/***                                                                    ***/
/***	InitStatusBar							***/
/***                                                                    ***/
/**************************************************************************/

void InitStatusBar( HWND hWnd )
{
    RECT rcParent;

    GetClientRect( hWnd, &rcParent );

    // This font will be used to paint the status line.
    hfontStatus = CreateFont(-PointsToHeight(STATUSPOINTSIZE),
	0, 0, 0, 400, 0, 0, 0,
	ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
	DEFAULT_QUALITY, VARIABLE_PITCH | FF_SWISS, "Helv" );

    dyStatus = 2 * PointsToHeight(STATUSPOINTSIZE);

    // Initialize the status line data.
    dxHelpField = 20 * rcParent.right / 32;
    dxHitRateField = 10 * rcParent.right / 32;
    rcStatusLine = rcParent;
    rcStatusLine.top = rcStatusLine.bottom - dyStatus;
}
