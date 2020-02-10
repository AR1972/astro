/**************************************************************************/
/***									***/
/***	RATE.C	- Cache hit rate charting routines			***/
/***									***/
/***									***/
/***									***/
/***									***/
/**************************************************************************/

#include <windows.h>
#include "smartmon.h"

DWORD PrevHits = 0;	// last cache hit count
DWORD PrevTotal = 0;	// last cache hit+miss total
DWORD BaseHits = 0;
DWORD BaseTotal = 0;
WORD  CurrentRate;	// current hit rate
WORD  AveHitRate;	// averate hit rate
WORD  SpecInterval;	// specified sampling interval
WORD  Interval; 	// actual sampling intervals in seconds
WORD  Frequency;	// sampling frequency in msec
WORD  xDelta;		// width of each bar in histogram
HDC   hdcChart;
HBITMAP hbmpChart;
HBITMAP hbmpOrgChart;
HBITMAP hbmpIconDrv;
BOOL  fCacheIdle = TRUE;
COLORREF rgbHitRate;

HBRUSH hbrBkgnd;
HBRUSH hbrRed;
HPEN hpenFrame;
HBRUSH hbrOrg;
HPEN hpenOrg;

WNDPROC lpOrgWndProc = NULL;
LRESULT FAR PASCAL RBSubclass( HWND, WORD, WPARAM, LPARAM );
void DrawRateBox( HWND, HDC );
void GetRateBoxRect( HWND, LPRECT, BOOL );
void UpdateRateChart( HWND );
void UpdateRateIcon( HWND, HDC );
void DrawShadowedFrame( HDC, LPRECT );
void ResetRateChart( HWND );
BOOL GetHitRate( void );
void ResetRateChartColor( BOOL );

extern BOOL fIconic;
extern RECT rcIconic;
extern int PointsToHeight( int );
extern void MySetObjectOwner( HANDLE );


/**************************************************************************/
/***                                                                    ***/
/***	GetRateBoxRect							***/
/***                                                                    ***/
/**************************************************************************/

void GetRateBoxRect( HWND hCtl, LPRECT lprc, BOOL fChartOnly )
{
    GetWindowRect( hCtl, lprc );

    //
    // Rectangle returned by GetWindowRect is in screen
    // coordinates.  Convert it to zero-based.
    //
    lprc->bottom -= lprc->top;
    lprc->right  -= lprc->left;
    lprc->left	  = 0;
    lprc->top	  = 0;

    //
    // Subtract the frame and shadow.
    //
    if ( fChartOnly ) {
	lprc->top++;
	lprc->left++;
	lprc->right -= 2;
	lprc->bottom -= 2;
    }
}


/**************************************************************************/
/***                                                                    ***/
/***	InitRateBox							***/
/***                                                                    ***/
/**************************************************************************/

void InitRateBox( HWND hDlg )
{
    int i;
    HWND hCtl;
    RECT rc;
    HDC hdcScreen;
    int nPlanes;

    //
    // The hit rate box is a dummy LTEXT control item in the resource
    // template.  Subclass it and turn it into an owner-draw item.
    //
    hCtl = GetDlgItem( hDlg, IDD_CHARTBOX );
    lpOrgWndProc = (WNDPROC)GetClassLong( hCtl, GCL_WNDPROC );
    SetWindowLong( hCtl, GWL_WNDPROC, (LPARAM)(DWORD)RBSubclass );

    //
    // Initialize hit rate
    //
    PrevHits = get_cache_hits();
    PrevTotal = PrevHits + get_cache_misses();
    AveHitRate = (WORD)(PrevHits * 100 / PrevTotal);

    //
    // Set up a shadow bitmap for drawing the chart.
    //
    GetRateBoxRect( hCtl, (LPRECT)&rc, FALSE );
    hdcScreen = GetDC( 0 );
    if ( hdcChart = CreateCompatibleDC( hdcScreen ) ) {
	hbmpChart = CreateBitmap( rc.right-rc.left+1, rc.bottom-rc.top+1,
	    nPlanes = GetDeviceCaps( hdcChart, PLANES ),
	    GetDeviceCaps( hdcChart, BITSPIXEL ), NULL );
	hbmpOrgChart = SelectObject( hdcChart, hbmpChart );
	hbrRed = CreateSolidBrush( RGB(255, 0, 0) );
	UnrealizeObject( hbrRed );
	MySetObjectOwner( hdcChart );
	MySetObjectOwner( hbmpChart );
	MySetObjectOwner( hbrRed );
    }
    ReleaseDC( 0, hdcScreen );

    //
    // Show hit rate in red or white, depending on monitor type
    //
    rgbHitRate = nPlanes > 1 ? RGB(255, 0, 0) : RGB(0, 0, 0);

    ResetRateChartColor( TRUE );
    ResetRateChart( hCtl );
}


void ResetRateChart( HWND hCtl )
{
    int  i,j;
    RECT rc;
    WORD xSpan, xExtra, ySpan;

    GetRateBoxRect( hCtl, (LPRECT)&rc, FALSE );
    xSpan = rc.right - rc.left + 1;
    ySpan = rc.bottom - rc.top + 1;

    //
    // Erase whole box to background color
    //
    PatBlt( hdcChart, 0, 0, xSpan, ySpan, PATCOPY );

    //
    // Draw frame and shadow
    //
    MoveTo( hdcChart, rc.left, rc.top );
    LineTo( hdcChart, rc.right-2, rc.top );
    LineTo( hdcChart, rc.right-2, rc.bottom-2 );
    LineTo( hdcChart, rc.left, rc.bottom-2 );
    LineTo( hdcChart, rc.left, rc.top );
    MoveTo( hdcChart, rc.right-1, 1 );
    LineTo( hdcChart, rc.right-1, rc.bottom-1 );
    LineTo( hdcChart, 1, rc.bottom-1 );
    MoveTo( hdcChart, rc.right, 2 );
    LineTo( hdcChart, rc.right, rc.bottom );
    LineTo( hdcChart, 2, rc.bottom );

    //
    // Because the size of the graph is fixed, the width may not
    // evenly divide by the number of intervals.  This will cause
    // a total repaint to look like it had lost some data.  So we
    // take a short cut here to bump up the actual number of intervals
    // to match the width.
    //
    xDelta = max( 2, xSpan / SpecInterval );
    xExtra = xSpan - (xDelta-1) * SpecInterval;
    if ( xExtra )
	Interval = SpecInterval + (xExtra / xDelta + 1);
    else
	Interval = SpecInterval;
}


/**************************************************************************/
/***                                                                    ***/
/***	UpdateRateChart 						***/
/***                                                                    ***/
/**************************************************************************/

void UpdateRateChart( HWND hCtl )
{
    RECT rc;
    WORD xSpan, ySpan;
    HPEN hpenTmp;

    GetRateBoxRect( hCtl, (LPRECT)&rc, TRUE );
    xSpan = rc.right - rc.left + 1;
    ySpan = rc.bottom - rc.top + 1;

    //
    // Blt repaint area to the right to make room for new drawing.
    //
    BitBlt( hdcChart, xDelta, 0, rc.right - xDelta, rc.bottom,
	    hdcChart, 0, 0, SRCCOPY );

    //
    // Erase vacated area with background color.
    //
    PatBlt( hdcChart, rc.left, rc.top, xDelta, rc.bottom-1, PATCOPY );

    //
    // Draw new rate.  The '101' keeps the bar from reaching the top.
    //
    rc.right = xDelta;	    // +1 removes gap
    rc.top = ySpan - (ySpan * CurrentRate / 101);
    SelectObject( hdcChart, hbrRed );
    PatBlt( hdcChart, rc.left, min(rc.top+1, rc.bottom),
	rc.right-rc.left, max(0, rc.bottom-rc.top-1), PATCOPY );
    SelectObject( hdcChart, hbrBkgnd );
}


/**************************************************************************/
/***                                                                    ***/
/***	DrawRateBox							***/
/***                                                                    ***/
/**************************************************************************/

void DrawRateBox( HWND hCtl, HDC hDC )
{
    RECT rc;

    GetRateBoxRect( hCtl, (LPRECT)&rc, FALSE );

    //
    // Draw the whole chart
    //
    BitBlt( hDC, 0, 0, rc.right - rc.left + 1, rc.bottom - rc.top + 1,
	    hdcChart, 0, 0, SRCCOPY );
}


/**************************************************************************/
/***                                                                    ***/
/***	GetHitRate							***/
/***                                                                    ***/
/**************************************************************************/

BOOL GetHitRate()
{
    DWORD hits, total;

    //
    // Calculate current hit rate
    //
    hits = get_cache_hits();
    total = hits + get_cache_misses();
    if ( total == PrevTotal ) {
	fCacheIdle = TRUE;
	return FALSE;
    } else
	fCacheIdle = FALSE;

    if ( hits == PrevHits ) {
	CurrentRate = 0;
    } else {
	CurrentRate = (hits - PrevHits) * 100 / (total - PrevTotal);
	PrevHits = hits;
    }
    PrevTotal = total;

    //
    // Calculate lifetime average hit rate based on the last reset
    //
    if ( PrevTotal == BaseTotal )
	AveHitRate = 0;
    else
	AveHitRate = (WORD)((PrevHits-BaseHits) * 100 / (PrevTotal-BaseTotal));

    return TRUE;
}


/**************************************************************************/
/***                                                                    ***/
/***	ForceRateBoxRedraw						***/
/***                                                                    ***/
/**************************************************************************/

void ForceRateBoxRedraw( HWND hDlg )
{
    RECT rect;
    HWND hCtl;

    //
    // Update the shadow bitmap
    //
    hCtl = GetDlgItem( hDlg, IDD_CHARTBOX );
    UpdateRateChart( hCtl );

    if ( !fIconic ) {
	GetRateBoxRect( hCtl, (LPRECT)&rect, TRUE );
	InvalidateRect( hCtl, (LPRECT)&rect, FALSE );
    }
}


/**************************************************************************/
/***                                                                    ***/
/***	RBSubclass							***/
/***                                                                    ***/
/**************************************************************************/

LRESULT FAR PASCAL RBSubclass( HWND hCtl, WORD msg, WPARAM wP, LPARAM lP )
{
    HDC hDC;
    RECT rect;
    PAINTSTRUCT ps;

    switch ( msg )
    {
    case WM_PAINT:
	hDC = BeginPaint( hCtl, &ps );
	DrawRateBox( hCtl, hDC );
	EndPaint( hCtl, &ps );
	break;

    default:
	return( CallWindowProc( lpOrgWndProc, hCtl, msg, wP, lP ) );
    }

    return 0L;
}


/**************************************************************************/
/***                                                                    ***/
/***	UpdateRateIcon							***/
/***									***/
/**************************************************************************/

void UpdateRateIcon( HWND hWnd, HDC hDC )
{
    extern WORD CurrentRate;
    extern HFONT hfontStatus;
    extern char szBuffer[];
    extern HDC hdcMemory;

    HFONT hfontOld = NULL;
    HBRUSH hBrush;
    HBITMAP hbmp;
    RECT rc;
    int dyBorder;
    WORD x, y;

    dyBorder = GetSystemMetrics( SM_CYBORDER );
    rc = rcIconic;

    //
    // Draw frame
    //
    if ( hBrush = CreateSolidBrush(GetSysColor(COLOR_BTNSHADOW)) ) {
	rc.bottom = dyBorder;
	FillRect( hDC, &rc, hBrush );
	rc.bottom = rcIconic.bottom;

	rc.right = dyBorder;
	FillRect( hDC, &rc, hBrush );
	rc.right = rcIconic.right;

	rc.left = rc.right - dyBorder;
	FillRect( hDC, &rc, hBrush );
	rc.left = rcIconic.left;

	rc.top	= rc.bottom - dyBorder;
	FillRect( hDC, &rc, hBrush );
	rc.top = rcIconic.top;

	DeleteObject( hBrush );
    }

    //
    // Draw the current rate in top half
    //
    if ( hfontStatus )
	hfontOld = SelectObject( hDC, hfontStatus );

    SetBkColor( hDC, GetSysColor(COLOR_BTNFACE) );
    if ( fCacheIdle ) {
	SetTextColor( hDC, GetSysColor(COLOR_BTNTEXT) );
	wsprintf( szBuffer, "%u%%", AveHitRate );
    } else {
	//
	// Use red for color systems, white for mono
	//
	SetTextColor( hDC, rgbHitRate );
	wsprintf( szBuffer, "%u%%", CurrentRate );
    }

    InflateRect( (LPRECT)&rc, -dyBorder, -dyBorder );
    x = rc.left + (rc.right - rc.left - (int)GetTextExtent( hDC,
	(LPSTR)szBuffer, lstrlen(szBuffer) )) / 2;
    y = rc.top + ((rc.bottom-rc.top)/2 - PointsToHeight(STATUSPOINTSIZE))/2;

    ExtTextOut( hDC, x, y, ETO_OPAQUE | ETO_CLIPPED, &rc, szBuffer,
	lstrlen(szBuffer), NULL );

    if ( hfontOld )
	SelectObject( hDC, hfontOld );

    //
    // Draw the drive bitmap in bottom half
    //
    hbmp = SelectObject( hdcMemory, hbmpIconDrv );
    y = (rc.bottom - rc.top) / 2;
    BitBlt( hDC, rc.left + (rc.right - rc.left - 16) / 2,
	    y + (y - 9)/2, 16, 9, hdcMemory, 0, 0, SRCCOPY );
    SelectObject( hdcMemory, hbmp );
}


void DrawIconBackground( HDC hDC )
{
    HBRUSH hBrush;

    //
    // Fill the rectangle with grey background color.
    //
    if ( hBrush = CreateSolidBrush(GetSysColor(COLOR_BTNFACE)) ) {
	FillRect( hDC, (LPRECT)&rcIconic, hBrush );
	DeleteObject( hBrush );
    }
}


void ResetRateChartColor( BOOL fInit )
{
    if ( !fInit ) {
	SelectObject( hdcChart, hbrOrg );
	DeleteObject( hbrBkgnd );
	SelectObject( hdcChart, hpenOrg );
	DeleteObject( hpenFrame );
    }

    hbrBkgnd = CreateSolidBrush( GetSysColor(COLOR_WINDOW) );
    UnrealizeObject( hbrBkgnd );
    hpenFrame = CreatePen( PS_SOLID, 0, GetSysColor(COLOR_WINDOWFRAME) );
    UnrealizeObject( hpenFrame );

    MySetObjectOwner( hbrBkgnd );
    MySetObjectOwner( hpenFrame );

    hbrOrg = SelectObject( hdcChart, hbrBkgnd );
    hpenOrg = SelectObject( hdcChart, hpenFrame );
}


void CleanupRate()
{
    SelectObject( hdcChart, hbmpOrgChart );
    DeleteObject( hbmpChart );

    SelectObject( hdcChart, hbrBkgnd );
    DeleteObject( hbrBkgnd );

    SelectObject( hdcChart, hbrRed );
    DeleteObject( hbrRed );

    SelectObject( hdcChart, hpenFrame );
    DeleteObject( hpenFrame );

    DeleteDC( hdcChart );
}
