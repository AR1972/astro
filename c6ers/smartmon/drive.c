/**************************************************************************/
/***									***/
/***	DRIVE.C     - Drive selection routines				***/
/***									***/
/***	Largely borrowed from ClarkC's COMMDLG FILEOPEN.C               ***/
/***									***/
/***									***/
/**************************************************************************/

#include <windows.h>
#include "smartmon.h"

#define cbCaption       64
#define _MAX_PATH	256
#define CBN_DRAW	0x8000	 /* Used with OFN_COMBODOWN */
#define dxSpace         4

extern WORD wWinVer;
extern HANDLE hInst;

static WORD dyItem = 0;
static WORD dyText;
WORD dxDirDrive = 0;
WORD dyDirDrive = 0;
HDC  hdcMemory = 0;
HBITMAP hbmpOrigMemBmp;
HBITMAP hbmpDirDrive = (HANDLE)NULL;
char CurDriveList[27] = {0};
WORD CurDriveCount;
WORD cCurDriveSel = -1;

static DWORD rgbWindowColor = 0xFF000000;  /* Not a valid RGB color */
static DWORD rgbWindowText  = 0xFF000000;
static DWORD rgbHiliteColor = 0xFF000000;
static DWORD rgbHiliteText  = 0xFF000000;
static DWORD rgbDDWindow    = 0xFF000000;
static DWORD rgbDDHilite    = 0xFF000000;

#define rgbSolidBlue	0x00FF0000

BOOL ChangeDrive( char );
void InitDriveBox( HWND );
void ListDrives( HWND );
char SimpleLower( char );
VOID StringLower( LPSTR );
int  GetDriveIndex( WORD, WORD );
HBITMAP LoadAlterBitmap( int, DWORD, DWORD );
void vDeleteDirDriveBitmap( void );
BOOL LoadDirDriveBitmap( void );
void MySetObjectOwner( HANDLE );
LONG RgbInvertRgb( LONG );
void SetRGBValues( void );
void ResetDriveBox( HWND );

/**************************************************************************/
/***                                                                    ***/
/***	ChangeDrive							***/
/***                                                                    ***/
/**************************************************************************/

BOOL ChangeDrive( char chDrv )
{
    char cCurDrive;
    BOOL rc = FALSE;

    chDrv |= 0x60;
    cCurDrive = get_current_drive();
    set_current_drive( chDrv - 'a' );

    if ( is_CDROM_drive( chDrv - 'a' ) )
	if ( !is_valid_CD() )
	    goto cd_failed;

    if ( chDrv == get_current_drive() )
	return TRUE;

cd_failed:

    set_current_drive( cCurDrive - 'a' );
    return FALSE;
}


/**************************************************************************/
/***                                                                    ***/
/***	SetRGBValues							***/
/***                                                                    ***/
/**************************************************************************/

void SetRGBValues()
{
    rgbWindowColor = GetSysColor( COLOR_WINDOW );
    rgbWindowText  = GetSysColor( COLOR_WINDOWTEXT );
    rgbHiliteColor = GetSysColor( COLOR_HIGHLIGHT );
    rgbHiliteText  = GetSysColor( COLOR_HIGHLIGHTTEXT );
}


void InitDriveBitmap()
{
    HBITMAP hbmpTemp;
    HDC hdcScreen;

    //
    // Create a DC that is compatible with the screen and find the
    // handle of the null bitmap
    //
    SetRGBValues();
    hdcScreen = GetDC( 0 );
    hdcMemory = CreateCompatibleDC( hdcScreen );
    hbmpTemp = CreateCompatibleBitmap( hdcMemory, 1, 1 );
    hbmpOrigMemBmp = SelectObject( hdcMemory, hbmpTemp );
    SelectObject( hdcMemory, hbmpOrigMemBmp );
    DeleteObject( hbmpTemp );
    MySetObjectOwner( hdcMemory );
    ReleaseDC( 0, hdcScreen );

    LoadDirDriveBitmap();
}


/**************************************************************************/
/***                                                                    ***/
/***	InitDriveBox							***/
/***                                                                    ***/
/**************************************************************************/

void InitDriveBox( HWND hWnd )
{
    if ( wWinVer >= 0x030A )
	SendDlgItemMessage( hWnd, IDD_DRIVEID, CB_SETEXTENDEDUI,
	    (WPARAM)1, (LPARAM)0);
    else if ( GetSysModalWindow() )
	/* Windows 3.00, sysmodal bug.	clarkc	 2 May 1991 */
	EnableWindow( GetDlgItem(hWnd, IDD_DRIVEID), FALSE );

    ResetDriveBox( hWnd );
}


/**************************************************************************/
/***                                                                    ***/
/***	ResetDriveBox							***/
/***									***/
/**************************************************************************/

void ResetDriveBox( HWND hWnd )
{
    int i;
    HWND hCtl;

    SetRGBValues();
    LoadDirDriveBitmap();

    CurDriveCount = count_valid_drives( CurDriveList );

    //
    // If the current drive selection is gone, (say someone removed
    // a network drive from a DOS session), select the current drive.
    //
    if ( cCurDriveSel != -1 ) {
	for ( i = 0; i < CurDriveCount; i++ )
	    if ( cCurDriveSel == CurDriveList[i+1] )
		goto drive_still_good;
    }

    cCurDriveSel = get_current_drive() - 'a';

drive_still_good:

    hCtl = GetDlgItem( hWnd, IDD_DRIVEID );
    InvalidateRect( hCtl, (LPRECT)NULL, TRUE );

    ListDrives( hWnd );

    PostMessage( hWnd, WM_COMMAND, IDD_DRIVEID, MAKELPARAM(hCtl, CBN_SELCHANGE) );
}


/**************************************************************************/
/***                                                                    ***/
/***	ListDrives							***/
/***                                                                    ***/
/**************************************************************************/

void ListDrives( HWND hWnd )
{
    short i;
    char cBuffer[_MAX_PATH];
    HWND hCmb = GetDlgItem( hWnd, IDD_DRIVEID );
    WORD iCurrentDrive, iCurrentDriveType;

    SendMessage( hCmb, WM_SETREDRAW, FALSE, 0L );
    SendMessage( hCmb, CB_RESETCONTENT, 0, 0L );

    for( i = 0; i < CurDriveCount; i++ ) {

	iCurrentDrive = (WORD)CurDriveList[i+1];

	/* Note: it is very important that the uppercase 'A' be used for the
	 *	 drive letter in cBuffer[0], as the Novell Netware driver
	 *	 will GP Fault if you pass in a lowercase drive letter.
	 *	 30 October 1991	 Clark Cyr
	 */
	cBuffer[0] = (char) (iCurrentDrive + 'A');
	cBuffer[1] = ':';
	cBuffer[2] = '\0';

	iCurrentDriveType = GetDriveType( iCurrentDrive );
	if ( iCurrentDriveType < 2)  /* Is it a phantom?  Skip it! */
	    continue;

	if ( (iCurrentDriveType != DRIVE_REMOVABLE) &&
	     !((iCurrentDriveType == DRIVE_REMOTE) &&
	       is_CDROM_drive(iCurrentDrive)) ) {
	    if ( ChangeDrive(cBuffer[0]) ) {
		if ( iCurrentDriveType != DRIVE_REMOTE ) {
		    cBuffer[2] = ' ';
		    get_volume_label( iCurrentDrive, (LPSTR)(cBuffer+3) );
		    OemToAnsi( (LPSTR)cBuffer, (LPSTR)cBuffer );
		} else {
		    WORD iSel;
		    char szTempField[cbCaption];

		    /* Set the first character to zero.  If the drive is
		     * disconnected, the call to WNetGetConnection() will
		     * return a value other than WN_SUCCESS, but the string
		     * will be valid.  If the string isn't altered, wsprintf
		     * will just place the null string after the space.
		     * 18 July 1991    ClarkC
		     */
		    szTempField[0] = '\0';
		    iSel = cbCaption;
		    WNetGetConnection( (LPSTR)cBuffer, (LPSTR)szTempField,
			(LPWORD)&iSel);
		    wsprintf( (LPSTR)(cBuffer+2), " %s", (LPSTR)szTempField );
		}
	    }
	}

	StringLower( (LPSTR)cBuffer );
	SendMessage( hCmb, CB_INSERTSTRING, (WPARAM)i,
	    (LPARAM)(LPSTR)cBuffer);
	SendMessage( hCmb, CB_SETITEMDATA, (WPARAM) i, (LPARAM)(DWORD)(
	    GetDriveIndex( iCurrentDrive, iCurrentDriveType ) ) );
	if ( iCurrentDrive == cCurDriveSel )
	   SendMessage( hCmb, CB_SETCURSEL, (WPARAM) i, 0 );
    }

    SendMessage( hCmb, WM_SETREDRAW, (WPARAM)TRUE, 0L );
    ChangeDrive( (char)(CurDriveList[0] + 'a') );
}


/**************************************************************************/
/***                                                                    ***/
/***	SimpleLower							***/
/***                                                                    ***/
/**************************************************************************/

char SimpleLower( char chChar )
{
    _asm {
	mov	al, chChar
	cmp	al,'A'
	jb	NoChange
	cmp	al,'Z'
	ja	NoChange
	add	al,'a'-'A'
NoChange:
    }
}


/**************************************************************************/
/***                                                                    ***/
/***	StringLower							***/
/***                                                                    ***/
/**************************************************************************/

VOID StringLower( LPSTR lpsz )
{
    if ( lpsz )
	while ( *lpsz ) {
	    *lpsz = SimpleLower( *lpsz );
	    lpsz++;
        }
}


/**************************************************************************/
/***                                                                    ***/
/***	GetDriveIndex							***/
/***                                                                    ***/
/**************************************************************************/

int GetDriveIndex( WORD wDrive, WORD wDriveType )
{
    register short i = HARDDRVBMP;

    if ( wDriveType == 1 )	/* Drive doesn't exist! */
	return(0);

    if ( is_CDROM_drive( wDrive ) )
	i = CDDRVBMP;

    else if ( wDriveType == DRIVE_REMOVABLE )
	i = FLOPPYBMP;

    else if ( wDriveType == DRIVE_REMOTE )
	i = NETDRVBMP;

    else if ( is_RAM_drive( wDrive ) )
	i = RAMDRVBMP;

    return i;
}


/**************************************************************************/
/***                                                                    ***/
/***	MeasureItem							***/
/***                                                                    ***/
/**************************************************************************/

void MeasureItem( HANDLE hDlg, LPMEASUREITEMSTRUCT mis )
{
    HDC hDC = GetDC( hDlg );
    TEXTMETRIC tm;
    HANDLE hFont;

    if ( !dyItem ) {
	hFont = (HANDLE)(DWORD)SendMessage(hDlg, WM_GETFONT, 0, 0L);
	if (!hFont)
	    hFont = GetStockObject( SYSTEM_FONT );
	hFont = SelectObject( hDC, hFont );
	GetTextMetrics( hDC, &tm );
	SelectObject( hDC, hFont );
	ReleaseDC( hDlg, hDC );
	dyText = tm.tmHeight;
	dyItem = max( dyDirDrive, dyText );
    }

    mis->itemHeight = dyItem;
}


/**************************************************************************/
/***                                                                    ***/
/***	DrawItem							***/
/***                                                                    ***/
/**************************************************************************/

void DrawItem( HWND hDlg, WPARAM wParam, LPDRAWITEMSTRUCT lpdis, BOOL bSave )
{
    HDC hdcList;
    RECT rectHilite;
    char szText[_MAX_PATH+1];
    WORD dxAcross;
    short nHeight;
    LONG rgbBack, rgbText, rgbOldBack, rgbOldText;
    short nShift = 1;		     /* to shift directories right in lst2 */
    BOOL bSel;
    int BltItem;

    *szText = 0;
    if ( lpdis->CtlID != IDD_DRIVEID )
	return;

    hdcList = lpdis->hDC;

    SendDlgItemMessage( hDlg, lpdis->CtlID, CB_GETLBTEXT,
	(WPARAM)lpdis->itemID, (LPARAM)(LPSTR)szText );

//  This piece of code will cause the default optimization to truncate
//  the rest of the routine.
//
//  if (*szText == 0) {
//	/* if empty listing */
//	DefWindowProc( hDlg, WM_DRAWITEM, wParam, (LPARAM)lpdis );
//	return;
//  }

    AnsiLower((LPSTR) szText);

    nHeight = dyItem;

    CopyRect((LPRECT)&rectHilite, (LPRECT) &lpdis->rcItem);
    rectHilite.bottom = rectHilite.top + nHeight;

    /* Under Win 3.0 in a combobox, if it's in the listbox, it only has to
    /* be selected because focus isn't noted properly.
     */
    if ((wWinVer < 0x030A) && (lpdis->CtlType == ODT_COMBOBOX) &&
	(lpdis->rcItem.left == 0) && (lpdis->itemState & ODS_SELECTED))
	lpdis->itemState |= ODS_FOCUS;

    /* Careful checking of bSel is needed here.  Since the file listbox (lst1)
     * can allow multiselect, only ODS_SELECTED needs to be set.  But for the
     * directory listbox (lst2), ODS_FOCUS also needs to be set.
     * 03 September 1991      Clark Cyr
     */
    bSel = (lpdis->itemState & (ODS_SELECTED | ODS_FOCUS));
    if ((bSel & ODS_SELECTED) || (bSel & ODS_FOCUS)) {
	rgbBack = rgbHiliteColor;
	rgbText = rgbHiliteText;
    } else {
	rgbBack = rgbWindowColor;
	rgbText = rgbWindowText;
    }
    rgbOldBack = SetBkColor( hdcList, rgbBack );
    rgbOldText = SetTextColor( hdcList, rgbText );

    dxAcross = dxDirDrive / BMPHIOFFSET;
    BltItem = (int)(WORD)(DWORD)SendDlgItemMessage(hDlg, IDD_DRIVEID,
	CB_GETITEMDATA, (WPARAM) lpdis->itemID, 0);
    if ( bSel & ODS_SELECTED )
	BltItem += BMPHIOFFSET;

    /* Draw the name */
    ExtTextOut( hdcList, rectHilite.left+dxSpace+dxAcross + dxSpace * nShift,
	rectHilite.top + (nHeight - dyText)/2, ETO_OPAQUE | ETO_CLIPPED,
	(LPRECT) &rectHilite, (LPSTR) szText, lstrlen((LPSTR) szText),
	NULL );

    BitBlt(hdcList, rectHilite.left+dxSpace*nShift,
	rectHilite.top + (dyItem - dyDirDrive)/2,
	dxAcross, dyDirDrive, hdcMemory, BltItem*dxAcross, 0, SRCCOPY);

    SetTextColor( hdcList, rgbOldText );
    SetBkColor( hdcList, rgbOldBack );

    if ( lpdis->itemState & ODS_FOCUS )
	DrawFocusRect( hdcList, (LPRECT)&lpdis->rcItem );
    return;
}


/**************************************************************************/
/***                                                                    ***/
/***	vDeleteDirDriveBitmap						***/
/***                                                                    ***/
/**************************************************************************/

void vDeleteDirDriveBitmap()
{
    SelectObject( hdcMemory, hbmpOrigMemBmp );
    if ( hbmpDirDrive != (HANDLE)NULL ) {
	DeleteObject( hbmpDirDrive );
	hbmpDirDrive = (HANDLE)NULL;
    }
}


/**************************************************************************/
/***                                                                    ***/
/***	LoadDirDriveBitmap						***/
/***                                                                    ***/
/***	Creates the drive/directory bitmap.  If an appropriate bitmap	***/
/***	already exists, it just returns immediately.  Otherwise, it	***/
/***	loads the bitmap and creates a larger bitmap with both regular	***/
/***	and highlight colors.						***/
/***                                                                    ***/
/**************************************************************************/

BOOL LoadDirDriveBitmap()
{
    BITMAP  bmp;
    HANDLE  hbmp, hbmpOrig;
    HDC     hdcTemp;
    BOOL    bWorked = FALSE;

    if ((hbmpDirDrive != (HANDLE)NULL) &&
	(rgbWindowColor == rgbDDWindow) && (rgbHiliteColor == rgbDDHilite))
	if ( SelectObject( hdcMemory, hbmpDirDrive ) )
            return(TRUE);

    vDeleteDirDriveBitmap();

    rgbDDWindow = rgbWindowColor;
    rgbDDHilite = rgbHiliteColor;

    if ( !(hdcTemp = CreateCompatibleDC(hdcMemory)) )
        goto LoadExit;

    if ( !(hbmp = LoadAlterBitmap(IDS_DRVBMP, rgbSolidBlue, rgbWindowColor)))
        goto DeleteTempDC;

    GetObject( hbmp, sizeof(BITMAP), (LPSTR) &bmp );
    dyDirDrive = bmp.bmHeight;
    dxDirDrive = bmp.bmWidth;

    hbmpOrig = SelectObject( hdcTemp, hbmp );

    hbmpDirDrive = CreateDiscardableBitmap( hdcTemp, dxDirDrive*2, dyDirDrive );
    if ( !hbmpDirDrive )
        goto DeleteTempBmp;

    if ( !SelectObject(hdcMemory, hbmpDirDrive) ) {
        vDeleteDirDriveBitmap();
        goto DeleteTempBmp;
    }

    BitBlt( hdcMemory, 0, 0, dxDirDrive, dyDirDrive, hdcTemp, 0, 0, SRCCOPY );
    SelectObject( hdcTemp, hbmpOrig );
    DeleteObject( hbmp );

    if ( !(hbmp = LoadAlterBitmap(IDS_DRVBMP, rgbSolidBlue, rgbHiliteColor)) )
        goto DeleteTempDC;

    hbmpOrig = SelectObject( hdcTemp, hbmp );
    BitBlt( hdcMemory, dxDirDrive, 0, dxDirDrive, dyDirDrive,
	hdcTemp, 0, 0, SRCCOPY );
    SelectObject( hdcTemp, hbmpOrig );

    MySetObjectOwner( hbmpDirDrive );
    bWorked = TRUE;

DeleteTempBmp:
    DeleteObject( hbmp );
DeleteTempDC:
    DeleteDC( hdcTemp );
LoadExit:
    return bWorked;
}


/**************************************************************************/
/***                                                                    ***/
/***	LoadAlterBitmap 						***/
/***									***/
/***	Loads a bitmap given its name and gives all the pixels that are ***/
/***	a certain color a new color.					***/
/***                                                                    ***/
/**************************************************************************/

HBITMAP LoadAlterBitmap(int id, DWORD rgbReplace, DWORD rgbInstead)
{
    LPBITMAPINFOHEADER  qbihInfo;
    HDC                 hdcScreen;
    BOOL                fFound;
    HANDLE              hresLoad;
    HANDLE              hres;
    DWORD FAR *         qlng;
    LPBYTE              qbBits;
    HANDLE              hbmp;

    hresLoad = FindResource( hInst, MAKEINTRESOURCE(id), RT_BITMAP );
    if ( hresLoad == (HANDLE)NULL )
	return (HANDLE)NULL;
    hres = LoadResource( hInst, hresLoad );
    if ( hres == (HANDLE)NULL )
	return (HANDLE)NULL;

    rgbReplace = RgbInvertRgb( rgbReplace );
    rgbInstead = RgbInvertRgb( rgbInstead );
    qbihInfo = (LPBITMAPINFOHEADER)LockResource( hres );
    qlng = (LPLONG)((LPSTR)(qbihInfo) + qbihInfo->biSize);

    fFound = FALSE;
    while ( !fFound ) {
	if (*qlng == rgbReplace) {
            fFound = TRUE;
            *qlng = (LONG) rgbInstead;
	}
        qlng++;
    }
    UnlockResource( hres );

    qbihInfo = (LPBITMAPINFOHEADER)LockResource( hres );

    /* First skip over the header structure */
    qbBits = (LPBYTE)(qbihInfo + 1);

    /* Skip the color table entries, if any */
    qbBits += (1 << (qbihInfo->biBitCount)) * sizeof(RGBQUAD);

    /* Create a color bitmap compatible with the display device */
    hdcScreen = GetDC( (HANDLE)NULL );
    if ( hdcScreen != (HANDLE)NULL ) {
	hbmp = CreateDIBitmap( hdcScreen, qbihInfo, (LONG)CBM_INIT,
	    qbBits, (LPBITMAPINFO) qbihInfo, DIB_RGB_COLORS );
	ReleaseDC( (HANDLE)NULL, hdcScreen );
    }

    UnlockResource( hres );
    FreeResource( hres );

    return( hbmp );
}


/**************************************************************************/
/***                                                                    ***/
/***	MySetObjectOwner						***/
/***									***/
/***	Call SetObjectOwner in GDI, eliminating "<Object> not released" ***/
/***	error messages when an app terminates.				***/
/***                                                                    ***/
/**************************************************************************/

void MySetObjectOwner( HANDLE hObject )
{
    VOID (FAR PASCAL *lpSetObjOwner)(HANDLE, HANDLE);
    HMODULE hMod;

    if ( wWinVer >= 0x030A ) {
	if ( hMod = GetModuleHandle("GDI") )
	    if ( lpSetObjOwner = GetProcAddress(hMod, MAKEINTRESOURCE(461)) )
	       (lpSetObjOwner)( hObject, hInst );
    }
}


/**************************************************************************/
/***                                                                    ***/
/***	RgbInvertRgb							***/
/***									***/
/***	To reverse the byte order of the RGB value (for file format)	***/
/***	Return new color value (RGB to BGR)				***/
/***									***/
/**************************************************************************/

LONG RgbInvertRgb( LONG rgbOld )
{
    return((LONG) RGB(GetBValue(rgbOld), GetGValue(rgbOld), GetRValue(rgbOld)));
}
