/**************************************************************************/
/***									***/
/***	AUTO.C	- AUTOEXEC.BAT file munging routines			***/
/***									***/
/***									***/
/***									***/
/***									***/
/**************************************************************************/

#include <windows.h>
#include <io.h>
#include <stdio.h>
#include <string.h>
#include "smartmon.h"

#define MAX_LINE_LEN	128

extern char szBatchFile[];
char linebuf[ MAX_LINE_LEN ];

extern char szBuffer[];
extern HCURSOR hCurWait;
extern char CurDriveList[];
extern WORD CurDriveCount;

extern int  GetDriveIndex( WORD, WORD );
extern BOOL CheckForStacker( WORD );


LPSTR GetLine( LPSTR lpSrc, LPSTR lpEOF, char *pLine )
{
    //
    // Look for carriage return
    //
    while ( (*lpSrc != '\r') && (lpSrc < lpEOF) )
	*pLine++ = *lpSrc++;

    *pLine = 0;

    //
    // Skip next character if it's a linefeed
    //
    if ( lpSrc < lpEOF ) {
	lpSrc++;
	if ( (lpSrc < lpEOF) && (*lpSrc == '\n') )
	    lpSrc++;
    }

    return lpSrc;
}


BOOL IsSmartDrvLine( char *pLine )
{
    char ch;
    int nc;
    char *pLineOrg = pLine;
    char *pTemp;
    char *pSub;

    //
    // Skip leading spaces or any control chars
    //
    while ( ch = *pLine )
	if ( ch > ' ' )
	    break;
	else
	    pLine++;

    //
    // Make a copy of the line up to the first blank or tab
    //
    pSub = szBuffer;
    pTemp = pLine;
    while ( ch = *pLine ) {
	if ( (ch == ' ') || (ch == 0x09) )
	    break;
	else
	    if ( ch == '=' )
		//
		// Reject things like "path=smartdrv"
		//
		return FALSE;
	    else
		*pSub++ = ch;
	pLine++;
    }

    *pSub = 0;

    //
    // Reject labels
    //
    if ( szBuffer[0] == ':' )
	return FALSE;

    //
    // See if the substring ends with "smartdrv" or "smartdrv.exe"
    //
    AnsiLowerBuff( (LPSTR)szBuffer, nc = pLine-pTemp );
    if ( nc < 8 )
	return FALSE;
    if ( strcmp( (char *)(pSub-8), "smartdrv" ) == 0 )
	return TRUE;
    if ( (nc >= 12) && (strcmp( (char *)(pSub-12), "smartdrv.exe" ) == 0) )
	return TRUE;

    return FALSE;
}


BOOL WriteNewSmartDrvLine( int fh, char *pOldLine )
{
    extern DWORD DosCacheSize;	// Cache size (Kbyte) under DOS
    extern DWORD WinCacheSize;	// Cache size (Kbyte) under Windows

    int  i;
    char ch;
    char *pNewLine = szBuffer;
    WORD iDrv, iDrvIndex, status;

    //
    // Skip leading spaces or any control chars
    //
    while ( ch = *pOldLine )
	if ( ch > ' ' )
	    break;
	else
	    pOldLine++;

    //
    // Copy everything up to the first blank
    //
    while ( *pOldLine ) {
	ch = *pNewLine++ = *pOldLine++;
	if ( (ch == ' ') || (ch == 0x09) )
	    break;
    }

    //
    // Copy the options, if present
    //
    while ( TRUE )
	if ( *pOldLine == '/' )
	    while ( *pOldLine ) {
		ch = *pNewLine++ = *pOldLine++;
		if ( (ch == ' ') || (ch == 0x09) )
		    break;
	    }
	else
	    break;

    //
    // Eat the last blank
    //
    if ( *pOldLine )
	pNewLine--;


    //
    // Write out the status of each cacheable drive if it's
    // different from the default for that type of drive.
    // For floppy the default is read-only; for hard drive
    // it's read/write.
    //
    CurDriveCount = count_valid_drives( CurDriveList );
    for ( i = 0; i < CurDriveCount; i++ ) {

	iDrv = CurDriveList[i+1];
	status = cache_a_drive( GET, iDrv );
	iDrvIndex = GetDriveIndex( iDrv, GetDriveType( iDrv ) );

	if ( iDrvIndex == FLOPPYBMP ) {

	    if ( status & NO_READ )
		wsprintf( pNewLine, " %c-", (iDrv+'A') );
	    else if ( status & NO_WRITE )
		continue;   // default for floppy
	    else
		wsprintf( pNewLine, " %c+", (iDrv+'A') );

	    pNewLine += 3;

	} else if ( (iDrvIndex == HARDDRVBMP) && !CheckForStacker(iDrv) ) {

	    if ( status & NO_READ ) {
		wsprintf( pNewLine, " %c-", (iDrv+'A') );
		pNewLine += 3;
	    } else if ( status & NO_WRITE ) {
		wsprintf( pNewLine, " %c", (iDrv+'A') );
		pNewLine += 2;
	    } else
		continue;   // default for hard drives
	} else
	    continue;	    // default for non-cacheable drives
    }

    //
    // Write out cache size under DOS and Windows
    //
    wsprintf( pNewLine, " %lu %lu\r\n", DosCacheSize, WinCacheSize );

    if ( (i = lstrlen(szBuffer)) < MAX_LINE_LEN ) {
	if ( _lwrite( fh, (LPSTR)szBuffer, i ) == i )
	    return TRUE;
    }

    return FALSE;
}


BOOL WriteNewBatchFile( int fhNew, LPSTR lpf, LPSTR lpEOF )
{
    BOOL fFound = FALSE;
    BOOL rc = FALSE;

    *lpEOF = 0;
    while ( lpf < lpEOF ) {
	lpf = GetLine( lpf, lpEOF, linebuf );
	if ( linebuf[0] ) {
	    if ( IsSmartDrvLine( linebuf ) ) {
		if ( fFound )
		    //
		    // Abandon if found more than one smartdrv line
		    //
		    return FALSE;

		fFound = TRUE;
		rc = WriteNewSmartDrvLine( fhNew, linebuf );
		continue;
	    }
	    _lwrite( fhNew, (LPSTR)linebuf, lstrlen(linebuf) );
	}
	_lwrite( fhNew, (LPSTR)"\r\n", 2 );
    }

    return rc;
}


BOOL MungeBatchFile()
{
    BOOL rc = FALSE;
    int fhOld, fhNew, i;
    OFSTRUCT of;
    DWORD ulFileSize;
    HANDLE hFileBuf;
    BYTE far *lpf;
    char szNewFile[144];
    char szTempFile[144];
    HCURSOR hCur;
    BYTE cDrv;

    hCur = SetCursor( hCurWait );

    //
    // Because the rename function cannot do files on different drives,
    // find out where the targe batch file is, and force the temp files
    // to go there.
    //
    cDrv = (BYTE)AnsiUpper((DWORD)szBatchFile[0]) - 'A';
    if ( szBatchFile[1] != ':' )
	return FALSE;

    CurDriveCount = count_valid_drives( CurDriveList );
    for ( i = 0; i < CurDriveCount; i++ )
	if ( CurDriveList[i+1] == cDrv )
	    goto valid_drive;
    return FALSE;

valid_drive:

    cDrv = (cDrv + 'A') | TF_FORCEDRIVE;

    //
    // Open batch file for exclusive access
    //
    if (OpenFile( (LPSTR)szBatchFile, (OFSTRUCT FAR *)&of, OF_EXIST ) == -1)
	goto cleanup0;
    if ( (fhOld = OpenFile( (LPSTR)szBatchFile, (OFSTRUCT FAR*)&of,
	OF_READ | OF_SHARE_EXCLUSIVE )) == -1 )
	goto cleanup0;

    //
    // Don't do anything if file is > 64K.  Add an extra byte to
    // mark the end of file.
    //
    ulFileSize = _llseek( fhOld, 0L, 2 );
    if ( HIWORD( ulFileSize ) )
	goto cleanup1;

    //
    // Open a new file
    //
    GetTempFileName( cDrv, (LPSTR)"sdm", 0, (LPSTR)szNewFile );
    if ( (fhNew = OpenFile( (LPSTR)szNewFile, (OFSTRUCT FAR *)&of,
	OF_CREATE | OF_WRITE | OF_SHARE_EXCLUSIVE )) == -1 )
	goto cleanup1;

    //
    // See if there is enough space on the drive for the new
    // batch file plus one extra line.	The drive letter returned
    // by GetTempFileName is always in upper case.
    //
    if ( get_free_space(szNewFile[0]-'A') < (ulFileSize+MAX_LINE_LEN) )
	goto cleanup1;

    //
    // Load batch file into memory.  Don't close it until we are done.
    //
    _llseek( fhOld, 0L, 0 );
    if ( (hFileBuf = GlobalAlloc( GHND, ulFileSize+1 )) == 0 )
	goto cleanup1;
    lpf = GlobalLock( hFileBuf );
    if ( _lread( fhOld, lpf, ulFileSize ) != ulFileSize )
	goto cleanup2;

    //
    // Transfer old content to new file, modify SMARTDRV line in the process.
    //
    rc = WriteNewBatchFile( fhNew, lpf, lpf+(WORD)ulFileSize );

    _lclose( fhOld );
    _lclose( fhNew );

    //
    // Give new file the batch file name, and remove old one.
    //
    if ( rc ) {
	GetTempFileName( cDrv, (LPSTR)"sdm", 0, (LPSTR)szTempFile );
	DeletePathname( szTempFile );	// for the rename below

	if ( rename( szBatchFile, szTempFile ) == 0 ) {
	    if ( rename( szNewFile, szBatchFile ) == 0 ) {
		DeletePathname( szTempFile );
	    } else {
		//
		// Restore old AUTOEXEC.BAT
		//
		rename( szTempFile, szBatchFile );
		goto bail_out;
	    }
	} else
	    goto bail_out;
    } else {

bail_out:
	DeletePathname( szNewFile );
	rc = FALSE;
    }

cleanup2:
    GlobalUnlock( hFileBuf );
    GlobalFree( hFileBuf );
    _lclose( fhNew );

cleanup1:
    _lclose( fhOld );

cleanup0:
    SetCursor( hCur );
    return rc;
}
