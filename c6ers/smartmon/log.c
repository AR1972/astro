/**************************************************************************/
/***									***/
/***	LOG.C	- Cache hit rate logging routines			***/
/***									***/
/***									***/
/***									***/
/***									***/
/**************************************************************************/

#include <windows.h>
#include <io.h>
#include <stdio.h>
#include "smartmon.h"

char szLogFile[MAXFILENAMELEN];
char *szLogTitle = "   ticks,    total,     hits\r\n";
BOOL fLogAppend = TRUE;
BOOL fAutoStop;
WORD AutoLogTime;	// minutes
int hLogFile;


/**************************************************************************/
/***									***/
/***	StartLog							***/
/***									***/
/**************************************************************************/

BOOL StartLog()
{
    OFSTRUCT of;
    WORD mode;

    mode = OF_WRITE | OF_SHARE_DENY_WRITE;

    //
    // Create file if it doesn't exist or new file is requested
    //
    if ( !fLogAppend ||
	 OpenFile( (LPSTR)szLogFile, (OFSTRUCT FAR *)&of, OF_EXIST ) == -1)
	mode |= OF_CREATE;

    hLogFile = OpenFile( (LPSTR)szLogFile, (OFSTRUCT FAR*)&of, mode );

    if ( hLogFile != -1 ) {
	if ( mode & OF_CREATE )
	    write( hLogFile, szLogTitle, lstrlen(szLogTitle) );
	if ( fLogAppend ) {
	    lseek( hLogFile, 0L, SEEK_END );
	    write( hLogFile, "\r\n", 2 );
	}
	return TRUE;
    }

    return FALSE;
}


/**************************************************************************/
/***									***/
/***	StopLog 							***/
/***									***/
/**************************************************************************/

void StopLog()
{
    if ( hLogFile ) {
	close( hLogFile );
	hLogFile = 0;
    }
}


/**************************************************************************/
/***									***/
/***	WriteLog							***/
/***									***/
/**************************************************************************/

BOOL WriteLog( void )
{
    extern DWORD PrevTotal, PrevHits;
    extern char szBuffer[];

    wsprintf( szBuffer, "%8lu, %8lu, %8lu\r\n",
	GetTickCount(), PrevTotal, PrevHits );
    return write( hLogFile, szBuffer, lstrlen(szBuffer) ) != -1;
}
