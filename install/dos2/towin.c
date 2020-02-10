#include		"winenv.h"
#include		"ws.h"
#include		"wsrc.h"
#include		"pro.h"
#include		"lib\\common\\sulib.h"

typedef		unsigned char		UCHAR;
#include		<ctype.h>
#include		<string.h>
#include 	<copy.h>
#include		<strlib.h>
#include		<errors.h>

/***************************************************************************/

#define		OK								0
#define		FATAL_MEMORY_ERROR		0
#define		MAX_PATH_LEN				128
#define		READ							0

#define		FATAL_DLL_LOAD_ERROR		28

extern char					szTmpDos[ MAX_PATH_LEN ];
extern unsigned char 	gInstallMode;	
extern int              CreatingRecovery; /* Signals creating Uninstal disk	*/


/***************************************************************************/
/*
**	Windows area functions
*/

WORD PUBLIC wsCopyStatus(int msg, int n, LPSTR szFile);
//BOOL PUBLIC AppQuit( void );

VOID FAR PASCAL fdos_dskreset( void );

/***************************************************************************/
/*
**	DOS area functions
*/

void		InitUninstallDisk	   ( int iUninstallDiskNum );
void		FatalError				( int ErrorType );
int		WinDiskNum				( int DosDiskNum );
void		ProcessCopyError		( char *szFile, int ErrNo );
void		DisplayFileStatus		( char *szFile, int Type );

void 		InsertDisk( int DskNum );

/***************************************************************************/
/*			 	
**	GLobals
*/

static int	gDiskNum;
struct MULT_FILES *gFiles;

int iUninstallDiskNum;   /* Always contains the uninstall disk number. (Zero Based) */

/***************************************************************************/
/*
**	Windows publics
*/

extern int	   XDosCopy;			/* Defined in Windows setup copy.c	*/
extern char		szDosDesName[];	/* Defined in Windows setup copy.c	*/

/***************************************************************************/

void *GetMemory( unsigned uMemSize )
{
	HANDLE	Ptr;

	if ( (Ptr = LocalAlloc( LMEM_FIXED, (WORD)uMemSize )) != NULL )
		return( (void *)Ptr );
	else
		FatalError( FATAL_MEMORY_ERROR );
}

/***************************************************************************/

void FreeMemory( void *Ptr )
{
	if ( LocalFree( (HANDLE)Ptr ) != NULL )
		FatalError( FATAL_MEMORY_ERROR );
}

/***************************************************************************/

void FatalError( int ErrorType )
{
	extern	iWillReboot;

	wsFatalError( ErrorType, iWillReboot );
	AppQuit();
}

/***************************************************************************/

void Xcopy( struct MULT_FILES *Files )
{
	#define			COPY_FLAGS			(FC_QUALIFIED | FC_DEST_QUALIFIED)
	char				szSource[ MAX_PATH_LEN ];
	char				szDestin[ MAX_PATH_LEN ];
	char				*szPtr;
	int				iDiskNum;

	XDosCopy = TRUE;

   gFiles = Files;
	if ((iDiskNum = Files->DiskNumber) != gDiskNum)  
		InsertDisk(gDiskNum = iDiskNum);

	while ( Files->Name.Source != NULL )
	{
		gFiles = Files;
		gDiskNum = Files->DiskNumber;
		if (iDiskNum != gDiskNum)
			// This file is on a different distribution disk. Prompt for it.
			InsertDisk(iDiskNum = gDiskNum);

		BuildPath( szSource, Files->Drive.Source, Files->Path.Source,
					  Files->Name.Source );
			
		BuildPath( szDestin, Files->Drive.Destin, Files->Path.Destin, "" );

		szPtr = strchr( szDestin, EOL );
		if ( szPtr > (szDestin + 3) )
			*(--szPtr) = EOL;				/* Strip trailing backslash	*/

							/* Set destination file name Windows copy will use	*/
		strcpy( szDosDesName, Files->Name.Destin );

		if ( CreatingRecovery )
			InitUninstallDisk(iUninstallDiskNum);

		DisplayFileStatus( szDosDesName, READ );

      if ( FileCopy(szSource,szDestin,(FPFNCOPY)wsCopyStatus,COPY_FLAGS) && CreatingRecovery )
			ProcessCopyError( ParseFileName( szSource ), ERR_PROCESSING );

		Files++;
	}
	XDosCopy = FALSE;

	#undef		COPY_FLAGS
}		

/***************************************************************************/
/*																									*/
/*		Routine Name		: PromptForDisk												*/
/*																									*/
/*																									*/
/***************************************************************************/

void PromptForDisk( char chSourceDrv )
{
	static char		szPath[] = "X:\\";

	szPath[ 0 ] = chSourceDrv;
	wsInsertDisk( (LPSTR) szPath );

   wsYield(NULL); // Allow repaint to erase disk prompt dialog.
}

/***************************************************************************/

void PromptForUninstall(char *szUserLabel )
{
	wsInsertUninstall(szUserLabel);

   wsYield(NULL); // Allow repaint to erase uninstall disk prompt.
}

/***************************************************************************/

void FmtStat( unsigned uTrack, unsigned uHead )
{
	register				Percent;
	static unsigned	uTotalTrks;

	if ( uHead == (unsigned)-1 )
	{												/* Display dialog box at 0%		*/
		uTotalTrks = uTrack;					/* Save total number of tracks	*/
		wsFmtDlgOpen();
	}
	else if ( uTrack == (unsigned)-1	) {
		wsFmtDlgClose();						/* Remove status dialog	*/
      wsYield(NULL);                   // Yield a bit to fix unsightly re-paint delay.
   }
	else
	{												/* Update status dialog percentage	*/
		Percent = (int)( (long)(uTrack + 1) * 100L / (long)(uTotalTrks) );
		wsFmtDlgUpdate( Percent );
	}
}

/***************************************************************************/

void ProcessCopyError( char *szFile, int ErrorType )
{
	extern int		CreatingRecovery;

	wsDosCopyError( szFile, ErrorType, CreatingRecovery );

	if ( CreatingRecovery )
		AppQuit();
}

/***************************************************************************/
/* The drive letter argument is not needed in the DOS/Windows Setup			*/
/* because the only removeabale drive that is ever accessed will be A:.		*/
/***************************************************************************/

void WriteProtectPrompt( char Disk )
{
   wsNonFatalError( IDS_WRITEPROTECT );
}

/***************************************************************************/

void NotReadyPrompt( char Disk )
{
   wsNonFatalError( IDS_NOTREADY );
}

/***************************************************************************/

int PromptForDiskFmt( int iDriveType )
{
	return( (int)wsSelectDskFmt(iDriveType) );
}

/***************************************************************************/
/***************************************************************************/

void ProcessDiskError( int iErrorType )
{
	iErrorType--;						/* Convert to Zero based message offset	*/
	wsNonFatalError( iErrorType );
}

#if 0

/***************************************************************************/
/* Interface function which calls the Windows Setup copy function.			*/
/*																									*/
/*	int WinFileCopy( char *szSource, char *szDestPath, char *szName );		*/
/*																									*/
/*	ARGUMENTS:	szSource		- Source file and disk number "1:msdos.sys"		*/
/*					szDestPath	- Fully qualified destination path only			*/
/*					szName		- Destination file name									*/
/*	RETURNS:		int			- OK if no errors else !OK								*/
/*																									*/
/***************************************************************************/

int WinFileCopy( char *szSource, char *szPath, char *szName )
{
	#define		COPY_FLAGS			(FC_FILE | FC_DEST_QUALIFIED)
	int			iStatus;


	XDosCopy = TRUE;
	strcpy( szDosDesName, szName );
	szSource[ 0 ] = (char)WinDiskNum( (int)szSource[ 0 ] ); 


	DisplayFileStatus( szDosDesName, READ );

	iStatus = FileCopy( szSource, szPath, (FPFNCOPY)wsCopyStatus,
							  COPY_FLAGS );
	XDosCopy = FALSE;
	return( iStatus );

	#undef		COPY_FLAGS
}

/***************************************************************************/
/***************************************************************************/

int WinDiskNum( int iDosDiskNum )
{
	char		szNum[ MAX_INF_LINE_LEN ];	
	int		iFirstDosDisk;	

	infGetProfileString( NULL, "data", "firstdosdisk", szNum );
	iFirstDosDisk = atoi( szNum );
	return( iDosDiskNum + iFirstDosDisk );
}

#endif

/***************************************************************************/
/* Displays the status of the current file in a the Windows dialog.			*/
/*																									*/
/*	void DisplayFileStatus( char *szFile, int Type )								*/
/*																									*/
/*	ARGUMENTS:	szFile	- Ptr to current file's name.								*/
/*					Type		- Type of file processing.									*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void DisplayFileStatus( char *szFile, int Type )
{
	char			szName[ 25 ];

	strcpy( szName, szFile );

	#if 0
		char			*szPtr;
		char			ch;	

		if ( (szPtr = strchr( szName, '.' )) != NULL )	/* Delete file extension*/
			*szPtr = EOL;
		strlwr( szName );
	#endif

	strupr( szName );

	ProPrintf( ID_STATUS2, wsLoadSz( IDS_FSTATUS + Type, NULL, NULL ),
				  (LPSTR)szName );
}

/***************************************************************************/
/* Windows code interface function to update the file copy gage by a			*/
/* specified number of files.																*/
/*																									*/
/*	void UpdateFileBar( int iFiles )														*/
/*																									*/
/*	ARGUMENTS:	iFiles	- Number of files to add to bar position				*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void UpdateFileBar( int iFiles )
{
	ProDeltaPos( iFiles );
}

/***************************************************************************/
/* Copies the README.NOW text from the resource file into the callers		*/
/* buffer, adding the proper carriage return - line feed extension to each	*/
/* line in the buffer.																		*/
/*																									*/
/*	void GetReadMeText( char *Buf )														*/
/*																									*/
/*	ARGUMENTS:	Buf		- Ptr to a buffer large enough to hold the entire	*/
/*								  README.NOW file.											*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void GetReadMeText( char *Buf )
{
	register			RcId;
	static char		CrLf[ 3 ] = { CR, LF, EOL };

	for ( *Buf = EOL, RcId = IDS_DOSREADME; RcId <= IDS_END_README; RcId++ )
	{
		lstrcat( Buf, wsLoadSz( RcId, NULL, NULL ) );
		lstrcat( Buf, CrLf );

	}
}

/***************************************************************************/
/***************************************************************************/

/*
int _dos_dskreset( void )
{
   char     		szDllName[MAX_SYS_INF_LEN];
	HANDLE			hLibrary;
	static int		iFirstTime = TRUE;
	static void		(FAR PASCAL *lpFunc)( void );

	if (GetWinFlags() & WF_PMODE )
		return( OK );

	if ( iFirstTime )
	{
		if ( !infGetProfileString( NULL, "data", "int13dll", szDllName ) ||
			  (hLibrary = LoadLibrary( szDllName )) < 32 ||
			  (lpFunc = GetProcAddress( hLibrary, "fdos_dskreset" )) == NULL )
			wsFatalError( FATAL_DLL_LOAD_ERROR,  FALSE );
		else
			iFirstTime = FALSE;
	}

	(*lpFunc)();
	return( OK );
}
*/

/***************************************************************************/
/***************************************************************************/

void wffree( void far *n )
{
	GlobalFree((HANDLE)HIWORD((LONG)n));
}

/***************************************************************************/
/***************************************************************************/
void far *wfmalloc( WORD n )
{
	return( (VOID FAR *)MAKELONG(0, GlobalAlloc(GPTR, (DWORD)n)) );
}

/***************************************************************************/
/***************************************************************************/

void ForceExitReboot( void )
{
	fExit |= EF_NOEXIT;
}


void XHcopy( struct MULT_FILES *Files ,char * szTmpDos)
{
	#define			COPY_FLAGS			(FC_QUALIFIED | FC_DEST_QUALIFIED)
	char				szSource[ MAX_PATH_LEN ];
	char				szDestin[ MAX_PATH_LEN ];
	char				*szPtr;

	gFiles = Files;
	ProPrintf(ID_STATUS1, wsLoadSz(IDS_WAITCOPYRECHD,NULL,NULL));	
	XDosCopy = TRUE;
	InsertDisk(gDiskNum = Files->DiskNumber);

	while ( Files->Name.Source != NULL )
	{
		gFiles = Files;
		BuildPath( szSource, Files->Drive.Source, Files->Path.Source,
					  Files->Name.Source );

		BuildPath( szDestin, szTmpDos[0], szTmpDos + 2, "" );

		szPtr = strchr( szDestin, EOL );
		if ( szPtr > (szDestin + 3) )
			*(--szPtr) = EOL;				/* Strip trailing backslash	*/

	  	/* Set destination file name Windows copy will use	*/
		strcpy( szDosDesName, Files->Name.Destin );

		DisplayFileStatus( szDosDesName, READ );

      if ( FileCopy(szSource,szDestin,(FPFNCOPY)wsCopyStatus,COPY_FLAGS) && CreatingRecovery )
			ProcessCopyError( ParseFileName( szSource ), ERR_PROCESSING );

		Files++;
	}
	XDosCopy = FALSE;
	#undef		COPY_FLAGS
}

void CopyFiles( int id, struct MULT_FILES *Files )
{
	if ( id != NULL )
		ProPrintf(ID_STATUS1, wsLoadSz(id,NULL,NULL));	

	Xcopy(Files);
}


void ProPrintfStat1( int id )
{
		ProPrintf(ID_STATUS1, wsLoadSz(id,NULL,NULL));	
}
	


