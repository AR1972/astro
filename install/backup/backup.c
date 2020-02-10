/***************************************************************************/
/*																									*/
/* BACKUP.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Backup utility for the MS-DOS 4.0 retail upgrade. These functions will	*/
/* backup a specified disk to another specified disk. Then entry point to	*/
/* the utiltity is BackUp(). The file copying is buffered using an area of	*/
/* memory which as allocated previously by a call to InitCpyBuffer().		*/
/* Most functions communicate via a group of static global variables to		*/
/* keep the size of code to a mininum. 												*/
/*																									*/
/* All code is written so that it will run under MS-DOS version 2.11 and	*/
/* above. The backup format is restorable with versions of DOS 3.3 and		*/
/* above.																						*/
/* 																								*/
/* Created 07-12-89 johnhe																	*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include		<malloc.h>
#include 	<dos.h>
#include 	<fcntl.h>
#include		<io.h>
#include 	<string.h>
#include		<ctype.h>
#include		<bios.h>
#include		<sys\\types.h>
#include		<sys\\stat.h>

#include		<alias.h>
#include		<strlib.h>
#include		<disk_io.h>
#include		<fmt_io.h>
#include		<hdisk.h>
#include 	<window.h>
#include		<backup.h>

/***************************************************************************/
/* Static globals for this module.														*/
/* 																								*/
/* NOTE: pchCpyBuffer must be allocated at program initialization being		*/
/*			sure to leave enough DOS memory for the the rest of the program	*/
/*			to use through malloc. This is necessary because malloc doesn't	*/
/*			release DOS memory allocations until the end of the program and	*/
/*			this can cause large chunks for non-contiguous block which aren't	*/
/*			suitable for use by this module's file copy functions.				*/
/*																									*/
/***************************************************************************/


char huge			*pchCpyBuffer;			/* Huge buffer for files				*/
long					lBufSize;				/* Number of bytes int pchCpyBuffer */

char					chDestin;				/* Destination drive character		*/
char					chSource;				/* Destination drive character		*/

		/* Info will be allocated as an array of FCB structures for use		*/
		/* while transversing the levels of the disk directory. iDepth will	*/
		/* always indice the current directory level									*/

static struct INFO_STATE	*InfoState;	/* Buffer for current search states */
static struct find_t	*Info;				/* Array of FCB structures				*/
static char			*szPath;					/* Current DOS find search path		*/


static int			iDepth;					/* Info structure indice				*/
static int			iAttrib;					/* Current search attribute			*/

static int			OldDepth; 				/* Hold depth at start of disk 		*/
static int			OldAttrib; 				/* Holds attrib at start of disk		*/
static long			lOldFileOffset;		/* File offset at start of disk		*/

static int			iCtrlHandle = -1;		/* Control file handle					*/
static int			iBkupHandle = -1;		/* Backup file handle					*/

char					*szCtrlFile = "X:\\CONTROL.XXX      ";	/* File names */
char					*szBkupFile = "X:\\BACKUP.XXX       ";
char					*szDiskLabel= "BACKUP  000          ";	/* Disk label */


static struct RECORDS		*Records; 	/* Buffer for all the records */

static struct Disk_Header	*pHeader; 	/* The current ctrl file records */
static struct Dir_Block		*pDir;
static struct File_Header	*pFile;
static struct File_Header	*pPrevFile;

int					iBkupMode;				/* May be CONTROL, BACKUP or COUNT	*/
LONG					lDiskSize;				/* Bytes available on current disk	*/
int					iCount;					/*	Current disk number					*/


static int			iNewPath;				/* Flags that dir just changed		*/
static int			iDiskFull; 				/* Flags that disk is full				*/

static long			lCtrlOffset;			/* Current offset in Control file	*/
static long			lBkupOffset;			/* Current offset in Backup file		*/
static long			lFileOffset;			/* Offset in file being backed up	*/

static int			iBytesPerClus = CLUSTERS_360K; /* Default for 360K disk	*/

unsigned long	StartTime;					/* Time and date of first disk used	*/
unsigned long	LastTime;					/* Time and date of last disk used	*/


static int			iDskFmtType;			/* Default disk format, set by user	*/


/***************************************************************************/
/* Entry point for RUP backup function. Accepts a source and destination	*/
/* drive character and then proceeds to backup all files on the source to	*/
/* the destination drive, first prompting the user to insert the proper		*/
/* disks and formating any disk which is not formatted. Is responsible for	*/
/* allocating all buffers and initializing all static globals for this 		*/
/* modules but it is assumed that pchCpyBuffer has already been allocated.	*/
/* After intialization a loop gets the next filename on the hard disk and	*/
/* then calls CopyFile() to copy the file. After completion all buffers		*/
/* are freed. NOTE: pchCpyBuffer must never be freed.								*/
/*																									*/
/* void Backup( char chSourceDrv, char chDestDrv )									*/
/*																									*/
/*	ARGUMENTS:	chSource	 - Source drive letter.										*/
/*					chDestDrv -	Destination drive letter.								*/
/*	RETURNS:		void																			*/
/*																									*/
/*	GLOBALS:		iDepth	- Initializes to -1											*/
/*					szPath	- Allocates and initializes to "X:\", deallocates	*/
/*					chSource	- Initializes to source drive character				*/
/*					chDestin	- Initializes to destination drive characater		*/
/*					Info		- Allocates and deallocates memory for this buffer	*/
/*																									*/
/***************************************************************************/

int Backup( char chSourceDrv, char chDestDrv )
{
	register		iStatus;					/* Return status	*/
	register		i;							/* Loop counter	*/
	extern char	FmtDrvLst[];

	if ( !FmtDrvLst[ chSourceDrv - 'A'] )
	{
		NotFormattedPrompt();
		return( OK );
	}
										/* Set source and destination drives */
	chSource = chSourceDrv;
	chDestin = chDestDrv;

	InitGlobals();					/* Allocate buffers and initialize statics */
	lDiskSize = 362496L;			/* Base on 360K disk and then convert later */
	SaveState();					/* Save starting search state	*/
	CalcPrompt();					/* Display please wait prompt */

 										/*	Count the need 360K disks	*/
	for( i = 1, iStatus = OK; iStatus == OK; i++ )
		iStatus = DoBackup( i, COUNT );

	i--;								/* Ajust for starting at disk 1 */
	RestoreState();				/* Restore the find first state to root dir	*/

										/* Make sure first disk contains some files	*/
										/* by checking the length of the ctrl file	*/
	if ( i == 1 && lCtrlOffset <= (long)(DISK_REC_LEN)  )
		NoFilesPrompt();

 										/* See if user has enough disks */
	else if ( (iStatus = DisplayDiskTotals( i )) == OK )
		iStatus = MainBkupLoop( i );				/* Do the backup */

	FreeMemory( InfoState );	/* Clean up */
	FreeMemory( Records );

	return( iStatus == NOT_FOUND ? OK : iStatus );
}

/***************************************************************************/
/* Main loop to backup a hard drive. Prompt user to insert a disk and then	*/
/* backs up to the disk and then updates the gas gage. The backup loop 		*/
/* first builds the control file on the disk and then builds the backup		*/
/* file. On entry to the function the directory search state must be			*/
/* setup for starting at the directory depth where the backup is to start.	*/
/*																									*/
/*	int MainBkupLoop( int iTotalDisks )													*/
/*																									*/
/*	ARGUMENTS:	iTotalDisks	- Total number of disks to fill gas gage.			*/
/*																									*/
/*	RETURNS:		int			- NOT_FOUND if complete else error code			*/
/*																									*/
/***************************************************************************/

static int MainBkupLoop( int iTotalDisks )
{
	register	i;							/* Loop counter					*/
	register	iStatus;					/* Return value					*/
	extern long		lTotalBytes;

	lTotalBytes	= iTotalDisks * 362496L;
	DisplayGage();
	UpdateByteCount( -1L );
											/* Loop until all disk have been */
											/* done or an error is detected	*/
	for( i = 1, iStatus = OK; iStatus == OK; i++ )
	{
											/* Loop to allow re-doing a disk	*/
		do
		{
			if ( iStatus == REDO_DISK )
				RestoreState();		/* Restore state to re-do the disk	*/
			else
				SaveState();			/* Else save the current state		*/

											/* Initialize the next disk			*/
			if ( (iStatus = InitBkupDisk( i )) == OK )
			{
				HelpLine( PLEASE_WAIT );				/* Update help line	*/
				iStatus = DoBackup( i, CONTROL );	/* Create ctrl file	*/
				if ( iStatus == OK || iStatus == NOT_FOUND )
				{
					RestoreState();	/* Reset state and create backup file */
					iStatus = DoBackup( i, BACKUP );
				}
			}

			DisplayFileStatus( "", CLEAR );
		}
		while( iStatus == REDO_DISK );
	}

	return( iStatus );
}

/***************************************************************************/
/* Saves the current state of the directory search by coping the current	*/
/*	FCB structures	and current indices to the structures. Must also save		*/
/* the name of the last file that was found to allow continuing with a		*/
/* file that 																					*/
/* is spread across backup disks.														*/
/*																									*/
/*	void SaveState( void )																	*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/* RETURNS:		void																			*/
/*																									*/
/***************************************************************************/
static void SaveState( void )
{
	OldDepth = iDepth;
	OldAttrib = iAttrib;
	lOldFileOffset = lFileOffset;
	memcpy( InfoState + 1, InfoState, STATE_LEN );
	memcpy( pPrevFile, pFile, FILE_REC_LEN );
}

/***************************************************************************/
/* Restores the current state of the directory search by coping the current*/
/*	FCB structures	and current indices to the structures. Must also restore	*/
/* the name of	the last file that was found to allow continuing with a		*/
/*	file that is spread across backup disks.											*/
/*																									*/
/*	void RestoreState( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/* RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

static void RestoreState( void )
{
	iDepth = OldDepth;
	iAttrib = OldAttrib;
	lFileOffset = lOldFileOffset;
	memcpy( InfoState, InfoState + 1, STATE_LEN );
	memcpy( pFile, pPrevFile, FILE_REC_LEN );
}

/***************************************************************************/
/* Finds the next file on the source disk. Uses the DOS findfirst-findnext	*/
/* functions to find the next file in the current directory. If the next	*/
/* file name is a directory entry the function will appened this name to	*/
/* search path in "szPath" and then does an call to findfirst to get the	*/
/* next file name from this new directory level. If a call to FindNext()	*/
/* returns an error it signals there are no more files in the current		*/
/* directory so the last level directory name is striped from szPath and	*/
/* the search continues on the previous level. iDepth will always index		*/
/* the current directory level in the Info structures. If iDepth == -1 on	*/
/* the function will start the search in the root directory.					*/
/*																									*/
/*	static int GetNextFile( void )														*/
/*																									*/
/*	ARGUMENTS:	void																			*/
/*	GLOBALS:		iDepth	- Must contain current directory level	or -1			*/
/*					szPath	- Must contain current directory search string		*/
/*					iAttrib	- If not first call to the function it must be 		*/
/*								  be initialize to _A_FILE or _A_ALLDIR				*/
/*																									*/
/* RETURNS:		int		- OK if a filename was found. Upon return the		*/
/*								  file name will be in Info[iDepth].name and the	*/
/*								  directory will be the base part of szPath.			*/
/*								  ELSE !OK														*/
/*	GLOBALS:		iDepth	- Updates value to current directory level			*/
/*					szPath	- Updates string to current directory path			*/
/*					Info		- Updates Info[iDepth] with file info					*/
/*					iAttrib	- Is updated based on the current search 				*/
/*																									*/
/***************************************************************************/

static int GetNextFile( void )
{
	register		iStatus;		/* Return status */

					/* See if first call */
	if ( iDepth != -1 )
		iStatus = FindNext();
	else
	{
		iDepth = 0;
		iAttrib = _A_FILE;
		iStatus = FindFirst();
	}
						
				/* Check for need to move to another directory level */
	while( (iStatus == OK && iAttrib == _A_ALLDIR) ||
			 (iStatus != OK && iDepth > 0 ) )
	{
		if ( iStatus == OK )
		{ 									/* Move down a level */
			IncPath();
			iStatus = FindFirst();
		}
		else
		{ 									/* Move back up a level */
			DecPath();
			iStatus = FindNext();
		}
	}

	return( iStatus == OK ? iStatus : NOT_FOUND );
}

/***************************************************************************/
/* Finds the first valid file name or directory name in a directory. Will	*/
/* skip over the current or parent directory entry. 								*/
/*																									*/
/*	static int FindFirst( void )															*/
/*																									*/
/*	ARGUMENTS:	void																			*/
/*	GLOBALS:		iDepth	- Must contain current directory level	or -1			*/
/*					szPath	- Must contain current directory search string		*/
/*																									*/
/* RETURNS:		int		- OK if a filename was found. Upon return the		*/
/*								  file name will be in Info[iDepth].name ELSE !OK	*/
/*	GLOBALS:		Info		- Updates Info[iDepth] with file info					*/
/*					iAttrib	- Is updated based on the current search 				*/
/* 																								*/
/***************************************************************************/

static int FindFirst( void )
{
	int		iStatus;

					/* Get the first file name in this directory if		*/
					/* search for file fails then try for subdirectory	*/
	iStatus = _dos_findfirst( szPath, iAttrib, &Info[iDepth] );
	if ( iStatus != OK && iAttrib == _A_FILE )
	{
		iAttrib = _A_ALLDIR;
		iStatus = _dos_findfirst( szPath, iAttrib, &Info[iDepth] );
	}

					/* If not a valid file name then get the first valid one */
	if ( iStatus == OK && !IsValidName() )
		iStatus = FindNext();

	return( iStatus );
}

/***************************************************************************/
/* Finds the next valid file name or directory name in a directory. Will	*/
/* skip over the current or parent directory entry. 								*/
/*																									*/
/*	static int FindNext( void )															*/
/*																									*/
/*	ARGUMENTS:	void																			*/
/*	GLOBALS:		iDepth	- Must contain current directory level					*/
/*					szPath	- Must contain current directory search string		*/
/*					iAttrib	- Must be initialize to _A_FILE or _A_ALLDIR			*/
/*																									*/
/* RETURNS:		int		- OK if a filename was found. Upon return the		*/
/*								  file name will be in Info[iDepth].name ELSE !OK	*/
/*	GLOBALS:		Info		- Updates Info[iDepth] with file info					*/
/*					iAttrib	- Is updated based on the current search 				*/
/* 																								*/
/***************************************************************************/

static int FindNext( void )
{
	int		iStatus;

			/* Loop until a valid file name or no more files	*/
			/* If find next fails for file call FindFirst to	*/
			/* see if any directories are in the path.			*/
	do
	{
		if ( (iStatus = _dos_findnext( &Info[iDepth] )) != OK &&
				iAttrib == _A_FILE )
		{
			iAttrib = _A_ALLDIR;
			iStatus = FindFirst();
		}
	}
	while ( iStatus == OK && !IsValidName() );

	return( iStatus );
}

/***************************************************************************/
/* Check the name in Info[iDept] and returns TRUE if the name is not a the	*/
/* current or parent directory entry and not a volume label and iAttrib	is	*/
/* set to _A_SUBDIR. If iAttrib == _A_FILE then only the file's attribute	*/
/* bit is checked to be sure it's not a subdirectory.								*/
/* 																								*/
/*	int IsValidName( void )																	*/
/*																									*/
/*																									*/
/*	ARGUMENTS:	void																			*/
/*	GLOBALS:		iDepth	- Must contain current directory level					*/
/*					Info		- Must be initialized at iDepth level					*/
/*					iAttrib	- Must be initialize to _A_FILE or _A_ALLDIR			*/
/*																									*/
/* RETURNS:		int		- TRUE if a valid file name in Info[iDepth]			*/
/*								  else FALSE													*/
/* 																								*/
/***************************************************************************/

static int IsValidName( void )
{
	int		iStatus;

	iStatus = FALSE;

	if ( iAttrib == _A_ALLDIR )
	{
		if ( strcmp( Info[iDepth].name, "." ) != 0 )
			if ( strcmp( Info[iDepth].name, ".." ) != 0 )
				if ( (Info[iDepth].attrib & _A_SUBDIR ) != 0 )
						iStatus = TRUE;
	}
	else if ( (Info[iDepth].attrib & (_A_SUBDIR | _A_VOLID)) == 0 )
		iStatus = TRUE;

	return( iStatus );
}

/***************************************************************************/
/* Creats a new search path and increments the search level by appends the	*/
/* name in Info[iDepth].name to the current search path and then				*/
/* incrementing iDepth.																		*/
/*																									*/
/* static void IncPath( void )															*/
/*																									*/
/* ARGUMENTS:	void																			*/
/*	GLOBALS:		iDepth	- Must contain current directory level					*/
/*					szPath	- Must contain current directory search string		*/
/*																									*/
/* RETURNS:		void																			*/
/*	GLOBALS:		szPath	- Updates string to next directory level path		*/
/*					iDepth	- Increments to next directory level					*/
/*					iNewPath - Set this flag to TRUE										*/
/*																									*/
/***************************************************************************/
static void IncPath( void )
{

	*(ParseFileName( szPath )) = EOL;
	strcat( szPath, Info[iDepth].name );
	strcat( szPath, "\\*.*" );
	iDepth++;
	iAttrib = _A_FILE;
	iNewPath = TRUE;
}


/***************************************************************************/
/* Creats a new search path and decrements the search level by parsing out	*/
/* the last directory name in the path part of the string and then			*/
/* decrementing iDepth.																		*/
/*																									*/
/* static void IncPath( void )															*/
/*																									*/
/* ARGUMENTS:	void																			*/
/*	GLOBALS:		iDepth	- Must contain current directory level					*/
/*					szPath	- Must contain current directory search string		*/
/*																									*/
/* RETURNS:		void																			*/
/*	GLOBALS:		szPath	- Updates string to previous directory level path	*/
/*					iDepth	- Decrements to previous directory level				*/
/*					iNewPath - Set this flag to TRUE										*/
/*																									*/
/***************************************************************************/

static void DecPath( void )
{
	*(ParseFileName( szPath ) - 1) = EOL;
	*(ParseFileName( szPath )) = EOL;
 	strcat( szPath, "*.*" );
	iDepth--;
	iAttrib = _A_ALLDIR;
	iNewPath = TRUE;
}

/***************************************************************************/
/* Initializes all global variables to set the search state to the root		*/
/* directory of the drive specified in chSource. Allocates all FCB buffers,*/
/* path string buffers and ctrl file field buffers and then builds the 		*/
/* starting path string to "X:*.*"														*/
/*																									*/
/*	void InitGlobals( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/* RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

static void InitGlobals( void )
{
			/* Allocate memory for assign all static global pointers */

	InfoState = GetMemory( STATE_LEN * 2 );
	Records   = GetMemory( RECS_LEN );

	szPath	= &(InfoState[0].Path[0]);
	Info		= &(InfoState[0].Info[0]); 

	pHeader	= &(Records->Header);
	pDir		= &(Records->Dir);
	pFile		= &(Records->File);
	pPrevFile= &(Records->pPrevFile);

							/* Signal first call to GetNexFile() */
	iDepth = -1;
							/* Initialize the path buffer */
	BuildPath( szPath, chSource, "\\", "*.*" );

							/* Initialize the control file record */
	pHeader->Len = sizeof ( struct Disk_Header );
   strcpy( pHeader->Id, "BACKUP  " );
   pHeader->Seq = 0;
   memset( pHeader->CmdLine, 0, sizeof( pHeader->CmdLine ) );
   pHeader->IsLast = FALSE;

							/* Initialize directory block record */
	pDir->Len = sizeof( struct Dir_Block );
	pDir->Entries = 0;
	pDir->NextDir = -1L;

							/* Initialize file block record */
	pFile->HeadLen = sizeof( struct File_Header );
	pFile->FileLen = -1L;		/* Mark the record as never been used */
	pFile->PartSize = 0L;

	lFileOffset = 0L;				/* Set offset into current file */
}


/***************************************************************************/
/* 																								*/
/***************************************************************************/

static int DoBackup( int DiskNum, int iMode )
{
	register			iStatus;
	register			iCloseStatus;
	extern int		iHandle;

	BufInit();				/* Get ready to write new file */
	
			/* Set file handle for BACKUP.??? or CONTROL.??? depending */
			/* whether we're creating the control file or coping files */
	iHandle = (iMode == BACKUP ? iBkupHandle : iCtrlHandle);

	iBkupMode = iMode;	/* Set global to show if ctrl file or file copy		*/
	iNewPath	= TRUE;		/* Need to force creation of the first DIR record	*/
	iDiskFull = FALSE;	/* Disk isn't full, it should be blank					*/
	pDir->Entries = -1;	/* Tell UpdateDirRec() that 1st rec not created yet*/
	lCtrlOffset = 0L;		/* Reset Ctrl file offset indice */
	lBkupOffset	= 0L;		/* Reset Backup file offset indice */

					/* Write the backup control file header */
	iStatus = AddCtrlHeader( DiskNum, FALSE );

 					/* Loop while no errors and disk has free space */
	while( iStatus == OK  && !iDiskFull )
		iStatus = ProcessNextFile();

					/* Update last dir header and disk header */
	if ( iStatus == OK || iStatus == NOT_FOUND )
		if ( (iCloseStatus = CloseOutDisk( DiskNum, iStatus )) != OK )
			iStatus = iCloseStatus;

					/* Make sure both files are closed on error */
	if ( iStatus != OK && iStatus != NOT_FOUND )
	{
		iStatus = CloseFile( &iCtrlHandle, iStatus );
		iStatus = CloseFile( &iBkupHandle, iStatus );
	}

	return( iStatus );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

static int ProcessNextFile( void )
{
	int	iStatus;
	
	if ( (iStatus = BuildFileRec()) == OK )
	{
					/* If szPath has changed then update old DIR rec	*/
					/* and then create a new DIR rec							*/

		if ( iNewPath && (iStatus = UpdateDirRec( FALSE )) == OK )
			iStatus = AddDirRec();

	 					/* Add a new file rec and inc dir entry count */

		if ( iStatus == OK && (iStatus = AddFileRec()) == OK )
			pDir->Entries++;	/* Update files in this dir */
	}

	return( iStatus );
}

/***************************************************************************/
/* Finished with a disk so if no errors we have to update the last DIR		*/
/* record and if no more files to  be copied need to mark the disk header	*/
/* If argument iLastDisk == TRUE the disk header will be flagged as the		*/
/* last disk.																					*/
/***************************************************************************/

static int CloseOutDisk( int DiskNum, int iLastDisk )
{
	int				iStatus;
	extern int		iHandle;

	iStatus = OK;

	if ( iBkupMode == CONTROL )
	{
		iStatus = UpdateDirRec( TRUE );
		if ( iLastDisk )
			iStatus = AddCtrlHeader( DiskNum, iLastDisk );
	}

	if ( iBkupMode != COUNT )
	{
		if ( iStatus == OK )
			iStatus = BufFlush(); 				/* Flush disk buffer */

		if ( iBkupMode == CONTROL )
			iStatus = CloseFile( &iCtrlHandle, iStatus );
		else
			iStatus = CloseFile( &iBkupHandle, iStatus );
	}

	return( iStatus );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

static int AddDirRec( void )
{
	char	*szTmp;
	int	iStatus;
	int	iStrLen;

	iNewPath = FALSE;					/* Clear the new directory flag */
	iStatus = OK;
	
	if ( iBkupMode != COUNT )
	{
							/* Copy path name less the drive and first backslash	*/
		szTmp = GetMemory( MAX_PATH_LEN );
		strcpy( szTmp, szPath + 3 );

										/* Remove file name and trailing backslash	*/
		*(ParseFileName( szTmp )) = EOL;
		iStrLen = RemoveTrailing( szTmp, '\\' );

														/* Padd end of name with zeros	*/
		memcpy( pDir->Path, szTmp, iStrLen );
		memset( pDir->Path + iStrLen, EOL, sizeof( pDir->Path ) - iStrLen );

		pDir->Entries = 0;
		pDir->NextDir = lCtrlOffset;

		if ( iBkupMode == CONTROL )
			iStatus = BufAppend( pDir, DIR_REC_LEN );

		FreeMemory( szTmp );
	}

	lCtrlOffset += (LONG)(DIR_REC_LEN);
	return( iStatus );
}

/***************************************************************************/

static int UpdateDirRec( int iIsLast )
{
	int	iStatus;
	long	lOffset;

 			/* See if doing file copy or if DIR record not created yet */

	if ( iBkupMode == CONTROL  && pDir->Entries != -1 )
	{
		lOffset = pDir->NextDir;			/* Find this record's offset */

	 			/* Set it to the new offset or -1 if last dir record on disk */
		pDir->NextDir = iIsLast ? -1L : lCtrlOffset;
		iStatus = BufSeekWrite( pDir, DIR_REC_LEN, lOffset	);
	}
	else
		iStatus = OK;

	return( iStatus );
}

/***************************************************************************/

static int AddCtrlHeader( int DiskNum, int IsLastDisk )
{
	int 	iStatus;

	if ( iBkupMode == CONTROL )
	{
		pHeader->Seq = (UCHAR)(DiskNum);

		if ( IsLastDisk == FALSE )		/* Not marking last backup disk */
			iStatus = BufAppend( pHeader, DISK_REC_LEN );
		else
		{ 									/* Marks last backup disk */
			pHeader->IsLast = 0xff;
			iStatus =  BufSeekWrite( pHeader, DISK_REC_LEN, 0L );
		}
	}
	else
		iStatus = OK;

	if ( !IsLastDisk )
		lCtrlOffset	+=  (LONG)(DISK_REC_LEN);

	return( iStatus );
}

/***************************************************************************/

static int AddFileRec( void )
{
	char		*szFullPath;
	char		*szDir;
	int		iStatus;

	if ( iBkupMode == CONTROL )
		iStatus = BufAppend( pFile, FILE_REC_LEN );
	else if ( iBkupMode == BACKUP )
	{
		szFullPath = GetMemory( 200 );
		szDir = szFullPath + 100;
		strncpy( szDir, pDir->Path + (pDir->Path[0] == '\\' ? 1 : 0), 63 );
		*(szDir + 63) = EOL;

		BuildPath( szFullPath, chSource, szDir, Info[iDepth].name );

		ShowFileStatus( Info[iDepth].name, READ );

		iStatus = BufCopyFile( szFullPath, lFileOffset, pFile->PartSize );
		FreeMemory( szFullPath );
	}
	else
		iStatus = OK;

	lCtrlOffset +=  (LONG)(FILE_REC_LEN);
	lBkupOffset += pFile->PartSize;

	return( iStatus );
}

/***************************************************************************/
/* NOTE: if pFile->FileLen == -1 it means this is the first file to be 		*/
/*			processed so force the action of getting the next file.				*/
/***************************************************************************/

static int BuildFileRec( void )
{
	int		iStatus = OK;
	long		lDiskFree;
	long		lToWrite;

	lDiskFree =  CalcDiskFree();

	if ( lDiskFree > 0 )
	{
						/* Find out how much of last file has been written */
		lFileOffset += pFile->PartSize;

						/* See if need to process a file spanning disks */
		if ( pFile->FileLen != -1L && lFileOffset < pFile->FileLen )
		{	
			pFile->Seq++;
			pFile->Offset = 0L;
			lToWrite = pFile->FileLen - lFileOffset;
			iStatus = OK;
		}
		else if ( (iStatus = GetNextFile()) != NOT_FOUND )
		{
			GetFileRec();
			lToWrite = pFile->FileLen;
			lFileOffset = 0L;			/* Reset file offset pointer */
		}
						/* See if remaining block of file can be written 	*/
						/* and if not write remaining disk size and set 	*/
						/* disk full flag												*/
		if ( lToWrite > lDiskFree )
		{
			pFile->PartSize = lDiskFree;
			pFile->Flags = 2;					/* Not last part of file */
			iDiskFull = TRUE;
		}
		else
		{
			pFile->PartSize = lToWrite;
			pFile->Flags = 3;					/* Is last part of file */
		}
						/* Increment offset in backup file */
	}
	else
		iDiskFull = TRUE;

	return( iStatus );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

static void GetFileRec( void )
{
	int		iStrLen;

				/* Get the file name and padd the end of the name with 0's */
	memcpy( pFile->Name, Info[iDepth].name, sizeof( pFile->Name )  );
	iStrLen = strlen( pFile->Name );
	while( iStrLen < sizeof( pFile->Name )	)
		pFile->Name[ iStrLen++ ] = 0;

	pFile->Flags = 3;
	pFile->FileLen	= Info[iDepth].size;
	pFile->Seq = 1;
	pFile->Offset = lBkupOffset;
	pFile->PartSize = Info[iDepth].size;
	pFile->Attrib = Info[iDepth].attrib;
	*((unsigned *)(&pFile->Time))	= Info[iDepth]. wr_time;
	*((unsigned *)(&pFile->Date)) = Info[iDepth].wr_date;
}

/***************************************************************************/
/* Calculates the number of bytes free on the destination disk based on		*/
/* how much has already been written to it and it's original free space.	*/
/* The returned size takes into account the fact that the disk is				*/
/* allocated in clusters and will round to the end of the current cluster.	*/
/*																									*/
/*	long CalcDiskFree( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/* RETURNS:		long		- Number of bytes which can still be allocated to	*/
/*								  any file on the disk.										*/
/*																									*/
/*	GLOBALS:		iBytesPerCluster	-	Must have already been set by a			*/
/*												previous a call to GetDiskFree			*/
/*																									*/
/***************************************************************************/

long CalcDiskFree( void )
{
	long		lDiskUsed;


		/* Disk space used = lBkupOffset + (lCtrloffset padded to 512 bound) */
		/* lCtrloffset must also include 1 file rec and 1 possible DIR rec	*/

	lDiskUsed = (lCtrlOffset + FILE_REC_LEN + DIR_REC_LEN);
	lDiskUsed +=  (iBytesPerClus - (lDiskUsed % iBytesPerClus));
	lDiskUsed += lBkupOffset;

	return( lDiskSize - lDiskUsed );
}

/***************************************************************************/
/*	Closes the passed file handle if it is opened. The passed file handle	*/
/* will be -1 if it is not open. The argument iStatus is used to signal		*/
/* that the file is being closed because of a previous disk error so we		*/
/* don't want to inform the user if the close fails. This will keep from	*/
/* getting multiple error messages when there is error closing a file		*/
/* on a disk which is already known to be un-useable. The return value		*/
/* will be either the orignal value of the iStatus argument or an error		*/
/* code from error handler if there was an error closing the file and 		*/
/* and iStatus was OK on entry.															*/
/*																									*/
/*	int CloseFile( int *iFileHandle, int iStatus )									*/
/*																									*/
/*	ARGUMENTS:	iFileHandle	- File handle to be closed.							*/
/*					iStatus		- Current error status of the file being closed	*/
/*	RETURNS:		int			- See above description									*/
/*																									*/
/***************************************************************************/

int CloseFile( int *iFileHandle, int iStatus )
{

	if ( *iFileHandle != -1 )
		if (  _dos_close( *iFileHandle ) != OK && iStatus == OK )
			iStatus = ProcDiskErr( BAD_MEDIA );

	*iFileHandle = -1;
	return( iStatus );
}

/***************************************************************************/
/*	Prepare a floppy disk to be used as a backup disk. First checks to see	*/
/* if the disk is formatted and if not it formats it. If the disk is			*/
/*	already formatted the root directory and FAT are wiped out (bad cluster	*/
/*	are retained in the new FAT) and then a label is put on the disk in the	*/
/* form of "BACKUP  XXX" where XXX is the sequence number. The Ctrl and 	*/
/* BACKUP files are then created and left open and ready to write to. If	*/
/* there is an error the disk will be considered un-useable and the user	*/
/* will be prompt to insert another disk.												*/
/*																									*/
/*	int InitBkupDisk( int Num )															*/
/*																									*/
/*	ARGUMENTS:	Num	-	The disk sequence number									*/
/*	RETURNS:		int	-	OK or ABORT if user selected to stop the backup		*/
/*																									*/
/*	GLOBALS:		szDiskLabel	- Sequence number is appended							*/
/*					iCtrlHandle	- Set to open control file handle					*/
/*					iBkupHandle	- Set to open backup file handle						*/
/*																									*/
/***************************************************************************/

static int InitBkupDisk( int Num )
{
	register				iStatus;			/* Return status		*/
	char					*szPtr;			/* Ptr to disk label */

												/* Create the disk label string */
	szPtr = szDiskLabel + 8;
	if ( Num < 100 )
	  *(szPtr++) = '0';
	if ( Num < 10 )	
	  *(szPtr++) = '0';
	itoa( Num, szPtr, 10 );

												/* Loop until good disk or user aborts */
	do
	{		
		if ( (iStatus = PrepBkupDsk( Num )) != OK ||
			  (iStatus = OpenDest( &iCtrlHandle, Num, szCtrlFile, 0 )) != OK ||
			  (iStatus = OpenDest( &iBkupHandle, Num, szBkupFile, 0 )) != OK )
		{
			CloseFile ( &iCtrlHandle, iStatus );	/* Error so close files */
			CloseFile ( &iBkupHandle, iStatus );
			iStatus = ProcDiskErr( iStatus );	/* Prompt the user		*/
		}
	}
	while( iStatus != ABORT && iStatus != OK );

	return( iStatus );
}

/***************************************************************************/
/* Appends an numeric extension to a file name and then creates the			*/
/* specified file on the destination disk. The attributes for the file can	*/
/* be specified in the arguments. Returns OK if successfull else a DOS		*/
/* error code.																					*/
/*																									*/
/*	int OpenDest( int *iFile, int DiskNum, char *szFileName, int iAttrib )	*/
/*																									*/
/*	ARGUMENTS:	iFile		- Ptr to file handle to be initialized					*/
/*					DiskNum	- Backup disk seq. number									*/
/*					szFileName - Path and base part of file name						*/
/*																									*/
/* GLOBALS:		chDestin	- Should be initialized to the destin drive letter	*/
/*																									*/
/***************************************************************************/

static int OpenDest( int *iFile, int DiskNum, char *szFileName, int iAttrib )
{
	char		szExt[10]; 					/* File name extension */
	int		iStatus;

	itoa( DiskNum, szExt, 10 ); 		/* Create the ascii file extension */
	while( strlen( szExt ) < 3 )
		InsertChar( szExt, '0' ); 		/* Prefix with ascii zeros */

	*szFileName = chDestin;
												/* Append the file extensions */
	strcpy( strchr( szFileName, '.' ) + 1, szExt );

	if	( _dos_creat( szFileName,  iAttrib, iFile ) == OK )
		iStatus = OK;
	else
	{
		*iFile = -1;
		iStatus = BAD_MEDIA;
	}

	return( iStatus );
}

/***************************************************************************/
/* Initializes the copy buffer to max available memory less 20K bytes. If	*/
/* there is less than 5K of memory available for use the program is			*/
/* with a call to FatalError(). The static global pchCpyBuffer is set to	*/
/* point to the successfully allocated buffer.										*/
/*																									*/
/*	void InitCpyBuffer( void )																*/
/*																									*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS:		void																			*/
/*																									*/
/* GLOBALS:		lBufSize	- Set to size of allocated buffer						*/
/*					pchCpyBuffer - Pointed to allocated buffer						*/
/*																									*/
/***************************************************************************/

int InitCpyBuffer( void )
{
	register	iStatus;

	iStatus = ERROR;		/* Assume may not have enough memory */

						/* Find out how much memory we can get */
	lBufSize = GetMaxHugeSize() - MALLOC_MEMORY;

	if ( lBufSize >= MIN_COPY_BUFFER )
		if ( (pchCpyBuffer = halloc( lBufSize, 1)) != NULL )
			iStatus = OK;

	return( iStatus );
}

/***************************************************************************/
/* Returns the size of the max block of huge memory that is available. The	*/
/* amount available is determined doing a DOS call to allocate the largest	*/
/* block avaliable and then freeing the allocated block and returning the	*/
/* the size of the block that was allocated.											*/
/*																									*/
/*	long GetMaxHugeSize( void )															*/
/*																									*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS:		long		-	Number of bytes of huge memory avaiable			*/
/*																									*/
/***************************************************************************/

long GetMaxHugeSize( void )
{
	unsigned		uNumParagraphs;

					/* Request more than is possible to get the max size  */
	_dos_allocmem( 0xffff, &uNumParagraphs );

					/* Return paragraphs converted to bytes */
	return( ((unsigned long) uNumParagraphs) << 4);
}


