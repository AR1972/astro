/***************************************************************************/
/* 																								*/
/* SETUP.C																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Creates the recovery disk when doing a hard disk upgrade.					*/
/* New functions to allow creating a recovery floppy when memory size is	*/
/* less than size of the FAT by breaking up the reads and writes into		*/
/* small enough pieces. 																	*/
/* 																								*/
/* 10-25-80 johnhe																			*/
/* 																								*/
/* New functions to replace existing CreateRecoveryFloppy() code. This new */
/* implementation will use the new multiple file copy function xcopy() to	*/
/* actually copy the files to the recovery disk rather that relying on a	*/
/* seperate function and buffer. 														*/
/* 																								*/
/*	11-05-89 johnhe																			*/
/***************************************************************************/

#include 	<stdio.h>
#include 	<stdlib.h>
#include 	<dos.h>
#include 	<string.h>
#include		<malloc.h>

#include		<alias.h>
#include 	<copy.h>
#include		<global.h>
#include 	<data.h>
#include 	<disk_io.h>
#include		<hdisk.h>
#include 	<strlib.h>
#include 	<upgrade.h>
#include		<errors.h>
#include		<prompts.h>
#include		"wsrc.h"

/***************************************************************************/

#define		MAX_SECTOR_SIZE		4096		/* Max allowable sector size */
#define		EXT_BPB_LEN				51

#define		SYS_FILES				(_A_RDONLY | _A_HIDDEN | _A_SYSTEM )
#define		RECOVERY 				0
#define		BUF_LEN					(sizeof( struct MULT_FILES ) * MAX_PER_DISK)

#define		_A_FINDALL				(_A_HIDDEN | _A_SYSTEM | _A_RDONLY)

/***************************************************************************/
/* Global variables needed by functions in this module							*/
/* 11-05-89 johnhe																			*/
/***************************************************************************/

extern struct BPB				HdBpb;
extern char		szTmpDos[ MAX_PATH_LEN ];
/*
 *  Declared in towin.c, always contains the currect uninstall disk
 *  number (zero based)
 */
extern int iUninstallDiskNum;

extern char		*szCommand;
extern char 	*szAutoBat;
extern char 	*szConfSys;
extern char		*szWinA20;

char				*szSysFile = "X:\\*.*";

char				*szUninstall = "uninstal";	/* Contents of AUTOEXEC.BAT */


struct FILES_REC
{
	int		Valid;						/* TRUE if file exists		*/
	char		SourceName[13];			/* Source file name			*/
	char		DestinName[13];			/* Destination file name	*/

};

enum	{ IO_OLD, DOS_OLD };
enum  { CMD_OLD, AUTO_OLD, CFG_OLD, WINA20 };
#define END_LIST  999

struct FILES_REC FileRecAnyDisk[] =  { { FALSE,    "COMMAND.COM",  "COMMAND.DAT",	},
					                        { FALSE,    "AUTOEXEC.BAT", "AUTOEXEC.DAT",  },
					                        { FALSE,    "CONFIG.SYS",   "CONFIG.DAT",	   },
					                        { FALSE,    "WINA20.386",   "WINA20.386"     },
                                       { END_LIST, "",             "",              }};

struct FILES_REC FileRecDisk0[] =    { { FALSE,    "",				 "BIOS.OLD",		},
					                        { FALSE,    "",				 "DOS.OLD",		   },
                                       { END_LIST, "",             "",              }};

					                        /******************************************/
					                        /* 		     				  				*/
					                        /*	Valid,     SourceName,	  DestinName	*/
					                        /******************************************/

/***************************************************************************/

/* This guy lives in resolve.c */
extern void	InitUninstallDisk	( int iUninstallDiskNum );

extern void fmemcpy				( char far *Str1, char far *Str2, UINT Len );
extern void	UpdateFileBar		( int iFiles );

extern void	SaveBootRecs		( void );
extern void DispInsertUserDisk(int UserDisk,char chDrive);
extern void	InitFileStruc		( struct MULT_FILES *FileStruc, int iDisk,
			  						  	  int IsHdUpgrade );
extern void CopyFiles( int id, struct MULT_FILES *Files );
extern void XHcopy( struct MULT_FILES *Files ,char * szTmpDos);

void GetOldSysNames( void );

static void near 	CopyRecFiles( void );
static int near	NeedToSaveFile		( char *szPath );
static void near	InitRecoveryFiles	( struct MULT_FILES *Files );
static void near  CopyRecStruct( struct MULT_FILES *File, struct FILES_REC *FileRec );
static void near	CopyFATRoot			(void );
static void near	SaveRootEntries	( void );
static int near	SectorsToFile		( int iFile, int iDrive, long lStartSec,
												  UINT uNumSectors, UINT uBytesPerSector);

/***************************************************************************/
/* Sets up the structure needed by xcopy() and then calls it copy all of	*/
/* system files from the install disk and all of the old system files from */
/* the hard disk onto the recovery floppy. Next it calls functions to copy */
/* the FAT and root directory, master and partition boot records and then	*/
/* the global data structure "vInfo" to the recovery disk.						*/
/* 																								*/
/* void CreateRecoveryFloppy( void )													*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/* 11-05-89 johnhe																			*/
/***************************************************************************/

void CreateRecoveryFloppy( void )
{
	struct MULT_FILES 	*Files;
   struct MULT_FILES    *FilesHeadPtr;
	extern int				CreatingRecovery; /* Signal can't continue on error */

	Files = GetMemory( BUF_LEN ); /* Allocate memory for file list. */
   FilesHeadPtr = Files;         /* Keep a pointer to the head of the list. */

	vInfo.uchFloppyType = 1;      /* Default floppy type = 360kb */
   /*
    *  Init the uninstall disk number. This is a global that is used to track
    *  the uninstall disk number. It's declared in towin.c, it's incremented in
    *  resolve.c when an uninstall disk becomes full and it becomes necessary
    *  to prompt for a second uninstall disk. MC Janus work 4/9/92 
    */
   iUninstallDiskNum = 0;   /* Init uninstall disk number (zero based). */

	GetHdBpb( &HdBpb );						/* Get hd's bpb from boot sect		*/

	CreatingRecovery = 1;			/* Signal need to abort if error detected */

	CopyRecFiles();

	/* This call inits both the FileRecDisk0 and FileRecAnyDisk data structures. */
   GetOldSysNames();

	InitRecoveryFiles( Files );	/* Create MULTI_FILES structure for xcopy	*/

	CopyFiles(IDS_WAITCOPYRECFL, Files );	/* Copy files to RECOVERY DISK 0 */

	MakeHidden( 'A' ); 				/* Set system file attributes					*/

	SaveBootRecs();
	UpdateFileBar( 1 );

	SaveRootEntries();				/* Copy 1st 2 root entries to vInfo			*/
	UpdateFileBar( 1 );
											/* Copy user's option choices             */
	CopyBuf( "GLOBAL.DAT", &vInfo, sizeof( vInfo ) );
	UpdateFileBar( 1 );
											/*  Create AUTOEXEC.BAT */
	CopyBuf( "AUTOEXEC.BAT", (void *)szUninstall, strlen( szUninstall ) );
	UpdateFileBar( 1 );

   /* The remaining uninstall files can be copied to either disk 0 or 1. */

   Files = FilesHeadPtr;                   /* Reset files pointer. */
	CopyRecStruct(Files, FileRecAnyDisk);
	CopyFiles(IDS_WAITCOPYRECFL, Files );	 /* Copy files to ANY recovery disk */

	CopyFATRoot();						/* Copy FAT and Root dir to recovery disk */
	UpdateFileBar( 1 );

	CreatingRecovery = 0;			/* Signal recovery disk is complete 		*/
	FreeMemory( Files );
}

/***************************************************************************/
/* Initializes the MULT_FILES struct to contain all information necessary	*/
/* to create the recovery diskette. First gets info about files from the	*/
/* installation disk and then add info about the user's existing files     */
/* which need to be saved. 																*/
/* 																								*/
/* void InitRecoveryFiles( struct MULT_FILES *Files )								*/
/* 																								*/
/* ARGUMENTS:	Files - Ptr to array of MAX_PER_DISK # of MULT_FILES strucs */
/* RETURNS: 	void																			*/
/* 																								*/
/* 11-19-89 johnhe																			*/
/***************************************************************************/

void near InitRecoveryFiles( struct MULT_FILES *Files )
{
	struct MULT_FILES 	*OldFiles;

	memset( Files, 0, BUF_LEN );
											/* Init info about files on distrib disk	*/
	InitFileStruc( Files, RECOVERY, FALSE );

		/* Find where user file info should start and set distr. file's path	*/
		/* destination drive																	*/

	for ( OldFiles = Files; OldFiles->Path.Source != NULL; OldFiles++	)
	{
		OldFiles->Drive.Source = szTmpDos[ 0 ];
		OldFiles->Path.Source = szTmpDos + 2;
		OldFiles->Drive.Destin = 'A';

		//	We hard code the DiskNumber to NOT_REMOVEBALE as we know that the 
		// source is directory is on the hard disk.

		OldFiles->DiskNumber = NOT_REMOVEABLE;

	}
	/* Create struct needed by file copy func */
	CopyRecStruct( OldFiles, FileRecDisk0);
}

/***************************************************************************/
/* Finds the name of the original bios and dos files by doing a findfirst	*/
/* and assuming that the first 2 files in the root directory are the 		*/
/* system files. Saves these 2 names in the file name structure. Next it	*/
/* checks to see if an autoexec.bat or config.sys exists and updates the	*/
/* structure to reflect that the file exist. The existing system files		*/
/* must be marked as hidden, system and readonly.									*/
/* 																								*/
/* void GetOldSysNames( void ) 															*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/* 11-05-89 johnhe																			*/
/***************************************************************************/

void GetOldSysNames( void )
{

   *szCommand = *szAutoBat = *szConfSys = *szSysFile =
	*szWinA20 = vInfo.chDestin;

	GetSysFiles();

	if ( vInfo.Flag.fIoSys )
	{
		strcpy( FileRecDisk0[ IO_OLD	].SourceName, vInfo.szIoSys );
		FileRecDisk0[ IO_OLD ].Valid = TRUE;
	}
	if ( vInfo.Flag.fMsDos )
	{
		strcpy( FileRecDisk0[ DOS_OLD ].SourceName,	vInfo.szMsDos );
		FileRecDisk0[ DOS_OLD ].Valid = TRUE;
	}
	if ( NeedToSaveFile( szCommand ) )
		vInfo.Flag.fCommandCom = 1, FileRecAnyDisk[ CMD_OLD ].Valid = TRUE;

	if ( NeedToSaveFile( szAutoBat ) )
		vInfo.Flag.fAutoexecBat = 1, FileRecAnyDisk[ AUTO_OLD ].Valid = TRUE;

	if (  NeedToSaveFile( szConfSys ) )
		vInfo.Flag.fConfigSys = 1, FileRecAnyDisk[ CFG_OLD ].Valid = TRUE;

	if (  NeedToSaveFile( szWinA20 ) )
		vInfo.Flag.fWinA20 = 1, FileRecAnyDisk[ WINA20 ].Valid = TRUE;
}

/***************************************************************************/
/* Determines if a file needs to be saved by checking to see if it exists	*/
/* and ensuring that it's not zero length.											*/
/* 																								*/
/* int NeedToSaveFile( char *szPath )													*/
/* 																								*/
/* ARGUMENTS:	szPath	- Ptr to file's full path name							*/
/* RETURNS: 	int		- TRUE if file exists and is not zero length			*/
/*								  else FALSE													*/
/* 																								*/
/***************************************************************************/

int near NeedToSaveFile( char *szPath )
{
	struct find_t		Find;

	return( _dos_findfirst( szPath, _A_FINDALL, &Find ) == OK &&
           Find.size != 0L );
}

/***************************************************************************/
/* Creates information in a MULT_FILES structure to allow coping the files */
/* from the user's hard disk which will be neccessary for doing a recovery.*/
/* 																								*/
/* void CreateRecStruct( struct MULT_FILES *File ) 								*/
/* 																								*/
/* ARGUMENTS:	File		- Ptr to 1st element in array of MULT_FILES strucs */
/* 																								*/
/* 				FileRec  - Ptr to recovery file struct to be added to       */
/* 							  the mult_files (File) data struct.               */
/* 																								*/
/* RETURNS: 	void																			*/
/* 																								*/
/* 11-05-89 johnhe																			*/
/***************************************************************************/

void near CopyRecStruct( struct MULT_FILES *File, struct FILES_REC *FileRec )
{
	int					i = 0;
	extern char			*szRootPath;

	while( FileRec[ i ].Valid != END_LIST )
	{
		if ( FileRec[ i ].Valid != FALSE )
		{
			File->Name.Destin = FileRec[ i ].DestinName;

			File->Path.Source = szRootPath;
			File->Path.Destin = szRootPath;

			File->Drive.Destin = 'A';
			File->UserDisk = RECOVERY;					/* Recovery disk number	*/

			File->Name.Source = FileRec[ i ].SourceName;
			File->Drive.Source = vInfo.chDestin;
			File->DiskNumber = NOT_REMOVEABLE;

			File++;
		}
      i++;
	}
   File->Name.Source = NULL; /* Tie off the list !! */
}

/***************************************************************************/
/* Copies the FAT and root directory to the recovery disk. Allocates a		*/
/* variable	size buffer and then reads in up to 3f sectors at a time and	*/
/* then writes them to the recovery disk.	The root is copied first and		*/
/* then a check has to be made to see if the hard disk has a big FAT and	*/
/* if it does the recovery disk must have > 360K free space or else the 	*/
/* user is prompted for recovery disk #2.												*/
/*																									*/
/*	void CopyFATRoot( void )																*/
/*																									*/
/*	ARGUMENTS:	void																			*/
/*	RETURNS:		void																			*/
/*																									*/
/* 10-25-89 johnhe																			*/
/***************************************************************************/

static void near CopyFATRoot( void )
{
	static char		*szPath[] = { "A:\\ROOT.DAT", "A:\\FAT.DAT" };
	char				chDrive;							/* Physical drive number	*/
	int				i;									/* Loop counter & indice	*/
	int				iFile;							/* DOS file handle			*/
	int				iStatus;
	unsigned			uStartSector;					/* Starting sector to read	*/
	unsigned			uNumSectors;					/* Total sectors to read	*/
	long				lNumBytes;
	struct BPB		Bpb;								/* Hard disk's bpb			*/

	GetHdBpb( &Bpb );									/* Get hd's bpb from boot sect*/
							/* Boot sector was left in the pchBootBuf so copy	*/
							/* to the recovery disk before doing anything else	*/
	chDrive = vInfo.chDestin - (char) 0x41;	/* Calculate the drive number */

	uStartSector = (Bpb.uSecPerFat * Bpb.uchNumberFats) + Bpb.uReservSec;
	uNumSectors = (Bpb.uRootEntries * 32) / Bpb.uBytesPerSec;

						/* Loop once for once for root directory and FAT			*/
	for ( i = 0, iStatus = OK;
			iStatus == OK && i < 2;
			i++ )
	{
		lNumBytes = (long)uNumSectors * (long)Bpb.uBytesPerSec;
		if ( GetDiskFree( 'A' ) < lNumBytes )
			InitUninstallDisk(1);

		DisplayFileStatus( szPath[i] + 3, WRITE );

		if ( _dos_creat( szPath[i], _A_NORMAL, &iFile) == OK )
		{
			if ( (iStatus = SectorsToFile( iFile, chDrive, (long)(uStartSector),
					uNumSectors, Bpb.uBytesPerSec )) == OK )
			{
				if ( _dos_close( iFile ) == OK )
				{
					if ( 0 == i )
					{	
						uStartSector = Bpb.uReservSec;/* Set start sec to 1st fat*/
						uNumSectors = Bpb.uSecPerFat;	/* Set total sec in FAT	*/
						lNumBytes = (long)uNumSectors * (long)Bpb.uBytesPerSec;
						lNumBytes += 1024L;
					}
				}
				else
					iStatus = BAD_RECOVERY_DISK;	/* Couldn't close the file */
			}
		}
		else
			iStatus = BAD_RECOVERY_DISK;			/* Couldn't open the file	*/
	}
	if ( iStatus != OK )								/* Check for error			*/
		FatalError( iStatus );						/* Abort program if error	*/
}

/***************************************************************************/
/* Read the specified sectors from a disk and write them to a an opened		*/
/* file handle. Will first allocate a buffer as large as possible (up to	*/
/* 64K using halloc()). Next it sets up a loop to read in as many sectors	*/
/* as possible in iteration and then write them to the file. The loop will	*/
/* continue until all sectors are done or an error is detected.				*/
/*																									*/
/*	int SectorsToFile( int iFile, int iDrive, long lStartSec,					*/
/*							 unsigned uNumSecs, int uBytesPerSec )						*/
/*																									*/
/*	ARGUMENTS:	iFile			- File handled which was opened for writing to	*/
/*					iDrive		- Drive number to read the sectors from			*/
/*					lStartSec	- Sector to starting reading from					*/
/*					uNumSecs		- Number of of sectors to transfer					*/
/*					uBytesPerSec- Number of bytes out for each sector read in	*/
/*																									*/
/*	RETURNS:		int			- OK if no errors else a FATAL error code			*/
/*																									*/
/*	GLOBALS:		NONE																			*/
/*																									*/
/* 10-25-89 johnhe																			*/
/***************************************************************************/

int near SectorsToFile( int iFile, int iDrive, long lStartSec,
								unsigned uNumSectors, unsigned uBytesPerSector )
{
	int							iStatus;
	unsigned						uBufSize;
	unsigned						uBytesToWrite;
	unsigned						uBytesWritten;
	struct	ABSIO_PACKET	Packet;

							/* Allocate the buffer as large as possible */
	for ( uBufSize = 0xf800; 
			(Packet.pchBuffer = _fmalloc( uBufSize )) == NULL;
			uBufSize -= 0x400 )
	{
		if ( uBufSize <= 0x800 )
			return( FATAL_MEMORY_ERROR );
	}
						/* Determine number of sector which fit in the buffer	*/
	Packet.uNumSectors = uBufSize / uBytesPerSector;

	if ( Packet.uNumSectors > uNumSectors )
		Packet.uNumSectors = uNumSectors;
	uBufSize = Packet.uNumSectors * uBytesPerSector;

													/* Load rest of access packet	*/
	Packet.lStartSector = lStartSec;
	uBytesToWrite = uBufSize;				/* Set maximum write size */
					 
	iStatus = OK;							/* Loop until all sector are done	*/
	while ( iStatus == OK && uNumSectors != 0 )
	{
		if ( AbsReadWrite( iDrive, &Packet, READ ) == OK )
		{
			if ( _dos_write( iFile, Packet.pchBuffer, uBytesToWrite,
										 (unsigned *)&uBytesWritten ) == OK &&
				  uBytesWritten == uBytesToWrite )
			{
						/* Find new starting sector and number left to do	*/
				Packet.lStartSector += (long)(Packet.uNumSectors);
				uNumSectors -= Packet.uNumSectors;
				if ( uNumSectors < Packet.uNumSectors )
				{
					Packet.uNumSectors = uNumSectors;
					uBytesToWrite = uNumSectors * uBytesPerSector;
				}
			}					
			else
				iStatus = BAD_RECOVERY_DISK;
		}
		else
			iStatus = FATAL_HD_READ_ERROR;
	}

	_ffree( Packet.pchBuffer );
	return( iStatus );
}

/***************************************************************************/
/* Gets the boot sector from the hard drive being upgraded.						*/
/*																									*/
/*	void GetHdBpb( struct BPB *Bpb )														*/
/*																									*/
/*	ARGUMENTS:	Bpb		- Ptr to a bpb structure to be filled in.				*/
/*	RETURNS:		void																			*/
/*																									*/
/* 10-25-89 johnhe																			*/
/***************************************************************************/

void GetHdBpb( struct BPB *Bpb )
{
	struct	ABSIO_PACKET	Packet;

	Packet.lStartSector = 0L;
	Packet.uNumSectors = 1L;
	Packet.pchBuffer = pchBootBuf;

	if ( AbsReadWrite( vInfo.chDestin - 'A', &Packet, READ ) == OK )
		fmemcpy( (char far *)Bpb, Packet.pchBuffer + 11, sizeof( struct BPB ) );
	else
		FatalError( FATAL_HD_READ_ERROR );
}

/***************************************************************************/
/* Create a specified file on the recovery disk and copies the contents of */
/* of the specified buffer to the file.												*/
/*																									*/
/* void CopyBuf( char *szFile, void *pchBuf, unsigned uBytes )					*/
/*																									*/
/*	ARGUMENTS:	szFile	- Name of file to be created. 							*/
/* 				pchBuf	- Ptr to buffer to be copied to the file				*/
/* 				uBytes	- Size of the buffer in bytes 							*/
/*	RETURNS:		void		- Will not return if an error is detected.			*/
/*																									*/
/* 11-06-89 johnhe																			*/
/***************************************************************************/

void CopyBuf( char *szFile, void *pBuf, unsigned uBytes )
{
	char			szPath[ 25 ];
	int			iFile;
	unsigned 	uWritten;

	szPath[0] = 'A';
	szPath[1] = ':';
	szPath[2] = '\\';
	strcpy( szPath + 3, szFile );

	DisplayFileStatus( szFile, WRITE );

	if ( _dos_creat( szPath, 0, &iFile ) != OK ||
		  _dos_write( iFile, pBuf, uBytes, &uWritten) != OK ||
		  uBytes != uWritten ||
		  _dos_close( iFile ) != OK )
		ProcessCopyError( szFile, ERR_WRITING );
}

/***************************************************************************/
/* Reads in the first sector of the root directory and then copies the		*/
/* first 2 entries into the vInfo structure.											*/
/*																									*/
/*	void SaveRootEntries( void )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void near SaveRootEntries( void )
{
	int				iDosDrv;							/* Physical drive number	*/
	struct BPB		Bpb;								/* Hard disk's bpb			*/


	GetHdBpb( &Bpb );									/* Get hd's bpb from boot sect*/

	iDosDrv = vInfo.chFirstHd - 'A';
	if ( ReadWriteRoot( iDosDrv, &Bpb, pchBootBuf, 0, READ ) == OK )
		memcpy( vInfo.RootEntry, pchBootBuf, sizeof( struct DIR ) * 2 );
	else
		FatalError( FATAL_HD_READ_ERROR );
}

/***************************************************************************/
/*	Function to copy the DOS files from the distribution disk (or net) to	*/
/* the tmp DOS directory and returns the total number of files copied. The	*/
/* function argument allows the caller to specify if it really want the		*/
/* files to be copied or if just the count is needed.								*/
/*																									*/
/* NOTE:																							*/
/*		Since this function is copying files for the recovery disk it must	*/
/*		make sure that it starts at user disk 0.										*/
/*																									*/
/*	void CopyRecFiles( void )																*/
/*																									*/
/***************************************************************************/

void near CopyRecFiles( void )
{
	#define			BUF_LEN (sizeof( struct MULT_FILES ) * MAX_PER_DISK)

	struct MULT_FILES *TmpFiles;

	extern char		szDosDesName[];		/* Defined in Windows setup copy.c	*/

	TmpFiles = GetMemory( BUF_LEN );

	memset( TmpFiles, 0, BUF_LEN );
	InitFileStruc( TmpFiles, RECOVERY, TRUE );
   XHcopy(TmpFiles, szTmpDos);

	FreeMemory( TmpFiles );

}

