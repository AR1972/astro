/***************************************************************************/
/* 																								*/
/* UPGRADE.C																					*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* This module contains the functions which upgrade a floppy disk or 		*/
/* hard disk system. UpgradeHard() will upgrade a hard disk system and		*/
/* return to the caller when complete. UpgradeFloppy() will upgrade a		*/
/* floppy disk system and return to the caller when complete.					*/
/*																									*/
/* Created 11-13-89 - johnhe																*/
/***************************************************************************/

#include 	<stdio.h>
#include 	<fcntl.h>
#include 	<io.h>
#include 	<stdlib.h>
#include 	<string.h>
#include 	<ctype.h>
#include 	<dos.h>
#include 	<process.h>
#include 	<sys\types.h>
#include 	<sys\stat.h>
#include 	<direct.h>

#include		<alias.h>
#include		<copy.h>
#include 	<data.h>
#include 	<disk_io.h>
#include		<file_io.h>
#include 	<strlib.h>
#include		<global.h>
#include 	<upgrade.h>
#include		<prompts.h>
#include		<errors.h>
#include		<install.h>
#include		"wsrc.h"
#undef NULL
#include		"lib\common\sulib.h"
#include    "log.h"


#define		EXT_BPB_LEN		51

/***************************************************************************/


char				*szBios		= "X:\\io.sys";
char				*szDos		= "X:\\msdos.sys";
char 				*szCommand	= "X:\\command.com";
char				*szVxd		= VXD_NAME;

char				*szAutoBat	= "X:\\AUTOEXEC.BAT";
char				*szConfSys	= "X:\\CONFIG.SYS";
char				*szWinA20	= "X:\\WINA20.386";

#if 0
	static char 	*szOldBios	= "X:\\12345678.123";
	static char 	*szOldDos	= "X:\\12345678.123";
#endif
																	/* Executable extensions */
char 	*szExeExt[] = { "EXE", "COM", "BAT", NULL };

int				iWillReboot = FALSE;


/***************************************************************************/

extern void		FixupRoot			( void );
extern void		RenameOemFiles		( int IsRecovery );

/***************************************************************************/

extern void		NewAutoConfig	( int iType, char *szFilePath );
extern void		InitFileStruc	( struct MULT_FILES *FileStruc, int iDisk,
			  						 	  int IsHdUpgrade );
extern void		UpdatePartDrivers	( void );				// MC 4/13/92 #1

static void near	InstallFiles	( int iDisk, int IsHdUpgrade );
static void near	CreateTmpDir	( void );
static int near	MoveDeleteFile ( char *szSource, char *szDestin, int Type );
static void near	CreateNulFile	( int SeqNum );
static void near	DeleteNulFiles	( void );
static void near	CreateReadMe		( void );

static void near	CopyCommandCom	( void );
static void near	SetTotalBytes	( int iStartDisk, int iTotalDsks );
static int near	InitFileInfo	( char *szName, struct MULT_FILES *File,
											  int iDisk, int IsHdDisk );
static void near	DelTmpFiles		( void );


extern void		UpdateFileBar		( int iFiles );
extern void		ForceExitReboot	( void );
extern void    GetReadMeText		( char *Buf );

extern void CopyFiles( int id, struct MULT_FILES *Files );
extern void ProPrintfStat1( int id );

extern void 	GetOldSysNames		( void );
extern void 	GetHdBpb				( struct BPB *Bpb );
extern struct BPB	HdBpb;

extern char	szTmpDos[];

/***************************************************************************/
/* HARD DISK INSTALLATION																	*/
/* 																								*/
/* Main procedure for upgrading a hard disk. First creates the recovery 	*/
/* disk and then does a fixup on the master partition record, creates a 	*/
/* new tmp boot which prompts for the recovery disk, copies all of the		*/
/* files, updates the lie table in msdos.sys, creates new autoexec.bat and */
/* config.sys files, installs a new master boot record, changes all bpbs	*/
/* on all partitions on all hard disks to remove logical sectoring and		*/
/* then writes the new boot records to all partitions on all hard disks 	*/
/* making sure to do the boot partition last.										*/
/* 																								*/
/* This code is also used by the recovery program to continue the upgrade	*/
/* if there is a failure part of the way through so it is designed to be	*/
/* able to continue from an place based on existence of the number of NULL */
/* files which have already been created in the TMP directory. This is the */
/* reason for the #ifdef RECOVERY_PROGRAM condition compilation statements.*/
/* 																								*/
/* void UpgradeHard( int iNextFile )													*/
/* 																								*/
/* ARGUMENTS:	iNextFile	- Defines the starting point for the upgrade 	*/
/* 								  based on null files created in tmp dir, the	*/
/* 								  caller should use the last null file number	*/
/* 								  + 1 														*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void UpgradeHard( int iNextFile )
{

	register 	iDiskNum;

   LOG("Updating Partition drivers");
   UpdatePartDrivers();				// MC 4/13/92 #1
   LOG("Updating Partition drivers done");

				/*Create the target path, if it doesn't already exist.*/
	CreatTmpTwo();
	IsValidPath( vInfo.szPath + 2, (UINT)((int)vInfo.chDestin) - 0x40, TRUE );
	CreateTmpDir();								/* Create the rup directory	*/
	DeleteTmpTwo();

				/* Create tmp directory in OLD_DOS to copy Recovery files into*/
	BuildPath( szTmpDos, vInfo.szTmpDir[0], vInfo.szTmpDir + 2, TMP_DOS_DIR );

	if ( lpInstall->Flags.fUninstall )
   {
      LOG("Creating recovery");

		if ( mkdir( szTmpDos ) != OK )
			FatalError( BAD_TMPDIR );

		CreateRecoveryFloppy(); 		  /* Create the recovery floppy */

	   DelTmpFiles();                  /* Delete temporary scratch files and dir. */

      LOG("Creating recovery Done");
   }
	else
	{
		GetHdBpb( &HdBpb );						/* Get hd's bpb from boot sect */

		GetOldSysNames();							/* Init. global values */

		/* Read MBR into pchMbrBuf, and copy to pchOldPartRec if valid.
		 * FixupBootRec() needs pchOldPartRec.
		 */
		if ( RdWrSector(0x80, 0, 0, 1, READ) == OK )
		{
			if ( IsValidPartTable() )
				memcpy( pchOldPartRec, pchMbrBuf, SECTOR_SIZE );
		}
		else
			FatalError( FATAL_HD_READ_ERROR );
	}
	CreateNulFile( iNextFile++ ); 			/* Create null file 0			*/

	if ( lpInstall->Flags.fUninstall )
	{
		iWillReboot = TRUE;
		vCurrentFile.chValidErrors = REBOOT;	/* Signal can't quit now    */
		ForceExitReboot();
      LOG("Writing out temporary master boot record");
		WriteNewBoot( FALSE );						/* Write temp boot record		*/
      LOG("Writing out temporary master boot record done");
	}
   LOG("Creting readme");
	CreateReadMe();
   LOG("Cretaing readme done");

	CreateNulFile( iNextFile++ );

   LOG("Moving Files");
	MoveFiles( DO_MOVE );						/* Move DOS files to tmp dir	*/
   LOG("Loving Files done");

	CreateNulFile( iNextFile++ );

	LOG("Fixing up Root");
   FixupRoot();
	LOG("Fixing up Root done");

	CreateNulFile( iNextFile++ );

   LOG("renaming OEM Files");
	RenameOemFiles( FALSE );					/* Rename old utilities kept	*/
   LOG("Renaming OEM Files done");

	CreateNulFile( iNextFile++ );

	iDiskNum = FIRST_USER_DISK;

   LOG("Entering Main file install loop");

						/* NOTE: Recovery disks were 0 & 1 so min here must be 2 */
	while( iDiskNum < (int)vInfo.uchNumDisks )
	{
		InstallFiles( iDiskNum, TRUE );		/* Install files to hard disk	*/
		CreateNulFile( iNextFile++ );
		iDiskNum++;
	}
   LOG("Exiting Main file install loop");

	if ( !vInfo.Args.fMininum ) {		/* M002 Don't update for minimum	*/
      LOG("Updating lie table");
		UpdateLie();						/* Update lie table					*/
   }
	MakeHidden( vInfo.chDestin ); 	/* Set system file attributes		*/

	if ( !vInfo.Args.fMininum ) {		/* M002 Don't copy for minimum	*/
      LOG("Copying command.com");
		CopyCommandCom();					/* Copy command.com to DOS dir	*/
   }

	CreateNulFile( iNextFile++ );

   LOG("Processing Config");
	ProcessConfig();
   LOG("Processing Auto");
	ProcessAuto();
	CreateNulFile( iNextFile++ );

   LOG("Move Auto/Config");
	MoveAutoConfig();
	CreateNulFile( iNextFile++ );

   LOG("Fixup Boot record");
	FixupBootRecs();							/* Fixup all boot records		*/
	CreateNulFile( iNextFile++ );

   LOG("Write new Master boot record");
	WriteNewBoot( TRUE );		/* Replace tmp boot record with real one */
	UpdateFileBar( 1 );

   LOG("delete all the NULL files.");
	DeleteNulFiles();
   LOG("Returning from function UpgradeHard()");
}

/***************************************************************************/
/* Create the RUP tmp directory and then expands it to make sure it has 	*/
/* all clusters allocated to it when the FAT is saved on the recovery		*/
/*	disk. The RUP directory will be "\RUPXX" where the XX is a number from	*/
/* 1 to 99. 																					*/
/* Because the install program in DOS 4.x left old msdos system files		*/
/* laying around we have to delete any io.sys or msdos.sys files that are	*/
/* not the first 2 directory entries on the disk.									*/
/*																									*/
/* void CreateTmpdir( void )																*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void near CreateTmpDir( void )
{
	#define			_A_FINDALL (_A_HIDDEN | _A_SYSTEM | _A_RDONLY)
	static char 	*szPath = "X:\\OLD_DOS.000";
	int				i;

	static char		*szAnyFile = "X:\\*.*";
	char				szName1[ 15 ];
	char				szName2[ 15 ];
	struct find_t	Info;
	#ifdef NET_SETUP
		static char		*szNetDir = NET_DIR;
	#endif


												 			/* Set drive letter in paths	*/
	szPath[ 0 ] = szAnyFile[0] = szBios[0] = szDos[0] = vInfo.chDestin;

									/* Following code deletes bogus system files		*/
	szName1[0] = szName2[0] = EOL;
	if ( _dos_findfirst( szAnyFile, _A_FINDALL, &Info ) == OK )
	{
		strcpy( szName1, Info.name );
		if ( _dos_findnext( &Info ) == OK )
			strcpy( szName2, Info.name );
	}

	if ( strcmpi( szName1, szBios + 3 ) != OK &&
		  strcmpi( szName2, szBios + 3 ) != OK )
		AbsUnlink( szBios );
	if ( strcmpi( szName1, szDos + 3 ) != OK &&
		  strcmpi( szName2, szDos + 3 ) != OK )
		AbsUnlink( szDos );
									/* End of code for deleting bogus system files	*/
	for (i = 1; i < 100; i++)
	{
		itoa( i, szPath + 11, 10 );
		DisplayFileStatus( szPath + 3, CREATE );
		if ( mkdir( szPath ) == OK )
			break;
	}

	if ( i < 1000 )
	{
		strcpy( vInfo.szTmpDir, szPath );
		ExpandRupDir( MoveFiles( DO_COUNT ) + MAX_DISK + 10 );

								/* Create tmp autoexec.bat and config.sys names */
		strcpy( vInfo.szTmpConfig, szPath );
		strcat( vInfo.szTmpConfig, "\\CONFIG.XXX" );					/* M003	*/
		strcpy( vInfo.szTmpAuto, szPath );
		strcat( vInfo.szTmpAuto, "\\AUTOEXEC.XXX" );					/* M003	*/

		#ifdef NET_SETUP
												/* Create the directory for net stuff	*/
			strcpy( vInfo.szNetDir, szPath );
			strcat( vInfo.szNetDir, szNetDir );
			DisplayFileStatus( szNetDir + 1, CREATE );
			if ( mkdir( vInfo.szNetDir ) != OK )
				FatalError( FATAL_HD_WRITE_ERROR );
		#endif
	}
	else
		FatalError( ROOT_DIR_FULL );
}

/***************************************************************************/
/* Create a null file in the rup directory.											*/
/*																									*/
/*	void CreateNulFile( iType )															*/
/*																									*/
/*	ARGUMENTS:	iType			- The sequence number for the null file			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void near CreateNulFile( int SeqNum )
{
	register 		iStatus;
	static char 	*szNullFile = "diskXXXX.XXX   ";
	char				*szPath;
	int				hFile;

   LOG("Creating Null File");

	iStatus = ERROR;

	itoa( SeqNum, szNullFile + 4, 10 );
	strcat( szNullFile, ".NUL" );

	szPath = GetMemory( MAX_PATH_LEN );
	BuildPath( szPath, vInfo.szTmpDir[0], vInfo.szTmpDir + 2, szNullFile );

	if ( _dos_creat( szPath, _A_NORMAL, &hFile ) == OK &&
		  _dos_close( hFile ) == OK )
		iStatus = OK;

	FreeMemory( szPath );

	if ( iStatus != OK )
		FatalError( FATAL_HD_WRITE_ERROR );

   LOG("Creating Null File done");

}

/***************************************************************************/
/* Renames the existing OEM files which we want to keep but need to rename	*/
/* so they can be replaced by the new DOS utility. Function will also 		*/
/* rename the files back to the orginal names during recovery. To reduce	*/
/* the code size we need to cheat by changing the name of the TmpDir to		*/
/* the same name as the new DOS directory so that we can call the function	*/
/* MoveDeleteFile() and have it rename all files with an executable			*/
/* extension for us. We restore the name of the Tmp directory before			*/
/* returning.																					*/
/* 																								*/
/* void RenameOemFiles( int IsRecovery )												*/
/* 																								*/
/* ARGUMENTS:	IsRecovery - FALSE if upgrade or TRUE if recovery				*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void RenameOemFiles( int IsRecovery )
{
	register 	i; 						/* Loop and array indice					*/
	char			*szSource; 				/* Current file name 						*/
	char			*szDestin;
	char			*szTmpDir;

	if ( vInfo.Args.fMininum )			/* M002 Don't rename for minimum	setup	*/
		return;

	szTmpDir = GetMemory( MAX_PATH );
	strcpy( szTmpDir, vInfo.szTmpDir );
	strcpy( vInfo.szTmpDir, vInfo.szPath );

	for ( i = 0; (szSource = GetDataString( RE_NAME, i)) != NULL; i += 2 )
	{
		szDestin = GetDataString( RE_NAME, i + 1 );
		if ( IsRecovery )
			MoveDeleteFile( szDestin, szSource, DO_MOVE );
		else
			MoveDeleteFile( szSource, szDestin, DO_MOVE );
	}

	strcpy( vInfo.szTmpDir, szTmpDir );
	FreeMemory( szTmpDir );
}

/***************************************************************************/
/* Moves all files which need to be moved from the DOS directory. First 	*/
/* moves all the files marked for deletion in the dosdata file and then 	*/
/* moves all of the files which are being replaced by new files. The files */
/* are renamed into the upgrade tmp directory.										*/
/* 																								*/
/* If the argument Type == DO_DELETE the new files are deleted from the 	*/
/* DOS directory. If Type == DO_COUNT files are not moved or deleted and	*/
/* only counted.																				*/
/* 																								*/
/* int MoveOldFiles( int Type )															*/
/* 																								*/
/* ARGUMENTS:	Type		- 0 == move, 1 == delete file, other == count		*/
/* RETURNS: 	int		- Count of file found needing to be moved 			*/
/* 																								*/
/***************************************************************************/

int MoveFiles( int Type )
{
	register 	i; 						/* Loop and array indice					*/
	register 	iDisk;					/* Current distribution disk				*/
	char			*szFile; 				/* Current file name 						*/
	int			iCount;					/* Total number of files found			*/

	if ( vInfo.Args.fMininum )			/* M002 Don't move for minimum setup	*/
		return( 0 );

	iCount = 0;
							/* First make sure there are some files in the dir 	*/
	if ( !IsDirEmpty( vInfo.szPath ) )
	{
												/* Now get all files which are marked	*/
												/* for deletion in the dosdata file 	*/
		for ( i = 0; (szFile = GetDataString( DELETE_FILE, i )) != NULL; i++ )
			iCount += MoveDeleteFile( szFile, szFile, Type );

												/* Now chk for duplicate renamed files	*/
		if ( Type != DO_DELETE )		/* but recovery can't delete them		*/
		{
			for ( i = 0; GetDataString( RE_NAME, i) != NULL; i += 2 )
			{
				szFile = GetDataString( RE_NAME, i + 1 );
				iCount += MoveDeleteFile( szFile, szFile, Type );
			}
		}
							/* Now move all files being replaced with new version */
							/* If recovery we need to delete the files which 		*/
							/* replace the renamed files.									*/
		for ( iDisk = FIRST_USER_DISK; iDisk < (int)vInfo.uchNumDisks; iDisk++ )
			for ( i = 0; (szFile = GetFileName( iDisk, i )) != NULL; i++ )
				if ( (szFile = GetRealDestName( szFile )) != NULL &&
						!FindDataMatch( szFile, NET_FILES ) &&
						(Type == DO_DELETE || !FindDataMatch( szFile, RE_NAME )) )
					iCount += MoveDeleteFile( szFile, szFile, Type );
	}
	return( iCount );
}

/***************************************************************************/
/* Moves the specified file from the DOS directory to the tmp directory.	*/
/* The argument Type signals if the file is to be moved, deleted or just	*/
/* counted. If the file has an executable extension the move or delete is	*/
/* repeated for all 3 possible executable file extensions.						*/
/* 																								*/
/* If the argument Type == DO_DELETE the new files are deleted from the 	*/
/* DOS directory. If Type == DO_COUNT files are not moved or deleted and	*/
/* only counted.																				*/
/* 																								*/
/* int MoveDeleteFile( char *szFile, int Type	)									*/
/* 																								*/
/* ARGUMENTS:	szFile	- Ptr to name of file to be moved 						*/
/* 				Type		- 0 == move, 1 == delete file, other == count		*/
/* RETURNS: 	int		- Returns TRUE if file exists 							*/
/* 																								*/
/***************************************************************************/

int near MoveDeleteFile( char *szFileSource, char *szFileDestin, int Type )
{

	char			*szDestin;
	char			*szSource;
	register 	iFileCount;
	register 	i;
	int			IsExeFile;

	if (Type == DO_DELETE)
		ProPrintfStat1(IDS_WAITDEL);
	else if ( Type == DO_MOVE)
		ProPrintfStat1(IDS_WAITREN);

	IsExeFile = (FindExtMatch( szFileSource, szExeExt ) != -1 ? TRUE: FALSE);

	szSource = GetMemory( MAX_PATH_LEN * 2 ); /* Get memory for file paths	*/
	szDestin = szSource + MAX_PATH_LEN;

	BuildPath( szSource, vInfo.szPath[0], vInfo.szPath + 2, szFileSource );
	BuildPath( szDestin, vInfo.szTmpDir[0], vInfo.szTmpDir + 2, szFileDestin );

	for ( i = iFileCount = 0; i < 3; i++ )
	{
		if ( IsExeFile )
		{
			strcpy( strchr( ParseFileName( szSource ), '.' ) + 1, szExeExt[i] );
			strcpy( strchr( ParseFileName( szDestin ), '.' ) + 1, szExeExt[i] );
		}

		if ( FileExists( szSource ) ) 			/* Make sure file exists		*/
		{
			iFileCount++;								/* Increment file found count */
			if ( Type != DO_COUNT )					/* Display the file status 	*/
				DisplayFileStatus( szFileSource,
										 Type == DO_DELETE ? DELETE : RENAME );

			if ( Type == DO_DELETE )				/* See if deleting the file	*/
				remove( szSource );
			else if ( Type == DO_MOVE )			/* See if moving the file		*/
				if ( rename( szSource, szDestin ) != OK )
					FatalError( ERROR_MOVING_FILE );
		}
		if ( IsExeFile == FALSE )					/* If file is not executable	*/
			break;										/* break out of for loop		*/
	}

	FreeMemory( szSource );
	return( iFileCount );
}

/***************************************************************************/
/* Set the 2 system file attribute to hidden system on the specified 		*/
/* drive.																						*/
/* 																								*/
/* void Makehidden( char Drive ) 														*/
/* 																								*/
/* ARGUMENTS:	Drive 	- DOS drive letter to set the file attribs on		*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void MakeHidden( char Drive )
{
	*szBios = *szDos = *szVxd = Drive;

	_dos_setfileattr( szBios, _A_HIDDEN | _A_SYSTEM | _A_RDONLY );	/* m103	*/
	_dos_setfileattr( szDos,  _A_HIDDEN | _A_SYSTEM | _A_RDONLY );	/* m103	*/
	_dos_setfileattr( szVxd,  _A_RDONLY );
}

/***************************************************************************/
/* Builds a MULT_FILES structure for the specified disk and then calls		*/
/* Xcopy() to install the new files.													*/
/* 																								*/
/* void InstallFiles( int iDisk )														*/
/* 																								*/
/* ARGUMENTS:	iDisk - Distribution disk number to install						*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void near InstallFiles( int iDisk, int IsHdUpgrade )
{
	#define					BUF_LEN (sizeof( struct MULT_FILES ) * MAX_PER_DISK)
	struct MULT_FILES 	*Files;

	Files = GetMemory( BUF_LEN );
	memset( Files, 0, BUF_LEN );

	InitFileStruc( Files, iDisk, IsHdUpgrade );

			/* M002  If minimum install only copy 1st 3 DOS files	*/
	if ( vInfo.Args.fMininum && FIRST_USER_DISK == iDisk )
		Files[ 3 ].Name.Source = NULL;
//	Xcopy( Files );
	CopyFiles( IDS_WAITCOPY, Files );


	FreeMemory( Files );
	#undef					BUF_LEN											/* m111	*/
}

/***************************************************************************/
/* Initializes a MULT_FILES structure for the specified disk.					*/
/* 																								*/
/* void InitFileStruc( struct MULT_FILES *FileStruc, int iDisk,				*/
/*							  int IsHdUpgrade )												*/
/* void InstallFiles( int iDisk )														*/
/* 																								*/
/* ARGUMENTS:	FileStruc	- Array of MULT_FILES strucs to be filled in 	*/
/* 				iDisk 		- Distribution disk number to be copied			*/
/* 				IsHdUpgrade - Signals doing a hard disk upgrade if TRUE		*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void InitFileStruc( struct MULT_FILES *FileStruc, int iDisk, int IsHdDisk )
{
	register 				i;
	char						*szTmp;
	struct MULT_FILES 	*File;

	for ( i = 0, File = FileStruc;
			(szTmp = GetFileName( iDisk, i )) != NULL;
			i++ )
	{
		if ( InitFileInfo( szTmp, File, iDisk, IsHdDisk ) == OK )
			File++;
	}
}

/***************************************************************************/
/* Initializes a file info structure for the specified file name. The data	*/
/*	base is checked for the file and all translations for this specific 		*/
/*	file are done. 																			*/
/* 																								*/
/* Possible translations and the order in which they are done					*/
/* 																								*/
/* 	Destin file - Don't copy the file            - [no-copy]             */
/* 					  Rename file							- [rename]					*/
/* 					  Use different directory path	- [diff-path]				*/
/* 																								*/
/* 	Source file - Use alternate file					- [diff-file]				*/
/* 					  Get file from alternate disk	- [diff-disk]				*/
/* 																								*/
/* 																								*/
/*	int InitFileInfo( char *szFileName, struct MULT_FILES *FileStruc,			*/
/*							int iIsHdDisk )													*/
/* 																								*/
/* ARGUMENTS:	szFileName	- The name of the file to initialize				*/
/*					FileStruc	- Array of MULT_FILES strucs to be filled in 	*/
/* 				iDisk 		- Distribution disk number to be copied			*/
/* 				iIsHdDisk	- Signals destination is a hard drive				*/
/* RETURNS: 	int			_ OK if file gets copy else ERROR for no copy	*/
/* 																								*/
/***************************************************************************/

int near InitFileInfo( char *szName, struct MULT_FILES *File, int iDisk,
							  int IsHdDisk )
{
											/* Determine destination information		*/

	if ( (File->Name.Destin = GetRealDestName( szName )) == NULL )
		return( ERROR );				/* Don't copy was specifed for this file  */

	File->Path.Destin = GetRealDestPath( File->Name.Destin,
													 vInfo.szPath + 2 );
	File->UserDisk = IsHdDisk ? NOT_REMOVEABLE : iDisk;/* User's disk			*/
	File->Drive.Destin = vInfo.chDestin;					/* Set drive letter	*/

														/* Determine source information	*/
	if ( (File->Name.Source = GetRealSrcName( szName )) == NULL )
		return( ERROR );

															/* Set path to distrib files 	*/
	File->Path.Source =  vInfo.szSource + 2;
													/* Distrib disk checking for a path	*/

	File->DiskNumber =  GetRealSrcDisk( File->Name.Source, iDisk );
	if ( vInfo.chSource >= vInfo.chFirstHd )
		File->DiskNumber = NOT_REMOVEABLE;

	File->Drive.Source = vInfo.chSource;					/* Set drive letter	*/

	return( OK );
}

/***************************************************************************/
/* Copies the command.com file from the root dir into the DOS dir.			*/
/* 																								*/
/* void CopyCommandCom( void )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/*	m108	-	Changed so DOSSHELL.INI is copied from OLD_DOS.xxx if exists	*/
/***************************************************************************/

void near CopyCommandCom( void )
{
// m108	struct MULT_FILES 	File[2];
	struct MULT_FILES 	File[ 3 ];											/* m108	*/
	static char				*szFileName = "COMMAND.COM";
	extern char				*szRootPath;
	char						szPath[ MAX_PATH_LEN ];							/* m108	*/
	static char				*szDosShell = "DOSSHELL.INI";					/* m108	*/

// m108	memset( File, 0, sizeof( struct MULT_FILES ) * 2 );
	memset( File, 0, sizeof( struct MULT_FILES ) * 3 );				/* m108	*/

	File[ 0 ].Name.Destin = File[ 0 ].Name.Source = szFileName;
	File[ 0 ].Path.Destin = vInfo.szPath + 2;
	File[ 0 ].Path.Source = szRootPath;
	File[ 0 ].UserDisk = File[ 0 ].DiskNumber = NOT_REMOVEABLE;
	File[ 0 ].Drive.Destin = File[ 0 ].Drive.Source = vInfo.chDestin;

					/* m108 If DOSSHELL.INI exists in OLD_DOS, copy to DOS dir	*/
	BuildPath( szPath, vInfo.szTmpDir[0], vInfo.szTmpDir + 2,szDosShell );/* m108	*/
	if ( FileExists( szPath ) )												/* m108	*/
	{																					/* m108	*/
		File[ 1 ].Name.Destin = File[ 1 ].Name.Source = szDosShell;	/* m108	*/
		File[ 1 ].Path.Destin = vInfo.szPath + 2;
		File[ 1 ].Path.Source = vInfo.szTmpDir + 2;							/* m108	*/
		File[ 1 ].UserDisk = File[ 1 ].DiskNumber = NOT_REMOVEABLE;	/* m108	*/
		File[ 1 ].Drive.Destin = File[ 1 ].Drive.Source = vInfo.chDestin;/* m108*/
	}																					/* m108	*/

//	Xcopy( File );

	CopyFiles( (int)NULL, File );

}	

/***************************************************************************/
/* Creates a unique file name on the specified drive in the specified		*/
/* directory and then deletes the file and returns the name, including		*/
/* the path in a buffer supplied by the caller. The name will be a			*/
/* an acii number between "0" and "999". If the file can't be created		*/
/* the program will be aborted with a FatalError() call.							*/
/*																									*/
/* void CreatUniqueName( char chDrive, char *szPath, char *szName )			*/
/*																									*/
/* ARGUMENTS:	chDrive		- Drive letter	for the unique file.					*/
/*					szPath		- Directory path for the unique file.				*/
/*					szname		- Buffer to store the complete file pathname		*/
/* RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void CreatUniqueName( char chDrive, char *szPath, char *szName )
{
	int		i, iHandle;

			/* Create the path string */
	*szName = chDrive;
	*(szName + 1) = ':';
	*(szName + 2) = '\\';
	strcat( szName + 2, szPath );

				/* Keep trying to create a file with an ascii number name */
	for( i = 0; i < 1000; i++ )
	{
		itoa( i, szName + 3, 10 );
		if ( (iHandle=creat( szName, S_IREAD | S_IWRITE )) != -1 )
		{
			if ( close( iHandle ) != OK )
				FatalError( ROOT_DIR_FULL );
			break;
		}
	}
				/* If i < 1000 we created a file so now delete it */
	if ( i < 1000 && unlink( szName ) == OK )
		;
	else
		FatalError( ERROR_DELETING_TMP_FILE );
}

/***************************************************************************/

static char		szTmp1[25];
static char		szTmp2[25];

/***************************************************************************/

/***************************************************************************/
/* Creates 2 tmp files in the root directory. This is to act as place		*/
/* holders in case the first 2 directory entries are free so that creating	*/
/* the OLDDOS the DOS directory and the OLD_DOS directory do not take		*/
/* get allocated these 2 entries.														*/
/*																									*/
/*	void CreatTmpTwo( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void CreatTmpTwo( void )
{
	extern char		*szRootPath;

	CreatUniqueName( vInfo.chDestin, szRootPath, szTmp1 );
	CreatUniqueName( vInfo.chDestin, szRootPath, szTmp2 );
}

/***************************************************************************/

void DeleteTmpTwo( void )
{
	remove( szTmp1 );
	remove( szTmp2 );
}

/***************************************************************************/
/* Deletes all of the DISK.NUL files in the OLD_DOS.xxx directory. The		*/
/* function should be called after writing the final master boot record		*/
/* onto the destination hard disk.														*/
/*																									*/
/*	void DeleteNulFiles( void )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void near DeleteNulFiles( void )
{
	char				szPath[ MAX_PATH ];
	char				*szName;
	register			i;

	BuildPath( szPath, vInfo.szTmpDir[0], vInfo.szTmpDir + 2, "" );
	szName = ParseFileName( szPath );
	strcpy( szName, "DISK" );

	for ( i = 0; i <= MAX_DISK; i++ )
	{													/* Delete all DISKxx.NUL files */
			itoa( i, szName + 4, 10 );
			strcat( szName, ".NUL" );
			DisplayFileStatus( szName, DELETE );
			AbsUnlink( szPath );
	}
}

/***************************************************************************/
/* Creates the readme.now file in the OLD_DOS.xxx directory. 					*/
/*																									*/
/*	void CreateReadMe( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void near CreateReadMe( void )
{
	char			szPath[ MAX_PATH ];
	char			*Buf;
	int			iFile;
	unsigned		uToWrite;
	unsigned		uWritten;
	
	Buf = GetMemory( 2000 );
	GetReadMeText( Buf );

	BuildPath( szPath, vInfo.szTmpDir[0], vInfo.szTmpDir + 2, README_FILE );
	DisplayFileStatus( README_FILE, WRITE );

	uToWrite = strlen( Buf );
	if ( _dos_creat( szPath, 0, &iFile ) != OK ||
		  _dos_write( iFile, Buf, uToWrite, &uWritten) != OK ||
		  uToWrite != uWritten ||
		 _dos_close( iFile ) != OK )
		FatalError( FATAL_HD_WRITE_ERROR );

	UpdateFileBar( 1 );

	FreeMemory( Buf );
}


/***************************************************************************/
/* Expands the backup directory created to store the user's old DOS files	*/
/* in. The directory needs to be expanded before we save the FAT so that	*/
/* it will be reflected in the saved FAT. The directory is expanded by		*/
/* creating MAX_DOS_FILES number of zero lenght files in it. If the			*/
/* directory can't be expanded as would be reflected by an error in			*/
/* in creating one of the files, the program will be aborted with a call	*/
/* to FatalError(). All files which are created are also deleted.				*/
/*																									*/
/* void ExpandRupDir( void )																*/
/*																									*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void ExpandRupDir( int iTotalFiles )
{
	char		*szName;
	char		*szFile;

	int		i;
	int		iHandle;
	int		Status;

	Status = OK;

	szName = vInfo.szTmpDir;				/* Use existing path buf for the 	*/
	szFile = strchr( szName, EOL );		/* file path								*/
	strcpy( szFile, "\\RUP" ); 			/* Append RUP to path					*/

					/* Create each new file and then close it */

	for( i = 0; Status == OK && i < iTotalFiles; i++ )
	{
		itoa( i, szFile + 4, 10 );
		if ( (iHandle = creat( szName, S_IREAD | S_IWRITE )) != -1 )
			close( iHandle );
		else
			Status = ERROR;
	}

	while( Status == OK && --i >= 0 )	/* Close any files we created 		*/
	{
		itoa( i, szFile + 4, 10 );
		if ( unlink( szName ) != OK )
			Status = ERROR;
	}	

	if ( Status != OK )
		FatalError( BAD_TMPDIR );

	*(szFile) = EOL;				/* Restore EOL at end of original path name	*/
}

/***************************************************************************/
/* Deletes all files remaining in the TmpDos directory and then deletes		*/
/* the directory entry.																		*/
/*																									*/
/*	void DelTmpFiles( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		VOID																			*/
/*																									*/
/***************************************************************************/

void near DelTmpFiles( void )
{
	register				iStatus;
	char					szFile[ MAX_PATH ];
	struct find_t		Info;

	BuildPath( szFile, szTmpDos[ 0 ], szTmpDos + 2, "*.*" );

	for ( iStatus = (int)_dos_findfirst( szFile, _A_FINDALL|_A_NORMAL, &Info );
			iStatus == OK;
			iStatus = (int)_dos_findnext( &Info ) )
	{
		BuildPath( szFile, szTmpDos[ 0 ], szTmpDos + 2, Info.name );
		AbsUnlink( szFile );
	}

	/* Make current directory something other than the one we're deleting */
	chdir( vInfo.szTmpDir );

	rmdir( szTmpDos );

	UpdateFileBar( 2 );
}

#ifdef LOGGING

/* void FAR LogProgress(char *pszInfo);
 *
 * Function is used to log progress information. Used in debug and
 * special "LOGGING = ON" mode.
 *
 */
void FAR LogProgress(char *pszInfo)
{
   static BOOL bLogCreated = 0;
	static char	CrLf[ 3 ] = { CR, LF, EOL };
   char        szTmp[256];
	int			iFile;
	unsigned		uToWrite;
   unsigned    uWritten;
	
   strcpy( szTmp, pszInfo );
   strcat( szTmp, CrLf );
	uToWrite = strlen( szTmp );

   if (! bLogCreated ) {
      _dos_creat("c:\\dos2gui.log", 0, &iFile );
      bLogCreated = TRUE;
   }
   else
      _dos_open("c:\\dos2gui.log", O_WRONLY, &iFile);

   lseek(iFile, 0L, SEEK_END);  // Seek to the end of the file.

   _dos_write( iFile, szTmp, uToWrite, &uWritten);
   _dos_close( iFile );
}

#endif
