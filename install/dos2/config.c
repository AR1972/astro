/***************************************************************************/
/*																									*/
/* CONFIG.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Processes the CONFIG.SYS file in the root directory or creates one		*/
/* if it doesn't already exist.															*/
/*																									*/
/* NOTE:																							*/
/* 		In order to correctly process the mouse driver the config.sys		*/
/* 		file processing must be done before calling ProcessAutoexec().		*/
/* 		This will insure that any existing mouse.sys driver in the			*/
/* 		config.sys file has been identified and removed and the				*/
/* 		vInfo.fMouse flag	has been set to signal adding mouse to				*/
/* 		autoexec.bat.																		*/
/*																									*/
/*																									*/
/* Change log:																					*/
/*																									*/
/*   	Date      Who   #		Description													*/
/* 	--------  ---  ----	---------------------------------------------	*/
/*      12/07/89  JAH          Created													*/
/*      02/19/91  DLB  M006    Rename HIMEM.DOS and EMM386.DOS to				*/
/*										 HIMEM.SYS and EMM386.EXE. Also move EMM386	*/
/*										 after rename is done, not before.				*/
/*																									*/
/***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <io.h>
#include <dos.h>
#include <string.h>
#include <ctype.h>

#include	<alias.h>
#include <global.h>
#include <strlib.h>
#include <data.h>
#include	<upgrade.h>
#include	<errors.h>
#include	<prompts.h>

/***************************************************************************/

extern void	NewAutoConfig	( int iType, char *szFile );
void		ProcessFile			( int iType, char *szOldFile, char *szNewFile,
									  int (*Funcs[])( int iStatus ) );
int		BuildNewFile( int (*Funcs[])( int Status	) );
void		CreateDeviceLine	( char *szLine, char *szDevice, int iType );
/* void		ProcessConfig		( void ); */
int		NewConfigFirst		( int iStatus );
int		NewConfigLast		( int iStatus );
int		ProcessConfigLine	( int iStatus );

char		*NewFileGets		( char *szBuffer, int Count, FILE *File );
int		NewFilePuts			( char *szString, FILE *File );

static int near	ProcessShellLine	( char *szStr );
static int near	ProcessDeviceLine	( char *szPtr, int iType	);
static int near	InstallNewDrivers	( void );
static int near	AddNewDevice		( char *szDevName, int iType	);
static void	near	MoveToEndFile		( char *szStr, int iDevice );

static int	near AddDrvParm		( void );
static int	near add_driv_line	( char *szDrivparm, UCHAR uchDrv );
static void near find_drive		( UCHAR *puchDrv1, UCHAR *puchDrv2 );

/***************************************************************************/

/* M006 */
enum			DEV_TAGS 		{ HIMEMSYS, HIMEMDOS, SMARTDRV, EMM386SYS, EMM386DOS, EMM386EXE,
									  MOUSE, RAMDRIVE, EGASYS, SHARE, SETVER,
									  LAST_DEVICE };

#define		MAX_LEN			256		/* Max length of line in autoexec.bat	*/
#define		STR_BUF_LEN		128
#define		MAX_RAM_DRIVE	23 		/* Max number of RAM drives installed	*/
#define		MAX_DEV_LINE	RAMDRIVE + MAX_RAM_DRIVE + 1

#define		HIMEM_SYS		Devices[ HIMEMSYS ]     /* M006 */
#define		HIMEM_DOS		Devices[ HIMEMDOS ]     /* M006 */
#define		SMARTDRV_SYS	Devices[ SMARTDRV ]
#define		EMM386_SYS		Devices[ EMM386SYS ]
#define		EMM386_DOS		Devices[ EMM386DOS ]    /* M006*/
#define		EMM386_EXE		Devices[ EMM386EXE ]
#define		MOUSE_SYS		Devices[ MOUSE ]
#define		RAMDRIVE_SYS	Devices[ RAMDRIVE ]

#define		DEVICE			0			/* Specifies a device driver				*/
#define		INSTALL			1			/* Specifies an installable program 	*/
#define		LOAD_HIGH		2

#define		REM_LINE			1
#define		DEL_LINE			2

/***************************************************************************/

/* declared in upgrade.c */
extern char		*szAutoBat; 			/* Autoexec drive, path and name str	*/
extern char		*szConfSys;				/* Config.sys drive, path and name str */

FILE		*OldFile;						/* File handle for existing autoexec	*/
FILE		*NewFile;						/* File handle for new autoexec			*/

char		*szNewString;					/* Config.sys line for new file			*/
char		*szOldString;					/* Config.sys line from old file			*/

char 				*szShell   	= "SHELL=";
char				*szCommandShell = "COMMAND.COM ";

static char 	*szDevice  	= "DEVICE=";
static char 	*szInstall 	= "INSTALL=";
static char		*szLoadHi	= "DEVICEHIGH=";
static char		*szDosEqu  	= "DOS";
static char		*szShellEqu	= "SHELL";
static char		*szInstallEqu = "INSTALL";
static char		*szDeviceEqu = "DEVICE";
static char		*szLoadHiEqu = "DEVICEHIGH";

struct BITS
{
	unsigned 	fXma			:1;
	unsigned 	fHimem		:1;
	unsigned 	fSmartDrv	:1;
	unsigned 	fRamDrv		:1;
	unsigned 	fEmm386		:1;
	unsigned 	fPath 		:1;
	unsigned 	fMouse		:1;
	unsigned 	fDosKey		:1;
	unsigned 	fDosShell	:1;
	unsigned 	fNeedHiMem	:1;
	unsigned		fHaveDosEqu	:1;
	unsigned		fHaveEga		:1;
	unsigned		fHaveComspec:1;
} DevFlags;

/* M006 */
static char	*Devices[] = { "HIMEM.SYS", "HIMEM.DOS", "SMARTDRV.SYS",
									"EMM386.SYS", "EMM386.DOS", "EMM386.EXE", "MOUSE.SYS",
									"RAMDRIVE.SYS", "EGA.SYS", "SHARE.EXE",
									"SETVER.EXE", NULL };

static UCHAR DeviceNameLen[] = { (UCHAR) 9, (UCHAR) 9, (UCHAR) 12,
											(UCHAR) 10, (UCHAR) 10, (UCHAR) 10, (UCHAR) 9,
											(UCHAR) 12,	(UCHAR) 7, (UCHAR) 9,
											(UCHAR) 10, (UCHAR) 0 };
/* M006 */

static char *DeviceLines[ MAX_DEV_LINE ];
static int	RamDriveCount;				/* # of times RAMDRIVE installed 	*/

/***************************************************************************/
/* Process an autoexec or config.sys file by opening the old file and and	*/
/* and creating a new file and then passing a series of parsing functions	*/
/* from the caller to BuildNewFile(). If the original file doesn't exist	*/
/* or there is an error creating the new file a new file will be created	*/
/* using the information in the vInfo structure.									*/
/*																									*/
/*	void ProcessFile( int iType, char *szOldFile, char *szNewFile,				*/
/*						int (*Funcs[])( int iStatus ) )									*/
/*																									*/
/*	ARGUMENTS:	iType			- 0 == AUTOEXEC.BAT, 1 == CONFIG.SYS				*/
/*					szOldFile	- Name of existing file to process					*/
/*					szNewFile	- Name for new file to be created					*/
/*					Funcs			- Array of ptrs to file processing functions		*/
/*	RETURNS:		void																			*/
/*																									*/
/* 																								*/
/***************************************************************************/

void ProcessFile( int iType, char *szOldFile, char *szNewFile,int (*Funcs[])( int iStatus ) )
{
	register	iStatus;

	iStatus = ERROR;

	if ( !vInfo.Flag.fNewSysFiles &&
		  (OldFile = fopen( szOldFile, "rt" )) != NULL )
	{
		if ( (NewFile = fopen( szNewFile, "wt" )) != NULL )
		{
			iStatus = BuildNewFile( Funcs );
			iStatus |= fclose( NewFile );
		}
		iStatus |= fclose( OldFile );

		if ( iStatus != OK )
			ProcessCopyError( szOldFile, ERR_PROCESSING );
	}
											
	if ( iStatus != OK )
		NewAutoConfig( iType, szNewFile );
}

/***************************************************************************/
/* Generic file processing function. Accepts an array of 3 pointers to 3	*/
/* functions which will do the actual processing of the file. The 3			*/
/* 3 functions must be provided which do the following with the int arg 	*/
/* passed to the function the current error status up to that point. 		*/
/* 																								*/
/* int Function1( int ) - Add new lines to the file which are needed 		*/
/* 							  before lines from the original file are processed*/
/* int Function2( int ) - Called once for each line found in original file */
/* 							  returns 0 if line is to be added else !0			*/
/* 							  The line read from the file is in szOldString &	*/
/* 							  the function fills in szNewString to be written	*/
/* int Function3( int ) - Cleanup function to do anything needed before 	*/
/* 							  the new file is closed.									*/
/* 																								*/
/* 																								*/
/* int BuildNewFile( *Func[])( int Status ) )										*/
/* 																								*/
/* ARGUMENTS:	Funct 	- Array of pointers to 3 functions.						*/
/* RETURNS: 	int		- OK if sucessfull else ERROR 							*/
/* 																								*/
/* NOTE: When a line is specified for not being copied it is prefixed		*/
/* 		with a REM and written anyway.												*/
/***************************************************************************/

int BuildNewFile( int (*Func[])( int Status	) )
{
	register 	iStatus;
	register		iToDo;

	szOldString = GetMemory( MAX_LEN * 3 );
	szNewString = szOldString + (MAX_LEN + 10);

	iStatus = OK;


	iStatus = (*Func[0])( OK );						/* Call startup function	*/

																/* Loop once for each line */
																/* line in the old file 	*/
	while ( iStatus == OK && !feof( OldFile ) )
	{
		if ( NewFileGets( szOldString, MAX_LEN, OldFile ) != NULL )
		{
			if ( (iToDo = (*Func[1])( OK )) != OK )
			{
				if ( iToDo == REM_LINE )
				{
					strcpy( szNewString, "REM " );
					strncpy( szNewString + 4, szOldString, MAX_LEN );
					iStatus = NewFilePuts( szNewString, NewFile );
				}
					/* Else just don't copy the line */
			}
			else
				iStatus = NewFilePuts( szNewString, NewFile );
		}
		if ( ferror( OldFile ) || ferror( NewFile ) )
			iStatus = ERROR;
	}
	iStatus = (*Func[2])( iStatus );

	FreeMemory( szOldString );
	return( iStatus );
}

/***************************************************************************/
/* Creates a new config.sys file in the tmp directory. If an old config.sys*/
/* exists in the root directory it will be used as a model and will be		*/
/* fixed up to reflect then new paths to the DOS directory and the new		*/
/* requirement that EMM386.EXE, RAMDRIVE.SYS and SMARTDRV.SYS require that */
/* HIMEM.SYS be installed to suppy XMA memory access. If there was not an	*/
/* old file a new default file will created. 										*/
/* 																								*/
/* void ProcessConfig( void ) 															*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void ProcessConfig( void )
{
	static int (*Funcs[])(int) = { NewConfigFirst, ProcessConfigLine,
											 NewConfigLast };

	ProcessFile( CONFIG_SYS, szConfSys, vInfo.szTmpConfig, Funcs );
}

/***************************************************************************/
/* Initializes the DeviceLines[] array to all NULL ptrs and then install	*/
/* any new devices which are specified in the DOSDATA.DAT file.				*/
/* 																								*/
/* int NewConfigFirst( int iStatus )													*/
/* 																								*/
/* ARGUMENTS:	iStatus	- Current error status of the file processing		*/
/* RETURNS: 	int		- OK if successfull else ERROR							*/
/* 																								*/
/***************************************************************************/
																		/*lint -e715 */
int NewConfigFirst( int iStatus )
{
	register 	i;

	for ( i = 0; i < MAX_DEV_LINE; i++ )
		DeviceLines[ i ] = NULL;					/* Set line ptrs to NULL	*/

	RamDriveCount = 0;

		/* Add the line "DEVICE=X:\dos\SETVER.EXE"  and any new drivers	*/
	if ( (iStatus = AddNewDevice( "SETVER.EXE", DEVICE )) == OK )
		iStatus = InstallNewDrivers();

	return( iStatus );
}																		/*lint +e715 */

/***************************************************************************/
/* Adds any lines to the end of the config.sys file which need to be added */
/* or which needed to moved to the end of the file and frees any memory 	*/
/* allocated for device line strings in DevicesLines[]. If status value arg*/
/* reflects an error the lines will not be written and only the allocated	*/
/* memory will be freed.																	*/
/* 																								*/
/* int NewConfigLast( int iStatus ) 													*/
/* 																								*/
/* ARGUMENTS:	iStatus	- Current processing status								*/
/* RETURNS: 	int		- Original arg status if successfull else ERROR 	*/
/* 																								*/
/***************************************************************************/

int NewConfigLast( int iStatus )
{
	register 	i;
											
	if ( !DevFlags.fHaveComspec )	/* See if shell= needs to be added		*/
	{
		DefaultShellLine( szNewString );
		NewFilePuts( szNewString, NewFile );
	}
											/* See if Himem.sys needs to be added	*/
	if ( vInfo.Hw.ExtMem >= HIMEM_K_SIZE )
		DevFlags.fNeedHiMem = TRUE;

	if ( vInfo.NoCopy[ NO_HIMEM ] == FALSE &&
		  (DevFlags.fNeedHiMem && !DevFlags.fHimem) )
		MoveToEndFile( Devices[ HIMEMSYS ], HIMEMSYS	);

	if ( vInfo.Hw.VideoType == VIDEO_EGA && !DevFlags.fHaveEga )
		MoveToEndFile( Devices[ EGASYS ], EGASYS );

	for ( i = 0; i < MAX_DEV_LINE; i++ )
	{
		if ( DeviceLines[ i ] != NULL )
		{
			if ( iStatus == OK )
				iStatus = AddNewDevice( DeviceLines[ i ],	DEVICE );
			FreeMemory( DeviceLines[ i ] );
		}
	}

	if ( vInfo.Hw.ExtMem >= HIMEM_K_SIZE && !DevFlags.fHaveDosEqu )
		iStatus = fputs( DOS_HIGH, NewFile );
	return( iStatus );
}

/***************************************************************************/
/* Process a line in the config.sys file. Search for DEVICE= or SHELL=		*/
/* line and then calls function to process that line. If neither of these	*/
/* commands are detected the original string is copied to the new string	*/
/* buffer for adding to the new file.													*/
/* 																								*/
/* int ProcessConfigStr( void )															*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	int		- OK if sucessfull else ERROR to signal don't      */
/* 							  write this string to the new config.sys 			*/
/* 																								*/
/***************************************************************************/

int ProcessConfigLine( int iStatus )
{
	char				*szPtr;
	static char		*ConfEntry[] = { "BUFFERS", "BREAK", "DEVICE", "DEVICEHIGH",
											  "FILES", "FCBS", "LASTDRIVE", "MULTITRACK",
											  "DRIVPARM", "STACKS", "COUNTRY", "SHELL",
											  "INSTALL", "COMMENT", "REM", "SWITCHES",
											  "DOS", NULL };

	iStatus = OK;
	szPtr = szOldString;
	while ( *szPtr == ' ' || *szPtr == '\t' )
		szPtr++;
	/*	szPtr = SkipLeadingWhite( szOldString ); */

	ExtractNextWord( szPtr, szNewString, 25 );	/* Get the entry type	*/
	strupr( szNewString );

	szPtr = SkipWord( szPtr ); 						/* Skip to device name */

	if ( *szPtr != EOL )									/* Check for null str		*/
	{
																/* Check for "DOS="			*/
		if ( strcmpi( szDosEqu, szNewString ) == OK )
		{
			DevFlags.fHaveDosEqu = TRUE;
			strcpy( szNewString, szOldString );
			iStatus = OK;
		}

		else if ( strcmpi( szDeviceEqu, szNewString ) == OK )
			iStatus = ProcessDeviceLine( szPtr, DEVICE );

																/* Check for "INSTALL="		*/
		else if ( strcmpi( szInstallEqu, szNewString ) == OK  )
			iStatus = ProcessDeviceLine( szPtr, INSTALL );

		else if ( strcmpi( szLoadHiEqu, szNewString ) == OK )
			iStatus = ProcessDeviceLine( szPtr, LOAD_HIGH );

																/* Check for "SHELL="		*/
		else if ( strcmpi( szShellEqu, szNewString ) == OK )
			iStatus = ProcessShellLine( szPtr );
																/* Check for invalid entry	*/
		else if ( StrSearch( szNewString, ConfEntry ) == -1 &&
					 *szNewString != EOL )
			iStatus = REM_LINE;

		else													/* Using existing string	*/
			strcpy( szNewString, szOldString );
	}

	return( iStatus );
}

/***************************************************************************/
/* Processes the SHELL= line from the config.sys. If the command file		*/
/* name is COMMAND the path to it will be removed and any reload path will */
/* be removed. This will force it to always be loaded from the root dir.	*/
/* Any existing parameters for /e or /p will be used in the new string. 	*/
/* 																								*/
/* int ProcessShellLine( char *szStr ) 												*/
/* 																								*/
/* ARGUMENTS:	szStr 	- Ptr to device pathname									*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

static int near ProcessShellLine( char *szString )
{
	char			*szPtr;
	char			*szOpts;

	strcpy( szNewString, szShell );			/* Copy "SHELL=" */

	szPtr = strchr( szNewString, EOL );
	BuildPath( szPtr, HD_BOOT_DRV, vInfo.szPath + 2, szCommandShell );
	szPtr = strchr( szNewString, EOL );
	BuildPath( szPtr, HD_BOOT_DRV, vInfo.szPath + 2, " " );

					/* If the existing shell is COMMAND.COM we have to check		*/
					/* the /E: switch and if found copy it to the new string		*/

	szPtr = ParseFileName( szString );			/* Get original comspec file	*/
	if ( strnicmp( szPtr, szCommandShell, 11 ) == OK )
	{
		if ( (szPtr = strchr( szPtr, '/' )) != NULL )
		{
			do		/* Loop looking at all switches but only save the /E switch	*/
			{
				szOpts = strchr( szNewString, EOL );
				szPtr++;												/* Ptr to option	*/
				if ( UCASE_E == toupper( *szPtr ) )
				{
					*(szOpts++) = '/';
					ExtractNextWord( szPtr, szOpts, 25 );
				}
			}
			while ( (szPtr = strchr( szPtr, '/' )) != NULL );
		}
	}
	strcat( szNewString, " /p" );

	DevFlags.fHaveComspec = TRUE;
	return( OK );
}

/***************************************************************************/
/* Processes the DEVICE= line from the config.sys. Changes the paths for	*/
/* devices or deletes the entry based on entries in the dosdata file. The	*/
/* new SMARTDRV, EMM386 and RAMDRIVE need HIMEM installed to work so 		*/
/* SMARTDRV and RAMDRIVE entries as copied to a buffer to be added at the	*/
/* end of the config file and if EMM386 is detected a line to load HIMEM	*/
/* will be added at the current location and the xma already installed		*/
/* flag will be set so it won't be added at the end if it is needed by     */
/* smartdrive or ramdrive. 																*/
/* 																								*/
/* NOTE: See the file SYS.DOC for an explaination of work done to support	*/
/* 		changes to RAMDRIVE, SMARTDRV and EMM386 which now require XMA.	*/
/* 																								*/
/* Changed to also process the INSTALL= lines										*/
/* Changed to cause any DEVICE=SETVER.EXE lines to be deleted					*/
/* 																								*/
/* int ProcessDeviceLine( char *szStr )												*/
/* 																								*/
/* ARGUMENTS:	szStr 	- Ptr to device pathname									*/
/* 				iType 	- Determine if device is DEVICE= or INSTALL= 		*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

int near ProcessDeviceLine( char *szStr, int iType )
{
	char			szDevName[25]; 			/* Storage for device file name	*/
	char			*szPtr;						/* String ptr							*/
	register		i; 							/* Array index 						*/

	struct	TmpFlags
	{
		unsigned 	fDelete		:1;		/* Delete current line				*/
		unsigned		fChangePath :1;		/* Change path on this line		*/
		unsigned 	fNoCopy		:1;		/* Already copied to szNewString */
	}f;


	*((unsigned *)(&f)) = 0;
	szPtr = ParseFileName( szStr );
	ExtractNextWord( szPtr, szDevName, 25 );

	for ( i = 0; i < LAST_DEVICE; i++ )
		if ( strnicmp( szDevName, Devices[i], (size_t)DeviceNameLen[i] ) == OK )
			break;

	switch( i )
	{
		case	HIMEMSYS:						/* Process "DEVICE=HIMEM.SYS" line		*/
		case	HIMEMDOS:						/* M006: Process "DEVICE=HIMEM.DOS" line	*/
			if ( DevFlags.fHimem != TRUE )
			{
				DevFlags.fHimem = TRUE;
				if ( !vInfo.NoCopy[ NO_HIMEM ] )
				{
						/* Need to create a new name in case was HIMEM.DOS. */
					memcpy( szPtr, HIMEM_SYS, strlen(szDevName) );

					f.fChangePath = TRUE;
				}
			}
			else
				f.fDelete = TRUE;

			break;

		case	SMARTDRV:					/* Process "DEVICE=SMARTDRV.SYS" line	*/
			if ( !vInfo.NoCopy[ NO_SMARTDRV ] )
			{
				if ( DevFlags.fSmartDrv )			/* Check for duplicate entry	*/
					f.fDelete = TRUE;
				else if ( !DevFlags.fHimem && 	/* If XMA not installed			*/
							 FindParam( szPtr, 'A' ) == FALSE )	/* Using EMM mem?	*/
				{
					DevFlags.fNeedHiMem = TRUE;				/* If not need himem */
					MoveToEndFile( szPtr, SMARTDRV );
					f.fDelete = TRUE;
				}
				else
					f.fChangePath = TRUE;
				DevFlags.fSmartDrv = TRUE;
			}
			break;

		case	RAMDRIVE:					/* Process "DEVICE=RAMDRIVE.SYS" line	*/
			if ( !vInfo.NoCopy[ NO_RAMDRV ] )
			{
				if ( !DevFlags.fHimem && FindParam( szPtr, 'E' ) == TRUE )
				{
					MoveToEndFile( szPtr, RAMDRIVE + RamDriveCount );
					f.fDelete = TRUE;
					DevFlags.fNeedHiMem = TRUE;
					RamDriveCount++;
				}
				else
					f.fChangePath = TRUE;
			}
			break;

		case	EMM386SYS:						/* Process "DEVICE=EMM386.SYS" line       */
		case	EMM386DOS:						/* M006: Process "DEVICE=EMM386.DOS" line */
		case	EMM386EXE:						/* Process "DEVICE=EMM386.EXE" line       */
			if ( vInfo.NoCopy[ NO_EMM386 ] )
				break;
			else if ( DevFlags.fEmm386 )			/* Check for duplicate entries	*/
				f.fDelete = TRUE;
			else
			{										/* See if need to install himem driver */
				if ( !DevFlags.fHimem )
				{
					MoveToEndFile( szPtr, EMM386EXE );
					f.fDelete = TRUE;
					DevFlags.fNeedHiMem = TRUE;

                                	/* M006: Create new name in case
                                           was EMM386.SYS or EMM386.DOS. */
					memcpy( DeviceLines[EMM386EXE],
                                		EMM386_EXE,
                                		strlen(szDevName) );
				}
                                else
                                {
					/* M006: Create new name in case
                                    	   was EMM386.SYS or EMM386.DOS. */
					memcpy( szPtr,
                                        	EMM386_EXE,
                                                strlen(szDevName) );
                                }

				f.fChangePath = TRUE;
				DevFlags.fEmm386 = TRUE;
			}
			break;

#ifdef ADD_MOUSE
												/* Process "DEVICE=MOUSE.SYS" line		*/
		case	MOUSE:
			if ( vInfo.Flag.fMouse ) 	/* If adding new mouse driver need to	*/
				f.fDelete = TRUE;			/* REM out this line							*/
			break;
#endif

		case	EGASYS:
			f.fChangePath = TRUE;
			DevFlags.fHaveEga = TRUE;
 			break;

		case	SHARE:
			f.fChangePath = TRUE;
			break;

		case	SETVER:
			return( DEL_LINE );						/* Just don't copy the line	*/
			
		default: 													/* No match found 	*/
			if ( iType == DEVICE )
			{
				if ( FindDataMatch( szDevName, REM_DEVICE ) ||
					  FindDataMatch( szDevName, DELETE_DRIVER )	)
					f.fDelete = TRUE;

				else if ( IsDistrFile( szDevName ) )
					f.fChangePath = TRUE;
			}
			else if ( iType == INSTALL || iType == LOAD_HIGH )
				if ( IsDistrFile( szDevName ) )
					f.fChangePath = TRUE;
			break;
	}																  /* End switch			*/

	if ( f.fChangePath == TRUE && !f.fNoCopy )
		CreateDeviceLine( szNewString, szPtr, iType );

	else if ( !f.fDelete )
		strcpy( szNewString, szOldString );

	return( f.fDelete ? REM_LINE : OK );
}

/***************************************************************************/
/* Function to add a device line to the new config.sys file.					*/
/* 																								*/
/* int AddNewDevice( char *szDevName, int iType	)									*/
/* 																								*/
/* Arguments:	szDevName	- Ptr to the device name and any parameters		*/
/* 				iType 	- Specifies if should be DEVICE= or INSTALL= 		*/
/* RETURNS: 	int			- Status of fputs() function							*/
/* 																								*/
/***************************************************************************/

static int near AddNewDevice( char *szDevName, int iType	)
{
	char			*szOldString;
	register 	iStatus;

	szOldString = GetMemory( MAX_LEN * 2 );

	CreateDeviceLine( szOldString, szDevName, iType );
	iStatus = NewFilePuts( szOldString, NewFile );

	FreeMemory( szOldString );

	return( iStatus );
}

/***************************************************************************/
/* Creates a device string for the config.sys file in the form of:			*/
/* "DEVICE=X:\DOS_PATH\DEVICE /x/x/x"													*/
/* The drive letter is derived from vInfo.chDestin and the path from 		*/
/* vInfo.szPath.																				*/
/* 																								*/
/* void CreateDeviceLine( char *szLine, char *szDevice, int iType	)			*/
/* 																								*/
/* ARGUMENTS:	szLine	- Ptr to buffer to hold the new string 				*/
/* 				szDevice - The name of the device with any needed params 	*/
/* 				iType 	- Specifies if should be DEVICE= or INSTALL= 		*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void CreateDeviceLine( char *szLine, char *szDevString, int iType	)
{
	char			*szDevicePath;

	if ( iType == LOAD_HIGH )
		strcpy( szLine, szLoadHi );
	else
		strcpy( szLine, iType == DEVICE ? szDevice : szInstall );

	szDevicePath = strchr( szLine, EOL );
	BuildPath( szDevicePath, HD_BOOT_DRV, vInfo.szPath + 2, szDevString );
}

/***************************************************************************/
/* Allocates a buffer for and copies a device name and parameters to the	*/
/* specifed place in the DeviceLines array which holds ptrs to devices to	*/
/* add to the end of the new config.sys file.										*/
/* 																								*/
/* void MoveToEndFile( char szStr, int iDevice )									*/
/* 																								*/
/* ARGUMENTS:	szStr 	-	Ptr to a device name and any needed	parameters	*/
/* 				iDevice	-	An enumerated device descriptor for specifing	*/
/* 								an element in the DeviceLines[] array				*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

static void near MoveToEndFile( char *szStr, int iDevice )
{
	DeviceLines[ iDevice ] = GetMemory( MAX_LEN + 10 );
	strcpy( DeviceLines[ iDevice ], szStr );
}

/***************************************************************************/
/* Reads a line from the specified open 'C' file handle and then parses 	*/
/* off any trailing carriage return or newline characters. The function 	*/
/* has the same calling arguments and returns the same thing as a fgets().	*/
/* 																								*/
/* char *NewFileGets( char *szBuffer, int Count, FILE *File )					*/
/* 																								*/
/* ARGUMENTS:	szBuffer - Buffer to read the line into							*/
/* 				iCount	- Max character to read in 								*/
/* 				File		- Ptr to open 'C' file structure							*/
/* RETURNS: 	char *	- Ptr to szBuffer or NULL if error or end of file	*/
/* 																								*/
/***************************************************************************/

char *NewFileGets( char *szBuffer, int Count, FILE *File )
{
	char	*szPtr;

	if ( (szBuffer = fgets( szBuffer, Count, File )) != NULL )
	{
		if ( (szPtr = strchr( szBuffer, '\n' )) != NULL )
			*szPtr = EOL;
		if ( (szPtr = strchr( szBuffer, '\r' )) != NULL )
			*szPtr = EOL;
	}

	return( szBuffer );
}

/***************************************************************************/
/* Appends a newline character to a string and then writes the string to	*/
/* an open file.																				*/
/* 																								*/
/* int NewFilePuts( char *szString, FILE *File )									*/
/* 																								*/
/* ARGUMENTS:	szString - Ptr to string to write to file 						*/
/* 				File		- Ptr to open 'C' file structure							*/
/* RETURNS: 	int		- OK if sucessfull else non-zero value 				*/
/* 																								*/
/***************************************************************************/

int NewFilePuts( char *szString, FILE *File )
{
	strcat( szString, "\n" );
	return( fputs( szString, File ) );
}

/***************************************************************************/
/* Checks the dos data and adds all lines from the [add-dev] class to the	*/
/* current location in the open new	config.sys file.								*/
/* 																								*/
/* static int AddDevice( void )															*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	int			- File write status from fputs() 					*/
/* 																								*/
/***************************************************************************/

static int near InstallNewDrivers( void )
{
	char			*szNewStr;
	register 	i;
	register 	iStatus;

	for ( i = 0, iStatus = OK;
			iStatus == OK &&
			(szNewStr = GetDataString( ADD_DEVICE, i )) != NULL;
			i++ )
		iStatus = AddNewDevice( szNewStr, DEVICE );

	if ( iStatus == OK )
		iStatus = AddDrvParm();

	return( iStatus );
}

/***************************************************************************/

static int near AddDrvParm()
{
	char			*szDrivparm;
	char			*szDataStr;
	int			iStatus;
	UCHAR 		uchDrivparmDrv1;
	UCHAR 		uchDrivparmDrv2;

	iStatus = OK;
	szDrivparm = GetMemory( MAX_LEN + 10 );
	szDataStr = GetDataString( DRIV_PARM, 0 );
	if ( szDataStr	!= NULL )
	{
		find_drive( &uchDrivparmDrv1, &uchDrivparmDrv2 );
		strncpy( szDrivparm, szDataStr, MAX_LEN + 9 );

		if ( uchDrivparmDrv1 != 0xff )
			iStatus = add_driv_line( szDrivparm, uchDrivparmDrv1 );

		if ( iStatus == OK && uchDrivparmDrv2 != 0xff )
			iStatus = add_driv_line( szDrivparm, uchDrivparmDrv2 );
	}

	FreeMemory( szDrivparm );
	return( iStatus );
}

/***************************************************************************/

static int near add_driv_line( char *szDrivparm, UCHAR uchDrv )
{
	char		szNum[4];

	itoa( uchDrv, szNum,10 );						/* Create drive number string */

	strcat( szDrivparm, " /d:" );
	strcat( szDrivparm, szNum	);

	return( NewFilePuts( szDrivparm, NewFile ) );
}

/***************************************************************************/

void near find_drive( UCHAR *puchDrv1, UCHAR *puchDrv2 )
{
	UCHAR 		chByte;

	*puchDrv1 = *puchDrv2 = 0xff;
											/*Get TANDY id byte at 0xf000:0xc000*/

	chByte = (UCHAR) *( (UCHAR far *) ((0xf000L << 16L) | 0xc000L) );

	if (chByte == 0x21)
	{
			/* At 40:B5 - bit 0 = 1 => drive A is 720K
							  - bit 1 = 1 => drive B is 720K */

		chByte = (UCHAR) *( (UCHAR far *) ((0x0040L << 16) | 0x00b5L) );

		if ( (chByte >>= 1) != 0 )
			*puchDrv1 = 0;

		if ( (chByte >> 1) != 0 )
			*puchDrv2 = 1;
	}
}

