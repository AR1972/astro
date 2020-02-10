//=========================================================================
// 																								
// UPGINIT.C																					   
// 																								
//		Copyright (c) 1992 - Microsoft Corp.											
//		All rights reserved.																	
//		Microsoft Confidential																
//
//	Contains routines that initiliazes all the info for the upgrade process.
//
//  	Created 01-28-92
//
//=========================================================================

#include <stdio.h>
#include <stdlib.h>
#include <dos.h>
#include <string.h>
#include <memory.h>
#include	<io.h>
#include	<fcntl.h>
#include	<direct.h>


#include <alias.h>
#include <install.h>
#include	<data.h>
#include <disk_io.h>
#include <global.h>
#include	<strlib.h>
#include	<file_io.h>
#include <copy.h>
#include <upgrade.h>
#include <errors.h>
#include "prompts.h"

typedef int BOOL;
#define		_A_FINDALL				(_A_HIDDEN | _A_SYSTEM | _A_RDONLY)
#define		DEVICE					0


INFO 		vInfo;
BOOL 		fMono;
char		szDataPath[ MAX_PATH_LEN ];
char		szTmpDos[ MAX_PATH_LEN ];

char		szWinAuto[ 25 ];				/* "X:\autoexec.xxx"	*/
char		szWinConf[ 25 ];				/* "X:\config.xxx"	*/
char		*szTmpConfigPath;
char		*szTmpAutoPath;
void far *lpInt13Buf;         	/* declared/inited here. Used in b_disk.asm */
void far *lpfnInt13Func;         /* declared/inited here. Used in b_disk.asm */

INSTALL far	*lpInstall = NULL;	/* Address of CUI Dos Setup resident data. */

extern char		*szAutoBat;   /* declared in upgrade.c */
extern char		*szConfSys;   /* declared in upgrade.c */

char far	*DiskChangePtr;
unsigned char	gInstallMode;

extern void fmemcpy( char far *Str1, char far *Str2, UINT Len );
extern void	InitFileStruc	( struct MULT_FILES *FileStruc, int iDisk,
			  					  	  int IsHdUpgrade );
extern void	CreateDeviceLine( char *szLine, char *szDevString, int iType );
extern void	FatalError( int ErrorType );

extern char far *GetInfo(void);


/***************************************************************************/
/*	Function to initialize the DOS part of the DOS/WINDOWS combined setup.	*/
/*																									*/
/*	void InitDosUpgrade( void )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		FALSE if DosSetup already done (NON-UPGRADE case)  			*/
/*				   TRUE  otherwise.
/*																									*/
/***************************************************************************/

BOOL InitDosUpgrade()
{

	struct DIR			DirEntry;
	struct find_t		FileInfo;

	char far * InfoPtr;

	if ( !(InfoPtr = GetInfo()) )
				FatalError( FATAL_MEMORY_ERROR );

	if ( _fstrcmp(((INSTALL far *)InfoPtr)->szSignature, SIGNATURE) != 0 )
		return FALSE;
	else
		lpInstall = (INSTALL far *) InfoPtr;

	if ( lpInstall->Done.fDosSetup == TRUE )
		return FALSE;

	DiskChangePtr = (char far *)(InfoPtr + sizeof(INSTALL) + sizeof(vInfo));

	gInstallMode = lpInstall->Method;

	fmemcpy( (char far *)&vInfo, 
				(char far *)(InfoPtr + sizeof(INSTALL)),
				sizeof(vInfo) );

	fMono = vInfo.Args.fIsMono ? TRUE : FALSE;

	BuildPath( szDataPath, vInfo.szSource[0], vInfo.szSource + 2,
				  DOS_DATA_FILE );

	InitDosData( szDataPath );		/* Read in and initialize data file */

	LoadOemData( (struct DDR *)&vInfo.ddrRestore );

	SetFileTimeDate( &DirEntry );
																	/*lint  -e740 */
	vInfo.ulTmpSerial = *( (UL *)&DirEntry.Time );	/*lint  +e740 */
	vInfo.ulFinalSerial = vInfo.ulTmpSerial + 1;

//	pchBootBuf = GetMemory( (unsigned)((int)vInfo.uchBootRecs[0][0]) * 512);
	pchBootBuf = GetMemory( (unsigned)((int)vInfo.uchBootRecs[0]) * 512);

	pchMbrBuf = GetMemory( SECTOR_SIZE * 4 );

	pchOldPartRec = GetMemory( SECTOR_SIZE );

	*szAutoBat = *szConfSys = vInfo.chDestin;

   /* Init real-mode Int 13h pointers from vInfo structure:
	 *	
	 *	lpInt13Buf    -> 512 byte buffer.
	 *	lpfnInt13Func -> Function which performs Int 13h. (follows Int 13h buffer)
	 */
	lpInt13Buf    = vInfo.lpfnInt13;
   lpfnInt13Func = (void far *) (((char far *) vInfo.lpfnInt13) + 512);

	if ( _dos_findfirst( szAutoBat, _A_FINDALL, &FileInfo ) == OK )
		vInfo.Flag.fAutoexecBat = TRUE;
	if ( _dos_findfirst( szConfSys, _A_FINDALL, &FileInfo ) == OK )
		vInfo.Flag.fConfigSys = TRUE;


	szTmpConfigPath = vInfo.szTmpConfig;	/* Give windows code access	*/
	szTmpAutoPath = vInfo.szTmpAuto;			/* to these files.				*/

	return TRUE;

}

/***************************************************************************/
/* Function that determines number of DOS files to copy							*/
/*																									*/
/*	int far CopyDosFiles( void )															*/
/*						  																		   */
/* ARGUMENTS: NONE																			*/
/*	RETURNS:		int		-	Number of files to copy									*/
/*																									*/
/***************************************************************************/

int CopyDosFiles( void )
{
	#define			BUF_LEN (sizeof( struct MULT_FILES ) * MAX_PER_DISK)

	char					**apszNames;
	register				iCount;
	register				iDisk;
	struct MULT_FILES *Files;
	struct MULT_FILES *TmpPtr;

	extern int	   XDosCopy;				/* Defined in Windows setup copy.c	*/
	extern char		szDosDesName[];		/* Defined in Windows setup copy.c	*/

	Files = GetMemory( BUF_LEN );
	apszNames = GetMemory( MAX_DOS_FILES * sizeof( char * ) );
	memset( apszNames, 0, MAX_DOS_FILES * sizeof( char * ) );

	XDosCopy = TRUE;

	if ( lpInstall->Flags.fUninstall )
		iDisk = 0;
	else
		iDisk = FIRST_USER_DISK;			/* Skip Uninstall files */

	for ( iCount = 0;							/* Outer loop	*/
			iDisk < (int)vInfo.uchNumDisks;
			iDisk++ )
	{
		memset( Files, 0, BUF_LEN );
		InitFileStruc( Files, iDisk, TRUE );

		for ( TmpPtr = Files;									/* Inner loop	*/
				TmpPtr->Name.Source != NULL;
				TmpPtr++ )
		{	
											/* First see if file is a duplicate	*/
			if ( StrSearch( TmpPtr->Name.Source, apszNames ) != -1 )
				continue;
			apszNames[ iCount++ ] = TmpPtr->Name.Source;	/* Add new name	*/
		}
	}
   /*  Increment the file count appropiatly if we have to upgrade DMDriver
    *  or speedstor drivers. The code below assumes that these drivers are
    *  mutualy exclusive.
    */
   if ( vInfo.Flag.fDMdriver )   // MC 4/13/92 #1
      iCount += 2;               // MC 4/13/92 #1
   if ( vInfo.Flag.fSpeedstor )  // MC 4/13/92 #1
      iCount++;                  // MC 4/13/92 #1

	FreeMemory( apszNames );
	FreeMemory( Files );

	XDosCopy = FALSE;
	return( iCount );
}

/***************************************************************************/
/* Creates a unique file name based on the full pathname passed as an		*/
/* argument to the function by appending an ascii number as an extension.	*/
/* 																								*/
/*																									*/
/*	int CreateUniqueName( szBuf, szFile )												*/
/*																									*/
/*	ARGUMENTS:	szBuf		- Buffer to copy new file name to or NULL				*/
/*					szFile	- Full path and file name to be renamed				*/
/*	RETURNS:		int		- OK if successfull else ERROR							*/
/*																									*/
/***************************************************************************/

int far CreateUniqueName( char *szBuf, char *szFile )
{
	register			i;
	char				*szExt;
	char				*szTmp;
	char				szNew[ MAX_PATH_LEN ];
	struct find_t	Info;

	strcpy( szNew, szFile );
	szExt = ParseFileName( szNew );

	if ( (szTmp = strchr( szExt, '.' )) != NULL )
		szExt = szTmp + 1;
	else
		szExt = strchr( szExt, EOL );

	for ( i = 1; i < 999; i++ )
	{
		itoa( i, szExt, 10 );
		if ( _dos_findfirst( szNew, _A_FINDALL, &Info  ) != OK )
		{
			if ( szBuf != NULL )
				strcpy( szBuf, szNew );
			return( OK );
		}
	}
	return( ERROR );
}

/***************************************************************************/
/* Far memory copy function copies memory from one location to another.		*/
/*																									*/
/*	void fmemcpy( char far *Str1, char far *Str2, UINT Len )						*/
/*																									*/
/*	ARGUMENTS:	Str1	- Far ptr to destination  memory location 				*/
/*					Str2	- Far ptr to source memory location							*/
/*					Len	- Number of bytes to move										*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void fmemcpy( char far *Str1, char far *Str2, UINT Len )
{
	while( Len-- )
		*(Str1++) = *(Str2++);
}

/***************************************************************************/
/* Entry point for Windows to do the main portition of the DOS upgrade.		*/
/*																									*/
/*	void far wsUpgradeHard( void )														*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void far wsUpgradeHard( void )
{
//BUGBUG	ToggleTsrs( OFF );								/* Turn ASSIGN & APPEND off	*/
	UpgradeHard( 0 );
}

/***************************************************************************/
/* Interface function which creates a DEVICE= line in the callers buffer	*/
/* using the DOS directory path.															*/
/*																									*/
/*	void far wsBuildConfigLine( char *szBuf, char *szDeviceName )				*/
/*																									*/
/*	ARGUMENTS:	szBuf		- Buffer big enought to hold the new line				*/
/*					szDev		- Device drive file name									*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void far wsBuildConfigLine( char *szBuf, char *szDeviceName )
{
	CreateDeviceLine( szBuf, szDeviceName, DEVICE );
}



