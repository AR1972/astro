/***************************************************************************/
/*																									*/
/*	AUTOCONF.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Functions for creating new config.sys and autoexec.bat files for the		*/
/* DOS 5.0 retail nstallation program.													*/
/*																									*/
/* Created 02-28-90	- johnhe																*/
/***************************************************************************/

#include 	<stdio.h>
#include 	<stdlib.h>
#include 	<memory.h>
#include 	<string.h>
#include		<dos.h>
#include		<io.h>
#include 	<malloc.h>

#include 	<alias.h>
#include 	<global.h>
#include 	<disk_io.h>
#include		<file_io.h>
#include 	<strlib.h>
#include		<upgrade.h>
#include		<prompts.h>

/***************************************************************************/

#define		FILE_BUF_SIZE		2000		/* Size of file buffer in bytes		*/

/***************************************************************************/

static char	*apszFile[] = { "X:\\AUTOEXEC.BAT", "X:\\CONFIG.SYS" };
static char	*apszOld[]	= { "X:\\AUTOEXEC.OLD", "X:\\CONFIG.OLD" };

static char	*szBuf;									/* Buffer to build file in		*/
static char *szEndLine = "\r\n"; 				/* End of line characters		*/
static char	chBootDrv;

/***************************************************************************/

extern void AutoConfig		( void );
extern int	NetUpgradeStr	( char *szBuf );

static unsigned CreatAutoexec( void );
static unsigned CreatConfig( void );

/***************************************************************************/
/* Main function for creating autoexec.bat and config.sys files. First 		*/
/* renames an existing file of the same name to *.OLD and then call a		*/
/* function for each of the files.														*/
/*																									*/
/*	void NewAutoConfig( int iType, char *szFile )									*/
/*																									*/
/*	ARGUMENTS:	iType	- Type of file to create 0 == AUTOEXEC, 1 = CONFIG		*/
/*					szFile - Full path name for the new file							*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void NewAutoConfig( int iType, char *szFile )
{
//	char					*apszError[ ERROR_LINES ];
	register				iStatus;
	int					iFile;
	unsigned				uToWrite;
	unsigned				uWritten;


#ifdef	NEED_FIX
	static unsigned	Errors[] = { AUTOEXEC_BAT_ERROR, CONFIG_SYS_ERROR };
#endif

	static unsigned	(*Func[])(void) =	{ CreatAutoexec, CreatConfig };
							
	chBootDrv = (vInfo.Flag.fHardInstall ? (char)'C' : (char)'A');

	szBuf = GetMemory( FILE_BUF_SIZE );

	apszOld[iType][0] = chBootDrv;

											/* There may be an old files so delete it	*/
	AbsUnlink( szFile );
	uToWrite = (*Func[iType])();				/* Create the text for the file	*/

														/* Next create the file				*/
	if ( uToWrite > 0 )
	{
		DisplayFileStatus( apszFile[iType]+ 3, WRITE );
		iStatus = ERROR;
		if ( _dos_creat( szFile, 0, &iFile ) == OK )
		{
			if ( _dos_write( iFile, szBuf, uToWrite, &uWritten )	== OK &&
				  uWritten == uToWrite )
				iStatus = OK;

				iStatus |= (int)_dos_close( iFile );
		}
		if ( iStatus != OK )
		{
#ifdef	NEED_FIX
apszError is not being used anwhere so why was this done?

			GetMessage( apszError, Errors[iType] );
			Error( apszError );
#endif
		}
	}

	FreeMemory( szBuf );
}

/***************************************************************************/
/* Creates a new CONFIG.SYS file in the file buffer. First checks for		*/
/* extended memory and if available forces DOS to load into the high 		*/
/* memory area and then checks to see if SMARTDRV.SYS should be installed	*/
/* and adds the line if installing. If SMARTDRV is installed the number of */
/* buffers are	reduced from the normal default that DOS would set. Next a	*/
/* check is done to see if a COUNTRY line should be added.						*/
/* 																								*/
/* unsigned CreatConfig( void )															*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	unsigned - Number of bytes in the new file.						*/
/* 							  NOTE: this number may be zero which will signal	*/
/* 									  that no CONFIG.SYS file will be created.	*/
/* 																								*/
/***************************************************************************/

unsigned CreatConfig( void )
{
	char				*szPtr;
	extern char		*szShell;


	szPtr = szBuf;
	*szBuf = EOL;

	strcat( szPtr, DEVICE_STR );                    /* "DEVICE=SETVER.EXE"	*/
   szPtr = strchr( szPtr, EOL );
   BuildPath( szPtr, chBootDrv, vInfo.szPath + 2, SETVER_STR );
   strcat( szPtr, szEndLine );

							/* If extended memory is available put DOS in himem.  */
	if ( vInfo.NoCopy[ NO_HIMEM ] == FALSE &&
		  vInfo.Hw.ExtMem >= HIMEM_K_SIZE )
	{
		strcat( szBuf, DEVICE_STR );							/* "DEVICE=HIMEM		*/
		szPtr = strchr( szBuf, EOL );
		BuildPath( szPtr, chBootDrv, vInfo.szPath + 2, HIMEM_STR );
		strcat( szPtr, XMA_STR );								/* "DOS=HIGH\r\n"		*/
	}

	strcat( szBuf, FILES_STR );								/* FILES= line 		*/

	if ( vInfo.Hw.VideoType == VIDEO_EGA )
	{
		strcat( szBuf, DEVICE_STR );							/* "DEVICE=EGA.SYS	*/
		szPtr = strchr( szBuf, EOL );
		BuildPath( szPtr, chBootDrv, vInfo.szPath + 2, EGA_STR );
	}

	if ( vInfo.Flag.fHardInstall )
	{
		szPtr = DefaultShellLine( strchr( szBuf, EOL ) );
		strcat( szPtr, "\r\n" );
	}

	#ifdef	STACKS_STR
		strcat( szBuf, STACKS_STR );							/* STACKS= line		*/
	#endif

	szPtr = strchr( szBuf, EOL );
	return( (UINT)(szPtr - szBuf) );
}

/***************************************************************************/
/* Creates an new AUTOEXEC.BAT file in the file buffer. First adds all of	*/
/* the standard items which are used on all systems and then based on the	*/
/* user options may add the keyboard driver, mouse or dos shell.				*/
/* 																								*/
/* unsigned CreatAutoexec( void )														*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	unsigned - Then number of bytes in the new file.				*/
/* 																								*/
/***************************************************************************/

unsigned CreatAutoexec( void )
{
	char			*szPtr;

	*szBuf = EOL;									/* Needed for strcat to work		*/

#ifdef NET_SUPPORT														
	if ( NetUpgradeStr( szBuf ) )				/* Add the net setup string		*/
		strcat( szBuf, szEndLine );			/* Add the CR LF characters		*/
#endif

	strcat( szBuf, AUTO_STR ); 				/* Add standard autoexec items	*/
														/* Append the dos path string 	*/
	if ( vInfo.Flag.fHardInstall )
	{
		strcat( szBuf, PATH_STR );
		szPtr = strchr( szBuf, EOL );
		BuildPath( szPtr, chBootDrv, vInfo.szPath + 2, "" );
		RemoveTrailing( szPtr + 3, '\\' );	/* Remove trailing seperater		*/
		strcat( szPtr, szEndLine );			/* Add the CR LF characters		*/

		strcat( szBuf, TEMP_STR );
		szPtr = strchr( szBuf, EOL );
		BuildPath( szPtr, chBootDrv, vInfo.szPath + 2, "" );
		RemoveTrailing( szPtr + 3, '\\' );	/* Remove trailing seperater		*/
		strcat( szPtr, szEndLine );			/* Add the CR LF characters		*/
	}

#ifdef	ADD_MOUSE
	if ( vInfo.Flag.fMouse )
	{
		if ( !vInfo.ToFloppy )
		{
			szPtr = strchr( szBuf, EOL );
			BuildPath( szPtr, chBootDrv, vInfo.szPath + 2, MOUSE_STR );
		}
		else
			strcat( szBuf, MOUSE_STR );
	}
#endif
																/* See if adding dos shell */
	if ( vInfo.Flag.fShell && vInfo.Flag.fHardInstall )
	{
		szPtr = strchr( szBuf, EOL );
		strcpy( szPtr, SHELL_STR );
	}

	szPtr = strchr( szBuf, EOL );
	return( (UINT)(szPtr - szBuf) );
}

#ifdef NET_SUPPORT
/***************************************************************************/
/* Creates a string to launch the net upgrade program from the autoexec		*/
/* file. 																						*/
/*																									*/
/*	int NetUpgradeStr( char *szStr )														*/
/*																									*/
/*	ARGUMENTS:	szStr	- Ptr to buffer to hold the string.							*/
/* RETURNS:		int	- TRUE if a string was created, FALSE if the net		*/
/*							  upgrade program doesn't need to be run.					*/
/***************************************************************************/

int NetUpgradeStr( char *szStr )
{
	if ( vInfo.Flag.fHardInstall )
	{
		BuildPath( szStr, vInfo.szPath[0], vInfo.szPath + 2, NET_UPGRADE );
		strcat( szStr, " " );

		if ( vInfo.chSource > vInfo.chFirstHd )	/* Upgrading from net?	 	*/
		{
			strcat( szStr, vInfo.szNetDir );
			RemoveTrailing( szStr, '\\' );
		}
		else
			strcat( szStr, vInfo.szSource );

		strcat( szStr, " /UPGRADE" );
		if ( vInfo.Args.fIsMono )
			strcat( szStr, " /B" );
		return( TRUE );
	}		
	else
		return( FALSE );
}

#endif

/***************************************************************************/
/* Creates a default SHELL= line for a config.sys file using the path to	*/
/* the DOS directory. The function assumes the drive is the boot hard disk	*/
/* C: since there is no need to call this function for a floppy upgrade.	*/
/*																									*/
/*	void DefaultShellLine( char *szPtr )												*/
/*																									*/
/*	ARGUMENTS:	szPtr		- Ptr to buffer where shell= line to be created		*/
/*	RETURNS:		char *	- Ptr to end of line which was created					*/
/*																									*/
/***************************************************************************/

char *DefaultShellLine( char *szPtr )
{
	extern char		*szCommandShell;
	extern char		*szShell;

	strcpy( szPtr, szShell );
	BuildPath( strchr( szPtr, EOL ), 'C', vInfo.szPath + 2, szCommandShell );
	BuildPath( strchr( szPtr, EOL ), 'C', vInfo.szPath + 2, " " );

	strcat( szPtr, " /p" );
	return( strchr( szPtr, EOL ) );
}
