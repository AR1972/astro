/***************************************************************************/
/* 																								*/
/* AUTO.C																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Processes the autoexec.bat file in the root directory or creates one 	*/
/* if it doesn't already exist.                                            */
/* 																								*/
/* NOTE: 																						*/
/* 		In order to correctly process the mouse driver the config.sys		*/
/* 		file processing must be done before calling ProcessAutoexec(). 	*/
/* 		This will insure that any existing mouse.sys driver in the			*/
/* 		config.sys file has been identified and the vInfo.fMouse flag		*/
/* 		has been set to signal adding mouse to autoexec.bat.					*/
/* 																								*/
/* 12-10-89 johnhe																			*/
/***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include	<alias.h>
#include	<data.h>
#include <disk_io.h>
#include	<file_io.h>
#include <global.h>
#include <strlib.h>
#include	<errors.h>
#include	<prompts.h>

/***************************************************************************/

#define		MAX_FNAME_LEN	12			/* m104	*/
#define		FILE_EXT_LEN	3			/* m104	*/

#define		MAX_LEN		256			/* Max length of line in autoexec.bat	*/
#define		STR_BUF_LEN	128
#define		MAX_DEFAULT 10 			/* Max # of default autoexe.bat lines	*/
#define		MAX_TO_ADD	4				/* Max lines added to existing file 	*/
#define		MOUSE 		1				/* Offset of "MOUSE" in apszFiles[] 	*/

enum			CmdStrs		{ CMD_PATH, CMD_SET, CMD_DOSSHELL, CMD_MOUSE };
enum			SetStrs		{ SET_PATH, SET_COMSPEC };

struct AUTO_LINE_FLAGS
{
	unsigned 	fHasPath :1;			/* Cmd has a path prefixed to it 		*/
	unsigned 	fRemOut	:1;			/* Prefix line with a REM cmd 			*/
	unsigned 	fCopyOld :1;			/* strcat the old line to szNewLine 	*/
	unsigned 	fHasExt	:1;			/* Cmd had an extension on it 			*/
	unsigned		fDelLine	:1;			/* Delete the line from the file			*/
}ThisLine;

/***************************************************************************/

extern struct BITS
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
} DevFlags;


extern FILE		*OldFile;			/* File handle for existing autoexec	*/
extern FILE		*NewFile;			/* File handle for new autoexec			*/

extern char 	*szOldString;		/* Original line from file 				*/
extern char 	*szNewString;		/* Processed line to add to file 		*/
extern char 	*szAutoBat; 		/* Full path & name for old autoexe.bat*/
extern char 	*szConfSys;			/* Full path & name for old config.sys */

static char 	*apszAlways[] = { "@ECHO OFF", "PROMPT $p$g" };
static char 	*apszFiles[]  = { NULL, "MOUSE ", "DOSKEY", "DOSSHELL" };
static char 	*apszDefault[ MAX_DEFAULT ];	/* Ptrs to default strs 	*/

static char 	szPath[STR_BUF_LEN] = "PATH ";	/* Default path cmd string */
static int		iFlags[ MAX_TO_ADD ];			/* Flag for user options	*/


static char		*apszCmdStrs[] = { "PATH", "SET", "DOSSHELL", "MOUSE", NULL };
static char 	*szPathCmd		=	"PATH=";
static char 	*szComspec		=	"COMSPEC=";


#define			PATH_STRING 	2	/* Indice to "PATH"	in apszDefault[]	*/
#define			PATH_CMD_LEN	5					/* strlen( "PATH=" ) 		*/
#define			COMSPEC_LEN 	8					/* strlen( "COMSPEC=" ) 	*/


#define			REM_LINE			1
#define			DEL_LINE			2

/***************************************************************************/

extern void	ProcessFile		( int iType, char *szOldFile, char *szNewFile,
									  int (*Funcs[])( int iStatus ) );

extern int	NewFilePuts		( char *szString, FILE *File );

extern void MoveAutoConfig( void );
extern void	ProcessAuto		( void );
extern void ProcessConfig	( void );
extern int	NetUpgradeStr	( char *szBuf );

static int	NewAutoFirst		( int iStatus );
static int	NewAutoLast		( int iStatus );
static int	ProcessAutoStr	( int iStatus );

static void	near BuildDefaultAuto( void );
static void near CreateEnvPath	( char *szPtr );
static int	near IsDosFile			( char *szName );
static int	near UniqueRename		( char *szFile, char *szNew );


/***************************************************************************/
/* Renames the tmp autoexec.bat and config.sys files into the root dir. 	*/
/* A check is done first to see if the tmp file exists before deleting the */
/* original just in case we need to continue if there is a system crash 	*/
/* and this is the second try and one of the files was already successfully*/
/* moved.																						*/
/* 																								*/
/* void MoveAutoConfig( void )															*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void MoveAutoConfig( void )
{
	char		*szNewAutoStr = "X:\\AUTOEXEC.NEW";			/* M002 */
	char		*szNewConfStr = "X:\\CONFIG.NEW";			/* M002 */
	char		*szAutoNew;											/* M002 */
	char		*szConfNew;											/* M002 */

	if ( vInfo.Args.fMininum )
	{
		szAutoNew = szNewAutoStr;
		szConfNew = szNewConfStr;		
		*szAutoNew = *szConfNew = vInfo.chDestin;
	}
	else
		szAutoNew = szAutoBat,	szConfNew = szConfSys;

	if ( (vInfo.Flag.fConfigSys && vInfo.Flag.fNewSysFiles &&
			UniqueRename( szConfSys, vInfo.szRenConfig ) != OK) ||
		  ReplaceFile( vInfo.szTmpConfig, szConfNew ) != OK )
		ProcessCopyError( vInfo.szTmpConfig, ERR_RENAMING );

 	if ( (vInfo.Flag.fAutoexecBat && vInfo.Flag.fNewSysFiles &&
		   UniqueRename( szAutoBat, vInfo.szRenAuto ) != OK) ||
		  ReplaceFile( vInfo.szTmpAuto, szAutoNew ) != OK )
		ProcessCopyError( vInfo.szTmpAuto, ERR_RENAMING );

	if ( vInfo.Args.fMininum )									/* M002 */
	{
		AbsUnlink( szAutoBat );
		AbsUnlink( szConfSys );
	}
}

/***************************************************************************/
/* Builds a list of default strings for a new autoexec.bat file and the 	*/
/* calls ProcessFile() to create a new autoexec.bat in the tmp directory	*/
/* using processed lines from the original if it exists or else the			*/
/* list of default strings if it doesn't exist.                            */
/* 																								*/
/* void ProcessAuto( void )																*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void ProcessAuto( void )
{
	char			*Buffer;
	register 	i;
	static int	(*Funcs[])(int) = { NewAutoFirst, ProcessAutoStr, NewAutoLast };

										/* Allocate memory for and assign ptrs to 	*/
										/* buffers for default autoexec file strings */

	Buffer = GetMemory( STR_BUF_LEN * MAX_DEFAULT );
	for ( i = 0; i < MAX_DEFAULT; i++ )
		apszDefault[ i ] = Buffer + (i * STR_BUF_LEN);

	BuildDefaultAuto();						  /* Create default strings array	*/

	ProcessFile( AUTOEXEC_BAT, szAutoBat, vInfo.szTmpAuto, Funcs );

	FreeMemory( Buffer );
}

/***************************************************************************/
/* Builds a list of strings for the default autoexec.bat file. 				*/
/* 																								*/
/* void BuildDefaultAuto( void ) 														*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void near BuildDefaultAuto( void )
{
	register 		i;
	register 		iStr;
											/* Set ptrs to default strings which will */
											/*	always be added to a new autoexec file	*/

	for ( iStr = 0; iStr < PATH_STRING; iStr++ )
		strcpy( apszDefault[ iStr ], apszAlways[ iStr ] );

	strcat( szPath, vInfo.szPath );					/* Create the path string	*/
	apszDefault[ iStr ] = apszFiles[0] = szPath;	/* Copy ptr to 2 arrays		*/
	iStr++;

	iFlags[0] = TRUE; 									/* Always set path			*/
	iFlags[1] = vInfo.Flag.fMouse;					/* Check user option flags */
	iFlags[2] = vInfo.Flag.fDosKey;
	iFlags[3] = vInfo.Flag.fShell;

									/* Skip over the path command and then create a	*/
									/* a complete command string with path for each */
									/* user option to be added to the autoexec file */

	for ( i = 1; i < MAX_TO_ADD; i++ )
	{
		if ( iFlags[ i ] )
		{
			if ( i == (MAX_TO_ADD - 1) )				/* No path for DOSSHELL		*/
				strcpy( apszDefault[ iStr ], apszFiles[ i ] );
			else
				BuildPath( apszDefault[ iStr ], vInfo.szPath[ 0 ],
							  vInfo.szPath + 2, apszFiles[ i ] );
			iStr++;								/* Incr default strings array ptr	*/
		}
	}
	apszDefault[ iStr ] = NULL;			/* Mark the end of the list			*/
}

/***************************************************************************/
/* Adds any lines to the new autoexec.bat file which need to go before any */
/* of the original lines are processed.												*/
/* 																								*/
/* NOTE: For now there is nothing to add. 											*/
/* 																								*/
/* int NewAutoFirst( int iStatus )														*/
/* 																								*/
/* ARGUMENTS:	iStatus	- Current processing status								*/
/* RETURNS: 	int		- Original arg status if successfull else ERROR 	*/
/* 																								*/
/***************************************************************************/

int NewAutoFirst( int iStatus )
{
#ifdef NET_SUPPORT
	char			*szBuf;

	szBuf = GetMemory( 100 );					/* Add net upgrade line if			*/
	if ( NetUpgradeStr( szBuf ) )				/* it's needed							*/
		iStatus = NewFilePuts( szBuf, NewFile );
	FreeMemory( szBuf );
#endif
	return( iStatus );
}

/***************************************************************************/
/* Adds new lines to the end of the new autoexec.bat file based on the		*/
/* user's selected options and whether or not the lines were already       */
/* detected and updated during the processing of the lines in the original */
/* autoexec.bat file.																		*/
/* 																								*/
/* int NewAutoLast( int iStatus )														*/
/* 																								*/
/* ARGUMENTS:	iStatus	- Current error status of the file processing		*/
/* RETURNS: 	int		- OK if successfull else ERROR							*/
/* 																								*/
/***************************************************************************/

int NewAutoLast( int iStatus )
{
	register 	i; 								/* Loop indice							*/
	register		iDefault;						/* Default file strings	indice	*/
	int			iAddFlags[ MAX_TO_ADD ];	/* Flags line not added yet		*/

	iAddFlags[ 0 ] = !DevFlags.fPath;				/* Path may be done			*/
	iAddFlags[ 1 ] = !DevFlags.fMouse;				/* Mouse may be done 		*/

											/* We know doskey and shell not done yet	*/
	iAddFlags[ 2 ] = iAddFlags[ 3 ] = TRUE;

									/* If string was created in the default list 	*/
									/* and has not already been processed we	need	*/
									/* to add it to the end of the file					*/

	for ( i = 0, iDefault = PATH_STRING;
			iStatus == OK && i < MAX_TO_ADD;
			i++ )
	{
		if ( iFlags[ i ] )							/* If exists in default list	*/
		{
			if ( iAddFlags[ i ] )					/* If still needs to be added */
				iStatus = NewFilePuts( apszDefault[ iDefault ], NewFile );

			iDefault++; 							/* Index next str in default list */
		}
	}
	return( iStatus );
}

/***************************************************************************/
/* Processes each line in the original autoexec.bat file and copies the 	*/
/* lines to the new file in the root directory. The lines to be changed 	*/
/* are:																							*/
/* 																								*/
/* d:\xxx\DOSSHELL				 ; REM out the line									*/
/* SET COMSPEC 					 ; REM out the line									*/
/* d:\xxx\DOSHELL 				 ; REM out line if vInfo.fShell == TRUE		*/
/* d:\xxx\DOSKEY					 ; REM out line if vInfo.fDosKey == TRUE		*/
/* 																								*/
/* PATH								 ; Prefix new dos directory path and set		*/
/* SET PATH=						 ; flag to show it's been processed          */
/* d:\xxx\MOUSE					 ; Change path if vInfo.fMouse == TRUE	and	*/
/* 										set flag to show it's been processed		*/
/* 																								*/
/* NOTE: Need to REM out the existing lines rather than delete them. 		*/
/* 																								*/
/* int ProcessAutoStr( int iStatus	)													*/
/* 																								*/
/* ARGUMENTS:	iStatus	- Current error status. (not used by function)		*/
/* RETURNS: 	int		- OK if sucessfull else ERROR 							*/
/* 																								*/
/***************************************************************************/

int ProcessAutoStr( int iStatus )
{

	char				*szPtr;
	char				*szTmp;
	char				szName[ MAX_FNAME_LEN + 1 ];							/* m104 */
	register 		iFound;

	ThisLine.fCopyOld = TRUE;
	ThisLine.fHasPath = ThisLine.fRemOut = ThisLine.fHasExt = FALSE;

	szNewString[0] = EOL;							/* Setup for later strcat()	*/
	szTmp = SkipLeadingWhite( szOldString );	/* Point to 1st char in str	*/

	if ( *szTmp == COLON )							/* If it's a label just keep	*/
	{														/* the orginal string			*/
		strcat( szNewString, szOldString );
		return( iStatus );
	}

	szPtr = ParseFileName( szTmp );
	if ( szTmp != szPtr )
		ThisLine.fHasPath = TRUE;									/* Path check		*/

	ExtractNextWord( szPtr, szName, MAX_FNAME_LEN + 1 );	/* m104 Get cmd name*/
	if ( (szTmp = strchr( szName, '.' )) != NULL )
		*szTmp = EOL, ThisLine.fHasExt = TRUE;					/* Remove ext		*/

	if ( (iFound = StrSearch( szName, apszCmdStrs )) != -1 )
	{
		if ( !(ThisLine.fHasPath || ThisLine.fHasExt ) )
		{
			if ( iFound == CMD_SET )
			{
				szPtr = SkipWord( szPtr ); 				/* Skip to variable name */
				if ( strnicmp( szPtr, szPathCmd, PATH_CMD_LEN ) == OK )
					iFound = CMD_PATH;						/* Process farther down */

				else if ( strnicmp( szPtr, szComspec, COMSPEC_LEN ) == OK )
					ThisLine.fRemOut = TRUE;				/* Change line remark	*/
			}

			if ( iFound == CMD_PATH )
			{
				CreateEnvPath( szPtr ); 					/* Add new	path str 	*/
				ThisLine.fCopyOld = FALSE;
				DevFlags.fPath = TRUE;
			}
		}

	   if ( iFound == CMD_DOSSHELL )
		{
			if ( !vInfo.Flag.fShell )
				ThisLine.fRemOut = TRUE;
			else
				ThisLine.fDelLine = TRUE;
		}

		#ifdef ADD_MOUSE
			else if ( iFound == CMD_MOUSE && vInfo.Flag.fMouse )
			{
				ThisLine.fCopyOld = FALSE;
				DevFlags.fMouse = TRUE;
				BuildPath( szNewString, vInfo.szPath[0], vInfo.szPath+2, szPtr );
			}
		#endif
	}
	else if ( IsDosFile( szName ) )			/* See if need a path change		*/
	{
		ThisLine.fCopyOld = FALSE;
		BuildPath( szNewString, vInfo.szPath[0], vInfo.szPath+2, szPtr );
	}		
	else if ( FindDataMatch( szName, DELETE_FILE ) )
		ThisLine.fDelLine = TRUE;

	if ( ThisLine.fCopyOld )
		strcat( szNewString, szOldString );	/* Just copy old string				*/

	iStatus = OK;
	if ( ThisLine.fDelLine )
		iStatus = DEL_LINE;
	else if ( ThisLine.fRemOut )
		iStatus = REM_LINE;

	return( iStatus );
}

/***************************************************************************/
/* Creates a new path string from the existing path string. First seperates*/
/* the existing path string into individual paths and then creates a new	*/
/* string using the DOS path for the first entry and then appending each	*/
/* of the original strings. A check is done to be sure that if the DOS		*/
/* path is in the old string it is not duplicated in the new.					*/
/*																									*/
/*	NOTE:																							*/
/*			The value 64 for length of array of ptrs is derived from max of	*/
/*		   128 byte string / min 2 char path entry ";\".							*/
/*																									*/
/*	void CreateEnvPath( char *szString )												*/
/*																									*/
/*	ARGUMENTS:	szString	- Ptr to "PATH= xxxx" string from autoexec file		*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void near CreateEnvPath( char *szString )
{
	char		*TmpBuf;
	char		*chBuffer;							/* Work buffer							*/
	char		**apszPaths;						/* Ptrs to individ path strs		*/
	char		*chPtr;								/* Ptr to start of individ path	*/
	char		*pchEnd;								/* Ptr to end of old PATH string	*/
	int		i;										/* Indice for array apszPaths		*/


	chBuffer = GetMemory( MAX_LEN * 4 );
	TmpBuf = chBuffer + (MAX_LEN * 2);
	apszPaths = GetMemory( MAX_LEN * sizeof( char * ) );

	szString = SkipWord( szString );				/*	Jump over "PATH "				*/

	strcpy( chBuffer, szString );					/* Copy string to work buffer	*/

	chPtr = chBuffer;
	RemoveSpaces( chBuffer );						/* Clean up the string			*/
	pchEnd = strchr( chBuffer, EOL ); 			/* Find end of string			*/
	ReplaceChar( chBuffer, ';', EOL );			/* Convert to individ string	*/

	for ( i = 0; chPtr < pchEnd; i++ )
	{
		apszPaths[i] = chPtr;						/* Save pointer to this path	*/
		chPtr = strchr( chPtr, EOL ) + 1;		/* Find end of this path		*/
	}
	apszPaths[i] = NULL;								/* Mark end of array				*/

	strcpy( szNewString, "PATH " );
	if ( apszPaths[0][0] != '>' )
		strcat( szNewString, vInfo.szPath );	/* Add new DOS path 				*/

	for ( i = 0; apszPaths[i] != NULL; i++ ) 	/* Append original path strs	*/
	{
		strcpy( TmpBuf, apszPaths[i] );
		RemoveTrailing( TmpBuf, '\\' );
															/* Chk for duplicate path		*/
		if ( strcmpi( TmpBuf, vInfo.szPath ) != OK )
		{
			strcat( szNewString, ";" ); 			/* Not duplicate so append		*/
			strcat( szNewString, apszPaths[i] );
		}
	}

	FreeMemory( chBuffer );
	FreeMemory( apszPaths );
}

/***************************************************************************/
/* Checks to see if the specified command is really a DOS command which		*/
/* was copied from the distribution disks. The specified command may			*/
/* or may not contain an extension.														*/
/*																									*/
/*	int IsDosFile( char *szName )															*/
/*																									*/
/*	ARGUMENTS:	szName	- Ptr to command name or file name string				*/
/*	RETURNS:		int		- TRUE if command or filename is a DOS file on the	*/
/*								  distribution disk. Else FALSE.							*/
/***************************************************************************/

int near IsDosFile( char *szName )
{
	register			i;
	char				*szExt;
	char				szFile[ MAX_FNAME_LEN + 1 ];							/* m104	*/
	extern char		*szExeExt[];

	strncpy( szFile, szName, MAX_FNAME_LEN );								/* m104 */
	*(szFile + MAX_FNAME_LEN) = EOL;							/* m104 Error check */

	if ( (szExt = strchr( szFile, '.' )) == NULL )
		szExt = strchr( szFile, EOL );
	*(szExt++) = '.';

				/* Check for a base name which has more than 8 characters	*/
				/* the last line of code may have destroyed the EOL marker	*/
				/* so exit function on bad name										*/
	if ( (szExt - szFile) > (MAX_FNAME_LEN - FILE_EXT_LEN) )		/* m104	*/
		return( FALSE );										/* m104 Bad file name*/

	for ( i = 0; szExeExt[i] != NULL; i++ )
	{
		
		strcpy( szExt, szExeExt[i] );
		if ( IsDistrFile( szFile ) && !FindDataMatch( szFile, RE_NAME ) &&
			  !FindDataMatch( szFile, NET_FILES ) ) 
			return( TRUE );
	}
	return( FALSE );
}

/***************************************************************************/
/* Renames a file to a unique file with the same primary name but with an	*/
/* extension in the form of an ascii integer value between 0 & 999.			*/
/*																									*/
/*	int UniqueRename( char *szFile )														*/
/*																									*/
/*	ARGUMENTS:	szFile	- Full path and file name to be renamed				*/
/*					szName	- Buffer to copy new file name to or NULL				*/
/*	RETURNS:		int		- OK if successfull else ERROR							*/
/*																									*/
/***************************************************************************/

int near UniqueRename( char *szFile, char *szName )
{
	register	i;
	char		*szExt;
	char		*szTmp;
	char		szNew[ MAX_PATH_LEN ];


	if ( !FileExists( szFile ) )
		return( OK );

	strcpy( szNew, szFile );
	szExt = ParseFileName( szNew );

	if ( (szTmp = strchr( szExt, '.' )) != NULL )
		szExt = szTmp + 1;
	else
		szExt = strchr( szExt, EOL );

	for ( i = 1; i < 999; i++ )
	{
		itoa( i, szExt, 10 );
		if ( rename( szFile, szNew ) == OK )
		{
			if ( szName != NULL )
				strcpy( szName, szNew );
			return( OK );
		}
	}
	return( ERROR );
}
