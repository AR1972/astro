/***************************************************************************/
/*																									*/
/* DSK_DIR.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Functions to verify a multi-level directory path and create the path.	*/
/* If an error is detected any newly created levels in the path are			*/
/* removed to insure the disk is not left with random directories.			*/
/*																									*/
/* Created 07-23-89 johnhe																	*/
/*																									*/
/*	modified 07/26/90 t-gerrit -- IsValidPath now gives caller the option	*/
/*	to delete the portion of the path that is created regardless of whether */
/*	or not an error occurs.																	*/
/***************************************************************************/
 
#include 	<stdio.h>
#include 	<dos.h>
#include 	<direct.h>

#include 	<alias.h>
#include 	<disk_io.h>
#include 	<string.h>
#include		<strlib.h>

#define		ERROR 			-1

int IsReservedName( char *szPath );

/***************************************************************************/
/* Validates a directory path string by cleaning up the string and then		*/
/* creating the directory path. If the path can be successfully created it	*/
/* is assumed to be a valid path. If there is an error creating the path	*/
/* it is assumed the path name is invalid and any part of the path that		*/
/* did not exist originally is deleted. The path may contain a trailing		*/
/* '\' character but it is not necessary as one will be added by this		*/
/* function if it was omitted. If SavePath is FALSE, the part of the path  */
/* that has been created is deleted. The current path is is saved on entry */
/* and restored before the function returns to the caller. 						*/
/*																									*/
/*	int IsValidPath( char *szPath, unsigned DrvNumber, int SavePath )			*/
/*																									*/
/* ARGUMENTS:	szPath	- A directory path string									*/
/* 				DrvNumber - DOS drive number: 1 = A, 2 = B, 3 = C, ...		*/
/*					SavePath - TRUE if part of path that is created is to be 	*/
/*									kept, FALSE if it	is to be deleted.					*/
/* RETURNS:		int		- TRUE if a valid path else FALSE						*/
/***************************************************************************/

int IsValidPath( char *szPath, unsigned uDrvNumber, int SavePath )
{
	char				szCurPath[ MAX_PATH_LEN	];
	char				*szPtr;
	register 		Status;
	unsigned			uDrvTotal;
	unsigned 		uDrvCurrent;

	_dos_getdrive( &uDrvCurrent );					/* Save current drive		*/
	_dos_setdrive( uDrvNumber, &uDrvTotal );		/* Change to desired drive	*/
	if ( uDrvNumber > uDrvTotal )						/* Check for valid drive	*/
		return( FALSE );

#ifdef DBCS
	DBCSstrupr( szPath );
#else
	strupr( szPath );
#endif

	getcwd( szCurPath, (MAX_PATH_LEN - 1) );
	szPtr  = szPath;

	RemoveSpaces( szPath );
	RemoveTrailing( szPath, '\\' );
	if ( *szPtr != '\\' )
		InsertChar( szPath, '\\' );

	if ( strlen( szPath ) == 1 ) 						/* Check for root dir */
		return( FALSE );

	chdir( "\\" );											/* Change to root dir		*/

	if ( (Status = MoveToDir( szPath, SavePath )) == OK )
	  ;						/* New path was success 	*/
	else
		Status = ERROR;

	chdir( szCurPath );								/* Return to original dir	*/
	_dos_setdrive( uDrvCurrent, &uDrvTotal ); 	/* Restore current drive	*/

	return( Status == OK ? TRUE : FALSE );
}

/***************************************************************************/
/* Changes the current directory to the first directory and then				*/
/* recursively calls itself to go to the next directory so that eventually	*/
/* the current directory will be the same as the entire path string. If 	*/
/* any directories in the path string do not exist, they will be created	*/
/* but if the next recursive call to MoveToDir fails or SavePath is FALSE  */
/* any directory which was created will be deleted.								*/
/*																									*/
/* int	MoveToDir( char *szPath, int SavePath)										*/
/*																									*/
/*	ARGUMENTS:	szPath	- The path string to use to find the next directory*/
/*					SavePath - TRUE if directories created are to be deleted,	*/
/*								  FALSE ifthey are to be kept.							*/
/* RETURNS:		int		- OK if successful else ERROR								*/
/*																									*/
/***************************************************************************/
int	MoveToDir( char *szPath, int SavePath )
{
	int		Status = OK, NewDir = FALSE;
	char		*pchEnd = NULL;

	if ( *szPath == EOL )		/* See if there is a path to change to */
		;								/* If not just return success */
	else
	{
		szPath++; 					/* Step over the backslach character */
#ifdef DBCS
		pchEnd = DBCSstrchr( szPath, '\\' );
#else
		pchEnd = strchr( szPath, '\\' );
#endif
		if ( pchEnd != NULL )	/* If a back slash change it to an EOL */
			*pchEnd = EOL;
										/* Check for max directory name length */
		if ( !IsValidDirName( szPath ) )
			Status = ERROR;
		else if ( chdir( szPath ) == OK )
			;
		else
		{
			if ( mkdir( szPath ) == OK )
			{
				NewDir = TRUE;
				if ( chdir( szPath ) == OK )
				{
					Status = OK;

						/*If we aren't saving path's and aren't going to*/
						/*recurse any further then remove the directory.*/
					if ((SavePath == FALSE) && (pchEnd == NULL))
					{
						chdir ("..");
						rmdir ( szPath );
					}

				}
				else
					rmdir( szPath ), Status = ERROR, NewDir = FALSE;
			}
			else
				Status = ERROR;
		}
		if ( Status == OK  && pchEnd != NULL )
		{									/* Recursively move to the next directory */
			*pchEnd = '\\';			/* Replace directory seperator */
			if ( (Status = MoveToDir( pchEnd, SavePath )) == OK )
				{
					if ( ( NewDir == TRUE ) && ( SavePath == FALSE) )
					{
						chdir ("..");
						*pchEnd = EOL;
						rmdir ( szPath );
						*pchEnd = '\\';
					}
				}
			else
			{
				chdir( ".." );
				if ( NewDir == TRUE )
				{
					*pchEnd = EOL;
					rmdir( szPath );
					*pchEnd = '\\';				/* Replace directory seperator */
				}
			}
		}

	}
	return( Status );
}

/***************************************************************************/
/* Scans a string to see if the string can be used a valid directory name. */
/* The scan checks to be sure each character in the name is a valid			*/
/* character for a path name. There is also a check to be sure that only	*/
/* there is not more than 1 decimal in the name and that if there is a		*/
/* decimal that the primary name and extension do not exceed the maximum	*/
/* length of 8 chars for primary and 3 for extension. If the name does 		*/
/* not include a decimal the max length is 8 characters.							*/
/*																									*/
/* int IsValidDirName( char *szPath )													*/
/*																									*/
/* ARGUMENTS: szPath		- String containing a directory name.					*/
/* RETURNS	: int			- TRUE if valid name else FALSE.							*/
/*																									*/
/***************************************************************************/

int IsValidDirName( char *szPath )
{
	int		Status;
	char		*szDecimal;

	Status = FALSE;
	if ( strlen( szPath ) > 0 &&
		  ValidDirChars( szPath ) &&
		  !IsReservedName( szPath ) )
	{
		if ( (szDecimal = strchr( szPath, '.' )) != NULL )
		{
			if ( strchr( szDecimal + 1, '.' ) == NULL &&	/* Chk for more '.'s */
				  (szDecimal - szPath) <= 8 &&				/* Chk lengths */
				  (strchr( szDecimal, EOL ) - szDecimal - 1) <= 3  )
				Status = TRUE;
		}
		else if ( strlen( szPath ) <= 8 )
			Status = TRUE;
	}
	return( Status );
}

/***************************************************************************/
/* Checks all of the characters in a string to see if they are vaild path	*/
/* name characaters.																			*/
/*																									*/
/* int ValidDirChars( char *szPath )													*/
/*																									*/
/* ARGUMENTS:	szPath	-  Directory name string									*/
/* RETURN:		int		-	TRUE if chars in string are valid else FALSE		*/
/*																									*/
/***************************************************************************/

int ValidDirChars( char *szPath )
{
	int		IsOk = TRUE;

	while ( IsOk && *szPath != EOL )
#ifdef DBCS
	    if (IsDBCSLeadByte(*szPath))
		szPath += 2;
	    else
#endif
		IsOk = IsValidPathChar( *(szPath++) );

	return( IsOk );
}

/***************************************************************************/
/* Checks a file or path name against a list of reserved DOS filenames and	*/
/* returns TRUE if the name is a reserved name. The function must first		*/
/* off any extension from the name.														*/
/*																									*/
/*	int NotReservedName( char *szPath )													*/
/*																									*/
/* ARGUMENTS:	szPath	-  Directory name string									*/
/* RETURN:		int		-	TRUE if name is reserved DOS name					*/
/*																									*/
/***************************************************************************/

int IsReservedName( char *szPath )
{
	register			Status;
	register			i;
	char				*szTmp;
	static char		*apszRes[] = { "AUX", "CLOCK$", "COM1", "COM2",
											"COM3", "COM4", "CON", "LPT", "LPT1",
											"LPT2", "LPT3", "LST", "NUL", "PRN", NULL };

	if ( (szTmp = strchr( szPath, '.' )) != NULL )
		*szTmp = EOL;

	for ( i = 0, Status = FALSE; Status == FALSE && apszRes[i] != NULL; i++ )
		Status = !strcmpi( szPath, apszRes[i] );

	if ( szTmp != NULL )
		*szTmp = '.';

	return( Status );
}


