/***************************************************************************/
/*																									*/
/* DSK_DIR.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Functions to verify a multi-level directory path and create the path.	*/
/* If an error is detected any newly created levels in the path are			*/
/* removed to insure the disk is not left with random directories.			*/
/*																									*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include 	<stdio.h>
#include 	<dos.h>
#include 	<direct.h>

#include 	<alias.h>
#include 	<string.h>
#include		<strlib.h>
#include		<disk_io.h>

#define		ERROR 			-1


/***************************************************************************/
/* Validates a directory path string by cleaning up the string and then		*/
/* checking that each directory in the path has a valid directory name.		*/
/*																									*/
/*	int ValidatePath( char *szPath )														*/
/*																									*/
/* ARGUMENTS:	szPath	- A directory path string									*/
/* RETURNS:		int		- TRUE if a valid path else FALSE						*/
/***************************************************************************/

int ValidatePath( char *szPath )
{
	char				*szPtr;
	register 		Status;

#ifdef DBCS
	DBCSstrupr( szPath );
#else
	strupr( szPath );
#endif
	szPtr  = szPath;

	RemoveSpaces( szPath );
	RemoveTrailing( szPath, '\\' );
	if ( *szPtr != '\\' )
		InsertChar( szPath, '\\' );

	if ( strlen( szPath ) == 1 ) 						/* Check for root dir */
		return( FALSE );

	Status = ValidateDir( szPath );
	return( Status == OK ? TRUE : FALSE );
}	

/***************************************************************************/
/*	Parses out the first directory from a path string and validates that 	*/
/* directory name and if valid recursively calls itself until each name	in	*/
/* in the path has been validated or an error is detected.						*/
/*																									*/
/* int	ValidateDir( char *szPath )													*/
/*																									*/
/*	ARGUMENTS:	szPath	- The path string to use to find the next directory*/
/* RETURNS:		int		- OK if successful else ERROR								*/
/*																									*/
/***************************************************************************/
int	ValidateDir( char *szPath )
{
	int		Status;
	char		*pchEnd = NULL;

	if ( *szPath == EOL )		/* See if there is a path to change to */
		return( OK );				/* If not just return success */
	else
	{
		if ( *szPath == '\\' )	/* Step over leading backslach character */
			szPath++; 				
#ifdef DBCS
		pchEnd = DBCSstrchr( szPath, '\\' );
#else
		pchEnd = strchr( szPath, '\\' );
#endif
		if ( pchEnd != NULL )	/* If a back slash change it to an EOL */
			*pchEnd = EOL;
										/* Check for max directory name length */
		if ( IsValidDirName( szPath ) )
			Status = OK;
		else
			Status = ERROR;

		if ( pchEnd != NULL )
		{
			*pchEnd = '\\';			/* Replace directory seperator */
			if ( Status == OK )
				Status = ValidateDir( pchEnd );
		}
		return( Status );
	}
}



