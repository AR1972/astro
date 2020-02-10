/***************************************************************************/
/*																									*/
/*	BLDPATH.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Builds a valid path name when given the 3 elements of drive, path and	*/
/* file name along with a buffer for the created path.							*/
/*																									*/
/* void BuildPath( char *szPath, char *szDirPath, char *szFileName ) 		*/
/*																									*/
/*	ARGUMENTS:	szPath	-  Buffer to hold the created pathname					*/
/* 				chDrive	-	Drive designator											*/
/*					szDirPath -	Pointer to drive and directory path					*/
/*					szFileName - Pointer to a file name									*/
/* RETURNS: 	void																			*/
/*																									*/
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#include    <stdio.h>
#include 	<string.h>

#include 	<strlib.h>

void BuildPath( char *szPath, char chDrive, char *szDirPath, char *szFileName )
{
	char	*szPtr;								/* Temoporay place holder */

	*szPath = chDrive;						/* Set drive designator & seperator */
	*(szPath+1) = ':';
	*(szPath + 2 ) = '\\';					/* Add a backslash						*/
	*(szPath + 3 ) = EOL;					/* Terminate the string					*/

	if ( *szDirPath == '\\' )				/* Prevent a double backslash			*/
		szDirPath++;

		
	strncat( szPath, szDirPath, 63 );	/* Add the directory path */

	szPtr = strchr( szPath, EOL );		/* If szDir doesn't end with a   */
													/* backslash we need to add one	*/
#ifdef DBCS
	if ( *(szPtr - 1) != '\\' || CheckDBCSTailByte(szPath,szPtr -1))
#else
	if ( *(szPtr - 1) != '\\' )			
#endif
		strcat( szPath, "\\" );

	strcat( szPath, szFileName );			/* Add the file name */
}

