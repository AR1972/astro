/***************************************************************************/
/*																									*/
/*	F_REN.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Renames within the same directory of the specified full drive and path	*/
/* anem. This is necessary when renameing the system files because DOS		*/
/* versions < 3 move the physical directory entry when renaming which will */
/* move the system file entries out of the first 2 positions.					*/
/* 																								*/
/* int RenameFCB( char *szFrom, char *szTo )											*/
/* 																								*/
/* ARGUMENTS:	szFrom	- Original name of file in the form X:\NAME.EXT 	*/
/* 				szTo		- New name for	file in the form X:\FILENAME.EXT		*/
/* RETURNS: 	int		- OK if successfull else error							*/
/*																									*/
/* Created 10-28-89 johnhe																	*/
/***************************************************************************/


#include 	<stdio.h>
#include 	<stdlib.h>
#include 	<dos.h>
#include 	<string.h>
#include 	<direct.h>

#include 	<alias.h>
#include 	<strlib.h>
#include 	<disk_io.h>

int RenameFCB( char *szFrom, char *szTo )
{
	#define		SPEC_FCB_LEN	0x25
	#define		PATH_LEN 		70

	char			chDrive;
	char			Fcb[ SPEC_FCB_LEN + PATH_LEN ];
	char			*szOldPath;
	char			*szPtr;
	char			chTmp;
	int			iStatus;
													/* Check for same names */
	if ( strcmpi( szFrom + 3, szTo) == OK )
		return( OK );

	szOldPath = Fcb + SPEC_FCB_LEN;

	memset( Fcb, 0, SPEC_FCB_LEN );
	chDrive = (char)(toupper( *szFrom ) - 0x40); 	/* Get DOS drive number	*/

	strcpy( szOldPath, "X:\\" );			/* Create a path prefix 				*/
	*szOldPath = *szFrom;					/* Drive letter for path				*/

	iStatus = ERROR;							/* Assume may get some errors 		*/
	if (_dos_getdir( szOldPath + 3, (int)chDrive ) == OK )
	{
		szPtr = ParseFileName( szFrom );	/* Get ptr to end of directory path */
		chTmp = *szPtr;						/* Save first char of file name		*/
		*szPtr = EOL;							/* Form path with no file added		*/

		if ( chdir( szFrom ) == OK )		/* Change to specified directory */
		{
			*szPtr = chTmp;					/* Restore first char of file name */
													/* Use FcbParse to put the names in */
													/* the FCB and then call FcbRename	*/
			FcbParse( szPtr, Fcb );
			FcbParse( ParseFileName( szTo ), Fcb + 0x10 );
			*Fcb = chDrive;					/* Set the drive number in the FCB	*/
			if ( FcbRename( Fcb ) == OK && chdir( szOldPath ) == OK )
				iStatus = OK;
		}
		*szPtr = chTmp;						/* Necessary if there was an error */
	}

	return( iStatus );
}
