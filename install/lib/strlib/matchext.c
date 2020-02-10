/***************************************************************************/
/*																									*/
/*	MATCHEXT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Checks a file name to see if it's extension exists in a list of         */
/* different extensions. Returns the index to the first matching extension */
/* NOTE: the filename may consist of the full path to the file.				*/
/*                                                                         */
/* int FindExtMatch( char *szFile, char **szExt )									*/
/* 																								*/
/* ARGUMENTS:	szFile - Ptr to filename to chech for a matching extension	*/
/* RETURNS: 	int	 - Index to matching extension in the list or -1		*/
/* 																								*/
/* johnhe - 10/30/89																			*/
/***************************************************************************/

#include    <stdio.h>
#include 	<string.h>

#include 	<strlib.h>

int FindExtMatch( char *szFile, char **szExt )
{
	register 	iMatch;

	iMatch = -1;
	szFile = ParseFileName( szFile );
	if ( (szFile = strchr( szFile, '.' )) != NULL )
		iMatch = StrSearch( szFile + 1, szExt );

	return( iMatch );
}
