/***************************************************************************/
/*																									*/
/*	STRSRCH.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/*	Looks for a matching string in an array of pointers to strings.			*/
/*																									*/
/*	int StrSearch( char *szSearchStr, char **szStrings ) 							*/
/*																									*/
/*	ARGUMENTS:	szSearchStr	- String to match											*/
/*					szStrings	- Array of pointers to strings with last the		*/
/*									  end of the array marked with a NULL pointer	*/
/*	RETURNS:		int			- Index to the first matching string or -1 if	*/
/*									  there was no matching string found.				*/
/*                                                                         */
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#include    <stdio.h>
#include 	<string.h>
#define		OK 	0

int StrSearch( char *szSearchStr, char **szStrs )
{
	int		i;

	if ( szStrs == NULL )
		return( -1 );

 	for( i = 0; szStrs[i] != NULL; i++ )
	{
		if ( strcmpi( szSearchStr, szStrs[i] ) == OK )
			break;
	}

	return( szStrs[i] == NULL ? -1 : i );
}
