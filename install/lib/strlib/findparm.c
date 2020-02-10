/***************************************************************************/
/*																									*/
/*	FINDPARM.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Searches a string for the specified character prefixed with a forward	*/
/* slash character. The search for the parameter is not case sensitive. 	*/
/* 																								*/
/* int FindParam( char *szStr, char ch )												*/
/* 																								*/
/* ARGUMENTS:	szStr 	- Ptr to string to be searched							*/
/* 				ch 		- Parameter	character to search for 					*/
/* RETURNS: 	int		- TRUE if search parameter	is found else false		*/
/* 																								*/
/* johnhe - 12/01/89																			*/
/***************************************************************************/

#include    <stdio.h>
#include 	<string.h>
#include 	<ctype.h>

#define		FALSE 	0
#define		TRUE		1

int FindParam( char *szStr, char ch )
{
	register 	iFound;
	register		chParam;	

	iFound = FALSE;
	ch = (char)toupper( ch );

	while ( iFound == FALSE && (szStr = strchr( szStr, '/' )) != NULL )
	{
		chParam = *(++szStr);
		chParam = toupper( chParam );
		iFound = (chParam == ch );
	}
	return( iFound );
}
