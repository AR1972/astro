/***************************************************************************/
/*																									*/
/*	STRMSPC.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Removes all spaces in a string.														*/
/*                                                                         */
/* unsigned RemoveSpaces( char *szString )											*/
/* 																								*/
/* ARGUMENTS:	String	- pointer to string											*/
/* RETURNS: 	unsigned - length of the new string									*/
/* 																								*/
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#include    <stdio.h>
#include 	<strlib.h>

unsigned RemoveSpaces( char *szString )
{
	char		*szPtr, *szStart;

	szStart = szPtr = szString;
	while( *szPtr != EOL )
	{
		if ( *szPtr != SPC )
			*(szString++) = *(szPtr++);
		else
			szPtr++;
	}
	*szString = EOL;
	return( (unsigned)(szStart - szString) );
}
