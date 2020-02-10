/***************************************************************************/
/*																									*/
/*	RMTRAIL.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Removes all trailing characters of the type specified from a string		*/
/* and returns the length of the new string.                               */
/*                                                                         */
/* unsigned RemoveTrailing( char *String, char Char ) 							*/
/* 																								*/
/* ARGUMENTS:	String	- pointer to a string										*/
/* 				Char		- the ascii char to remove from the end of string	*/
/* 																								*/
/* RETURNS: 	unsigned - lenght of the new string									*/
/* 																								*/
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include 	<string.h>

#include 	<strlib.h>

/***************************************************************************/

unsigned RemoveTrailing( char *String, char Char )
{
   char     *EndOfString;

	EndOfString = strchr(String, EOL );

#ifdef DBCS
	while( EndOfString != String && *(EndOfString-1) == Char &&
				!CheckDBCSTailByte(String,EndOfString-1))
#else
	while( EndOfString != String && *(EndOfString-1) == Char )
#endif
		EndOfString--;

	*EndOfString = EOL;
	return( (unsigned)(EndOfString - String) );
}

