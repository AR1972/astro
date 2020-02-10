/***************************************************************************/
/*																									*/
/*	SKIPSPC.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns a ptr to first character after any white spaces or '=' chars.	*/
/* Checks for EOL and if encountered stops processing and returns a ptr 	*/
/* to the EOL character.																	*/
/*                                                                         */
/* char *SkipLeadingWhite( char *szPtr )												*/
/* 																								*/
/* ARGUMENTS:	Str		- Ptr to a string												*/
/* RETURNS: 	char *	- Ptr to first non-white space character or EOL 	*/
/* 																								*/
/* johnhe - 12/01/89																			*/
/***************************************************************************/

#include 	<strlib.h>

char *SkipLeadingWhite( char *szPtr )
{
	while ( IsWhite( *szPtr ) && *szPtr != EOL )
		szPtr++;

	return( szPtr );
}
