/***************************************************************************/
/*																									*/
/*	SKIPWORD.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns a ptr to the first character of the word following the first 	*/
/* encountered in the specified string.												*/
/* 																								*/
/* char *SkipWord( char *szPtr ) 														*/
/* 																								*/
/* ARGUMENTS:	szPtr 	- Ptr to string												*/
/* RETURNS: 	char *	- Ptr to first character of next word or EOL 		*/
/* 																								*/
/* johnhe - 12/01/89																			*/
/***************************************************************************/

#include 	<strlib.h>

char *SkipWord( char *szPtr )
{

	szPtr = SkipLeadingWhite( szPtr );
	szPtr = SkipNonWhite( szPtr );
																/* Find start of next word */
	szPtr = SkipLeadingWhite( szPtr );

	return( szPtr );
}
