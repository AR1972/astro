/***************************************************************************/
/*																									*/
/*	NONWHITE.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns a ptr to first white space or '=' character encountered.			*/
/*                                                                         */
/* char *SkipNonWhite( char *szPtr )													*/
/* 																								*/
/* ARGUMENTS:	Str		- Ptr to a string												*/
/* RETURNS: 	char *	- Ptr to first white space character or EOL	*/
/* 																								*/
/* johnhe - 12/01/89																			*/
/***************************************************************************/

#include 	<strlib.h>

char *SkipNonWhite( char *szPtr )
{
	while( !IsWhite( *szPtr ) && *szPtr != EOL )
#ifdef DBCS
	{
		if (IsDBCSLeadByte(*szPtr))
			szPtr++;
		szPtr++;
	}
#else
		szPtr++;
#endif

	return( szPtr );
}

