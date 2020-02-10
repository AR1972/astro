/***************************************************************************/
/*																									*/
/*	PARSEFN.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns a pointer to the first character in the filename which may or	*/
/* may not be appended to a path.														*/
/* 																								*/
/* char *ParseFileName( char *szPath ) 												*/
/* 																								*/
/* ARGUMENTS:	szPath	- Ptr to a file path in the form d:\xxxx\xxx.xxx	*/
/* RETURNS: 	char *	- Ptr to file name or character after last			*/
/* 							  backslash or ':' in the string if the path did	*/
/* 							  not contain a file name									*/
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#include    <stdio.h>
#include 	<strlib.h>

char *ParseFileName( char *szPath )
{
	char	*szPtr;

	for ( szPtr = szPath;
			*szPtr != EOL && (IsValidPathChar( *szPtr ) ||	*szPtr == ':');
			szPtr++ )
#ifdef DBCS
		if (IsDBCSLeadByte(*szPtr))
			szPtr++;
#else
		;
#endif

#ifdef DBCS
	while(( --szPtr >= szPath && *szPtr != '\\' && *szPtr != ':') ||
		(szPtr >= szPath && CheckDBCSTailByte(szPath,szPtr)) )
#else
	while( --szPtr >= szPath && *szPtr != '\\' && *szPtr != ':' )
#endif
		;

	return( ++szPtr );
}

