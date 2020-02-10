/***************************************************************************/
/*																									*/
/*	DIRTOFN.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Creates a unix type file name from a directory or FCB entry.				*/
/*																									*/
/*	void DirToFileName( char *Str, struct DIR *Dir )								*/
/*																									*/
/*	ARGUMENTS:	Str	- Buffer to create the file name string in				*/
/*					Dir	- Ptr to a directory structure containing a name		*/
/*																									*/
/* johnhe - 09-03-90																			*/
/***************************************************************************/

#define		EOL		'\0'
#define		NAME_LEN	8
#define		EXT_LEN	3
#define		FN_LEN	13

#include		<string.h>
#include		<strlib.h>

void DirToFileName( char *Str, char *Dir )
{

	memset( Str, EOL, FN_LEN );
	strncpy( Str, Dir, NAME_LEN );
	Str += RemoveTrailing( Str, SPC );

	Dir += NAME_LEN;									/* Address the file extension */
	if ( *Dir != SPC )
	{
		*(Str++) = '.';
		strncpy( Str, Dir, EXT_LEN );
		RemoveTrailing( Str, SPC );
	}		
}

