/***************************************************************************/
/*																									*/
/* CHRREPL.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Replaces all occurances of a specified character in a string with 		*/
/* another character.																		*/
/* 																								*/
/* void ReplaceChar( char *szString, char chOldChar, char chNewChar )		*/
/* 																								*/
/* ARGUMENTS:	szString 	- Ptr to string											*/
/* 				chOldChar	- Char to be replaced									*/
/* 				chNewChar	- Replacement character 								*/
/* RETURNS: 	void																			*/
/* 																								*/
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#include 	<stdio.h>
#include 	<string.h>

void ReplaceChar( char *szString, char chOldChar, char chNewChar )
{
	while ( (szString = strchr( szString, chOldChar )) != NULL )
		*(szString++) = chNewChar;

}
