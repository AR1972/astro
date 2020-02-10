/***************************************************************************/
/*																									*/
/*	CHRISNS.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Inserts a character at the begining of a string and returns the new		*/
/* length of the string.                                                   */
/*                                                                         */
/* unsigned InsertChar( char *String, int Char )									*/
/* 																								*/
/* ARGUMENTES: String		- Pointer to a string									*/
/* 				Char			- character to be inserted at begining of string*/
/* 																								*/
/* RETURNS: 	unsigned 	- length of orignal string - 1						*/
/* 																								*/
/* johnhe - 03-23-89																			*/
/***************************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include		<string.h>

/***************************************************************************/

unsigned InsertChar( char *String, int Char )
{
	unsigned StringLength;

	StringLength = strlen(String) + 1;
   memmove( String+1, String, StringLength );
   *String = (char)(Char);
	return( StringLength );							/* Return length of new string */
}
                                
