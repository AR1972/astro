/***************************************************************************/
/*																									*/
/*	STRSHL.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* ShiftStringLeft() moves a string left one character, including EOL.		*/
/* Returns the length of the new string.                                   */
/* 																								*/
/* unsigned ShiftStringLeft( char *String )											*/
/* 																								*/
/* ARGUMENTS:	String	- pointer to string to be shifted						*/
/* RETURNS: 	unsigned - length of original string - 1							*/
/* 																								*/
/* Created 03-23-89 - johnhe																*/
/***************************************************************************/

#include    <stdio.h>
#include 	<string.h>

unsigned ShiftStringLeft( char *String )
{
	unsigned 	Length;

   if ( (Length = strlen(String)) != 0 )
      memmove( String, String+1, Length-- );
   return( Length );
}
