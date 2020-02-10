/***************************************************************************/
/*																									*/
/*	STRPAD.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Pads the end of a string with a specified character.							*/
/* 																								*/
/* void PadStr( char *szStr, char chChar, int Len )								*/
/* 																								*/
/* ARGUMENTS:	szStr 	- Ptr to string												*/
/* 				chChar	- Character to pad the string with						*/
/* 				Len		- Total length of the padded string in bytes			*/
/* RETURNS: 	void																			*/
/* 																								*/
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#include 	<string.h>

#include 	<strlib.h>

void PadStr( char *szStr, char chChar, int Len )
{
	char	*Ptr;
	char	*MaxPtr;

	MaxPtr = szStr + Len;
	Ptr = strchr( szStr, EOL );

	while ( Ptr < MaxPtr )
		*(Ptr++) = chChar;

	*Ptr = EOL;
}
