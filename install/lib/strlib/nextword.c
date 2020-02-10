/***************************************************************************/
/*																									*/
/*	NEXTWORD.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Copies the first word from a string into a buffer with a specified max	*/
/* length.																						*/
/* 																								*/
/* void ExtractNextWord( char *szStr, char *szBuffer, int iBufLen )			*/
/* 																								*/
/* ARGUMENTS:	szStr 	- Ptr to string to parse word from						*/
/* 				szBuffer - Buffer to copy the word into							*/
/* 				iBufLen	- Total length of the buffer in bytes					*/
/* RETURNS: 	void																			*/
/* 																								*/
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#include 	<strlib.h>

unsigned ExtractNextWord( char *szStr, char *szBuffer, int iBufLen )
{
	register unsigned int	i;
	register unsigned int   iLen;

	iLen = (iBufLen - 1);

	for ( i = 0; i < iLen && *szStr != EOL; i++, szStr++ )
	{
		if ( !IsWhite( *szStr ) && *szStr != '/' )
			*(szBuffer++) = *szStr;
		else
			break;
	}
	*szBuffer = EOL;

   return (i);
}
