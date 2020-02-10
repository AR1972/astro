/***************************************************************************/
/*																									*/
/*	MSTRING.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Copies a single string from the message area to the user's buffer.      */
/* 																								*/
/* void GetMessStr( char *szBuf, unsigned TextOffset, int StrNum )			*/
/* 																								*/
/* ARGUMENS:	szBuf 		- Ptr to buffer to copy the specified string to */
/* 				TextOffset	- String group offset in the message file 		*/
/* 				StrNum		- The group string number to get 					*/
/* RETURNS: 	void																			*/
/* 																								*/
/* johnhe - 02-24-89																			*/
/***************************************************************************/

#include 	<stdio.h>
#include		<string.h>

#include 	<alias.h>

#define		END_MARKER			-1

void GetMessStr( char *szBuf, unsigned uOffset, int StrNum )
{
	register		i;
	char			*pchText;
	extern char	chMessage;

	pchText = (&chMessage) + uOffset;		/* Address the first string */

	for ( i = 0; i < StrNum && *pchText != END_MARKER; i++ )
	{
		while( *pchText != EOL )				/* Find end of this string */
			pchText++;
		pchText++;									/* Address start of next string */
	}

	if ( *pchText != END_MARKER )
		strcpy( szBuf, pchText );
	else
		*szBuf = EOL; 
}

/***************************************************************************/
#if 0

	char		*apszText[ MAX_STRINGS ];

	GetMessage( apszText, TextOffset );
	strcpy( szBuf, apszText[ StrNum ] );

#endif
/***************************************************************************/
