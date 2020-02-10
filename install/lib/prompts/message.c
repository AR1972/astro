/***************************************************************************/
/*																									*/
/*	MESSAGE.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Builds an array of pointers to the strings starting at a specified		*/
/* offset. The caller should get the offset from the #defines in the			*/
/* MESSAGE.H file which is created by the INDEX.EXE program. The the			*/
/* array passed as argument should be long enought to hold the number		*/
/* of lines store at the specified offset, this value is also generated		*/
/* by the INDEX.EXE program and is included in the MESSAGE.H file. A NULL	*/
/* pointer will be added to the array after the pointer to the last string	*/
/* in the specified group.																	*/
/*																									*/
/* void GetMessage( char **apszArray, unsigned uOffset )							*/
/* 																								*/
/* ARGUMENTS:	apszArray	- Array to hold the string pointers.				*/
/*					uOffset		- The offset of the first string in the			*/
/*									  MESSAGE.TXT file as defined in the MESSAGE.H	*/
/*									  file.														*/
/* RETURNS:		void																			*/
/*																									*/
/*	EXTERNS:		chMessage	- First character in message area declared in 	*/
/*									  MESSAGE.ASM by the message compiler.				*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include 	<stdio.h>
#include 	<alias.h>

#define		END_MARKER			-1

void GetMessage( char **apszArray, unsigned uOffset )
{
	char			**pszPtr;
	char			*pchText;
	extern char	chMessage;

	pchText = (&chMessage) + uOffset;		/* Address the first string */
	pszPtr = apszArray;							/* Point to first element array */

	while ( *pchText != END_MARKER )			/* Loop until -1 is detected */
	{
		*(pszPtr++) = pchText;					/* Save pointer to this string */
		while( *pchText != EOL )				/* Find end of this string */
			pchText++;
		pchText++;									/* Address start of next string */
	}
	*pszPtr = NULL;								/* Mark end of the array of ptrs */

}
