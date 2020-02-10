/***************************************************************************/
/*																									*/
/*	STRMAX.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns the length of the longest string in an array of strings.			*/
/*                                                                         */
/* unsigned MaxStrLen( char *Strings[] )												*/
/* 																								*/
/* ARGUMENTS:	Strings - array of pointers to strings								*/
/* RETURNS: 	unsigned - length of the longest string in the array			*/
/* 																								*/
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#include    <stdio.h>
#include 	<string.h>

unsigned MaxStrLen( char **Strings )
{
	register 		i;
	unsigned 		Len;
	unsigned 		MaxLen;

	for ( i = MaxLen = 0; Strings[i] != NULL; i++ )
	{
		Len = strlen( Strings[i] );
		if ( Len > MaxLen )
			MaxLen = Len;
	}
	return( MaxLen );
}
