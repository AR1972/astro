/***************************************************************************/
/*																									*/
/*	NUMSTRS.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns the number of strings in a array of strings. The last element   */
/* of the array must be a NULL pointer.                                    */
/*                                                                         */
/* int GetNumberStrings( char **Strings ) 											*/
/* 																								*/
/* ARGUMENTS:	Strings	- Array of pointers to strings							*/
/* RETURNS: 	int		- Total number of strings									*/
/* 																								*/
/* johnhe - 03-23-89																			*/
/***************************************************************************/

#include 	<stdio.h>

int GetNumberStrings( char **Strings )
{
   register      i;

	for ( i = 0; Strings[i] != NULL; i++ )
		;
   return( i );
}
