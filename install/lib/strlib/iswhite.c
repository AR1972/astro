/***************************************************************************/
/*																									*/
/*	ISWHITE.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns TRUE is the argument character is a valid DOS command line		*/
/* delimiter.																					*/
/* 																								*/
/* int IsWhite( char ch )																	*/
/* 																								*/
/* ARGUMENTS:	ch 	- Character to be tested										*/
/* RETURNS: 	int	- TRUE is arg is a DOS cmd line delimiter else FALSE	*/
/* 																								*/
/* johnhe - 12/01/89																			*/
/***************************************************************************/

#include    <ctype.h>

int IsWhite( char ch )
{
	return( isspace( ch ) || ch == ';' || ch == '=' || ch == ',' );
}
