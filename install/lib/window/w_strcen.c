/***************************************************************************/
/*																									*/
/*	W_STRCEN.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns the display column which cause the string passed as an argument */
/* to be displayed centered on the screen. 										   */
/*                                                                         */
/* int CenterStr( char *szString )														*/
/* 																								*/
/* ARGUMENTS:	szString - pointer to a string                              */
/* RETURNS: 	int column number which causes the string to be centered		*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include		<string.h>
#include		<bios_io.h>

int CenterStr( char *szString )
{
	unsigned 					cch;
	extern unsigned char 	ScreenWidth;

   cch = strlen( szString );
	return( (int)(((VideoGetWidth() - cch) / 2) + (cch & 1 ? 1 : 0)) );
}
