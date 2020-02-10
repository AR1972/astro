/***************************************************************************/
/*																									*/
/*	W_CENTER.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Function to return the starting column of a string consisting of StrLen */
/* number of characters for the string to be centered on a line of the     */
/* current screen. The function will adjust current width of the screen.   */
/* 																								*/
/* int CenterLength( int iStrLen )														*/
/* 																								*/
/* ARGUMENTS:	int iStrLen -	The lenght of a string to be centered on 		*/
/*										the current display.									*/
/* RETURNS:		int 			-  Starting column to make the string centered	*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include		<bios_io.h>

int CenterLength(int iStrLen)
{
	extern	unsigned char	ScreenWidth;

	return( ((VideoGetWidth() - iStrLen) / 2) + (iStrLen & 1 ? 1 : 0) );
}
