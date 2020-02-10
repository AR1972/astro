/***************************************************************************/
/*																									*/
/*	W_RSTCUR.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Displays the cursor at the screen position and size specified in the		*/
/* argument as an unsigned long with the  position in the high byte and		*/
/*	size in the low byte.																	*/
/*																									*/
/*	void RestoreCursor( unsigned long Cursor )										*/
/*																									*/
/*	ARGUMENTS:	Cursor	- Cursor size & position returned by SaveCursor()	*/
/* RETURNS:		void																			*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include		<bios_io.h>

void RestoreCursor( unsigned long Cursor )
{
	unsigned 		CursPos;
	unsigned			CursSize;

	CursPos  = (unsigned)(Cursor >> 16);
	CursSize = (unsigned)(Cursor & 0xffff);

	VideoSetRowCol( CursPos >> 8, CursPos & 0xff );
	VideoSetCursSize( CursSize );
}
