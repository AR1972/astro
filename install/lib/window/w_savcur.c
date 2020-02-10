/***************************************************************************/
/*																									*/
/*	W_SAVCUR.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/*	Returns the size and position of the cursor as an unsigned long value 	*/
/* with the position in the high byte and size in the low byte.				*/
/*																									*/
/*	unsigned long SaveCursor ( void )													*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/* RETURNS:		unsigned long	- Cursor screen position and size				*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include		<bios_io.h>

unsigned long SaveCursor ( void )

{
	unsigned long		CursorSizePos;

	CursorSizePos = (unsigned long)(VideoGetRowCol()) << 16;
	CursorSizePos += (unsigned long)(VideoGetCursSize());

	return( CursorSizePos );
}
