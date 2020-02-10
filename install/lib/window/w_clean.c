/***************************************************************************/
/*																									*/
/*	W_CLEAN.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Function to clear the screen and restore the cursor to normal size and	*/
/* top left corner position.																*/
/*																									*/
/*	void VideoCleanup( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include 	<alias.h>
#include		<bios_io.h>

void VideoCleanup( void )
{
	VideoCls( 7 );
	VideoSetRowCol( 0, 0 );
	VideoNormalCurs();
}
