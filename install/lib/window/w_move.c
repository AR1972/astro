/***************************************************************************/
/*																									*/
/*	W_MOVE.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Moves a window from the screen to a buffer if SaveRestore == SAVE       */
/* else it moves from the buffer to the screen.										*/
/*                                                                         */
/*	void  WindowMove( struct WindowStruct *Wind, char *Buffer,					*/
/*							int iSaveRestore )												*/
/*                                                                         */
/* ARGUMENTS:	Wind 			- Ptr to a completed window definition struc		*/
/* 				Buffer 		- Ptr to data area large enough to hold the		*/
/*									  screen contents  										*/
/* 				SaveRestore - signals copy from screen to buffer or buffer	*/
/*									  to screen													*/
/*									  SAVE = copy from screen								*/
/*									  RESTORE = copy to screen         					*/
/*	RETURNS:		void																			*/
/*																									*/
/* johnhe - 03/15/89																			*/
/* johnhe - 02/12/90 - Added shadow window support									*/
/***************************************************************************/

#include 	<alias.h>
#include		<window.h>
#include		<bios_io.h>

void  WindowMove( struct WindowStruct *Wind, char *Buffer,int iSaveRestore )
{
	register		iRow;
	register		iCol;
	int			Width;
	int			Len;
	int			BytesPerLine;

	Len = Wind->Bottom + (Wind->IsShadow ? 1 : 0);
	Width = (Wind->Right - Wind->Left) + 1;
	Width += (Wind->IsShadow ? 2 : 0);
	BytesPerLine = Width << 1;
														/* Copy a line at a time */

	for ( iRow = Wind->Top, iCol = Wind->Left; iRow <= Len; 
			iRow++, Buffer += BytesPerLine )
	{
		if ( iSaveRestore == SAVE )
			VideoGetBlock( iRow, iCol,	Buffer, Width );
		else
			VideoPutBlock( iRow, iCol,	Buffer, Width );

	}
}
