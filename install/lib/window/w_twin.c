/***************************************************************************/
/*																									*/
/*	W_TWIN.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Displays a titled window in the screen as define in the window          */
/* definition structure and displays the window's title.                   */
/*                                                                         */
/*	void  PutTitledWindow( struct WindowStruct *Window, char *Title )			*/
/*                                                                         */
/* ARGUMENTS:	Window	- Ptr to initialized window definition structure.  */
/* 				Title 	- Ptr to a title string                            */
/*	RETURNS:		void																			*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include 	<string.h>

#include 	<alias.h>
#include		<window.h>
#include		<bios_io.h>

void  PutTitledWindow( struct WindowStruct *Window, char *Title )
{
   int      Width;
   
	Width = (int)((Window->Right - Window->Left)) + 1;
   PutWindow( Window );    /* Draw the window */

		/* Draw the title Seperator */
#ifdef JAPAN	/* if KEISEN */
	VideoPutDBCharAttr( (int)Window->Top+2, (int)Window->Left, K_THIN_LEFT_TEE,
							(int)Window->BorderColor );
	VideoDupDBCharAttr( (int)Window->Top+2, (int)Window->Left+2, K_THIN_HORIZ_LINE,
							(int)Window->BorderColor,
                     (Width - 4)/2 );
	VideoPutDBCharAttr( (int)Window->Top+2, (int)Window->Right-1, K_THIN_RIGHT_TEE,
							(int)Window->BorderColor );

#else
	VideoPutCharAttr( (int)Window->Top+2, (int)Window->Left, 'Ã',
							(int)Window->BorderColor );
	VideoDupCharAttr( (int)Window->Top+2, (int)Window->Left+1, 'Ä',
							(int)Window->BorderColor,
                     Width - 2 );
	VideoPutCharAttr( (int)Window->Top+2, (int)Window->Right, '´',
							(int)Window->BorderColor );

#endif

		/* Display the title */
	VideoPutsRowCol( (int)Window->Top+1, (int)Window->Left +
						  ((Width - (int)strlen( Title )) / 2), Title );

}

