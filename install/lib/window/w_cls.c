/***************************************************************************/
/*																									*/
/*	W_CLS.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Function to clear the work area of the screen. The work are is defined  */
/* as the area below the screen header and above the help prompt at the    */
/* bottom of the screen.                                                   */
/*																									*/
/*	void WorkAreaCls( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include 	<alias.h>
#include 	<window.h>
#include		<bios_io.h>

void WorkAreaCls( void )
{
	extern unsigned char		ScreenWidth;

   VideoScrollDn( TITLE_ROW, 0, 23, VideoGetWidth() - 1,
						0, GetBackGroundColor() );
}
