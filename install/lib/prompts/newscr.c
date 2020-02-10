/***************************************************************************/
/*																									*/
/*	NEWSCR.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Clears the screen and displays the messages from the text file as			*/
/* specified by the Text argument and then displays the help line as 		*/
/* specified by the HelpFlags argument.												*/
/*																									*/
/*	void NewScreen( unsigned Text, unsigned Help )									*/
/*																									*/
/*	ARGUMENTS:	Text		- Offset of message in the text file					*/
/* 				HelpFlags- Bit flags for desired help line messages			*/
/*	RETURNS:		int		- Number of strings displayed.							*/
/*																									*/
/* johnhe - 02-24-89																			*/
/***************************************************************************/

#include 	<alias.h>
#include		<strlib.h>
#include		<window.h>

void GetMessage( char *apszText[], unsigned Text );

int NewScreen( unsigned Text, unsigned HelpFlags )
{
	char		*apszText[ MAX_STRINGS ];

	GetMessage( apszText, Text );
	WorkAreaCls();
   DisplayText( apszText, TITLE_ROW );
	HelpLine( (int)HelpFlags );

	return( GetNumberStrings( apszText ) );
}


