/***************************************************************************/
/*																									*/
/*	DISPLAY.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/*	Generic screen display functions.													*/
/* 																								*/
/* johnhe - 03/05/89																			*/
/***************************************************************************/

#include		<stdio.h>
#include 	<stdlib.h>
#include 	<string.h>

#include		<alias.h>
#include		<bios_io.h>
#include		<window.h>

/***************************************************************************/
/*	Displays multiple lines of text on the screen starting at the specified	*/
/* row. If the starting row is the title line the first string will be		*/
/* centered	horizonally on the screen.													*/
/*																									*/
/* void DisplayText( char *szText[], unsigned gfPrompts )						*/
/* 																								*/
/* ARGUMENTS:	szText		- array of pointers to strings. Last element		*/
/* 								  should be a NULL pointer. (max 9 strings)		*/
/* 				StartRow		- Screen row where text starts						*/
/*	RETURNS:		void																			*/
/*																									*/
/* johnhe - 03/05/89																			*/
/***************************************************************************/

void DisplayText( char **szText, int StartRow )
{
	int			i;									/* Loop counter						*/
	int			iRow;								/* Current row being displayed	*/

	for ( i = 0, iRow = StartRow; szText[i] != NULL; i++, iRow++ )
#if 0
		VideoPutsRowCol( iRow, START_COL, szText[i] );
#else
		VideoPutsAttrRowCol( iRow, START_COL, szText[i], GetBackGroundColor() );
#endif
}

/***************************************************************************/
/*	Displays a string centered horizonally on the screen starting at			*/
/* the position specified by the #define TITLE_ROW.								*/
/*																									*/
/*	void DisplayTitle( char *szTitle )													*/
/*																									*/
/*	ARGUMENTS:	szTitle	- Ptr to string to be displayed.							*/
/*	RETURNS:		void																			*/
/*																									*/
/* johnhe - 03/05/89																			*/
/***************************************************************************/

void DisplayTitle( char *szTitle )
{
	VideoPutsAttrRowCol( TITLE_ROW, CenterStr( szTitle ), szTitle,
								GetTitleColor() );
}


/***************************************************************************/
/* Displays a help line in reverse video on bottom line of the screen. The	*/
/* text displayed on the line is specified by the flags passed as an arg	*/
/* and defined by the string in the message file with the label				*/
/* HELP_LINE_TEXT each bit in the argument represents a line from the text	*/
/* group and are concatenated together and displayed as a single string.	*/
/*																									*/
/*	void HelpLine( int iFlags )															*/
/*																									*/
/*	ARGUMENTS:	iFlags	- Bit flags specifing the messages to display.		*/
/*	RETURNS:		void																			*/
/*																									*/
/*	EXTERNS:		HelpLineText - Declared in EXTERN.C									*/
/*																									*/
/* johnhe - 03/05/89																			*/
/***************************************************************************/

static int	iLastFlags;

#define		MAX_FLAGS		16
#define		MAX_HELP_ITEMS 16
#define		MAX_SCR_WIDTH	100

void HelpLine( unsigned iFlags )

{
	#define DIVIDER_COL 57

	register			i;
	char				szStr[ MAX_SCR_WIDTH ];
	static char		*apszText[ MAX_HELP_ITEMS ];
	extern UINT 	HelpLineText;

	if ( GetBackGroundColor() == 0x07 )
		iFlags &= (~KILL_COLOR);

	iLastFlags = iFlags;							/* Save flags for GetHelpFlags()	*/

			/* Load messages if this is first time called */
	if ( apszText[ 0 ] == NULL )
		GetMessage( apszText, HelpLineText );

	*szStr = 0;
	*(szStr+1) = 0;		/* For when iflags == 0 and we step over first char */
	for ( i = 0; i < MAX_FLAGS; i++, iFlags >>= 1 )
	{
		if ( iFlags & 1 )
			strcat ( szStr, apszText[i] );
	}
								/* Display line @ bottom of display */
								/* Use szStr + 1 to remove leading blank in the text*/

	VideoDupCharAttr( 24, 0, SPC, GetStatusColor(),	VideoGetWidth() ) ;
	VideoPutsRowCol( 24, 0, szStr + 1 );

	/* Display vertical divider unless messages would overlap divider. */

	if ( strlen( szStr ) <= DIVIDER_COL )
	{
#ifdef JAPAN	/* if KEISEN */
		VideoPutCharRowCol( 24, DIVIDER_COL-1, K_THIN_VERT_LINE >> 8 );
		VideoPutCharRowCol( 24, DIVIDER_COL, K_THIN_VERT_LINE & 0x00ff );
#else
		VideoPutCharRowCol( 24, DIVIDER_COL, '³' );
#endif
	}

	#undef DIVIDER_COL
}


/***************************************************************************/
/* Returns a bit flag of the current help line that is displayed on the 	*/
/* bottom of the screen.																	*/
/*																									*/
/*	int GetHelpFlags( void )																*/
/*																									*/
/*	ARGUMENTS:	void																			*/
/*	RETURNS:		int		- Bit flags specifing the current messages.			*/
/*																									*/
/* johnhe - 12/06/90																			*/
/***************************************************************************/

int GetHelpFlags( void )
{
	return( iLastFlags );
}


