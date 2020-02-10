/***************************************************************************/
/*																									*/
/*	HELP.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Displays the help screen associated with the value on the help stack		*/
/* pointed to by HelpStackPtr. Waits for the user to press a key and then	*/
/* restores the original screen. Functions which want to use help should	*/
/* push a help indentifier on the help stack with the macro PushHelp( x )	*/
/* and then call GetChar() to get input from the user. If F1 is pressed 	*/
/* GetChar will this function. If the value of *HelpStackPtr == -1 the 		*/
/* function will return without displaying any text.								*/
/*                                                                         */
/*	void Help( void )																			*/
/*                                                                         */
/* ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*                                                                         */
/*	EXTERNS:		HelpMemErrText	- Declared in EXTERN.C								*/
/*                                                                         */
/* johnhe - 12/29/89																			*/
/***************************************************************************/

#include 	<stdio.h>
#include 	<stdlib.h>

#include 	<alias.h>
#include 	<window.h>
#include 	<bios_io.h>

/***************************************************************************/
int GetChar( void );

void Help( void )
{
	char					*apszError[ ERROR_LINES ];
	char					*ScrnBuf;
	extern unsigned	HelpMemErrText;

	GetMessage( apszError, HelpMemErrText	);		/* Get mem error message	*/

	if ( *(HelpStackPtr-1) != 0xffff	)				/* Is valid help on stack?	*/
	{
		if ( (ScrnBuf = malloc( 5000 )) != NULL )	/* Is enough memory? 		*/
		{
			free( ScrnBuf );								/* Free this block			*/
			DisplayHelp();									/* Show the help				*/
		}
		else
			Error( apszError );							/* Too little memory 		*/
	}
}

/***************************************************************************/
/* Saves the contents of the original screen and then diplays a help			*/
/* screen. Must save the contents of the screen as well as the cursor size	*/
/* and location.																				*/ 
/*                                                                         */
/*	void DisplayHelp( void )																*/
/*                                                                         */
/* ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*                                                                         */
/***************************************************************************/

void DisplayHelp( void )
{
	char				*apszText[ MAX_STRINGS ];	/* Help strings	*/
	char				*ScrnBuf;					/* Storage for orignal screen	*/
	int 				OldHelp;
	long				Cursor;
	static WINDOW	Wind = { 0, 24, 0, 79, 0, 0, 0 }; /* Screen description	*/

	ScrnBuf = GetMemory( 4000 );				/* Allocate size of screen	*/
	Cursor = SaveCursor();
	VideoCursOff();								/* Turn cursor off			*/

	WindowMove( &Wind, ScrnBuf, SAVE );		/* Save current screen		*/

	GetMessage( apszText, *(HelpStackPtr - 1));/* Display help screen	*/
	VideoCls( GetBackGroundColor() );
	DisplayScreenHeader( 1 );
	DisplayText( apszText, TITLE_ROW );

	OldHelp = GetHelpFlags(); 					/* Save original help flags*/
	HelpLine( PREV_HLP | EXIT_HLP );

	PushHelp( 0xffff );							/* Can't have help on help	*/
	while( GetChar() != ESC )					/* Wait for <RETURN>			*/
		;
	PopHelp();										/* Turn help back on			*/
														/* Restore original screen */
	WindowMove( &Wind, ScrnBuf, RESTORE );
	RestoreCursor( Cursor );
	HelpLine ( OldHelp );						/* Restore orig. help flags*/

	FreeMemory( ScrnBuf );
}
