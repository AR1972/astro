/***************************************************************************/
/*																									*/
/*	W_PROMPT.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Displays an untitled window on the screen and prompts waits for input   */
/* from the user that matches an entry in a validation array specified by  */
/* the caller. The length and width of the window are calculated based on  */
/* number and length of the strings passed to the function.                */
/*                                                                         */
/*	int PromptWindow( char **String, int *ValidResponse, int wColor, 			*/
/*                  char *Buffer )														*/
/*                                                                         */
/* ARGUMENTS:	String 			- Array of ptrs to strings to be displayed	*/
/*										  in the window,the last element of the		*/
/*										  array must be a NULL pointer           		*/
/* 				ValidResponse 	- Array of ints which specify valid input		*/
/*										  from the user. If the first element of the	*/
/*										  array is 0 the function will not validate	*/
/*										  the input and will return after any key is	*/
/*										  pressed. If ValidResponse == NULL no input	*/
/*										  is waited for and the window is left			*/
/*										  displayed												*/
/*					wColor 			- color of the window to be displayed        */
/* 				Buffer 			- a ptr to buffer large enough to hold the	*/
/*										  contents of the orginal display under the	*/
/*										  window, if Buffer == NULL the function		*/
/*										  will allocate a buffer to hold the data.   */
/* RETURNS: 	int				- Character input by the user                */
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include		<stdio.h>
#include 	<malloc.h>

#include		<alias.h>
#include		<window.h>
#include 	<bios_io.h>
#include		<string.h>

#ifndef DBCS			/* ### if Not DBCS ### */

int PromptWindow( char **String, int *ValidResponse, int wColor,
                  char *Buffer )
{
	extern unsigned char ScreenWidth;
	char						*OldScrn;
	register					i;
	register 				Row;
	int						Response;
	UINT						BufferSize;
	UL 						OldCursor;
	struct WindowStruct	Wind;


	GetWindowInfo( String, &Wind );		/* Init wind struct */
	Wind.Color = (char)wColor;
	Wind.BorderColor = (char)wColor;
	OldCursor = SaveCursor();					/* Save cursor position & size	*/
	VideoCursOff();								/* Blank the cursor					*/

		/* If passed a buffer or not saving window we don't need to allocate */

	if ( ValidResponse != NULL )
	{
		if ( Buffer == NULL )
		{
			BufferSize = (UINT)((Wind.Bottom - Wind.Top + 2 ) *
									  (Wind.Right - Wind.Left + 3) * 2 );
			OldScrn = GetMemory( BufferSize );
		}
		else
			OldScrn = Buffer;
		WindowSave( &Wind, OldScrn );
	}

	PutWindow( &Wind );

	for ( i = 0, Row = Wind.Top; String[i] != NULL; i++, Row++ )
		VideoPutsAttrRowCol( Row, CenterStr( String[i] ), String[i], wColor );

	if ( ValidResponse != NULL )
	{
		Response = GetResponse( ValidResponse );

		WindowRestore( &Wind, OldScrn );
		RestoreCursor( OldCursor );

		if ( Buffer == NULL )
			FreeMemory( OldScrn );

	}

	return( Response );
}

#else			/* ### if DBCS ### */

int PromptWindow( char **String, int *ValidResponse, int wColor,
                  char *Buffer )
{
	extern unsigned char ScreenWidth;
	char						*OldScrn;
	register					i;
	register 				Row;
	int						Response;
	UINT						BufferSize;
	UL 						OldCursor;
	struct WindowStruct	Wind;


	GetWindowInfo( String, &Wind );		/* Init wind struct */
	Wind.Color = (char)wColor;
	Wind.BorderColor = (char)wColor;
	OldCursor = SaveCursor();					/* Save cursor position & size	*/
	VideoCursOff();								/* Blank the cursor					*/

		/* If passed a buffer or not saving window we don't need to allocate */

	if ( ValidResponse != NULL )
	{
/* #ifdef DBCS */
		BufferSize = (UINT)((Wind.Bottom - Wind.Top + 2 ) *
							  (Wind.Right - Wind.Left + 3 + 2) * 2 );
		OldScrn = GetMemory( BufferSize );
		if ( Buffer != NULL)
			WindowSave( &Wind, Buffer );
		Wind.Left--,Wind.Right++;
		WindowSave( &Wind, OldScrn );
		Wind.Left++,Wind.Right--;
/* #endif */
	}

	PutWindow( &Wind );

#ifdef JAPAN	/* if KEISEN */
	for ( i = 0, Row = Wind.Top; String[i] != NULL; i++, Row++ )
	{
	    if (Row == Wind.Top && strlen(String[i]))
	    {
		if (CheckLead(Row, CenterStr(String[i])+strlen(String[i])-1))
		{
		    VideoPutCharRowCol(Row, CenterStr(String[i])+strlen(String[i]), SPC);
		    VideoPutCharRowCol(Row, CenterStr(String[i])+strlen(String[i])-1, SPC);
		}
	    }
	    VideoPutsAttrRowCol( Row, CenterStr( String[i] ), String[i], wColor );
	}
#else
	for ( i = 0, Row = Wind.Top; String[i] != NULL; i++, Row++ )
		VideoPutsAttrRowCol( Row, CenterStr( String[i] ), String[i], wColor );
#endif

	if ( ValidResponse != NULL )
	{
		Response = GetResponse( ValidResponse );
/* #ifdef DBCS */
		Wind.Left--,Wind.Right++;
		WindowRestore( &Wind, OldScrn );
		Wind.Left++,Wind.Right--;

		RestoreCursor( OldCursor );

			FreeMemory( OldScrn );
/* #endif */
	}

	return( Response );
}

#endif			/* ### end if DBCS ### */

