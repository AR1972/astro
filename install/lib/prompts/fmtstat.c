/***************************************************************************/
/*																									*/
/*	FMTSTAT.C																					*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* This function is passed as an argument to the floppy disk format			*/
/* which will call it indirectly to allow displaying the status of the 		*/
/* formatting operation. The uTrack and uHead arguments specify the status	*/
/* and are also used to indicate if the temporary window should be			*/
/* displayed or removed.																	*/
/*																									*/
/*	void FmtStat( unsigned uTrack, unsigned uHead )									*/
/*																									*/
/*	ARGUMENTS:	uTrack	- Specifies the current track being formatted or	*/
/*								  if -1 signals a popup window should be removed	*/
/*					uHead		- Specified the cureent head being formatted or		*/
/*								  if -1 signals a popup window should be displayed	*/
/*	RETURNS:		void																			*/
/*																									*/
/*	EXTERNS:		FmtStatText	- Declared in EXTERN.C as FMT_STAT_TEXT.			*/
/*																									*/
/* johnhe - 12/29/89																			*/
/***************************************************************************/

#include		<stdio.h>
#include 	<stdlib.h>
#include 	<string.h>

#include		<alias.h>
#include		<bios_io.h>
#include		<window.h>
#include		<strlib.h>

void FmtStat( unsigned uTrack, unsigned uHead )
{
	char						szStatus[25];
	int						Percent;
	int						Offset;

	static char				*OldScrn;
	static char				*Strings[ MAX_STRINGS ];
	static unsigned		uTotalTrks;
	static unsigned long	Cursor;
	static WINDOW			Window;

	extern UINT 			FmtStatText;

	*szStatus = EOL;

				/* If uHead == -1 we need to allocate some memory and then	*/
				/* save the popup area and then display the pop window.		*/

	if ( uHead == (UINT)-1 )
	{
		uTotalTrks = uTrack;
																			/*lint -e727 */
		if ( Strings[0] == NULL )
			GetMessage( Strings, FmtStatText );
																			/*lint +e727 */
		GetWindowInfo( Strings, &Window );

							/* Note: Add 2 extra rows and colums just in	*/
							/* case shadow windows are inabled				*/

		OldScrn = GetMemory( (UINT)((Window.Bottom - Window.Top + 3) *
									(Window.Right - Window.Left + 3) * 2) );
		Cursor = SaveCursor();
		VideoCursOff();
		WindowSave( &Window, OldScrn );
		PromptWindow( Strings + 1, NULL, GetPromptColor(), OldScrn );
		strcat( szStatus, Strings[0] );
		PadStr( szStatus, SPC, 21 );
	}

	else if ( uTrack == (UINT)-1	)
	{ 													/* Remove window and restore	*/
		WindowRestore( &Window, OldScrn ); 	/* the original screen			*/
		RestoreCursor( Cursor );
		FreeMemory( OldScrn );
		*szStatus = EOL;
		PadStr( szStatus, SPC, 21 );
	}

	else 												/* Update status in window		*/
	{
		strcpy( szStatus, "     " );
		Percent = (int)( (long)(uTrack + 1) * 100L / (long)(uTotalTrks) );
		Offset = Percent > 9 ? ( Percent > 99 ? 1 : 2 ) : 3;
		itoa( Percent, szStatus + Offset, 10 );
	}

	VideoPutsRowCol( 24, 58, szStatus );
}
