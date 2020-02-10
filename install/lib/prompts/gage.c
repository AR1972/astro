/***************************************************************************/
/*																									*/
/*	GAGE.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Functions for displaying and updating the gage on the screen.				*/
/* color as defined by the macro GetHeaderColor().									*/
/* 																								*/
/* johnhe - 07/25/90																			*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include 	<string.h>

#include 	<alias.h>
#include		<window.h>
#include		<bios_io.h>

/***************************************************************************/

#ifdef JAPAN	/* if KEISEN */
static WINDOW	Gage = { 17, 20, 9, 70, 1, 0, 0, 0 }; /* Gage description	*/
#else
static WINDOW	Gage = { 17, 20, 10, 69, 1, 0, 0, 0 }; /* Gage description	*/
#endif

static char		*szCurStatus[ 2 ];
long	 			lTotalBytes;
long				lByteCount;

/***************************************************************************/
/* Displays and updates the percentage compelete gage on the screen. The	*/
/* function must first be called with a argument of 0 percent to				*/
/* initialize the display.																	*/
/*																									*/
/*	void UpdateGage( int PerCent )														*/
/*																									*/
/*	ARGUMENTS:	PerCent	- Percent to be displayed									*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

#ifndef JAPAN			/* ### if Not JAPAN ### */

void UpdateGage( int PerCent )
{
	char				szPerCent[ 10 ];
	char				*szPtr;
	char				*szRevPtr;
	register			NumCols;
	register			TotCols;
	int				i;

	if ( szCurStatus[0] == NULL)					/* See if need to initialize	*/
		DisplayGage();									  

	itoa( PerCent, szPerCent, 10 );

	szPtr = strchr( szCurStatus[0], PERCENT ) - 1;	/* Ptr to end of #		*/
	szRevPtr = strchr( szPerCent, EOL ) - 1;	/* Ptr to last char of % str	*/

	while( szRevPtr >= szPerCent )				/* Copy the # text backwards	*/
		*(szPtr--) = *(szRevPtr--);

	while( szPtr >= szCurStatus[0] )				/* Pad to begining of str		*/
		*(szPtr--) = SPC;
															/* Calc length of bar			*/	
	TotCols = (int)(Gage.Right - Gage.Left - 1);

	NumCols = (PerCent > 0 ? (TotCols * PerCent) / 100 : 0);

			/* Display the percent string, clear the box and then update it	*/

	VideoPutsRowCol( Gage.Top - 1, Gage.Left, szCurStatus[0] );

															/* Display the red bar			*/
	if ( NumCols != 0 )
		for ( i = (int)Gage.Top + 1; i < (int)Gage.Top + 3; i++ )
			VideoDupCharAttr( i, Gage.Left + 1 , BLK, GetGageColor(), NumCols );
															/* Clear the end of the bar	*/
															/* if any space is remaining	*/
	if ( TotCols != 0 && (Gage.Left + 1 + NumCols) < Gage.Right )
	   VideoScrollDn( Gage.Top + 1 , Gage.Left + 1 + NumCols,
							Gage.Bottom - 1,  Gage.Right - 1, 0, GetBoxColor() );
}

#else	/* if KEISEN */		/* ### if JAPAN ### */

void UpdateGage( int PerCent )
{
	char				szPerCent[ 10 ];
	char				*szPtr;
	char				*szRevPtr;
	register			NumCols;
	register			TotCols;
	int				i;

	if ( szCurStatus[0] == NULL)					/* See if need to initialize	*/
		DisplayGage();									  

	itoa( PerCent, szPerCent, 10 );

	szPtr = strchr( szCurStatus[0], PERCENT ) - 1;	/* Ptr to end of #		*/
	szRevPtr = strchr( szPerCent, EOL ) - 1;	/* Ptr to last char of % str	*/

	while( szRevPtr >= szPerCent )				/* Copy the # text backwards	*/
		*(szPtr--) = *(szRevPtr--);

	while( szPtr >= szCurStatus[0] )				/* Pad to begining of str		*/
		*(szPtr--) = SPC;
															/* Calc length of bar			*/	
/* #ifdef JAPAN */
	TotCols = (int)(Gage.Right - Gage.Left - 1 - 2);
/* #endif */

	NumCols = (PerCent > 0 ? (TotCols * PerCent) / 100 : 0);

			/* Display the percent string, clear the box and then update it	*/

/* #ifdef JAPAN */
	VideoPutsRowCol( Gage.Top - 1, Gage.Left + 1, szCurStatus[0] );
/* #endif */
															/* Display the red bar			*/
	if ( NumCols != 0 )
		for ( i = (int)Gage.Top + 1; i < (int)Gage.Top + 3; i++ )
/* #ifdef JAPAN */
			VideoDupCharAttr( i, Gage.Left + 2 , SPC, GetGageColor(), NumCols );
/* #endif */
															/* Clear the end of the bar	*/
															/* if any space is remaining	*/
/* #ifdef JAPAN */
	if ( TotCols != 0 && (Gage.Left + 1 + NumCols + 1) < (Gage.Right - 1) )
	   VideoScrollDn( Gage.Top + 1 , Gage.Left + 1 + NumCols + 1,
							Gage.Bottom - 1,  Gage.Right - 1 - 1, 0, GetBoxColor() );
/* #endif */
}

#endif			/* ### end if JAPAN ### */

/***************************************************************************/
/* Initializes the gage screen and sets the total byte count to zero.		*/
/*																									*/
/*	void DisplayGage( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/* RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void DisplayGage( void )
{
	extern unsigned		PercentComplete;
	extern unsigned		CopyText;

	GetMessage( szCurStatus, PercentComplete );
	Gage.Color = Gage.BorderColor = GetBackGroundColor();

	NewScreen( CopyText, 0 );
	PutWindow( &Gage );
}

/***************************************************************************/
/* Updates the global byte count and updates the gage with the percent of	*/
/* total bytes done.																			*/
/*																									*/
/*	void UpdateByteCount( long lBytes )													*/
/*																									*/
/*	ARGUMENTS:	lBytes	- Bytes to add to count										*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void UpdateByteCount( long lBytes )
{
	register		Percent;

	if ( lBytes < 0L )
		lByteCount = 0L;
	else
		lByteCount += lBytes;

	Percent = (int)(lByteCount / (lTotalBytes / 100L));

	if ( Percent > 99 )
		Percent = 99;
	else if ( Percent < 0 )
		Percent = 0;

	UpdateGage( Percent );
}

/***************************************************************************/
/* Function which gives far call access to UpdateByteCount();					*/
/*																									*/
/*	void far	fUpdateByteCount( long lBytes )											*/
/*																									*/
/*	ARGUMENTS:	lBytes	- Bytes to add to count										*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/
void far	fUpdateByteCount( long lBytes )
{
	UpdateByteCount( lBytes );
}

