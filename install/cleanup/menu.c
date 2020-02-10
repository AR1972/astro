/***************************************************************************/
/* 																								*/
/*	MENU.C																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* DOS 5.00 OEM install highlighted data selection functions					*/
/* 																								*/
/* Created 02-21-90 - johnhe																*/
/***************************************************************************/

#include		<stdio.h>
#include		<malloc.h>

#include		<alias.h>
#include		<bios_io.h>
#include		<window.h>
#include		<strlib.h>

#include		"global.h"

/***************************************************************************/

extern int	GetChar				( void );
extern int	GetCharExtension	( void );

/***************************************************************************/

static void InitMenu 		( int WindowTop );
static void HiLite			( int Option, int fOn );
static void OptionUp 		( void );
static void OptionDown		( void );
static void OptionLeft		( void );
static void OptionRight 	( void );
static int 	MyGetSelection	( int Top, int Cols, int DefaultOpt, char **apszList );

/***************************************************************************/

static char			**apszText;					/* Array of menu option strings	*/
static WINDOW		Win;							/* Menu window structure			*/
static int			Columns;						/* Number of option columns		*/
static int			StrsPerCol;					/* Number of strs in each column	*/
static int			ColWidth;					/* Number of chrs in each column	*/
static int			TotalStrs;					/* Total number of option strs	*/
static int			MaxOpt;						/* Max option number based 0		*/
static int			Current;						/* Current hi-lited option 		*/
static int			AddShadow = TRUE;			/* Used by MyPromptSelect() to	*/
														/* signal not a shadow window		*/

/***************************************************************************/
/* Allows the user to select an option from a menu by using the scroll		*/
/* bar. Blank entries can be included in the menu in which case the menu	*/
/* bar will skip over the blank selection.											*/
/* 																								*/
/*	int MyGetSelection( int Top, int Cols, int DefaultOpt, char **apszList )*/
/*																									*/
/*	ARGUMENTS:	Top		- Row on screen where top of menu window should be	*/
/*					DefaultOpt- The option number to hi-lite as the default		*/
/*					apszList	- Ptr to array of strings to display in the menu	*/
/* RETURNS: 	int	- selected option number, PREVIOUS or ABORT				*/
/* 																								*/
/***************************************************************************/

int MyGetSelection( int Top, int Cols, int DefaultOpt, char **apszList )
{
	#define			NO_SELECTION	-100		/* Arbitrary non-ascii char value*/

	register			Previous;					/* Last option highlighted			*/
	register			UserSelection;				/* Menu option selected				*/

	apszText = apszList;							/* in this module can access them*/
	Columns = Cols;
	Current = DefaultOpt;
	InitMenu( Top );

	VideoCursOff();
	Previous = Current;							/* Don't have a previous HiLite	*/
	HiLite( Current, ON );						/* HiLite the current option		*/

					/* Keep looping until user presses the <RETURN> key to make	*/
					/* a selection or <ESC> to abort the selection.					*/

	UserSelection = NO_SELECTION;
	do
	{
		if ( Previous != Current )
		{														/* HiLite proper selection */
			HiLite( Previous, OFF );
			HiLite( Current, ON );
			Previous = Current;
		}

		switch( GetChar() )								/* Get input from user	*/
		{

			case	ESC:											/* <ESC> key pressed		*/
				UserSelection = PREVIOUS;
				break;

			case CR:												/* <RETURN> key pressed	*/
				UserSelection = Current;
				break;

			case '\0':											/* Was an extended char	*/
				switch( GetCharExtension() )
				{
					case	UP:									/* Move up 1 row			*/
						OptionUp();
						break;

					case	DOWN:									/* Move down 1 row		*/
						OptionDown();
						break;

					case	LEFT:									/* Move left 1 column	*/
						OptionLeft();
						break;

					case	RIGHT:								/* Move right 1 column	*/
						OptionRight();
						break;

					default:
						break;									/* No exted char match	*/
				}
				break;

			default:												/* No reg char match 	*/
				break;
		}
	}
	while( UserSelection == NO_SELECTION  );			/* See if finished		*/

	VideoScrollDn ( Win.Top, Win.Left, Win.Bottom+1, Win.Right+2, 0 , GetBoxColor());
	
	return( UserSelection );
}

/***************************************************************************/
/* Initializes the menu window structure and displays the menu it on the	*/
/* screen. This function should only be called by MyGetSelection() because	*/
/* it relies on static globals which are initialized by MyGetSelection().	*/
/*																									*/
/*	static void InitMenu( int WindowTop )												*/
/*																									*/
/*	ARGUMENTS:	WindowTop	- Screen row where top of window should be		*/
/* RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

static void InitMenu( int WindowTop )
{
	register		Row;												/* Screen row			*/
	register		Col;												/* Screen col			*/
	int			i; 												/* Array indice		*/
	int			TotalWidth; 									/* Total wind width	*/

	ColWidth = (int)MaxStrLen( apszText ) + 2;
	TotalStrs = GetNumberStrings( apszText );
	MaxOpt = TotalStrs - 1;

	StrsPerCol = TotalStrs / Columns;
	TotalWidth = ColWidth * Columns;

#ifdef JAPAN
	TotalWidth += (TotalWidth & (UCHAR)1) ? (UCHAR)3 : (UCHAR)2;
#endif

	Win.Top = (char)WindowTop;
	Win.Bottom = (char)(WindowTop + StrsPerCol + 1);
	Win.Left = (char)CenterLength( TotalWidth + 2 );
	Win.Right = Win.Left + (char)TotalWidth + 1;

	Win.Color = Win.BorderColor = (char)GetBoxColor();
	Win.Type = 1;
	Win.IsShadow = (char)(AddShadow ? 1 : 0);

	PutWindow( &Win );

	for ( i = 0, Col = Win.Left + 2; Col < Win.Right; Col += ColWidth )
		for ( Row = WindowTop + 1;	Row < Win.Bottom && i < TotalStrs; Row++ )
#ifdef JAPAN
			VideoPutsRowCol( Row, Col+1, apszText[ i++ ] );
#else
			VideoPutsRowCol( Row, Col, apszText[ i++ ] );
#endif
						/* Need to be sure that default option not a 0 len str	*/

	if ( *(apszText[ Current ]) == EOL )
	{
		Current = 0;
		while( Current < TotalStrs && *(apszText[ Current ]) == EOL )
			Current++;
	}
}

/***************************************************************************/
/* Function to turn on or off the hi-lite on a specified option in the		*/
/* menu window. This function should only be called by MyGetSelection()		*/
/* because it relies on static globals which are initialized by				*/
/* MyGetSelection().																			*/
/*																									*/
/*	static void HiLite( int Option, int fOn )											*/
/*																									*/
/*	ARGUMENTS:	Option	- The option to apply the hi-lite to (options are	*/
/*								  base 0)														*/
/*					fOn		- If TRUE specifies the function should turn the	*/
/*								  hi-lite on else turns it off							*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

static void HiLite( int Option, int fOn )
{
	register		Row;												/* Screen row			*/
	register		Col;												/* Screen column		*/

															/* Get pos relative to window	*/
	if ( Option == 0 )								
		Row = Col = 0;												/* Divid by 0 check	*/
	else
	{
		Row = Option % StrsPerCol;
		Col = Option / StrsPerCol;
	}

	Row += Win.Top + 1;								/* Get pos relative to display*/
	Col *= ColWidth;

#ifdef JAPAN
	Col += Win.Left + 2;
#else
	Col += Win.Left + 1;
#endif

	VideoDupAttr( Row, Col,
					  fOn == ON ? GetBarColor() : GetBoxColor(),
					  ColWidth );
}

/***************************************************************************/
/* These functions handle the movement of the currently selected option.	*/
/*																									*/
/* NOTE: These functions call each other and will be come endlessly			*/
/*			recursive if all option strings are  0 lenght or if the current	*/
/*			option is on a row where all the options in the row are 0 lenght.	*/
/*			The only way this will happen is if the call of MyGetSelection()	*/
/*			specifies a row such as this, and a check is done for this			*/
/*			condition in InitMenu() and the default is changed to avoid the	*/
/*			problem. So there will only be a problem if all options are 0		*/
/*			length strings.																	*/
/*																									*/
/*	void OptionUp( void )																	*/
/*	void OptionDown( void )																	*/
/*	void OptionLeft( void )																	*/
/*	void OptionRight( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void OptionUp( void )
{
	if ( Current > 0 )
	{
		do
			Current--;
		while( Current > 0 && *(apszText[ Current ]) == EOL );
	}
}

void OptionDown( void )
{
	if ( Current < MaxOpt )
	{
		do
			Current++;
		while( Current < MaxOpt && *(apszText[ Current ]) == EOL );
	}
}

void OptionLeft( void )
{
	register		Tmp;

	Tmp = Current - StrsPerCol;
	if ( Columns > 1 && Tmp >= 0 )
		Current = Tmp;
}

void OptionRight( void )
{
	register		Tmp;

	Tmp = Current + StrsPerCol;
	if ( Columns > 1 && Tmp <= MaxOpt )
		Current = Tmp;
}

/***************************************************************************/
/* Displays a prompt window on the screen which contains text and a			*/
/* selection box with a variable number of options. The selection box is	*/
/* within the prompt window with the prompt window centered vertically and	*/
/* horizonally on the screen.																*/
/*																									*/
/*	int MyPromptSelect( char *szStrings[], char *szOpts[], int optdflt )		*/
/*																									*/
/*	ARGUMENTS:	szStrings	- Array of ptrs to prompt strings					*/
/*					szOpts		- Array of ptrs to options for selection box		*/
/*	RETURNS:		int			- User selection based 0 or PREVIOUS is <ESC>	*/
/*																									*/
/***************************************************************************/

int MyPromptSelect( char *szStrings[], char *szOpts[], int optdflt )
{
	static char		*szNull = ""; 						/* Zero length string		*/
	char				*szText[ MAX_STRINGS ];			/* Prompt window strs		*/
	char				*OldScrn;							/* Buf to hold orig scrn	*/
	register			i;										/* General purpose			*/
	unsigned char	OldBoxColor;						/* Orig box color				*/
	unsigned char	OldBarColor;						/* Orig bar color				*/
	unsigned long	Cursor;								/* Orig cusor size & pos	*/
	WINDOW			Window;								/* Prompt window structure	*/

	for ( i = 0; i < MAX_STRINGS; i++ )				/* Build array of ptrs to	*/
		szText[i] = szNull;								/* zero length strs			*/

	for ( i = 0; szStrings[i] != NULL; i++ )		/* Copy the prompt strings */
		szText[i] = szStrings[i];

						
						
	i++;								/* After this point i + top of prompt window	*/
	i++;								/* will be top of the selection box				*/

	szText[ GetNumberStrings( szOpts ) + i + 2 ] = NULL;

								/* Determine prompt wind size and allocate a scrn	*/
								/* buffer, save old scrn and display the prompt		*/

	GetWindowInfo( szText, &Window );
	OldScrn = GetMemory( (UINT)((Window.Bottom - Window.Top + 2 ) *
								(Window.Right - Window.Left + 3) * 2 ));
	Cursor = SaveCursor();
	VideoCursOff();

   WindowSave( &Window, OldScrn );
	PromptWindow( szText, NULL, GetPromptColor(), NULL );

												/* Selection box doesn't need a shadow	*/
	AddShadow = FALSE;					/* but the color has to be changed to	*/
	OldBoxColor = GetBoxColor();		/* to match the prompt window color		*/
	SetBoxColor( GetPromptColor() );

	OldBarColor = GetBarColor();
	SetBarColor( GetStatusColor() );
	
	i = MyGetSelection( Window.Top + i, 1, optdflt, szOpts );

	SetBarColor( OldBarColor );
	SetBoxColor( OldBoxColor );
	AddShadow = TRUE;

   WindowRestore( &Window, OldScrn );
	RestoreCursor( Cursor );
	FreeMemory( OldScrn );

	return( i );										/* Return user's selection		*/
}

