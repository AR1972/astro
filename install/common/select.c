/***************************************************************************/
/*                                                                      	*/
/* SELECT.C																						*/
/*                                                                      	*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                      	*/
/* DOS 4.01 retail upgrade highlighted data selection functions         	*/
/*                                                                      	*/
/* Created 890305 - johnhe                                              	*/
/***************************************************************************/

#include		<stdio.h>
#include		<malloc.h>

#include		<alias.h>
#include		<bios_io.h>
#include		<window.h>
#include		<strlib.h>


extern int	GetChar				( void );
extern int  GetCharExtension	( void );


extern  int PromptSelect(char * *szStrings,char * *szOpts);
extern  int GetSelection(char * *apszList,struct InputDef *NewDef);
static  void DisplayList(void );
static  void HiLite(int Current,int fOn);

int gDisplaySelectionList = TRUE;

/*************************************************************************/

static struct InputDef	*Def;
static char					**apszText;

/*************************************************************************/

/*
struct InputDef
{
	unsigned char		Top;
	unsigned char		Bottom;
	unsigned	char		Left;
	unsigned	char		Right;
	unsigned	char		Len;
	unsigned char		First;
	unsigned char		Total;
	unsigned	char		Current;
};
*/

/***************************************************************************/
/* Allows the user to select an option from a menu by using the scroll		*/
/* bar. Blank entries can be included in the menu in which case the menu	*/
/* bar will skip over the blank selection. The menu is also scrollable so	*/
/* that the total number of options don't have to fit in a fixed size		*/
/* menu.																							*/
/*                                                                         */
/*	int GetSelection( char **apszList, struct InputDef *NewDef  )				*/
/*																									*/
/*	ARGUMENTS:	apszList	- Ptr to array of strings to display in the menu	*/
/*					NewDef	- A menu definition structure which defines the		*/
/*								  size and position of the menu and the currently	*/
/*								  highlighted option.										*/
/* RETURNS: 	int	- selected option number, PREVIOUS or ABORT				*/
/*                                                                         */
/***************************************************************************/

int GetSelection( char **apszList, struct InputDef *NewDef  )
{
	#define			NO_SELECTION	-100		/* Arbitrary non-ascii char value*/
	int				PrevHiLite;					/* Last option highlighted			*/
	int				Input;						/* Value from the keyboard			*/
	int				UserSelection;				/* Menu option selected				*/


	Def = NewDef;									/* Set global values so all funcs*/
	apszText = apszList;							/* in this module can access them*/

	if (gDisplaySelectionList)
		DisplayList();								/* Display the menu					*/

   VideoCursOff();
	PrevHiLite = Def->Current;					/* Don't have a previous HiLite	*/
	HiLite( Def->Current, ON );			/* HiLite the current option		*/

					/* Keep looping until user presses the <RETURN> key to make	*/
					/* a selection or <ESC> to abort the selection.					*/

	UserSelection = NO_SELECTION;
   do
   {
		if ( PrevHiLite != Def->Current )
		{														/* HiLite proper selection */
			HiLite( PrevHiLite, OFF );
			HiLite( Def->Current, ON );
			PrevHiLite = Def->Current;
		}

      switch( Input = GetChar() )						/* Get input from user	*/
		{

			case	ESC:											/* <ESC> key pressed		*/
				UserSelection = PREVIOUS;
				break;

			case CR:												/* <RETURN> key pressed	*/
				UserSelection = Def->Current;
				break;

      	case '\0':											/* Was an extended char	*/
				Input = GetCharExtension();

				if ( Input == UP && Def->Current > 0)
				{								  					/* Move up an option		*/
					do
						Def->Current--;
					while( Def->Current > 0 &&
							apszList[ Def->Current ][0] == 0 );
				}
				else if ( Input == DOWN && Def->Current < (Def->Total - 1) )
				{													/* Move down  an option	*/
					do
						Def->Current++;
					while( Def->Current < (Def->Total - 1) &&
							apszList[ Def->Current ][0] == 0 );
				}
				break;

			default:
				break;

		}
	}
	while( UserSelection == NO_SELECTION );

	return( UserSelection );
}

/***************************************************************************/
/* Displays a menu on the screen as defined in the static global Def.		*/
/***************************************************************************/

static void DisplayList( void )
{
	int		Row;							/* Current screen row	*/
	int		Index;						/* String array index	*/
	int		BoxDif;
	int		EntryDif;
	WINDOW	Box;

	Def->Total = (unsigned char)(GetNumberStrings( apszText ));
	Def->First = Def->Current;
	Def->Len   = Def->Right - Def->Left + 1;
	
						/* Make user list fills the available window */

	BoxDif = Def->Bottom - Def->Top;
	EntryDif = Def->Total - Def->First;

	if ( BoxDif >= EntryDif )
		Def->First -= (unsigned char)(BoxDif - EntryDif + 1);

	if ( Def->First < 0 || Def->First > Def->Total )
		Def->First = 0;

						/* Display the box area */
	Box.Top = Def->Top - 1;
	Box.Bottom = Def->Bottom + 1;
	Box.Left = Def->Left - 1;
	Box.Right = Def->Right + 1;
	Box.Color = Box.BorderColor = GetBoxColor();
	Box.Type = 1;
	Box.IsShadow = 0;

	PutWindow( &Box );

						/* Display all the strings that fit in the box */
	for ( Row = Def->Top, Index = Def->First;
			Row <= Def->Bottom && Index < Def->Total;
			Index++, Row++ )
#ifdef JAPAN	/* if KEISEN */
		VideoPutsRowCol( Row, Def->Left + 2, apszText[ Index ] );
#else
		VideoPutsRowCol( Row, Def->Left + 1, apszText[ Index ] );
#endif
}		

/***************************************************************************/

static void HiLite( int Current, int fOn )

{
	int			Row; 				/* Screen row to HiLite 		*/
	int			Scroll;			/* Shows if screen gets scrolled	*/

	Row = Def->Top + Current - Def->First;
	Scroll = TRUE;					/* Assume we have to scroll		*/

	if ( Row < Def->Top )				/* Check for scroll down	*/
	{
		VideoScrollDn( Def->Top, Def->Left, Def->Bottom, Def->Right, 1,
							GetBoxColor() );
		Row++;
		Def->First--;
	}

	else if ( Row > Def->Bottom )		/* Check for scroll up		*/
	{
		VideoScrollUp( Def->Top, Def->Left, Def->Bottom, Def->Right, 1,
							GetBoxColor() );
		Row--;
		Def->First++;
	}

	else
		Scroll = FALSE;

#ifdef JAPAN	/* if KEISEN */
	if ( Scroll == TRUE )
		VideoPutsRowCol( Row , Def->Left + 2, apszText[ Current ] );

	VideoDupAttr( Row, Def->Left,
					  fOn == ON ? GetBarColor() : GetBoxColor(),
					  Def->Len-1 );
#else
	if ( Scroll == TRUE )
		VideoPutsRowCol( Row , Def->Left + 1, apszText[ Current ] );

	VideoDupAttr( Row, Def->Left,
					  fOn == ON ? GetBarColor() : GetBoxColor(),
					  Def->Len );
#endif
}

/***************************************************************************/
/* Displays a prompt window on the screen which contains text and a			*/
/* selection box with a variable number of options. The selection box is	*/
/* within the prompt window with the prompt window centered vertically and	*/
/* horizonally on the screen.																*/
/*																									*/
/*	int PromptSelect( char *szStrings[], char *szOpts[] )							*/
/*																									*/
/*	ARGUMENTS:	szStrings	- Array of ptrs to prompt strings					*/
/*					szOpts		- Array of ptrs to options for selection box		*/
/*	RETURNS:		int			- User selection based 0 or PREVIOUS is <ESC>	*/
/*																									*/
/***************************************************************************/

int PromptSelect( char *szStrings[], char *szOpts[] )
{
	char							*szText[ 25 ];
	char							*OldScrn;
	static char					*szNull = "";
	int							OptsOffset;
	int							TextOffset;
	int							i;
	int							iSelection;
	int							MaxLength;
	int							iHelpFlags;
	static unsigned long		Cursor;

	struct InputDef			Def;
	static WINDOW				Window;
	WINDOW						Box;

	for ( TextOffset = 0; szStrings[ TextOffset ] != NULL; TextOffset++ )
		szText[ TextOffset ] = szStrings[ TextOffset ];

	szText[ TextOffset++ ] = szNull;
	szText[ TextOffset++ ] = szNull;

	OptsOffset = TextOffset;
	for ( i = 0; szOpts[ i ] != NULL; i++, TextOffset++ )
		szText[ TextOffset ] = szOpts[ i ];

	szText[ TextOffset++ ] = szNull;
	szText[ TextOffset ] = NULL;

	GetWindowInfo( szText, &Window );

	OldScrn = GetMemory( (Window.Bottom - Window.Top + 2 ) *
								(Window.Right - Window.Left + 3) * 2 );

	Def.Top = Window.Top + OptsOffset;
	Def.Bottom = Def.Top + i - 1;

	MaxLength = MaxStrLen( szOpts );
	Def.Left = (UCHAR)(CenterLength( MaxLength ));
	Def.Right = Def.Left + MaxLength + 1;
	Def.Total = 2;
	Def.Current = 0;
	Def.First = 0;
	Def.Len = 0;
	Def.First = 0;

	Box.Top = Def.Top - 1;
	Box.Bottom = Def.Bottom + 1;
	Box.Left = Def.Left - 1;
	Box.Right = Def.Right + 1;
	Box.Color = Box.BorderColor = GetBoxColor();
	Box.Type = 1;
	Box.IsShadow = 0;

	Cursor = SaveCursor();
	VideoCursOff();

	iHelpFlags = GetHelpFlags();				/* Save current help line */
	HelpLine( (unsigned)iHelpFlags | CONT_HLP | ARROW_HLP );

   WindowSave( &Window, OldScrn );
	PromptWindow( szText, NULL, GetPromptColor(), OldScrn );

	PutWindow( &Box );
	iSelection = GetSelection( szOpts, &Def );

   WindowRestore( &Window, OldScrn );

	HelpLine( (unsigned)iHelpFlags );		/* Restore original help line	*/

	RestoreCursor( Cursor );
	FreeMemory( OldScrn );

	return( iSelection );
}





