/***************************************************************************/
/*																									*/
/* DFLTPRMT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Displays a prompt asking the user if they want to use the disk format	*/
/* they have previously selected as the default for the rest of the			*/
/* program. 																					*/
/* 																								*/
/* 																								*/
/* int PromptForDefault( void )															*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	int		TRUE if user selects YES else FALSE 					*/
/* 																								*/
/*	EXTERNS:		AcceptText 		- Defined in extern.c								*/
/*					SetDefaultText	- Defined in extern.c								*/ 
/*																									*/
/* johnhe - 12/29/89																			*/
/***************************************************************************/

#include		<alias.h>
#include		<window.h>

#define		YES_NO_LINES	4

int PromptForDefault( void )
{
	char				*apszYesNo[ YES_NO_LINES ];
	char				*apszText[ MAX_STRINGS	];
	int				iSelection;
	int				iHelpFlags;

	extern UINT 	AcceptText;
	extern UINT 	SetDefaultText;

	GetMessage( apszYesNo, AcceptText );
	GetMessage( apszText, SetDefaultText );

	PushHelp( 0xffff );							/* No help is set yet				*/
	iHelpFlags = GetHelpFlags();				/* Save current help line			*/
	HelpLine( EXIT_HLP );						/* Set new help line					*/

	while ( (iSelection = PromptSelect( apszText, apszYesNo )) == PREVIOUS )
		;

	HelpLine( iHelpFlags );						/* Restore original help line		*/
	PopHelp();

	return( !iSelection );
}
