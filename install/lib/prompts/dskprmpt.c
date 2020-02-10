/***************************************************************************/
/*																									*/
/*	DSKPRMPT.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/*	Prompts user to select the desired disk format using a scroll bar			*/
/* menu of options. 																			*/
/*																									*/
/*	int PromptForDiskFmt( int iDrvType )												*/
/*																									*/
/*	ARGUMENTS:	iDrvType		-  Type of drive: 0 = 1.2 meg : 1 = 1.44 meg		*/
/*	RETURNS:		int			- User's selection of 0 or 1							*/
/*																									*/
/*	EXTERNS:		Type525Text	- All declared in EXTERN.C								*/
/*					Type35Text																	*/
/*					DskFmtText																	*/
/*																									*/
/* johnhe - 12/29/89																			*/
/***************************************************************************/

#include		<alias.h>
#include		<window.h>

#define		TYPE_LINES	3

int PromptForDiskFmt( int iDrvType )
{
	char				*apszFmt[ TYPE_LINES ];
	char				*apszText[ MAX_STRINGS ];
	int				iSelection;
	int				iHelpFlags;

	extern UINT 	Type525Text;
	extern UINT 	Type35Text;
	extern UINT 	DskFmtText;

	GetMessage( apszFmt, iDrvType == 0 ? Type525Text : Type35Text );
	GetMessage( apszText, DskFmtText );

	PushHelp( 0xffff );							/* No help is set yet				*/
	iHelpFlags = GetHelpFlags();				/* Save current help line			*/
	HelpLine( EXIT_HLP | CONT_HLP );			/* Set new help line					*/

	while ( (iSelection = PromptSelect( apszText, apszFmt )) == PREVIOUS )
		;

	HelpLine( iHelpFlags );						/* Restore original help line		*/
	PopHelp();

	return( iSelection );
}
