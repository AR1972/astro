/***************************************************************************/
/*																									*/
/*	HEADER.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Displays the header and title at the top of the screen in the proper		*/
/* color as defined by the macro GetHeaderColor().									*/
/* 																								*/
/* void DisplayScreenHeader( void )														*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS:		void 																			*/
/*																									*/
/*	EXTERNS:		HelpHeaderText - Both declared in EXTERN.C						*/
/*					HeaderText;																	*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include 	<string.h>

#include 	<alias.h>
#include		<window.h>
#include		<bios_io.h>

#define		HEADER_LINES		5

void DisplayScreenHeader( int IsHelp )
{
	char				*apszTitle[ HEADER_LINES ];
	extern UCHAR	ScreenWidth;

	extern UINT		HelpHeaderText;
	extern UINT 	HeaderText;


	GetMessage( apszTitle, IsHelp ? HelpHeaderText : HeaderText );

	/* m200: First clear any previous header */
	VideoDupCharAttr( 1, 1,	' ', GetHeaderColor(), LAST_VIDEO_COL );
	VideoDupCharAttr( 2, 1,	' ', GetHeaderColor(), LAST_VIDEO_COL );

	VideoPutsAttrRowCol( 1, 1,	apszTitle[0], GetHeaderColor() );
#ifdef JAPAN	/* if KEISEN */
	VideoDupDBCharAttr( 2, 1, K_THICK_HORIZ_LINE, GetHeaderColor(),
							((int)strlen(apszTitle[0]) + 1) / 2);
#else
	VideoDupCharAttr( 2, 1, 'Í', GetHeaderColor(),
							(int)strlen( apszTitle[0] ) );
#endif

}	

