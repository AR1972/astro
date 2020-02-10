/***************************************************************************/
/*																									*/
/*	W_COLOR.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Sets the default colors based on whether IsColor is TRUE or FALSE.		*/
/*																									*/
/* void SetDefaultColors( int IsColor )												*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include 	<alias.h>
#include		<bios_io.h>


extern UCHAR	*uchAreaColors;


void SetDefaultColors( int IsColor )
{
	static UCHAR MonoDefault[ ] =
#ifdef JAPAN
		{ 0x07, 0x07, 0x07, 0x70, 0x70, 0x07, 0x70, 0x07, 0x70, 0x70 };
#else
		{ 0x07, 0x07, 0x07, 0x70, 0x70, 0x07, 0x70, 0x07, 0x70, 0x07 };
#endif
									   
	static UCHAR ColorDefault[ ] =
#ifdef JAPAN
		{ 0x17, 0x17, 0x17, 0x70, 0x30, 0x30, 0x70, 0x17, 0x70, 0x60 };
#else
		{ 0x17, 0x17, 0x17, 0x70, 0x30, 0x30, 0x70, 0x17, 0x70, 0x0e };
#endif

	uchAreaColors = IsColor ? ColorDefault : MonoDefault;
}

#if 0
	if ( IsColor != FALSE )
		IsColor = 1;
	SetHeaderColor( auchDefault[IsColor][0] );
	SetBackGroundColor( auchDefault[IsColor][1] );
	SetTitleColor( auchDefault[IsColor][2] );
	SetStatusColor( auchDefault[IsColor][3] );
	SetRevBackColor( auchDefault[IsColor][4] );
	SetPromptColor( auchDefault[IsColor][5] );
	SetErrorColor( auchDefault[IsColor][6] );
	SetBoxColor( auchDefault[IsColor][7] );
	SetBarColor( auchDefault[IsColor][8] );
	SetGageColor( auchDefault[IsColor][9] );

#endif


void SetHelpColors( int IsColor )
{

	static UCHAR MonoHelp[ ] =
#ifdef JAPAN
		{ 0x70, 0x70, 0x70, 0x07, 0x07, 0x70, 0x07, 0x70, 0x07, 0x70 };
#else
		{ 0x70, 0x70, 0x70, 0x07, 0x07, 0x70, 0x07, 0x70, 0x07, 0x70 };
#endif
									   
	static UCHAR ColorHelp[ ] =
#ifdef JAPAN
		{ 0x71, 0x70, 0x71, 0x17, 0x30, 0x30, 0x07, 0x70, 0x07, 0x60 };
#else
		{ 0x71, 0x70, 0x71, 0x17, 0x30, 0x30, 0x07, 0x70, 0x07, 0x0e };
#endif

	uchAreaColors = IsColor ? ColorHelp : MonoHelp;
}
