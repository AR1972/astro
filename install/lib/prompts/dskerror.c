/***************************************************************************/
/*																									*/
/*	DSKERROR.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Displays an error message based on a disk error passed as an argument.	*/
/* 																								*/
/* void ProcessDiskError( int ErrorType ) 											*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURN:		void																			*/
/* 																								*/
/*	EXTERNS:		DiskErrorText	- Declared in EXTERN.C								*/
/* 																								*/
/* johnhe - 12/29/89																			*/
/***************************************************************************/

#include 	<stdio.h>

#include		<alias.h>
#include		<window.h>

void ProcessDiskError( int ErrorType )
{
	static char		*szText[] = { "", "", NULL };
	char				*szError[ MAX_STRINGS ];
	int				iHelpFlags;

	extern UINT		DiskErrorText;

	GetMessage( szError, DiskErrorText );

	szText[0] = szError[ ErrorType ];
	szText[1] = szError[ 0 ];

	PushHelp( 0xffff );							/* No help is set yet				*/
	iHelpFlags = GetHelpFlags();				/* Save current help line			*/
	HelpLine( EXIT_HLP | CONT_HLP );			/* Set new help line					*/

	Error( szText );

	HelpLine( iHelpFlags );						/* Restore original help line		*/
	PopHelp();
}
