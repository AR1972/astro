/***************************************************************************/
/*																									*/
/*	W_SIZE.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Initializes a specified window structure based on the number and			*/
/* length of an array of strings.														*/
/*																									*/
/*	int GetWinInfo( char **String, WINDOW *Window ) 								*/
/*																									*/
/*	ARGUMENTS:	Strings	-	Array of pointers to strings (NULL terminated)	*/
/*					Window	- Pointer to a WINDOW structure to be initialized	*/
/* RETURNS:		int		- Number of strings which will fit in the window	*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include 	<alias.h>
#include		<window.h>
#include		<bios_io.h>
#include		<strlib.h>


int GetWindowInfo( char **String, WINDOW *Window )
{
	UCHAR		NumStrs;
	UCHAR 	WindowWidth;
	UCHAR 	MaxLength;

	NumStrs = (UCHAR)GetNumberStrings( String );
	MaxLength = (UCHAR)MaxStrLen( String );
	WindowWidth = MaxLength + (UCHAR)(3);
																				/*lint -e734	*/
#ifdef JAPAN	/* if KEISEN */
	WindowWidth += (WindowWidth & (UCHAR)1) ? (UCHAR)2 : (UCHAR)3;
#endif

	Window->Top = ((UCHAR)25 - NumStrs) / (UCHAR)2;
	Window->Bottom = Window->Top + NumStrs;
	Window->Left =  (UCHAR)( (VideoGetWidth() - (int)WindowWidth) / 2);
	Window->Right = Window->Left + WindowWidth;					/*lint +e734	*/

   Window->Type = WINDOW_TYPE;

	/* Disable shadowing in Janus */
#if 0
	Window->IsShadow = 1;
#else
	Window->IsShadow = 0;
#endif

	return( (int)NumStrs );
}

