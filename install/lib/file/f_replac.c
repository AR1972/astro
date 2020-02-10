/***************************************************************************/
/*																									*/
/*	F_REPLAC.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Replaces the destination file with the source file by first seeing if	*/
/* the source exists and then deleting the destination and renameing the	*/
/* destination and renaming the source to take it's place. The check for   */
/* the destination is done to be sure the replace has not already taken 	*/
/* place.																						*/
/* 																								*/
/* int ReplaceFile( char *szSource, char *szDestin )								*/
/* 																								*/
/* ARGUMENTS:	szSource - Ptr to full path and name for source file			*/
/* 				szDestin - Ptr to full path and name for destination file	*/
/* RETURNS: 	int		- OK if successful else ERROR 							*/
/*																									*/
/* Created 12-10-89 johnhe																	*/
/***************************************************************************/

#include 	<stdio.h>
#include		<io.h>
#include		<dos.h>

#include 	<alias.h>
#include 	<disk_io.h>

int ReplaceFile( char *szSource, char *szDestin )
{

	if ( FileExists( szSource ) )
	{
		_dos_setfileattr( szDestin, _A_NORMAL );
		unlink( szDestin );

	}
	
	if ( rename( szSource, szDestin ) != OK )
		return( ERROR );													/* ERROR EXIT	*/

	return( OK );															/* NORMAL EXIT	*/
}
