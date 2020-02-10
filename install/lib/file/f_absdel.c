/***************************************************************************/
/*																									*/
/*	F_ABSDEL.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Does an absolute delete of a file reguardless of the files attributes.	*/
/* 																								*/
/* int AbsUnlink( char *szFile )															*/
/* 																								*/
/* ARGUMENTS:	szFile	- Full path name of file to be deleted					*/
/* RETURNS: 	int		- OK if successful else ERROR 							*/
/*																									*/
/* Created 12-10-89 johnhe																	*/
/***************************************************************************/

#include 	<stdio.h>
#include		<io.h>
#include		<dos.h>
#include		<alias.h>

int AbsUnlink( char *szFile )
{
	if ( _dos_setfileattr( szFile, _A_NORMAL ) == OK )
		return( unlink( szFile ) );
	return( -1 );
}
