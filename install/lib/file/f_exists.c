/***************************************************************************/
/*																									*/
/*	F_EXISTS.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Determines if the specified file exists.											*/
/* 																								*/
/* int FileExists( char *szFile )														*/
/* 																								*/
/* ARGUMENTS:	szFile		- Ptr to full path name for file to be found 	*/
/* RETURNS: 	int			- TRUE if file was found else FALSE 				*/
/*																									*/
/* Created 10-28-89 johnhe																	*/
/***************************************************************************/

#include 	<dos.h>

#include		<alias.h>

int FileExists( char *szFile )
{
	struct find_t		FileInfo;

	return( _dos_findfirst( szFile, _A_NORMAL | _A_RDONLY | _A_ARCH,
									&FileInfo ) == OK );
}
