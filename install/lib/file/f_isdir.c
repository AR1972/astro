/***************************************************************************/
/*																									*/
/*	F_ISDIR.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Determines if any files exist in the specified directory path. 			*/
/* 																								*/
/* int IsDirEmpty( char *szPath )														*/
/* 																								*/
/* ARGUMENTS:	szPath	- Ptr to full path name for directory to search 	*/
/* RETURNS: 	int		- FALSE if file is found else TRUE						*/
/*																									*/
/* Created 10-28-89 johnhe																	*/
/***************************************************************************/

#include 	<stdio.h>

#include 	<alias.h>
#include 	<strlib.h>
#include 	<disk_io.h>

int IsDirEmpty( char *szPath )
{
	char				*szAnyFile = "????????.???";
	char				szFullPath[ MAX_PATH_LEN ];

	BuildPath( szFullPath, szPath[0], szPath + 2, szAnyFile );
	return( !FileExists( szFullPath ) );
}
