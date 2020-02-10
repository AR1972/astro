/***************************************************************************/
/*                                                                         */
/*	EXTERN.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/*                                                                         */
/* johnhe - 12/29/89																			*/
/***************************************************************************/


/* 
unsigned	ErrorMessageText	= ERROR_MESSAGE_TEXT;

unsigned	AcceptText			= ACCEPT_TEXT;
unsigned SetDefaultText 	= SET_DEFAULT_TEXT;

unsigned DiskErrorText		= DISK_ERROR_TEXT;

unsigned Type525Text 		= TYPE_525_TEXT;
unsigned Type35Text			= TYPE_35_TEXT;
unsigned DskFmtText			= DSK_FMT_TEXT;

unsigned FmtStatText 		= FMT_STAT_TEXT;

*/


#include		<stdio.h>
#include		<stdlib.h>
#include		<malloc.h>
#include		<dos.h>
#include		<direct.h>
#include    <io.h>
#include    <sys\types.h>
#include    <sys\stat.h>
#include    <fcntl.h>


#include		<alias.h>
#include		<global.h>
#include		<upgrade.h>


void FatalError( int );

/* Globals to resolve */

char	*szRootPath = "\\";

int	CreatingRecovery;

/***************************************************************************/

fmemcmp( char far *Str1, char far *Str2, UINT Len )
{
	while( Len && *Str1 == *Str2 )
		Str1++, Str2++, Len--;


	return( Len );
}

/***************************************************************************/
/*																									*/
/* Functions replaced with windows versions											*/
/*																									*/
/***************************************************************************/

#if 0
void Error( int x )
{;}

void PromptForDisk( int x )
{;}

int PromptForDiskFmt( int x )
{;}

int PromptForDefault( void )
{;}

void ProcessCopyError( char *szFile, int ErrorType )
{;}

void WriteProtectPrompt( void )
{;}

void NotReadyPrompt( void )
{;}

#endif

/***************************************************************************/

