/********************************* WINDOW.C ********************************/
/*	WINDOW.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* General windowing functions for the DOS 4.x retail upgrade install 		*/
/* program.																						*/
/*                                                                         */
/* Created 890306 - johnhe																	*/
/***************************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>

#include    <alias.h>
#include    <bios_io.h>
#include    <strlib.h>
#include    <window.h>
#include    <disk_io.h>

#include    "global.h"
#include    "message.h"		/* Must be in current directory */
#include    "intrface.h"


/***************************************************************************/

// BEGIN IPG - Change ?CASE_Y to ?CASE_YES
int		YesNoResponse[] = { UCASE_YES, LCASE_YES, CR, 0 };
// END IPG   - Change ?CASE_Y to ?CASE_YES

/***************************************************************************/
/* Fatal error function will display an error message detailing the error  */
/* and warning that program is being aborted and then wait for any key to  */
/* be pressed. After a key a general cleanup is done by closing open       */
/* file, restoring all of the original interrupt handlers, cleaning up the */
/* video and then return to DOS with the error number set                  */
/*                                                                         */
/* ErrorNumber - An error number corresponding to the fatal error type     */
/* RETURNS: Does not return to caller, exits to DOS                        */
/***************************************************************************/

void FatalError( int ErrorNumber)
{
	char		*apszMessage[ FATAL_MESSAGE_LINES ];
	char		*apszExit[ ERROR_EXIT_LINES ];
	char		*apszText[ FATAL_LINES ];
	static int	Response[] = {CR, 0};

	GetMessage( apszMessage, FATAL_MESSAGE_TEXT );
	GetMessage( apszExit, ERROR_EXIT_TEXT );
	GetMessage( apszText, FATAL_TEXT );


	apszMessage[2] = apszText[ErrorNumber];
	apszMessage[3] = apszExit[vCurrentFile.chValidErrors];

	if ( vCurrentFile.chValidErrors != REBOOT )
		HelpLine ( EXIT_HLP | CONT_HLP );
	
	PromptWindow( apszMessage, Response, GetErrorColor(), GetErrorBuffer() );
	ProgramCleanUp( RESTORE_SCREEN );	/* Restore video & interrupts */

	if (!IsRestore)
   {
		VideoCleanup();
		VideoRestoreMode();
   }
	exit( ErrorNumber );       /* Exit to DOS with error code set */
}      

/***************************************************************************/
/* Displays a window with the status of files being copied. It uses the	   */
/* global structure "vinfo" to get the name of the file and whether it is  */
/* is being read or written.						   */
/* 									   */
/* void FileStatusUpdate( void ) 					   */
/*                                                                         */
/* ARGUMENTS:	NONE                                                       */
/* RETURNS: 	void                                                       */
/*                                                                         */
/***************************************************************************/

void FileStatusUpdate( void )
{
	char		*apszStatus[ STATUS_LINES ];
	char		szStr[25];


	GetMessage( apszStatus, STATUS_TEXT );

	strcpy( szStr, apszStatus[vCurrentFile.chReadWrite] );
	strcpy( szStr + 9, vCurrentFile.szFileName );
	PadStr( szStr, SPC, 21 );

	VideoPutsRowCol( 24, 58, szStr );
}

/***************************************************************************/
/* Sets up the current file structure and then calls FileStatusUpdate to   */
/* display the read-write message and file name on the screen's current    */
/* status line.                                                            */
/*                                                                         */
/* void DisplayFileStatus( char *szFileName, int Type )                    */
/*                                                                         */
/* ARGUMENTS:	szFileName - Name of current file in the form "NAME.EXT"   */
/*		Type       - Flag access type of READ or WRITE             */
/* RETURNS: 	void                                                       */
/***************************************************************************/

void DisplayFileStatus( char *szFileName, int Type )
{
	vCurrentFile.chReadWrite = (unsigned char)Type;
	strcpy( vCurrentFile.szFileName, szFileName );
	FileStatusUpdate();
}

/***************************************************************************/
/* Displays a window with the status of directories being copied. It uses  */
/* the global structure "vinfo" to get the name of the directory and       */
/* whether it is being read or written.                                    */
/* 									   */
/* void DirStatusUpdate( void )                                            */
/*                                                                         */
/* ARGUMENTS:	NONE                                                       */
/* RETURNS: 	void                                                       */
/*                                                                         */
/***************************************************************************/

void DirStatusUpdate( void )
{
	char		*apszStatus[ STATUS_LINES ];
	char		szStr[25];


	GetMessage( apszStatus, STATUS_TEXT );

	strcpy( szStr, apszStatus[vCurrentFile.chReadWrite] );
	strcpy( szStr + 9, vCurrentFile.szFileName );
	PadStr( szStr, SPC, 21 );

	VideoPutsRowCol( DIR_STAT_ROW, DIR_STAT_COL + DIR_STAT_INDENT, szStr );
}

/***************************************************************************/
/* Sets up the current file structure and then calls DirStatusUpdate to	   */
/* display the	read-write message and file name on the screen's current   */
/* status line.								   */
/*                                                                         */
/* void DisplayDirStatus( char *szDirName, int Type )                      */
/*                                                                         */
/* ARGUMENTS:	szDirName - Name of current directory in form "NAME.EXT"   */
/*		Type       - Flag access type of READ or WRITE             */
/* RETURNS: 	void                                                       */
/***************************************************************************/

void DisplayDirStatus( char *szDirName, int Type )
{
	vCurrentFile.chReadWrite = (unsigned char)Type;
	strcpy( vCurrentFile.szFileName, szDirName );
	DirStatusUpdate();
}

/************************************************************************/
/* This function should be called if the user presses a key which 		*/
/* causes the program to end. It will prompt the user to confirm that	*/
/* they wish to end the program and if the user confirms it will call	*/
/* ProgramAbort().																		*/
/*																								*/
/* void AllowAbort( void )																*/
/*																								*/
/* ARGUMENTS:	NONE																		*/
/*	RETURNS:		void																		*/
/*																								*/
/************************************************************************/
void AllowAbort( void )
{
/*
	For cleanup program we don't need to support the F3 key so this
   function just needs to return.

	if ( AbortPrompt() == UCASE_Y )
		ProgramAbort();
*/
}

/************************************************************************/
/* Allows the program to abort gracefully by checking to see if a			*/
/* return to DOS can be done or if a re-boot is necessary. If a there	*/
/* has to be a re-boot it can be done without any cleanup but if there	*/
/* there is going to be a return to DOS all interrupt handlers must be	*/
/* removed and if debugging all allocated memory must be freed.			*/
/*																								*/
/* void ProgramAbort( void )															*/
/*																								*/
/* ARGUMENTS:	NONE																		*/
/* RETURNS:		void																		*/
/*																								*/
/************************************************************************/

void ProgramAbort( void )
{
		/*Only restore screen if we're not restroing old files.*/
	ProgramCleanUp( !IsRestore );
	exit( 0 );
}	

/************************************************************************/

void FatalDiskError( char Disk )
{
	int			AnyChar = { CR, 0 };
	char			*apszText[ FATAL_DISK_LINES ];
	char			*apszMessage[ MAX_STRINGS ];
	extern char *apszReadWriteError[];

	GetMessage( apszText, FATAL_DISK_TEXT );

	GetMessage( apszMessage, RESTART_TEXT );

	apszText[4] = apszMessage[0];
	apszText[5] = apszMessage[1];

	apszText[0] = apszReadWriteError[ 2 ];
	apszText[2][6] = Disk;
	PromptWindow( apszText, &AnyChar, GetErrorColor(), GetErrorBuffer() );

	ProgramAbort();
}

/***************************************************************************/
/* Removes the color the current screen display.									*/
/*																									*/
/*	void StripScreenColor( void )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void StripScreenColor( void )
{
	register			i;
	char				*Buf;
	char				*Ptr;
	static WINDOW	Win = { 0, 24, 0, 79, 0, 0, 0, 0 };

	Ptr = Buf = GetMemory( 25 * 80 * 2 );
	WindowMove( &Win, Buf, SAVE );
	
	for ( i = 0, Ptr++ ; i < (25 * 80 * 2); i += 2, Ptr += 2 )
		*Ptr = *Ptr >= (char)0x30 ? (char)0x70 : (char)0x07;						

 	WindowMove( &Win, Buf, RESTORE );
	FreeMemory( Buf );
	vInfo.Args.fIsMono = TRUE;
}


int Int24DiskError( char Disk )
{
	char			*apszText[ INT_24_LINES ];
	char			*apszOptions[ INT_24_EXIT_LINES ];

	GetMessage( apszText, INT_24_TEXT );
	GetMessage( apszOptions, INT_24_EXIT_TEXT );
 	*(apszText[4]) = Disk;

	return( MyPromptSelect( apszText, apszOptions, 0 ) );
}

/*****************************************************************************
 *	void WriteProtectPrompt ( char Disk )
 *
 *	Does nothing but satisfy the linker.  Since a library routine references
 *	it but doesn't use it ( we're only writing to a harddisk in NCFIND).
 *	The "Disk = Disk" is there to shut up the compiler which would otherwise
 *	warn "Unreferenced formal paramter line XXX..."
 *
 ****************************************************************************/


void WriteProtectPrompt( char Disk )
{
		/*Do nothing.*/
	Disk = Disk;

}


/***************************************************************************/
/*                                                                         */
/*	void Help( void )																			*/
/*                                                                         */
/* ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*                                                                         */
/***************************************************************************/

void Help( void )
{
	;	/* Do nothing: help disabled */
}


