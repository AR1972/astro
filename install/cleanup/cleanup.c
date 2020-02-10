/*****************************************************************************
 *	CLEANUP.C
 *
 *		Copyright (c) 1991 - Microsoft Corp.
 *		All rights reserved.
 *		Microsoft Confidential
 *
 *	Provides the functions for DELOLDOS running in cleanup mode.
 *
 *	t-gerrit	08/23/90	
 *
 ****************************************************************************/

#include <alias.h>
#include <bios_io.h>
#include <ctype.h>
#include <direct.h>
#include <dos.h>
#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <string.h>
#include <strlib.h>
#include <window.h>


#include "global.h"
#include "intrface.h"
#include "message.h"

/*****************************************************************************
 *	int DoCleanUp ( void )
 *
 *	If DELOLDOS is run with the cleanup option then DoCleanUp gets called.  
 *	First	it prompts the user to make sure he/she wants to do the cleanup.  
 *	Then it performs the cleanup.
 *
 *	EXIT
 *		returns TRUE if user chose to cleanup, FALSE otherwise
 *
 ****************************************************************************/	

int DoCleanUp ( void )
{
	char *apszText[ MAX (CLEANUP_PROMPT_LINES, MAX (CLEANUP1_LINES, CLEANUP2_LINES))];
	int response = 0;
#ifdef POSTPONE_501
	int count;
#endif

	vCurrentFile.chValidErrors = REBOOT;

	HelpLine( KILL_COLOR );
	PushHelp (0xffff);
	GetMessage (apszText, CLEANUP_PROMPT_TEXT );
	response = PromptWindow ( apszText, &response, GetPromptColor(), GlobalBuf);
	HelpLine( 0 );

// BEGIN IPG - Change UCASE_Y to UCASE_YES
	if ( toupper ( response ) != UCASE_YES )
// END IPG   - Change UCASE_Y to UCASE_YES
          	return ( FALSE );

	/* Prepare for the case where target files different names than */
	/* source file.*/

#ifdef POSTPONE_501
	for ( count = 0; INFData[count].NetString != EOL; 
		  	MakeSourceEntry (INFData[count++].Target));

	DoSearchWindow ();
#endif

	GetMessage ( apszText, CLEANUP1_TEXT );

	DisplayText ( apszText, TEXT_TOP );

	DoDriveSearch ( CLEANUP, COM_MASK | NET_MASK );

	WorkAreaCls();

	GetMessage ( apszText, CLEANUP2_TEXT );

	DisplayText ( apszText, TEXT_TOP );

	while ( GetChar() != CR );

	return ( TRUE );
}


#if 0

/*****************************************************************************
 *	void CleanUpFile ( char *path )
 *
 *	Takes a path that specifies a file (w/o an exetension), searches for
 *	all files that have packets and match (ie the file with 0-999 as an 
 *	extension and erases them.
 *
 *	ENTRY
 *	  path -- file to search for
 *	EXIT
 *	  returns nothing
 *
 ****************************************************************************/

void CleanUpFile ( char *path )
{

	struct find_t fileinfo;
	FILE_PACKET apacket;


	AddExtension ( path, "*" );

	if ( !_dos_findfirst ( path, _A_NORMAL | _A_SYSTEM | _A_HIDDEN, &fileinfo))
	{
		do 
		{

			RemovePath ( path );
			AddPath ( path, fileinfo.name );

			if ( GetPacket ( &apacket, path ))
			{
				DisplayFileStatus ( ParseFileName ( path ), DELETE );
				_dos_setfileattr ( path, 0 );
				remove ( path );
			}

		} while ( !_dos_findnext ( &fileinfo ) );
	}

}
#endif

/*****************************************************************************
 *	void RemOldDos ( char *path )
 *
 *	Given a path, it searches for the presence of all "OLD_DOS.*" directories 
 *	that contain a "DISk*.NUL" file and, deletes all such directories and
 *	their contents.
 *
 *	ENTRY
 *	  path -- path in which to search for directory
 *
 ****************************************************************************/

void RemOldDos ( char *path )
{
	struct find_t fileinfo, fileinfo2;
#ifdef POSTPONE_501
	int	result;
#endif

	AddPath ( path, "OLD_DOS.*" );

	if ( !_dos_findfirst  ( path, _A_SUBDIR, &fileinfo ) )
 	{
     	DoDirectoryWindow( TRUE );        /* show directory window */
		do 
		{
			RemovePath ( path );
			AddPath ( path, fileinfo.name );
			AddPath ( path, "README.NOW" );
			if (!_dos_findfirst ( path, _A_NORMAL, &fileinfo2 ))
			{
				RemovePath ( path );
				DisplayDirStatus ( ParseFileName ( path ), DELETE );
				RemoveDir ( path );
                                DisplayDirStatus ( "", CLEAR );
			}
			else
				RemovePath ( path );
		}
		while ( !(_dos_findnext ( &fileinfo )) );
     	DoDirectoryWindow( FALSE );        /* erase directory window */
	 }
	RemovePath ( path );
}

/*****************************************************************************
 *	void DoDriveSearch ( int restore, char searchflag );
 *
 *	Searches all the hard disk for upgradable or restoreable files and makes
 *	TargetStack entries for the upgradable files if restore is false, or 
 *	restores the restoreable files if restore is TRUE.
 *
 *	ENTRY
 *		restore -- type of operation to perform: CLEANUP, RESTORE, UPGRADE
 *
 ****************************************************************************/

void DoDriveSearch ( int restore, char searchflag )
{
	char path[MAX_PATH_LEN + FILE_SIZE + 4];

	restore = searchflag;

	strcpy (path, "C:\\");
	RemOldDos ( path );
}

/*****************************************************************************
 *	void DoDirectoryWindow ( void )
 *
 *	Displays the directory search window.
 *	If IfShow is TRUE, then it displays the window in the "error" color.
 *      If IfShow is FALSE, then it displays the window in the background
 *      color (ie. erases the window).
 *
 ****************************************************************************/

void DoDirectoryWindow ( int IfShow )
{
	WINDOW box;
	
	box.Top = DIR_STAT_ROW - 1;
	box.Bottom = DIR_STAT_ROW + 1;
#ifdef JAPAN
	box.Left =  DIR_STAT_COL - 1 - 1;
	box.Right = DIR_STAT_COL + DIR_STAT_LEN + 1;
#else
	box.Left =  DIR_STAT_COL - 1;
	box.Right = DIR_STAT_COL + DIR_STAT_LEN;
#endif
        if (IfShow)               /* show the window */
        {
		box.Type = 1;
		box.Color = GetErrorColor();
		box.BorderColor = GetErrorColor();
		box.IsShadow = 0;
	
		PutWindow ( &box );
        }
        else                      /* erase the window */
        {
            	VideoScrollDn( box.Top, box.Left, box.Bottom, box.Right, 0,
                               GetBackGroundColor() );
        }
}

