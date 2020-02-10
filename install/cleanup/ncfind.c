/***************************************************************************/
/* 																								*/
/*	NCFIND.C																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/***************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/************************************************************************/

#include <alias.h>
#include <bios_io.h>
#include <strlib.h>
#include <dos.h>

#include "global.h"
#include "message.h"


/************************************************************************/

int  main( int argc, char *argv[] );
int  ParseSwitches( int argc, char *argv[] );
void Initialize( void );
static void CommandHelp( void );
static void PrintLine(int handle, char *string, int len);

static char newline[2] = {0xd,0xa};
#define NEWLINE_LEN   2
#define STDOUT        1


/************************************************************************/

int main (int argc, char *argv[]) 
{
	int	aswitch;

 	aswitch = ParseSwitches ( argc, argv );	

	Initialize ( );

	switch ( aswitch )
	{
		case CLEANUP:
			if ( DoCleanUp() )
			{
				remove ( argv[0] );
				strcpy ( FileExtension (argv[0]), "EXE" );
				remove ( argv[0] );
			}
			ProgramCleanUp ( TRUE );
			break;

#ifdef POSTPONE_501
		case UPGRADE:
			IsRestore = FALSE;
			FixAutoExec ( *ComPath );
			FilesPath = strupr (argv[1]);
			DoIntro ();
			if ( DoDiskSearch ( DoComNetPrompt() ) )
				UpdateStack();
#if 0			
			if ( NetworkUpgrade () )
				RemoveDir ( FilesPath );
			/*  DoNonUpgradable();  For now don't do */
#endif			
			DoMessages();
			DoDone();
			ProgramCleanUp ( TRUE );
			break;
		
		case RESTORE:
			IsRestore = TRUE;
			FilesPath = NULL;
			DoSearchWindow ();
			DoDriveSearch (RESTORE, NET_MASK | COM_MASK );
			ProgramCleanUp ( FALSE );
			break;
#endif
	}
   return( 0 );
}

/*****************************************************************************
 *	void Initialize ( void )
 *
 *	Initializes the video display, allocates memory and read in the INF data
 *	file.
 *
 *	ENTRY  void
 *
 ****************************************************************************/

void Initialize ( void )
{

	ProgramInitialize ();


#ifdef POSTPONE_501
	GlobalBufSize = 0xF000;
	GlobalBuf = GetMemory ( GlobalBufSize );
	ErrorBuffer = GetMemory ( (size_t) 2000);
	lByteCount = 0l;
	lTotalBytes = 0l;
	
	strcpy ( FileExtension (infpath), "INF" );
	ReadINF ( infpath );
	ComPath = NULL;

	if ( argc != 1 )
	{
		RemovePath ( infpath );
		ComPath = strupr ( infpath );
	}					

	ReadCommandCom ( ComPath );
#endif
}

/*****************************************************************************
 *	int ParseSwitches ( int argc, char *argv )
 *
 *	Parse the command line switches and returns the appropriate action
 *	to take.  If the "/?" is specified it display help and exits.
 *	
 *	ENTRY
 *		argc -- command line argument count
 *		argv -- command line argument strings
 *	EXIT
 *		returns either UPGRADE, CLEANUP, or RESTORE
 *
 ****************************************************************************/

int ParseSwitches ( int argc, char *argv[] )
{
	register		i;
	int			iStatus;

	iStatus = CLEANUP;

	for ( i = 1; i < argc; i++ )
	{
		if ( !stricmp( argv[i], MONO_ARG ) )
			vInfo.Args.fIsMono = TRUE;

		else if ( !stricmp (argv[1], HELP_ARG ) )
			CommandHelp();

#ifdef POSTPONE_501
		else	if ( !stricmp( argv[i], RESTORE_ARG ) )
			iStatus = RESTORE;
		
		else if ( !stricmp (argv[i], UPGRADE_ARG ) )
			iStatus = UPGRADE;
#endif

	}
	return( iStatus );
}


/************************************************************************/
/* Displays the command line help and then exits to DOS.						*/
/*																								*/
/*	void CommandHelp( void )															*/
/*																								*/
/*	ARGUMENTS:	NONE																		*/
/*	RETURNS:		void																		*/
/*																								*/
/************************************************************************/

void CommandHelp( void )
{
    register i;
    char *apszText[ HELP_SWITCH_LINES ];

    GetMessage( apszText, HELP_SWITCH_TEXT );

    for ( i = 0; apszText[ i ] != NULL; i++ )
    {
      /* print help message to STDOUT */
      PrintLine( STDOUT, apszText[i], strlen(apszText[i]) );
      PrintLine( STDOUT, newline, NEWLINE_LEN);
    }
    exit( 0 );
}


/************************************************************************/
/* Prints one line to the handle given (stdout, stderr).                */
/*									*/
/*	void PrintLine( int handle, char string[], int len )            */
/*									*/
/*	ARGUMENTS:	handle - where to print (stdout, stderr)        */
/*                      string - string to print                        */
/*                      len    - length of string to print              */
/*	RETURNS:	void					        */
/*									*/
/************************************************************************/

void PrintLine(int handle, char string[], int len)
{
    union REGS reg;

    reg.h.ah = 0x40;
    reg.x.bx = handle;
    reg.x.cx = len;
    reg.x.dx = (unsigned)string;
    intdos(&reg, &reg);
}

