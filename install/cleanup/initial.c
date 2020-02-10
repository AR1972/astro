/************************************************************************/
/*	INITIAL.C																				*/
/*																								*/
/*		Copyright (c) 1991 - Microsoft Corp.										*/
/*		All rights reserved.																*/
/*		Microsoft Confidential															*/
/*                                                                      */
/* DOS %.0 network upgrade. Functions for initializing the program      */
/* video interface and misc. interrupt handlers.                        */
/*                                                                      */
/* Created 890305 - johnhe    														*/
/* Modified for use with DOS 5.0 cleanup progra  08/06/90 t-gerrit		*/
/************************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <bios.h>
#include    <dos.h>
#include    <io.h>
#include    <sys\types.h>
#include    <sys\stat.h>
#include    <fcntl.h>

#include    <alias.h>
#include    <disk_io.h>
#include    <bios_io.h>
#include    <data.h>
#include    <global.h>
#include    <hdisk.h>
#include    <message.h>
#include    <strlib.h>
#include    <window.h>

/************************************************************************/
/* Function prototypes                                                  */
/************************************************************************/

void interrupt cdecl far NewInt24 (unsigned es, unsigned ds,
			unsigned di, unsigned si, unsigned bp, unsigned sp,
			unsigned bx, unsigned dx, unsigned cx, unsigned ax );


/************************************************************************/
/* Global variables for this module					*/
/************************************************************************/

static void 	(interrupt far *OldInt1e)();	/* Original int 1eh addr. */
static void 	(interrupt far *OldInt1b)();	/* Original int 1bh addr. */
static void 	(interrupt far *OldInt23)();	/* Original int 23h addr. */
static void 	(interrupt far *OldInt24)();	/* Original int 23h addr. */

static void	ToggleTsrs ( int State );


static int	DosBreak;	/* Ctrl Break setting at program start */

char		*apszReadWriteError[ READ_WRITE_LINES ];
static char	*szDosDataPath;
char		*apszGlobal[ GLOBAL_LINES ];


/************************************************************************/
/* Initializes the program by disabling the DOS control C function,     */
/* BIOS control break functions and then initialize the video library.	*/
/* Program colors are turned on if a color card is active.              */
/************************************************************************/

void ProgramInitialize( void )
{
	extern unsigned	*HelpStack;
	extern unsigned	*HelpStackPtr;


	ToggleTsrs( OFF );

	/* Get disk error messages */
	GetMessage( apszReadWriteError, READ_WRITE_TEXT );
	GetMessage( apszGlobal, GLOBAL_TEXT );

	DosBreak = GetBreakSetting();		/* Save DOS break setting */
	UpdateBreakSetting( OFF );              /* Turn break check off */
	OldInt1b = _dos_getvect( 0x1b );	/* Save int 1bh vector */
	OldInt23 = _dos_getvect( 0x23 );	/* Save int 23h vector */
	OldInt24 = _dos_getvect( 0x24 );	/* Save int 24h vector */

	_dos_setvect( 0x1b, NewInt1b );	/* Set new int 1bh vector */
	_dos_setvect( 0x23, NewInt23 );	/* Set new int 23h vector */
	_dos_setvect( 0x24, NewInt24 );	/* Set new int 24h vector */

	VideoSaveMode();			/* Save the initial state */
	VideoInitial();				/* Initialize the video library */
	VideoCursOff();
	SetDefaultColors( VideoIsColor() && !vInfo.Args.fIsMono );
	VideoCls( GetBackGroundColor() );
	DisplayScreenHeader( 0 );
	HelpLine( 0 );

	HelpStackPtr = HelpStack = GetMemory( 100 * sizeof( unsigned ) );
	PushHelp( 0xffff );			/* No help is set yet */
}

/************************************************************************/
/* Restores the system the way it was found. Restores all interrupt     */
/* vectors which were replaced. Restores the DOS break check to it's 	*/
/* initial setting, clears the screen and homes the cursor.		*/
/************************************************************************/

void ProgramCleanUp( int RestoreScreen )
{
	if ( RestoreScreen )
        {
		VideoCleanup();
		VideoRestoreMode();
        }

	if ( OldInt1b != NULL )
	{
		_dos_setvect( 0x1b, OldInt1b );	/* Restore original int 1bh vector 	*/
		_dos_setvect( 0x23, OldInt23 );	/* Restore original int 23h vector 	*/
		_dos_setvect( 0x24, OldInt24 );	/* Restore original int 24h vector 	*/
		UpdateBreakSetting( DosBreak );

		OldInt1b = NULL;	/* Mark that memory is released */
	}
	ToggleTsrs( ON );
}


/************************************************************************/
/* Toggles the state of the TSRs.                                       */
/************************************************************************/

void ToggleTsrs( int State )
{
	if ( _osmajor >= 3 )
	{
		if ( State == OFF )
			DisableAppend();
		else
			EnableAppend();

		ToggleAssign();		/* Toggle assign if it's installed */
	}
}

