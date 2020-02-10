/***************************************************************************/
/*                                                                      	*/
/* INITIAL.C																					*/
/*                                                                      	*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                      	*/
/* Functions for initializing the program video interface and misc.			*/
/* interrupt handlers.																		*/
/*                                                                      	*/
/* Created 07-05-89 - johnhe                                            	*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include		<string.h>
#include		<bios.h>
#include		<dos.h>
#include    <io.h>
#include    <sys\types.h>
#include    <sys\stat.h>
#include    <fcntl.h>

#include		<alias.h>
#include		<bios_io.h>
#include		<disk_io.h>
#include		<hdisk.h>
#include		<window.h>
#include		<message.h>
#include		<global.h>
#include		<backup.h>

/************************************************************************/
/* Function prototypes																	*/
/************************************************************************/
void	ProgramInitialize		( void );

void	SystemRestore			( void );
int	DriveReady				( char chDrive );

static void	ToggleTsrs		( int State );
static int 	IsFloppyBoot	( void );

char	FmtDrvLst[ MAX_DOS_DRIVES ];

/************************************************************************/
/* Global variables for this module													*/
/************************************************************************/
static void 	(interrupt far *OldInt1e)();	/* Original int 1eh addr.	*/
static void 	(interrupt far *OldInt1b)();	/* Original int 1bh addr.	*/
static void 	(interrupt far *OldInt23)();	/* Original int 23h addr.	*/
static void 	(interrupt far *OldInt24)();	/* Original int 23h addr.	*/
static int		DosBreak;			/* Ctrl Break setting at program start */

char		*apszReadWriteError[ READ_WRITE_LINES ];

/************************************************************************/
/* Initializes the program by disabling the DOS control C function, 		*/
/* BIOS control break functions and then initialize the video library.	*/
/* Program colors are turned on if a color card is active.					*/
/************************************************************************/
void ProgramInitialize( void )
{
	extern unsigned	*HelpStack;
	extern unsigned	*HelpStackPtr;
	char					*Buf;
	int					Drv;	

	ToggleTsrs( OFF );
												/* Initialize disk error messages */
	GetMessage( apszReadWriteError, READ_WRITE_TEXT );

	vInfo.chNumFloppy = (char)GetNumberOfDrives();
	if ( _osmajor >= 4 && vInfo.chNumFloppy > 2 )
		vInfo.chNumFloppy = 2;

	vInfo.chDestin = 'A';

	DosBreak = GetBreakSetting();		/* Save DOS break setting */
	UpdateBreakSetting( OFF );			/* Turn break check off */

	OldInt1e = _dos_getvect( 0x1e );	/* Save int 1eh vector */
	OldInt1b = _dos_getvect( 0x1b );	/* Save int 1bh vector */
	OldInt23 = _dos_getvect( 0x23 );	/* Save int 23h vector */
	OldInt24 = _dos_getvect( 0x24 );	/* Save int 24h vector */

	_dos_setvect( 0x1b, NewInt1b );	/* Set new int 1bh vector */
	_dos_setvect( 0x23, NewInt23 );	/* Set new int 23h vector */
	_dos_setvect( 0x24, NewInt24 ); 	/* Set new int 24h vector */
	InitNew13( GetErrorBuffer() );	

	if ( vInfo.Args.fNoVideoChk )		/* m100	*/
		MinVideoInitial();				/* m100	*/
	else										/* m100	*/
		VideoInitial();					/* m100 Initialize the video library */

	// m100 VideoInitial();				/* Initialize the video library */

	VideoCursOff();
	SetDefaultColors( (VideoIsColor() && !vInfo.Args.fIsMono) ? TRUE : FALSE );
	VideoCls( GetBackGroundColor() );
	DisplayScreenHeader( 0 );
	HelpLine( 0 );
												/* Initialize the help system */
	HelpStackPtr = HelpStack = GetMemory( 100 * sizeof( unsigned ) );
	PushHelp( 0xffff );						/* No help is set yet */


	Buf = GetMemory( GetMaxSectorSize() );
	if ( (vInfo.chFirstHd = (char)FindFirstHd( Buf )) == (char)(0xff) )
		vInfo.chFirstHd = 'C';

	for ( Drv = (vInfo.chFirstHd - 'A');
		   Drv < MAX_DOS_DRIVES &&
			IsValidDrive( (char)Drv + 'A' ) &&
			IsLocalDrive( (char)Drv + 1 );
			Drv++ )
	{
		FmtDrvLst[ Drv ] = (char)(ReadWriteBoot( Drv, Buf, READ ) == OK &&
   							        IsFmtedBoot( Buf ));
	}

	FreeMemory( Buf );

	/* Set flag which determines Abort Exit Prompt */

	vInfo.Flag.fFloppyBoot = IsFloppyBoot();
}	

/************************************************************************/
/* Restores the system the way it was found. Restores all interrupt 		*/
/* vectors which were replaced. Restores the DOS break check to it's 	*/
/* initial setting, clears the screen and homes the cursor.					*/
/************************************************************************/

void	ProgramCleanUp( int RestoreScreen )
{
	extern unsigned	*HelpStack;


	if (RestoreScreen )
		VideoCleanup();

	if ( OldInt1b != NULL )
	{
		RestoreOld13();
		_dos_setvect( 0x1e, OldInt1e );	/* Restore original int 1eh vector 	*/
		_dos_setvect( 0x1b, OldInt1b );	/* Restore original int 1bh vector 	*/
		_dos_setvect( 0x23, OldInt23 );	/* Restore original int 23h vector 	*/
		_dos_setvect( 0x24, OldInt24 );	/* Restore original int 24h vector 	*/
		UpdateBreakSetting( DosBreak );

		OldInt1b = NULL;						/* Mark that memory is released */
	}

	ToggleTsrs( ON );
}

/************************************************************************/
void ToggleTsrs( int State )
{
	if ( _osmajor >= 3 )
	{
		if ( State == OFF )
			DisableAppend();
		else
			EnableAppend();

		ToggleAssign();					/* Toggle assign if it's installed */
	}
}

/***************************************************************************
 * Returns TRUE if the boot drive is physical floppy drive 0. Else
 * returns FALSE.																				
 *																									
 * int IsFloppyBoot( void )																
 *																									
 * ARGUMENTS:	NONE																			
 * RETURNS:		TRUE if boot drive is drive A: else FALSE
 *
 *	NOTES: If DOS version is < 4.0, returns FALSE. (In fact, we should only
 *			 be booting from Setup floppy (DOS version == 5.0).)
 *																									
 ***************************************************************************/

int IsFloppyBoot( void )
{
	int iRet = FALSE;
	union REGS inregs, outregs;

	if ( _osmajor >= 4 )
	{
		inregs.x.ax = 0x3305;	/* Get boot drive */
		intdos( &inregs, &outregs );

		iRet = (outregs.h.dl == 1 ? TRUE : FALSE);
	}
	
	return( iRet );
}

