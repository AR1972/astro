/***************************************************************************/
/*                                                                         */
/*	WINDOW.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* General windowing functions for the DOS  install                        */
/* program.																						*/
/*                                                                         */
/* Created 03-06-89 - johnhe																*/
/***************************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <dos.h>

#include		<alias.h>
#include 	<bios_io.h>
#include 	<strlib.h>
#include		<window.h>
#include		<disk_io.h>
#include    <data.h>

#include		"message.h" 						/* Must be in current directory */


#ifdef	OEM_PROGRAM
	#include <oem.h>
#else
	#include <global.h>
#endif

#if (UPGRADE_PROGRAM != 0) || (OEM_PROGRAM != 0)
	#include <install.h>
#endif

#ifdef DEBUG	/* ErrorEndPrompt() */
char *DebugContinueMessage="<NORMAL EXIT> DEBUG may continue with 'ENTER'";
#endif

extern unsigned gfCompressedDiskCritSection;

extern int GetChar( void );

#if defined(UPGRADE_PROGRAM) || defined(OEM_PROGRAM)
char * ultoacr (unsigned long, char *, int);
char * ultoac (unsigned long, char *);
#endif

void ReplaceParam (char **apszScreen, char *pszOld, char *pszNew,
                   char *pszBuf, int fKeepLength);

/***************************************************************************/

int	CR_Response[]		= { CR, 0 };		/* M200 */
int	YesNoResponse[]	= { UCASE_YES, LCASE_YES, CR, 0 }; // IPG- ?casey to ?caseyes

static int	IsUpgrade;				/* TRUE for upgrade & FALSE for recovery */

#ifndef	OEM_PROGRAM
	int			CreatingRecovery;			/* Set while creating recovery disk */
#endif

#ifdef DEBUG
char	szDebug[260];	/* Temp. buffer for Debug messages */
#endif

#if (OEM_PROGRAM != 0 || UPGRADE_PROGRAM != 0)
#define EXIT_MSG_TEXT	SETUP_EXIT_TEXT
#define EXIT_MSG_LINES	SETUP_EXIT_LINES
#else
#define EXIT_MSG_TEXT	ERROR_EXIT_TEXT
#define EXIT_MSG_LINES	ERROR_EXIT_LINES
#endif

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
	char			*apszMessage[ FATAL_MESSAGE_LINES ];
	char			*apszExit[ EXIT_MSG_LINES ];
   char			*apszText[ FATAL_LINES ];

	GetMessage( apszMessage, FATAL_MESSAGE_TEXT );
	GetMessage( apszExit, EXIT_MSG_TEXT );
	GetMessage( apszText, FATAL_TEXT );

	if ( ErrorNumber > MAX_FATAL_ERROR || ErrorNumber < 0 )
		ErrorNumber = MAX_FATAL_ERROR;

	apszMessage[2] = apszText[ErrorNumber];
	apszMessage[3] = apszExit[vCurrentFile.chValidErrors];

	HelpLine( CONT_HLP );

   PromptWindow( apszMessage, CR_Response, GetErrorColor(), GetErrorBuffer());
	ProgramCleanUp( RESTORE_SCREEN );			/* Restore video & interrupts */

	if ( vCurrentFile.chValidErrors == REBOOT )
		RebootSystem();
	else
	{
		VideoCleanup();
		ProgramExit( ErrorNumber );       /* Exit to DOS with error code set */
	}
}


/***************************************************************************/
/* Similar to FatalError() above, but lets the caller display a custom     */
/* dialog layout.                                                          */
/*                                                                         */
/* void FatalErrCustom( unsigned msg)                                      */
/*                                                                         */
/* msg - Text group to display                                             */
/* RETURNS: Does not return to caller, exits to DOS                        */
/***************************************************************************/
void FatalErrCustom( unsigned msg )
{
   char			*apszText[ MAX_STRINGS ];

	GetMessage( apszText, msg );

	PushHelp ( 0xffff );	               // No help for this
	HelpLine( CONT_HLP );

   PromptWindow( apszText, CR_Response, GetErrorColor(), NULL);
	ProgramCleanUp( RESTORE_SCREEN );			/* Restore video & interrupts */

	if ( vCurrentFile.chValidErrors == REBOOT )
		RebootSystem();
	else
	{
		VideoCleanup();
		ProgramExit( 1 );       /* Exit to DOS with error code set */
	}
}


/***************************************************************************/
/* Prompts user to place a diskette in a drive and press return.				*/
/*																									*/
/* Need to do a disk reset after each disk change because of a bug in		*/
/* DOS 3.2																						*/
/*																									*/
/*	void PromptForDisk( char *szName, char chDrive, int IsNewDisk )			*/
/*																									*/
/*	ARGUMENTS:	szName	- String describing disk									*/
/*					chDrive	- Drive letter for to to be inserted into				*/
/*					IsNewDisk- 0 if we need to include warning that files wil	*/
/*								  be erased from the disk when it's preped.			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void PromptForDisk( char *szName, char chDrive, int IsNewDisk )
{
	char			*apszText[ MAX_STRINGS ];
	int 			OldHelp;
	char			szTmp[ MAX_SCRN_LINE_LEN ];
	unsigned		uDiskMsg;

#ifdef	HD_BACKUP
	uDiskMsg = DISK_WARNING_TEXT;
#else
	uDiskMsg = IsNewDisk ? DISK_WARNING_TEXT : DISK_PROMPT_TEXT;
#endif

	GetMessage( apszText, uDiskMsg );

	if( uDiskMsg == DISK_WARNING_TEXT )
	{
		apszText[4] = szName;
		sprintf( szTmp, apszText[6], chDrive );
		apszText[6] = szTmp;
	}
	else	/* Must be DISK_PROMPT_TEXT */
	{
		apszText[4] = szName;
		sprintf( szTmp, apszText[2], chDrive );
		apszText[2] = szTmp;
	}

	OldHelp = GetHelpFlags();
	HelpLine( CONT_HLP |
				 (vCurrentFile.chValidErrors != REBOOT ? EXIT_HLP : 0) );

	VideoPutChar( BEL );

	PromptWindow( apszText, CR_Response, GetPromptColor(), NULL );
	HelpLine ( OldHelp );
	_dos_dskreset();
}

/***************************************************************************/
/* Displays a window with the status of files being copied. It uses the		*/
/* global structure "vinfo" to get the name of the file and whether it is	*/
/* is being read or written.																*/
/* 																								*/
/* void FileStatusUpdate( void ) 														*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void FileStatusUpdate( void )
{
	char		*apszStatus[ STATUS_LINES ];
	char		szStr[ 50 ];


	GetMessage( apszStatus, STATUS_TEXT );

	strcpy( szStr, apszStatus[vCurrentFile.chReadWrite] );
	strcpy( szStr + FNAME_OFFSET, vCurrentFile.szFileName );
	PadStr( szStr, SPC, STATUS_LINE_MAX );
	
	VideoPutsRowCol( LAST_VIDEO_ROW, STATUS_START_COL, szStr );
}

/***************************************************************************/
/* Sets up the current file structure and then calls FileStatusUpdate to	*/
/* display the	read-write message and file name on the screen's current    */
/* status line.																				*/
/* 																								*/
/* void DisplayFileStatus( char *szFileName, int Type )							*/
/* 																								*/
/* ARGUMENTS:	szFileName	- Name of current file in the form "NAME.EXT"	*/
/* 				Type			- Flag access type of READ or WRITE 				*/
/* RETURNS: 	void																			*/
/***************************************************************************/

void DisplayFileStatus( char *szFileName, int Type )
{
	vCurrentFile.chReadWrite = (unsigned char)Type;
	strcpy( vCurrentFile.szFileName, szFileName );
	FileStatusUpdate();
}

/************************************************************************
 * This function should be called if the user presses a key which 		
 * causes the program to end. It will prompt the user to confirm that
 * they wish to end the program and if the user confirms it will call	
 * ProgramAbort().
 *
 * Note: For Janus, if abort is confirmed, we will never return from
 * 		AbortPrompt().
 *																								
 * void AllowAbort( void )																
 *																								
 * ARGUMENTS:	NONE																		
 *	RETURNS:		void																		
 *																								
 ************************************************************************/
void AllowAbort( void )
{
	AbortPrompt();
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
	ProgramCleanUp( RESTORE_SCREEN );

	if ( vCurrentFile.chValidErrors == REBOOT )
		RebootSystem();
	else
		ProgramExit( 0 );
}	

/***************************************************************************/
/* Prompts the user to confirm abort what ever they are doing. User can    */
/* confirm or not.                                         						*/
/*                                                                         */
/* RETURNS: TRUE to abort, otherwise FALSE                                        */
/***************************************************************************/

int AbortPrompt( void )
{
	int		  	YesNo, AnyChar = 0;
	int 			OldHelp;
   extern int  YesNoResponse[];
   char		  	*apszText[ MAX_STRINGS ];

#if (UPGRADE_PROGRAM != 0) || (RECOVERY_PROGRAM != 0)
	GetMessage( apszText, vCurrentFile.chValidErrors == REBOOT ?
								 REBOOT_EXIT_TEXT : EXIT_TEXT );
#elif (OEM_PROGRAM != 0) && (UJANUS != 0)
	if ( vInfo.Flag.fFloppyBoot && vInfo.Flag.fOsFound )
		GetMessage( apszText, FLOPPY_EXIT_TEXT );
	else
		GetMessage( apszText, EXIT_TEXT );
#else
	if ( vInfo.Flag.fFloppyBoot )
		GetMessage( apszText, FLOPPY_EXIT_TEXT );
	else
		GetMessage( apszText, EXIT_TEXT );
#endif

  			/*Disable help during this prompt.*/
	PushHelp ( 0xffff );	

	OldHelp = GetHelpFlags();
	HelpLine( 0 );
	YesNo = PromptWindow( apszText, &AnyChar, GetErrorColor(), NULL );
	HelpLine( OldHelp );

	PopHelp();

	/* M200: F3 is intercepted in GetChar(), and is never passed back to
	 * this function through PromptWindow(), so we always return FALSE.
	 */
	return( FALSE );
}

/************************************************************************/

void WriteProtectPrompt( char Disk )
{
	char			*apszText[ WRITE_PROTECT_LINES ];
	int			OldHelp;
	extern char *apszReadWriteError[];
	char			szTmp[ MAX_SCRN_LINE_LEN ];

	GetMessage( apszText, WRITE_PROTECT_TEXT );
	apszText[0] = apszReadWriteError[ WRITE ];
	sprintf( szTmp, apszText[2], Disk );
	apszText[2] = szTmp;

	OldHelp = GetHelpFlags();
	HelpLine( CONT_HLP |
				 (vCurrentFile.chValidErrors != REBOOT ? EXIT_HLP : 0) );

	PromptWindow( apszText, CR_Response, GetErrorColor(), GetErrorBuffer() );
	HelpLine( OldHelp );
}


/************************************************************************/

void NotReadyPrompt( char Disk )
{
	int			OldHelp;
	char			*apszText[ NOT_READY_LINES ];
	extern char *apszReadWriteError[];
	char			szTmp[ MAX_SCRN_LINE_LEN ];

	if ( vCurrentFile.chReadWrite > 1 )
		vCurrentFile.chReadWrite = WRITE;

	GetMessage( apszText, NOT_READY_TEXT );
	apszText[0] = apszReadWriteError[vCurrentFile.chReadWrite];
	sprintf( szTmp, apszText[2], Disk );
	apszText[2] = szTmp;

	OldHelp = GetHelpFlags();
	HelpLine( CONT_HLP |
				 (vCurrentFile.chValidErrors != REBOOT ? EXIT_HLP : 0) );

	PromptWindow( apszText, CR_Response, GetErrorColor(), GetErrorBuffer() );
	HelpLine( OldHelp );
}


/***************************************************************************/

void CompressedDiskFullPrompt (void)
{
  unsigned long cbMinFree;
  char *apszText[MAX_STRINGS];         /* Error message text */
  char  szParam[16];
  char szBuf1[MAX_SCRN_LINE_LEN];
  char szBuf2[MAX_SCRN_LINE_LEN];

  /*Disable help */
  PushHelp ( 0xffff );

  /* Choose the correct error message */
  if (gfCompressedDiskCritSection == TRUE)
    GetMessage (apszText, CMP_DISK_FULL_UNBOOTABLE_TEXT);
  else
    {
      GetMessage (apszText, CMP_DISK_FULL_BOOTABLE_TEXT);
      // Insert the drive letter
      szParam[0] = vInfo.chDestin;
      szParam[1] = 0;
      ReplaceParam (apszText, "%2", szParam, szBuf1, TRUE);

#if defined(UPGRADE_PROGRAM) || defined(OEM_PROGRAM)
      // Insert the free-space-required figure
      cbMinFree = atol(GetDataString (COMPONENTS_BYTES, DOS_INSTALL_BYTES));
      ReplaceParam (apszText, "%1", ultoac(cbMinFree,szParam), szBuf2, TRUE);
#endif
    }

  HelpLine (CONT_HLP);

  PromptWindow (apszText, CR_Response, GetErrorColor(), GetErrorBuffer());

  PopHelp();
}


/***************************************************************************/

#if	(UPGRADE_PROGRAM != 0) || (RECOVERY_PROGRAM != 0)

/***************************************************************************/
/* A lot of code is used in both the upgrade and recovery programs so		*/
/* this function must be called by the main function of the program to		*/
/* allow the actual program to be detected.											*/
/*																									*/
/* void SetIsUpgrade( int fTrueFalse )													*/
/*																									*/
/* ARGUMENTS: int fTrueFalse		- Boolean TRUE if upgrade program 			*/
/*											- else FALSE if recovery program				*/
/* RETURNS	: void																			*/
/*																									*/
/***************************************************************************/

void SetIsUpgrade( int TrueFalse )
{
	extern int	IsUpgrade;

	IsUpgrade = TrueFalse;
}

#endif

/***************************************************************************/
/* Functions specific to the retail upgrade program.								*/
/***************************************************************************/

#ifdef	UPGRADE_PROGRAM

/***************************************************************************/
/* Prompts the user to do a final committal to do a hard disk upgrade. 		*/
/* User can press 'Y' to start upgrade or F3 to exit Setup.					*/
/*																			*/
/*	int CommittPrompt( void )												*/
/*																			*/
/* ARGUMENTS:	NONE														*/
/* RETURNS:		OK															*/
/*																			*/
/***************************************************************************/

int CommittPrompt( void )
{
	int		YesNo = 0xFF;
	static int	ValidChars[] = { UCASE_YES, LCASE_YES, 0 };// IPG- ucase_yes,
	int		OldHelp;                                   //      lcase_yes
	char	*apszText[ CONFIRM_LINES ];

	WorkAreaCls();
	HelpLine( START_HLP );

	GetMessage( apszText, CONFIRM_TEXT );

	OldHelp = GetHelpFlags();
	HelpLine(  START_HLP | EXIT_HLP );

	do
	{
		YesNo = PromptWindow( apszText, ValidChars, GetErrorColor(), NULL );
	}
	while (toupper (YesNo) != UCASE_YES); // IPG- ucase_y to ucase_yes

	return (OK);
	HelpLine( OldHelp );
}


#endif


#if (UPGRADE_PROGRAM != 0) || (OEM_PROGRAM != 0) || (HD_BACKUP != 0)

/***************************************************************************/
/* Displays a "PLEASE WAIT" "......" message and then								*/
/* returns leaving the message on the screen.										*/
/*																									*/
/*	void WaitPrompt( int message )														*/
/*																									*/
/* ARGUMENTS:																					*/
/*		message -- define for message that is displayed underneath 				*/
/*					"PLEASE WAIT"																*/
/*					   																			*/
/* RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void WaitPrompt( int	Message )
{
	char		*apszText[ WAIT_LINES + 5];

	GetMessage( apszText, WAIT_TEXT );
	GetMessage( &apszText[WAIT_LINES-1], Message );

	HelpLine( 0 );
	PromptWindow( apszText, NULL, GetPromptColor(), NULL );
}

#endif


#ifndef	HD_BACKUP

/***************************************************************************/
/* Displays an error message on the screen along with the file name being	*/
/* read or written. If the BuildRecovery flag is set the user will not be	*/
/* allowed to continue and the program will be aborted. If this flag is 	*/
/* not set the user will just be told that what file was not copied and 	*/
/* to press any key to continue. 														*/
/* 																								*/
/* int ProcessCopyError( char *FileName, int ReadWrite )							*/
/* 																								*/
/* ARGUMENTS:	FileName 	- The name of the file being read or written		*/
/* 				ReadWrite	- Flags if the file is being read or written 	*/
/* RETURN:		int			- OK always (may abort program and not return)	*/
/*																									*/
/***************************************************************************/

void ProcessCopyError( char *szFileName, int Type )
{
	char		*apszMessage[ COPY_ERROR_LINES ];
	char		*apszError[ ERROR_STRINGS_LINES ];
	char		*apszType[ VALID_RESPONSE_LINES ];
	int		OldHelp;

	GetMessage( apszMessage, COPY_ERROR_TEXT );
	GetMessage( apszError, ERROR_STRINGS_TEXT );
	GetMessage( apszType, VALID_RESPONSE_TEXT );

	apszMessage[5] = GetMemory( 80 );
	strcpy( apszMessage[5], apszError[ Type ] );
	strcat( apszMessage[5], "--> " );
	strcat( apszMessage[5], szFileName );

	#ifdef	OEM_PROGRAM
		apszMessage[7] = apszType[ 0 ];
	#else
		apszMessage[7] = apszType[ CreatingRecovery != FALSE ];
	#endif

	OldHelp = GetHelpFlags();
	HelpLine( CONT_HLP | EXIT_HLP );

	PromptWindow(apszMessage, CR_Response, GetErrorColor(), GetErrorBuffer());
	HelpLine( OldHelp );

	FreeMemory( apszMessage[5] );

	#ifdef	UPGRADE_PROGRAM
	{
		extern int		CreatingRecovery;

		if ( CreatingRecovery )	/* Need to abort if in middle of recovery disk */
			ProgramAbort();
	}
	#endif
}

#if 0 /* BUGBUG: unused? */
/************************************************************************/

void FatalDiskError( char Disk )
{
	char			*apszText[ FATAL_DISK_LINES ];
	char			*apszMessage[ MAX_STRINGS ];
	extern char *apszReadWriteError[];
	char			szTmp[ MAX_SCRN_LINE_LEN ];

	GetMessage( apszText, FATAL_DISK_TEXT );

	#ifdef OEM_PROGRAM
		GetMessage( apszMessage, RESTART_TEXT );
	#else
		GetMessage( apszMessage, (vCurrentFile.chValidErrors == REBOOT) ?
										  RECOVER_TEXT : RESTART_TEXT );
	#endif

	apszText[4] = apszMessage[0];
	apszText[5] = apszMessage[1];
	apszText[0] = apszReadWriteError[ 2 ];
	sprintf( szTmp, apszText[2], Disk );
	apszText[2] = szTmp;

	HelpLine( 0 );
	PromptWindow( apszText, CR_Response, GetErrorColor(), GetErrorBuffer() );

	ProgramAbort();
}
#endif

/***************************************************************************/
/* Prompts user to remove all diskettes and press any key to reboot.			*/
/*																									*/
/* void RebootPrompt( void )																*/
/*																									*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void RebootPrompt( void )
{
   char		*apszText[ MAX_STRINGS ];
	
	DisplayFileStatus( "", CLEAR );

#if (UPGRADE_PROGRAM != 0)
    if ( vInfo.Args.fManual)
        GetMessage( apszText, MANUAL_DONE_TEXT);

	else if ( vInfo.Flag.fHardInstall )
	{
   	    VideoPutChar( BEL );
	    WorkAreaCls();
		GetMessage( apszText, HD_REMOVE_FLOPPY_TEXT );
	    PromptWindow( apszText, CR_Response, GetPromptColor(), NULL );

		if ( Install.Software == SOFTWARE_DOSWIN )
			GetMessage( apszText, REBOOT_WINSETUP_TEXT );
		else
		{
			if ( Install.Flags.fUninstall )
				GetMessage( apszText, REBOOT_CONTINUE_TEXT );
			else
				GetMessage( apszText, HD_REBOOT_TEXT );
		}
	}
	else
		GetMessage( apszText, FD_DONE_TEXT );
#elif (RECOVERY_PROGRAM != 0)
	GetMessage( apszText, REBOOT_CONTINUE_TEXT );

#else
	if ( vInfo.Flag.fHardInstall )
   {
   	VideoPutChar( BEL );
	   WorkAreaCls();
		GetMessage( apszText, HD_REMOVE_FLOPPY_TEXT );
	   PromptWindow( apszText, CR_Response, GetPromptColor(), NULL );
   }

	GetMessage( apszText, vInfo.Flag.fHardInstall ?
								 HD_REBOOT_TEXT : FD_DONE_TEXT);
#endif

	WorkAreaCls();
	HelpLine( vInfo.Flag.fHardInstall ? CONT_HLP : (CONT_HLP | EXIT_HLP) );

	PromptWindow( apszText, CR_Response, GetPromptColor(), NULL );
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

#ifndef RECOVERY_PROGRAM
	strcat( Install.szCmdLine, " /B" );
	strcat( Install.szWinCmdLine, " /B" );
#endif
}

#endif

/************************************************************************/


int Int24DiskError( char Disk )
{
	char			*apszText[ INT_24_LINES ];
	char			*apszOptions[ INT_24_EXIT_LINES ];
	char			szTmp[ MAX_SCRN_LINE_LEN ];

	GetMessage( apszText, INT_24_TEXT );
	GetMessage( apszOptions, INT_24_EXIT_TEXT );
	sprintf( szTmp, apszText[4], Disk );
	apszText[4] = szTmp;

	return( PromptSelect( apszText, apszOptions ) );
}


/***************************************************************************/
/* Displays the help screen associated with the value on the help stack		*/
/* pointed to by HelpStackPtr. Waits for the user to press a key and then	*/
/* restores the original screen. Functions which want to use help should	*/
/* push a help indentifier on the help stack with the macro PushHelp( x )	*/
/* and then call GetChar() to get input from the user. If F1 is pressed 	*/
/* GetChar will this function. If the value of *HelpStackPtr == -1 the 		*/
/* function will return without displaying any text.								*/
/*                                                                         */
/*	void Help( void )																			*/
/*                                                                         */
/* ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*                                                                         */
/*	EXTERNS:		HelpMemErrText	- Declared in EXTERN.C								*/
/*                                                                         */
/* johnhe - 12/29/89																			*/
/***************************************************************************/

void Help( void )
{
	char					*apszError[ ERROR_LINES ];
	char					*ScrnBuf;
	extern unsigned	HelpMemErrText;

	GetMessage( apszError, HelpMemErrText	);		/* Get mem error message	*/

	if ( *(HelpStackPtr-1) != 0xffff	)				/* Is valid help on stack?	*/
	{
		if ( (ScrnBuf = malloc( 5000 )) != NULL )	/* Is enough memory? 		*/
		{
			free( ScrnBuf );								/* Free this block			*/
			DisplayHelp();									/* Show the help				*/
		}
		else
			Error( apszError );							/* Too little memory 		*/
	}
}

/***************************************************************************/
/* Saves the contents of the original screen and then diplays a help			*/
/* screen. Must save the contents of the screen as well as the cursor size	*/
/* and location.																				*/
/*                                                                         */
/*	void DisplayHelp( void )																*/
/*                                                                         */
/* ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*                                                                         */
/***************************************************************************/

void DisplayHelp( void )
{
	char				*apszText[ MAX_STRINGS ];	/* Help strings	*/
	char				*ScrnBuf;					/* Storage for orignal screen	*/
	int 				OldHelp;
	long				Cursor;
	static WINDOW	Wind = { 0, 24, 0, 79, 0, 0, 0 }; /* Screen description	*/

	ScrnBuf = GetMemory( 4000 );				/* Allocate size of screen	*/
	Cursor = SaveCursor();
	VideoCursOff();								/* Turn cursor off			*/

	WindowMove( &Wind, ScrnBuf, SAVE );		/* Save current screen		*/

	SetHelpColors( VideoIsColor() && !vInfo.Args.fIsMono ? TRUE : FALSE );

	GetMessage( apszText, *(HelpStackPtr - 1));/* Display help screen	*/
	VideoCls( GetBackGroundColor() );
	DisplayScreenHeader( 1 );
	DisplayText( apszText, TITLE_ROW );

	OldHelp = GetHelpFlags(); 					/* Save original help flags*/
	HelpLine( PREV_HLP | EXIT_HLP );

	PushHelp( 0xffff );							/* Can't have help on help	*/
	while( GetChar() != ESC )					/* Wait for <RETURN>			*/
		;

	SetDefaultColors( VideoIsColor() && !vInfo.Args.fIsMono ? TRUE : FALSE );

	PopHelp();										/* Turn help back on			*/

	WindowMove( &Wind, ScrnBuf, RESTORE );	/* Restore original screen */
	HelpLine ( OldHelp );						/* Restore orig. help flags*/
	RestoreCursor( Cursor );

	FreeMemory( ScrnBuf );
}


/***************************************************************************
 * Program exit.
 *																									
 * void ProgramExit( int iStatus )
 * 																							
 * ARGUMENTS:	iStatus - Exit status.
 * 				
 * RETURNS	:	NEVER (Does not return to caller.)
 *
 ***************************************************************************/
void ProgramExit( int iStatus )
{
   unsigned i;

   //
   // Set user back to the drive they started from
   //
   if (vInfo.drStart != 0)                   // Make sure its been initialized
      _dos_setdrive(vInfo.drStart, &i);

#if (OEM_PROGRAM != 0) && (UJANUS != 0)
	/* If existing operating system found, OK to exit to command-line. */

	if ( vInfo.Flag.fOsFound  || vInfo.fOemCmdLineOK )
		exit( iStatus );
	else
 		ErrorEndPrompt( DRIVE_ERROR_TEXT );
#else
	exit( iStatus );
#endif
}


#if (UPGRADE_PROGRAM != 0) || (OEM_PROGRAM != 0)

/***************************************************************************
 * void _Assert(char*,int);
 *
 * Called as a result of an assertion failure. Will print out an error
 * dialog containing the file and line number of where the assertion failure
 * occured and warning that program is being aborted and then wait for any
 * key to be pressed. After a key a general cleanup is done by closing open
 * file, restoring all of the original interrupt handlers, cleaning up the
 * video and then return to DOS.
 *	
 * ENTRY: Only from an assertion failure.
 * EXIT : Fatal Error (exit to dos).
 *
 ***************************************************************************/

void _Assert(char *szFile, int line)
{
#ifdef DEBUG
	char			*apszMessage[ ASSERT_FAIL_LINES ];
	char			*apszExit[ EXIT_MSG_LINES ];
	char			szTmp[MAX_SCRN_LINE_LEN];

	GetMessage( apszMessage, ASSERT_FAIL_TEXT );
	GetMessage( apszExit, EXIT_MSG_TEXT );

   sprintf(szTmp,apszMessage[2],szFile,line);
   apszMessage[2] = szTmp;
	apszMessage[3] = apszExit[vCurrentFile.chValidErrors];

	HelpLine( CONT_HLP );

   PromptWindow( apszMessage, CR_Response, GetErrorColor(), GetErrorBuffer());
	ProgramAbort();
#else
	szFile = szFile;
	line = line;
#endif
}


/***************************************************************************
 * DebugMsg(char*,int,char*);
 *
 * Will print out an message dialog containing the file and line number of
 * where the call occurred and a user specified string.  It will then wait for
 * any key to be pressed. When a key is pressed, the function returns.
 *
 * ENTRY:
 * EXIT : none
 *
 *
 ***************************************************************************/

void DebugMsg(char *szFile, int line, char *szText)
{
#ifdef DEBUG
	char			*apszMessage[ DEBUG_MESSAGE_LINES ];
	char			szTmp[MAX_SCRN_LINE_LEN];

	GetMessage( apszMessage, DEBUG_MESSAGE_TEXT );
   sprintf(szTmp,apszMessage[2],szFile,line);
   apszMessage[2] = szTmp;
   apszMessage[3] = szText;

	HelpLine( CONT_HLP | EXIT_HLP );

	PromptWindow( apszMessage, CR_Response, GetErrorColor(), GetErrorBuffer());
#else
	szFile = szFile;
	line = line;
	szText = szText;
#endif
}


/***************************************************************************
 * void FatalMsg(unsigned);
 *
 * Called as a result of an unrecoverable error. Will print out an message
 * dialog describing the error, and warning that program is being aborted
 * and then wait for any key to be pressed. After a key a general cleanup
 * is done by closing open file, restoring all of the original interrupt
 * handlers, cleaning up the video and then return to DOS.
 *
 * ENTRY: unsigned uOffset:	Offset of message text.
 * EXIT : Fatal Error (exit to dos).
 *
 * NOTE: This function differs from FatalError() in that these error messages
 * 		typically consist of more than one line; therefore, the offset and
 *			number of message lines are passed, rather than just a message index.
 *			Also, unlike FatalError(), this function will not reboot your
 *			system.
 *
 ***************************************************************************/

void FatalMsg(unsigned uOffset)
{
	char	*apszMessage[ MAX_STRINGS ];

	GetMessage( apszMessage, uOffset );
	HelpLine( CONT_HLP );
   PromptWindow( apszMessage, CR_Response, GetErrorColor(), GetErrorBuffer());
	ProgramAbort();
}


/***************************************************************************
 * Displays a TTY error message on the screen and enters an infinite loop.
 * This effectively terminates SETUP and does not return the User to the
 * command prompt.
 *																									
 * void ErrorEndTty( unsigned uOffset )
 * 																							
 * ARGUMENTS:	uOffset - Offset of error message text to display.
 * 				
 * RETURNS	:	NEVER (Does not return to caller.)
 *
 ***************************************************************************/
void ErrorEndTty( unsigned uOffset )
{
	int	i;
	char	*apszText[ MAX_STRINGS ];

	GetMessage( apszText, uOffset );

	for( i = 0; apszText[ i ] != NULL; i++ )
	{
		VideoPuts( apszText[i] );
		VideoPutChar( CR );
		VideoPutChar( 0x0a );
	}

	while( TRUE )
		;		/* Wait forever */
}


#ifdef	OEM_PROGRAM		/* NOT NEEDED BY UPGRADE_PROGRAM */
/***************************************************************************
 * Prompts with an error message on the screen and enters a loop.
 * This effectively terminates SETUP Asking for CtrlAltDel OR it
 * will allow user to return to the command prompt by pressing ENTER.
 *																									
 * void ErrorEndPrompt( unsigned uOffset )
 * 																							
 * ARGUMENTS:	uOffset - Offset of error message text to display.
 * 				
 * RETURNS	:	NEVER (Does not return to caller.)
 *
 *					[ DEBUG WILL RETURN - NOT EXIT ]
 *
 ***************************************************************************/
void ErrorEndPrompt( unsigned uOffset )
{
	int	i;
	char	*apszText[ MAX_STRINGS ];
	int		ExitChoice = 0xFF;

	GetMessage( apszText, uOffset );

	VideoPutChar( CR );
	VideoPutChar( 0x0a );
	for( i = 0; apszText[ i ] != NULL; i++ )
	{
		VideoPuts( apszText[i] );
		VideoPutChar( CR );
		VideoPutChar( 0x0a );
	}

#ifdef DEBUG

	/* DEBUG can avoid exit with ENTER at prompt */
	{
		VideoPutChar( CR );
		VideoPutChar( 0x0a );
		VideoPuts( DebugContinueMessage );
		VideoPutChar( CR );
		VideoPutChar( 0x0a );
	}

	ExitChoice = 0xff & KbdGetKey();

	if ( CR == ExitChoice)
		return;
	else
		exit(3);
#else
	do
	{	/* Wait for Enter or CtrlAltDel */
		ExitChoice = 0xff & KbdGetKey();
	}
	while ( CR != ExitChoice);

	exit( 3 );
#endif
}
#endif	/* OEM_PROGRAM	- NOT NEEDED BY UPGRADE_PROGRAM */


/***************************************************************************
 * Displays an error message on the screen and enters an infinite loop.
 * This effectively terminates SETUP and does not return the User to the
 * command prompt.
 *																									
 * void ErrorEndMsg( unsigned uOffset )
 * 																							
 * ARGUMENTS:	uOffset - Offset of error message text to display.
 * 				
 * RETURNS	:	NEVER (Does not return to caller.)
 *
 ***************************************************************************/
void ErrorEndMsg( unsigned uOffset )
{
	char	*apszText[ MAX_STRINGS ];

	AbortEnabled = FALSE;	/* Don't allow user to exit to command prompt */
	GetMessage( apszText, uOffset );
	WorkAreaCls();
	HelpLine(0);
	PromptWindow( apszText, NULL, GetErrorColor(), NULL );

	while( TRUE )
		;		/* Wait forever */
}

#endif

