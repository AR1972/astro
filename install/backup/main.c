/***************************************************************************/
/*																									*/
/* MAIN.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Entry point for the DOS 4.x retail upgrade package. The functions in		*/
/* this module decide if an upgrade can be done and what type of disk		*/
/* is being upgraded and then call the appropriate functions to begin		*/
/* letting the user determine the new configuration.								*/
/*																									*/
/* Created 03-01-89 - johnhe																*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include    <malloc.h>
#include		<dos.h>
#include		<message.h>
#include		<malloc.h>
#include		<string.h>
#include		<process.h>

#include		<alias.h>
#include		<bios_io.h>
#include		<disk_io.h>
#include		<window.h>
#include		<global.h>
#include		<backup.h>
#include		<install.h>

/************************************************************************/

#define		MIN_PARAGRAPHS		4096		/* Need this much memory to run */

/************************************************************************/

void	main					( int argc, char **argv );

void	ProgramInitialize	( void );
void	SpawnUpgrade		( void );
void	UpgradeReturnPrompt( void );

int	IsEnoughMemory		( void );
int	IsDiskSizeOk		( void );
int	IsVersionOk			( void );
int	InitCpyBuffer		( void );
static void ZenithBugCheck( void );

int	GetDestFloppy		( void );
int	GetHardDrive		( int iStatus );
int	DisplayTotals		( void );

unsigned	GetMemoryFree	( void );
int	GetDosDataVersion	( void );

char 	*ParseFileName		( char *szPath );

char far *DosInstallData( void );

/************************************************************************/

INSTALL far 		*lpInstall = NULL;		/* Address of Dos Setup resident
														 * data.
														 */

static unsigned	fInit;						/* Global flags for this module */

static char			**Args;						/* Command line arguments */

static char			*szPath;

/************************************************************************/
/* Entry function for DOS 4.0 Retail Upgrade. This function will first	*/
/* see if the system is upgrade-able and then initialize the video and	*/
/* interrutp handlers before allowing either a hard or floppy disk		*/
/* upgrade to be performed. 															*/
/* 																							*/
/* void main( int argc, char **argv )												*/
/*																								*/
/* ARGUMENTS: 	argc  - 	Number of command line switches.						*/
/*				  	argv  -	Array of ptrs to command line switch strings		*/
/* RETURNS	:	void																		*/
/*																								*/
/************************************************************************/

void	main( int argc, char **argv )
{
	register				iBkUpStatus;
	extern char	huge	*pchCpyBuffer;
	static char			*szInstall = IPG_SZINSTALL; // IPG - this string moved to window.h
	char					*apszText[ INIT_ERROR_LINES ];

	if ( (ErrorBuffer = malloc( 2000 )) == NULL ||
			(szPath = GetMemory( 80 )) == NULL )
	{
		GetMessage( apszText, INIT_ERROR_TEXT );
		puts( apszText[0] );				/* Display issuficient memory error	*/
		exit( -1 );
	}
	szPath = GetMemory( 80 );

	if ( _osmajor < 3 )
		strcpy( szPath, "A:\\" );
	else
		strcpy( szPath, argv[0] );

	strcpy( ParseFileName( szPath ), szInstall );

	/* Check if we've been spawned by Dos Setup */

	if ( ((lpInstall = (INSTALL far *)DosInstallData()) == NULL) ||
		  (strcmp(lpInstall->szSignature, SIGNATURE) != 0) )
		lpInstall = NULL;

	argc = argc;
	Args = argv;

	if ( argc > 1 )
	{
		strupr( Args[1] );
		if ( strchr( Args[1], 'B' ) )
			vInfo.Args.fIsMono = TRUE;
		if ( strchr( Args[1], 'I' ) )										/* m100	*/
			vInfo.Args.fNoVideoChk = TRUE;								/* m100	*/
	}

	ProgramInitialize();				/* Initialize video & interrupts */

	if ( InitCpyBuffer() == OK )
	{
		while ( GetDestFloppy() != PREVIOUS )
		{
			iBkUpStatus = OK;
			while ( GetHardDrive( iBkUpStatus ) != PREVIOUS )
				iBkUpStatus = Backup( vInfo.chSource, vInfo.chDestin );
		}
		hfree( pchCpyBuffer );
	}
	else
		FatalError( FATAL_MEMORY_ERROR );

	SpawnUpgrade();
}

/************************************************************************/
/*																								*/
/************************************************************************/

void SpawnUpgrade( void )
{
	struct find_t	Buf;

#if 0	/* This is only needed if we are going to respawn Setup. */
	ZenithBugCheck();
#endif

	HelpLine( CONT_HLP | EXIT_HLP );

	if ( szPath[0] <  vInfo.chFirstHd )
	{
		do
		{
			WorkAreaCls();
			UpgradeReturnPrompt();
		}
		while ( _dos_findfirst( szPath, _A_NORMAL, &Buf ) != OK );
	}
	else
		WorkAreaCls();

#if 1
	ProgramCleanUp( TRUE );
	exit( 0 );											/* Return to parent process */
#else
/* BUGBUG: Should we restore video also before returning to SETUP? */
	ProgramCleanUp( FALSE ); 						/* Restore interrupts only */
	WaitPrompt ( SETUP_WAIT );
	execl( szPath, szPath, Args[ 1 ], NULL );
	VideoCleanup();
	exit( -1 );
#endif
}

/************************************************************************/
/*																								*/
/************************************************************************/

void UpgradeReturnPrompt( void )
{
	char			*apszText[ UPGRADE_LINES ];
	char			*DrvPos;

	GetMessage( apszText, UPGRADE_TEXT );

	if ( (DrvPos = strchr( apszText[2], ':' )) != NULL )
		*(DrvPos-1) = szPath[0];

	PromptWindow( apszText, CR_Response, GetPromptColor(), NULL );
}

#if 0
/************************************************************************/
/* This function checks for a system running MS-DOS 2.x on a Zenith		*/
/* computer by checking the version number and then looking for a 'ZDS'	*/
/* string at memory location f000:800c. If both checks pass the user    */
/* will need to restart Setup because of a bug in Zenith DOS 2.11.   	*/
/* (The bug was such that the re-spawn of Setup would fail.)				*/
/* A dialog will instruct the user what to do and then the program will */
/* cleanup and exit, leaving the user at the DOS prompt. If either of	*/
/* the tests fail the function will just return to the caller.				*/
/*																								*/
/*	void ZenithBugCheck( void )														*/
/*																								*/
/*	ARGUMENTS:	NONE																		*/
/*	RETURNS:		void																		*/
/*																								*/
/************************************************************************/

void ZenithBugCheck( void )
{
	char					*apszText[ ZENITH_BUG_LINES ];
	static char far	*ZenithSig = (char far *)(0xf000800cL);

	if ( _osmajor == 2 && strncmp( ZenithSig, "ZDS", 3 ) == OK )
	{
		HelpLine( CONT_HLP );							/* Set help line			*/
		PushHelp( 0xffff );								/* Disable help system	*/
		WorkAreaCls();

		GetMessage( apszText, ZENITH_BUG_TEXT );
		PromptWindow( apszText, CR_Response, GetPromptColor(), NULL );

		ProgramCleanUp( TRUE );			/* Cleanup int vectors and screen	*/
		exit( 0 );
	}
	else
		return;
}
#endif


/***************************************************************************
 * char far * DosInstallData(void);
 *
 * Use our reserved Int2Fh function (INT2F_INSTALL_DATA) to get a pointer
 * to resident data structures.  Will return NULL if the function is not
 * being serviced (i.e. no one has previously hooked Int2Fh to service this
 * function).
 * 
 *
 * ENTRY: void
 * EXIT : Address of resident data structures,
 *			 else NULL
 *
 * HISTORY: M200: Created.
 *
 ***************************************************************************/

char far * DosInstallData( void )
{
	int 				i;
	char far 		*lpData = NULL;
	union REGS 		inregs, outregs;
	struct SREGS 	segregs;

	inregs.x.ax = INT2F_INSTALL_DATA;

	segread( &segregs );

	i = int86x( INT2F, &inregs, &outregs, &segregs );

	if( outregs.x.ax == 0xFFFF && ((outregs.x.bx | segregs.es) != 0))
	{
		FP_SEG(lpData) = segregs.es;
		FP_OFF(lpData) = outregs.x.bx;
	}

	return( lpData );
}


