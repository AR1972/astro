/***************************************************************************/
/*																									*/
/*	WORK.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/*	Functions used by the backup program to display the user menus and get	*/
/* the user's selections.																	*/
/*																									*/
/*	johnhe 07-05-91																			*/
/***************************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include		<malloc.h>
#include		<dos.h>

#include		<alias.h>
#include    <bios_io.h>
#include		<disk_io.h>
#include		<hdisk.h>
#include		<strlib.h>
#include		<window.h>
#include		<message.h>
#include		<backup.h>
#include		<global.h>
#include		<install.h>

/************************************************************************/


void		GetDrvList		( char *DrvList );
void		BuildDrvList	( char *DrvList, char *DrvText[] );
int		HardDriveMenu	( unsigned char *apszDrvText[], int LastDrv );
void		SpawnUpgrade	( void );
int		WantToExit		( void );

extern int GetSelection( char **apszList, struct InputDef *NewDef  );

/************************************************************************/
/* GetDestFloppy() 																		*/
/*																								*/
/* Displays the drive letter for each floppy drive in the system and		*/
/* allows the user to use the cursor keys to select one of the drives	*/
/* to be used for the upgrade. If the system only contain 1 floppy		*/
/* drive the drive A: is automatically selected with no input from the	*/
/* the user and no prompt displayed. vinfo.chDestin is set to the			*/
/* selected letter and then path string vinfo.szPath is updated unless	*/
/* the user presses ESC to return to the previous slection screen. If	*/
/* only 1 floppy drive is installed in the system the prompt won't be	*/
/* shown and the destination path will be set to "A:\".						*/
/*																								*/
/* Arguments:	NONE																		*/
/* Returns:		void																		*/
/*																								*/
/************************************************************************/

int GetDestFloppy( void )
{

	#define		BOX_TOP			(TITLE_ROW + FD_DRIVE1_LINES - 1)
	#define		BOX_BOT			(BOX_TOP +  FD_DRVS_LINES - 2)
	#define		BOT_ROW			(BOX_BOT + 1 )

	char			chDrive;
   char			*apszText[ MAX_STRINGS ];

	static struct InputDef Def = { BOX_TOP, BOX_BOT, START_COL2,
											 END_COL2, -1, 0, 0, 0 };


	PushHelp( SEL_DRV_HELP_TEXT );
	WorkAreaCls();
	GetMessage( apszText, FD_DRIVE1_TEXT );
	DisplayText( apszText, TITLE_ROW );

	GetMessage( apszText, FD_DRIVE2_TEXT );
	DisplayText( apszText, BOT_ROW );

	GetMessage( apszText, FD_DRVS_TEXT );
	apszText[vInfo.chNumFloppy] = NULL;
	Def.Bottom = Def.Top + vInfo.chNumFloppy - 1;
											/* Get the drive number */
	Def.Current = 	vInfo.chDestin - 'A';

	if ( lpInstall )
	{
		HelpLine( CONT_HLP | F1_HLP | EXIT_HLP | (PREV_HLP << 1) );

		chDrive = (char)GetSelection( apszText, &Def );
	}
	else
	{
		HelpLine( CONT_HLP | F1_HLP | EXIT_HLP );

		while ( (chDrive = (char)GetSelection( apszText, &Def )) == PREVIOUS )
			;
	}

	PopHelp();

	if ( chDrive != PREVIOUS )
	{
		vInfo.chDestin = chDrive + 'A';		/* Set drive letter */

		*(vInfo.szPath) = vInfo.chDestin;	/* Construct the path string */
		*(vInfo.szPath + 1) = ':';
		*(vInfo.szPath + 2) = EOL;
	}

	return( chDrive != PREVIOUS ? OK : PREVIOUS );
}

/************************************************************************/
/* Prompts the user to select a hard drive letter using a sroll bar		*/
/* menu. Each hard drive which has already been selected and backed up	*/
/* are marked on the menu with a check to show they have already been	*/
/* completed. Returns the drive letter of the selected drive.				*/
/*																								*/
/*	int GetHardDrive( int iLastStatus )												*/ 
/*																								*/
/*	ARGUMENTS:	iLastStatus	- Status of last hard drive backup attempt	*/
/*	RETURNS:		int	- Drive letter of selected drive or -1 if abort		*/
/*																								*/
/************************************************************************/

int GetHardDrive( int iLastStatus )
{
	static char					DrvList[ 27 ];
	static unsigned char		*apszDrvText[ 27 ];
	static int					LastDrv = -1;
	static int					ThisDrv = -100;
	
								/* If ThisDrv == -100 it means this is the	*/
								/* the first call to this function so the		*/
								/* lists of hard drives must be built			*/

	if ( ThisDrv == -100 )
	{
 		GetDrvList( DrvList );
		BuildDrvList( DrvList, apszDrvText );
		ThisDrv = -1;
	}
								/* Else see if we need to mark the last hard	*/
								/* that was selected as having been backed 	*/
								/* up successfully.									*/

	else if ( iLastStatus == OK )
	{
		LastDrv = ThisDrv;
		apszDrvText[LastDrv][0] = (char)('*');	/* Add check mark to menu	*/
	}


	if ( DrvList[0] == 0 )
		FatalError( FATAL_NO_HARD_ERROR );

								/* Set the global source drive letter			*/
	do
	{
		ThisDrv = HardDriveMenu( apszDrvText, LastDrv );

		if ( ThisDrv != PREVIOUS )
			vInfo.chSource = vInfo.chFirstHd + (char)ThisDrv;
	}
	while ( ThisDrv == ERROR );

	return( ThisDrv );
}

/************************************************************************/
/* Displays the text and allows menu selection for hard drive selection	*/
/* base on list of drives past as the parameter. Will determine the 		*/
/* drive to highlight base on the parameter LastDrv which is the int		*/
/* offset in the array of total hard drive of the last drive backed up.	*/
/* If this value is -1 it mean no drives have been done.						*/
/*																								*/
/*	int HardDriveMenu( char *apszDrvText[], int LastDrv );					*/
/*																								*/
/*	ARGUMENTS:	apszDrvText	-	Array of pointers to the menu text			*/
/*					LastDrv		-	Last drive in the array to be backedup		*/
/*	RETURNS:		int			-	User's selection or PREVIOUS if ESC			*/
/*																								*/
/* NOTE:			This function follows the following logic when deciding	*/
/*					which drive letter should be highlighted.						*/
/*																								*/
/*					DRIVE = LAST DRIVE DONE + 1										*/
/*					IF ( DRIVE IS ALREADY DONE )										*/
/*						DRIVE = NEXT DRIVE NOT DONE									*/
/*					IF ( DRIVE > MAX DRIVES )											*/
/*						DRIVE = FIRST DRIVE NOT DONE									*/
/*					IF ( DRIVE > MAX DRIVES )											*/
/*							DRIVE = 0														*/
/************************************************************************/

int HardDriveMenu( unsigned char *apszDrvText[], int LastDrv )
{

	#define		BOX2_TOP			(TITLE_ROW + CHOOSE_HD1_LINES - 1)
	#define		BOX2_BOT			21

   char			*apszText[ MAX_STRINGS ];
	int			i;
	int			iDrive;
	int			TotalStrs;


	static struct InputDef Def = { BOX2_TOP, BOX2_BOT, START_COL2,
											 END_COL2, -1, 0, 0, 0 };

	PushHelp( SEL_HD_HELP_TEXT );
	WorkAreaCls();
	GetMessage( apszText, CHOOSE_HD1_TEXT );
	DisplayText( apszText, TITLE_ROW );

	HelpLine( CONT_HLP | F1_HLP | EXIT_HLP | PREV_HLP );

	Def.Current = 	(UCHAR)(LastDrv + 1); /* Determine next drive to backup	*/
	TotalStrs = GetNumberStrings( apszDrvText );

	Def.Bottom = Def.Top + TotalStrs - 1;
	if ( Def.Bottom > BOX2_BOT )
		Def.Bottom = BOX2_BOT;	

	if ( Def.Current >= (UCHAR)TotalStrs ||
		  apszDrvText[Def.Current][0] == '*' )  /* IPG changed to Asterick */
	{
		for ( i = 0, Def.Current = 0; i < TotalStrs; i++ )
		{
			if ( apszDrvText[i][0] != '*' ) /* IPG changed to Asterick */
			{
				Def.Current = (UCHAR)i;
				break;
			}
		}
		if ( i == TotalStrs &&				/* See if all are backed up	*/
			  WantToExit() == OK )			/* Offer to exit program */
		{
			SpawnUpgrade();					/* Spawn the upgrade program */
		}
	}

	iDrive = GetSelection( apszDrvText, &Def );

	PopHelp();

	return( iDrive );
}

/************************************************************************/
/*	Fills in a an array with all valid drive letters which are not			*/
/* floppy disk supported by the ROM BIOS.											*/
/*																								*/
/*	void GetDrvList( char *DrvList )													*/
/*																								*/
/*	ARGUMENTS:	DrvList	- Array of chars to be filled in. (Max 27 chars)*/
/*	RETURNS:		void																		*/
/*																								*/
/************************************************************************/

void GetDrvList( char *DrvList )
{
	register	i;
	register	ThisDrv;
	
	ThisDrv = (int)vInfo.chFirstHd - ('A' - 1);

	for( i = 0; IsValidHardDrive( (char)(ThisDrv - 1) + 'A' ); ThisDrv++ )
		DrvList[i++] = (char)(ThisDrv - 1);
	DrvList[i] = 0;									/* Mark end of list				*/
}


/************************************************************************/
/* Uses the drive letters in the array DrvList to build an array of		*/
/* strings to be displayed in the hard disk selection menu. The bits		*/
/* in BitMap will be checked and any bits that are set will cause the	*/
/* corresponding drive to be marked as having already been backuped.		*/
/*																								*/
/*	void BuildDrvList( char *DrvList, int LastDrv, char *DrvText[],		*/
/*						 char *BitMap[] )													*/
/*																								*/
/*	ARGUMENTS:	DrvList	- Array of drive format of A=0, B=1, C=2, etc.	*/
/*					DrvText	- Array to be filled with ptrs to strings which	*/
/*								  will be displayed in the HD selection menu.	*/
/*	RETURNS:		void																		*/
/*																								*/
/************************************************************************/

void BuildDrvList( char *DrvList, char *DrvText[] )
{
	char			*apszText[ HD_DRVS_LINES ];
	int			i;

	GetMessage( apszText, HD_DRVS_TEXT );

	for ( i = 0; DrvList[i] != 0; i++ )
		DrvText[ i ] = apszText[ DrvList[i] ];

	DrvText[ i ] = NULL;

}


