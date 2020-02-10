/***************************************************************************/
/*																									*/
/* FMT_IO.C 																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Backup utility for the MS-DOS 4.0 upgrade. These functions will       	*/
/* backup a specified disk to another specified disk. Then entry point to	*/
/* the utiltity is BackUp(). The file coping is buffered using an area of	*/
/* memory which as allocated previously by a call to InitCpyBuffer().		*/
/* Most functions comminuicate via a group of static global variables to	*/
/* keep the size of code to a mininum. 												*/
/*																									*/
/* The backup format is restorable with most versions of DOS 2.x and above.*/
/* 																								*/
/* Created 07-12-89 johnhe																	*/
/* TABS = 3																						*/
/***************************************************************************/


#include		<stdio.h>
#include		<stdlib.h>
#include		<bios.h>
#include		<dos.h>
#include		<string.h>

#include		<alias.h>

#ifdef	OEM_PROGRAM
	#include		<oem.h>
#else
	#include		<global.h>
#endif

#include		<disk_io.h>
#include 	<data.h>
#include 	<strlib.h>
#include 	<format.h>
#include		<prompts.h>
#undef NULL
#include 	"lib\common\sulib.h"
#include    "wsrc.h"

/************************************************************************/

#define		BOOT_DRVLET			'A'
#define		BOOT_DRVNUM			0
/************************************************************************/
static unsigned long	StartTime = 0;			/* Time 1st disk was labeled	*/
static unsigned long	LastTime  = 0;			/* Time last disk was labeled	*/

int					iDskFmtType =  0;			/* Default format type			*/

static struct BPB	Bpb;							/* Bpb from current disk		 */

long					MinSize[] = { 0, 350000L, 720000L, 1211000L, 1455000L };

/***************************************************************************/

static int		PrepUninstallDsk( char *szLabel );
void 				PromptForUninstall( char *szUserLabel );

/*
 *  Declared in towin.c, always contains the currect uninstall disk
 *  number (zero based)
 */
extern int iUninstallDiskNum;

/***************************************************************************/
/* Check for a distribution disk being inserted and prompts the user for 	*/
/* the disk until the label on the user inserted disk matches the one in	*/
/* the DistDisk[] array element for that disk number.								*/
/*																									*/
/*	void InsertDisk( int DskNum )    													*/
/*																									*/
/* ARGUMENTS:	DskNum	- Distribution disk number									*/
/* RETURNS:		void																			*/
/*																									*/
/* GLOBALS:		vInfo		- Checks vInfo.chSource to find the drive for the	*/
/*								  to look for the disk on.									*/
/*					DistDisk[] - Gets the label to check for from this array of	*/
/*									 of pointers to strings.								*/
/*																									*/
/***************************************************************************/

void InsertDisk( int DskNum )
{
	char			szLabel[15];
	char			*szDisk;
	char			*pchBuffer;
	register 	iStatus;

	/* If hard drive don't need to do anything */

	if ( DskNum == NOT_REMOVEABLE )
      return;

	pchBuffer = GetMemory( SECTOR_SIZE );

	szDisk = GetDistribLabel( DskNum );
	iStatus = ERROR;

	do
	{
		while( !IsDiskReady( vInfo.chSource - 'A' ) )
			PromptForDisk( vInfo.chSource );

		if ( IsFormatted( vInfo.chSource - 'A', &Bpb, pchBuffer ) != FALSE )
			if ( GetDiskLabel( vInfo.chSource-'A', szLabel, &Bpb  ) != (UL)(-1L))
				if ( (iStatus = strcmp( szLabel, szDisk )) == OK )
					iStatus = OK;

		if ( iStatus != OK )
			PromptForDisk( vInfo.chSource );
	}
	while ( iStatus != OK );

	_dos_dskreset();
	FreeMemory( pchBuffer );
}

/***************************************************************************/
/* Check for a user's disk being inserted and prompts the user for the		*/
/* disk until the label on the user inserted disk matches the one in the	*/
/* DistDisk[] array element for that disk number.									*/
/*																									*/
/* void InitUninstallDisk( int iUserDisk )                                 */
/*																									*/
/* ARGUMENTS:	UserDisk	- User disk number											*/
/*					                                                      		*/
/* RETURNS:		void																			*/
/*																									*/
/* GLOBALS:		vInfo		- Checks vInfo.chDestin to find the drive for the	*/
/*								  to look for the disk on.									*/
/*																									*/
/***************************************************************************/

void InitUninstallDisk( int iUserDisk )
{
	register			iStatus;
	char				*szUserLabel;
	char				szLabel[15];
	char				*pchBuffer;
	static int		iFirstTime = 999;

	pchBuffer = GetMemory( SECTOR_SIZE + 80 );
	szUserLabel = GetUserLabel( iUserDisk );

	do
   {
      /*
       *  If this is the first time through for an un-install disk we'll
       *  need to prepare it.
       */
		if ( iFirstTime != iUserDisk )
      {
         do
         {
			   PromptForUninstall(szUserLabel);      /* Wait until disk present */
		   }
         while( !IsDiskReady( BOOT_DRVNUM ) );

			iStatus = PrepUninstallDsk( szUserLabel ); /* Do this only once per */
         iFirstTime = iUserDisk;                    /* each uninstall disk.  */
      }
		else
		{
			iStatus = REDO_DISK;

			if ( IsDiskReady( BOOT_DRVNUM ) )
			{
				if ( IsFormatted( BOOT_DRVNUM, &Bpb, pchBuffer ) != FALSE )
				{
					if ( GetDiskLabel( BOOT_DRVNUM, szLabel, &Bpb ) != (UL)(-1L))
						if ( strcmp( szLabel, szUserLabel ) == OK )
							iStatus = OK;
				}
			}
			if ( iStatus != OK )
				PromptForUninstall(szUserLabel);
		}
	}
	while ( iStatus != OK );

	FreeMemory( pchBuffer );
	_dos_dskreset();
}

/***************************************************************************/
/* Prepares a diskette as user diskette. First prompts user for a disk		*/
/* and loops until a disk has been inserted. Then checks to see if disk 	*/
/* is formatted and if not formats the disk. If the disk is formatted a		*/
/* check is made to be sure the user hasn't inserted a disk which was 		*/
/* previously used during this backup session. If the disk is useable		*/
/* a call is do to CleanFatDir() which will clear the FAT and root			*/
/* directory to free all of the disk for use. If there are errors the 		*/
/* function will continue to loop until a valid disk is inserted or the		*/
/* user elects to abort the backup.														*/
/*																									*/
/*																									*/
/***************************************************************************/

int PrepUninstallDsk( char *szLabel )
{
	char				*pchBuffer;
	register 		iStatus;
	UL					ulTime;

	pchBuffer = GetMemory( SECTOR_SIZE );

	do
	{
		iStatus = OK;							/* No errors as of yet				*/

      /*
       *  Assure disk was re-inserted after the error dialog was dismissed. 
       *  Bug fix #172 MC.
       */
      while( !IsDiskReady( BOOT_DRVNUM ) )
         NotReadyPrompt(BOOT_DRVNUM);

													/* See if we need to format disk */
		if ( IsFormatted( BOOT_DRVNUM, &Bpb, pchBuffer ) == FALSE )
		{
			if ( FormatNewDsk() != OK )
				iStatus = BAD_MEDIA;				/* Can't use this disk */
		}
													/* Make sure not a previous disk */
													/* and then srub the disk			*/

		else if ( (iStatus = IsNewDisk( BOOT_DRVNUM )) == OK )
			iStatus = ScrubFatRoot( BOOT_DRVNUM, &Bpb );

		if ( iStatus == OK )					/* Disk seems ok so label it	*/
		{
			if ( GetDiskFree( BOOT_DRVLET ) < MinSize[ vInfo.uchFloppyType	] )
				iStatus = NO_SPACE;
			else if ( (ulTime = SetDiskLabel( BOOT_DRVNUM, szLabel, &Bpb )) != (UL)(-1L) )
			{
				LastTime = ulTime;
				if ( StartTime == 0L )				/* If this is first disk	*/
					StartTime = ulTime;				/* need to set first time	*/
			}
			else
				iStatus = BAD_MEDIA;					/* Can't use this disk */
		}

		if ( iStatus != OK )
		{
			ProcessDiskError( iStatus + IDS_DSKPREPERR );
			iStatus = REDO_DISK;
		}
	}
	while( iStatus == REDO_DISK );

	FreeMemory( pchBuffer );
	return( iStatus );
}

/***************************************************************************/
/* Determines if a disk has already been used during the current backup		*/
/* seqence. Each backup disk has a label and the time and date on the		*/
/* along with the label sequence number to see if the label was created		*/
/* during the current installation process.											*/
/*																									*/
/*	int IsNewDisk( int iDrive )															*/
/*																									*/
/*	ARGUMENTS:	iDrive	- Physical drive number where diskette is located	*/
/*	RETURNS:		int		- OK if disk is not distrution or already used		*/
/*																									*/
/***************************************************************************/

int IsNewDisk( int iDrive )
{
	char				szLabel[15];						/* Disk label string 		*/
	char				*szBadLabel;						/* Ptr to unusable labels	*/
	int				i;										/* Loop counter				*/
	int				iStatus;								/* Return value				*/
	UL 				ulTime;								/* Label creation time		*/


	iStatus = OK;

	if ( (ulTime = GetDiskLabel( iDrive, szLabel, &Bpb )) != 0L &&
		  ulTime != (UL)(-1L) )
	{
				/* First loop thru all distribution labels looking for a match	*/
		for ( i = 0;
				iStatus == OK &&
				(szBadLabel = GetDistribLabel( i )) != NULL;
				i++ )
			if ( strcmp( szLabel, szBadLabel ) == OK	)
				iStatus = DIST_DISK;

				/* Loop thru looking for a label from a disk program created	*/
		for ( i = 0;
				iStatus == OK &&
				(szBadLabel = GetUserLabel( i )) != NULL;
				i++ )
		{
			if ( strcmp( szLabel, szBadLabel ) == OK )
			{
				if ( ulTime >= StartTime && ulTime <= LastTime )
					iStatus = DISK_USED;
			}
		}
	}
	else if ( ulTime == (UL)(-1) )
		iStatus = BAD_MEDIA;

	return( iStatus );
}

/***************************************************************************/
/* Determines if the format type has been set and if not does a call to 	*/
/* the the format layout and then call the FormatFloopy function to do the	*/
/* format.																						*/
/*																									*/
/*	int FormatNewDsk( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/* RETURNS:		int	- OK if disk is formatted successfull else and error	*/
/*							  code from the format function or ABORT if the user	*/
/*							  chooses this at one of the prompts.						*/
/*																									*/
/***************************************************************************/

int FormatNewDsk( void )
{
	int						iStatus;
	int						iDskType;
	extern struct BPB		DskBpb[];

	if ( iDskFmtType == 0 )
		iDskType = GetDskFmtType();
	else
		iDskType = iDskFmtType;

   /***********************************
   *  vInfo.uchFloppyType: 1 ==> 360K
   *                       2 ==> 720K
   *                       3 ==> 1.2M
   *                       4 ==> 1.44M
   ************************************
   *  iDskType:            3 ==> 360K
   *                       4 ==> 720K
   *                       5 ==> 1.2M
   *                       6 ==> 1.44M
   ************************************/
   /* Check format type if we know we are formatting a disk in the drive
   ** from which Setup was run. */

	if ( (iStatus = FormatFloppy( BOOT_DRVNUM, iDskType, FmtStat )) == OK )
		memcpy( &Bpb, DskBpb + iDskType, sizeof( struct BPB ) );
	
	return( iStatus );
}

/***************************************************************************/
/*	Determines the type of disk format to use. If there is a choice which	*/
/* can be made as to the type of format we let the user decide and then		*/
/* see if the user wants to always use the format selected.						*/
/*																									*/
/*	int GetDskFmtType( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		int	- Disk format type as defined in FORMAT.H.				*/
/*																									*/
/***************************************************************************/

int GetDskFmtType( void )
{
	char			*pchBuf;							/* Work buff for GetDriveType()	*/
	int			iDrvType;						/* Type of destination drive		*/
	int			iDskIndex;						/* Index to format type arrays	*/
	int			iFmtType;						/* Disk format being returned		*/

	static unsigned char	DrvOpts[6][2] = {	{ 3,0 },		/* 360K	*/
														{ 3,5 },		/* 1.2M	*/
														{ 4,0 },		/* 720K	*/
														{ 4,6 } };	/* 1.44M	*/

	pchBuf = GetMemory( 100 );                		/* Ioctl work buffer		*/
																	/* Determine drive type	*/
	iDrvType = GetDriveType( (UCHAR)BOOT_DRVNUM, pchBuf );

   /* iDrvType (phyisical hardware) will be one of:
    *
	 *	 1	   360K   5.25"
	 *	 2	   1.2M   5.25"
	 *	 3	   720K   3.5"
	 *	 4	   1.44M  3.5"
    *
    *  vInfo.uchFloppyType (type of floppy user is installing from):
    *
    *  1 ==> 360K
    *  2 ==> 720K
    *  3 ==> 1.2M
    *  4 ==> 1.44M
    */
	if ( iDrvType == 1 || iDrvType == 3 ) /* 360k or 720k ? no choice then ! */
		iDskIndex = 0;

   else
      /*  User has a 1.2mb or 1.44mb drive, for these cases we allow them
       *  to choose their uninstall disk type.
       *  For 1.2mb drives, the choices are 1.2mb or 360kb.
       *  For 1.44mb drives, the choices are 1.44mb or 720kb.
       *
       *  iDskIndex return value will be 0 for 360kb/720kb, 1 for 1.2mb/1.44mb
       */

		iDskIndex = PromptForDiskFmt(iDrvType);

	/* Convert the disk type into a type used by the format function.	*/

	iFmtType = DrvOpts[iDrvType-1][iDskIndex];

	/* If there is a bad Uninstal disk we need to ask again		*/
	/*	iDskFmtType	= iFmtType;	*/			/* Save global default */

	FreeMemory( pchBuf );
	return( iFmtType );
}


