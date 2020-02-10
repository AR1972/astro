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
#include		<window.h>
#include		<bios_io.h>
#include 	<data.h>
#include 	<strlib.h>
#include		"message.h" 			/* Must get from current directory */
#include 	<format.h>

/************************************************************************/

							/* Bit flags which are set as each disk is prepped */
static unsigned	DiskFlags = 0;

static char			chDestin;					/* Destination drive letter	*/
                                          /* MUST be capital letter.    */

static char 		*CurrentSource;
static char 		*CurrentDestin;

static unsigned long	StartTime = 0;			/* Time 1st disk was labeled	*/
static unsigned long	LastTime  = 0;			/* Time last disk was labeled	*/

int					iDskFmtType =  0;			/* Default format type			*/
int					DiskNum;						/* User disk number				*/

static struct BPB	Bpb;							/* Bpb from current disk		 */
int					iDrv;							/* Destination physical number */

long                                    MinSize[] = { 0, 350000L, 720000L, 1211000L, 1455000L };

int IsHardDisk(int chDrive);

/***************************************************************************/
/* Check for a distribution disk being inserted and prompts the user for 	*/
/* the disk until the label on the user inserted disk matches the one in	*/
/* the DistDisk[] array element for that disk number.								*/
/*																									*/
/*	void DispInsertDisk( int DiskNum )													*/
/*																									*/
/* ARGUMENTS:	DiskNum	- Distribution disk number									*/
/* RETURNS:		void																			*/
/*																									*/
/* GLOBALS:		vInfo		- Checks vInfo.chSource to find the drive for the	*/
/*								  to look for the disk on.									*/
/*					DistDisk[] - Gets the label to check for from this array of	*/
/*									 of pointers to strings.								*/
/*																									*/
/***************************************************************************/

void DispInsertDisk( int DiskNum )
{
	char			szLabel[15];
	char			*szDisk;
	char			*szPrompt;
	char			*pchBuffer;
	register 	iStatus;
									/* If hard drive don't need to do anything */
	if ( DiskNum == NOT_REMOVEABLE )
		return;

	pchBuffer = GetMemory( SECTOR_SIZE );

	szDisk = GetDistribLabel( DiskNum );
	szPrompt = GetDistribPrompt( DiskNum );
	iStatus = ERROR;

	do
	{
		while( !IsDiskReady( vInfo.chSource - 'A' ) )
			PromptForDisk( szPrompt, vInfo.chSource, OLD_DISK );

		if ( IsFormatted( vInfo.chSource - 'A', &Bpb, pchBuffer ) != FALSE )
			if ( GetDiskLabel( vInfo.chSource-'A', szLabel, &Bpb  ) != (UL)(-1L))
				if ( (iStatus = strcmp( szLabel, szDisk )) == OK )
					iStatus = OK;

		if ( iStatus != OK )
			PromptForDisk( szPrompt, vInfo.chSource, OLD_DISK );
	}
	while ( iStatus != OK );

#ifdef	UPGRADE_PROGRAM
	_dos_dskreset();
#endif
	FreeMemory( pchBuffer );
}

/***************************************************************************/
/* Check for a user's disk being inserted and prompts the user for the		*/
/* disk until the label on the user inserted disk matches the one in the	*/
/* DistDisk[] array element for that disk number.									*/
/*																									*/
/*	void DispInsertUserDisk( int UserDisk, char chDrive )							*/
/*																									*/
/* ARGUMENTS:	UserDisk	- User disk number											*/
/*					chDrive	- Drive letter													*/
/* RETURNS:		void																			*/
/*																									*/
/* GLOBALS:		vInfo		- Checks vInfo.chDestin to find the drive for the	*/
/*								  to look for the disk on.									*/
/*																									*/
/***************************************************************************/

void DispInsertUserDisk( int UserDisk, char chDrive )
{
	register			iStatus;
	char				*szUserLabel;
	char				*szPrompt;
	char				szLabel[15];
	char				*pchBuffer;
	unsigned			BitFlag;

										/* If hard drive don't need to do anything */
	if ( IsHardDisk( chDrive ) )
		return;
#if 0
	if ( chDrive >= (char)(GetNumberOfDrives() + 'A') )
		return;
#endif

	pchBuffer = GetMemory( SECTOR_SIZE + 80 );
	szPrompt = pchBuffer + SECTOR_SIZE;

	szPrompt = GetUserPrompt( UserDisk );
	szUserLabel = GetUserLabel( UserDisk );

	chDestin = chDrive;						/* Set the drive letter */
	DiskNum = UserDisk;
	BitFlag = 1 << UserDisk;				/* Set bit flag */

	do
	{
		if ( (DiskFlags & BitFlag) == 0 )			/* Need to prep this disk	*/
			iStatus = PrepNewDsk( szUserLabel, szPrompt );
		else
		{
			iStatus = REDO_DISK;

			if ( IsDiskReady( chDestin - 'A' ) )
			{
				if ( IsFormatted( chDestin - 'A', &Bpb, pchBuffer ) != FALSE )
				{
					if ( GetDiskLabel( chDestin-'A', szLabel, &Bpb ) != (UL)(-1L))
						if ( strcmp( szLabel, szUserLabel ) == OK )
							iStatus = OK;
				}
			}
			if ( iStatus != OK )
				PromptForDisk( szPrompt, chDrive, OLD_DISK );
		}
	}
	while ( iStatus != OK );

	DiskFlags |= BitFlag;				/* Signal disk has been preped */
	FreeMemory( pchBuffer );
	_dos_dskreset();
}

int IsHardDisk(int chDrive)
{
	return(chDrive >= vInfo.chFirstHd);
}

#ifndef	RECOVERY_PROGRAM

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
/*	int PrepNewDsk( char *szLabel, char *szPrompt )									*/
/*																									*/
/* ARGUMENTS:	szLabel - Ptr to label string for this disk						*/
/*	RETURNS:		int	  - OK is disk is ready to use or ABORT if user elects*/
/*																									*/
/***************************************************************************/

int PrepNewDsk( char *szLabel, char *szPrompt )
{
	char				*pchBuffer;
	register 		iStatus;
	UL					ulTime;

	iDrv = chDestin - 'A';								/* Find bios disk number	*/
	pchBuffer = GetMemory( SECTOR_SIZE );

	do
	{
		iStatus = OK;							/* No errors as of yet				*/

		do											/* Wait until disk is present 	*/
			PromptForDisk( szPrompt, chDestin, NEW_DISK );
		while( !IsDiskReady( iDrv ) );

													/* See if we need to format disk */
		if ( IsFormatted( iDrv, &Bpb, pchBuffer ) == FALSE )
		{
			if ( FormatNewDsk() != OK )
				iStatus = BAD_MEDIA;				/* Can't use this disk */
		}
													/* Make sure not a previous disk */
													/* and then srub the disk			*/

		else if ( (iStatus = IsNewDisk( iDrv )) == OK )
			iStatus = ScrubFatRoot( iDrv, &Bpb );

		if ( iStatus == OK )					/* Disk seems ok so label it	*/
		{
			if ( GetDiskFree( chDestin ) < MinSize[ vInfo.uchFloppyType	] )
				iStatus = NO_SPACE;
			else if ( (ulTime = FloppySetDiskLabel( iDrv, szLabel, &Bpb )) != (UL)(-1L) )
			{
				LastTime = ulTime;
				if ( StartTime == 0L )				/* If this is first disk	*/
					StartTime = ulTime;				/* need to set first time	*/
			}
			else
				iStatus = BAD_MEDIA;				/* Can't use this disk */
		}

		if ( iStatus != OK )
		{
			ProcessDiskError( iStatus );
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
	int						iDrv;
	extern struct BPB		DskBpb[];

	iDrv = chDestin - 'A';

	if ( iDskFmtType == 0 )
	{
		if ( (iDskType = GetDskFmtType()) == ABORT )
			iStatus = ABORT;
	}
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
	if ( chDestin == vInfo.chSource && iDskType < vInfo.uchFloppyType + 2 )
		iDskType = vInfo.uchFloppyType + 2;

	if ( iStatus != ABORT )
		if ( (iStatus = FormatFloppy( (UINT)iDrv, iDskType, FmtStat )) == OK )
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
	int			iSaveDefault; 					/* Signals make choice static		*/
	static unsigned char	DrvOpts[6][2] = {	{ 3,0 },		/* 360K	*/
														{ 3,5 },		/* 1.2M	*/
														{ 4,0 },		/* 720K	*/
														{ 4,6 } };	/* 1.44M	*/

	pchBuf = GetMemory( 100 );								/* Ioctl work buffer		*/
	iDrvType = GetDriveType( (UCHAR)iDrv, pchBuf );	/* Determine drive type	*/

							/* 360k or 720k drives only allow a single disk type	*/
	if ( iDrvType == 1 || iDrvType == 3 )
		iDskIndex = 0,	iSaveDefault = TRUE;

	else if ( (iDrvType == 2 && vInfo.uchFloppyType == 3) ||
				 vInfo.uchFloppyType > 2 ||
				 (iDrvType == 2 && vInfo.uchFloppyType == 2) )
		iDskIndex = 1,	iSaveDefault = TRUE;

						/* If we got here it means the drive will allow disks		*/
						/* bigger than distribution disk so let user decide		*/
	else
	{
		iDskIndex = PromptForDiskFmt( (UINT)(iDrvType-1) >> 1 );
		iSaveDefault = TRUE;
	}

						/* Convert the disk type into a type used by the format	*/
						/* function.															*/

	iFmtType = DrvOpts[iDrvType-1][iDskIndex];
	if ( iSaveDefault == TRUE )
			iDskFmtType	= iFmtType;				/* Save global default */

	FreeMemory( pchBuf );
	return( iFmtType );
}

#endif

