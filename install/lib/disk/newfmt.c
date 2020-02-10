/***************************************************************************/
/*																									*/
/* NEWFMT.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Modular Floppy disk format functions.												*/
/* 																								*/
/* This module is designed to format a floppy diskette under any version   */
/* of the MS-DOS (PC-DOS) operating system version 2.0 or greater on any	*/
/* IBM PC(AT) compatible. To accomplish this requirement will mean that		*/
/* the use of DOS function 44h to perform a format via an ioctl will not	*/
/* be allowed and all formating must be accomplished via the direct use		*/
/* of ROM BIOS interrupt 13h.																*/
/*																									*/
/* SUPPORTED FORMATS:																		*/
/* 																								*/
/* 5 1/4 INCH DRIVE - 40 TRACKS 2 HEADS - DOUBLE DENSITY							*/
/* -----------------------------------------------------  						*/
/* 360K  - 40 tracks - 09 sectors per track - 2 heads								*/
/* 320K  - 40 tracks - 08 sectors per track - 2 heads								*/
/* 180K  - 40 tracks - 09 sectors per track - 1 head								*/
/* 160K  - 40 tracks - 08 sectors per track - 1 head								*/
/*																									*/
/*																									*/
/* 5 1/4 INCH DRIVE - 80 TRACKS 2 HEADS - HIGH DENSITY							*/
/* ----------------------------------------------------- 						*/
/* 1.2M  - 80 tracks - 15 sectors per track - 2 heads								*/
/* 720K  - 80 tracks - 09 sectors per track - 2 heads								*/
/* 360K  - 40 tracks - 09 sectors per track - 2 heads								*/
/* 320K  - 40 tracks - 08 sectors per track - 2 heads								*/
/* 180K  - 40 tracks - 09 sectors per track - 1 head								*/
/* 160K  - 40 tracks - 08 sectors per track - 1 head								*/
/*																									*/
/*																									*/
/* 3 1/2 INCH DRIVE - 80 TRACKS 2 HEADS - DOUBLE DENSITY							*/
/* -----------------------------------------------------							*/
/* 720K  - 80 tracks - 09 sectors per track - 2 heads								*/
/* 360K  - 40 tracks - 09 sectors per track - 2 heads								*/
/* 320K  - 40 tracks - 08 sectors per track - 2 heads								*/
/*																									*/
/*																									*/
/* 3 1/2 INCH DRIVE - 80 TRACKS 2 HEADS - HIGH DENSITY							*/
/* -----------------------------------------------------							*/
/* 1.44M - 80 tracks - 18 sectors per track - 2 heads								*/
/* 720K  - 80 tracks - 09 sectors per track - 2 heads								*/
/* 360K  - 40 tracks - 09 sectors per track - 2 heads								*/
/* 320K  - 40 tracks - 08 sectors per track - 2 heads								*/
/*																									*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/
#include		<stdio.h>
#include 	<dos.h>
#include 	<bios.h>
#include 	<string.h>
#include 	<malloc.h>

#include		<alias.h>
#include		<disk_io.h>
#include		<format.h>

extern void 			NotReadyPrompt( char Disk );
extern void 			WriteProtectPrompt( char Disk );


/***************************************************************************/
/* Global variables which define the various characteristics of the disks	*/
/* which will be formatted.  All of the variables execept DskBpb are			*/
/* static to this module. DskBpb is used by other modules to avoid having	*/
/* duplicate data.																			*/
/***************************************************************************/

extern struct BPB DskBpb[];

#if	0									/* Array was moved to libary file */
											/* Array of valid BPB structures */
struct BPB DskBpb[] =
{
				/* 160K  - 40 tracks - 8 sectors per track - 1 head	*/
	{ 512, 1, 1, 2,  64, 1*8*40, 0xfe, 1,  8, 1, 0, 0L },

				/* 180K  - 40 tracks - 9 sectors per track - 1 head	*/
	{ 512, 1, 1, 2,  64, 1*9*40, 0xfc, 2,  9, 1, 0, 0L },

				/* 320K  - 40 tracks - 8 sectors per track - 2 heads	*/
	{ 512, 2, 1, 2, 112, 2*8*40, 0xff, 1,  8, 2, 0, 0L },

				/* 360K  - 40 tracks - 9 sectors per track - 2 heads	*/
	{ 512, 2, 1, 2, 112, 2*9*40, 0xfd, 2,  9, 2, 0, 0L },

				/* 720K  - 80 tracks - 09 sectors per track - 2 heads */
	{ 512, 2, 1, 2, 112, 2*9*80, 0xF9, 3,  9, 2, 0, 0L },

				/* 1.2M  - 80 tracks - 15 sectors per track - 2 heads	*/
	{ 512, 1, 1, 2, 224, 2*15*80, 0xF9, 7, 15, 2, 0, 0L },

				/* 1.44M - 80 tracks - 18 sectors per track - 2 heads	*/
	{ 512, 1, 1, 2, 224, 2*18*80, 0xF0, 9, 18, 2, 0, 0L }
};

#endif

					/* Sector layout areas required by bios format functions.	*/

static char						*WorkBuffer;
static struct FIELD_LIST	*FldLst;

							/* Used by the SetDskParams function to choose the		*/
							/* format type for a BIOS int 13h function 17h			*/
							/* USE: uchDiskType[DriveType][FormatType]				*/
							/*lint -e570 */
static unsigned char uchDiskType[5][7] =
							{
								{ ERR, ERR, ERR, ERR, ERR, ERR, ERR },
								{   1,   1,   1,   1, ERR, ERR, ERR },
								{   2,   2,   2,   2,   2,   3, ERR },
								{   4,   4,   4,   4,   4,   4, ERR },
								{   4,   4,   4,   4,   4,   4,   4 }
							};
										/*lint +e570 */
										/* Format gap length for each type of drive	*/

static unsigned char	DskFmtGap[]	= { 0x00, 0x50, 0x54, 0x50, 0x6c };

static int			iFmt;			/* Format type for current diskette */
static unsigned	uDrv;			/* Physical drive number 0-7fh		*/

static unsigned	uHead;		/* Current disk head						*/
static unsigned	uTrack;		/* Current disk track					*/

static struct BPB			*Bpb;				/* Ptr to BPB for specified format */

		 		/* Total number of tracks for each disk size */
static unsigned char	auchTotalTracks[] = { 40, 40, 40, 40, 80, 80, 80 };

static struct DSK_PARMS		DskParms;	/* Disk parameters for format	*/

									/* Disk parmeters if funct 18h not supported */
static struct DSK_PARMS		LocalDskParms =
									{ 0xdf, 0x02, 0x25, 0x02, 0x09, 0x1b, 0xff,
									  0x50, 0xf6, 0x0f, 0x04, 0x00 };

static char *puchFat;						/* Pointer to a work buffer */

/***************************************************************************/
/*	Formats a floppy diskette using int 13h function calls. Caller must		*/
/*	specify the drive number of 0 - 7fh and disk layout via a disk type of	*/
/* 0 - 6 as reflected in the table below.												*/
/*																									*/
/* -------------------------------------------------------						*/
/*																									*/
/* 0 | 160K  - 40 tracks - 08 sectors per track - 1 head							*/
/* 1 | 180K  - 40 tracks - 09 sectors per track - 1 head							*/
/* 2 | 320K  - 40 tracks - 08 sectors per track - 2 heads						*/
/* 3 | 360K  - 40 tracks - 09 sectors per track - 2 heads						*/
/* 4 | 720K  - 80 tracks - 09 sectors per track - 2 heads						*/
/* 5 | 1.2M  - 80 tracks - 15 sectors per track - 2 heads						*/
/* 6 | 1.44M - 80 tracks - 18 sectors per track - 2 heads						*/
/*																									*/
/* -------------------------------------------------------						*/
/*																									*/
/*																									*/
/* int FormatFloppy( unsigned uDrv, int iFmt, void (*vStatus)( int, int ))*/
/*																									*/
/* ARGUMENTS:	uDrv		Physical drive letter to format the disk on			*/
/*								which must be a floppy drive.								*/
/*					iFmt		Layout of the disk from the table above.				*/
/*					vStatus  Pointer to a function which will be called for		*/
/*								each time a track is formatted with the arguments	*/
/*								specifing the current track and head.					*/
/*																									*/
/* RETURNS:		A valid error code or OK (0) if format was successfull.		*/
/*					The error code are the same as those returned by an iotcl	*/
/*					format.																		*/
/* 																								*/
/*	GLOBALS:		uDrv			-  Sets this value to the physical drive #		*/
/*					Bpb			-	Sets this value to the BPB for specifed fmt	*/
/* 																								*/
/*																									*/
/***************************************************************************/

int FormatFloppy( unsigned uDrive, int iFormat, void (*vStatus)( UINT, UINT ) )
{
	register			iMaxTrack;		/* Number of tracks on selected media	*/
	register			iMaxHead;		/* Number of heads on selected media	*/
	void far			*OrigDskParms;	/* Original vector at 0x1e					*/
	int				iStatus;			/* Current error status						*/

	if ( iFormat > MAX_FORMAT_TYPE )
		return( -1 );

	uDrv = uDrive;						/* Set global drive number					*/
	iFmt = iFormat;					/* Set global format indexer				*/

	Bpb = &(DskBpb[iFmt]);			/* Set pointer to bpb for formt type	*/
	OrigDskParms = _dos_getvect( DSK_PARAMS_VECT ); 	/* Save vect 1eh	*/
	AllocFat();							/* Allocate a work buffer					*/

	if ( (iStatus = SetDskParms()) == OK )
	{
						/* Initialize register variables used as loop counters */
		iMaxTrack = (int)(auchTotalTracks[iFmt]);
		iMaxHead = (int)Bpb->uNumberHeads;

		if ( vStatus != NULL )
			(*vStatus)( (UINT)iMaxTrack, 0xffff	); /* Initialize status function */

							/* Outer loop once for each track on the disk and	*/
							/* inner loop once for each head on drive 			*/

		for ( uTrack = 0; iStatus == OK && uTrack < (UINT)iMaxTrack; uTrack++ )
			for ( uHead = 0; iStatus == OK && uHead < (UINT)iMaxHead; uHead++ )
			{
				if ( vStatus != NULL )
					(*vStatus)( uTrack, uHead );	/* Status update function */
				if ( (iStatus = FmtTrack()) != OK )
					iStatus = MarkBadTrack();
			}
		if ( iStatus == OK )
			if ( (iStatus = WriteBoot( (int)uDrive, Bpb, NULL )) == OK )
				if ( (iStatus = WriteFat()) == OK )
					iStatus = CreatRootDir( (int)uDrive, Bpb );
	}
	else
		iStatus = IVALID_DRIVE_TYPE;

	FreeMemory( puchFat );
	FreeMemory( WorkBuffer );

	_dos_setvect( DSK_PARAMS_VECT, OrigDskParms ); 	/* Restore vect 1eh */

	if ( vStatus != NULL )
		(*vStatus)( 0xffff, 0 );			/* De-initalize status function */
	return( iStatus );
}

/***************************************************************************/
/* Formats a track on a floppy disk using BIOS int 13h calls. Must first	*/
/* set the fields of the tracklayout and then does a format track call.		*/
/* The formatted track is checked with a call to verify the entire track	*/
/* can be read. If the verify fails a call is done to reset the disk and	*/
/* the format is done again or until a successful verify or until all		*/
/* retries specified in MAX_RETRIES have been exhausted.							*/
/*																									*/
/*	int FmtTrack( void )																		*/
/*																									*/
/* ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/* GLOBALS:		DskParms	-	Must be pointed to by int vector 1eh on entry	*/
/*					Bpb		-	Must point to a BPB for disk being formatted		*/
/*					uTrack	-	Must contain current head being formatted			*/
/*					uHead		-	Must conatain current track being formatted		*/
/*																									*/
/***************************************************************************/

int FmtTrack( void )
{
	register					iRetry;			/* Current number of retries done	*/
	register					iStatus;			/* Return status value					*/
	struct diskinfo_t		DiskInfo;		/* Structure for int 13h calling		*/

	InitialFmtFlds();							/* Put track and heads in fields		*/

	DiskInfo.drive = uDrv;					/* Set up for bios int 13h call		*/
	DiskInfo.head = uHead;
	DiskInfo.track = uTrack;
	DiskInfo.sector = 1;
	DiskInfo.buffer = (void far *)(FldLst);

													/* Loop until the track is 			*/
													/* successfully formatted or all		*/
													/* retries have been exhausted		*/

	for ( iStatus = ERR, iRetry = 0;
			iStatus != OK && iRetry < MAX_RETRIES;
			iRetry++ )
	{
		DiskInfo.nsectors = Bpb->uSecPerTrack;

		if ( (iStatus = (_bios_disk( _DISK_FORMAT, &DiskInfo ) >> 8)) == OK )
		{
			DiskInfo.nsectors = Bpb->uSecPerTrack;
			iStatus = (_bios_disk( _DISK_VERIFY, &DiskInfo ) >> 8);
		}

		switch( iStatus )
		{
			case	OK:
				continue;			/* Quick out if no errors */
			case	06:			 	/* Disk change */
				iRetry = 0;
				break;
			case	DISK_TIME_OUT:
				NotReadyPrompt( (char)(uDrv + 'A') );
				iRetry = 0;
				break;
			case 03:
				WriteProtectPrompt( (char)(uDrv + 'A') );
				break;
			default:
				if ( iFmt == 6 && DskParms.FmtGap == 0x6c )
				{
					DskParms.FmtGap = 0x65;			/* Fix for 1.44 meg drives */
					iRetry = 0;							/* Allow max retries again */
				}
				break;
		}

					/* Must have been an error if it got here so do a reset */
		_bios_disk( _DISK_RESET, &DiskInfo );

	}
	return( iStatus );
}

/***************************************************************************/
/* Initializes the format lay fields required by the ROM BIOS format	track	*/
/* function. Each structure is the field is initialized to the current		*/
/* track and head being formatted. The uchSector and uchSize fields are		*/
/* static since we always assume 512 byte sectors and contigous sector		*/
/* numbers from 1 to n.																		*/
/*																									*/
/*	void	InitialFmtFlds( void )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/* RETURNS:		void																			*/
/*																									*/
/*	GLOBALS:		FldLst	- Changes the uchTrack and uchHead fields 			*/
/*					uTrack	- Must contain current head being formatted			*/
/*					uHead		- Must conatain current track being formatted		*/
/*					Bpb		- Must point to a BPB for disk being formatted		*/
/*																									*/
/***************************************************************************/

void	InitialFmtFlds( void )
{
	register		iSector;			/* Loop counter for each disk sector		*/

										/* Loop once for each sector on the track */
	for ( iSector = 0; iSector < (int)(0x24); iSector++ )
	{
		FldLst[iSector].uchTrack = (UCHAR)(uTrack);		/* Set track	*/
		FldLst[iSector].uchHead = (UCHAR)(uHead);			/* set head		*/
		FldLst[iSector].uchSector = (UCHAR)(iSector + 1);	/* Set sector	*/
		FldLst[iSector].uchSize = 2;							/* Set 512 bytes	*/
	}
}

/***************************************************************************/
/* Allocates a buffer large enough to hold one entire FAT copy as				*/
/* specified in the BPB for the current disk. Also allocates a work buffer	*/
/* to use for the track layout.															*/
/*																									*/
/*	void AllocFat( void )																	*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/* RETURN:		void																			*/
/*																									*/
/***************************************************************************/

void AllocFat( void )
{
	register		iFatSize;				/* Calculated size of FAT */

	iFatSize = (int)(Bpb->uBytesPerSec * Bpb->uSecPerFat);

	puchFat = GetMemory( (UINT)iFatSize );
	memset( puchFat, 0, (UINT)iFatSize );
	*(puchFat) = (UCHAR)Bpb->uchMediaDescr;
	*(puchFat+1) = (UCHAR)0xff;
	*(puchFat+2) = (UCHAR)0xff;

					/* Because of a bug in the PC BIOS we have to make		*/
					/* sure that the FldLst is not within 512 bytes of		*/
					/* a DMA boundary so allocate 2 * 512 bytes and use	*/
					/* the second 512 bytes of the buffer if the first 	*/
					/* is within 512 bytes of the DMA boundary.				*/

	WorkBuffer = GetMemory( 1024 );
	FldLst = (struct FIELD_LIST *) ( CheckDmaBound( WorkBuffer, 512 ) == OK ?
												WorkBuffer : WorkBuffer + 512 );
}

/***************************************************************************/
/* Marks a track as bad in the FAT in puchBuf. If any of the sectors are	*/
/* in clusters started on a previous track the joining cluster on the		*/
/* previous track is also marked bad. If the track that is bad contains		*/
/* clusters that are in the Boot, FAT or root directory area the disk is	*/
/* not use-able and BAD_DISK_ERROR will be returned.								*/
/*																									*/
/*	int MarkBadTrack( void )																*/
/*																									*/
/*	RETURNS:		int		- OK if no system clusters bad else BAD_DISK_ERROR	*/
/*																									*/
/*	GLOBALS:		uTrack	- Must contain current head being formatted			*/
/*					uHead		- Must conatain current track being formatted		*/
/*					Bpb		- Must point to a BPB for disk being formatted		*/
/*																									*/
/***************************************************************************/

int MarkBadTrack( void )
{
	register				fOddCluster;		/* Flags if even or odd offset	*/
	register				iSector;				/* Current sector	on a track		*/
	int					iCluster;			/* Current cluster number			*/
	int					iStatus;				/* Return status						*/
	unsigned char		*puchEntry;			/* Pointer to a FAT entry			*/

	puchEntry = puchFat;

								/* Find absolute starting sector of the bad track */

	iSector = Bpb->uSecPerTrack * uTrack * Bpb->uNumberHeads;
	iSector += (uHead * Bpb->uSecPerTrack);

	iSector -= GetSysSec();					 /* Subtract system sectors */

	if ( iSector >= 0  )						/* If not a system sector	*/
	{
		iCluster = iSector / Bpb->uchSecPerClus; /* Sector to cluster */
		iCluster += 2;						/* Add # of clusters for FAT signature */

		iCluster *= 3;							/* Div by 1 1/2 ( n*3 / 2 ) */
		fOddCluster = ( iCluster & 1 );	/* Save remainder of divide by 2 */
		iCluster >>= 1;						/* Divide by 2 */

		puchEntry += iCluster;				/* Add cluster number */

		for ( iSector = 0;
			   iSector < Bpb->uSecPerTrack;
		   	iSector += Bpb->uchSecPerClus )
		{
			if ( fOddCluster )					/* If this is an odd offset */
			{
				*(unsigned *)(puchEntry) |= (0xff7 << 4);
			 	puchEntry += 2;					/* Point to next FAT entry */
			}
			else										/* Else it's an even offset */
			{
				*(unsigned *)(puchEntry) |= 0xff7;
				puchEntry += 1;
			}
			fOddCluster ^= 1;						/* Toggle remainder bit */

		}
		iStatus = OK;								/* Disk is useable */
	}
	else
		iStatus = BAD_DISK_ERROR;				/* Disk is not useable	*/

	return( iStatus );
}

#if 0

/************************************************************************/
/* Copies the BPB structure for the current disk to a boot record and	*/
/* then writes the boot record to the disk.										*/
/*																								*/
/*	int WriteBoot( void )																*/
/*																								*/
/*	ARGUMENTS:	NONE																		*/
/*	RETURNS:		int		- OK or BIOS int 13h disk error						*/
/*																								*/
/*	GLOBALS:		Bpb			-	Must point to BPB struct for current disk	*/
/*					NewBootRec	-	Must point to buffer holding the boot rec	*/
/*																								*/
/************************************************************************/

int WriteBoot( void )
{
	extern char		NewBootRec;			/* 512 byte boot sector	*/

	memcpy( (&NewBootRec) + 11, Bpb, sizeof( struct BPB ) );

	return( WriteSectors( 0, 1, &NewBootRec ) );
}

#endif


/************************************************************************/
/* Writes the contents of puchFat to both FATs on a floppy disk.			*/
/*																								*/
/*	int WriteFat( void )																	*/
/*																								*/
/*	ARGUMENTS:	NONE																		*/
/*	RETURNS:		int	-	OK or BIOS int 13 write sector error code			*/
/*																								*/
/*	GLOBALS:		Bpb		-	Must point to BPB struct for current disk		*/
/*					puchFat	-	Must point to a a buffer with the new FAT		*/
/*																								*/
/************************************************************************/
int WriteFat( void )
{
	register		iCount;						/* Count of FATS written			*/
	register		iStatus;						/* Current ERROR status				*/
	unsigned		uSec;							/* Starting sector for the FAT	*/

	iStatus = OK;								/* This may change */

	for( uSec = 1, iCount = 0; iCount < 2 && iStatus == OK; iCount++ )
	{
		iStatus = WriteSectors( uSec, Bpb->uSecPerFat, puchFat );
		uSec += Bpb->uSecPerFat;
	}
	return( iStatus );
}

/************************************************************************/
/* Calculates the drive head which will access an absolute sector on		*/
/* a disk.																					*/
/* 																							*/
/* STRATAGEY: Head =																		*/
/* ((AbsSector % SectorPerTrack) * NumberOfHeads) / SectorsPerTrack		*/
/*																								*/
/*	unsigned DiskHead( unsigned uSec )												*/
/*																								*/
/* ARGUMENTS:	uSec		-  Absolute disk sector									*/
/* RETURNS:		unsigned	-	Head that will access the specified sector	*/
/*																								*/
/************************************************************************/

unsigned DiskHead( unsigned uSec )
{
	return( (uSec % Bpb->uSecPerTrack * Bpb->uNumberHeads)) /
			  Bpb->uSecPerTrack;
}


/************************************************************************/
/*	Calculates the total number of sectors on a disk used by the Boot		*/
/* record, all copies of the FAT and the root directory.	Uses the			*/
/* static global Bpb make the determination.										*/
/*																								*/
/*	int GetSysSec( void )																*/
/*																								*/
/* ARGUMENTS:	NONE																		*/
/*	RETURNS:		int	- Total number of system sectors							*/
/*																								*/
/************************************************************************/

int GetSysSec( void )
{
	register		iFatSec;						/* Total disk FAT sectors		*/
	register		iRootSec;					/* Total sectors in root dir	*/

	iFatSec = Bpb->uchNumberFats * Bpb->uSecPerFat;
	iRootSec = Bpb->uRootEntries * sizeof( struct DIR );
	iRootSec /=	Bpb->uBytesPerSec;

	return( Bpb->uReservSec + iRootSec + iFatSec );
}

/************************************************************************/
/* Sets up the disk parmeters at vector 1eh for the type of disk being	*/
/* formatted. First get the drive type and attempts to get a DSAD from	*/
/* the ROM BIOS. If this fails we have to use our own DASD and set the	*/
/* Format Gap Length and the Max Sector fields to match the specified	*/
/* format and then do an int 13h funct 17h to attempt to set if for old	*/
/* IBM AT ROMs (there is no error checking on this call because old PCs	*/
/* and XTs don't support it and it won't make any difference if it		*/
/* fails. Which ever DASD is used is then copied to a local buffer and	*/
/* vector 1eh is pointed to the local buffer.									*/
/*																								*/
/*	int SetDskParms( void )																*/
/*																								*/
/*	ARGUMENTS:	NONE																		*/
/* RETURNS:		int	- OK if a valid format for the type of drive is 	*/
/*							  specified else INVALID_DRIVE_TYPE						*/
/*																								*/
/* GLOBALS:		DskParms - Initializes the structure to current format	*/
/*																								*/
/************************************************************************/

int SetDskParms( void )
{
	register						i;						/* Loop counter				*/
	register						iStatus;				/* Return error status		*/
	int							iDrvType;			/* BIOS drive type			*/
	struct DSK_PARMS far		*TmpParms;			/* Ptr to selected DASD		*/
	char 							*IoctlBuf;			/* Tmp work buffer 			*/

	iStatus  = OK;										/* Will change if errors	*/
	IoctlBuf = GetMemory( 100 );					/* Allocate work buffer		*/
	iDrvType = GetDriveType( (UCHAR)uDrv, IoctlBuf ); /* Determine drive type */

									/* If ROM BIOS supports int 13h funct 18h */
									/* we use the ROM supplied disk parmeters */

	if ( (TmpParms = SetMediaType( uDrv, (UINT)auchTotalTracks[iFmt],
							Bpb->uSecPerTrack )) == NULL )
	{
									/* Else set format type with funct 17h	*/
									/* and setup our own dsk params table	*/

		if ( uchDiskType[iDrvType][iFmt] != ERR )
		{
			SetDiskType( uDrv, uchDiskType[iDrvType][iFmt] );

			LocalDskParms.FmtGap = DskFmtGap[iDrvType];	/* Format gap length	*/
			LocalDskParms.EOT = (UCHAR)Bpb->uSecPerTrack;/* Sectors per track */
			TmpParms = (struct DSK_PARMS far *)(&LocalDskParms);
		}
		else
			iStatus = IVALID_DRIVE_TYPE; 	/* Not valid format for drive type	*/

	}

	if ( iStatus == OK )
	{
						/* Copy the dsk params to the local buffer and then	*/
						/* set int vector 1eh to point to local buffer. Use	*/
						/* a copy loop because this may be small memory model	*/

		for ( i = 0; i < sizeof( struct DSK_PARMS ); i++ )
			*((char far *)(&DskParms) + i)  =
			*((char far *)(TmpParms) + i ); 

		_dos_setvect( DSK_PARAMS_VECT, (void far *)(&DskParms) );
	}
	
	FreeMemory( IoctlBuf );
	return( iStatus );
}

/************************************************************************/
/*	Calls RdWrSectors() to write the specified number of sectors to a		*/
/* disk.																						*/
/*																								*/
/*	ARGUMENTS:	uStartSec	-	Sector on disk where write is to start		*/
/*					uNumSecs		-	Number of sectors to write						*/
/*					pchBuf		-	Ptr to buffer to be written					*/
/*	RETURNS:		int			-	OK if success or error code if error		*/
/*																								*/
/*	GLOBALS:		uDrv			-  Uses this value for the physical drive #	*/
/*					Bpb			-	Must contain the BPB for drive uDrv			*/
/*																								*/
/************************************************************************/

int WriteSectors( unsigned uStartSec, unsigned uNumSecs, char *pchBuf )
{

	return( RdWrSectors( uDrv, uStartSec, uNumSecs, pchBuf, Bpb, WRITE ) );

}
