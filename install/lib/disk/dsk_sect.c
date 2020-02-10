/***************************************************************************/
/*																									*/
/*	DSK_SECT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Reads or writes the specified number of sectors to a floppy disk			*/
/*	using interrupt 13h calls. 															*/
/*																									*/
/*	int RdWrSectors( int iDrv, unsigned uSec, unsigned uNumSec,					*/
/*						  char *puchBuf, struct BPB *Bpb, int RdWr )					*/
/*																									*/
/*	ARGUMENTS:	unsigned iDrv		Physical drive to write to						*/
/* 				unsigned uSec		Absolute starting sector to begin at		*/
/*					unsigned uNumSec	Number of sectors to write						*/
/*					int		iFmt		Disk type listed in first header				*/
/*					char 		*puchBuf	Ptr to buffer holding data to write			*/
/*					struct BPB *Bpb	BPB struct for disk in drive					*/
/*					int		RdWr		Specifies read (0) or write (1)				*/
/*																									*/
/*	RETURNS:		int					Error code or OK if successfull				*/
/* 																								*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include		<stdio.h>
#include 	<bios.h>
#include 	<malloc.h>
#include 	<string.h>
#include		<dos.h>

#include		<alias.h>
#include 	<disk_io.h>
#include 	<window.h>

#define		DSK_PARAMS_VECT		0x1e

int RdWrSectors( int iDrv, unsigned uSec, unsigned uNumSec, char *puchBuf,
					  struct BPB *Bpb, int RdWr )
{
	register					i;								/* Loop counter				*/
	register					iStatus;						/* Return status				*/
	struct diskinfo_t		DiskInfo;					/* _bios_disk struct			*/
	struct DSK_PARMS 		NewDskParms;
	struct DSK_PARMS far	*OrigDskParms;

						/* Set disk parameters in dasd to allow proper access		*/
	OrigDskParms = (void far *)_dos_getvect( DSK_PARAMS_VECT );

	/* memcpy( &NewDskParms, OrigDskParms, sizeof( struct DSK_PARMS ) ); */
	for ( i = 0; i < sizeof( struct DSK_PARMS ); i++ )
		*(((char *)(&NewDskParms)) + i) = *(((char far *)(OrigDskParms)) + i);

	NewDskParms.EOT = (UCHAR)Bpb->uSecPerTrack;
	_dos_setvect( DSK_PARAMS_VECT, (void far *)(&NewDskParms) );

	iStatus = OK;											/* This may change			*/
	DiskInfo.drive = iDrv;

													/* Read or write a track at a			*/
													/* time until all sectors are done	*/

	while( uNumSec > 0 && iStatus == OK )
	{
		DiskInfo.head = GetDiskHead( uSec, Bpb );
		DiskInfo.track = GetDiskTrack( uSec, Bpb );
		DiskInfo.sector = (uSec % Bpb->uSecPerTrack) + 1;
		DiskInfo.nsectors = Bpb->uSecPerTrack - (DiskInfo.sector - 1);

		if ( DiskInfo.nsectors > uNumSec )
			DiskInfo.nsectors = uNumSec;

		DiskInfo.buffer = (void far *)(puchBuf);	/* Point to start of buf	*/

													/* Loop until successfull or until	*/
													/* all retries are exhausted			*/

		for ( i = 0; i < MAX_RETRIES; i++ )
		{
			iStatus = _bios_disk( RdWr == READ ? 2 : 3, &DiskInfo );

			if ( (iStatus >>= 8) == OK )		/* Get status from high byte */
				break;
			else
			{
				switch( iStatus )
				{
					case	OK:
						continue;			/* Quick out if no errors */
					case	06:			 	/* Disk change */
						i = 0;
						break;
					case	DISK_TIME_OUT:
						NotReadyPrompt( (UCHAR)(iDrv + 'A') );
						i = 0;
						break;
					case 03:
						WriteProtectPrompt( (UCHAR)(iDrv + 'A') );
						break;
					default:
						break;
				}
							/* Must have been an error if it got here so do a reset */
			}

			_bios_disk( _DISK_RESET, &DiskInfo );
		}

		uNumSec -= DiskInfo.nsectors;				/* Sub sectors on last track	*/
		uSec += DiskInfo.nsectors;					/* Calc next sector to do		*/

															/* Point to next sector 		*/
		puchBuf += (UINT)DiskInfo.nsectors * Bpb->uBytesPerSec;
	}

	_dos_setvect( DSK_PARAMS_VECT, (void far *)OrigDskParms );
	return( iStatus );
}
