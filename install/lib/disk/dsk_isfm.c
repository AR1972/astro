/***************************************************************************/
/*																									*/
/*	DSK_ISFM.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Determines if the disk in the specified drive has been formatted by		*/
/* reading in the boot record and checking the BPB with all normal floppy	*/
/* disk BPB layouts. If there is an error reading the boot record or if		*/
/* the BPB is not valid returns FALSE else TRUE. If the disk is formmatted	*/
/* the Bpb struct specified by the argument Bpb will be filled in. Before	*/
/* this function is called to IsDiskReady should be done to be sure that	*/
/* there is a disk in the drive.															*/
/*																									*/
/*	int IsFormatted( int iDrv )															*/
/*																									*/
/* ARGUMENTS:	iDrv	- Physical drive number											*/
/*					Bpb	- Ptr to BPB struct to be filled in							*/
/*					pchBuffer - Ptr to work buffer at least SECTOR_SIZE 			*/
/* RETURNS:		int	- TRUE if disk is formatted else false						*/
/*																									*/
/* GLOBALS:		DskBpb - Array of valid BPB structures in NEWFMT.C				*/
/*																									*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include		<stdio.h>
#include 	<string.h>

#include 	<alias.h>
#include 	<disk_io.h>

int IsFormatted( int iDrv, struct BPB *Bpb, char *pchBuffer	)
{
	register				iStatus;			/* Return status 							*/
	register				i;					/* Loop counter							*/
	extern struct BPB DskBpb[];		/* Array of valid BPB structures		*/

	iStatus = FALSE;						/* Assume not formatted 				*/
												/* Try to read the boot record		*/
	if ( GetBootSector( iDrv, pchBuffer ) == OK )
	{
		memcpy( Bpb, pchBuffer + 11, sizeof(struct BPB) );	/* Get BPB */

										/* Loop for all valid disk types to see	*/
										/* if the BPB is valid							*/

		for ( i = 3; i < 7; i++ )
		{
			if ( memcmp( &(DskBpb[i]), Bpb, sizeof(struct BPB) - 6 ) == OK )
			{
				iStatus = TRUE;			/* Disk's BPB was valid				*/
				break;
			}
		}
	}
	return( iStatus );
}
