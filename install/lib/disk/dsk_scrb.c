/***************************************************************************/
/*																									*/
/*	DSK_SCRB.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Empties the FAT and root directory to make the disk look freshly			*/
/* formatted by zeroing all good clusters in all copies of the FAT and		*/
/* then zeroing out all entries in all root directory sectors.					*/
/*																									*/
/*	int ScrubFatRoot( int iDrv, struct BPB *Bpb )									*/
/*																									*/
/*	ARGUMENTS:	iDrv	-	Physical floppy drive number								*/
/*					Bpb	-	Ptr to Bpb struct for the disk being scrubbed		*/
/*	RETURNS:		int	-	OK if successfull else !OK									*/
/* 																								*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include		<stdio.h>
#include 	<bios.h>
#include 	<string.h>
#include 	<malloc.h>
#include		<dos.h>

#include		<alias.h>
#include 	<disk_io.h>


void *GetMemory( unsigned Bytes );
void free( void * );

int ScrubFatRoot( int iDrv, struct BPB *Bpb )
{
	unsigned char			*pchFat;					/* Buffer to read FAT into	*/
	register					Index;					/* Count of FATS written	*/
	register					iStatus;					/* Loop indice					*/
	unsigned					uSec;						/* Abs starting sector		*/

	pchFat = GetMemory( (Bpb->uSecPerFat * 512) + 100 );
														/* Read the first FAT		*/
	if ( (iStatus = RdWrSectors( iDrv, 1, Bpb->uSecPerFat, pchFat, Bpb,
										 READ )) == OK )
	{
														/* Zero all good clusters	*/
		ScrubFat( pchFat, Bpb->uTotalSectors / Bpb->uchSecPerClus );

														/* Write both FAT copies	*/
		for( uSec = 1, Index = 0; Index < 2 && iStatus == OK; Index++ )
		{
			iStatus = RdWrSectors( iDrv, uSec, Bpb->uSecPerFat, pchFat, Bpb,
											WRITE );
			uSec += Bpb->uSecPerFat;
		}
	}

	FreeMemory( pchFat );						/* Free the work buffer		*/

	if ( iStatus == OK )							/* Scrub the root dir 		*/
		iStatus = CreatRootDir( iDrv, Bpb );

	_dos_dskreset();								/* Force disk resets */

	return( iStatus == OK ? OK : BAD_MEDIA );
}

/***************************************************************************/
/*	Scrubs a 12 bit FAT by zeroing out all good clusters. Will not change	*/
/* the clusters marked bad.																*/
/*																									*/
/*	int ScrubFat( unsigned char *pchFat, unsigned Clusters )						*/
/*																									*/
/*	ARGUMENTS:	pchFat	-	Ptr to buffer holding the FAT							*/
/*					Clusters	-	Total number of clusters in the FAT					*/
/*	RETURNS:		void																			*/
/* 																								*/
/***************************************************************************/

void ScrubFat( unsigned char *pchFat, unsigned Clusters )
{
	register		i;								/* Loop counter				 */


	pchFat += 3;								/* Jmp over the 2 signature entries	*/

	for ( i = 2; i < Clusters; i += 2 )
	{
															/* Do even cluster			*/
		if ( *(unsigned *)(pchFat) != 0xff7 )
			*(unsigned *)(pchFat) &= 0xf000;
	
		pchFat++;										/* Incr to odd cluster		*/

		if ( *(unsigned *)(pchFat) != (0xff7 << 4) )
			*(unsigned *)(pchFat) &= 0x000f;

	 	pchFat += 2;									/* Incr to next even clust	*/
	}
}
