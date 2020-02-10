/***************************************************************************/
/*																									*/
/*	DSK_ROOT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Creates an empty root directory on the specified drive using the			*/
/* specified BPB for the size and location.											*/
/*																									*/
/*	int CreatRoot( int iDrv, struct BPB *Bpb )										*/
/*																									*/
/*	ARGUMENTS:	iDrv	-	Physical drive number										*/
/*					Bpb	-	Bpb structure for disk in the drive						*/
/*	RETURNS:		int	-	OK if successful												*/
/* 																								*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include		<stdio.h>
#include 	<malloc.h>
#include 	<string.h>

#include		<alias.h>
#include 	<disk_io.h>

int CreatRootDir( int iDrv, struct BPB *Bpb )
{
	char			*pchBuf;					/* Buffer to hold 1 sector				*/
	int			iStatus;					/* Return value							*/
	unsigned		uThisSec;				/* Current dir sector being written	*/
	unsigned		uLastSec;				/* Last sector number in directory	*/

	pchBuf = GetMemory( Bpb->uBytesPerSec );	/* Allocate a sector			*/
	memset( pchBuf, 0, Bpb->uBytesPerSec );	/* Fill it with zeros		*/

												/* Find root dir starting sector		*/
	uThisSec = Bpb->uSecPerFat * Bpb->uchNumberFats;
	uThisSec += Bpb->uReservSec;

												/* Find last sector in the root 		*/
	uLastSec = (Bpb->uRootEntries * sizeof( struct DIR )) /
				  Bpb->uBytesPerSec;
	uLastSec += uThisSec;

							 					/* Once for each sector in root dir	*/
	for ( iStatus = OK;
			iStatus == OK && uThisSec < uLastSec;
			uThisSec++ )
		iStatus =  RdWrSectors( iDrv, uThisSec, 1, pchBuf,	 Bpb, WRITE );

	FreeMemory( pchBuf );
	return( iStatus );
}
