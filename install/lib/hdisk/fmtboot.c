/***************************************************************************/
/*																									*/
/*	FMTBOOT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Determines if a boot record if from a partition that was formatted		*/
/* under any version of DOS. Checks for proper opcode in the first 3 bytes */
/* (e9,XX,XX or eb,XX,90).																	*/
/* 																								*/
/* int IsFmtedBoot( char *SectorBuf )													*/
/* 																								*/
/* ARGUMENTS:	SectorBuf	- Ptr to buf holding the boot sector to check	*/
/* RETURNS: 	int			- TRUE if a formatted boot record else FALSE 	*/
/*																									*/
/* Created 02-07-90 johnhe																	*/
/***************************************************************************/

#include		<alias.h>
#include 	<hdisk.h>
#include 	<disk_io.h>

#define	SECTOR_SIZE		512

unsigned IsFmtedBoot( char *SectorBuf )
{
	struct BPB	*Bpb;

	Bpb = (struct BPB *)(SectorBuf + 11);
	
	if ( Bpb->uBytesPerSec < SECTOR_SIZE ||
		  (Bpb->uBytesPerSec % SECTOR_SIZE) ||
		  (Bpb->uchMediaDescr != (UCHAR)0xf8 &&
		   Bpb->uchMediaDescr != (UCHAR)0xfa) ) 
		return( FALSE );
	return( TRUE );
}

