/***************************************************************************/
/*																									*/
/*	DSK_HEAD.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Calculates the drive head which will access an absolute sector on			*/
/* a disk.																						*/
/* 																								*/
/* STRATAGEY: Head =																			*/
/* ((AbsSector % SectorPerTrack) * NumberOfHeads) / SectorsPerTrack			*/
/*																									*/
/*	int GetDiskHead( unsigned uSec, struct BPB *Bpb )								*/
/*																									*/
/* ARGUMENTS:	uSec		- Absolute disk sector										*/
/*					Bpb		- Ptr to BPB structure for disk being accessed.		*/
/* RETURNS:		int		- The drive head which corresponds to uSec			*/
/*																									*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include		<disk_io.h>

int GetDiskHead( unsigned uSec, struct BPB *Bpb )
{
	return( (int)((uSec % (Bpb->uSecPerTrack * Bpb->uNumberHeads)) /
			  Bpb->uSecPerTrack) );
}
