/***************************************************************************/
/*																									*/
/*	DSK_TRK.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Caculates the track that an absolute sector is on.								*/
/* STRATAGEY:	Track =	AbsSector / ( NumberOfHeads * SectorsPerTrack )		*/
/*																									*/
/*	int GetDiskTrack( unsigned uSec, struct BPB *Bpb )								*/
/* 																								*/
/* ARGUMENTS:	uSec		- Absolute disk sector										*/
/*					Bpb		- Ptr to BPB structure for disk being accessed.		*/
/* RETURNS:		int		- The drive track which corresponds to uSec			*/
/* 																								*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include		<disk_io.h>


int GetDiskTrack( unsigned uSec, struct BPB *Bpb )
{

	return( (int)(uSec / ((unsigned)Bpb->uNumberHeads * Bpb->uSecPerTrack)) );

}
