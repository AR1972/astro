/***************************************************************************/
/*																									*/
/*	RW_ROOT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Reads or writes a specified root directory sector on the specified		*/
/*	drive.																						*/
/*																									*/
/*	ReadWriteRoot( int iDosDrv, struct BPB *Bpb, void *Buf, int Sector		*/
/*					  			int ReadWrite )												*/
/*																									*/
/*	ARGUMENTS:	iDosDrv	- The DOS drive number to get the sector from		*/
/*					Bpb		- Ptr to bpb structure for the specified drive		*/
/*					Buf		- Ptr to sector buffer										*/
/*					Sector	- The root directory sector number based 0			*/
/*					ReadWrite- Flags reading or writing sector - READ or WRITE 	*/
/*	RETURNS:		int		- OK if successfull else int 25h error code			*/
/*																									*/
/*	johnhe - 02-14-90																			*/
/***************************************************************************/

#include		<alias.h>
#include		<disk_io.h>

int ReadWriteRoot( int iDosDrv, struct BPB *Bpb, void *Buf, int Sector,
						 int ReadWrite )
{
	struct ABSIO_PACKET	absPack;

	absPack.uNumSectors = 1;
	absPack.pchBuffer = (char far *)Buf;
	absPack.lStartSector = (long)(Bpb->uReservSec +
											((UINT)Bpb->uchNumberFats * Bpb->uSecPerFat) +
											(UINT)Sector );

  return( AbsReadWrite( iDosDrv, &absPack, ReadWrite ) );
}


