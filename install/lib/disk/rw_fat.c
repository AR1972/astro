/***************************************************************************/
/*																									*/
/*	RW_FAT.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Reads or writes a specified FAT sector on the specified drive.	Will		*/
/* update the same sector in all copies of the FAT.								*/
/*																									*/
/*	ReadWriteFat( int iDosDrv, struct BPB *Bpb, void *Buf, int Sector,		*/
/*					  int ReadWrite )															*/
/*																									*/
/*	ARGUMENTS:	iDosDrv	- The DOS drive number to get the sector from		*/
/*					Bpb		- Ptr to bpb structure for the specified drive		*/
/*					Buf		- Ptr to sector buffer										*/
/*					Sector	- The FAT sector number based 0							*/
/*					ReadWrite- Flags reading or writing sector - READ or WRITE 	*/
/*	RETURNS:		int		- OK if successfull else int 25h error code			*/
/*																									*/
/*	johnhe - 02-14-90																			*/
/***************************************************************************/

#include		<alias.h>
#include		<disk_io.h>

int ReadWriteFat( int iDosDrv, struct BPB *Bpb, void *Buf, int Sector,
						int ReadWrite )
{
	register					i;							/* Loop indice 					*/
	register					iStatus;					/* Disk write status 			*/
	struct ABSIO_PACKET	absPack;					/* DOS 4.0 int 25h,26h packet	*/

	absPack.uNumSectors = 1;
	absPack.pchBuffer = (char far *)Buf;
	absPack.lStartSector = (long)(Bpb->uReservSec + (UINT)Sector );

	for ( iStatus = OK, i = 0;
			iStatus == OK && i < Bpb->uchNumberFats;
			absPack.lStartSector += (long)Bpb->uSecPerFat, i++ )
	{
		iStatus = AbsReadWrite( iDosDrv, &absPack, ReadWrite );
		if ( ReadWrite == READ )
			break; 											/* Only need to read once */
	}
	return( iStatus );
}


