/***************************************************************************/
/*																									*/
/*	RW_BOOT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Uses int 25h&26h to read or write the boot record for the specified		*/
/* drive.																						*/
/*																									*/
/*	int ReadWriteBoot( int iDosDrv, void *Buffer, int ReadWrite )				*/
/*																									*/
/*	ARGUMENTS:	iDosDrv	- DOS drive number to get boot record from			*/
/*					Buffer	- Ptr to buffer to hold the boot record				*/
/*	RETURNS:		int		- OK if successful else a DOS error code for			*/
/*																									*/
/*	johnhe - 02-14-90																			*/
/***************************************************************************/

#include		<alias.h>
#include		<disk_io.h>

int ReadWriteBoot( int iDosDrv, void *Buffer, int ReadWrite )
{
	struct ABSIO_PACKET	absPack;

	absPack.lStartSector = 0L;
	absPack.uNumSectors = 1;
	absPack.pchBuffer = (char far *)Buffer;

	return( AbsReadWrite( iDosDrv, &absPack, ReadWrite ) );
}	



