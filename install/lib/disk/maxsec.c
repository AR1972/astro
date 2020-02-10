/***************************************************************************/
/*																									*/
/*	MAXSEC.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/*	Returns the max number of bytes for any hard disk partition in a system	*/
/*	by starting at drive C: and then checking every drive until a remote or	*/
/* remove-able disk is detected.															*/
/*																									*/
/*	unsigned GetMaxSectorSize( void )													*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		unsigned	- Max bytes per sector of all hard disk partitions	*/
/*								  0 if insuffient memory for int 25h read buffer.	*/
/*																									*/
/* Created 12-11-90 johnhe																	*/
/***************************************************************************/


#include		<stdio.h>
#include		<malloc.h>
#include		<disk_io.h>

#define		SECTOR_SIZE		512

unsigned GetMaxSectorSize( void )
{
	unsigned						MaxSize;
	unsigned						TmpSize;
	unsigned char				Drv;
	struct ABSIO_PACKET		absPack;

											/* Allocate a buffer to for int 25h reads	*/
	if ( (absPack.pchBuffer = halloc( 0xffffL, 1 )) == NULL )
		return( 0 );					/* Not enough memory so return 0 size		*/

	absPack.lStartSector = 0L;					/* Setup packet for read of 1st	*/
	absPack.uNumSectors = 1;					/* disk sector.						*/
	MaxSize = SECTOR_SIZE;						/* Set default size to 512 bytes	*/

	for ( Drv = 3;
			(Drv < 26) && (IsValidDrive( Drv + 0x40 ) && IsLocalDrive( Drv ));
			Drv++ )
	{
			if ( !IsRemoveable( Drv ) )
				if ( AbsReadWrite( (Drv - 1), &absPack, READ ) == 0 )
					if ( (TmpSize = GetSectorSize( Drv )) > MaxSize )
						MaxSize = TmpSize;
	}

	hfree( absPack.pchBuffer );
	return( MaxSize );
}

