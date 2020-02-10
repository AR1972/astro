/***************************************************************************/
/*																									*/
/*	DSK_SETL.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/*																									*/
/* Creates a volume label on the specified drive.									*/
/*																									*/
/*	UL SetDiskLabel( int iDrv, char *szLabel, struct *Bpb ) 						*/
/*						 																			*/
/*	ARGUMENTS:	iDrv		- Drive to read 												*/
/*					Bpb		- Ptr to Bpb struct for this disk						*/
/*					szLabel	- String to use for disk label							*/
/*	RETURNS:		UL			- Label creation time or -1L if disk write error	*/
/*																									*/
/*																									*/
/*	johnhe - 02-14-90																			*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include		<bios.h>
#include		<dos.h>
#include		<string.h>

#include		<alias.h>
#include		<strlib.h>
#include		<disk_io.h>


#define		SECTOR_SIZE			512

/***************************************************************************/

UL SetDiskLabel( int iDrv, char *szLabel, struct BPB *Bpb )
{
	char					*pchBuf;
	unsigned				uSec;
	UL						ulTime;
	struct DIR			*Dir;						/* Disk label DIR entry		*/

	ulTime =  (UL)(-1L);
	pchBuf = GetMemory( SECTOR_SIZE );		/* Allocate a sector and	*/
	memset( pchBuf, 0, SECTOR_SIZE );		/* then fill it with 0's	*/

	Dir = (struct DIR *)(pchBuf);
	Dir += 2;
	strcpy( Dir->Name, szLabel );		/* Set volume label name 			*/
	PadStr( Dir->Name, ' ', 11 );		/* Add ' ' chars to end of name	*/
	Dir->Attrib = 8;						/* Set attrib to volumne label	*/
	SetFileTimeDate( Dir );				/* Set the dir entry time & date */

 	*(pchBuf) = (char)0xe5;
	*(pchBuf + sizeof( struct DIR )) = (char)0xe5;

														/* Find sector offset of root	*/
	uSec = Bpb->uSecPerFat * Bpb->uchNumberFats;
	uSec += Bpb->uReservSec;

	if ( RdWrSectors( iDrv, uSec, 1, pchBuf, Bpb, WRITE ) == OK &&
		  WriteBoot( iDrv, Bpb, szLabel ) == OK )
	{
		_dos_dskreset();
												/* Convert time to long timedate */
		ulTime = (unsigned long) (*((unsigned*)(&Dir->Date)));
		ulTime <<= 16;
		ulTime += (unsigned long)(*((unsigned *)(&Dir->Time)));
	}

	FreeMemory( pchBuf );
	return( ulTime );
}

