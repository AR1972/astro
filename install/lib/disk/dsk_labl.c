/***************************************************************************/
/*																									*/
/*	DSK_LABL.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/*	Reads a disk label directly from the root directory of a floppy disk and*/
/* returns the labels creation time and date in the form of a long and 		*/
/* copies the label to a caller supplied buffer.									*/
/*																									*/
/*	 UL GetDiskLabel( int iDrive, char *szLabel )									*/
/*																									*/
/*	ARGUMENTS:	iDrive	- Physical drive number where disk is located.		*/
/*					szLabe	- Ptr to a buffer to copy the label to.				*/
/*	RETURNS:		UL			- Label creation time or -1L if no label was found	*/
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

UL GetDiskLabel( int iDrive, char *szLabel, struct BPB *Bpb )
{
	#define		FOUND_IT		1000					/* Signals found a label		*/
	#define		END_DIR		2000					/* Signals end of root dir		*/

	char			*Buf;									/* Sector buffer					*/
	unsigned		uLastSec;							/* Last sector of root dir		*/
	register		iStatus;								/* Current search status		*/
	register		i;										/* Loop indice	for entry scan	*/
	unsigned		uSec;									/* Current disk sector			*/
	struct DIR	*Dir;									/* Ptr to current dir entry	*/
	UL 			ulTime;								/* Label creation time/date	*/

	ulTime = 0L;										/* Assume no label found		*/
	*szLabel = EOL;									/* Create default label str	*/

	Buf = GetMemory( SECTOR_SIZE );

	uSec = Bpb->uSecPerFat * Bpb->uchNumberFats;/* Determine first and last	*/
	uSec += Bpb->uReservSec;							/* sector of the root dir		*/
	uLastSec = uSec + (Bpb->uRootEntries * sizeof( struct DIR )) /
							 Bpb->uBytesPerSec;

								 					/* Once for each sector in root dir	*/
	for ( iStatus = OK;
			uSec < uLastSec && iStatus == OK;
			uSec++ )
	{
		Dir = (struct DIR *)(Buf);
		if ( (iStatus = RdWrSectors( iDrive, uSec, 1, Buf, Bpb, READ )) == OK )
		{
										/* Scan through current dir sector for label	*/
			for ( i = 0;
					i < (SECTOR_SIZE / sizeof( struct DIR )) && iStatus == OK;
					i++, Dir++ )
			{
				if ( Dir->Name[0] == 0 )			/* Check for last entry	mark	*/
				{
					iStatus = END_DIR;				/* No more entries				*/
					break;
				}
				else if ( (Dir->Attrib & _A_VOLID) != 0 )	/* Chk for VOL id		*/
				{
					Dir->Name[11] = EOL;					/* Cnvrt VOL id to string	*/
					strcpy( szLabel, Dir->Name );		/* Copy to caller's buf		*/
					RemoveTrailing( szLabel, ' ' );	/* Clean up the string		*/

																/* Cnvrt time/date to long	*/
					ulTime = (UL) (*((unsigned*)(&Dir->Date)));
					ulTime <<= 16;
					ulTime += (UL)(*((unsigned *)(&Dir->Time)));

					iStatus = FOUND_IT;					/* Signal found it			*/
				}
			}
		}
		else
		{
			ulTime = (UL)(-1L);				/* Disk error while reading label	*/
			break;
		}
	}

	FreeMemory( Buf );
	return( ulTime );
}


