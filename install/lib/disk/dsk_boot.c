/***************************************************************************/
/*																									*/
/*	DSK_BOOT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/*	Misc. floppy disk access function via the ROM BIOS int 13h interface.	*/
/* Original created for the RUP format and backup programs.						*/
/*																									*/
/* 																								*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include		<stdio.h>
#include 	<string.h>
#include		<memory.h>

#include		<alias.h>
#include 	<disk_io.h>

/************************************************************************/
/* Copies the BPB structure for the current disk to a boot record and	*/
/* then writes the boot record to the disk.										*/
/*																								*/
/* int WriteBoot( int iDrv, struct BPB *Bpb, char *szLabel )				*/
/*																								*/
/*	ARGUMENTS:	Bpb		- BPB to use for this disk								*/
/*					szLabel	- Disk label string or NULL for no label			*/
/*	RETURNS:		int		- OK or BIOS int 13h disk error						*/
/*																								*/
/************************************************************************/

int WriteBoot( int iDrv, struct BPB *Bpb, char *szLabel )
{
	extern char				NewBootRec;		/* 512 byte boot sector	*/
	struct BOOT_HEADER	*Header;
	struct DIR				Dir;
	int						i;

	Header = (struct BOOT_HEADER *)(&NewBootRec);

	memcpy( &(Header->Bpb), Bpb, sizeof( struct BPB ) );

	Header->uchPhysDrv = 0;
	Header->uchCurHd = 0;

		/* If boot header doesn't have extended BPB signature we need	*/
		/* to zero any extended fields not explicitly set by us.			*/

	if ( Header->uchExtSig != 0x29 )
	{												/* Zero hi-word of hidden sectors	*/
		Header->Bpb.ulHiddenSec &= 0x0000ffffL;
		Header->Bpb.ulTotalBigSecs = 0L;
		Header->uchExtSig = 0x29;
	}

	SetFileTimeDate( &Dir );
	Header->ulSerial = *( (long *)(&(Dir.Time)) );
	
	if ( szLabel != NULL )
	{
		for ( i = 0; i < 11; i++ )
		{
			if ( *szLabel != 0 )
				Header->uchVolLabel[i] = *(szLabel++);
			else
				Header->uchVolLabel[i] = ' ';	
		}
	}
	else
		memset( Header->uchVolLabel, ' ', 11 );


	memcpy( Header->uchSystemId, "FAT12   ", 8 );

	return( RdWrSectors( iDrv, 0, 1, &NewBootRec, Bpb, WRITE ) );
}
