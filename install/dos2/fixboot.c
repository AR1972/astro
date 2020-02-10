/***************************************************************************/
/* 																								*/
/* FIXBOOT.C																					*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/*	Module which does the fixup on all of the boot records and partition		*/
/* tables on the harddisks.																*/
/* 																								*/
/* CHANGE LOG:																					*/
/*		m102	- Fixed boundary condition bug when converting partitions to	*/
/*				  to BIGFOOT and now only convert small primary partitions to	*/
/*				  BIGFOOT if part of it lies physically past 32 meg boundary	*/
/*				  																					*/
/* johnhe - 09/15/90																			*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include 	<bios.h>
#include		<dos.h>
#include		<string.h>

#include 	<alias.h>
#include 	<disk_io.h>
#include 	<global.h>
#include		<hdisk.h>
#include		<upgrade.h>
#include		<install.h>

/***************************************************************************/

#define	PART_LEN			sizeof( struct Part )
#define	HEADER_LEN		sizeof( struct BOOT_HEADER )

#define	HEADER_OFFSET	0
#define	TABLE_LEN		(SECTOR_SIZE - TABLE_OFFSET)
/* #define 	BOOT_CODE_LEN	0xe0 */

/***************************************************************************/

static int				iHdNum;
static struct Part	*NewTable;

/***************************************************************************/

extern int	ConvertBoot			( struct Part *Part, int IsPrimary ); /* m102*/
static void	near FixupPartitions( struct Part *PartLocation, int IsPrimary ); /*m102*/
static void	near PackPartitionTable( int Entries );
static struct Part * near GetNextFreePartEntry( void );

/***************************************************************************/
/* Function to do fixup of all boot records and partition tables in the		*/
/* system for up to 2 hard disks. Copies the master partition table for		*/
/* each drive into the pchMbrBuf, creates a dummy partition entry with the	*/
/* sectors phyical location and then calls FixupPartitions() to do the		*/
/* actual processing. Fixup requires converting DOS compatible parititions	*/
/* to types 1, 4 or 6 and then converting logical sectors into 512 byte		*/
/* sectors in the partition boot record.												*/
/*																									*/
/* NOTE:																							*/
/*			Rather than reading the partition record from the first hard disk	*/
/*			we have to get it from the pchOldPartRec buffer because we put		*/
/*			a tmp one on the hard disk which may be missing some entries.		*/
/*																									*/
/*	void FixupBootRecs( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void FixupBootRecs( void )
{
	register					i;
	register					Entries;
	static struct Part	MasterPart = { 0, 0, 1, 0,  0, 0, 0, 0,  0, 0 };
	extern char				TmpBoot;

	NewTable = GetPartTable( 4 );			/* Start of DOS 5.x partition table	*/

	memcpy( pchMbrBuf, pchOldPartRec, SECTOR_SIZE );

	/* Write temp. boot code if we have created Uninstall disk */

	if ( lpInstall->Flags.fUninstall )
	{
#ifdef	IADF_SUPPORT						/* M008 Added for IADF support		*/
		if ( !vInfo.Flag.fHasIadf )
		{				/* If real MBR is on first sector we need tmp boot code	*/
						/* and the TMP serial number restored							*/
			memcpy( pchMbrBuf, &TmpBoot, BOOT_CODE_LEN );
			*( (UL *)(pchMbrBuf+(BOOT_CODE_LEN - 4)) ) = vInfo.ulTmpSerial;
		}
#else
		memcpy( pchMbrBuf, &TmpBoot, BOOT_CODE_LEN );
		*( (UL *)(pchMbrBuf+(BOOT_CODE_LEN - 4)) ) = vInfo.ulTmpSerial;
#endif
	}

	*(pchMbrBuf + TABLE_OFFSET + (vInfo.chBootPart * 0x10)) = (CHAR)ACTIVE;

	for ( i = 0, iHdNum = 0x80; i < (int)vInfo.Hw.NumHard; i++, iHdNum++ )
	{
		if ( iHdNum == 0x80 || 
			  ReadPartBootRec( &MasterPart, pchMbrBuf, iHdNum ) == OK )
		{
			if ( !IsValidPartTable() )
				continue;

			Entries = GetPartEntries();		/* Determine size of part table	*/
			if ( Entries > 4 )
				PackPartitionTable( Entries );
														/* Write the master boot record	*/
			FixupPartitions( &MasterPart, TRUE );	/*m102 Added argument	*/
		}	
		else
			FatalError( FATAL_HD_READ_ERROR );
	}
}

/***************************************************************************/
/* Scans a partition table in the pchMbrBuf looking for compatible DOS 		*/
/* partitions. The BPB in the boot record on each DOS compatible partition	*/
/* is converted to DOS 5.x compatible format and the correct partition		*/
/* type inicator is placed in the partition table entry for than partition	*/
/* and then the boot record is written back. After all of the compatible	*/
/* entries are fixed up the updated partition table in pchMbrBuf is			*/
/* written back. Next a check is done for an extended partition entry and	*/
/* if one is found the entry for it is copied to a local buffer, the 		*/
/* sector for the extended paritition table is read in and the function		*/
/* recursively calls itself to process the extended partition.					*/
/*																									*/
/* NOTE: 																						*/
/* 		Because the function is recursive it may need at least 500 bytes	*/
/* 		of stack to allow for a possible 22 extended partitions.				*/
/*																									*/
/*	void FixupPartitions( struct Part *Part, int IsPrimary )						*/
/*																									*/
/*	ARGUMENTS:	struct Part * - Partition entry which specifies where the	*/
/*										 partition table in pchMbrBuf needs to be		*/
/*										 written back to the disk.							*/
/*					IsPrimary	- Flags if converting a primary partition			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void near FixupPartitions( struct Part *PartLocation, int IsPrimary ) /* m102 */
{
	register			i;
	struct Part		ExtPartition;
	struct Part		*PartEntry;

	PartEntry = NewTable;								/* Addr first table entry	*/
																/* in pchMbrBuf				*/
	for ( i = 0; i < 4; i++, PartEntry++ )
	{
		if ( IsCompatPart( PartEntry->SystemIndicator ) )
		{
			if ( ReadPartBootRec( PartEntry, pchBootBuf, iHdNum ) == OK )
			{
				if ( ConvertBoot( PartEntry, IsPrimary ) == OK )		/* m102	*/
					if ( WritePartBootRec( PartEntry, pchBootBuf, iHdNum ) != OK )
						FatalError( FATAL_HD_WRITE_ERROR );
			}
			else
				FatalError( FATAL_HD_READ_ERROR );
		}
	}
											/* Write out the updated partition table	*/
	if ( WritePartBootRec( PartLocation, pchMbrBuf, iHdNum ) != OK )
		FatalError( FATAL_HD_WRITE_ERROR );
		
											/* Now handle possible extended partition	*/
	if ( (PartEntry = FindExtPartEntry( 4 )) != NULL )
	{
		memcpy( &ExtPartition, PartEntry, sizeof( struct Part ) );
		if ( ReadPartBootRec( PartEntry, pchMbrBuf, iHdNum ) == OK )
		{
			if ( IsValidPartTable() )
				FixupPartitions( &ExtPartition, FALSE );					/* m102	*/
		}
		else
			FatalError( FATAL_HD_READ_ERROR );
	}
}	

/***************************************************************************/
/* Function to transform partition boot records with logical sectoring 		*/
/* into DOS 5.0 compatible format and determines what the system indicator	*/
/* in the partition should be changed to. The boot sector must have			*/
/* already been read into the pchBootBuf before this function is called.	*/
/*																									*/
/*	int ConvertBoot( struct Part *Part )												*/
/*																									*/
/*	ARGUMENTS:	Part	- Ptr to a partition table entry for this boot sector	*/
/*					IsPrimary - Flags if converting a primary partition			*/
/*	RETURNS:		int	- OK if successful conversion else ERROR if the boot	*/
/*							  record is not a valid DOS record.							*/
/*																									*/
/***************************************************************************/

int ConvertBoot( struct Part *Part, int IsPrimary )					/* m102	*/
{
	unsigned					Factor;
	unsigned long			TotalSectors;
	struct DIR				Dir;
	struct BOOT_HEADER	Header;
	extern char				NewBootRec;
	unsigned					Clusters;

	memcpy( &Header, &NewBootRec, sizeof( struct BOOT_HEADER ) );
	memcpy( &Header.Bpb, pchBootBuf + 11, sizeof( struct BPB ) );

						/* First a sanity check to be sure it can be converted	*/
	if ( !IsFmtedBoot( (char *)(&Header) ) )
		return( ERROR );								/* Not a real DOS partition	*/

	Factor = Header.Bpb.uBytesPerSec / SECTOR_SIZE;
	TotalSectors = ((UL)(Header.Bpb.uTotalSectors)) * ((UL)Factor);

	Header.Bpb.uBytesPerSec		=  SECTOR_SIZE;
	Header.Bpb.uTotalSectors	=  0;					/* Already save the value	*/
	Header.Bpb.ulTotalBigSecs	*= (UL)Factor;
	Header.Bpb.uchSecPerClus = (UCHAR)(Factor*(UCHAR)Header.Bpb.uchSecPerClus);
	Header.Bpb.uReservSec		*= Factor;
	Header.Bpb.uSecPerFat		*= Factor;
	Header.Bpb.ulHiddenSec		=  Part->RelativeSector;
	Header.Bpb.uchMediaDescr	=  (UCHAR)0xf8;
													/* Check for missing total sectors	*/
	if ( TotalSectors == 0L )
	{
		if ( Header.Bpb.ulTotalBigSecs == 0L )
			  TotalSectors = Header.Bpb.ulTotalBigSecs = Part->TotalSectors;
		else
			TotalSectors = Header.Bpb.ulTotalBigSecs;
	}
	else
		Header.Bpb.ulTotalBigSecs = TotalSectors;

	strncpy( (char *)Header.uchSystemId,"FAT16   ",sizeof( Header.uchSystemId ));

		/* m102 We need to work around a bug in old versions of DOS by making*/
		/*      sure any primary partition lies completely in first 32 megs	*/
		/*      of the physcial disk or else we make it a bigfoot partition	*/

	if ( (TotalSectors + (IsPrimary ? Header.Bpb.ulHiddenSec : 0L)) > 0x10000L )
		Part->SystemIndicator = DOSNEW;
	else
	{
		Header.Bpb.ulTotalBigSecs = 0L;
		Header.Bpb.uTotalSectors = (UINT)TotalSectors;

													/* Determine if 16 or 12 bit FAT	*/
		Clusters = Header.Bpb.uTotalSectors -
					  (Header.Bpb.uSecPerFat * (UINT)Header.Bpb.uchNumberFats) -
					  (Header.Bpb.uRootEntries * sizeof( struct DIR ) / 512) -
					  Header.Bpb.uReservSec;
		Clusters = Clusters / (unsigned)Header.Bpb.uchSecPerClus;

		if ( Clusters >= (4096-10) )	/* DOS uses less than (4096-10)=12 bit */
			Part->SystemIndicator = DOS16;
		else
		{
			Header.uchSystemId[ 4 ] = '2';
			Part->SystemIndicator = DOS12;
		}
	}

	SetFileTimeDate( &Dir );
																/*lint -e740 */
	Header.ulSerial = *( (UL *)(&(Dir.Time)) );	/*lint +e740 */
	Header.uchExtSig = 0x29;

	memset( Header.uchVolLabel, ' ', sizeof( Header.uchVolLabel ) );
	Header.uchPhysDrv = (UCHAR)(iHdNum);
 	Header.uchCurHd = 0;

	memcpy( pchBootBuf, &Header, sizeof( struct BOOT_HEADER ) );
	return( OK );
}

/***************************************************************************/
/* Moves all entries in a partition table into the normal DOS 5.x entry		*/
/* positions. Any entries already in the correct location will not be		*/
/* moved. The partition sector must have already been read into the 			*/
/* pchMbrBuf before this function is called.											*/
/*																									*/
/*	void PackPartitionTable( int Entries )												*/
/* 																								*/
/*	ARGUMENTS:	Entries	- Total partition entry fields to process.			*/
/*	RETURNS:		void																			*/
/* 																								*/
/***************************************************************************/

void near PackPartitionTable( int Entries )
{
	struct Part		*OldTable;
	struct Part 	*NextFreeEntry;

	for ( OldTable = GetPartTable( Entries ); OldTable < NewTable; OldTable++ )
	{
		if ( IsCompatPart( OldTable->SystemIndicator ) )
		{
			if ( (NextFreeEntry = GetNextFreePartEntry()) != NULL )
			{
				memcpy( NextFreeEntry, OldTable, sizeof( struct Part ) );
				memset( OldTable, 0, sizeof( struct Part ) );
			}
		}
	}
}

/***************************************************************************/
/* Returns a ptr to then next free partition table entry in the pchMbrBuf.	*/
/* based on a DOS 5.x limit of 4 entries in the table.							*/
/*																									*/
/*	struct Part *GetNextFreePartEntry( void )											*/
/*																									*/
/*	ARGUMENTS:	void																			*/
/*	RETURNS:		struct Part *	- Ptr to first free partition entry				*/
/*																									*/
/***************************************************************************/

struct Part * near GetNextFreePartEntry( void )
{
	register			i;
	struct Part 	*NextFreeEntry;

	NextFreeEntry = NewTable;

	for ( i = 0; i < 4; i++, NextFreeEntry++ )
		if ( NextFreeEntry->SystemIndicator == 0 )
			return( NextFreeEntry );

	return( NULL );
}

/***************************************************************************/
/* Returns TRUE if a sector in pchMbrBuf contains a valid boot signature	*/
/* in the last 2 bytes of the sector (offset 0x1fe - 0x1ff).					*/
/*																									*/
/* int IsValidPartTable( void )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		int	- TRUE if signature exists else FALSE						*/
/*																									*/
/***************************************************************************/

int IsValidPartTable( void )
{
	return( *((UINT *)(pchMbrBuf + BOOT_SIG_OFFSET)) == (UINT)BOOT_SIG );
}

/***************************************************************************/
/* Returns a ptr to the first extended partition entry in partition table	*/
/* in the pchMbrBuf. If no extended partition entry is found in the number	*/
/* of entries specified a NULL ptr is returned.										*/
/*																									*/
/*	struct Part *FindExtPartEntry( int Entries )										*/
/*																									*/
/*	ARGUMENTS:	int	- TotalEntries in the partition table						*/
/*	RETURNS:		struct Part * - Ptr to first ext. partition entry or NULL	*/
/*																									*/
/***************************************************************************/

struct Part *FindExtPartEntry( int Entries )
{
	register		i;
	struct Part	*Part;

	Part = GetPartTable( Entries );
	for ( i = 0; i < Entries; i++, Part++ )
		if ( Part->SystemIndicator == EXTENDED )
			return( Part );
	return( NULL );
}

/***************************************************************************/
/* Returns a ptr to the start of a partition table in the pchMbrBuf based	*/
/* on the number of entries in the table.												*/
/*																									*/
/*	struct Part *GetPartTable( int Entries )											*/
/*																									*/
/*	ARGUMENTS:	Entries	- Total entries which will be in the table			*/
/* RETURNS:		struct Part * - Ptr to start of the partition table			*/
/*																									*/
/***************************************************************************/

struct Part *GetPartTable( int Entries )
{
	return( (struct Part *)(pchMbrBuf + BOOT_SIG_OFFSET -
								   ((UINT)Entries * PART_LEN)) );
}
