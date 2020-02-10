/***************************************************************************/
/* 																								*/
/* HARDDISK.C																					*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Functions which check the hard disk partition table to be sure they are	*/
/* compatible with DOS 5.0 and determine which drive is going to be the		*/
/* boot drive when the system reboots with DOS 5.0.								*/
/* 																								*/
/* johnhe - 09/03/90																			*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include 	<bios.h>
#include		<memory.h>
#include		<dos.h>
#include		<string.h>

#include 	<alias.h>
#include 	<disk_io.h>
#include 	<global.h>
#include		<hdisk.h>
#include		<strlib.h>
#include		<upgrade.h>
#include		"wsrc.h"

void ChkBootRecs( void );

/***************************************************************************/

extern int far	IOMFIXBPB			( char far * );
		 int	SetBootDrive			( void );

static int	near GetBootPart		( void );
static int	near NumDosParts		( void );
static int	near ParseHardInfo	( int Error );
static int	near IsNonCompatiblePart( void );
static int	near SpecialPartCheck( struct Part *PartEntry );

int	IsSystemFile			( struct DIR *Dir );
int	VfeatureCheck			( void );
int	SyquestCheck			( void );

static int	near IsDiskSizeOK	( void );
static void	near AppendToRoot	( struct DIR *Entry, int iDrv, struct BPB *Bpb );
static int	near IsValidConfig	( char DrvLetter );
static void	near FilesInRootChk	( void );
static void near BadWyseDosCheck	( char *pchBootRec );	  /* m010 */
extern void CopyFiles( int id, struct MULT_FILES *Files ); /* m110 */

void			PowerOf2Error	( void );

/***************************************************************************/

#define		OFFSET  			(BOOT_SIG_OFFSET - ((UINT)Entries * sizeof(struct Part)))
#define		EXT_BPB_LEN		51

struct BPB				HdBpb;
static struct Part	*Part;
static int				Entries;
static char				DosName[2][12] = {{ "IO      SYS" }, { "MSDOS   SYS" }};

/***************************************************************************/

#define		DM_PART		1														/* m110	*/
#define		SPEED_PART	-1														/* m110	*/
#define		MakeDate(m,d,y)	( d + (m<<5) + ((y-1980) << 9) )		/* m110	*/

// m110	The date format is as follows:
// m110
// m110	Bits      Contents
// m110	0-4       Day of month (1-31)
// m110	5-8       Month (1-12)
// m110	9-15      Year (1980-2099)

static unsigned DvrDate = MakeDate( 4U, 9U, 1991U );					/* m110	*/
static char		szDmDriver[] = "DMDRVR.BIN";								/* m110	*/
static char		szSsDriver[] = "SSTOR.SYS";								/* m110	*/
static char		szOverLay[] = "XBIOS.OVL";									/* m110	*/
static char		szOverLaySrc[] = "X:\\XBIOS.OVL";						/* m110	*/
static char		szOverLayDst[] = "X:\\XBIOS.BAK";						/* m110	*/


/***************************************************************************/
/* Determines the number of entries in a partition table by examining the	*/
/* master partition record for specific OEMs which use more than 4 entries.*/
/*																									*/
/* First check is for an AST 8 partition entry table. If will have a 		*/
/* signature of 0x0A55A at offset 0x17c and may have partitions of type		*/
/* 0x14.																							*/
/*																									*/
/*	int GetPartEntries( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		int	- Number of entries in the partition table.				*/
/*																									*/
/***************************************************************************/

int GetPartEntries( void )
{
	register			i;
	UCHAR				Type;
	struct Part		*PartEntry;

	if ( *((UINT *)(pchMbrBuf + AST_SIG_OFFSET)) == (UINT)AST_8_ENT_SIG )
	{
		PartEntry = (struct Part *) (pchMbrBuf + AST_TABLE_OFFSET);

		for ( i = 0; i < 4; i++, PartEntry++ )
		{
			Type = PartEntry->SystemIndicator;
			if ( IsCompatPart( Type ) )
				return( 8 );
		}
	}
	return( 4 );
}

/***************************************************************************/
/* Writes either the tmp master boot record or both the new master boot		*/
/* record and boot partition record.													*/
/* 																								*/
/* NOTE:																							*/
/*			There is a problem with Zenith DOS version 3.21 which causes the	*/
/*			drive letter assignments to change after changing the boot record	*/
/*			and the next time a DOS disk reset is done the drive letter			*/
/*			assigments change. The fix is to zero out all partition entries	*/
/*			except the boot one when the tmp master boot record is written.	*/
/*																									*/
/* void WriteNewBoot( int IsFinal )														*/
/* 																								*/
/* ARGUMENTS:	IsFinal	- If TRUE will write the real boot record.			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void WriteNewBoot( int IsFinal )
{
	#define			PartRec		(Part + vInfo.chBootPart)
	register			i;
	char				*PartBootRec;
	extern char		TmpBoot;
	extern char		NewBootRec;
	extern char		MasterBootRec;

				/* M008 if doing an IADF upgrade we need to use the 3rd sector	*/
				/* for the boot record and the 1st sector for partition table	*/

	PartBootRec = IsFinal ? &MasterBootRec : &TmpBoot;

	if ( RdWrSector( 0x80, 0, 0, 1, READ ) == OK )
	{
		Part = (struct Part *)(pchMbrBuf + TABLE_OFFSET);
		Entries = GetPartEntries();
			
		if ( !vInfo.Flag.fHasIadf )											/* m008 */
			memcpy( pchMbrBuf, PartBootRec, BOOT_CODE_LEN );
		for( i = 0; i < Entries; i++ )
		{		/* If tmp boot record need to zero out all but active  part	*/
			if ( !vInfo.Flag.fHasIadf && !IsFinal && i != (int)vInfo.chBootPart )
				memset( Part + i, 0, sizeof( struct Part ) );
			else
				(Part + i)->BootIndicator = 0;	/* Clear part active field	*/
		}
		PartRec->BootIndicator = ACTIVE;

		if ( !vInfo.Flag.fHasIadf )											/* m008 */
			*( (UL *)(pchMbrBuf+(BOOT_CODE_LEN - 4)) ) = vInfo.ulTmpSerial;

		if ( IsFinal )
		{						/* Add new partition boot record on boot drive	*/
			if ( ReadPartBootRec( PartRec, pchBootBuf, 0x80 ) == OK )
			{
				memcpy( &NewBootRec + 11, pchBootBuf + 11,
						  sizeof( struct BOOT_HEADER ) - 11 );
				if ( WritePartBootRec( PartRec, &NewBootRec, 0x80 ) != OK )
					FatalError( FATAL_HD_WRITE_ERROR );
			}
														/* Also set final serial #			*/
			if ( !vInfo.Flag.fHasIadf )										/* m008 */
				*( (UL *)(pchMbrBuf+(BOOT_CODE_LEN - 4)) ) = vInfo.ulFinalSerial;
		}
		else
		{		/* Tmp partition record must be readable by DOS 5 for recovery	*/
			if ( !IsDosPart( PartRec->SystemIndicator ) )
				PartRec->SystemIndicator = (UCHAR)DOSNEW;
		}
			/* Write new partition boot rec and if system has IADF also need	*/
			/* to put the MBR code on the 3rd sector.									*/
		if ( RdWrSector( 0x80, 0, 0, 1, WRITE ) == OK )
		{
			if ( vInfo.Flag.fHasIadf )
			{			/* Need to write MBR to 3rd sector with proper serial #	*/
				memcpy( pchMbrBuf, PartBootRec, SECTOR_SIZE );

				*( (UL *)(pchMbrBuf+(BOOT_CODE_LEN - 4)) ) = IsFinal ?
				vInfo.ulFinalSerial : vInfo.ulTmpSerial;

				*((UINT *)(pchMbrBuf + BOOT_SIG_OFFSET)) = (UINT)BOOT_SIG; 
				if ( RdWrSector( 0x80, 0, 0, 3, WRITE ) == OK )
					return;
			}
			else
				return;
		}
	}
	FatalError( FATAL_HD_WRITE_ERROR );
}

/***************************************************************************/
/* Gets the name of the 2 system files in the root directory if they exist.*/
/*																									*/
/*	void GetSysFiles( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void GetSysFiles( void )
{
	struct DIR		Dir[2];

	if ( ReadWriteRoot( vInfo.chFirstHd-'A', &HdBpb, pchBootBuf,
							  0, READ ) == OK )
	{
		memcpy( Dir, pchBootBuf, sizeof( Dir ) );

		if ( IsSystemFile( &Dir[0] ) )
		{													/* Save bios file	name	*/
			vInfo.Flag.fIoSys = TRUE;
			DirToFileName( vInfo.szIoSys, Dir[0].Name );
		}
		if ( IsSystemFile( &Dir[1] ) )
		{													/* Save dos file name	*/
			vInfo.Flag.fMsDos = TRUE;
			DirToFileName( vInfo.szMsDos, Dir[1].Name );
		}				
	}
	else
		FatalError( FATAL_HD_READ_ERROR );
}

/***************************************************************************/
/* Moves the first 2 entries in the root directory to the end of the 		*/
/* directory if they are not DOS system files and then creates 2 new first	*/
/* entries for MSDOS.SYS and IO.SYS.													*/
/*																									*/
/*	int FixupRoot( int iDosDrv )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void FixupRoot( void )
{
	register			i; 								/* Loop indice 					*/
	register			iDosDrv;
	struct DIR		OldEntry[ 2 ];					/* Old 1st 2 root entries		*/

	iDosDrv = vInfo.chFirstHd - 'A';
	if ( ReadWriteRoot( iDosDrv, &HdBpb, pchBootBuf, 0, READ ) == OK )
	{
		memcpy( OldEntry, pchBootBuf, sizeof( OldEntry ) );
		for ( i = 0; i < 2; i++ )
		{
			if ( !IsSystemFile( &(OldEntry[i]) ) &&
				  OldEntry[i].Name[0] != 0 &&
				  OldEntry[i].Name[0] != (char)0xe5 )
			{
				AppendToRoot( OldEntry + i, iDosDrv, &HdBpb );
				OldEntry[i].Name[0] = (char)(0xe5);
			}
		}
								/* Have to read the root sector again in case we	*/
								/* changed it by appending entries						*/
		if ( ReadWriteRoot( iDosDrv, &HdBpb, pchBootBuf, 0, READ ) == OK )
		{
			if ( OldEntry[0].Name[0] == 0 || OldEntry[0].Name[0] == (char)0xe5 )
				memset( pchBootBuf, 0, sizeof( struct DIR ) );

			if ( OldEntry[1].Name[0] == 0 || OldEntry[1].Name[0] == (char)0xe5 )
				memset( pchBootBuf + sizeof(struct DIR), 0, sizeof(struct DIR) );

									/* strcpy will also set the attrib to _A_NORMAL	*/
			strcpy( pchBootBuf, DosName[ 0 ] );
			strcpy( pchBootBuf+sizeof( struct DIR ), DosName[ 1 ] );

			if ( ReadWriteRoot( iDosDrv, &HdBpb, pchBootBuf, 0, WRITE ) == OK )
			{
				_dos_dskreset();
				return;
			}
			FatalError( FATAL_HD_WRITE_ERROR );
		}
	}
	FatalError( FATAL_HD_READ_ERROR );
}

/***************************************************************************/
/* Appends a directory entry to the end of the root directory on the			*/
/* the specified drive. The first 2 root directory entries are always		*/
/* skipped over in the search for the first free entry.							*/
/*																									*/
/*	int AppendToRoot( struct DIR *Entry, int iDrv, struct BPB *Bpb )			*/
/*																									*/
/*	ARGUMENTS:	Entry	- Ptr to directory entry structure to append				*/
/*					iDrv	- Drive number (A=1, B=2, ...)								*/
/*					Bpb	- Ptr to BPB struct for disk being fixed up				*/
/*	RETURNS:		int	- OK if successful else ERROR if no room or disk err	*/
/*																									*/
/***************************************************************************/

void near AppendToRoot( struct DIR *Entry, int iDosDrv, struct BPB *Bpb )
{
	register			i;
	register			iSector;
	int				iTotalSectors;
	int				iEntriesPerSector;
	struct DIR		*Dir;

	iEntriesPerSector =	(int)Bpb->uBytesPerSec / (int)(sizeof( struct DIR ));
	iTotalSectors = (int)Bpb->uRootEntries / iEntriesPerSector;

	for ( iSector = 0; iSector < iTotalSectors; iSector++ )
	{
		if ( !ReadWriteRoot( iDosDrv, Bpb, pchBootBuf, iSector, READ ) )
		{
			Dir = (struct DIR *)pchBootBuf;

			for ( i = 0; i < iEntriesPerSector; i++, Dir++	)
			{											/* Skip over 2 reserved entries */
				if ( iSector == 0 && i == 0 )	
					Dir += 2, i += 2;
				if ( Dir->Name[0] == (char)0xe5 || Dir->Name[0] == 0 )
				{
					memcpy( Dir, Entry, sizeof( struct DIR ) );
					if ( !ReadWriteRoot( iDosDrv, Bpb, pchBootBuf, iSector, WRITE ) )
						return;
				}
			}
		}
	}
	FatalError( FATAL_HD_WRITE_ERROR );
}

/***************************************************************************/
/* Read a single disk sector into a global buffer using int 13h.				*/
/* 																								*/
/* int RdWrSector( int iDrv, int Head, int Track, int Sector, int RdWr	)	*/
/* 																								*/
/* ARGUMENTS:	iDrv		- Physical drive number (starts from 0)				*/
/* 				iHead 	- Head number				(starts from 0)				*/
/* 				iTrack	- Track number 			(starts from 0)				*/
/* 				iSector	- Sector number			(starts from 1)				*/
/* 				RdWr		- Signals READ or WRITE 									*/
/* RETURNS: 	int		- OK if successful else !OK								*/
/* 																								*/
/***************************************************************************/

unsigned RdWrSector( int iDrv, int iHead, int iTrack, int iSector, int RdWr )
{
	struct diskinfo_t 	Info;

	Info.drive = (unsigned)iDrv;
	Info.head = (unsigned)iHead;
	Info.track = (unsigned)iTrack;
	Info.sector = (unsigned)iSector;
	Info.nsectors = 1;
	Info.buffer = pchMbrBuf;

	return( _bios_disk( RdWr == READ ? _DISK_READ : _DISK_WRITE, &Info ) >> 8 );
}

/***************************************************************************/
/* Returns TRUE if a directory entry is a system file as noted by it having*/
/* and IBM or MS system file name.														*/
/*																									*/
/*	int IsSystemFile( struct DIR *Dir )													*/
/*																									*/
/*	ARGUMENTS:	Dir	- Ptr to directory structure to check.						*/
/*	RETURNS:		int	- TRUE if entry is a system file else false				*/
/*																									*/
/***************************************************************************/

int IsSystemFile( struct DIR *Dir )
{
	register			i;

	for ( i = 0; i < 2; i++ )
	{
		if ( memcmp( Dir->Name, DosName[i], 11 ) == OK )
			return( TRUE );
	}

	return( FALSE );
}

/***************************************************************************/
/* Determines if a partition system indicator is either a valid DOS or		*/
/* compatible system indicator.															*/
/*																									*/
/*	int IsCompatPart( UCHAR SystemIndicator )											*/
/*																									*/
/*	ARGUMENTS:	SystemIndicator - Partition type value								*/
/*	RETURNS:		int				 - TRUE is DOS compatible else FALSE			*/
/*																									*/
/***************************************************************************/

int IsCompatPart( UCHAR SystemIndicator )
{
	register			i;
	static int OkType[] = { DOS12, DOS16, DOSNEW, ASTid, ATNTid, TANDYid,
								 		 UNISYSid, L_EDGEid, NECid, COMMODOREid };

	for ( i = 0; i < (int)(sizeof( OkType ) / sizeof( int )); i++ )
		if ( (int)SystemIndicator == OkType[i] )
			return( TRUE );

	return( FALSE );
}

/***************************************************************************/
/*	Renames an existing Disk Manager or SpeedStor device driver to .BAK and	*/
/*	then copy over a newer driver from the distribution disk. If the driver	*/
/*	being replaced is Disk Manager a new XBIOS.OVL file will be copied to	*/
/* the root directory of the boot hard disk after the existing file is		*/
/* renamed to .BAK.																			*/
/*																									*/
/*	void near UpdatePartDrivers( void )													*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

#include		<data.h>
#include		<copy.h>
#include		<file_io.h>

void UpdatePartDrivers( void )
{
	char						*szFileName;
	struct MULT_FILES 	File[ 3 ];
	extern int				CreatingRecovery; /* Signal can't continue on error */
	char						szSrcPath[ MAX_PATH_LEN ];
	char						szDstPath[ MAX_PATH_LEN ];

   if ( !vInfo.Flag.fSpeedstor && !vInfo.Flag.fDMdriver ) // MC 4/13/92 #1
      return;                                             // MC 4/13/92 #1

   if ( vInfo.Flag.fSpeedstor )                   // MC 4/13/92 #1
      szFileName = szSsDriver;                    // MC 4/13/92 #1
   else                                           // MC 4/13/92 #1
      szFileName = szDmDriver;                    // MC 4/13/92 #1
   
   memset( File, 0, sizeof( struct MULT_FILES ) * 3 );

	File[ 0 ].Name.Destin = File[ 0 ].Name.Source = szFileName;

	File[ 0 ].Path.Destin = vInfo.szPartDrvPath+2;
	if ( File[ 0 ].Path.Destin[0] == EOL )
		File[ 0 ].Path.Destin = "\\";
	File[ 0 ].Path.Source = vInfo.szSource + 2;


	File[ 0 ].Drive.Destin = vInfo.szPartDrvPath[0];
	File[ 0 ].Drive.Source = vInfo.chSource;

	File[ 0 ].UserDisk = NOT_REMOVEABLE;
	File[ 0 ].DiskNumber = GetRealSrcDisk( szFileName, 0 );

	if ( vInfo.chSource >= vInfo.chFirstHd )
		File[ 0 ].DiskNumber = NOT_REMOVEABLE;

									/* Rename existing driver(s) to "xxx.bak"	*/
	BuildPath(szSrcPath,vInfo.szPartDrvPath[0],vInfo.szPartDrvPath+2,szFileName);
	strcpy( szDstPath, szSrcPath );	

			/* The strchr won't return NULL so long as we supply the driver	*/
			/* name with the .ext added.													*/
	strcpy( strchr( ParseFileName( szDstPath ), '.' ), ".BAK" );
	AbsUnlink( szDstPath );
	rename( szSrcPath, szDstPath );

		/* If Disk Manager we have to copy XBIOS.OVL to root of boot drive	*/
   if ( vInfo.Flag.fDMdriver )                   // MC 4/13/92 #1
	{
		File[ 1 ].Name.Destin = File[ 1 ].Name.Source = szOverLay;

		File[ 1 ].Path.Destin = "\\";						/* Always to the root	*/
		File[ 1 ].Path.Source = vInfo.szSource + 2;

		File[ 1 ].Drive.Destin = vInfo.chDestin;
		File[ 1 ].Drive.Source = vInfo.chSource;

		File[ 1 ].UserDisk = NOT_REMOVEABLE;
		File[ 1 ].DiskNumber = GetRealSrcDisk( szOverLay, 0 );

		if ( vInfo.chSource >= vInfo.chFirstHd )
			File[ 1 ].DiskNumber = NOT_REMOVEABLE;

			/* Rename X:\XBIOS.OVL to X:\XBIOS.BAK	*/
		*szOverLaySrc = *szOverLayDst = vInfo.chDestin;
		AbsUnlink( szOverLayDst );
		rename( szOverLaySrc, szOverLayDst );
	}
	CopyFiles( IDS_WAITCOPY, File );
}

