/***************************************************************************/
/*																									*/
/*	HDISK.H																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Definition for hard disk partition information. 								*/
/*																									*/
/***************************************************************************/

#define INVALID		0xFF
#define PRIMARY		0x00
#define EXTENDED		0x05
#define XENIX1 		0x02
#define XENIX2 		0x03
#define PCIX			0x75
#define DOS12			0x01
#define DOS16			0x04
#define DOSNEW 		0x06


/* OS/2 Boot Manager (BM) partition types */

#define BM_PART			0x0a
#define BM_FAT12_PART	0x11
#define BM_FAT16_PART	0x14
#define BM_BIGFAT_PART	0x16
#define BM_HPFS_PART		0x17
#define HPFS_PART			0x07

/* DR-DOS Password Protected (PP) partition types */

#define PP_FAT12_PART	0xC1
#define PP_FAT16_PART	0xC4
#define PP_BIGFAT_PART	0xC6
#define PP_EXT_PART		0xC5

#define FAT16_SIZE	32680
#define VOLUME 		0x00
#define LOGICAL		0x05

#define TABLE_OFFSET 		0x1be
#define BOOT_SIG_OFFSET		510
#define BOOT_SIG				0xaa55
#define ACTIVE					0x80
#define INACTIVE           0x00
#define NO_DOS_PART        (-1)
#define DOS_MAX				65535 	/* Allow exactly 32mb of partitions */

#define BYTES_PER_SECTOR	512
#define MAX_PART_ENTRIES	4
#define MAX_DIR_ENTRIES		0x200
#define HD_MEDIA_BYTE		0xf8
#define BPB_OFFSET			11

#define NO_DRIVE					-1
#define MAXIMUM_DRIVE_NUMBER	7
#define MAX_HARD_DRIVES 		8
#define FIRST_HARD_NUM			0x80

#define NO_VOLUME		""
#define NO_FORMAT		"UNKNOWN "
#define FAT_12			"FAT12   "
#define FAT_16			"FAT16   "
#define EXT_DISK_INFO 0x0F

#define VOL_LABEL 	0x08
#define DECIMAL		0x2E
#define PERIOD 		0x2E
#define ONE_MEG		1048576

#define	MAX_MAP_ENTRIES	9
#define	MAP_SIZE			(sizeof( struct PartMap ) * MAX_MAP_ENTRIES)
#define	BIG_MAP_SIZE	(MAP_SIZE *  MAX_HARD_DRIVES)
#define	HD_INFO_SIZE	(sizeof( struct HdInfo ) *  MAX_HARD_DRIVES)
#define	MIN_DOS_PART	(4000L)

/* Used to set unformatted media flag with SetHDAccess(). */
#define  ALLOW_HD_ACCESS   1
#define  DENY_HD_ACCESS    0

/***************************************************************************/
/* Hard disk partition record																*/
/***************************************************************************/

struct	Part
{
	unsigned char	BootIndicator; 	/* If 80h means this is boot partition */
	unsigned char	StartHead;			/* Partition starting head based 0		*/
	unsigned char	StartSector;		/* Partition starting sector based 1	*/
	unsigned char	StartCylinder; 	/* Partion starting track based 0		*/
	
	unsigned char	SystemIndicator;	/* Partition type signature field		*/
	unsigned char	EndHead; 			/* Partition ending head based 0 		*/
	unsigned char	EndSector;			/* Partition ending sector based 1		*/
	unsigned char	EndCylinder;		/* Partition ending track based 0		*/

	unsigned long	RelativeSector;	/* Physcial starting sector based 0 	*/
	unsigned long	TotalSectors;		/* Total physical sectors in partition	*/
};

/***************************************************************************/
/* Hard disk partition table																*/
/***************************************************************************/

struct   PartTable
{
	struct Part    PartEntry[MAX_PART_ENTRIES];
};

/***************************************************************************/
/* Hard disk physical layout structure. Information to fill in the struct	*/
/* can be found using int 13h function 8h.											*/
/***************************************************************************/

struct	HdParms
{
	unsigned 		MaxHead; 			/* Maximum head number based from 0 	*/
	unsigned 		MaxSec;				/* Maximum sector number based from 1	*/
	unsigned			MaxCyl;				/* Maximum track number based from 0	*/
};

/***************************************************************************/
/* Hard disk information structure. 													*/
/***************************************************************************/

struct	HdInfo
{
	int					DrvNum;									/* Phys drv #			*/
	struct HdParms 	Parms;									/* Dsk parameters 	*/
	struct Part 		PartEntry[ MAX_PART_ENTRIES ];	/* Partition table	*/
	unsigned 			DiskError	:1;
	unsigned				HasMbr		:1;						/* Flags valid mbr	*/
	unsigned 			HasDosPart	:1;						/* Flags if dos part */
	unsigned 			IsFmted		:1;						/* Flags if fmted 	*/
	unsigned				IsDos4		:1;						/* Dos 4 compatible	*/
	unsigned 			HasFree		:1;						/* Has free space 	*/
	unsigned 			HasBmPart	:1;						/* OS/2 Boot Manager Part. */
	unsigned 			HasBmDosPart:1;						/* Dos parts reserved by BM */
	unsigned 			HasHpfsPart	:1;						/* HPFS partition		*/
	unsigned 			HasPpDosPart:1;						/* Password Protected Part. */
};

/***************************************************************************/
/* Partition map entry structure for keeping information about a paritions */
/* physical position on the hard disk. 												*/
/***************************************************************************/

struct	PartMap

{
	unsigned 	PartType;				/* Partition type signature				*/
	UL				StartSec;				/* Partition physical starting sector	*/
	UL 			EndSec;					/* Partition physical last sector		*/
	UL 			TotalSecs;				/* Total physical sectors in partition */
};

/***************************************************************************/

struct diskaccess
{
	char			dac_special_func;
	char			dac_access_flag;
};

/***************************************************************************/


extern UINT IsDosPart		( unsigned char PartitionType );
extern UINT IsFmtedBoot		( char *SectorBuf );
extern UINT	IsDos4Compat	( char *SectorBuf );
extern UL 	GetTotalHdSectors( struct HdParms *Parms );

extern void CreateMapEntry	( struct PartMap *Map, struct Part *Part );
extern UL	BuildPartMap	( struct Part *Part, struct PartMap *Map,
									  struct HdParms *Parms );

extern int 	GetNumHardDisks( void );
extern int	GetDrvParms 	( struct HdParms *Parms, int DrvNum );
extern int	ReadPartBootRec( struct Part *Entry, void *SectorBuf,
									  int HardDrvNum );

extern int	WritePartBootRec( struct Part *Entry, void *SectorBuf,
									   int HardDrvNum );

extern int	FindMaxFreePart( struct PartMap *Map );
extern void BuildPartEntry	( struct PartMap *Map, struct HdParms *Parms,
									  struct Part *PartEntry );

extern int	HdIsFormatted	( char DriveLetter, struct diskaccess *Dac );


