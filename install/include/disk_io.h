/***************************************************************************/
/*																									*/
/* DISK_IO.H																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Prototypes and struct definitions for low level disk access via			*/
/* int 13h. 																					*/
/*																									*/
/* 																								*/
/* Created 06-06-89 johnhe																	*/
/***************************************************************************/


/***************************************************************************/

#pragma 		pack(1)							/* Pack all structures */

/***************************************************************************/

#define		DOS_O_RDWR				2

#define		LAST_DISK				256
#define		REDO_DISK				257
#define		NOT_FOUND				258

#define		BAD_MEDIA				1
#define		BAD_HARDWARE			2
#define		BAD_SOURCE				3
#define		NO_SPACE					4

#define		DMA_ERROR				0x09
#define		BAD_DISK_ERROR			-100
#define		MEMORY_ERROR			-200
#define		IVALID_DRIVE_TYPE		-300

#define		MAX_RETRIES				5
#define		DSK_PARAM_LEN			13
#define		DISK_INTERRUPT 		0x13

#define		DISK_TIME_OUT			0x80
#define		DISK_WRITE_PROTECT	0x03

#define		DSK_PARAMS_VECT		0x1e

#define		READ						0
#define		WRITE						1

#define		MAX_FORMAT_TYPE		6

#define     MAX_HIDDEN_SECTORS   1UL


/***************************************************************************/
/* Structure for passing to DOS 4.x int 25h & 26h disk io operations.		*/
/***************************************************************************/

struct ABSIO_PACKET
{
	long			lStartSector;			/* Starting sector	*/
	unsigned 	uNumSectors;			/* Number of sectors */
	char far 	*pchBuffer;				/* Ptr to buffer		*/
};


/***************************************************************************/
/*                                                                    		*/
/* Media bios parameter block structure. Gives information about FAT based	*/
/* block devices.																				*/
/*                                                                    		*/
/***************************************************************************/

struct BPB
{
	unsigned int		uBytesPerSec;		/* Bytes per sector			  		*/
	unsigned char		uchSecPerClus;		/* Sectors per cluster		  		*/
	unsigned int		uReservSec;			/* Number of reserved sectors		*/
	unsigned char		uchNumberFats;		/* Number of copies of the FAT	*/
	unsigned	int		uRootEntries;		/* Number of root dir entries		*/
	unsigned	int		uTotalSectors;		/* Total sectors on the disk		*/
	unsigned char		uchMediaDescr;		/* Media descriptor					*/
	unsigned int		uSecPerFat;			/* Number sectors per FAT copy	*/
	unsigned int		uSecPerTrack; 		/* Number of sectors per track	*/
	unsigned	int		uNumberHeads; 		/* Total number of heads			*/
	unsigned long		ulHiddenSec;		/* Total hidden sectors (boot)	*/
	unsigned long		ulTotalBigSecs;	/* Total sectors on > 32M disks	*/
};

/***************************************************************************/

													/* MS-DOS 4.0 boot record layout	*/
struct BOOT_HEADER
{
	unsigned char		BootJmp[3];			/* 3 byte jmp 							*/
	unsigned char		OemString[8];		/* 8 byte OEM name string 			*/
	struct BPB			Bpb;					/* Disk BPB structure				*/
	unsigned	char		uchPhysDrv;			/* Physical drive number			*/
	unsigned char		uchCurHd;			/* Current hard disk					*/
	unsigned char		uchExtSig;			/* Extended BPB signature 0x29	*/
	unsigned long		ulSerial;			/* Serial number						*/
	unsigned char		uchVolLabel[11];	/* Volume label string				*/
	unsigned char		uchSystemId[8];	/* System ID ie: "FAT12" etc.		*/
};


/***************************************************************************/
/* Format of file's creation time in a DIR structure								*/
/***************************************************************************/

struct	TIME
{
	unsigned		tSec:5;
	unsigned		Min:6;
	unsigned    Hour:5;
};

/***************************************************************************/

#ifndef	DATE_DEFINED

	struct	DATE
	{
		unsigned		Day:5;
		unsigned		Month:4;
		unsigned 	Year:7;
	};
	#define	DATE_DEFINED

#endif

/***************************************************************************/

struct DIR										/* MS-DOS directory entry layout	*/
{
	char				Name[8];					/* File name padded with spaces	*/
	char				Ext[3];					/* Padded file extension			*/
	char				Attrib;					/* File attributes					*/
	char     		Reserv[10];				/* Reserved for MS-DOS				*/
	struct TIME		Time;						/* Packed file creation time		*/
	struct DATE    Date;						/* Packed file creation date		*/
	unsigned			Cluster;					/* File's starting cluster			*/
	unsigned long	Size;						/* File size in bytes				*/
};

/***************************************************************************/
													/* Track layout information		*/
													/* needed by the ROM BIOS format	*/
													/* track function.					*/
struct FIELD_LIST
{
	unsigned	char		uchTrack;			/* Zero based track					*/
	unsigned	char		uchHead;				/* Zero based head					*/
	unsigned	char		uchSector;			/* One based sector					*/
	unsigned char		uchSize;				/* Sector size ( 2 == 512 bytes)	*/
};

/***************************************************************************/
												/* ROM BIOS DASD structure for	*/
												/* disk parameters.					*/
struct DSK_PARMS
{
	unsigned char	Specify1;
	unsigned char	Specify2;
	unsigned char	MotorWait;			/* Wait till motor off 				*/
	unsigned char	SecSize;				/* Bytes/Sector (2 = 512)			*/
	unsigned char	EOT;					/* Sectors per track (MAX)			*/
	unsigned char	RWGap;				/* Read Write Gap						*/
	unsigned char	DTL;					/*  										*/
	unsigned char	FmtGap;				/* Format Gap Length					*/
	unsigned char	Fill;					/* Format Fill Byte					*/
	unsigned char	HdSettle;			/* Head Settle Time (MSec)			*/
	unsigned char	MotorStrt;			/* Motor start delay					*/
	unsigned char	Res1;					/* Reserve field for new ROMs		*/
	unsigned char	Res2;					/* Reserve field for new ROMs		*/
};

/***************************************************************************/

struct DOS_FCB
{
	char				Drive;
	char				Name[8];
	char				Ext[3];
	unsigned			CurBlock;

	unsigned			RecSize;
	unsigned long	FileSize;
	unsigned			Date;
	unsigned			Time;

	char				Reserved[8];
	unsigned char	CurRecord;
	unsigned long	RelRecord;
};

/***************************************************************************/
/* Function prototype for all functions in disk_lib								*/
/***************************************************************************/

extern int	IsInterlnk		( int Drive );
extern int	IsRemoveable	( int Drive );
extern int	IsLocalDrive	( int Drive );
extern int	IsValidDrive	( char DriveLetter );
extern int	IsRamDrive		( char DriveLetter );
extern int	IsReallyValidHardDrive( char chDrive );
extern int	IsValidHardDrive( char chDrive );

extern void	EnableDiskAccess( unsigned char Drive );

extern void	far *SetMediaType	( unsigned uDrv,unsigned uTotalTracks,
									  unsigned uSecPerTrack );
extern int	ResetDrv 		( int Drive );
extern int	SetDiskType 	( unsigned uDrv, unsigned char DiskType );

extern int	IsDiskReady		( int Drive );
extern int	GetBootSector	( int Drive, char far *Buffer );

extern int	CheckDmaBound	( void *Buffer, unsigned Bytes );

extern int	PhyDiskRead		( char *pchBuf,int iSecCyl,int iHead,char chDrive,
									  int cSecs);
extern int	PhyDiskWrite	( char *pchBuf,int iSecCyl,int iHead,char chDrive,
									  int cSecs);

extern int	FcbOpen			( struct DOS_FCB *Fcb );
extern int	FcbRename		( char *Fcb );
extern int	FcbParse			( char *Name, char *Fcb );

extern void far Int24Fail		( void );	/* Must be FAR */
extern int	GetNumberOfDrives( void );
extern int	AbsReadWrite	( int Drive, struct ABSIO_PACKET *absPack,
									  int ReadWrite );

extern int IsValidPath		(char *szPath,unsigned int uDrvNumber, int SavePath);
extern int MoveToDir 		(char *szPath, int SavePath);
extern int IsValidDirName	(char *szPath);
extern int ValidDirChars	(char *szPath);
extern void SetFileTimeDate( struct DIR *Dir );
extern unsigned long ToLongTd( struct DIR *Dir );

extern int	ValidatePath	( char *szPath );
extern int	ValidateDir		( char *szPath );

extern int IsFormatted		( int iDrv, struct BPB *Bpb, char *pchBuffer	);
extern int GetDiskHead		( unsigned uSec, struct BPB *Bpb );
extern int GetDiskTrack 	( unsigned uSec, struct BPB *Bpb );

extern long	_dos_seek		( int Handle, long lOffset, int Mode );
extern int	_dos_dskreset	( void );
extern int	_dos_getdir 	( char *Buffer, int Drive	);

extern unsigned long	GetDiskLabel( int iDrv, char *szLabel, struct BPB *Bpb );
extern unsigned long	SetDiskLabel( int iDrv, char *szLabel, struct BPB *Bpb );

extern int	ReadWriteBoot	( int iDosDrv, void *Buffer, int ReadWrite );
extern int	ReadWriteRoot	( int iDosDrv, struct BPB *Bpb, void *Buf,
									  int Sector, int ReadWrite );
extern int	ReadWriteFat	( int iDosDrv, struct BPB *Bpb, void *Buf,
									  int Sector, int ReadWrite );


extern void interrupt cdecl far NewInt24 (unsigned es, unsigned ds,
														unsigned di, unsigned si,
														unsigned bp, unsigned sp,
														unsigned bx, unsigned dx,
														unsigned cx, unsigned ax );

extern unsigned GetMaxSectorSize	( void );
extern unsigned GetSectorSize		( unsigned char );


/* FILE_LIB */

extern int FileExists		( char *szFile );
extern int IsDirEmpty		( char *szPath );
extern int BigReadWrite		( int iFile,char far *Buf,long lBytes,int RdWr );
extern int RenameFCB			( char *szFrom,char *szTo );
extern int ReplaceFile		( char *szSource,char *szDestin );
extern int RdWrSectors		( int iDrv, unsigned uSec, unsigned uNumSec,
									  char *puchBuf, struct BPB *Bpb, int RdWr );
extern int	CreatRootDir	( int iDrv, struct BPB *Bpb );
extern int	ScrubFatRoot	( int iDrv, struct BPB *Bpb );
extern void ScrubFat 		( unsigned char *pchFat, unsigned Clusters );
extern int	WriteBoot		( int iDrv, struct BPB *Bpb, char *szLabel );
extern long GetDiskFree 	( int DrvLetter );
extern int	GetDskFmtType	( void );
extern int	GetDisketteType( int Drv );


extern int	FindFirstHd		( void *Buffer );
extern int	GetDriveType	( unsigned char Drive, void *Buffer );
extern void	InitNew13		( void *SectorBuffer );
extern void	RestoreOld13	( void );

extern unsigned long	FloppySetDiskLabel( int iDrv, char *szLabel, struct BPB *Bpb );
int FloppyWriteBoot( int iDrv, struct BPB *Bpb, char *szLabel );
