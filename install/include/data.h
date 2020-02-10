/***************************************************************************/
/* 																								*/
/* DATA.H																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Defines, prototypes and extern declarations for functions in DATA.C. 	*/
/*																									*/
/* Created 11-11-89 - johnhe																*/
/***************************************************************************/


#define	MAX_SRC_LIST		110
#define	MAX_PER_DISK		110

#define	EOC_MARKER			0xffff
#define	MAX_NUM_DISKS		15

#define	MAINTENANCE_MODE_INSTALL	-1

enum		UpgradeData			{ LIE_TO, RE_NAME, DELETE_FILE, ADD_DEVICE,
									  DRIV_PARM, REM_DEVICE, NO_INSTALL, DIF_FILE,
									  DIF_DISK, BIOS,

											/* Distribution disk label strings */
									  DIST_LABEL,

											/* Distribution disk prompts */
									  USER_LABEL,

									  COMPRESSION_RATIO,

									  USER_PROMPT, PROMPT,

									  DIF_PATH, GLOBAL_RENAME, UPDATE_DEV,
									  VIDEO_LIST, VIDEO_DRIVER, DISK_BYTES,
									  DISK_TYPE, GRABBER_DRIVER, NET_FILES,
									  DRIVER_VERSION, DELETE_DRIVER,
									  COMPONENTS_BYTES, TOTAL_BYTES,

									  BACKUP_WINDOS_FILES,
									  BACKUP_WIN_FILES,
									  BACKUP_DOS_FILES,
									  UNDELETE_WINDOS_FILES,
									  UNDELETE_WIN_FILES,
									  UNDELETE_DOS_FILES,
                             OPTCOMP_UNDELETE,
									  ANTIVIRUS_WINDOS_FILES,
									  ANTIVIRUS_WIN_FILES,
									  ANTIVIRUS_DOS_FILES,
                             OPTCOMP_ANTIVIRUS,

                                 /* Emergency-floppy layouts */
                             DOS360, DOS720, DOS120,

                                 /* 3rd part disk caching */
                             CACHES,

                                 /* Utilities smartdrv won't run with */
                             CONFLICTS_SMARTDRV,

                                 /* Drivers we can't add files before */
                             LOAD_AFTER_DRVS,

											/* Distribution disk file layouts */
									  DISTR_0, DISTR_1, DISTR_2, DISTR_3, DISTR_4,
                             DISTR_5, DISTR_6, DISTR_7, DISTR_8, DISTR_9,
                             DISTR_10,DISTR_11,DISTR_12,DISTR_13,DISTR_14,

											/* User disk file layouts */
									  DISK_0,DISK_1, DISK_2, DISK_3, DISK_4,
									  DISK_5, DISK_6, DISK_7, DISK_8, DISK_9,

									  OEM_TABLE, END_CLASS };

#define   MIN_INSTALL_BYTES               0
#define   DOS_INSTALL_BYTES               1

#define   BACKUP_COMPONENTS               2

#define   BACKUP_WIN_DOS_COMPONENTS       2
#define   BACKUP_WIN_COMPONENTS           3
#define   BACKUP_DOS_COMPONENTS           4

#define   UNDELETE_COMPONENTS             5

#define   UNDELETE_WIN_DOS_COMPONENTS     5
#define   UNDELETE_WIN_COMPONENTS         6
#define   UNDELETE_DOS_COMPONENTS         7

#define   ANTI_VIRUS_COMPONENTS           8

#define   ANTI_VIRUS_WIN_DOS_COMPONENTS   8
#define   ANTI_VIRUS_WIN_COMPONENTS       9
#define   ANTI_VIRUS_DOS_COMPONENTS       10

#define   FLOPPY_360_INSTALL_BYTES        11
#define   FLOPPY_720_INSTALL_BYTES        12
#define   FLOPPY_120_INSTALL_BYTES        13

#define   UNINSTALL_BYTES                 14

/***************************************************************************/
/* Structure to define the layout of the dos data OEM records. 				*/
/***************************************************************************/

#ifndef	OEM_SIZE
	#define OEM_SIZE	20

struct DDR
{
	char			szOemName[ OEM_SIZE ];		/* OEM name 						*/
	UCHAR			MajorVer;						/* Major DOS version number	*/
	UCHAR 		MinorVer;						/* Minor DOS verison number	*/
	unsigned 	uDataOffset;					/* Offset of start of data 	*/
};

#endif

/***************************************************************************/
/* Prototypes for all functions in DATA.C.											*/
/***************************************************************************/

extern void 		InitDosData 		( char *szFilePath );
extern void 		FreeDataMemory 	( void );
extern void 		LoadOemData			( struct DDR *Oem );
extern char 		**InitSearchData	( int MajorVer, int	MinorVer );
extern void 		FreeSearchData 	( void );

extern void 		InitBannerData 		( char *szFilePath );

static unsigned	GetNextDataBlock	( int iFile, void **DataBuf );
static void			ReadOemData			( unsigned uDataOffset );
static char			**GetClassData		( char *InfoClass );

extern struct DDR *GetOemRecord		( char *szOemName, int MajorVer,
												  int MinorVer );

extern char			*GetDataString		( int Type, int Index );
extern char 		**GetClassList 	( int Type );
extern char 		*TranslateString	( int Type, char *szString );
extern int			BuildOemList		( char **szList, int MajorVer,
												  int	MinorVer );

extern int			GetDistrDiskType	( void );
extern int			GetNumberDisks 	( void );
extern char 		*GetFileName		( int iDisk, int iFile );

extern char 		*GetDistribLabel	( int iDiskNum );
extern char 		*GetDistribPrompt	( int iDiskNum );
extern char 		*GetUserLabel		( int iDiskNum );
extern char 		*GetUserPrompt		( int iDiskNum );
extern long			GetDiskBytes		( int	iDiskNum );
extern unsigned	GetDriverVersion	( int iDriverNum );
extern int			IsDistrFile			( char *szFile );

extern char 		*GetRealDestName	( char *szFileName );
extern int			GetRealSrcDisk 	( char *szFile, int iOldDisk );
extern char 		*GetRealDestPath	( char *szFile, char *szOldPath );
extern char 		*GetRealSrcName	( char *szFile );
extern int			FindDataMatch		( char *szString, int Type );
extern char 		*GetSearchStr		( int FileNum, int StrNum );

