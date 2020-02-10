/***************************************************************************/
/* 																								*/
/* BACKUP.H																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Include file with definitions for RUP backup utility. These structures	*/
/* follow the standard backup layout of MS-DOS 3.30 backup and restore.		*/
/*																									*/
/* Created 07-13-89 johnhe																	*/
/*																									*/
/***************************************************************************/


#define		MALLOC_MEMORY			20000L	/* Max size for malloc()	*/
#define		MIN_COPY_BUFFER		5000L		/* Min size for cpy buffer	*/

#define		READ						0
#define		WRITE						1

#define		BACKUP					0			/* Backup modes */
#define		CONTROL					1
#define		COUNT						2

#define		ESC						0x1b		/* Ascii ESC character	*/

#define		ABORT						-1			/* Misc error codes */
#define		OK							0
#define		ERROR						-1
#define		BAD_MEDIA				1
#define		BAD_HARDWARE			2
#define		BAD_SOURCE				3
#define		DOOR_OPENED				4
#define		LAST_DISK				256
#define		REDO_DISK				257
#define		NOT_FOUND				258

#define		PLEASE_WAIT				64

#define		CLUSTERS_360K			512 * 2		/* Clusters on a 360K disk	*/
#define		MAX_DIR_DEPTH			33				/* Really 32 */

#define		DISK_REC_LEN			sizeof( struct Disk_Header )
#define		DIR_REC_LEN				sizeof( struct Dir_Block )
#define 		FILE_REC_LEN			sizeof( struct File_Header )

#define		RECS_LEN					sizeof( struct RECORDS )
#define		STATE_LEN				sizeof( struct INFO_STATE )

													/* Find all files */
#define		_A_FILE 					( _A_HIDDEN | _A_SYSTEM  )
													/* Find all files and subdirs */
#define		_A_ALLDIR				(_A_FILE | _A_SUBDIR)

#define		DOS_O_RDWR 				2	/* DOS create for read and write */


#define		LAST_TARGET				0xFF			/* -1 ==  Last disk */
#define		NOT_LAST_TARGET		0x00			/*  0 ==  !Last disk */
#define		LAST_DB					0xFFFFFFFF	/* -1L == Last dir block */

#define		LASTPART					1		/* Flags used in file headers */
#define		NOTSUCCESSFUL			0
#define		SUCCESSFUL				2
#define		NOTLASTPART				0

#define		MAX_DOS_DRIVES			26

#define		ROOTDIR					0		/* Level of root directory */

#define		BYTE						unsigned char
#define		WORD						unsigned short
#define		DWORD						unsigned long

#define		FAR						far
typedef		void						VOID;

/***************************************************************************/
/*                                                             				*/
/*    CONTROL BLOCK FOR EACH BACKUP DISKETTE		       							*/
/*                                                             				*/
/*    THIS STRUCTURE WILL MAKE UP THE FIRST DH_DHLength BYTES  				*/
/*    OF THE control.xxx FILE ON THE BACKUP TARGET.	       					*/
/*    IT IDENTIFIES THE DISK AS BEING A BACKUP, AND INCLUDES   				*/
/*    DISKETTE SEQUENCE NUMBER, PLACE FOR ORIGINAL PARAMETERS  				*/
/*    ENTERED FROM THE COMMAND LINE, AND A BYTE INDICATING THE					*/
/*    LAST TARGET, AND LENGTHS OF OTHER DATA STRUCTURES.       				*/
/*                                                             				*/
/***************************************************************************/

struct Disk_Header
{
	UCHAR		Len;						/* Length, in bytes, of disk header	*/
	UCHAR		Id[8];					/* Identifies disk as a backup	   */
	UCHAR		Seq;						/* Backup diskette sequence num    	*/
											/*    (binary 1 255)		   			*/

	UCHAR		CmdLine[128];			/* Save area for command line	   	*/
											/*     parameters.		   			*/

	UCHAR		IsLast; 					/* Indicates if this is last target */
											/* 0xFF if last target, 0 otherwise	*/
};


/***************************************************************************/
/*                                                                      	*/
/*    DIRECTORY BLOCK																		*/
/*                                                                      	*/
/*  THIS STRUCTURE IS WRITTEN TO THE control.xxx FILE AT LEAST ONCE			*/
/*  FOR EACH SUBDIRECTORY, INCLUDING THE ROOT, BACKED UP. IT SAVES			*/
/*  THE PATH TO THAT DIRECTORY, THE NUMBER OF FILES FROM THAT					*/
/*  DIRECTORY THAT ARE BACKED UP ON THE TARGET, AND A POINTER 					*/
/*  TO THE NEXT DIRECTORY BLOCK ON THAT DISKETTE, IF ONE EXISTS.				*/
/*  IF THERE IS NO OTHER DIRECTORY BLOCK, IT EQUALS  1. THERE IS ALSO A		*/
/*  RESERVED AREA FOR FUTURE POSSIBLE EXPANSION. AFTER THE RESERVED AREA	*/
/*  COMES AN ARRAY OF db_num_entries FileHeaders, DESCRIBED BELOW.			*/
/*                                                                     		*/
/***************************************************************************/

struct Dir_Block
{
	UCHAR		Len;				/* Length, in bytes, of dir block */
	UCHAR		Path[63];
									/* ASCII path of this directory,  */
									/* drive letter omitted 	  */
	UINT		Entries;			/* # of filenames in list for this disk */
	LONG		NextDir;			/* Offset of next directory block on this disk */
};									/* =0xffffffff if last dir block on this disk  */


/***************************************************************************/
/*                                                                    		*/
/*    CONTROL BLOCK FOR EACH BACKED UP FILE			      						*/
/*                                                                    		*/
/*  THIS STRUCTURE WILL BE REPEATED AFTER THE DIRECTORYBLOCK ONCE     		*/
/*  FOR EACH FILE BACKED UP FROM THAT DIRECTORY. IT CONTAINS THE      		*/
/*  FILENAME, DIRECTORY INFORMATION, AND OTHER NECESSARY INFORMATION. 		*/
/*                                                                    		*/
/***************************************************************************/

struct File_Header
{
	UCHAR		HeadLen;				/* length, in bytes, of file header			*/
	UCHAR		Name[12];			/* ascii file name		   					*/
	UCHAR		Flags;				/* bit 0=1 if last part of file    			*/
										/* bit 1=1 if it is backed  up successfully*/
	LONG		FileLen;				/* length of the file		   				*/
	UINT		Seq;					/* sequence #, files that span disks  		*/
	LONG		Offset;				/* offset of this file in BACKUP.???		*/
	LONG		PartSize; 			/* length of this part of file	   		*/
	UINT		Attrib;				/* file attribute from directory   			*/
	struct TIME	Time;				/* time when file was created	   			*/
	struct DATE	Date;				/* date when file was created	   			*/
};

/***************************************************************************/
/*                                                                    		*/
/*	CURRENT_DISK																				*/
/*                                                                    		*/
/* Structure to hold all current information about the disk being backed	*/
/* up.																							*/
/*                                                                    		*/
/***************************************************************************/

struct CURRENT_DISK
{
	UCHAR		Seq;					/* Backup diskette sequence number 	*/
	UINT		Flags; 				/*												*/
	UCHAR		Type; 				/* Type of disk from format table	*/
	LONG		DiskSize;			/* Total bytes on empty disk			*/
	LONG		Written; 			/* Total bytes already written		*/
	LONG		Free; 				/* Total bytes still available		*/
	LONG		OffsetDir;		  	/* Offset in file of last DIR record */
};

/***************************************************************************/
/*                                                                    		*/
/*	CTRL_FILE																					*/
/*                                                                    		*/
/* Structure to hold all current information about the control file being	*/
/* built.																						*/
/*                                                                    		*/
/***************************************************************************/

struct CTRL_FILE
{
	LONG		Offset;				/* Total current offset in the file */
	LONG		DirRecOffset;		/* Offset of current DIR entry in the file */
	LONG		DiskUsed;			/* Total disk space used by this file */
	LONG		FileRecOffset;		/* Offset of current FILE entry in the file */
};


/***************************************************************************/
/*                                                                    		*/
/*	BKUP_FILE																					*/
/*                                                                    		*/
/*	Structure to hold all current information about the backup file being	*/
/* built.																						*/
/*                                                                    		*/
/***************************************************************************/

struct BKUP_FILE
{
	LONG		Offset;				/* Total current offset in the file */
	LONG		DiskUse;				/* Total disk space used by this file */
};
	
	
/***************************************************************************/
/*                                                                    		*/
/*	WR_BUF_INFO																					*/
/*																									*/
/* Status structure which contains the currents status of the disk			*/
/* write buffer.																				*/
/*                                                                    		*/
/***************************************************************************/

struct WR_BUF_INFO
{
	UCHAR huge	*Start;			/* Far ptr to start of buffer */
	UCHAR huge	*Next;

	LONG			BufOffset;		/* Current offset for appending to file */
	LONG			FileOffset;		/* Offset in file where start of buffer is */

	LONG			BufLen;			/* Total length of the buffer */
	LONG			BytesFree;		/* Number free  bytes left in the buffer */
};

/***************************************************************************/
/*                                                                    		*/
/* Structure used for 2 work areas during backup. The second work area		*/
/* will be a duplicate of the first at the start of a disk copy so it can	*/
/* be used if it is necessary to redo an unuse-able diskette.					*/
/* 																								*/
/*                                                                    		*/
/***************************************************************************/

struct INFO_STATE
{
	char						Path[ MAX_PATH_LEN ];		/* Current search path	*/
	struct find_t			Info[ MAX_DIR_DEPTH + 1 ];	/* Directory FCBs			*/
};

struct RECORDS
{
	struct Disk_Header	Header;							/* Current disk header	*/
	struct Dir_Block		Dir;								/* Current dir struct	*/
	struct File_Header	File;								/* Current file struct	*/
	struct File_Header	pPrevFile;						/* Previous file struct	*/
};																	/* from previous disk	*/

/***************************************************************************/
/* Function prototypes for all extern functions in  BACKUP.C					*/
/***************************************************************************/

extern int	Backup			( char chSourceDrv, char chDestDrv );
extern long	GetMaxHugeSize	( void );
extern int	CloseFile		( int *iFileHandle, int iStatus );
extern long	GetDiskFree		( int DrvLetter );
extern long	CalcDiskFree	( void );
extern int	InitCpyBuffer	( void );

/***************************************************************************/
/* Function prototypes for all static functions in  BACKUP.C					*/
/***************************************************************************/

static int	DoBackup			( int DiskNum, int IsBackup );
static int	MainBkupLoop	( int iTotalDisks );
static int	InitBkupDisk	( int DiskNum );
static void	IncPath			( void );
static void	DecPath			( void );
static void	InitGlobals		( void );
static void SaveState		( void );
static void RestoreState	( void );
static int	GetNextFile		( void );
static int	IsValidName		( void );
static int	FindFirst		( void );
static int	FindNext			( void );
static void SetPathName		( void );
static void	InitBkupFileName( int DiskNum );
static void	CreatNewDirRec	( void );
static int	AddCtrlHeader	( int DiskNum, int iMode );
static int	AddDirRec		( void );
static int	UpdateDirRec	( int iIsLastDirRec );
static int	AddFileRec		( void );
static int	BuildFileRec	( void );
static void GetFileRec		( void );
static int	ProcessNextFile( void );
static int	CloseOutDisk	( int iLastDisk, int IsLastDisk );
static int	ExtendBkFile	( int DiskNum );
static int	OpenDest			( int *iFile, int DiskNum, char *szFileName,
									  int iAttrib );

/***************************************************************************/
/* Function prototypes for all functions in  BUFFER.C								*/
/***************************************************************************/

extern INT	BufAppend			( VOID FAR *vPtr, UINT uBytes );
extern INT	BufSeekWrite		( VOID *vPtr, UINT uBytes, LONG lOffset );
extern INT	DoWrite				( VOID *vPtr, LONG lOffset, UINT Bytes );
extern INT	DoSeek				( LONG Offset );
extern INT	BufFlush				( VOID );
extern VOID	BufInit				( VOID );
extern INT	BufCopyFile			( char *szPath, LONG lOffset, LONG lBytes );
extern CHAR	FAR *NormalizePtr	( char far *Ptr );

/***************************************************************************/
/* Function prototypes for extern functions found in misc other modules		*/
/***************************************************************************/

extern void		ShowFileStatus( char *szFileName, int ReadWrite );
extern int		GetCurrentFileStr	( char *szStr );
extern void		NoFilesPrompt		( void );
extern int		CalcPrompt			( void );
extern int		PrepBkupDsk			( int DiskNum );
extern void		NotFormattedPrompt( void );
extern int		ProcDiskErr			( int ErrorType );

void	ProgramCleanUp		( int );
void	ProgramAbort		( void );

/***************************************************************************/
/* End of file BACKUP.H																		*/
/***************************************************************************/

