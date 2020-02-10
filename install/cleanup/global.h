/***************************************************************************/
/*	GLOBAL.H																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/***************************************************************************/


/* "Strings" */

#define	COMMAND_COM			(apszGlobal[ 0 ])
#define	COMMAND_PROCESSOR	(apszGlobal[ 1 ])
#define	NEXT_SCREEN			(apszGlobal[ 2 ])
#define	PREVIOUS_SCREEN	(apszGlobal[ 3 ])
#define	UPGRADE_STR			(apszGlobal[ 4 ])


#define	RESTORE_ARG			"/RESTORE"
#define	UPGRADE_ARG			"/UPGRADE" 
#define	MONO_ARG				"/B"
#define	HELP_ARG				"/?"


#define	RESTORE_SCREEN 	1
#define	NET_STRING_SIZE	20
#define	FILE_SIZE			12		
#define	LABEL_SIZE			11
#define	VERSION_SIZE		5
#define	MAX_PATH_SIZE		67
#define	INF_ARRAY_SIZE		20
#define	MAX_NETWORK_TYPES	20
#define	MAX_TARGET_FILES	20
#define	MAX_SOURCE_NAMES	MAX_TARGET_FILES*2
#define	ID_STR_LEN			20
#define	MAX_DISTRIB_DISK	1
#define	MAX_REQUEST			20		
#define	DO_COM				0
#define	DO_NET				1
#define	FROM_END				2		
#define	FROM_FRONT			0
#define	SEEK_ERROR			-1l
#define	NO_COMPRESSION		0
#define	SPACE					32
#define	SECTOR_SIZE			512
#define	NOT_REMOVEABLE 	16
#define	IS_DIR 				(char ) 0x10
#define	RENAME				3
#define	COM_MASK				0x01
#define	NET_MASK				0x02
#define	NUM_MESSAGES		8
#define	MESSAGE_SIZE		8
#define	MAX_H_DRIVES		24
#define	SIGNATURE			"GBW614"
#define	UPGRADE				0
#define	RESTORE				1
#define	CLEANUP				2

		/*Fatal Error Codes.*/
#define	AUTO_EXEC_ERR		7
#define	LIE_TABLE_ERR		6
#define	CORRUPT_INF_FILE	5
#define	INF_READ_ERR		4
#define	TOO_MANY_FILES		2


#ifndef		TRUE
	#define	TRUE	1
#endif

#ifndef		FALSE
	#define	FALSE	0
#endif

#ifndef    OK
   #define		OK			      0
#endif
#ifndef	EOL
	#define	EOL		0
#endif

#ifndef	_A_ALLFILES
	#define	_A_ALLFILES			_A_SUBDIR | _A_HIDDEN | _A_SYSTEM
#endif

#ifndef	UCHAR
	#define	UCHAR	unsigned char
#endif
#ifndef UL
	#define	UL unsigned long
#endif
#ifndef    ABORT
   #define		ABORT			   -1
#endif
#ifndef    ERROR
   #define		ERROR			   -1
#endif

#ifndef    ESC
	#define	  ESC				  	0x1b
#endif

#ifndef    PREVIOUS
	#define	  PREVIOUS			-2
#endif




#define	MAX(x,y)	((x) > (y)) ? (x) : (y)
#define	MIN(x,y)	((x) > (y)) ? (y) : (x)
#define	NetworkUpgrade() ( toupper (*FilesPath) > 'B')

  									 
typedef struct {
	char		*NetString;
	int		SourceIndex;				/*Index to old RDR name.*/
	char		Target[FILE_SIZE+1];		/*New RDR name.*/
	char		Distrib[FILE_SIZE+1];
	long		Length;						/*Decompressed length of target file.*/
	char		IDString[ID_STR_LEN+1];
	char		IDString2[ID_STR_LEN+1];
	char		IDString3[ID_STR_LEN+1];
	char		IDString4[ID_STR_LEN+1];
	int		Message;
	char		Version[VERSION_SIZE+1];								
	char		DistribDisk;  /*Number of disk with new RDR on it.*/
} INF_ENTRY;



typedef struct {
	char	Path[MAX_PATH_SIZE+1];
	int	Index;
	char	Update;	  /*Flag set by user to determine whether or not to update.*/
} TARGET_ENTRY;
	


typedef struct {
	char		Signature[7];
	char		Extension[4];
	char		NewFile[FILE_SIZE + 1];
	unsigned	Time;
	unsigned	Date;
} FILE_PACKET;


struct INFO	
{
	struct ARGS
	{
		unsigned		fIsMono		:1;		/* Force to run in mono colors /B	*/
		unsigned		fDoneBackup	:1;		/* Program returned from backup /D	*/
		unsigned		fFloppy		:1;		/* Force a floppy upgrade		 /F	*/
		unsigned		fHardDisk	:1;		/* Is hard disk upgrade			 /H	*/
		unsigned		fNewSysFiles:1;		/* New autoexec & config.sys	 /N	*/
		unsigned		fHelp			:1;		/* Help switch 					 /?	*/
	} Args;
}	vInfo;




	/*Globals variables used by all modules.*/


INF_ENTRY		INFData [MAX_NETWORK_TYPES+1];
TARGET_ENTRY	TargetStack [MAX_TARGET_FILES];
char				SourceFiles [MAX_SOURCE_NAMES][FILE_SIZE + 1];
char				DistribDisks [MAX_DISTRIB_DISK][LABEL_SIZE +1];
int				NumTargets;
int				IsRestore;		
char				*ComPath;		/*Points to path with valid COMMAND.COM*/
char				*FilesPath;		/*Location of redirector files.*/
char				*GlobalBuf;
unsigned			GlobalBufSize;	/*Must be an even number.*/
char far			*ErrorBuffer;
char				*HelpText[NUM_MESSAGES][MESSAGE_SIZE+1];

char				*apszGlobal[];

	/*Globals defined in COMMON.LIB (\PROMPTS\GAGE.C)*/

extern	long	lByteCount;
extern	long	lTotalBytes;



	/*Functions defined in CLEANUP.C*/

extern	int	DoCleanUp ( void );
extern	void 	CleanUpFile ( char *path );
extern	void 	RemOldDos ( char *path );


	/*Function defined in FINDSTR.C*/

extern	int MultStrMatch( char *szPath, char *apszText[4][MAX_NETWORK_TYPES]);



	/*GETCHAR.C */

extern	int	GetChar ( void );


	/* INITIAL.C */

extern	int  DriveReady(char chDrive);
extern	void ProgramCleanUp( int RestoreScreen );
extern	void ProgramInitialize(void );


	/* INTRFACE.C  */

extern	void 	DispInsertDisk ( int DiskNum );
extern	char 	DoComNetPrompt ( void );
extern	int 	DoDiskSearch ( char searchflag );
extern	void	DoDone ( void );
extern	void	DoIntro ( void );
extern	void 	DoMessages ( void );
extern	void 	DoNoFiles ( void );
extern	void 	DoNonUpgradable ( void );
extern	void 	DoSearchWindow ( void );
extern	void 	DoDirectoryWindow ( int IfShow );
extern	int 	NumNonUpgradable ( void );
extern	void 	ShowSearchStatus ( char *path );
extern	void 	UserSelect ( int start, int end );


	/*MEMORY.C  */

extern	void	ProgramAbort( void );


	/* MENU.C  */ 

extern	int 	MyPromptSelect( char *szStrings[], char *szOpts[], int optdflt );

					

	/*Functions defined in NETSET.C.*/

extern	void 	DoDriveSearch ( int restore, char searchflag );
extern	void 	FixAutoExec ( char drive );
extern	int 	MakeSourceEntry ( char *sourcename );
extern	void 	ReadCommandCom ( char *path );
extern	void	ReadINF ( char *path );
extern	void 	UpdateVersionTable ( void );


	/*RESTORE.C*/

void 	RestoreFile ( char *path, char *buffer );


	/*Functions defined in UPDATE.C.*/

extern	void	MakeTargetEntry ( int index, char *path );
extern	void 	ReplaceFiles ( int start, int end);
extern	void 	UpdateStack ( void );
extern	int 	UpgradeStart ( void );



	/*Functions defined in UTILITY.C*/

extern	void 	AddPath (char *path, char *extension);
extern	void 	AddExtension ( char *path, char *extension );
extern	int 	CheckSum ( char *path, int *checksum );
extern	int 	CheckSumBuffer ( int *buffer, unsigned size );
extern	char 	*FileExtension ( char *path );
extern	long 	FileSize ( char *path );
extern	int 	FileNameCompare ( char *file1, char *file2 );
extern	void 	GetDrvList ( char *DrvList );
extern	int 	GetPacket ( FILE_PACKET *apacket, char *path );
extern	char 	*MakeBufCString ( char **buffer );
extern	void 	MakeOptionString ( char *target, char *source, int upgrade );
extern	void 	MyPuts (char *str);
extern	void	RemovePath (char *path);
extern	void 	RemoveDir ( char *path );



	/*Functions defined in WINDOW.C*/

extern	void	DisplayDirStatus( char *szFileName, int Type );


