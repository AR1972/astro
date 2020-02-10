/***************************************************************************/
/*                                                                         */
/* UPGRADE.H																					*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Function prototypes and #defines for retail DOS 5.0 install program		*/
/*																									*/
/* Created 11-13-89 - johnhe																*/
/***************************************************************************/

#define	IADF_SUPPORT			1

#define	HD_BOOT_DRV				'C'
#define	REBOOT					1

#define	DO_MOVE					0
#define	DO_DELETE				1
#define	DO_COUNT 				2

#define	ASTid						0x14
#define	ATNTid					0x56
#define	TANDYid					0xe5
#define	UNISYSid					0xf2
#define	L_EDGEid					0x11
#define	NECid						0x24
#define	COMMODOREid				0x08

#define	DM_DRIVERid1			0x50
#define	DM_DRIVERid2			0x51

#define	AST_8_ENT_SIG			0x0A55A			/* Sig for AST 8 partitions*/
#define	AST_SIG_OFFSET			0x17c				/* Offset of AST signature	*/
#define	AST_TABLE_OFFSET		0x17e				/* Offset of AST part table*/

#define	IADF_SIG					0x5a5a
#define	IADF_SIG_OFFSET		0x1bc

#define	MULTI_DOS_PARTS		1
#define	MULTI_FLOPPY			2
#define	BAD_CONFIG_FILE		3

#define	HD_READ_ERROR			10
#define	TOO_MANY_PARTS			11
#define	NO_BOOT_LETTER			12
#define	FUNNY_PARTITION		13
#define	ROOT_FULL				14
#define	DISK_FULL				15
#define	NOT_COMPAT_PART		16

#define	PRIAM_DETECTED			17
#define	VFEATURE_DETECTED		18
#define	SYQUEST_DETECTED		19
#define	IADF_DETECTED			20
#define	EVEREX_DETECTED		21
#define	SPEEDSTOR_DETECTED	22
#define	DISKMAN_DETECTED		23

#define	NOT_HDISK				FALSE

#define	BAD_PART_LIST { 0x50, 0x51,\
								 0x61, 0x63, 0x64, 0x66,\
								 0x71, 0x73, 0x74, 0x76,\
								 0xe1, 0xe3, 0xe4, 0xe6,\
							    0xf1, 0xf3, 0xf4, 0xf6,\
								 0x21, 0x23, 0x24, 0x26,\
								 0x31, 0x33, 0x34, 0x36,\
								 0xa1, 0xa3, 0xa4, 0xa6,\
								 0xb1, 0xb3, 0xb4, 0xb6 }
								 
															
/***************************************************************************/

extern char chArgSwitch[];

/***************************************************************************/

/* UPGRADE.C */

extern void 	UpgradeFloppy	( void );
extern void 	UpgradeHard 	( int iNullFile );
extern void 	SpecialIds		( void );
extern int		FileExists		( char *szFile );
extern int		MoveFiles		( int Type );
extern void 	MakeHidden		( char Drive );
/* FDUPGRAD.C */

extern  void FloppyDiskUpgrade(void );
extern  void FloppyEndPrompt(void );
extern  void GetVideoType( void );

/* FINDOEM.C */

extern void	SaveVenderInfo		( int iVender );
extern void	BestPathGuess		( void );
extern int	BestVenderGuess	( void );
extern void	BuildComspec		( void );
extern int	FindDosFiles		( char *szPath );

/* HDUPGRAD.C */
extern  void HardDiskUpGrade	( void );
extern  void far TooManyPartitions( int Number,int Drive );

/* INITIAL.C */
extern void	ProgramInitialize	(void );
extern int	RoomForSystem		( char chDrv );
extern void	ExpandRupDir		( int iTotalFiles );
extern void CreatUniqueName	( char chDrive, char *szPath, char *szName );
extern void	CreatTmpTwo			( void );
extern void DeleteTmpTwo		( void );


/* SETUP.C */
extern void CreateRecoveryFloppy( void );
extern void GetHdBpb			( struct BPB *Bpb );
extern void	CopyBuf			( char *szFile, void *pBuf, UINT uBytes );

/* SETVER.C */

extern void	UpdateLie		( void );

/* AUTO.C */
extern void	ProcessAuto		( void );
extern void MoveAutoConfig( void );

/* CONFIG.C */
extern void ProcessConfig	( void );

/* AUTOCONF.C */
extern char	*DefaultShellLine( char *szPtr );

/* HARDDISK.C */
extern int	HardDiskCheck	( void );
extern void GetSysFiles		( void );
extern int	GetPartEntries	( void );
extern void	WriteNewBoot	( int IsFinal );
extern unsigned RdWrSector	( int iDrv, int iHead, int iTrack, int iSector,
									  int RdWr );

/* FIXBOOT.C */
extern void	FixupBootRecs	( void );
extern int	GetPartEntries	( void );
extern int	IsValidPartTable( void );
extern int	IsCompatPart	( UCHAR SystemIndicator );

extern struct Part *FindExtPartEntry	( int Entries );
extern struct Part *GetPartTable		( int Entries );

/* Burnoulli */
extern int far IOMINIT			( void );
extern int far	IOMREM			( void );
