/***************************************************************************
 *
 * INSTALL.H																						
 *
 *		Copyright (c) 1991 - Microsoft Corp.											
 *		All rights reserved.																	
 *		Microsoft Confidential																
 *
 * Installation status structure
 *
 * History:	M200: Created.
 *
 ***************************************************************************/

/* XLATOFF */					/* M200: Disable translation by H2INC */

/***************************************************************************/

#pragma pack(1)				/* Pack all structures */

/***************************************************************************/

/* XLATON */					/* M200: Enable translation by H2INC */

/***************************************************************************/

/* Global structure for storing installation information. A copy of this
 * structure is stored in resident data, so that it can be passed from DOS
 * Setup to Win Setup.
 */

#define SIG_LEN 12


/* This string length constant has been made large enough to account for
 * drive letter, directory name, file name, terminating null, plus some pad
 * on both FAT and Installable File Systems (IFS), which may implement
 * long file names.
 */
#define MAXSTRLEN  260
#define MAXLINELEN  80

typedef struct _BDONE { /* done */		/* Progress Bit Flags */
	unsigned		fResData	:1;				/* TRUE if Install data is resident */
	unsigned		fSrcPath	:1;				/* TRUE if Source path set */
	unsigned		fNet		:1;				/* TRUE if Network detection done */
	unsigned		fBackup	:1;				/* TRUE if HDBKUP done */
	unsigned		fDosPath	:1;				/* TRUE if Dos path set */
	unsigned		fWinInf	:1;				/* TRUE if Win SETUP.INF filespec set */
	unsigned		fDisplay	:1;				/* TRUE if Display set */
	unsigned		fDosSetup:1;				/* TRUE if MS-DOS install complete */
	unsigned		fWinSetup:1;				/* TRUE if Win Setup done (may have failed). */
} BDONE;

typedef struct _BFLAGS { /* flag */		/* General purpose Bit Flags */
	unsigned		fAdmin   		:1;		/* TRUE if is Administrative Setup */
	unsigned		fDosFirst		:1;		/* TRUE if must do Dos Setup before Win Setup. */
	unsigned		fUninstall		:1;		/* TRUE if must do Uninstall procedure */
	unsigned		fINT2FChained	:1;		/* TRUE if must leave INT 2F hook around. */
	unsigned		fINT2FOrphan	:1;		/* TRUE if we are INT 2F orphan (chain only) */
	unsigned		fMaintenance	:1;		/* TRUE if in Maintenance Mode */
} BFLAGS;


typedef struct _INSTALL { /* inst */
	unsigned char	Software;				 	 	/* Software to install:
											 		 	  	 * DOS-WIN, DOS-ONLY, WIN-ONLY
											 		  	  	 */
#ifndef RECOVERY_PROGRAM
	char				szSignature[SIG_LEN]; 	 	/* Identifying signature */
	unsigned char	Method;					 	 	/* Method of install: Express,Custom */
   unsigned int   wHwndWinSetup;             /* WinSetup windows handle to which dossetup */
                                             /* posts it's "finished" message */
	char				szCmdLine[MAXSTRLEN]; 		/* Original Setup command line */
	char				szWinCmdLine[MAXSTRLEN]; 	/* Win Setup command line */
	char				szWinInf[MAXSTRLEN]; 	 	/* Location of Win SETUP.INF */
	char				szCountryCode[MAXLINELEN];	/* Windows country code */
	char				szCountryLang[MAXLINELEN];	/* Windows country language */
	char				szKeyLayout[MAXLINELEN]; 	/* User specified Keyboard Layout */
	char				szLanguage[MAXLINELEN];	 	/* User specified language */
	BDONE				Done;							 	/* Progress flags */
	BFLAGS			Flags;						 	/* Status flags */
#endif
} INSTALL;


/* Multiplex Interrupt */

#define INT2F					0x2F			/* DOS multiplex interrupt */

#define INT2F_INSTALL_FMT	0x4900		/* Int 2Fh, function 4900h dedicated
													 * for Setup format operation.
													 * (See OEM\AUTOFMT.C)
													 */

#define INT2F_INSTALL_DATA	0x4910		/* Int 2Fh, function 4910h dedicated
													 * for Setup data transfer.
													 * (See COMMON\RESIDENT.ASM)
													 */

/*  WM_USER = 0x400 from windows.h This is the private message that is posted
 *  from Dossetup to Winsetup so that dossetup can communicate the fact that
 *  he's completed.
 */

#define UM_CHILD_TERMINATED 0x0400 + 1

/***************************************************************************/

/* XLATOFF */					/* M200: Disable translation by H2INC */

/***************************************************************************/

extern INSTALL Install;

/* Install.Signature */

#define SIGNATURE "$DOSWIN$"

/* Install.Software values */

enum software { SOFTWARE_DOSWIN, SOFTWARE_WINONLY, SOFTWARE_DOSONLY };

/* Install.Method values */

enum method { METHOD_EXPRESS, METHOD_CUSTOM, METHOD_BATCH };


/* WINGROUP.C */

#define MAX_PROGMAN_INI_LINE_LEN       MAX_PATH_LEN + 10
#define MAX_SYSTEM_INI_LINE_LEN        MAX_PATH_LEN + 30
#define MAX_PROGRAM_MANAGER_GROUPS     40
#define MAX_INF_LINE_LEN               150  /* Max length of any .inf line */


extern char *gszGroup;
extern char *gszGroupsSection;
extern char *gszGroupTemplate;
extern char *gszSystemIni;
extern char *gszSystemTmp;
extern char *gszSystemBak;
extern char *gszProgmanIni;
extern char *gszProgmanTmp;
extern char *gszProgmanBak;
extern char *gszWinfileIni;
extern char *gszWinfileTmp;
extern char *gszWinfileBak;
extern char *gszWinCom;

int DeleteFile (char *szPathname);
void mycatpath (char *path, char *sz);
void myfnTruncFN (char *szPathStr);
int myfnSearchForOldWin3 (char *szWinPath, char *szFile1, char *szFile2,
                          char *szFile3);
int SetWinPath (void);
int MyFindSection (char *szSection, int iFileIn, int iFileOut);
void DosUnreadLine (char *szBuffer);
int DosReadLine (char *szBuffer, int iLength, int iFile);
int DosWriteLine (char *szBuffer, int iFile);


/* resident.asm */

extern void far ResExec 			(char *szFile, char *szCmdLine);
extern void far InitNewInt2Fh		(void);
extern void far RestoreOldInt2Fh	(void);

/* Declarations for obtaining pointer to resident data */

extern char far * DosInstallData( void );
extern INSTALL far *lpInstall;
