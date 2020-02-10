/***************************************************************************/
/* ADDFILES.C                                                              */
/*                                                                         */
/* Microsoft Confidential                                                  */
/* Copyright (c) Microsoft Corporation 1989-1991                           */
/* All Rights Reserved.                                                    */
/*                                                                         */
/* This module contains the functions which upgrade a floppy disk or       */
/* hard disk system. UpgradeHard() will upgrade a hard disk system and     */
/* return to the caller when complete. UpgradeFloppy() will upgrade a      */
/* floppy disk system and return to the caller when complete.              */
/*                                                                         */
/* Created 11-13-89 - johnhe                                               */
/***************************************************************************/

#include 	<stdio.h>
#include 	<stdlib.h>
#include 	<string.h>
#include 	<dos.h>
#include 	<io.h>
#include 	<direct.h>

#include		<alias.h>
#include		<oem.h>
#include 	<disk_io.h>
#include		<bios_io.h>
#include 	<data.h>
#include		<copy.h>
#include 	<strlib.h>
#include 	<window.h>
#include		<dosonly.h>
#include		<message.h>

static void near	DisplayAdText	(void);

/***************************************************************************/

		/* Executable extensions */
static char 	*szExeExt[] = { "EXE", "COM", "BAT", NULL };

		/* Dblspace.bin file name */
static char 	*szDblSpace	= "X:\\dblspace.bin";

		/* root */
static char		*szRootPath = "\\";

/***************************************************************************/

static void near	MySetTotalBytes( void );
static void SetTotalBytes	( int iStartDisk, int iTotalDsks );
static void MakeHidden		( char Drive );
static void InstallFiles	( int iDisk, int IsHdUpgrade );
static void InitFileStruc	( struct MULT_FILES *FileStruc, int iDisk,
									  int IsHdDisk);
static int	InitFileInfo( char *szName, struct MULT_FILES *File, int iDisk,
								  int IsHdDisk );

static void DeleteConflicts( void );
static void	DeleteDosFile	( char *szFile );
static void CopyCommandCom	( void );
static void CopyFileSet   (int iDiskSet);
static void near CopyDblspaceBin (void);

extern void DispInsertUserDisk  (int UserDisk,char chDrive);
void near   DisplayFloppyExit (void);

/* Uncomment the next line to calculate the total bytes for the gas gauge */
// #define CALC_BYTE_COUNT 1

#ifdef CALC_BYTE_COUNT
void DisplayTotalByteCount (void);
#endif




/***************************************************************************/
/* FLOPPY DISK INSTALLATION																*/
/* 																								*/
/* Main procedure for a creating an emergency boot floppy.  Formats the    */
/* floppy if needed, else zeros it out.  Copies over the boot files, plus  */
/* a few useful utilities.  If the user's floppy is 720k, 1.2m, or 1.44m,  */
/* we copy over a larger set of utilities.  We don't create CONFIG.SYS or  */
/* AUTOEXEC.BAT.                                                           */
/*																									*/
/*	void near InstallFloppy( void )			   										*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*                                                                         */
/* WARNING: Never returns!                                                 */
/*																									*/
/***************************************************************************/

void InstallFloppy( void )
{
   long        cbFree;
   long        cbMin;
   extern long lTotalBytes;
   extern long lByteCount;


	UserDiskScreen();
//
// Init the gas gauge, and set the total-bytes-to-copy, which we have to do
// before calling CopyFileSet().  We don't yet how many bytes we'll be copying,
// but we'll assume 1.2/1.44m set, since this'll be most common.
//
   DisplayGage();
   cbMin = atol(GetDataString(TOTAL_BYTES, FLOPPY_360_INSTALL_BYTES));
   lTotalBytes = (long)(cbMin +
                 atol(GetDataString(TOTAL_BYTES, FLOPPY_120_INSTALL_BYTES)));
   lByteCount = 0L;

//
// At this point, user hasn't inserted their floppy yet, so we don't know
// how large it is.  We'll start with 360k file set.
//
   // Tell Xcopy() to allow 360k disks, as well as larger sizes
	vInfo.uchFloppyType = 1;
   CopyFileSet (DOS360);

//
// Now make sure user's disk is still in drive, and see how much free space
// is left.  Then we'll copy largest set of additional files we can.
//
   DispInsertUserDisk(FIRST_USER_DISK, vInfo.chDestin);
   cbFree = GetDiskFree(vInfo.chDestin);

   if (cbFree > atol(GetDataString(COMPONENTS_BYTES, FLOPPY_120_INSTALL_BYTES)))
   {
      CopyFileSet (DOS120);
   }
   else if (cbFree > atol(GetDataString(COMPONENTS_BYTES, FLOPPY_720_INSTALL_BYTES)))
   {  // Reset gas gauge to 720k file set.
      lTotalBytes = (long)(cbMin +
                    atol(GetDataString(TOTAL_BYTES, FLOPPY_720_INSTALL_BYTES)));
      CopyFileSet (DOS720);
   }

   UpdateGage (99);
	MakeHidden( vInfo.chDestin );				/* Set system file attributes 	*/

	DisplayFloppyExit();
	ProgramCleanUp( RESTORE_SCREEN ); 	/* Restore video & interrupts */
	ProgramExit( 0 );
}


/***************************************************************************/
/* 																								*/
/* Copies a set of files from distribution disk to emergency boot floppy.  */
/*                                                                         */
/*	void CopyFileSet( int iDiskSet )                                        */
/*																									*/
/*	ARGUMENTS:  iDiskSet - section containing list of files to copy         */
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void CopyFileSet (int iDiskSet)
{
	#define					BUF_LEN (sizeof( struct MULT_FILES ) * MAX_PER_DISK)
	register 				i;
	char						*szTmp;
	struct MULT_FILES 	*Files;
	struct MULT_FILES 	*TmpFile;


	Files = GetMemory( BUF_LEN );
   memset( Files, 0, BUF_LEN );

   for (i=0, TmpFile=Files; (szTmp=GetDataString(iDiskSet, i)) != NULL; i++)
   {
      if ( InitFileInfo( szTmp, TmpFile, FIRST_USER_DISK, FALSE ) == OK )
         TmpFile++;
   }
   Xcopy( Files );

	FreeMemory( Files );
}


/***************************************************************************
 * Displays the setup-done screen for floppy-install mode.
 *																									
 * void DisplayFloppyExit (void)
 *																									
 * ARGUMENTS:	void																			
 * RETURNS:		void
 *																									
 ***************************************************************************/
void near DisplayFloppyExit (void)
{
	char			*apszText[MAX_STRINGS];

   if (vInfo.Flag.fFloppyBoot)
      GetMessage (apszText, FD_AUTO_DONE_TEXT);
   else
      GetMessage (apszText, FD_DONE_TEXT);

	WorkAreaCls();
	HelpLine (CONT_HLP);

	PromptWindow( apszText, CR_Response, GetPromptColor(), NULL );
}



/***************************************************************************/
/* HARD DISK INSTALLATION																	*/
/* 																								*/
/* First creates the 2 system files if they don't already exist and a new	*/
/* boot sector. Then creates the DOS directory specified by the user. Next	*/
/* initializes the the screen and disk count and then enters a loop to		*/
/* install all of the disks. Lastly the system files are made hidden and	*/
/* the autoexec and config.sys files are created.									*/
/* 																								*/
/* void InstallHard( void )																*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void InstallHard( void )
{
	int 	iDskNum;
	int	iTotalDsks;
						
	PrepForSysFiles( vInfo.chDestin - 'A' );
	CreateDosPath();

	DisplayGage();
	DeleteConflicts();

	iTotalDsks = GetNumberDisks();
	iDskNum = 0;	/* Use iDskNum temporarily for scratch. */

	MySetTotalBytes();

#ifndef OEMBASE

	/* #4399 - Copy DBLSPACE.BIN (and 'sys') if necessary */
	CopyDblspaceBin();

#endif	// !OEMBASE

	for ( iDskNum = FIRST_USER_DISK; iDskNum < iTotalDsks; iDskNum++ )
	  {
		DisplayAdText();
		InstallFiles( iDskNum, TRUE );	/* Install files to hard disk	*/
	  }

	CopyCommandCom();

	MakeHidden( vInfo.chDestin ); 		/* Set system file attributes	*/
	AutoConfig();
	
	UpdateGage ( 100 );						/* 100% done	*/

#ifdef CALC_BYTE_COUNT
	DisplayTotalByteCount();
#endif
}

/***************************************************************************
 * CompressedDiskIsFull - Called when the compressed disk fills up.
 ***************************************************************************/

void far CompressedDiskIsFull (void)
{
  /* The compressed disk filled up */

  /* Inform the user of their situation, and reboot */
  CompressedDiskFullPrompt();
  RebootSystem();
}

/***************************************************************************/
/* Set the global total byte count by getting the bytes for each disk from	*/
/* the dosdata library function.															*/
/*																									*/
/*	void SetTotalBytes( int iStartDisk, int iTotalDsks ) 							*/
/*																									*/
/*	ARGUMENTS:	iStartDisk	- First disk to be installed							*/
/*					iTotalDisks	- Total number of user disks in the set			*/
/*																									*/
/***************************************************************************/

void SetTotalBytes( int iStartDisk, int iTotalDsks )
{
	register			i;
	extern long		lTotalBytes;
	extern long		lByteCount;
	
	for ( i = iStartDisk; i < iTotalDsks; i++ )
		lTotalBytes += GetDiskBytes( i );

	lByteCount = 0L;
}

/***************************************************************************
 * Set the global total byte count.
 *
 * void MySetTotalBytes( int iStartDisk, int iTotalDsks )
 *
 * ARGUMENTS: None.
 ***************************************************************************/

void near MySetTotalBytes (void)
{
  extern long     lTotalBytes;
  extern long     lByteCount;


  lTotalBytes = atol (GetDataString (TOTAL_BYTES, DOS_INSTALL_BYTES));

  /* Add the amounts for each component */
  if (vInfo.OptComp.BackupChoice    != NO_COMPONENT)
    lTotalBytes += atol (GetDataString (TOTAL_BYTES, BACKUP_COMPONENTS +
                                        vInfo.OptComp.BackupChoice));

  if (vInfo.OptComp.UndeleteChoice  != NO_COMPONENT)
    lTotalBytes += atol (GetDataString (TOTAL_BYTES, UNDELETE_COMPONENTS +
                                        vInfo.OptComp.UndeleteChoice));

  if (vInfo.OptComp.AntiVirusChoice != NO_COMPONENT)
    lTotalBytes += atol (GetDataString (TOTAL_BYTES, ANTI_VIRUS_COMPONENTS +
                                        vInfo.OptComp.AntiVirusChoice));

  // astro Raid #5118 - Bad dossetup.ini file left machine unbootable
  if (lTotalBytes <= 0)
    lTotalBytes = 1;

  lByteCount = 0L;
}


#ifdef CALC_BYTE_COUNT

/***************************************************************************
 * DisplayTotalByteCount - Displays the total number of bytes that the
 *                         Setup program read and wrote.  The value is used
 *                         to make the Gas Gauge accurate.
 *
 * ARGUMENTS:  None.
 * RETURNS:    void.
 ***************************************************************************/

void DisplayTotalByteCount (void)
{
  extern long lByteCount;
  char   szTemp[MAX_SCRN_LINE_LEN];
  static char *apszText[MAX_STRINGS] =
  {
    "",
    "The total number of bytes (read and written) is:",
    NULL,
    ""
    "This is used for the Gas Gauge calculation.",
    NULL
  };

  sprintf (szTemp, "%ld", lByteCount);
  apszText[2] = szTemp;
  PromptWindow (apszText, CR_Response, GetPromptColor(), NULL);
}

#endif


/***************************************************************************/
/* Set the 2 system file attribute to hidden system on the specified 		*/
/* drive.  Also makes Command.com read only.											*/
/* If creating boot-floppy, we also copy DBLSPACE.BIN to it and set it SHR */
/* If dblspace.bin on hard drive, we set it SHR after copy                 */
/* 																								*/
/* void Makehidden( char Drive ) 														*/
/* 																								*/
/* ARGUMENTS:	Drive 	- DOS drive letter to set the file attribs on		*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void MakeHidden( char Drive )
{
	extern char		*SysFile[];
   extern char    *CommandName;
   extern char    *DblSpace;

	SysFile[0][0] = SysFile[1][0] = Drive;
	_dos_setfileattr( SysFile[0], _A_HIDDEN | _A_SYSTEM  | _A_RDONLY );
	_dos_setfileattr( SysFile[1],  _A_HIDDEN | _A_SYSTEM | _A_RDONLY );

   DblSpace[0] = Drive;
	if ( !vInfo.Flag.fHardInstall )
   {
	   _dos_setfileattr( DblSpace, _A_HIDDEN | _A_SYSTEM | _A_RDONLY );
   }
	else if ( 0 == access (DblSpace, 0) )
   {	// astro #4399 prep dblspace.bin on hard drive
	   _dos_setfileattr( DblSpace, _A_HIDDEN | _A_SYSTEM | _A_RDONLY );
   }

   CommandName[0] = Drive;
   _dos_setfileattr( CommandName, _A_RDONLY );
}

/***************************************************************************/
/* Builds a MULT_FILES structure for the specified disk and then calls		*/
/* Xcopy() to install the new files.													*/
/* 																								*/
/* void InstallFiles( int iDisk )														*/
/* 																								*/
/* ARGUMENTS:	iDisk - Distribution disk number to install						*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void InstallFiles( int iDisk, int IsHdUpgrade )
{
	#define					BUF_LEN (sizeof( struct MULT_FILES ) * MAX_PER_DISK)
	struct MULT_FILES 	*Files;

	Files = GetMemory( BUF_LEN );
	memset( Files, 0, BUF_LEN );

	InitFileStruc( Files, iDisk, IsHdUpgrade );

	Xcopy( Files );

	FreeMemory( Files );
}

/***************************************************************************/
/* Initializes a MULT_FILES structure for the specified disk.					*/
/* 																								*/
/* void InitFileStruc( struct MULT_FILES *FileStruc, int iDisk,				*/
/*							  int IsHdUpgrade )												*/
/* void InstallFiles( int iDisk )														*/
/* 																								*/
/* ARGUMENTS:	FileStruc	- Array of MULT_FILES strucs to be filled in 	*/
/* 				iDisk 		- Distribution disk number to be copied			*/
/* 				IsHdUpgrade - Signals doing a hard disk upgrade if TRUE		*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void InitFileStruc( struct MULT_FILES *FileStruc, int iDisk, int IsHdDisk )
{
	register 				i;
	char						*szTmp;
	struct MULT_FILES 	*File;

	for ( i = 0, File = FileStruc;
			(szTmp = GetFileName( iDisk, i )) != NULL;
			i++ )
	{
		if ( InitFileInfo( szTmp, File, iDisk, IsHdDisk ) == OK )
			File++;
	}
}

/***************************************************************************/
/* Initializes a file info structure for the specified file name. The data	*/
/*	base is checked for the file and all translations for this specific 		*/
/*	file are done. 																			*/
/* 																								*/
/* Possible translations and the order in which they are done					*/
/* 																								*/
/* 	Destin file - Don't copy the file            - [no-copy]             */
/* 					  Rename file							- [rename]					*/
/* 					  Use different directory path	- [diff-path]				*/
/* 																								*/
/* 	Source file - Use alternate file					- [diff-file]				*/
/* 					  Get file from alternate disk	- [diff-disk]				*/
/* 																								*/
/* 																								*/
/*	int InitFileInfo( char *szFileName, struct MULT_FILES *FileStruc,			*/
/*							int iIsHdDisk )													*/
/* 																								*/
/* ARGUMENTS:	szFileName	- The name of the file to initialize				*/
/*					FileStruc	- Array of MULT_FILES strucs to be filled in 	*/
/* 				iDisk 		- Distribution disk number to be copied			*/
/* 				iIsHdDisk	- Signals destination is a hard drive				*/
/* RETURNS: 	int			_ OK if file gets copy else ERROR for no copy	*/
/* 																								*/
/***************************************************************************/

int InitFileInfo( char *szName, struct MULT_FILES *File, int iDisk,
						int IsHdDisk )
{
											/* Determine destination information		*/

	if ( (File->Name.Destin = GetRealDestName( szName )) == NULL )
		return( ERROR );				/* Don't copy was specifed for this file  */

	File->Path.Destin = GetRealDestPath( File->Name.Destin,
													 vInfo.szPath + 2 );
	File->UserDisk = IsHdDisk ? NOT_REMOVEABLE : iDisk;/* User's disk			*/
	File->Drive.Destin = vInfo.chDestin;					/* Set drive letter	*/

														/* Determine source information	*/
	if ( (File->Name.Source = GetRealSrcName( szName )) == NULL )
		return( ERROR );

	File->Path.Source = "\\";
																		/* Distrib disk		*/
	File->DiskNumber = GetRealSrcDisk( File->Name.Source, iDisk );
	File->Drive.Source = vInfo.chSource;					/* Set drive letter	*/

	return( OK );
}

/***************************************************************************/
/* Deletes any existing files in the DOS directory which have a name			*/
/* conflict with the new DOS files.														*/
/* 																								*/
/* int DeleteConflicts( void )															*/
/* 																								*/
/* ARGUMENTS:	void																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void DeleteConflicts( void )
{
	register 	i; 						/* Loop and array index 					*/
	register 	iDisk;					/* Current distribution disk				*/
	int			iTotalDsks;				/* Total number of user disks				*/
	char			*szFile; 				/* Current file name 						*/

							/* First make sure there are some files in the dir 	*/
	if ( !IsDirEmpty( vInfo.szPath ) )
	{
		iTotalDsks = GetNumberDisks();
		for ( iDisk = FIRST_USER_DISK; iDisk < iTotalDsks; iDisk++ )
			for ( i = 0; (szFile = GetFileName( iDisk, i )) != NULL; i++ )
				if ( (szFile = GetRealDestName( szFile )) != NULL )
					DeleteDosFile( szFile );
	}
}

/***************************************************************************/
/* Deletes the specified file in the DOS directory. If the file has an		*/
/* executable extension the delete is repeated for all 3 possible				*/
/* executable file extensions (.exe, .com, .bat).									*/
/* 																								*/
/* int DeleteDosFile( char *szFile ) 													*/
/* 																								*/
/* ARGUMENTS:	szFile	- Ptr to  name for file to be moved 					*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void DeleteDosFile( char *szFile )
{
	char			*szFullPath;
	register 	iFileCount;
	register 	i;
	int			IsExeFile;


	IsExeFile = (FindExtMatch( szFile, szExeExt ) != -1 ? TRUE: FALSE);
	szFullPath = GetMemory( MAX_PATH_LEN );	/* Get memory for file path	*/

	BuildPath( szFullPath, vInfo.szPath[0], vInfo.szPath + 2, szFile );

	for ( i = iFileCount = 0; i < 3; i++ )
	{
		if ( IsExeFile )
			strcpy( strchr( ParseFileName( szFullPath ), '.' ) + 1, szExeExt[i] );

		if ( FileExists( szFullPath ) ) 			/* Make sure file exists		*/
		{
			DisplayFileStatus( szFile, DELETE );
			remove( szFullPath );
		}
		if ( IsExeFile == FALSE )					/* If file is not executable	*/
			break;										/* break out of for loop		*/
	}

	FreeMemory( szFullPath );
}

/***************************************************************************/
/* Copies the command.com file from the root dir into the DOS dir.			*/
/* 																								*/
/* void CopyCommandCom( void )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/***************************************************************************/

void CopyCommandCom( void )
{
	struct MULT_FILES 	File[2];
	static char				*szFileName = "COMMAND.COM";

	memset( File, 0, sizeof( struct MULT_FILES ) * 2 );

	File[ 0 ].Name.Destin = File[ 0 ].Name.Source = szFileName;
	File[ 0 ].Path.Destin = vInfo.szPath + 2;
	File[ 0 ].Path.Source = szRootPath;
	File[ 0 ].UserDisk = File[ 0 ].DiskNumber = NOT_REMOVEABLE;
	File[ 0 ].Drive.Destin = File[ 0 ].Drive.Source = vInfo.chDestin;

	Xcopy( File );
}	


/***************************************************************************/
/* Displays the advertising text .                                         */
/*                                                                         */
/*  void near DisplayAdText(void)                                          */
/*                                                                         */
/*  ARGUMENTS:  Number of the disk ready to be copied.                     */
/*  RETURNS:    void                                                   		*/
/*                                                                         */
/***************************************************************************/

void near DisplayAdText (void)
{
	static iScreen = 0;  /* Index for which banner "screen" to display */

	/* Clear the banner area of the screen */
	VideoScrollDn (TITLE_ROW, 0, TITLE_ROW + MAX_BANNER_LINES,
						VideoGetWidth() - 1, 0, GetBackGroundColor());

	/* Display the banner's text */
	DisplayText (ppszBanners[iScreen++], 4);

	if (ppszBanners[iScreen][0] == NULL)
		iScreen = 0;
}


#ifndef OEMBASE
/***************************************************************************
 * CopyDblspaceBin - Copies DBLSPACE.BIN when in use, or found on root.
 * 						And if the boot drive is swapped, IO.SYS, COMMAND.COM
 *                   and MSDOS.SYS are also copied to the root of the
 * 						boot drive.
 *                   Also makes the files system-hidden-readonly.
 *
 * ARGUMENTS:  None.
 * RETURNS:    Void.
 *
 ***************************************************************************/

#define MAX_DBLSPACE_FILES_LIST   5

void near CopyDblspaceBin (void)
{
  int fDblspaceBinFound = FALSE;  /* TRUE if DBLSPACE.BIN found   */
  int i;                          /* Looping variable             */
                                  /* Xcopy() structure            */
  struct MULT_FILES File[MAX_DBLSPACE_FILES_LIST];
  char   **pszCopyNames;          /* Points to names to copy      */

#ifdef UJANUS
  static char *szSysFileNames[]    = { "IO.SYS", "MSDOS.SYS", "COMMAND.COM",
                                     "DBLSPACE.BIN", NULL };
#else
  // astro raid #5125 - OEM names should not be hardcoded
  static char *szSysFileNames[]    = { BIOS_FILE, MSDOS_FILE,
													"COMMAND.COM", "DBLSPACE.BIN", NULL };
#endif


  /* Does DBLSPACE.BIN exist on the HOST boot drive */
  *szDblSpace = vInfo.chHdHostDrv;
  fDblspaceBinFound = !access (szDblSpace, 0);

  /* Is there anything we need to do */
  if (vInfo.Flag2.fDoubleSpace == FALSE && fDblspaceBinFound == FALSE)
    return;


  /* Special case if DBLSPACE.BIN is found at root,
   *   or DoubleSpace driver active in memory,
   *   but we are in a non-swapped case, only copy dblspace.bin
   */
  if (vInfo.Flag2.fDualInstall == FALSE)
    pszCopyNames      = &szSysFileNames[3];
  else
    pszCopyNames      = szSysFileNames;


  /****************************************************/
  /* Install the new files from the distribution disk */
  /****************************************************/

  /* Clear out the Xcopy() structure */
  memset (File, 0, sizeof (struct MULT_FILES) * MAX_DBLSPACE_FILES_LIST);

  /* Name each of the files to be copied */
  for (i = 0; pszCopyNames[i] != NULL; i++)
    {
      /* Source drive, path, and filename */
      File[i].Drive.Source = vInfo.chSource;
      File[i].Path.Source  = vInfo.szSource + 2;
      File[i].Name.Source  = GetRealSrcName (pszCopyNames[i]);

      /* Return if there was no real source name */
      if (File[i].Name.Source == NULL)
        return;


      /* Destination drive, path, and filename */
      File[i].Drive.Destin = vInfo.chHdHostDrv;
      File[i].Path.Destin  = szRootPath;
      File[i].Name.Destin  = GetRealDestName (pszCopyNames[i]);

      /* Return if there was no real destination name */
      if (File[i].Name.Destin == NULL)
        return;


      /* Set up source and destination disk "names" */
      File[i].UserDisk     = NOT_REMOVEABLE;

      if (vInfo.chSource >= vInfo.chFirstHd)
        File[i].DiskNumber = NOT_REMOVEABLE;
      else
        File[i].DiskNumber = GetRealSrcDisk (File[i].Name.Source,
                                             FIRST_USER_DISK);
    }

  /* Perform the copy */
  Xcopy (File);

  /* Set the file attributes */
  MakeHidden (vInfo.chHdHostDrv);
}
#endif	// !OEMBASE

