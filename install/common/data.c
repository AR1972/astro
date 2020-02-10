/***************************************************************************/
/* 																								*/
/* DATA.C																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Functions for accessing the data in DOSDATA.DAT. The functions 			*/
/* InitDosData() must be called before any other functions in this			*/
/* module are called.																		*/
/*																									*/
/* Created 11-11-89 - johnhe																*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include 	<string.h>
#include		<dos.h>
#include 	<fcntl.h>
#include 	<io.h>
#include		<share.h>
#include 	<sys\\types.h>
#include 	<sys\\stat.h>

#include		<alias.h>
#include 	<disk_io.h>
#include 	<data.h>
#include 	<strlib.h>
#include	<window.h>
#include	<dosonly.h>
#include	<install.h>
#include 	<message.h>

#ifdef	OEM_PROGRAM
#include		<oem.h>
#else
#include		<global.h>
#endif

char ***ppszBanners = NULL;

void DosUnreadLine (char *szBuffer);
int DosReadChar (int iFile);
void DosUnreadChar (char ch);

/***************************************************************************/

#define	USIZE					sizeof( unsigned )	/* Size of unsigne value	*/
#define	RELOC_LEN			(1024 * 2)				/* Size of relocate buffer */

/***************************************************************************/

							/* Class is an array of ptrs to the different labels	*/
							/* indentifing a specific class in dosdata.dat			*/

static char *Class[] = {		"[lie-to]", "[rename]", "[delete]",
										"[device]", "[drivparm]", "[rem-device]",
										"[no-install]", "[dif-file]",

											/* Start of nonOEM sepecific data classes	*/

										"[dif-disk]", "[bios]",

											/* Distribution disk label strings */
										"[dist_label]",

											/* User disk label strings */
										"[user_label]",

											/* Compression ratio values */
										"[compression-ratio]",

											/* Distribution disk prompts */
										"[user_prompt]", "[prompt]",

										"[dif-path]", "[global-rename]", "[upd-device]",
										"[video-list]", "[video-drv]", "[disk-bytes]",
										"[disk-type]",	"[video-grb]", "[netfiles]",
										"[drv-vers]", "[del-driver]",
										"[component-bytes]", "[total-bytes]",

										"[backup-windos]",
										"[backup-win]",
										"[backup-dos]",
										"[undelete-windos]",
										"[undelete-win]",
										"[undelete-dos]",
                              "[optcomp-undelete]",
										"[antivirus-windos]",
										"[antivirus-win]",
										"[antivirus-dos]",
                              "[optcomp-antivirus]",

                                 /* Emergency-floppy layouts */
                              "[dos360]", "[dos720]", "[dos120]",

                                 /* 3rd party disk caching */
                              "[caches]",

                                 /* Utilities that won't run with smartdrv */
                              "[conflicts-smartdrv]",

                                 /* Drivers we can't add files before */
                              "[LoadAfter]",

											/* Distribution disk file layouts */
										"[distr-0]", "[distr-1]", "[distr-2]",
										"[distr-3]", "[distr-4]", "[distr-5]",
										"[distr-6]", "[distr-7]", "[distr-8]",
                              "[distr-9]", "[distr-10]","[distr-11]",
                              "[distr-12]","[distr-13]","[distr-14]",

											/* User disk file layouts */
										"[disk-0]", "[disk-1]", "[disk-2]",
										"[disk-3]", "[disk-4]", "[disk-5]",
										"[disk-6]", "[disk-7]", "[disk-8]",
										"[disk-9]",

										"OEMTABLE" };

							/* Data is an array of ptrs to the arrays of strings	*/
							/* for each of the different classes listed by label	*/
							/*	above. To access a specific string in one of the	*/
							/* class use the method:										*/
							/*			szStr = *(Data[ ENUM_CLASS ] + Index)			*/
							/* Where Index is the string in the array you need.	*/

static char			**Data[ END_CLASS ];

static char			*szDosDataFile;

static char			*szUpgrade = "INSTALL"; 	/* Label for upgrade data		*/
static char 		*szSearch  = "OEMTABLE";	/* Label for search data		*/

static char			*pchStringBuf;		/* Flat buffer to hold the data text	*/
static struct DDR	*OemList;			/* Array of DDR structures					*/

static unsigned	MaxOem;				/* Number of DDR structs in OEM list	*/
static unsigned	StrListLen; 		/* Number of bytes in string buffer 	*/
static long			lDataOffset;		/* Ptr offset in file to data area		*/

static unsigned	*RelocBuf;			/* Ptr to buffer for relocating data	*/

static int			iDiskType;			/* Distribution diskette type 1=360K,	*/
												/* 2 = 720K, 3 = 1.2meg, 4 = 1.44meg	*/

static unsigned	ShareAccess;



/***************************************************************************/
/* Initializes the data from the DOSDATA file. Allocates buffers and data	*/
/* lengths and then reads in the OEM list and the string buffer and then	*/
/* initializes the disk labels and list of files from each disk.				*/
/* 																								*/
/* void InitDosData( char *szFile )														*/
/* 																								*/
/* ARGUMENTS:	szFile	- Ptr to complete dosdata.dat file						*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void InitDosData( char *szFile )
{
	register 	i;
	char			*szTmp;
	int			iFile;
	struct DDR	*Oem;

	if ( _osmajor < 3  || (_osmajor == 3 && _osminor < 10) )
		ShareAccess = O_RDONLY;
	else
		ShareAccess = SH_DENYWR;

										/* Start with NULL ptr for all data classes	*/
	szDosDataFile = szFile;
	for ( i = 0; i < END_CLASS; i++ )
		Data[ i ] = NULL;

	RelocBuf = GetMemory( RELOC_LEN );		/* Allocate a relocation buffer	*/

										/* Allocate buffers for and read in the list */
										/* of OEM structures and the data area which */
										/* contains all of the data strings 			*/

	if ( _dos_open( szDosDataFile, ShareAccess, &iFile	) == OK )
	{
		MaxOem = GetNextDataBlock( iFile, &OemList ) / sizeof( struct DDR );
		StrListLen = GetNextDataBlock( iFile, &pchStringBuf );

															/* Save start of OEM data area */

		lDataOffset = _dos_seek( iFile, 0L, SEEK_CUR ) + (long)USIZE;
		_dos_close( iFile );
														/* Load non-OEM specific data */
		if ( (Oem = GetOemRecord( szUpgrade, 0, 0 )) == NULL )
			FatalError( CORRUPT_DATA_ERROR );

		LoadOemData( Oem );
	}
	else
		FatalError( FATAL_DATA_READ_ERROR );

	szTmp = GetDataString( DISK_TYPE, 0 );
	if ( szTmp == NULL )
		FatalError( FATAL_DATA_READ_ERROR );
	else
		iDiskType = atoi( szTmp );
}


#if defined (UPGRADE_PROGRAM) || defined (OEM_PROGRAM)
/***************************************************************************/
/* Reads and stores the advertising banner strings in the DOSMSG file.     */
/* 																								*/
/* void InitBannerData (char *szFile)												   */
/* 																								*/
/* ARGUMENTS:	szFile	- Ptr to SETUP.MSG file                            */
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/
void InitBannerData( char *szFile )
{
	register 	i, u;         /* Index variables               */
	char		*szBuffer;    /* Input line                    */
	int			iFile;        /* DOS file handle               */
   char *aszDefault[DEFAULT_BANNER_LINES];


   GetMessage (aszDefault, DEFAULT_BANNER_TEXT);
   ppszBanners = GetMemory (MAX_BANNER_SCREENS * sizeof (char **));

	if (_osmajor < 3  || (_osmajor == 3 && _osminor < 10))
      ShareAccess = O_RDONLY;
	else
	  ShareAccess = SH_DENYWR;

	if (_dos_open (szFile, ShareAccess, &iFile) != OK)
      {
        /* If the banner file isn't there, indiciate that there */
        /*   are no banners to display.                         */
        ppszBanners[0] = GetMemory (DEFAULT_BANNER_LINES * sizeof (char *));
        for (i=0; i < DEFAULT_BANNER_LINES-1; ++i)
          ppszBanners[0][i] = aszDefault[i];

        ppszBanners[1] = GetMemory (sizeof (char *));
        ppszBanners[1][0] = NULL;
        return;
      }

    szBuffer = GetMemory (MAX_SCRN_LINE_LEN + 1);

    /* Initalize the banner screen number */
    i = 0;

    /* Find the section */
    while (i < MAX_BANNER_SCREENS - 1 &&
           MyFindSection (BANNER_SECTION, iFile, (int) NULL) != TRUE)
      {
        /* Initialize the banner lines */
        ppszBanners[i] = GetMemory (MAX_BANNER_LINES * sizeof (char *));
        u = 0;

        while ((DosReadLine (szBuffer, MAX_SCRN_LINE_LEN, iFile)) == FALSE &&
               szBuffer[0] != '[')
          {
            /* Ignore lines beginning with a semi-colon */
            if (szBuffer[0] == ';')
              continue;

            if (u < MAX_BANNER_LINES - 1)
              {
                ppszBanners[i][u] = GetMemory (strlen (szBuffer) + 1);
                strcpy (ppszBanners[i][u++], szBuffer);
              }
          }

        /* Unread this line if it was a [section] so */
        /*   that MyFindSection can find it          */
        if (szBuffer[0] == '[')
          DosUnreadLine (szBuffer);

        /* NULL out this screen's last pointer */
        if (u < MAX_BANNER_LINES)
          ppszBanners[i][u] = NULL;

        /* Bump i */
        ++i;
      }

    /* NULL out the next screen's first pointer */
    if (i < MAX_BANNER_SCREENS)
      {
        ppszBanners[i] = GetMemory (sizeof (char *));
        ppszBanners[i][0] = NULL;
      }

    /* Close the message file */
    _dos_close (iFile);
}
#endif


/*********************************************************************
 * MyFindSection - Finds the specified section entry in fpInsertFile.
 *                 All information with fpInsertFile up to and including
 *                 the section line are written to fpOutputFile.
 *
 * szSection   - Section to search for.
 * iFileIn     - DOS filehandle to read.
 * iFileOut    - DOS filehandle to write, NULL if write is not required.
 *
 * Returns:  TRUE if an error occured, or the section was not found.
 *           FALSE if the section was found.
 *********************************************************************/

int MyFindSection (char *szSection, int iFileIn, int iFileOut)
{
  char *szBuffer;  /* Input buffer     */
  register int i;  /* Looping variable */


  /* Allocate sufficient space for szBuffer */
  szBuffer = GetMemory (MAX_SYSTEM_INI_LINE_LEN + 1);

  /* Read the line */
  while (DosReadLine (szBuffer, MAX_SYSTEM_INI_LINE_LEN, iFileIn) != TRUE)
    {
      /* Write the line out to the output file */
      if (iFileOut)
        DosWriteLine (szBuffer, iFileOut);

      /* Skip whitespace at beginning of szBuffer */
      for (i = 0; i < MAX_SYSTEM_INI_LINE_LEN && szBuffer[i] != '\0' &&
                   (szBuffer[i] == ' ' || szBuffer[i] == '\t');  ++i)
        ;

      /* Do we have a match */
      if (strnicmp (szSection, &szBuffer[i], strlen (szSection)) == 0)
        {
          FreeMemory (szBuffer);
          return (FALSE);
        }
    }

  /* Section was not found */
  FreeMemory (szBuffer);
  return (TRUE);
}

char *szUnreadLine = NULL;  /* Stores the unread line */
int iUnread = -1;           /* Index to the unread line */

/*********************************************************************
 * DosUnreadLine - Stores a line of text to pass out of DosReadLine.
 *
 * szBuffer - String is to be stored
 *
 * Returns:  Nothing.
 *********************************************************************/

void DosUnreadLine (char *szBuffer)
{
  szUnreadLine = GetMemory (strlen (szBuffer) + 1);
  strcpy (szUnreadLine, szBuffer);
  iUnread = 0;
}

/*********************************************************************
 * DosReadLine - Reads a single text line from disk using _dos_read.
 *
 * szBuffer - String is stored here
 * iLength  - Maximum number of characters to read
 * iFile    - DOS file handle
 *
 * Returns:  TRUE at end of file, FALSE otherwise.
 *********************************************************************/

int DosReadLine (char *szBuffer, int iLength, int iFile)
{
  int  i = 0;                 /* Index for szBuffer                 */
  int  c = 0x7F;              /* Character read in from disk        */
  int  iNext;                 /* Next Character read in from disk   */

  /* BUGBUG 27-Feb-1993 bens Bogus input routine
   *
   * The following loop is not very graceful about handling long lines.
   * Current behavior with the "--iLength" in the for loop causes us to
   * read n-1 characters into the buffer, and then stop, leaving room
   * to put the null terminator.
   *
   * The "feature" of this loop is that lines longer than iLength will
   * be split into two or more lines.
   */

  while (--iLength && c != '\0')
    {
      /* Pull from the "unread" data first */
      if (iUnread != -1)
        {
          if ((c = (int) szUnreadLine[iUnread++]) == '\0' || c == '\r')
            {
              /* Nothing left in the "unread" buffer */
              FreeMemory (szUnreadLine);
              iUnread = -1;
            }
        }
      else
        {
          /* Read from the disk if there's no "unread" data */
          if ((c = DosReadChar (iFile)) == -1)
            {
              /* Error or EOF */
              szBuffer[i] = '\0';
              // If a Ctr-Z at end of file, remove it.
              if (i > 0 && szBuffer[i-1] == 26)
                 szBuffer[--i] = 0;

              // If this line had any chars, then pretend the line was cr/lf
              // terminated.  Since we're now at end of file, the next call
              // will have i=0, so will return TRUE (EOF).
              return ((i == 0) ? TRUE : FALSE);
            }
        }

      /* Newlines are ignored */
      if (c == '\n')
        {
          ++iLength;
          continue;
        }

      /* Carriage returns mark the end of the line */
      if (c == '\r')
        {
          c = '\0';

          /* Check to see if the next character is a linefeed */
          if ((iNext = DosReadChar (iFile)) != '\n')
            {
              /* Corrupt file -- simulate EOF */
              szBuffer[0] = '\0';
              return (TRUE);
            }
          else
            DosUnreadChar ((char) iNext);
        }

      /* Store the character */
      szBuffer[i++] = (char) c;
    }

  /* Put a zero byte at the end of the string, if necessary */
  if (c)
    szBuffer[i] = '\0';

  return (FALSE);
}


char chUnreadChar;        /* Stores unread character */
int fUnreadChar = FALSE;  /* TRUE when chUnreadChar stores a character */

/*********************************************************************
 * DosReadChar - Reads a single character from the disk.
 *
 * iFile - DOS file handle.
 *
 * RETURNS: -1 if EOF or error, character otherwise.
 *********************************************************************/

int DosReadChar (int iFile)
{
  char c;                     /* Character read in from disk        */
  int  iNumRead;              /* Number of characters actually read */


  /* Check the unread character */
  if (fUnreadChar)
    {
      fUnreadChar = FALSE;
      return (chUnreadChar);
    }

  /* Read the character from the disk */
  if (_dos_read (iFile, (char far *) &c, 1, &iNumRead) != 0 || iNumRead == 0)
    {
      /* Error or EOF */
      return (-1);
    }
  else
    return ((int) c);
}


/*********************************************************************
 * DosUnreadChar - Stores a single character from the disk.
 *
 * iFile - DOS file handle.
 *
 * RETURNS: -1 if EOF or error, character otherwise.
 *********************************************************************/

void DosUnreadChar (char ch)
{
  chUnreadChar = ch;
  fUnreadChar = TRUE;
}


/*********************************************************************
 * DosWriteLine - Writes a single text line from disk using _dos_write.
 *
 * szBuffer - String is stored here
 * iFile    - DOS file handle
 *
 * Returns:  ERROR if out of disk space, OK otherwise.
 *********************************************************************/

int DosWriteLine (char *szBuffer, int iFile)
{
  unsigned fReturnValue = 0;    /* Return value from _dos_write    */
  unsigned uNmbrToWrite;        /* Actual number of bytes to write */
  unsigned uNmbrWritten;        /* Actual number of bytes written  */


  /* Write the line */
  if (szBuffer)
    {
      uNmbrToWrite = strlen (szBuffer);
      fReturnValue = _dos_write (iFile, szBuffer, uNmbrToWrite,
                             &uNmbrWritten);
      if (fReturnValue)
        FatalError (FATAL_DISK_ERROR);
    }
  else
    {  // Safe simulation of write of blank line
      uNmbrToWrite = 0;
      uNmbrWritten = 0;
    }

  /* If we weren't out of disk space */
  if (uNmbrToWrite == uNmbrWritten)
    {
      /* Write the carriage return/line feed */
      uNmbrToWrite = 2;
      fReturnValue = _dos_write (iFile, "\r\n", uNmbrToWrite, &uNmbrWritten);

      if (fReturnValue)
        FatalError (FATAL_DISK_ERROR);
    }

  /* Return an error if out of disk space */
  return ((uNmbrToWrite != uNmbrWritten) ? ERROR : OK);
}


/***************************************************************************/
/* Cleanup function to free all memory allocated by any of the database 	*/
/* management functions. This is needed for debugging purposes only and 	*/
/* MEM_BUG should not be defined in the distribution build. 					*/
/* 																								*/
/* void FreeDataMemory( void )															*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

#ifdef	MEM_BUG

void FreeDataMemory( void )
{
	register		i;

	for ( i = 0; i < END_CLASS; i++ )
		if ( Data[ i ] != NULL )
			FreeMemory( Data[ i ] );

	if ( RelocBuf != NULL )
		FreeMemory( RelocBuf );

	if ( OemList != NULL )
		FreeMemory( OemList );

	if ( pchStringBuf != NULL )
		FreeMemory( pchStringBuf );
}
#endif

/***************************************************************************/
/* Initializes the Data[] array with arrays of pointers to strings for		*/
/* each class specifed in the dosdata.dat file associated with this OEM.	*/
/* This function is also used to load the non OEM specific data specified	*/
/* by the "UPGRADE 0.0" area in dosdata.dat. 										*/
/* 																								*/
/* void LoadOemData( struct DDR *Oem ) 												*/
/* 																								*/
/* ARGUMENTS:	Oem	- Ptr to DDR struct with name an version number 		*/
/* RETURNS: 	struct DDR *- Ptr to DDR structure for specifed OEM			*/
/* 																								*/
/***************************************************************************/

void LoadOemData( struct DDR *Oem )
{
	register 	i;
	char			**apszArray;

	ReadOemData( Oem->uDataOffset );
	for ( i = 0; i < END_CLASS; i++ )
		if ( (apszArray = GetClassData( Class[ i ] )) != NULL )
			Data[ i ] = apszArray;
}

/***************************************************************************/
/* Reads in a block of data from the DOSDATA.DAT file. First reads a 2		*/
/* byte value at the start of the block which contains the size of the		*/
/* block and then allocates a buffer the proper size and reads the data 	*/
/* into the allocated memory area. If there is an error the FatalError()	*/
/* function is called to abort the program. It is assumed that the file 	*/
/* pointer's position in the data file is already pointing at the first    */
/* 2 bytes at the start of the buffer. 												*/
/* 																								*/
/* unsigned GetNextDataBlock( int iFile, void **DataBuf )						*/
/* 																								*/
/* ARGUMENTS:	iFile 		- Open file handle to the data file 				*/
/* 				DataBuf		- Ptr to the data buffer ptr							*/
/* 																								*/
/***************************************************************************/

static unsigned GetNextDataBlock( int iFile, void **DataBuf )
{
	register		iStatus;
	unsigned 	uBufLen;
	unsigned 	uRead;

	iStatus = ERROR;							/* Assume there may be an error	*/

	if ( _dos_read( iFile, &uBufLen, USIZE, &uRead ) == OK &&
			  uRead == USIZE )
	{
		*DataBuf = GetMemory( uBufLen );
		if ( _dos_read( iFile, *DataBuf, uBufLen, &uRead ) == OK )
			if (uRead == uBufLen )
				iStatus = OK;
	}

	if ( iStatus != OK )
		FatalError( FATAL_DATA_READ_ERROR );

	return( uBufLen	);
}

/***************************************************************************/
/* Reads in a block of data into the relocation buffer. The read starts at */
/* the begining of the dosdata file data area + the specified offset.		*/
/* 																								*/
/* void ReadOemData( unsigned uDataOffset )											*/
/* 																								*/
/* ARGUMENTS:	uDataOffset - Offset of data from an OEM DDR structure		*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

static void ReadOemData( unsigned uDataOffset )
{
	register		iStatus; 				/* Keeps track of error conditions		*/
	int			iFile;					/* DOS file handle							*/

	unsigned 	uRead;					/* Number of bytes read by a dos_read	*/
	long			lOffset; 				/* Offset in file to start read at		*/

	lOffset = (long)uDataOffset + lDataOffset;
	iStatus = ERROR;

	if ( _dos_open( szDosDataFile, ShareAccess, &iFile	) == OK )
	{
		if ( _dos_seek( iFile, lOffset, SEEK_SET ) == lOffset )
			if ( _dos_read( iFile, RelocBuf, RELOC_LEN, &uRead ) == OK )
				if ( uRead > 0 )
					iStatus = OK;

		_dos_close( iFile );
	}

	if ( iStatus != OK )
		FatalError( FATAL_DATA_READ_ERROR );
}

/***************************************************************************/
/* Scans through the array of OEM DDR structures looking for a structure	*/
/* in which the OEM name field matchs the argument string. Returns a ptr	*/
/* to the first matching DDR structure or NULL if no match is found. 		*/
/* 																								*/
/* struct DDR *GetOemRecord( char *szOemName, int MajorVer, int MinorVer ) */
/* 																								*/
/* ARGUMENTS:	char *szOem 	-	Ptr to OEM name string							*/
/* 				MajorVer 		-	DOS major version number						*/
/* 				MinorVer 		-	DSO minor version number						*/
/* RETURNS: 	struct DDR *	-	Ptr to ddr struct for specifed OEM if the */
/* 										OEM is not found returns NULL ptr			*/
/* 																								*/
/***************************************************************************/

struct DDR *GetOemRecord( char *szOemName, int MajorVer, int MinorVer )
{
	register 	i;
	struct DDR	*Oem;

	Oem = NULL; 							/* Returns NULL if can't find match */

	for ( i = 0; i < (int)MaxOem; i++ )
	{
		if ( (int)OemList[ i ].MajorVer == MajorVer &&
			  (int)OemList[ i ].MinorVer == MinorVer &&
			  strcmpi( OemList[ i ].szOemName, szOemName ) == OK )
		{
			Oem = &(OemList[ i ]);
			break;
		}
	}

	return( Oem );
}

/***************************************************************************/
/* Scans through the relocation buffer and builds an array of pointers to	*/
/* to all of the strings in the specified class. The array is dynamically	*/
/* allocated after the total number of strings in the class is determined. */
/* An error check is done to be sure this function is only called once for */
/* any specified class of data															*/
/* 																								*/
/* char **GetClassData( char *InfoClass ) 											*/
/* 																								*/
/* ARGUMENTS:	InfoClass	- Ptr to data class string 							*/
/* RETURNS: 	char **		- Array of pointers to strings in this class 	*/
/* 								  or NULL if the class wasn't found             */
/* 																								*/
/***************************************************************************/

static char **GetClassData( char *InfoClass )
{
	register 	uFirst;					/* First entry for specified class	*/
	register 	uLast;					/* Last entry for specified class	*/
	char			**szStrings;			/* Array of ptrs to strings			*/
	int			i; 						/* Indice for array of ptrs			*/
	unsigned 	uArraySize;

	szStrings = NULL; 					/* Just in case class isn't found	*/

												/* Locate the specified class 		*/
	for ( uFirst = 0;
			uFirst < RELOC_LEN && RelocBuf[ uFirst ] != EOC_MARKER;
			uFirst++ )
		if ( strcmpi( (pchStringBuf + RelocBuf[ uFirst ]), InfoClass ) == OK )
			break;

	if ( RelocBuf[ uFirst ] != EOC_MARKER )	/* May not have found class*/
	{
		uFirst++;							/* Skip over class label */

												/* Locate end of this class */
		for ( uLast = uFirst;
				uLast < RELOC_LEN &&
				RelocBuf[ uLast ] != EOC_MARKER	&&
			  *(pchStringBuf + RelocBuf[ uLast ]) != '[';
				uLast++ )
			;
							/* Determine num class entries + end of array marker	*/
		uArraySize = (unsigned)(uLast - uFirst) + 1;

		if ( uArraySize > 1 )
		{
			szStrings = GetMemory( uArraySize * sizeof(char *) );

												/* Fill in array with ptrs to strings */
			for ( i = 0; uFirst < uLast; uFirst++, i++ )
				szStrings[ i ] = pchStringBuf + RelocBuf[ uFirst ];

			szStrings[ i ] = NULL;			/* Mark end of array with NULL ptr	*/
		}
	}

	return( szStrings );
}

/***************************************************************************/
/* Returns a ptr to a string of the specified type which is from the list	*/
/* of strings for that type. The argument Index specifies which string		*/
/* in the array for that type is returned. If the specified type is ivalid */
/* a NULL ptr will be returned but there is no error checking on the Index */
/* value but if the caller makes calls using a sequence of values the end	*/
/* end of the data for that type will be signaled when a NULL ptr is 		*/
/* returned.																					*/
/* 																								*/
/* Valid type are the enunerated values defined by "UpgradeData". 			*/
/* 																								*/
/* char *GetDataString( int Type, int Index )										*/
/* 																								*/
/* ARGUMENTS:	Type		- Specifies the data array to look in					*/
/* 				Index		- String in the list to be returned 					*/
/* RETURNS: 	char *	- Ptr to sting or NULL if no more strings 			*/
/* 																								*/
/***************************************************************************/

char *GetDataString( int Type, int Index )
{
	return ( (Type < 0 || Type >= END_CLASS || Data[ Type ] == NULL) ?
				NULL : *(Data[ Type ] + Index) );
}

/***************************************************************************/
/* Returns an array of ptrs to all of the strings in the specified class.	*/
/* Returns NULL ptr if argument is not a valid class. 							*/
/* 																								*/
/* Valid class type are the enunerated values defined by "UpgradeData".		*/
/* 																								*/
/* char **GetClassList( int Type )														*/
/* 																								*/
/* ARGUMENTS:	Type		- Specifies the data class to array to return		*/
/* RETURNS: 	char **	- Array of ptrs to strings in the specified class	*/
/* 																								*/
/***************************************************************************/

char **GetClassList( int Type )
{
	return ( (Type < 0 || Type >= END_CLASS || Data[ Type ] == NULL) ?
				NULL : Data[ Type ] );
}

/***************************************************************************/
/* Returns the distribution disk type where 1=360K, 2 = 720K, 3 = 1.2meg,	*/
/* 4 = 1.44meg																					*/
/* 																								*/
/* void GetDistrDiskType( int iType )													*/
/* 																								*/
/* ARGUMENTS:	void																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

int GetDistrDiskType( void )
{
	return( iDiskType );
}

/***************************************************************************/
/* Returns the total number of distrubution disks for the upgrade.			*/
/* 																								*/
/* int GetNumberDisks( void )																*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	int	-	Total number of distribution disks						*/
/* 																								*/
/***************************************************************************/

int GetNumberDisks( void )
{

	return ( GetNumberStrings( Data[ USER_LABEL ] ) );

}

/***************************************************************************/
/* Returns a ptr to a disk label string for the specified disk. No error	*/
/* checking is done for disk number.													*/
/* 																								*/
/* char *GetDiskLabel( int iDiskNum )													*/
/* 																								*/
/* ARGUMENTS:	iDiskNum 	- File seqence number									*/
/* RETURNS: 	char *		- Ptr to label string									*/
/* 																								*/
/***************************************************************************/

char *GetDistribLabel( int iDiskNum )
{
	return( GetDataString( DIST_LABEL, iDiskNum) );
}

/***************************************************************************/
/* Returns a ptr to a prompt string for the specified distribution disk.	*/
/*	No error checking is done for disk type or prompt number.					*/
/* 																								*/
/* char *GetDiskPrompt( int iDiskNum )													*/
/* 																								*/
/* ARGUMENTS:	iDiskNum 	- File seqence number									*/
/* RETURNS: 	char *		- Ptr to prompt string									*/
/* 																								*/
/***************************************************************************/

char *GetDistribPrompt( int iDiskNum )
{
	return( GetDataString( PROMPT, iDiskNum) );
}

/***************************************************************************/
/* Returns a ptr to a prompt string for the specified user supplied disk.	*/
/*	No error checking is done for disk type or prompt number.					*/
/* 																								*/
/* char *GetDiskPrompt( int iDiskNum )													*/
/* 																								*/
/* ARGUMENTS:	iDiskNum 	- File seqence number									*/
/* RETURNS: 	char *		- Ptr to prompt string									*/
/* 																								*/
/***************************************************************************/

char *GetUserPrompt( int iDiskNum )
{
	return( GetDataString( USER_PROMPT, iDiskNum) );
}

/***************************************************************************/
/* Returns a ptr to a prompt string for the specified disk. No error 		*/
/* checking is done for disk number.													*/
/* 																								*/
/* char *GetUserLabel( int iDiskNum )													*/
/* 																								*/
/* ARGUMENTS:	iDiskNum 	- File seqence number									*/
/* RETURNS: 	char *		- Ptr to label string									*/
/* 																								*/
/***************************************************************************/

char *GetUserLabel( int iDiskNum )
{
	return( GetDataString( USER_LABEL, iDiskNum ) );
}

/***************************************************************************/
/* Returns a ptr to the specified file on the specified disk. The name is	*/
/* directly from the DosData file and is not translated. Error checking 	*/
/* is done to be sure the disk number is valid but there is no checking 	*/
/* done on the file number but if caller gets the files name using a valid */
/* sequence a NULL ptr will be returned to signal the end of the list.		*/
/* 																								*/
/* char *GetFileName( int iDisk, iFile )												*/
/* 																								*/
/* ARGUMENTS:	Disk			- Distribution disk number 							*/
/* 				iFile 		- File seqence number									*/
/* RETURNS: 	char *		- Ptr to file name or NULL if invalid file # 	*/
/* 																								*/
/***************************************************************************/

char *GetFileName( int iDisk, int iFile )
{
  if (iDisk < 0)
    {
      /* This is maintenance mode -- get the file */
      /*   from the optional components list.     */
      /*   No error checking is performed.        */
      return (GetDataString (-iDisk, iFile));
    }
  else
    {
      /* This is for non-maintenance mode */
      iDisk += DISK_0;

      return ((iDisk >= DISK_0 && iDisk < END_CLASS) ?
               GetDataString (iDisk, iFile) : NULL);
    }
}

/***************************************************************************/
/* Translates a file name based on the content of the data field RENAME 	*/
/* and NO_COPY from the data file. The return ptr will be the correct name */
/* to be used when creating it on the user's disk. If the file is marked   */
/* don't copy a NULL ptr will be returned. The order of precedence is		*/
/* NO_COPY then GLOBAL_RENAME.															*/
/*																									*/
/* Updated 08/21/90 to return NULL if floppy upgrade and the file is a net	*/
/* upgrade file.																				*/
/*																									*/
/* char *GetRealDestName ( char *szFileName )										*/
/* 																								*/
/* ARGUMENTS:	szFileName	- Ptr to name of file to translate					*/
/* RETURNS: 	char *		- Ptr to correct file name for destination file	*/
/* 																								*/
/***************************************************************************/

char *GetRealDestName( char *szFileName )
{
	register		i;
	char			*szString;

												/* See if file in in the NO_COPY list	*/
	if ( FindDataMatch( szFileName, NO_INSTALL ) )
		return( NULL );
												/* Now check for drivers which may not	*/
												/* need copying and net update files	*/
	if ( Data[ DRIVER_VERSION ] != NULL &&
		  ((i = StrSearch( szFileName, Data[ DRIVER_VERSION ] )) != -1 &&
			vInfo.NoCopy[ (UINT)i >> 1 ] == TRUE) )
   {

#if defined(UPGRADE_PROGRAM) || defined(RECOVERY_PROGRAM)
      // If HIMEM.SYS, check if we copy it anyway even though there's already
      // an existing XMS driver active (we won't update CONFIG.SYS though).
      //
      if ((i>>1) != NO_HIMEM && (i>>1) != NO_EMM386)
         return ( NULL );
#else
      return (NULL);
#endif
   }

#if defined(UPGRADE_PROGRAM) || defined(RECOVERY_PROGRAM)
	/* Update only Microsoft DOS Mouse drivers */
   /* BUGBUG - this should be handled via the vInfo.NoCopy array in        */
   /* the future.                                                          */
	if (vInfo.Flag.fMouse == FALSE &&
		 stricmp (szFileName, "MOUSE.COM") == 0)
		return (NULL);
#endif

#ifndef	OEM_PROGRAM
	if ( (!vInfo.Flag.fHardInstall || vInfo.chSource < vInfo.chFirstHd)
		  && FindDataMatch( szFileName, NET_FILES ) )
		return( NULL );
#endif

												/* Now see if it get's renamed 			*/
	if ( (szString = TranslateString( GLOBAL_RENAME, szFileName )) != NULL )
		return( szString );
	else
		return( szFileName );
}


/***************************************************************************/
/* Scans the DIF_PATH class to see if the specified file name is in the 	*/
/*	list. If the file name is found the new path associated with the			*/
/*	file name is returned. If the file name is not found in the list the 	*/
/* ptr to the default path is returned.												*/
/*																									*/
/* Updated 09/21/90 to support a different path for network files which		*/
/* will be copied to a new directory in OLD_DOS.xxx.								*/
/* 																								*/
/* char *GetRealPath( char *szFile, char *szOldPath )								*/
/* 																								*/
/* ARGUMENTS:	szFile	-	Ptr to file name string 								*/
/* 				szOldPath-	Ptr to default path										*/
/* RETURNS: 	char *	-	Ptr to real path for specified file 				*/
/* 																								*/
/***************************************************************************/

char *GetRealDestPath( char *szFile, char *szOldPath )
{
	char		*szNewPath;

	szNewPath = TranslateString( DIF_PATH, szFile );

#ifndef	OEM_PROGRAM
	if ( szNewPath == NULL && vInfo.Flag.fHardInstall &&
		  FindDataMatch( szFile, NET_FILES ) )
		szNewPath = vInfo.szNetDir + 3;
#endif

	return( szNewPath != NULL ? szNewPath : szOldPath );
}

/***************************************************************************/
/* Scans the DISTRIB_X classes to see if the specified file name is in the	*/
/*	list. If the file name is found the disk number associated with this 	*/
/*	file name is returned. If the file name is not found in the list the 	*/
/*	orignal disk number is returned. 													*/
/* 																								*/
/* int GetRealDisk( char *szFile, int iDiskNum )									*/
/* 																								*/
/* ARGUMENTS:	szFile	-	Ptr to file name string 								*/
/* 				iDiskNum -	Default disk number										*/
/* RETURNS: 	int		-	Disk number for this disk or -1 if not found 	*/
/* 																								*/
/***************************************************************************/

int GetRealSrcDisk( char *szFile, int iDiskNum )
{
	register 	i;
	register		iEnd;

	iEnd = DISTR_0 + MAX_NUM_DISKS;

	for ( i = DISTR_0; i < iEnd && Data[ i ] != NULL; i++ )
	{
		if ( StrSearch( szFile, Data[ i ] ) != -1 )
		{
			iDiskNum = i - DISTR_0;
			break;
		}
	}

	return( iDiskNum );
}

/***************************************************************************/
/* Scans the specified class of translation strings for a string matching	*/
/* the arugment szString. If a match is found a ptr to the string 			*/
/* associated with the matched string is return. If a match is not found	*/
/* a NULL ptr is returned. 																*/
/* 																								*/
/* char *TranslateString( int Type, char *szString )								*/
/* 																								*/
/* ARGUMENTS:	Type		-	Specifies the data class to scan						*/
/* 				szString -	Ptr to string to be translated						*/
/* RETURNS: 	char *	-	Ptr to new string if argument string was found	*/
/* 								in specified class else NULL ptr						*/
/* 																								*/
/***************************************************************************/

char *TranslateString( int Type, char *szString )
{
	register 	i;
	char			*szMatch;

	for( i = 0; (szMatch = GetDataString( Type, i)) != NULL; i += 2 )
		if ( strcmpi( szMatch, szString ) == OK )
		{
			szMatch = GetDataString( Type, ++i );
			break;
		}

	return( szMatch );
}

/***************************************************************************/
/* Checks for a matching string in the specified OEM information class. 	*/
/* 																								*/
/* int FindOemInfo( char *szString, int Type )										*/
/* 																								*/
/* ARGUMENTS:	szString -	Ptr to a string											*/
/* RETURNS: 	int		-	TRUE	string exists in the specifed class			*/
/* 								else FALSE													*/
/* 																								*/
/***************************************************************************/

int FindDataMatch( char *szString, int Type )
{
	if ( Data[ Type ] == NULL )
		return( FALSE );
	else
		return( StrSearch( szString, Data[ Type ] ) == -1 ? FALSE : TRUE );
}

/***************************************************************************/
/* Returns the total bytes that would be read plus bytes written when		*/
/* the specified disk is copied.															*/
/*																									*/
/*	long GetDiskBytes( iDiskNum )															*/
/*																									*/
/*	ARGUMENTS:	iDiskNum -	Disk number to get bytes on (0 based)				*/
/*	RETURNS:		long		-	Total bytes													*/
/*																									*/
/***************************************************************************/

long	GetDiskBytes( int iDiskNum )
{
	char		*szByteStr;

	szByteStr = GetDataString( DISK_BYTES, iDiskNum);

	if ( szByteStr != NULL )
		return( atol( szByteStr ) );
	else
		return( 0L );
}

/***************************************************************************/

#ifndef	OEM_PROGRAM

/***************************************************************************/
/* Initializes the Data[ OEM_TABLE ] array with arrays of pointers to		*/
/*	strings which describe all the information needed to search a series of */
/* files for a specific string to determine the user's OEM. Returns an     */
/* array of pointers to the strings that specify this information.			*/
/* 																								*/
/* char **InitSearchData( int MajorVer, int	MinorVer )							*/
/* 																								*/
/* ARGUMENTS:	MajorVer -	DOS major version number								*/
/* 				MinorVer -	DOS minor verison number								*/
/* RETURNS: 	char **	-	Array of pointer to file,search,OEM strings		*/
/* 								or NULL if no information for this version		*/
/* 																								*/
/* Array format:																				*/
/* 																								*/
/* "FILENAME.EXT" 																			*/
/* "string" 																					*/
/* "OEMNAME"																					*/
/* "string" 																					*/
/* "OEMNAME                                                                */
/* "&&"																							*/
/* "FILENAME.EXT" 																			*/
/* "string1"																					*/
/* "OEMNAME"																					*/
/* "string" 																					*/
/* "OEMNAME"																					*/
/* NULL																							*/
/* 																								*/
/***************************************************************************/

char **InitSearchData( int MajorVer, int	MinorVer )
{
	static char		*szVer = "[0.00]";
	struct DDR		*Oem;

	itoa( MajorVer, szVer + 1, 10 );				/* Make version string [0.00] */
	szVer[2] = '.';
	szVer[3] = '0';
	itoa( MinorVer, szVer + (MinorVer > 9 ? 3 : 4), 10 );
	szVer[5] = ']';
	szVer[6] = EOL;

	if ( (Oem = GetOemRecord( szSearch, 0, 0 )) != NULL )
	{
		ReadOemData( Oem->uDataOffset );
		Data[ OEM_TABLE ] = GetClassData( szVer );
	}
	else
		Data[ OEM_TABLE ] = NULL;

	return( Data[ OEM_TABLE ] );
}

/***************************************************************************/
/* Frees the memory allocated to OEM_TABLE array. This can be called as 	*/
/* soon as the OEM has been identified since the search list is only used	*/
/* the initial time the OEM is determined.											*/
/* 																								*/
/* void FreeSearchData( void )															*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void FreeSearchData( void )
{
	if ( Data[ OEM_TABLE ] != NULL )
	{
		FreeMemory( Data[ OEM_TABLE ] );
		Data[ OEM_TABLE ] = NULL;
	}
}

/***************************************************************************/
/* Builds a list of ptrs to all of the OEM vender names for the specifed	*/
/* DOS version. The total number of names in the list is returned and the	*/
/* a NULL ptr will be added to mark the end of the list. 						*/
/* 																								*/
/* int BuildOemList( char *apszList, int MajorVer, int MinorVer )				*/
/* 																								*/
/* ARGUMENTS:	apszList		-	Arary to hold ptrs to oem names					*/
/* 				MajorVer 	-	DOS major version number							*/
/* 				MinorVer 	-	DSO minor version number							*/
/* RETURNS: 	int			-	Total names added to the list 					*/
/* 																								*/
/***************************************************************************/

int BuildOemList( char **apszList, int MajorVer, int MinorVer )
{
	register 	i;
	register 	iCount;

	for ( i = iCount = 0; i < (int)MaxOem; i++ )
		if ( (int)OemList[ i ].MajorVer == MajorVer &&
			  (int)OemList[ i ].MinorVer == MinorVer )
			apszList[ iCount++ ] = OemList[ i ].szOemName;

	apszList[ iCount ] = NULL;

	return( iCount	);
}

/***************************************************************************/
/* Returns a ptr to the string indexed by the argument StrNum in the list	*/
/*	of search strings for the search file specified by FileNum. 				*/
/* 																								*/
/* char	*GetOemStr( int FileNum, int StrNum )										*/
/* 																								*/
/* ARGUMENTS:	FileNum	- Search file sequence number 							*/
/* 				StrNum	- Search string sequence # for specified search 	*/
/* 							  file.															*/
/* RETURNS: 	char *	- Ptr to search string or NULL if invalid file or	*/
/* 							  string sequence number									*/
/* 																								*/
/***************************************************************************/

char *GetSearchStr( int FileNum, int StrNum )
{
	register 	i;
	register 	iStrCount;
	int			iFileCount;
	char			*szString;

	for ( i = iFileCount = 0; iFileCount < FileNum; i++ )
	{
		if ( (szString = GetDataString( OEM_TABLE, i )) == NULL )
			break;
		else
			if ( szString[0] == '&' )
				iFileCount++;
	}

	for ( iStrCount = 0;
			iStrCount <= StrNum && szString != NULL;
			iStrCount++, i++ )
	{
		if ( (szString = GetDataString( OEM_TABLE, i )) != NULL )
			if ( szString[0] == '&' )
				szString = NULL;
	}

	return( szString );
}

/***************************************************************************/
/* Returns the distribution version number for the specified device driver	*/
/* which is in the data file.																*/
/*																									*/
/* NOTE:																							*/
/* 	See the file "retail\global.h" for the number assigned to each			*/
/*		of the distributed device drivers.												*/
/*																									*/
/*	unsigned GetDriverVersion( int iDriverNum )										*/
/*																									*/
/*	ARGUMENTS:	iDriverNum - Driver number to get the version for				*/
/*	RETURNS:		unsigned	  - The specified driver's internal revision #		*/
/*																									*/
/***************************************************************************/

unsigned GetDriverVersion( int iDriverNum )
{
	char		*szVersion;

	szVersion = GetDataString( DRIVER_VERSION, (iDriverNum * 2) + 1);

	if ( szVersion != NULL )
		return( (unsigned)atoi( szVersion ) );
	else
		return( 0 );
}

/***************************************************************************/
/* Checks a filename to see if it's a file the gets copied from the			*/
/* distribution disks.																		*/
/*																									*/
/*	int IsDistrFile( char *szFile )														*/
/*																									*/
/*	ARGUMENTS:	szFile	- Ptr to file name to check for							*/
/*	RETURNS:		int		- TRUE if file is to be copied else FALSE				*/
/*																									*/
/***************************************************************************/

int IsDistrFile( char *szFile )
{
	unsigned register		iDisk;
	register		iFile;
	char			*szTmp;

	for ( iDisk = FIRST_USER_DISK; iDisk < vInfo.uchNumDisks; iDisk++ )
	{
		for ( iFile = 0;
				(szTmp = GetFileName( iDisk, iFile )) != NULL;
				iFile++ )
		{
			if ( (szTmp = GetRealDestName( szTmp )) != NULL &&
				  strcmpi( szFile, szTmp ) == OK )
				return( TRUE );
		}
	}
	return( FALSE );
}

/***************************************************************************/
/* Enumerates the distribution files.													*/
/*																									*/
/* char * EnumDistrFiles( unsigned fCont )											*/
/*																									*/
/* ARGUMENTS:	fCont 	- 0 if start new enumeration, !0 if continue 		*/
/* RETURNS: 	char *	- ptr to first/next distribution file name			*/
/* 							  NULL if no more names 									*/
/*																									*/
/***************************************************************************/

char * EnumDistrFiles(unsigned fCont)
{
	static unsigned iDisk;
	static int		 iFile;
	char	 *szTmp;

	if (fCont == 0) { 									/* initialize counters if	*/
		iDisk = FIRST_USER_DISK;						/*   starting a new enum	*/
		iFile = 0;
	}

	while (iDisk < vInfo.uchNumDisks)
	{
		if ((szTmp = GetFileName(iDisk, iFile++)) != NULL)
		{
			if ((szTmp = GetRealDestName(szTmp)) != NULL)
				return(szTmp);

		} else {

			iFile = 0;										/* switch to next disk */
			iDisk++;
		}
	}

	return(NULL);
}

/***************************************************************************/

#endif

/***************************************************************************/
/* Scans the DIF_FILE class in the data list	to see if the specified file	*/
/*	name is in the	list. If the file name is found the new file name			*/
/*	associated with the specified	file name is returned. If the file name	*/
/*	is not found in the list the original default file name is returned.		*/
/* 																								*/
/* char *GetRealSrcName( char *szFile )												*/
/* 																								*/
/* ARGUMENTS:	szFile	-	Ptr to file name string 								*/
/* RETURNS: 	char *	-	Ptr to new path or if not in list arg string		*/
/* 																								*/
/*	Video Types:																				*/
/*			0 = MONO																				*/
/*			1 = CGA																				*/
/*			2 = EGA																				*/
/*			3 = VGA																				*/
/*			4 = HERCULES																		*/
/***************************************************************************/

char *GetRealSrcName( char *szFile )
{
	char		*szString;
	char		**apszFiles;
	static char	szSourceGrpName[13] = { '\0' };
#ifndef RECOVERY_PROGRAM
	char		szPath[MAX_PATH];    /* Used in checking for existing */
                                     /*   windows utilities.          */
#endif

#ifndef OEM_PROGRAM
	extern char *gszWNToolsGrp;
#endif

	szString = NULL;

	if ( strcmpi( szFile, "DOSSHELL.VID" ) == OK )		/* Video driver chk		*/
	{
		if ( vInfo.Hw.VideoType == 0 )		/* Don't need a driver if mono	*/
			return( NULL );

		if ( (apszFiles = GetClassList( VIDEO_DRIVER )) != NULL )
			szString = apszFiles[ vInfo.Hw.VideoType ];
	}

	else 	if ( strcmpi( szFile, "DOSSHELL.INI" ) == OK )	/* .INI file chk	*/
	{
		if ( (apszFiles = GetClassList( VIDEO_LIST )) != NULL )
			szString = apszFiles[ vInfo.Hw.VideoType ];
	}		

	else 	if ( strcmpi( szFile, "DOSSHELL.GRB" ) == OK )	/* .GRB file chk	*/
	{
		if ( (apszFiles = GetClassList( GRABBER_DRIVER )) != NULL )
			szString = apszFiles[ vInfo.Hw.VideoType ];
	}		
#ifndef OEM_PROGRAM
	else 	if ( strcmpi( szFile, gszWNToolsGrp ) == OK )	/* .GRP file chk	*/
	{
        /* If there is no source .GRP filename, create one */
        if (szSourceGrpName[0] == '\0')
          {
            /* Create the name of the source group file:     */
            /*   BK = Backup, UD = Undelete, AV = Anti-Virus */

            /* Is there, or will there be a Windows Backup utiltity */

#ifdef RECOVERY_PROGRAM
            if (vInfo.OptComp.BackupChoice    == WINDOWS_AND_DOS_COMPONENT ||
                vInfo.OptComp.BackupChoice    == WINDOWS_COMPONENT)
#else
            if (Install.Flags.fMaintenance)
              {
                strcpy (szPath, vInfo.szPath);
                mycatpath (szPath, "MWBACKUP.EXE");
              }

            if (vInfo.OptComp.BackupChoice    == WINDOWS_AND_DOS_COMPONENT ||
                vInfo.OptComp.BackupChoice    == WINDOWS_COMPONENT         ||
                (Install.Flags.fMaintenance && access (szPath, 0) != -1))
#endif
              strcat (szSourceGrpName, "bk");


            /* Is there, or will there be a Windows Undelete utility */

#ifdef RECOVERY_PROGRAM
            if (vInfo.OptComp.UndeleteChoice  == WINDOWS_AND_DOS_COMPONENT ||
                vInfo.OptComp.UndeleteChoice  == WINDOWS_COMPONENT)
#else
            if (Install.Flags.fMaintenance)
              {
                strcpy (szPath, vInfo.szPath);
                mycatpath (szPath, "WNUNDEL.EXE");
              }

            if (vInfo.OptComp.UndeleteChoice  == WINDOWS_AND_DOS_COMPONENT ||
                vInfo.OptComp.UndeleteChoice  == WINDOWS_COMPONENT         ||
                (Install.Flags.fMaintenance && access (szPath, 0) != -1))
#endif
              strcat (szSourceGrpName, "ud");


            /* Is there, or will there be a Windows Anti-Virus utility */

#ifdef RECOVERY_PROGRAM
            if (vInfo.OptComp.AntiVirusChoice == WINDOWS_AND_DOS_COMPONENT ||
                vInfo.OptComp.AntiVirusChoice == WINDOWS_COMPONENT)
#else
            if (Install.Flags.fMaintenance)
              {
                strcpy (szPath, vInfo.szPath);
                mycatpath (szPath, "MWAV.EXE");
              }

            if (vInfo.OptComp.AntiVirusChoice == WINDOWS_AND_DOS_COMPONENT ||
                vInfo.OptComp.AntiVirusChoice == WINDOWS_COMPONENT         ||
                (Install.Flags.fMaintenance && access (szPath, 0) != -1))
#endif
              strcat (szSourceGrpName, "av");

            strcat (szSourceGrpName, ".grp");
          }

        szString = szSourceGrpName;
	}
#endif
	else if ( (szString = TranslateString( DIF_FILE, szFile )) == NULL )
		szString = szFile;

	return( szString  );
}


/***************************************************************************/
/*   void ReplaceParam()                                                   */
/*                                                                         */
/*   Useful for replaceable parameters in setup screens.  It allows the    */
/*   parameter to be on any line in the screen.                            */
/*   In its current incarnation, you have to call it once for each         */
/*   replaceable param.                                                    */
/*                                                                         */
/*   ARGUMENTS:   apszScreen  - array of strings (from GetMessage).        */
/*                pszOld      - pattern to look for.                       */
/*                pszNew      - pattern to replace it with.                */
/*                pszBuf      - buffer to return modified line in. Buffer  */
/*                              will be inserted in apszScreen[] array     */
/*                fKeepLength - TRUE if line length must remain the same   */
/*   RETURNS:     NOTHING.                                                 */
/***************************************************************************/
void ReplaceParam (char **apszScreen, char *pszOld, char *pszNew,
                   char *pszBuf, int fKeepLength)
{
   char     *psz;
   unsigned  i, cbOld;
   int       delta;

   while (*apszScreen != NULL)
   {
      if ((psz = strstr(*apszScreen, pszOld)) != NULL)
      {
         cbOld = strlen(pszOld);
         i = psz-*apszScreen;

         strncpy (pszBuf, *apszScreen, i);
         strcpy (pszBuf+i, pszNew);
         strcat (pszBuf, psz+cbOld);

         if (fKeepLength)
         {
            delta = (int)((int)strlen(pszNew) - (int)cbOld);
            i     = strlen(pszBuf);

            if (delta > 0)
               pszBuf[ i-(unsigned)delta ] = 0;
            else if (delta < 0)
            {
               psz = i+pszBuf;
               for (i=0, delta=-(delta); i<(unsigned)delta; i++)
                  *(psz+i) = ' ';
               *(psz+i) = 0;
            }
         }

         *apszScreen = pszBuf;
      }
      apszScreen++;
   }
}
