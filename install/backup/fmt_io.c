/***************************************************************************/
/*																									*/
/* FMT_IO.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Backup utility for the MS-DOS 4.0 retail upgrade. These functions will	*/
/* backup a specified disk to another specified disk. Then entry point to	*/
/* the utiltity is BackUp(). The file coping is buffered using an area of	*/
/* memory which as allocated previously by a call to InitCpyBuffer().		*/
/* Most functions comminuicate via a group of static global variables to	*/
/* keep the size of code to a mininum. 												*/
/*																									*/
/* The backup format is restorable with most versions of DOS 2.x and above.*/
/* 																								*/
/* Created 07-12-89 johnhe																	*/
/* TABS = 3																						*/
/***************************************************************************/

#include		<stdio.h>
#include		<malloc.h>
#include 	<string.h>
#include 	<dos.h>
#include		<stdlib.h>
#include		<ctype.h>

#include		<alias.h>
#include		<bios_io.h>
#include		<disk_io.h>
#include		<strlib.h>
#include		<message.h>
#include		<fmt_io.h>
#include		<window.h>
#include		<global.h>

extern int	ProcDiskErr		( int ErrorType );
extern int	GetChar			( void );
extern int	FormatFloppy	( unsigned uDrive, int iFormat,
						  			  void (*vStatus)( unsigned int, unsigned ) );
static void		BkupDskPrompt( int DiskNum );

/***************************************************************************/

int					iDskFmtType =  0;			/* Default format type				*/
int					DiskNum;						/* User disk number					*/

static struct BPB	Bpb;							/* Bpb from current disk		 	*/
int					iDrv;							/* Destination physical number 	*/
char					szOldFileName[25];
int					IsReadWrite;

extern int			CR_Response[];
int					ExitResponse[] = EXIT_RESPONSE_TEXT;

/***************************************************************************/
/* Prepares a diskette as user diskette. First prompts user for a disk		*/
/* and loops until a disk has been inserted. Then checks to see if disk 	*/
/* is formatted and if not formats the disk. If the disk is formatted a		*/
/* check is made to be sure the user hasn't inserted a disk which was 		*/
/* previously used during this backup session. If the disk is useable		*/
/* a call is do to CleanFatDir() which will clear the FAT and root			*/
/* directory to free all of the disk for use. If there are errors the 		*/
/* function will continue to loop until a valid disk is inserted or the		*/
/* user elects to abort the backup.														*/
/*																									*/
/*	int PrepBkupDsk( void )																	*/
/*																									*/
/* ARGUMENTS:	NONE																			*/
/*	RETURNS:		int		OK is disk is ready to use or ABORT if user elects	*/
/*																									*/
/***************************************************************************/

int PrepBkupDsk( int DiskNum )
{
	extern unsigned char	chDestin;
	extern char				*szDiskLabel;
	char						*pchBuffer;
	int						iStatus;
	extern long				lDiskSize;
	UL							ulTmp;
	extern UL				LastTime;
	extern UL				StartTime;

	pchBuffer = GetMemory( SECTOR_SIZE );
	iDrv = chDestin - 'A';					/* Find bios disk number			*/
	do
	{
		iStatus = OK;							/* No errors as of yet				*/

		do											/* Wait until disk is present 	*/
			BkupDskPrompt( DiskNum );
		while( !IsDiskReady( iDrv ) );

													/* See if we need to format disk */
		if ( IsFormatted( iDrv, &Bpb, pchBuffer ) == FALSE )
		{
			if ( FormatNewDsk() != OK )
			iStatus = BAD_MEDIA;				/* Can't use this disk */
		}
													/* Make sure not a previous disk */
													/* and then srub the disk			*/

		else if ( (iStatus = IsNewDisk( DiskNum )) == OK )
			iStatus = ScrubFatRoot( iDrv, &Bpb );

		if ( iStatus == OK )					/* Disk seems ok so label it	*/
		{
			if ( (lDiskSize = GetDiskFree( chDestin )) >= 100000L &&
				  (ulTmp = SetDiskLabel( iDrv, szDiskLabel, &Bpb )) != (UL)(-1L) )
			{
				LastTime = ulTmp;
				if ( StartTime == 0L )				/* If this is first disk	*/
					StartTime = ulTmp;				/* need to set first time	*/
				_dos_dskreset();
			}
			else
				iStatus = BAD_MEDIA;					/* Can't use this disk */
		}

		if ( iStatus != OK )
			iStatus = ProcDiskErr( iStatus );

	}
	while( iStatus == REDO_DISK );

	FreeMemory( pchBuffer );
	return( iStatus );
}

/***************************************************************************/
/* Determines if the format type has been set and if not does a call to 	*/
/* the the format layout and then call the FormatFloopy function to do the	*/
/* format.																						*/
/*																									*/
/*	int FormatBkupDsk( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/* RETURNS:		int	- OK if disk is formatted successfull else and error	*/
/*							  code from the format function or ABORT if the user	*/
/*							  chooses this at on of the prompts.						*/
/*																									*/
/*	GLOBALS:		chDestin	-	Should contain the DOS drive letter on entry		*/
/*																									*/
/***************************************************************************/

int FormatNewDsk( void )
{
	extern unsigned char	chDestin;
	int						iStatus;
	int						iDskType;
	int						iDrv;
	extern struct BPB		DskBpb[];


	iDrv = chDestin - 'A';

	if ( iDskFmtType == 0 )
	{
		if ( (iDskType = GetDskFmtType()) == ABORT )
			iStatus = ABORT;
	}
	else
		iDskType = iDskFmtType;

	if ( iStatus != ABORT )
		if ( (iStatus = FormatFloppy( iDrv, iDskType, FmtStat )) == OK )
			memcpy( &Bpb, DskBpb + iDskType, sizeof( struct BPB ) );
	
	return( iStatus );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

int GetDskFmtType( void )
{
	char			*pchBuf;
	int			iDrvType;
	int			iDskIndex;
	int			iFmtType;
	int			iSaveDefault;
	static unsigned char	DrvOpts[4][2] = {	{ 3,0 },		/* 360K	*/
														{ 3,5 },		/* 1.2M	*/
														{ 4,0 },		/* 720K	*/
														{ 4,6 }};	/* 1.44M	*/

	pchBuf = GetMemory( 100 );							/* Ioctl work buffer		*/
	iDrvType = (int)GetDriveType( (UCHAR)iDrv, pchBuf );
	iDrvType--;												/* Convert to zero base	*/

								/* 360k or 720k only allow a single disk type	*/

	if ( iDrvType == 0 || iDrvType == 2 )
	{
		iDskIndex = 0;
		iSaveDefault = TRUE;					/* Only 1 type so make it default */
	}

							/* Let user choose the format type */
	else if ( (iDskIndex = PromptForDiskFmt( iDrvType >> 1 )) != -1 )
	{
		if ( PromptForDefault() == TRUE ) 	/* Does user want to make 		*/
			iSaveDefault = TRUE;					/* this the default format?	*/
		else
			iSaveDefault = FALSE;
	}

	if ( iDskIndex != -1 )
	{
		iFmtType = DrvOpts[iDrvType][iDskIndex];
		if ( iSaveDefault == TRUE )
				iDskFmtType	= iFmtType;				/* Save global default */
	}

	FreeMemory( pchBuf );
	return( iDskIndex == -1 ? ABORT : iFmtType );
}


/***************************************************************************/
/*																									*/
/***************************************************************************/

int ProcDiskErr( int ErrorType )
{
	static char		*szText[3] = { NULL, NULL, NULL };
	char				*szError[ DISK_ERROR_LINES ];

	GetMessage( szError, DISK_ERROR_TEXT );
	szText[ 0 ] = szError[ ErrorType ];
	szText[ 1 ] = szError[ 0 ];
	Error( szText );
	return( REDO_DISK );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

void BkupDskPrompt( int DiskNum )
{
	extern unsigned char	chDestin;
	char						*szText[ DISK_LABEL_LINES ];
	char						szLabel[ 20 ];

	GetMessage( szText, DISK_LABEL_TEXT );
	strcpy( szLabel, szText[0] );
	itoa( DiskNum, strchr( szLabel, EOL ), 10 );
	
	PromptForDisk( szLabel, chDestin, TRUE );
}

/***************************************************************************/
/* Determines if a disk has already been used during the current backup		*/
/* seqence. Each backup disk has a label and the time and date on the		*/
/* along with the label sequence number to see if the label was created		*/
/* during the current 																		*/
/*																									*/
/***************************************************************************/

int IsNewDisk( int DiskNum )
{
	extern unsigned char	chDestin;

	char				szLabel[15];				/* Disk label string 	*/
	int				iStatus;						/* Return value			*/
	int				VolNum;						/* Decimal disk number	*/
	long				lTime;						/* Label creation time	*/
	extern unsigned long	StartTime;			/* Time-date of first disk	*/
	extern unsigned long	LastTime;			/* Time-date of last disk	*/

	iStatus = OK;

	if ( (lTime = GetDiskLabel( chDestin - 'A', szLabel, &Bpb )) != 0L &&
		  lTime != -1L )
	{
		if ( (VolNum = atoi( szLabel + 8 )) > 0 )
			if ( VolNum < DiskNum )
			{
				if ( lTime >= StartTime && lTime <= LastTime )
						iStatus = DISK_USED;
			}
	}
	else if ( lTime == -1 )
		iStatus = BAD_MEDIA;

	return( iStatus );
}


/***************************************************************************/
/*																									*/
/***************************************************************************/

int DisplayDiskTotals( int iTotal )
{
	#define		BOX_TOP			(TITLE_ROW + CONF_BKUP1_LINES - 1)
	#define		BOX_BOT			(BOX_TOP + TOTALS_LINES - 2)
	#define		BOT_ROW			(BOX_BOT + 1 )

	int			c;
   char			*apszText[ MAX_STRINGS ];
	char			*szTmpLine, *szPtr;
	int			i;
	int			Totals[ 4 ];
	long			lTmp;

	szTmpLine = GetMemory( 120 );

	Totals[0] = iTotal;
	Totals[1] = (iTotal + 1) / 2;

	lTmp = (long)iTotal;
	lTmp *= 362496L;
	lTmp += 1213951L;
	lTmp /= 1213952L;
	Totals[2] = (int)( lTmp );

	Totals[3] = (iTotal + 3) / 4;

	PushHelp( CONF_BKUP1_HELP_TEXT );
	WorkAreaCls();

	HelpLine( CONT_HLP | F1_HLP | EXIT_HLP | PREV_HLP );

	GetMessage( apszText, CONF_BKUP1_TEXT );
	DisplayText( apszText, TITLE_ROW );

	GetMessage( apszText, TOTALS_TEXT );

	szPtr = szTmpLine + TOTALS_TEXT_OFFSET; // IPG- Defined in window.h
	for ( i = 0; i < (TOTALS_LINES - 1); i++ )
	{
		strcpy( szTmpLine, apszText[i] );
		itoa( Totals[i], szPtr, 10 );
		VideoPutsRowCol( BOX_TOP + i, START_COL, szTmpLine );
	}

	do
		c = GetChar();
	while ( c != CR && c != ESC );

	PopHelp();
	FreeMemory( szTmpLine );
	return( c == CR ? OK : PREVIOUS );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

void CalcPrompt( void )
{
	char	*szText[ CALCULATE_LINES ];

	WorkAreaCls();
	HelpLine( 0 );
	GetMessage( szText, CALCULATE_TEXT );
	PromptWindow( szText, NULL, GetPromptColor(), NULL );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

void NoFilesPrompt( void )
{
	char	 		*szText[ NO_FILES_LINES ];
	
	GetMessage( szText, NO_FILES_TEXT );
	PromptWindow( szText, CR_Response, GetPromptColor(), NULL );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

int GetCurrentFileStr( char *szStr )
{
	strcpy( szStr, szOldFileName );
	return( IsReadWrite );
}
/***************************************************************************/
/*																									*/
/***************************************************************************/

void ShowFileStatus( char *szFileName, int ReadWrite )

{
	IsReadWrite = ReadWrite;
	strcpy( szOldFileName, szFileName );

	DisplayFileStatus( szFileName, ReadWrite );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

void NotFormattedPrompt( void )
{
	char	 		*szText[ NO_FORMAT_LINES ];
	
	GetMessage( szText, NO_FORMAT_TEXT );
	PromptWindow( szText, CR_Response, GetPromptColor(), NULL );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

int WantToExit( void )
{
	char			*szText[ WANT_TO_EXIT_LINES ];

	int			Input;

	GetMessage( szText, WANT_TO_EXIT_TEXT );
	Input = PromptWindow( szText, ExitResponse, GetPromptColor(), NULL );

	Input = toupper( Input );
	return( Input == UCASE_REPEAT ? -1 : OK ); // IPG- changed UCASE_R
                                                    // IPG_ to UCASE_REPEAT
}


