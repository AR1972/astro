/***************************************************************************
 * AUTOFMT.C
 *
 * Microsoft Confidential
 * Copyright (c) Microsoft Corporation 1990-1991
 * All Rights Reserved
 *
 * Functions for automatically spawning format for all unformatted hard
 * disks.
 *
 * Change log:
 *
 *   	Date       #		Description
 * 	--------  ----	----------------------------------------------------
 *      02/18/90        Created
 *      02/27/91  M001  Fix szFormat offset in GetRealSrcDisk().
 *      02/28/91  M002  Call DisplayFormatScreen() every iteration of format
 *                      loop in HdFormatLoop().
 ***************************************************************************/

#include 	<stdio.h>
#include 	<stdlib.h>
#include 	<bios.h>
#include 	<memory.h>
#include		<dos.h>
#include		<fcntl.h>
#include 	<string.h>
#include 	<process.h>

#include 	<alias.h>
#include		<oem.h>
#include 	<message.h>
#include 	<bios_io.h>
#include 	<disk_io.h>
#include		<hdisk.h>
#include 	<window.h>
#include		<data.h>
#include		<format.h>
#include		<nudoswin.h>

/***************************************************************************/

extern char	*SectorBuf;        /* sector work buffer declared in initial.c */
extern char	*BootSecBuf;       /* sector work buffer declared in initial.c */
extern struct HdInfo *HdInfo;  /* declared in initial.c                    */

static int	StatusRow;								/* Format status screen row	*/
static int	StatusCol;								/* Format status screen col	*/
static char	*StatusStr; 							/* Ptr to status string			*/

/***************************************************************************/

static int	HdFormatLoop	( char FirstHd );

static void	(interrupt far	*OldInt2f)();		/* Original int 2fh addr.		*/

void interrupt far NewInt2f( unsigned es, unsigned ds,
			unsigned di, unsigned si, unsigned bp, unsigned sp,
			unsigned bx, unsigned dx, unsigned cx, unsigned ax );

static int	DoubleSpaceInUse	(void);

/***************************************************************************/
/* Function to automatically format all hard disks in a system which have	*/
/* unformatted parititions.																*/
/*																									*/
/*	void AutoHdFormat( void )																*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		int	- OK if primary partition is OK else ERROR				*/
/* 																								*/
/***************************************************************************/

int AutoHdFormat( void )
{
	register						iStatus;

	OldInt2f = _dos_getvect( 0x2f );						/* Set our message hook	*/
	_dos_setvect( 0x2f, NewInt2f );

	iStatus = HdFormatLoop( vInfo.chFirstHd );

	_dos_setvect( 0x2f, OldInt2f );						/* Restore orig int 2f	*/

	if ( iStatus == OK )
	{
		if (GetHardDiskInfo() == 0 || HdInfo->DiskError == TRUE)
			iStatus = ERROR;
	}

	return( iStatus );
}

/***************************************************************************/
/* This is an interrupt function which will be used by the format program	*/
/* update the format status window on the screen. It is passed the			*/
/* percent of format completed in AX. All registers are preserved by the	*/
/* interrupt handler except AX which returns 0 for OK or !0 if aborting	   */
/* the format (not implemented).															*/
/* 																								*/
/*	ENTRY						- AX == 0x4900													*/
/*								  BX == Percent of format complete						*/
/*	REGISTERS DESTROYED	- NONE															*/
/* 																								*/
/***************************************************************************/
																				/*lint -e715	*/
void interrupt far NewInt2f( unsigned es, unsigned ds,
			unsigned di, unsigned si, unsigned bp, unsigned sp,
			unsigned bx, unsigned dx, unsigned cx, unsigned ax )
{
	char		szPercent[ 10 ];
	char		*szPtr;
	char		*szRevPtr;

	((void)es), ((void)ds),	((void)si),	((void)bp), ((void)sp);
	((void)di), ((void)dx), ((void)dx), ((void)cx);

	if ( ax != 0x4900 )
		_chain_intr( OldInt2f );

	szPtr = strchr( StatusStr, '%' ) - 1;

	itoa( (int)bx, szPercent, 10 );
	szRevPtr = strchr( szPercent, EOL ) - 1;

	while( szRevPtr >= szPercent )
		*(szPtr--) = *(szRevPtr--);

	while( szPtr >= StatusStr )
		*(szPtr--) = ' ';

	VideoPutsRowCol( StatusRow, StatusCol, StatusStr );

	ax = 0;						/* Signal everything ok to continue formatting	*/
}																				 /*lint +e715	*/

/***************************************************************************
 * Displays the auto format screen.														
 *																									
 *	void DisplayFormatScreen( unsigned uText, char chDrv )
 *																									
 *	ARGUMENTS:	uText - Offset of message text to use.
 *					chDrv - Drive letter ('A','B','C',...)
 *	RETURN:		void																			
 *																									
 ***************************************************************************/

void DisplayFormatScreen( unsigned uText, char chDrv )
{
	char						*apszPrompt[ HD_FMT_STAT_LINES ];
	char						szTmp[ MAX_SCRN_LINE_LEN ];
	struct WindowStruct	Wind;

	NewScreen( uText, 0 );
	GetMessage( apszPrompt, HD_FMT_STAT_TEXT );

	sprintf( szTmp, apszPrompt[2], chDrv );

	GetWindowInfo( apszPrompt, &Wind );					/* Display a window 		*/
	Wind.Color = (char)(GetPromptColor());
	Wind.BorderColor = (char)(GetPromptColor());
	PutWindow( &Wind );

	VideoPutsRowCol( Wind.Top+2, CenterStr(szTmp), szTmp );

	StatusStr = apszPrompt[ 3 ];
	StatusRow = Wind.Top + 3;
	StatusCol = CenterStr( StatusStr );
}


/***************************************************************************/
/* Function which loops through all drive letters starting from the first	*/
/* fixed disk and checks if the drive is partitioned and if not will 		*/
/* spawn format to format the disk. Requires that the new int 2fh handler	*/
/* has already been installed so that format can return the format status.	*/
/* Will update the prompt with the current drive letter for each disk		*/
/* that needs formatting. An immediate return of error is done if there		*/
/* are any errors on the first drive letter because this would mean a		*/
/* bad primary parition.																	*/
/*																									*/
/*	void HdFormatLoop( char FirstHd )													*/
/*																									*/
/*	ARGUMENTS:	FirstHd	- Drive letter of first hard disk in the system		*/
/*	RETURNS:		int		- OK if first paritition is alright else ERROR		*/
/*																									*/
/* NOTE: Need to remember TRUE == 1, FALSE == 0, OK == 0 & ERROR == -1		*/
/***************************************************************************/

int HdFormatLoop( char FirstHd )
{
	static char			*szFormat = FORMAT_STR;
	char					CurrentDisk;
	register				ProgDisk;
	register				iStatus;
	static char			*apszSwitches[] = { FORMAT_STR, "X:", "/U",
													  "/SELECT", "/V:DOS", NULL };

	ProgDisk = GetRealSrcDisk( szFormat + 3, 0 );	/* M001 */
	*szFormat = vInfo.chSource;

   /* We need to set the unformatted media bit for each partition here, so
   ** the IsValidHardDrive() call can be completed successfully. */
	for (CurrentDisk = FirstHd; CurrentDisk <= 'Z'; CurrentDisk++)
      EnableDiskAccess((UCHAR)CurrentDisk - 'A');

	for( CurrentDisk = FirstHd; IsValidHardDrive( CurrentDisk ); CurrentDisk++ )
	{
		if ( (iStatus = HasValidFormat( CurrentDisk )) == FALSE )
		{
         /* Warn the user that we are about to format an unformatted hard
         ** disk partition.  Give them a choice between formatting the
         ** partition and bailing out of Setup.  The fAskPartPrompt flag in
         ** vInfo is initialized to TRUE in initial.c and only changed to
         ** FALSE if the user selects "Allocate all free space for MS-DOS" in
         ** prompts.c!GetPartOpts().
         */
         if (vInfo.Flag.fAskPartFormat == TRUE)
            UnformattedPartPrompt(CurrentDisk);

			DisplayFormatScreen( HD_FMT_TEXT, CurrentDisk );
			DoInt2f( 0 ); 								/* Prompt with 0% formatted	*/
			DispInsertDisk( ProgDisk );

			apszSwitches[1][0] = CurrentDisk;
			iStatus = spawnv( P_WAIT, szFormat, apszSwitches );

         if (iStatus == OK)
            /* Make a note that this drive has been formatted, so we can
            ** leave the state of its unformatted media bit set properly.
            */
            NowFormatted(CurrentDisk);
		}
		else if ( iStatus != ERROR )
			iStatus = OK;

		if ( iStatus != OK )
		{
			if ( CurrentDisk == FirstHd )
				return( ERROR );
			else
				FormatError( CurrentDisk );
		}
	}
	return( OK );
}

/***************************************************************************/
/* Reads in the boot record of a drive using the specified drive letter		*/
/* and then does a call to see if the boot record is formatted.				*/
/*																									*/
/*	int HasValidFormat( char DrvLetter )												*/
/*																									*/
/*	ARGUMENTS:	DrvLetter	- Dos drive letter of drive to check				*/
/*	RETURNS:		int			- TRUE if valid formatted boot record				*/
/*									  FALSE if not a valid boot record					*/
/*									  ERROR if couldn't read the boot sector			*/
/*																									*/
/***************************************************************************/

int HasValidFormat( char DrvLetter )
{
	if ( ReadWriteBoot( DrvLetter - 'A', BootSecBuf, READ ) != OK )
		return( ERROR );
	else if ( IsFmtedBoot( BootSecBuf ) == TRUE )
		return( TRUE );
	else
		return( FALSE );
}

/***************************************************************************/
/*																									*/
/*	Makes a disk formatted under DOS 2 or DOS 3 (< 3.31) bootable under     */
/* DOS 5.                                                                  */
/*																									*/
/*    The extended BPB was introduced with DOS 3.31, and added 6 bytes to  */
/* end of the regular BPB.  The extended BPB changed the last BPB entry    */
/* from an unsigned word (unsigned int) into an unsigned dword (an         */
/* unsigned long -- called ulHiddenSec) and added an unsigned dword entry  */
/* following it called ulTotalBigSecs.  This change only causes a boot     */
/* problem for the DOS 4 and DOS 5 boot loaders on DOS 2 and DOS 3         */
/* formatted disks because the 6 bytes added to the old BPB to create the  */
/* extended BPB were used for other purposes in DOS 2 and DOS 3, and so    */
/* contain garbage values as far as DOS 4 and DOS 5 are concerned.  Only   */
/* the bogus ulHiddenSec value actually causes problems for the DOS 4 and  */
/* DOS 5 boot loaders, since it causes them to compute an erroneously      */
/* large offset into the disk to look for IO.SYS.                          */
/*																									*/
/*    We only need to correct the ulHiddenSec value to make the disk       */
/* bootable under DOS 4 and DOS 5, but we fix up the ulTotalBigSecs entry  */
/* for anyone else who might use it.  (Although they shouldn't use it if   */
/* uTotalSectors != 0U.)                                                   */
/*																									*/
/*    We begin by copying the RelativeSector entry from the partition's    */
/* entry in the partition table to the ulHiddenSec entry in the new        */
/* extended BPB.  Then, if the uTotalSectors entry is non-zero, we assume  */
/* it is correct and zero out the ulTotalBigSecs entry in the new extended */
/* BPB.  If the uTotalSectors entry is zero, we copy the TotalSectors      */
/* entry from the partition table into the ulTotalBigSecs entry in the new */
/* extended BPB.  The media descriptor byte is always made 0xf8.  We also  */
/* make sure the system indicator byte in the partition table is changed   */
/* to 6 for DOS 5 if necessary.                                            */
/*																									*/
/*    Of course, this disk will now no longer be bootable under DOS 2 or   */
/* DOS 3 since we will have destroyed some of the data used by those       */
/* earlier versions of the boot loader.                                    */
/*																									*/
/*																									*/
/*	int FixUpBPB(char DriveLetter)     	   											*/
/*																									*/
/*	ARGUMENTS:	DriveLetter	- DOS drive letter of boot drive to fix 			*/
/*									  (This better be correct or we'll fix up the   */
/*                            wrong BPB, and fail to fix up the boot BPB.) */
/*	RETURNS:		int         - TRUE if fix was successful,                   */
/*                           FALSE if not                                  */
/*																									*/
/***************************************************************************/

int FixUpBPB(char DriveLetter)
{
	struct ABSIO_PACKET absPack;
   int BootPart;        /* partition table offset of boot partition [0..3] */

   /* BootSecBuf is filled in with the boot sector by HasValidFormat().    */
   struct BPB  *pBPB  = (struct BPB *) (BootSecBuf + BPB_OFFSET);

   /* SectorBuf is filled in with the MBR by RdWrSector().                 */
	struct Part *pPart = (struct Part *)(SectorBuf  + TABLE_OFFSET);


   /* First make sure the drive has an ok DOS format, then look at the boot
   ** sector which has been loaded into the global BootSecBuf by
   ** HasValidFormat().
   */
   if (HasValidFormat(DriveLetter) == TRUE)
   {
      /* Read the partition table into the global SectorBuf to get the boot
      ** partition's RelativeSector and TotalSectors values.
      */
   	if (RdWrSector(0x80, 0, 0, 1, READ) != OK ||
          (BootPart = GetBootPart((struct PartTable *)pPart)) == NO_DOS_PART)
         return(FALSE);

      /* Set up pPart to point to boot partition entry in partition table. */
      pPart += BootPart;

      /* Copy RelativeSector entry from partition table to ulHiddenSec entry
      ** in BPB.
      */
      pBPB->ulHiddenSec = pPart->RelativeSector;

      /* Make sure one and only one of the uTotalSectors and ulTotalBigSecs
      ** entries is zero.
      */
      if (pBPB->uTotalSectors != 0U)
         /* Assume uTotalSectors is correct. */
         pBPB->ulTotalBigSecs = 0UL;
      else
         pBPB->ulTotalBigSecs = pPart->TotalSectors;

      /* Make sure the media descriptor byte is HD_MEDIA_BYTE (0xf8).  Some
      ** versions of DOS, e.g., Tandy, put 0xfa in this field.
      */
      pBPB->uchMediaDescr = HD_MEDIA_BYTE;

      /* Set up absPack for BPB write. */
	   absPack.lStartSector = 0L;
	   absPack.uNumSectors = 1;
	   absPack.pchBuffer = BootSecBuf;

		if ( FALSE == DoubleSpaceInUse() )
			{	/* Write out the patched BPB. */
				if (AbsReadWrite(DriveLetter - 'A', &absPack, WRITE) != OK)
					return(FALSE);
			}

      /* Make sure the system indicator entry in the partition table is
      ** changed to type 6 if appropriate for this partition.
      */

      if (pPart->SystemIndicator != DOSNEW &&
			 (pBPB->uTotalSectors + pBPB->ulTotalBigSecs + pBPB->ulHiddenSec)
          > (unsigned long)DOS_MAX)
      {
         pPart->SystemIndicator = DOSNEW;
   	   if (RdWrSector(0x80, 0, 0, 1, WRITE) != OK)
            return(FALSE);
      }

      /* The fix-ups and writes have gone ok. */
      return(TRUE);
   }
   else
      /* We don't recognize this boot sector format, so bail out without
      ** making any changes.
      */
      return(FALSE);
}


/***************************************************************************/
/* Determines wether DoubleSpace disk compression technology is being used.*/
/*                                                                         */
/* int DoubleSpaceInUse (void)                                             */
/*                                                                         */
/* ARGUMENTS:  NONE                                                        */
/* RETURNS;    int      - TRUE if DoubleSpace is in use.                   */
/***************************************************************************/

int DoubleSpaceInUse (void)
{
   union REGS regs;
   int  fDoubleSpace; /* TRUE if this drive is a DS drive      */

   fDoubleSpace = FALSE;

   /* If the INT 2Fh vector is null, return FALSE */
   if (_dos_getvect (0x2f) == NULL)
	   return ( fDoubleSpace );

   /* Ask DoubleSpace if it exists */
   regs.x.ax = 0x4A11;
   regs.x.bx = 0x0000;
   int86 (0x2F, &regs, &regs);

   /* BX == 444Dh if DoubleSpace exists */
   if (regs.x.bx == 0x444D)
   	{	/* DoubleSpace is running on this system */
      fDoubleSpace = TRUE;
	   }

   return ( fDoubleSpace );
}

