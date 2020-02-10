/*********************************** GET.H *********************************/
/*                                                                         */
/*	GLOBAL.H																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Global variables and macros for all modules										*/
/*                                                                         */
/* Created 880305 - johnhe                                                 */
/***************************************************************************/

                                                                  //IPG- changed to ipg constants
#define EXIT_RESPONSE_TEXT	{ UCASE_REPEAT, LCASE_REPEAT, \
				  UCASE_YES, LCASE_YES, CR, 0 }   //     defined in window.h
#define	CR_RESPONSE_TEXT  	{ CR, 0 }
int		CR_Response[];


/***************************************************************************/


#define		READ					0
#define		WRITE					1
#define 		DELETE				2
#define		RENAME				3

#define		REBOOT				1

#define     LEAVE_SCREEN      0           /* Don't cleanup screen on exit  */
#define     RESTORE_SCREEN    1           /* Cleanup screen on exit        */

/***************************************************************************/

#define		BAD_MEDIA				1
#define		DISK_USED				2
#define		DIST_DISK				3

#define		FATAL_NO_HARD_ERROR		8

#define		SECTOR_SIZE				512

/************************************************************************/

	/* Flag manipulation macros */

#define	SetFlag( Flags, BitValue )			(Flags |= BitValue)
#define	ClearFlag( Flags, BitValue )		(Flags &= (~BitValue))
#define	ToggleFlag( Flags, BitValue )		(Flags ^= BitValue )
#define	GetFlag( Flags, BitValue )			(Flags &  BitValue)

/***************************************************************************/

/* Program switch flags	*/

#define			ALLOW_IBM			1			/* Flag to allow upgrade on IBM */
#define			FORCE_FLOPPY		2			/* Flag to force floppy upgrade */
#define			BACKUP_DONE			4			/* Flag if spawned by backup	  */
#define			HARD_DISK			8			/* Flag if doing a hard upgrade */

/***************************************************************************/

char			*ErrorBuffer;
void 			*GetMemory( unsigned int Bytes );

/***************************************************************************/

#define		GetErrorBuffer()		ErrorBuffer
#define		SetErrorBuffer( x )  ErrorBuffer = x

/***************************************************************************/

void	AllowAbort( void );
void	ProgramAbort();

/***************************************************************************/


/***************************************************************************/
/* This is a structure containing all of the information needed to perform */
/* a recovery of the user's original operating system. It is saved in the	*/
/* file GLOBAL.H on the recovery disk and is used allow with the some other*/
/* files on the recovery disk to restore the user's original operating		*/
/* system.																						*/
/***************************************************************************/

/***************************************************************************/

typedef struct CurrentFile CF;

struct _INFO	
{
	CF				CurrentFile;			/* Current File structure from above */
	char				chSource;				/* Source drive character */
	char				chDestin;				/* Destination drive character */
	unsigned char	uchVersMajor;			/* Major version number */
	unsigned char	uchVersMinor;			/* Minor version number */
	char				chNumFloppy;			/* Number of floppy drives */
	char				Is720K;
	char				szPath[128];
	char				chFirstHd;
	struct _BIT_FL
	{
		unsigned		fFloppyBoot:1;			/* Booted from Floppy Disk */
	} Flag;
	struct _ARGS
	{
		unsigned		fIsMono		:1;		/* Force to run in mono colors /B	*/
		unsigned		fDoneBackup	:1;		/* Program returned from backup /D	*/
		unsigned		fFloppy		:1;		/* Force a floppy upgrade		 /F	*/
		unsigned		fHardDisk	:1;		/* Is hard disk upgrade			 /H	*/
		unsigned		fNewSysFiles:1;		/* New autoexec & config.sys	 /N	*/
		unsigned		fHelp			:1;		/* Help switch 					 /?	*/
		unsigned		fAllowBadPart:1;		/* Allow upgrading funy partitions	*/
		unsigned		fQuickUp		:1;		/* Only make hard disk bootable		*/
		unsigned		fNoBernoulli:1;		/* Don't set bernoulli int 13h		*/
		unsigned		fRootChkDone:1;
		unsigned		fNoVideoChk	:1;		/* M100 - Disable the video check	*/
	} Args;
}	vInfo;



