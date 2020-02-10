/***************************************************************************/
/* 																								*/
/* FMT_IO.H																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Include file with definitions for RUP upgrade utility.						*/
/* They are used by the functions in FMT_IO.C										*/
/*																									*/
/* Created 07-13-89 johnhe																	*/
/*																									*/
/***************************************************************************/


/***************************************************************************/

#define		DOS_O_RDWR 				2

#define		LAST_DISK				256
#define		REDO_DISK				257
#define		NOT_FOUND				258


/***************************************************************************/
/* Function prototypes for all functions in  FMT_IO.C								*/
/***************************************************************************/

void	DispInsertUserDisk	( int UserDisk, char chDrive );

long	CalcDiskFree			( void );
int	PrepNewDsk				( void );
int	IsNewDisk				( int iDrv );

int	FormatNewDsk			( void );
int	FoundUsedDiskPrompt	( void );
int	FormatErrorPrompt		( void );
/* int	PromptForDiskFmt		( int iDrvType );	*/
int	PromptForDefault		( void );

int	FormatNewDsk			( void );
int	DisplayDiskTotals		( int iTotal );
void	BkupCompletePrompt( void );

