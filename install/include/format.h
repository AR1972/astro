/***************************************************************************/
/* 																								*/
/* FORMAT.H																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Include file with definitions for FORMAT.C. and FMT_IO.C						*/
/*																									*/
/* Created 07-23-89 johnhe																	*/
/*																									*/
/***************************************************************************/


#define		OK					0
#define		FALSE				0
#define		TRUE				1

#define		ERR				-1


/***************************************************************************/
/*																									*/
/* Prototypes for functions in FORMAT.C												*/
/*																									*/
/***************************************************************************/

int  			FormatFloppy	( unsigned uDrive, int iFormat,
									  void (*vStatus)( unsigned int, unsigned ) );

static int	WriteSectors	( unsigned uStartSec, unsigned uNumSecs,
									  char *pchBuf );

static int	WriteFat			( void );
static int 	FmtTrack			( void );
static void	InitialFmtFlds	( void );
static void AllocFat			( void );

static int	MarkBadTrack	( void );


static int	GetSysSec		( void );
static int	SetDskParms		( void );


 /***************************************************************************/
/* Function prototypes for all functions in  FMT_IO.C								*/
/***************************************************************************/


extern  void DispInsertDisk(int DiskNum);
extern  void DispInsertUserDisk(int UserDisk,char chDrive);
extern  int PrepNewDsk(char *szLabel, char *szPrompt );
extern  int IsNewDisk(int iDrive);
extern  unsigned long ToLongTd(struct DIR *Dir);
extern  int FormatNewDsk(void );
extern  int GetDskFmtType( void );
