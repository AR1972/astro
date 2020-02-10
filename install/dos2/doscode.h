/***************************************************************************/
/* DOSCODE.H																					*/
/*
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/*	This module serves as the calling interface between the Windows and		*/
/* DOS portition of the DOS/WINDOWS combined Setup program.						*/
/*																									*/
/* 																								*/
/*	05-23-91 johnhe																			*/
/***************************************************************************/


/*
	FUNCTION PROTOTYPES
*/

int  far	IsDosUpgrade				( char *szWinPath );
BOOL far InitDosUpgrade				( void );
void far wsCreateRecoveryFloppy	( void );
int  far	CopyDosFiles				( void );
void far wsBuildConfigPath       ( char *szPath, char *szDeviceName );
void far wsUpgradeHard				( void );
char far * far AutoFixup			( char far *InBuf, int StartWin );
