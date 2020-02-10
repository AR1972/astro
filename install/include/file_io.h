/***************************************************************************/
/*																									*/
/*	FILE_IO.H																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Created 10-28-89 johnhe																	*/
/* Updated 06-09-92 royha	- Added file attributes 									*/
/***************************************************************************/


/* File attributes */

#ifndef SLASH
  #define SLASH(c)     ((c) == '/' || (c) == '\\')
#endif

#ifndef CHSEPSTR
  #define CHSEPSTR                "\\"
#endif

extern unsigned gbPathSearch;

extern int	FileExists		( char *szFile);
extern int	IsDirEmpty		( char *szPath);
extern int	BigReadWrite	( int iFile, char far *Buf, long lBytes, int RdWr);
extern int	RenameFCB		( char *szFrom, char *szTo);
extern int	ReplaceFile 	( char *szSource, char *szDestin);
extern int	AbsUnlink		( char *szFile );
