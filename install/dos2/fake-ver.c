/***************************************************************************/
/* 																								*/
/* FAKE-VER.C																					*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* This module contains the functions which read in the version table		*/
/* from SETVER.EXE and then updates the table with new entries and 			*/
/* writes it back to the file.															*/
/*																									*/
/* The fake version table is located in the SETVER.EXE file and it's			*/
/* location and length are specified right after the device header in		*/
/* the executable part of the file.														*/
/* 																								*/
/* Created 			12-06-89 -  johnhe													*/
/* Major change 	04-18-90	-  johnhe													*/
/*										Ported from SERVER.C to support the new		*/
/*										lie table format in DOS 5.00 which moved		*/
/*										the table from the end of MSDOS.SYS to a		*/
/*										a random location within the file and			*/
/*										deleted the FAKE COUNT field.						*/
/* Major change	01-04-91	- johnhe														*/
/*									  Move the table from MSDOS.SYS to					*/
/*									  SETVER.EXE.												*/
/*																									*/
/*	Major change	08-10-91	- New code to copy the version table from the	*/
/*									  OLD_DOS.xxx directory if it's compatible m113	*/
/***************************************************************************/

#include 	<stdio.h>
#include 	<string.h>
#include 	<dos.h>
#include 	<io.h>
#include 	<fcntl.h>
#include		<malloc.h>															/* m113	*/

#include		<alias.h>
#include		<global.h>
#include 	<disk_io.h>
#include		<strlib.h>
#include 	<data.h>

/***************************************************************************/

#define	MAX_NAME_LEN		13
#define	MAX_ENTRY_SIZE		(MAX_NAME_LEN + 1 + 2 + 1)

struct ExeHeader
{
	UINT		Signature;
	UINT		LastPageLen;
	UINT		TotalFilePages;
	UINT		NumRelocEntries;
	UINT		HeaderParas;
	UINT		MinEndParas;
	UINT		MaxEndParas;
	UINT		StackSeg;
	UINT		StackPtr;
	UINT		NegChkSum;
	UINT		IndexPtr;
	UINT		CodeSeg;
	UINT		RelocTblOffset;
	UINT		OverlayNum;
};

struct DevHeader
{
	char far		*NextDevice;
	unsigned		DeviceAttrib;
	char near	*Strategy;
	char near	*Entry;
	char			Name[ 8 ];
	char			VersMinor;
	char			VersMajor;
	long			TblOffset;
	unsigned		TblLen;
};

/***************************************************************************/

static char		szPath[ MAX_PATH ];
static char		*LieBuffer;		/* Buffer to read lietable into	*/
static char		*NextEntry; 	/* Ptr to next free entry in lietable buffer */
static int 		iFile;

struct ExeHeader		ExeHdr;
struct DevHeader		DevHdr;
static char				*szSetVer = "SETVERXX";
long						FileOffset;

static unsigned		OldTblLen;												/* m113	*/
static char				*OldLieBuffer;											/* m113	*/

/***************************************************************************/

extern void	UpdateLie		( void );
int			SeekRead			( void *Buf, long lOffset, unsigned uBytes );

static int near	ReadLieTable	( void );
static int near	WriteLieTable	( void );
static int near	FindLastEntry	( void );
static void near	AddToTable		( char *szFile );

static int near	ReadVersionTable( void );
static int near	WriteVersionTable( void );
static int near	LoadOldTable	( void );								/* m113	*/

/***************************************************************************/
/* Gets all strings in the LIE_TO list for this OEM and adds them to the	*/
/* lie table in the SETVER.EXE file on the boot drive in the DOS directory	*/
/* m113 - If there is a version table in the OLD_DOS.xxx directory it will	*/
/*			 be used instead.																	*/
/*																									*/
/* void UpdateLie( void )																	*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void UpdateLie( void )
{
	register			i; 								/* Loop indice 					*/
	int				iStatus; 						/* Keep track of any errors	*/
	char				*szString;						/* File name to lie to			*/
	char				*MaxEntry;

	if ( LoadOldTable() == OK )												/* m113	*/
	{																					/* m113	*/
		iStatus = ERROR;															/* m113	*/
	   BuildPath( szPath, vInfo.szPath[0], vInfo.szPath + 2,			/* m113	*/
					  SETVER_STR );												/* m113	*/
		if ( ReadVersionTable() == OK )										/* m113	*/
		{																				/* m113	*/
			memset( LieBuffer, 0, DevHdr.TblLen );							/* m113	*/
			memcpy( LieBuffer, OldLieBuffer,									/* m113	*/
					  (DevHdr.TblLen < OldTblLen ?							/* m113	*/
						DevHdr.TblLen : OldTblLen) );							/* m113	*/
			FreeMemory( OldLieBuffer );										/* m113	*/
			iStatus = WriteVersionTable ();									/* m113	*/
			_dos_close( iFile );													/* m113	*/
			FreeMemory( LieBuffer );											/* m113	*/
		}																				/* m113	*/
		if ( iStatus != OK )														/* m113	*/
			FatalError( LIE_TABLE_UPDATE_ERROR );							/* m113	*/
		return;																		/* m113	*/
	}																					/* m113	*/
									/* Make sure some lie-to files were specified	*/
									/* for this OEM in the dosdata file 				*/

	if ( (szString = GetDataString( LIE_TO, 0 )) == NULL )
		return;
   BuildPath( szPath, vInfo.szPath[0], vInfo.szPath + 2, SETVER_STR );

	LieBuffer = NULL;
	iStatus = ERROR;
	if ( ReadVersionTable() == OK )				/* Read in the old lie table	*/
	{
		NextEntry = LieBuffer;
		MaxEntry = LieBuffer + (DevHdr.TblLen - MAX_ENTRY_SIZE);

															/* Find where new entries go	*/
		while( *NextEntry != 0 && NextEntry < MaxEntry )
			NextEntry += (*NextEntry + 3);

									/* Set up loop to get all the	lie-to strings		*/
		for ( i = 0;
				(szString = GetDataString( LIE_TO, i )) != NULL &&
				NextEntry <= MaxEntry;
				i++ )
			AddToTable( szString );

		iStatus = WriteVersionTable ();				/* Write file back to disk	*/
	}

	if ( iFile )
		_dos_close( iFile );
	if ( LieBuffer != NULL )
		FreeMemory( LieBuffer );

	if ( iStatus != OK )
		FatalError( LIE_TABLE_UPDATE_ERROR );	/* Error was detected			*/
}

/***************************************************************************/
/* Add a file name to the lie table buffer. Copies up to 12 character in	*/
/* in the file name and then set the length of the string, major and minor */
/* version numbers and fake count.														*/
/* 																								*/
/* void AddToTable( char *szFile )														*/
/* 																								*/
/* ARGUMENTS:	szFile	- Ptr to file name to add to the list					*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void near AddToTable( char *szFile )
{
	char			*Ptr;
	register 	iLen;

	Ptr = NextEntry + 1;
	strupr( szFile );
	strncpy( Ptr, szFile, MAX_NAME_LEN );		/* Copy the entry filename */

	iLen = (int)strlen( Ptr );						/* Get number bytes copied */
	*NextEntry = (char)iLen;						/* Set length of field		*/

	NextEntry++;										/* Add size of len field	*/
	NextEntry += iLen;								/* Add len of file name		*/
	*(NextEntry++) = (char)vInfo.uchVersMajor;/* Copy major version # 	*/
	*(NextEntry++) = (char)vInfo.uchVersMinor;/* Copy minor versoin # 	*/
}

/***************************************************************************/
/* Opens the SETVER.EXE file and reads in the .EXE header to find the		*/
/* start of the code and then reads in the device header there which has	*/
/* the offset and length of the lie table.											*/
/*																									*/
/*	int ReadVersionTable( void )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		int	- OK if successful else error code							*/
/*																									*/
/***************************************************************************/

int near ReadVersionTable( void )
{
	register		iStatus;						/* Function's return value				*/
	unsigned		uRead;						/* Number of bytes read from file	*/

			/* Open the file and read in the max buffer len from stack seg		*/
	if ( _dos_open( szPath, O_RDWR, &iFile ) != OK )
		return( ERROR );

	iStatus = ERROR;

	if ( _dos_read( iFile, &ExeHdr, sizeof( ExeHdr ), &uRead ) == OK &&
		  uRead == sizeof( ExeHdr ) )
	{
		/* m113 Can't assume FileOffset was 0 if function is called twice		*/
		/* m113	FileOffset += (long)(ExeHdr.HeaderParas * 16);					*/

		FileOffset = (long)(ExeHdr.HeaderParas * 16);					/* m113	*/

		if ( SeekRead( &DevHdr, FileOffset, sizeof( DevHdr ) ) == OK )
		{
			if ( strncmp( DevHdr.Name, szSetVer, 8 ) == OK &&
				  DevHdr.VersMajor >= 1 )
			{
				FileOffset += DevHdr.TblOffset;
				LieBuffer = GetMemory( DevHdr.TblLen );
				if ( SeekRead( LieBuffer, FileOffset, DevHdr.TblLen ) == OK )
					iStatus = OK;
			}
		}
	 }
	return( iStatus );
}

/***************************************************************************/
/* Writes the version table to the already open SETVER.EXE file.				*/
/*																									*/
/*	int WriteVersionTable( void )															*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		int	- OK if successful else ERROR									*/
/*																									*/
/***************************************************************************/

int near WriteVersionTable( void )
{
	unsigned			uWritten;				/* Number of bytes written to file	*/

	if ( _dos_seek( iFile, FileOffset, SEEK_SET ) == FileOffset &&
		  _dos_write(iFile, LieBuffer, DevHdr.TblLen, &uWritten ) == OK &&
		  uWritten == DevHdr.TblLen )
		return( OK );
	else
		return( ERROR );
}

/***************************************************************************/
/* Seeks to the specified offset in a file and reads in the specified		*/
/* number of bytes into the caller's buffer.											*/
/*																									*/
/*	unsigned SeekRead( char *Buf, long lOffset, unsigned uBytes )				*/
/*																									*/
/*	ARGUMENTS:	Buf		- Ptr to read buffer											*/
/*					lOffset	- Offset in file to start reading at					*/
/*					uBytes	- Number of bytes to read									*/
/*	RETURNS:		unsigned	- OK if successfull else ERROR							*/
/*																									*/
/***************************************************************************/

int SeekRead( void *Buf, long lOffset, unsigned uBytes )
{
	unsigned		uRead;

	if ( _dos_seek( iFile, lOffset, SEEK_SET ) == lOffset &&
		  _dos_read( iFile, Buf, uBytes, &uRead ) == OK &&
		  uRead == uBytes )
		return( OK );
	else
		return( ERROR );
}

/***************************************************************************/
/*	Searches for a SETVER.EXE file in the OLD_DOS.xxx directory and if one	*/
/* is found and it's compatible with DOS 5.0 the table will be copied to	*/
/* to OldLieBuffer.																			*/
/*																									*/
/*	int LoadOldTable( void )																*/
/*																									*/
/*	ARGUEMENT:	NONE																			*/
/*	RETURNS:		int	-	OK if a table was loaded into OldLieBuffer			*/
/*								else ERROR														*/
/***************************************************************************/

int near LoadOldTable( void )													/* m113	*/	
{																						/* m113	*/	
	int		iStatus = ERROR;													/* m113	*/

   BuildPath( szPath, vInfo.szTmpDir[0], vInfo.szTmpDir + 2,		/* m113	*/
				  SETVER_STR );													/* m113	*/	
	LieBuffer = NULL;																/* m113	*/	
	if ( ReadVersionTable() == OK )											/* m113	*/
	{																					/* m113	*/
		OldTblLen = DevHdr.TblLen;												/* m113	*/
		OldLieBuffer = LieBuffer;												/* m113	*/
		iStatus = OK;																/* m113	*/
	}																					/* m113	*/
	else if ( LieBuffer != NULL )												/* m113	*/
	{																					/* m113	*/
		FreeMemory( LieBuffer );												/* m113	*/
		LieBuffer = NULL;															/* m113	*/
	}																					/* m113	*/

	if ( iFile )																	/* m113	*/
	{																					/* m113	*/
		_dos_close( iFile );														/* m113	*/
		iFile = 0;																	/* m113	*/	
	}																					/* m113	*/	

	return( iStatus );															/* m113	*/	
}																						/* m113	*/	

