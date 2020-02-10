/* DOSDATA.C -

		Copyright (c) 1991 - Microsoft Corp.
		All rights reserved.
		Microsoft Confidential


		Program to maintain the data file for the retail upgrade install
		utility.  Usage is as follows:

		DOSDATA filename
			Incorporates the information from the specified text information
			file into the master data file.

		DOSDATA /D oemname version
			Deletes all information for the specified version of DOS
			from the master data file.


		An typical text information file might contain:

			Compaq 3.31
			lie:
			mode.com
			fdisk.exe
			rename:
			mode.com mode40.com
			fdisk.exe fdisk40.exe
			delete:
			print.exe

		The master data file has the format:

			Dos_record_1
			Dos_record_2
			...
			Dos_record_n
			Null_dos_record
			Name_record_1
			Name_record_2
			...
			Name_record_n
			Null_name_record
			Name_entry_1
			Name_entry_2
			...

		A Dos_record is:

			[oem-name][dos-version][data-addr]
			    18          2           4       = 24 bytes

			(The data-addr field is a pointer to the beginning of the
			various data tables for that DOS.)

		A Name_record is a variable length record containing a
			zero-terminated string with a file name or action name.

		A Name_entry is a 1 byte record containing an integer index into
			the name record table.

*/

#include <stdio.h>
#include <fcntl.h>
#include <io.h>
#include <stdlib.h>
#include <string.h>
#include <dos.h>
#include <errno.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <memory.h>
#include "messages.h"


#define	FALSE					0
#define	TRUE					1
#define	OK 					0
#define	EOL					'\0'
#define	USIZE					sizeof( unsigned )


#define OEM_SIZE				20
#define COMMENT_CHAR 		';'
#define OPEN_MODE 			( O_CREAT | O_RDWR | O_BINARY )


typedef char			FLAG;

char			*szEOD			= "###";
char			*szDosDataFile	= "dosdata.dat";
char			*szOldDataFile = "dosdata.bak";


/***************************************************************************/

#define	MAX_OEMS 			256
#define	MAX_STRINGS			1000
#define	MAX_DATA 			4000
#define	MAX_TEXT 			20000U


struct NEW_DDR
{
	char			szOemName[ OEM_SIZE ];		/* OEM name 						*/
	char			MajorVer;						/* Major DOS version number	*/
	char			MinorVer;						/* Minor DOS verison number	*/
	unsigned 	uDataOffset;					/* Offset of start of data 	*/
}nDDR;


struct NEW_DDR	*ddrOemList;		/* Array of DDR structures 				*/
struct NEW_DDR	*pNextOem;

char			*pchStringBuf;			/* Flat buffer to hold the data text	*/
char			*pchNextStr;			/* Ptr to next free data text addr		*/

unsigned		*uDataBuf;				/* Array to hold offset of data text	*/
unsigned		uDataNext;				/* Next free entry in the data array	*/

char			**apszPtr;				/* Araray of ptrs to the data text		*/
unsigned		uNextPtr;				/* Index to next free text list entry	*/

void			*Buf[3];					/* Ptrs to 3 allocated buffers			*/

/***************************************************************************/

void		main					( int cszArg,char * *rgszArg );
void		TrimSz				( char *sz );
void		ExitMsg				( char *szMsg );
void		ExitMsg1				( char *szMsg,void *pParam );
void		GetFileName			( char *szFileName,int cszArg,char * *rgszArg );
char		DoesItExist			( void );


void		InitializeBuffers ( void );
void		ReadOldFile 		( void );
void		GetDosId 			( FILE *pFile, char *szSourceFile );
void		WriteNewFile		( void );
void		ReadSourceData 	( char *szSourceFile );
unsigned GetTextOffset		( char *szText );
void		*GetMemory			( unsigned int	Bytes );

/***************************************************************************/

void main( int cszArg, char **rgszArg )
{	
        char                                    szSourceFile[ 200 ];
	struct find_t		File;

	InitializeBuffers();

	GetFileName( szSourceFile, cszArg, rgszArg );

	if ( !(_dos_findfirst( szDosDataFile, _A_NORMAL, &File )) )
		ReadOldFile();

	ReadSourceData( szSourceFile );

	WriteNewFile();
}
/***************************************************************************/
/* Allocate all needed buffers.															*/
/* 																								*/
/* void InitializeBuffers( void )														*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* 																								*/
/* RETURNS: 	void																			*/
/* 																								*/
/* johnhe 11-10-89																			*/
/***************************************************************************/

void InitializeBuffers( void )
{

	ddrOemList = GetMemory( sizeof( struct NEW_DDR	) * MAX_OEMS );
	pchStringBuf = GetMemory( sizeof( unsigned ) * MAX_TEXT );
	uDataBuf = GetMemory( sizeof( unsigned ) * MAX_DATA );
	apszPtr = GetMemory( sizeof( char *	) * MAX_STRINGS );

	memset( ddrOemList, 0, sizeof( struct NEW_DDR ) * MAX_OEMS );

										/* Initialize all ptrs and indexes in	*/
										/* case starting new file					*/
	pNextOem = ddrOemList;
	pchNextStr = pchStringBuf;
	uDataNext = 0;
	uNextPtr = 0;

	Buf[0] = ddrOemList; 		/* Initialize array of ptrs to buffers */
	Buf[1] = pchStringBuf;
	Buf[2] = uDataBuf;

}

/***************************************************************************/
/* Read in all the data from the existing data file and initialize the		*/
/* array of indexes to the strings in the text buffer. On return the all	*/
/* of the indices and ptrs to the next free positions will be set to the	*/
/* correct value and ready for adding new data to each of the data areas.	*/
/* 																								*/
/* void ReadOldFile( void )																*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* 																								*/
/* RETURNS: 	void																			*/
/* 																								*/
/* johnhe 11-10-89																			*/
/***************************************************************************/

void ReadOldFile( void )
{
	char			*EndStrBuf;
	int			iFile;
	int			i;
	unsigned 	uSize[3];

									/* Read in the 3 parts of the file	*/

	if ( (iFile = open( szDosDataFile, O_RDONLY | O_BINARY )) != -1 )
	{
		for ( i = 0; i < 3; i++ )
			if ( read( iFile, (char *) &uSize[i], USIZE ) != USIZE ||
				  read( iFile, (char *)Buf[i], uSize[i] ) != (int)uSize[i] )
						ExitMsg1( szMsgErrReading, szOldDataFile );

		close ( iFile );
	}
									/* Initialize the array of ptrs to text */
	EndStrBuf = pchStringBuf + uSize[1];
	pchNextStr = pchStringBuf;

	for ( uNextPtr = 0;
			pchNextStr < EndStrBuf;
			uNextPtr++ )
	{
		apszPtr[ uNextPtr ] = pchNextStr;
		pchNextStr = strchr( pchNextStr, EOL ) + 1;
	}
	pNextOem = ddrOemList + (uSize[0] / sizeof( struct NEW_DDR ));
	uDataNext = uSize[2] / sizeof( unsigned );

}

/***************************************************************************/
/* Opens the file specified on the command line and processes the text in	*/
/* in the file and then updates the 4 data areas in memory with the new 	*/
/* information.																				*/
/* 																								*/
/* void ReadSourceData( char *szSourceFile ) 										*/
/* 																								*/
/* ARGUMENTS:	szSource 	- Path and name string for the file to process	*/
/* 																								*/
/* RETURNS: 	void																			*/
/* 																								*/
/* johnhe 11-10-89																			*/
/***************************************************************************/

void ReadSourceData( char *szSourceFile )
{
	FILE				*pFile;
	char				szBuf[ 80 ];
	char				*szString;

	if ( (pFile = fopen( szSourceFile, "rt" )) == NULL )
		ExitMsg1( szMsgErrOpening, szSourceFile );

	while ( ! feof( pFile ) )
	{
											/* Create an OEM struct for this vender */
		GetDosId( pFile, szSourceFile );

		while ( !feof( pFile ) )
		{
			if ( !fgets( szBuf, 80, pFile ) )	/* Get next line from file */
			{
				if ( ferror( pFile ) )
					ExitMsg1( szMsgErrReading, szSourceFile );
			}
			else
			{													/* Check for buffer overflow */
				if ( uDataNext >= MAX_DATA - 1 ||
					  uNextPtr >= (MAX_STRINGS - 1) ||
					  (pchNextStr - pchStringBuf) > (int)(MAX_TEXT - 100) )
					ExitMsg1( szMsgTooMuch, szSourceFile );

				szString = strchr( szBuf, COMMENT_CHAR );
				if ( szString )
					*szString = 0	;						/* truncate comments */

				TrimSz( szBuf );
				if ( strcmp( szBuf, szEOD ) == 0 )
					break;									/* End of this OEM's data */

				if ( *szBuf )
					uDataBuf[ uDataNext++ ] = GetTextOffset( szBuf );
			}
		}
		uDataBuf[ uDataNext++ ] = 0xffffU;		/* Mark end of OEM data 	*/
	}

	fclose( pFile );
}
/***************************************************************************/
/* Reads in an OEM string and initializes the next free OEM ddr structure	*/
/* with the OEM name, version number and then increments the next free OEM */
/* structure pointer.																		*/
/* 																								*/
/* void GetDosId( FILE *pFile, char *szSourceFile )								*/
/* 																								*/
/* ARGUMENTS:	pFile 		- Open file structure									*/
/* 				szSourceFile - The name of the source file (for error msgs) */
/* 																								*/
/* RETURNS: 	void																			*/
/* 																								*/
/* johnhe 11-10-89																			*/
/***************************************************************************/

void GetDosId( FILE *pFile, char *szSourceFile )
{
	char			*szString;
	char			szBuf[ 80 ];
	char			szOemName[ OEM_SIZE ];
	register 	i;
	int			MajorVer;
	int			MinorVer;

	do
	{
		if ( feof( pFile ) )
			return;

		if ( ! fgets( szBuf, 80, pFile ) )
			ExitMsg1( szMsgErrReading, szSourceFile );

		szString = strchr( szBuf, COMMENT_CHAR );

		if ( szString )
			*szString = 0;		/* truncate comments */

		TrimSz( szBuf );
	} 
	while ( *szBuf == 0 );

	for ( i = 0;
			szBuf[i] != '*' &&
			szBuf[i] != EOL &&
			i < (OEM_SIZE - 1);
			i++ )
		szOemName[i] = szBuf[i];

	szOemName[i++] = EOL;
	if ( sscanf( szBuf+i, "%d.%d", &MajorVer, &MinorVer ) != 2 )
		ExitMsg1( szMsgBadFormat, szSourceFile );

	strncpy( pNextOem->szOemName, szOemName, sizeof( pNextOem->szOemName ) - 1 );
	pNextOem->MajorVer = (char)MajorVer;
	pNextOem->MinorVer = (char)MinorVer;
	pNextOem->uDataOffset = uDataNext * USIZE;
	pNextOem++; 								/* Increment ptr to next OEM			*/

	printf( "OEM: %-24s Version: %2d.%2.2d\n", szOemName, MajorVer, MinorVer );
}



/* johnhe 11-10-89																			*/
/***************************************************************************/

void WriteNewFile( void )
{
	int			iFile;
	int			i;
	unsigned 	uSize[3];

	uSize[0] = (unsigned)((pNextOem - ddrOemList) * sizeof( struct NEW_DDR ));
	uSize[1] = (unsigned)(pchNextStr - pchStringBuf);
	uSize[2] = uDataNext * USIZE;

	unlink( szOldDataFile );						/* Delete existing .bak file */
	rename( szOldDataFile, szOldDataFile	);

	if ( (iFile = open( szDosDataFile, OPEN_MODE, S_IWRITE )) != -1 )
	{
		for ( i = 0; i < 3; i++ )
			if ( write( iFile, (char *) &uSize[i], USIZE ) != USIZE ||
				  write( iFile, (char *)Buf[i], uSize[i] ) != (int)uSize[i] )
						ExitMsg1( szMsgErrWriting, szOldDataFile );

		close ( iFile );
	}
	else
		ExitMsg1( szMsgErrWriting, szOldDataFile );
}

/***************************************************************************/

/* Remove trailing spaces, tabs, and new-lines from string */

void TrimSz( char *szString )
{
	char *szPtr;

												/* szPtr -> end of string marker */
	szPtr = strchr( szString, EOL );

												/* go back to first non-space, non-tab */
	while ( (--szPtr >= szString ) &&
			  (*szPtr == ' ' || *szPtr == '\t' || *szPtr == '\n') )
		;

	*(++szPtr) = EOL; 						/* terminate the string after that */
}	

/***************************************************************************/

void ExitMsg( char *szMsg )
{
	printf( szMsg );
	exit( 1 );
}

/***************************************************************************/

void ExitMsg1( char *szMsg, void *pParam )
{
	char		szBuf[ 80 ];

	sprintf( szBuf, szMsg, pParam );
	ExitMsg( szBuf );
}

/***************************************************************************/
/* Returns an index to the array of pointers which has a pointer to the 	*/
/* specified string. If the string does not exist a buffer is allocated 	*/
/* for it and a pointer to this buffer is added in the next available		*/
/* element in the array of pointers an the index to this element is			*/
/* returned.																					*/
/* 																								*/
/* unsigned GetTextOffset( char *szText ) 											*/
/* 																								*/
/* ARGUMENTS:	szText		- Point to a string to find in or add to buffer */
/* 																								*/
/* RETURNS: 	unsigned 	- Offset in text buffer where string is located */
/* 																								*/
/* johnhe 11-10-89																			*/
/***************************************************************************/

unsigned GetTextOffset( char *szText )
{
	int				i;
	unsigned 		uOffset;
												/* See if the string is a duplicate */
	for ( i = 0; i < (int)uNextPtr; i++ )
	{
		if ( strcmp( apszPtr[ i ], szText ) == OK )
		{
			uOffset = (unsigned)(apszPtr[i] - pchStringBuf);
			break;
		}
	}
												/* If didn't find a match need to add  */
												/* this string to the text buffer		*/
	if ( i >= (int) uNextPtr )
	{
		uOffset = (unsigned)(pchNextStr - pchStringBuf);

		strcpy( pchNextStr, szText );
		apszPtr[ uNextPtr++ ] = pchNextStr;

		pchNextStr = strchr( pchNextStr, EOL ) + 1;
	}

	return( uOffset );
}

/***************************************************************************/

void GetFileName( char *szFileName, int cszArg, char *rgszArg[] )
{
	if ( cszArg < 2 )
		ExitMsg( szMsgInfParam );

        if ( strlen( rgszArg [1] ) > 128 )
		ExitMsg( szMsgIllegalParam );

	strcpy( szFileName, rgszArg[ 1 ] );
}

/***************************************************************************/
/* Allocates the specified size buffer and returns a pointer to it. Checks */
/* for error and aborts program if memory is not available. 					*/
/* 																								*/
/* void *GetMemory( unsigned int Bytes )												*/
/* 																								*/
/* ARGUMENTS:	Bytes 	- Size of buffer to allocate in bytes					*/
/* 																								*/
/* RETURNS: 	void *	- Ptr to allocated buffer									*/
/* 																								*/
/* johnhe 11-10-89																			*/
/***************************************************************************/

void *GetMemory( unsigned int Bytes )
{
	void		*Ptr;

	if ( (Ptr = malloc( Bytes )) == NULL )
		ExitMsg( szMsgMemFailed );

	return( Ptr );
}
