/***************************************************************************/
/* FILECOPY.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* 																								*/
/* File copy functions for copying multiple files across disks.				*/
/* 																								*/
/* 																								*/
/* Created 11-01-89 johnhe																	*/
/* TABS = 3																						*/
/***************************************************************************/


#include 	<stdio.h>
#include		<stdlib.h>
#include 	<malloc.h>
#include 	<dos.h>
#include 	<fcntl.h>
#include 	<io.h>
#include		<share.h>
#include 	<sys\\types.h>
#include 	<sys\\stat.h>
#include 	<string.h>
#include 	<ctype.h>

#include		<alias.h>
#include 	<disk_io.h>
#include 	<window.h>
#include 	<strlib.h>
#include 	<copy.h>
#include 	<decomp.h>

extern unsigned gfCompressedDiskIsFull;
extern unsigned gfCompressedDiskCritSection;
extern unsigned gfCompressedDiskInstall;

/***************************************************************************/

#define		_A_FINDALL	(_A_HIDDEN | _A_SYSTEM)

/* m116 #define		MIN_BUF_SIZE			10000L	*/
/* m116 #define		MALLOC_RESERVE 		40000L	*/

#define		MIN_BUF_SIZE			10000L	/* m116	*/
#define		MALLOC_RESERVE 		40000L	/* m116	*/

#define		MAX_BLOCK				0xff00

extern  void DispInsertDisk(int DiskNum);
extern  void DispInsertUserDisk(int UserDisk,char chDrive);

#ifndef	SYSTEMS_COMPRESSION
extern  int IsHardDisk(int chDrive);
#endif

/***************************************************************************/
/*                                                                    		*/
/*	WR_BUF_INFO																					*/
/*																									*/
/* Status structure which contains the currents status of the disk			*/
/* write buffer.																				*/
/*                                                                    		*/
/***************************************************************************/

struct BUF_INFO
{
	char	far	*Start;			/* Far ptr to start of buffer */
	char	far	*Next;
	long			BufLen;			/* Total length of the buffer */
	long			BytesFree;		/* Number free  bytes left in the buffer */
};

/***************************************************************************/
/* Static globals for this module.														*/
/***************************************************************************/

static struct MULT_FILES	*File;	/* Ptr to current file information		*/
static struct BUF_INFO		Buf;		/* Copy buffer information struct		*/

#ifdef SYSTEMS_COMPRESSION
static char	far *UnpackArea;			/* Ptr to unpack area						*/
extern char	far *UnpackSeg;			/* Ptr segment	allocated for unpack()	*/
#endif

/***************************************************************************/
/* Prototypes for local functions within this module. 							*/
/***************************************************************************/

static UINT		ShareAccess;			/* File access mode */
static void		BufCopyFile		( struct MULT_FILES *File );

static int		BufFlush			( void );
static int		InitCpyBuffer	( void );
static int		InitFile			( struct MULT_FILES *File, char *szFile );
static long 	WriteFile		( int iFile, struct MULT_FILES *ThisFile,
										  long lToWrite );
long far			Unpack			( int iFile, char far *InBuffer,
										  long lPackedBytes );
void far			ClearRingBuffer( int CompressType );
void far 		CompressedDiskIsFull (void);

#ifndef SYSTEMS_COMPRESSION

static long 	ExpandFile		( int iFile, struct MULT_FILES *File );

#endif

/***************************************************************************/
/* Functions to copy multiple files between different drives. Will prompt	*/
/* for the disks before each file is read or written if the disk is not 	*/
/* already inserted. Will allow the user to decide whether to continue or	*/
/* or not if there is an error reading any of the files. 						*/
/*																									*/
/* int Xcopy( struct MULT_FILES *FileList )									*/
/*																									*/
/* ARGUMENTS:	FileList 	- Array of structures which describe everything */
/* 								  necessary about the files.							*/
/* RETURNS: 	void																			*/
/*																									*/
/***************************************************************************/

void Xcopy( struct MULT_FILES *FileList )
{
	register 	i;

	if ( _osmajor < 3  || (_osmajor == 3 && _osminor < 10) )
		ShareAccess = O_RDONLY;
	else
		ShareAccess = SH_DENYWR;


	File = FileList;								/* Set ptr to file structures */

	if ( InitCpyBuffer() == OK )				/* Initialize the copy buffer */
	{
														/* Loop for each file			*/
		for ( i = 0; gfCompressedDiskIsFull == FALSE &&
                   File[i].Name.Source    != NULL; i++ )
			BufCopyFile( File+i );

		if (gfCompressedDiskIsFull == FALSE)
			BufFlush();								/* Flush any files in memory	*/

		hfree( Buf.Start );						/* Free the copy buffer 		*/
#ifdef SYSTEMS_COMPRESSION
		hfree( UnpackArea );						/* Free the unpacked segment	*/
/*		FreeMemory( UnpackArea );*/			/* Free the unpacked segment	*/
#endif
		if (gfCompressedDiskIsFull == TRUE)
		{
			/* The compressed disk filled up */
			CompressedDiskIsFull();		/* This routine reboots */
		}
	}
	else
		FatalError( FATAL_MEMORY_ERROR );
}

/***************************************************************************/
/* Reads a specified file and appends it to the copy buffer. The copy		*/
/* buffer is flushed to disk as often as necessary. If an error occurs		*/
/* while reading a file the file is marked with a zero length and the   	*/
/* user is given a chance to continue if no making recovery disk. 			*/
/*																									*/
/* void BufCopyFile( int Index )															*/
/*																									*/
/* ARGUMENTS:	File		- File structure for file to be copied.				*/
/* 							  Source and Destination path and file members		*/
/* 							  must be complete											*/
/* RETURNS: 	void																			*/
/*																									*/
/***************************************************************************/

static void BufCopyFile( struct MULT_FILES *File )
{
	int			iStatus; 							/* Return status				*/
	int			iFile;								/* Open file handle			*/
	char			*szFile; 							/* Full path name for file */
	long			lToRead;
#ifndef SYSTEMS_COMPRESSION
	int			fFlush;
	long			lDecompRead;

	File->pchStart = NULL;
#endif

	szFile = GetMemory( MAX_PATH_LEN );
	BuildPath( szFile, File->Drive.Source, File->Path.Source,
				  File->Name.Source );
	do
	{
#ifndef SYSTEMS_COMPRESSION
	  fFlush = FALSE;
#endif
		iStatus = ERROR;

		DispInsertDisk( File->DiskNumber );
		DisplayFileStatus( File->Name.Source, READ );

																/* Set up file's structure */
		if ( File->lSourceSize == 0L && InitFile( File, szFile ) != OK )
		{
			File->lSourceSize = 0L;
			break;
		}
									/* Determine how much of the file will fit in	*/
									/* the buffer and where it will be written to	*/

#ifdef SYSTEMS_COMPRESSION
		File->pchStart = Buf.Next;
#else
		if ( File->pchStart == NULL )
			File->pchStart = Buf.Next;
#endif
		lToRead = File->lSourceSize - File->lRead;
		if ( lToRead > Buf.BytesFree )
			lToRead = Buf.BytesFree;

									/* Open the file and seek to the location for	*/
									/* the start of the next read. Then read in the	*/
									/* portition of the file and update structures	*/

		if ( _dos_open( szFile, ShareAccess, &iFile ) == OK )
		{

#ifdef SYSTEMS_COMPRESSION

			if ( _dos_seek( iFile, File->lRead, SEEK_SET ) == File->lRead )
			{
				if ( BigReadWrite( iFile, Buf.Next, lToRead, READ ) == OK )
				{
									/* Update buffer information */
					Buf.Next = HugeAdd( Buf.Next, lToRead );
					Buf.BytesFree -= lToRead;

									/* Update the file's structure */
					File->lRead	+= lToRead;
					iStatus = OK;				/* Can only get here if no error */
				}
			}
#else
			if ( File->IsPacked )
			{
				/* The file is compressed, it gets uncompressed as it's read
					into the buffer.	There is a big time hit for every pass
					through the same file because the decompression code has to
					(re)decompress and through away everything it's already done
					for a file on subsequent passes.  So, if a packed file will
					not fit into the remaining buffer space, but it will fit
					into an empty buffer, force a BufFlush() call and loop back
					here again. */

				if ( lToRead == Buf.BytesFree )		/* reading to fill buffer? */
				{
					/* reading less when remaining, but remaining would fit in
						empty buffer? */

					lDecompRead = File->lSourceSize - File->lRead;	/* remaining*/

					if ( lToRead < lDecompRead )

						if ( lDecompRead <= Buf.BufLen ) 	/* remaining fit? 	*/
						{
							lToRead = 0;				/* don't read anything this   */
							iStatus = OK;				/*   pass, just force a flush */
							fFlush = TRUE; 			/*   and read on next pass 	*/

						} else

							/* The packed file is too big for even an empty buffer,
								but if installing to a hard disk (non swappable)
								it can be expanded directly from file to file.
								Otherwise just go ahead and buffer it. */

							if ( IsHardDisk(File->Drive.Destin) )
							{
								lToRead = 0;			/* don't try to buffer below  */

								lDecompRead = ExpandFile( iFile, File );

								UpdateByteCount (lDecompRead * 2);	/* Update gauge   */

								if ( lDecompRead == File->lDestinSize )/* got all? */
									iStatus = OK;
							}
				}

				if ( lToRead > 0 )					/* may have been zeroed above */
				{

					/* read and decompress a chunk (maybe all) of the file		*/

					lDecompRead = LcbDecompressToBuffer( iFile, Buf.Next, lToRead,
																	 File->lRead, FALSE );

					UpdateByteCount (lDecompRead);				/* Update gauge   */

					if ( lDecompRead == lToRead ) /* got all that was requested? */
						iStatus = OK;
				}

			} else {

				/* The file is not packed, so just seek to the read offset
					and read the next chunk */

				if ( _dos_seek( iFile, File->lRead, SEEK_SET ) == File->lRead )
					iStatus = BigReadWrite( iFile, Buf.Next, lToRead, READ );
			}

			if ( iStatus == OK )
			{
							/* Update buffer information */
				Buf.Next = HugeAdd( Buf.Next, lToRead );
				Buf.BytesFree -= lToRead;

							/* Update the file's structure */
				File->lRead += lToRead;
			}
#endif

			if ( _dos_close( iFile ) != OK ) /* Close the file checking 	*/
				iStatus = ERROR;					/* for errors						*/
		}
									/* Check for an error reading the file and	*/
									/* if error mark this file as 0 length if 	*/
									/* user decides to continue						*/

		if ( iStatus != OK )
			File->lSourceSize = 0L;

#ifdef SYSTEMS_COMPRESSION
		else if ( Buf.BytesFree == 0 )	/* See if the buffer needs flushing */
#else
		else if ( fFlush || Buf.BytesFree == 0 )	/* Buffer need flushing?	*/
#endif
		{
#ifdef SYSTEMS_COMPRESSION
			if ( BufFlush() != OK )
				File->lSourceSize = 0L; 	/* Signal this file not written	*/
#else
			if ( BufFlush() == OK )
				File->pchStart = NULL;
			else
				File->lSourceSize = 0L; 	/* Signal this file not written	*/
#endif
		}
	}
	while ( iStatus == OK && File->lRead < File->lSourceSize &&
				gfCompressedDiskIsFull == FALSE);

	if ( iStatus != OK )
		ProcessCopyError( File->Name.Destin, ERR_READING );

	FreeMemory( szFile );
}

/***************************************************************************/
/* Initializes a file structure for use by the BufCopyFile() function.		*/
/* Also reads first 12 bytes of the files to see if it is a packed file.	*/
/* 																								*/
/* int InitFile( struct MULT_FILES *File, char *szFile ) 						*/
/* 																								*/
/* ARGUMENTS:	File		- Ptr to file structure to be initialized 			*/
/* 				szFile	- Complete file and path for a source file			*/
/* RETURNS: 	int		- OK if no file errors else ERROR						*/
/* 																								*/
/***************************************************************************/

static int InitFile( struct MULT_FILES *File, char *szFile )
{
	char						*szEndChar;
	register 				i; 					/* Loop Indice 						*/
	int						iFile;				/* DOS file handle					*/
	unsigned 				uStatus; 			/* Error status						*/
	struct find_t			Info;
	uStatus = (UINT) ERROR; 					/* Assume may not be successful	*/

#ifdef SYSTEMS_COMPRESSION
	unsigned 				uRead;				/* Count of byte read from file	*/
	struct PACKED_HEADER	Fh;					/* Struct to hold file header 	*/
	struct WIN_HEADER		Wh;					/* Alternate windows file header	*/
	static char 			MagicValue[8] =	{ 'S',	'Z',	  ' ',	  '\x88',
														 '\xf0', '\x27', '\x33', '\xd1' };
#endif

								/* Try to open the file normally and if it fails	*/
								/* try opening the file as a compressed file by 	*/
								/* switching last char of name extension to a '$'	*/

	for ( i = 0; i < 2; i++ )
	{
		if ( (_dos_findfirst( szFile, _A_FINDALL, &Info ) == OK &&
				_dos_open( szFile, ShareAccess, &iFile ) == OK) )
			break;
		szEndChar = strchr( szFile, EOL );				/* Find end of string	*/
		if ( szEndChar != NULL )
			*(szEndChar - 1) = '_';
	}

	if ( i < 2 )												/* See if open was OK	*/
	{
		File->lDestinSize = File->lSourceSize = filelength( iFile );

#ifdef SYSTEMS_COMPRESSION
																/* Read in the file header */
		if ( _dos_read( iFile, &Wh, sizeof( Wh ), &uRead ) == OK &&
			  uRead > 0 )
		{
												/* Need the header in both structures	*/
			memcpy( &Fh, &Wh, sizeof( Wh ) );

											/* Check for a languages group's header	*/
			if ( memcmp( MagicValue, Fh.MagicStr, sizeof(MagicValue)) == OK )
			{
				File->lDestinSize = Fh.lDestinSize;
				File->lRead = File->lUnpacked = (long)sizeof( Fh );
				File->IsPacked = LANGUAGE_COMPRESS;
			}
									/* Now check for a Windows group's header */
			else if ( memcmp( WIN_STR, Fh.MagicStr, sizeof(MagicValue)) == OK )
			{
				File->lDestinSize = Wh.lDestinSize;
				File->lRead = File->lUnpacked = (long)sizeof( Wh );
				File->IsPacked = WINDOWS_COMPRESS;
			}
			uStatus = OK;
		}
#else
        if ((int) (uStatus = WReadHeaderInfo (iFile)) > 0)
          {
            /* This is a compressed file, using APPS compression.  */
            /*   Unfortunately, the file must be decompressed into */
            /*   RAM, as the APPS compression APIs do not have a   */
            /*   method to decompress from a buffer to a file (but */
            /*   it can decompress from a file to a buffer).       */

            File->lDestinSize = LcbCalculateDecompressedLength (iFile, TRUE);
            File->lSourceSize = File->lDestinSize;
            File->IsPacked = TRUE;
            uStatus = OK;
          }
        else
            uStatus = (uStatus == rcNoHeader) ? OK : ERROR;
#endif

		uStatus |= _dos_getftime( iFile, &File->Date, &File->Time );
		uStatus |= _dos_close( iFile );
	}
	return( (uStatus == OK && File->lSourceSize > 0L) ? OK : ERROR );
}

/***************************************************************************/
/* Initializes the copy buffer to max available memory less 20K bytes. If	*/
/* there is less than 5K of memory available for use the program is			*/
/* with a call to FatalError(). The static global pchCpyBuffer is set to	*/
/* point to the successfully allocated buffer a buffer must also be			*/
/* allocate for the unpack functions. This buffer will have to be				*/
/* normalized so the functions can use it with an offset of 0 so 16 must	*/
/* be added to the offset before normalization to allow for this.				*/
/*																									*/
/*	void InitCpyBuffer( void )																*/
/*																									*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS:		void																			*/
/*																									*/
/* GLOBALS:		lBufSize	- Set to size of allocated buffer						*/
/*					pchCpyBuffer - Pointed to allocated buffer						*/
/*					UnPackSeg	- Normalized ptr to an unpack segment area		*/
/*																									*/
/***************************************************************************/

static int InitCpyBuffer( void )
{
	register	iStatus;

	iStatus = ERROR;									/* Error if fails any test 	*/

															/* Allocate an unpack segment	*/

#ifdef SYSTEMS_COMPRESSION
	/*	UnpackArea = GetMemory(UNPACK_BUF_LEN+RING_BUF_LEN+MAX_STR_LEN+100); */
	UnpackArea = halloc( (long)(UNPACK_BUF_LEN + RING_BUF_LEN +MAX_STR_LEN + 100), 1 );
		if ( UnpackArea == (char far *)(NULL) )
			return( iStatus );

	UnpackSeg = NormalizePtr( (char far *)UnpackArea + 16 );
#endif

						/* Find out how much memory we can get */
	Buf.BufLen = GetMaxHugeSize() - MALLOC_RESERVE;

	if ( Buf.BufLen >= MIN_BUF_SIZE )			/* Is there enough memory ?	*/
	{
		Buf.Start = halloc( Buf.BufLen, 1); 	/* Do we allocate it OK 		*/
		if ( Buf.Start != NULL )
		{
			Buf.Next = Buf.Start;					/* Set next free byte			*/
			Buf.BytesFree = Buf.BufLen;			/* Set number of bytes free	*/
			iStatus = OK;								/* Passed all tests				*/
		}
	}
	return( iStatus );
}

/***************************************************************************/
/* Walks throught the array of file structures and writes out each file 	*/
/* or a portition of each file that resides in the copy buffer.				*/
/* 																								*/
/* int BufFlush( void )																		*/
/* 																								*/
/* ARGUMENTS:	void																			*/
/* RETURN:		int		- OK if no error												*/
/* 																								*/
/* Return status will reflect the status of the last file written. This 	*/
/*	will be a signal to the BufCopyFile() to abort the current file being	*/
/* copied and go to the next file.														*/
/***************************************************************************/

static int BufFlush( void )
{
	char						*szFile;
	int						iFile;
	int						iStatus;

	long						lToWrite;
	long						lWritten;
	struct MULT_FILES 	*ThisFile;
	struct find_t			Info;


	DisplayFileStatus( "", CLEAR );
	szFile = GetMemory( MAX_PATH_LEN );		/* Allocate a path buffer			*/
	Buf.Next = Buf.Start;						/* Reset ptr to begining of buf	*/

											/* Loop until all files are checked */
	for ( iStatus = OK, ThisFile = File;
			ThisFile->Name.Source != NULL;
			ThisFile++ )
	{
		BuildPath( szFile, ThisFile->Drive.Destin, ThisFile->Path.Destin,
					  ThisFile->Name.Destin );

		if ( ThisFile->lUnpacked < ThisFile->lSourceSize )
		{
			iStatus = ERROR;
			lToWrite = ThisFile->lRead - ThisFile->lUnpacked;

			DispInsertUserDisk( ThisFile->UserDisk, ThisFile->Drive.Destin );
			DisplayFileStatus( ThisFile->Name.Destin, WRITE );

			_dos_setfileattr( szFile, _A_NORMAL );

			if ( (_dos_findfirst( szFile, _A_FINDALL, &Info ) == OK &&
				   _dos_open( szFile, O_RDWR, &iFile ) == OK)||
				  _dos_creat( szFile, 0, &iFile ) == OK )
			{
				if ( (lWritten = WriteFile( iFile, ThisFile, lToWrite )) != -1L )
				{
					iStatus = OK;
					ThisFile->lUnpacked += lToWrite;
					ThisFile->lWritten += lWritten;

					if ( ThisFile->lWritten == ThisFile->lDestinSize )
					{			
						chsize( iFile, ThisFile->lDestinSize );
						_dos_setftime( iFile, ThisFile->Date, ThisFile->Time );
					}
				}

				iStatus |= (int)_dos_close( iFile );
			}
		}

		if ( iStatus != OK ||
			  (ThisFile->lRead == ThisFile->lSourceSize &&
				ThisFile->lWritten != ThisFile->lDestinSize) )
		{
			if (gfCompressedDiskInstall)
			{
				gfCompressedDiskIsFull = TRUE;
				FreeMemory (szFile);				/* Free the path buffer 			*/
				return (ERROR);
			}
			else
			{
				ProcessCopyError( ParseFileName( szFile ), ERR_WRITING );
				ThisFile->lSourceSize = 0L;	/* If error set file size to 0	*/
				ThisFile->lDestinSize = 0L;
			}
		}

		if ( ThisFile->lSourceSize == 0L )	/* If source size == 0 we had	an	*/
			remove( szFile );						/* error so delete destin file	*/
	}

	Buf.BytesFree = Buf.BufLen;				/* Reset number of bytes free 	*/
	Buf.Next = Buf.Start;						/* Reset ptr to begining of buf	*/
	FreeMemory( szFile );						/* Free the path buffer 			*/

	DisplayFileStatus( "", CLEAR );
	return( iStatus );
}

/***************************************************************************/
/* Writes out the part of a file in the copy buffer with checking for		*/
/* using UnPack() or BigReadWrite() depending on whether the file is a		*/
/* packed file or not.																		*/
/* 																								*/
/* static long WriteFile( int iFile, struct MULT_FILES *ThisFile, 			*/
/* 							  long lToWrite ) 											*/
/* 																								*/
/* ARGUMENTS:	iFile 	- Open DOS file handle, file pointer must be set	*/
/* 							  to position for start of read or write				*/
/* 				ThisFile - Structure describing the file being written		*/
/* 				lToWrite	- Number of bytes in the copy buffer to write out	*/
/* RETURN:		long		- Number of bytes written to output file or -1L if */
/* 							  and error was detected.									*/
/*																									*/
/***************************************************************************/

static long WriteFile( int iFile, struct MULT_FILES *ThisFile, long lToWrite )
{
	long		lWritten;
#ifdef SYSTEMS_COMPRESSION
	int		HeaderLen;
#endif

	lWritten = -1L;								/* Assume there may be an error */

							/* Be sure file is the length it is supposed to be 	*/
							/* and then seek to the offset for the next write		*/

	if ( filelength( iFile ) >= ThisFile->lWritten &&
		  _dos_seek( iFile, ThisFile->lWritten, SEEK_SET ) == ThisFile->lWritten )
	{
#ifdef SYSTEMS_COMPRESSION
		if ( ThisFile->IsPacked )						/* Check for compress file */
		{
								/* If first time for file, initialize ring buffer	*/
			HeaderLen = ThisFile->IsPacked == WINDOWS_COMPRESS ?
							sizeof( struct WIN_HEADER ) :
							sizeof( struct PACKED_HEADER );

			if ( ThisFile->lUnpacked <= HeaderLen )
				ClearRingBuffer( ThisFile->IsPacked );

			lWritten = Unpack( iFile, ThisFile->pchStart, lToWrite );
		}
		else
#endif
            if ( BigReadWrite( iFile, ThisFile->pchStart, lToWrite,
					 WRITE ) == OK )
				lWritten = lToWrite; 						/* written == lToWrite		*/
	}
	return( lWritten );
}

#ifndef SYSTEMS_COMPRESSION


/***************************************************************************/
/* Expands a file directly from file to file without buffering in memory.	*/
/* Used as a performance enhancement when copying large packed files and	*/
/* installing to a hard disk (no disk swaps necessary).							*/
/* 																								*/
/* static long ExpandFile( int iFile, struct MULT_FILES *ThisFile )			*/
/* 																								*/
/* ARGUMENTS:	fhSrc 	- Open DOS file handle, file pointer must be set	*/
/* 							  to start of file (input packed file) 				*/
/* 				ThisFile - Structure describing the file being written		*/
/* RETURN:		long		- Number of bytes written to output file or -1L if */
/* 							  and error was detected.									*/
/*																									*/
/***************************************************************************/

static long ExpandFile( int fhSrc, struct MULT_FILES *ThisFile )
{
	char				*szFile;
	int				iFile;
	int				iStatus;
	long				lWritten;
	struct find_t	Info;

	DisplayFileStatus( "", CLEAR );
	szFile = GetMemory( MAX_PATH_LEN );		/* Allocate a path buffer			*/

	BuildPath( szFile, ThisFile->Drive.Destin, ThisFile->Path.Destin,
				  ThisFile->Name.Destin );

	iStatus = ERROR;

	DisplayFileStatus( ThisFile->Name.Destin, WRITE );

	_dos_setfileattr( szFile, _A_NORMAL );

	if ( (_dos_findfirst( szFile, _A_FINDALL, &Info ) == OK &&
			_dos_open( szFile, O_RDWR, &iFile ) == OK)||
		  _dos_creat( szFile, 0, &iFile ) == OK )
	{
		lWritten = LcbDecompressToFile( fhSrc, iFile, LCBNIL, 0L, FALSE );
		if ( lWritten == ThisFile->lDestinSize )
		{
			iStatus = OK;
			ThisFile->lWritten  = lWritten;
			ThisFile->lUnpacked = ThisFile->lRead = ThisFile->lSourceSize;

			chsize( iFile, ThisFile->lDestinSize );
			_dos_setftime( iFile, ThisFile->Date, ThisFile->Time );

		} else

			lWritten = 0;					/* error, return 0 size 				*/

		iStatus |= (int)_dos_close( iFile );
	}

	if ( iStatus != OK )
	{
		if (gfCompressedDiskInstall)
		{
			gfCompressedDiskIsFull = TRUE;
			FreeMemory (szFile); 			/* Free the path buffer 			*/
			return (ERROR);
		}
		else
		{
			ProcessCopyError( ParseFileName( szFile ), ERR_WRITING );
			ThisFile->lSourceSize = 0L;	/* If error set file size to 0	*/
			ThisFile->lDestinSize = 0L;
		}
	}

	if ( ThisFile->lSourceSize == 0L )	/* If source size == 0 we had an */
		remove( szFile ); 					/* error so delete destin file	*/

	FreeMemory( szFile );						/* Free the path buffer 			*/

	DisplayFileStatus( "", CLEAR );

	return ( lWritten );

}

#endif
