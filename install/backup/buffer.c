/***************************************************************************/
/*																									*/
/* BUFFER.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Bufffered disk output functions for the RUP backup functions.				*/
/* 																								*/
/* Created 07-14-89 johnhe																	*/
/***************************************************************************/


#include		<stdio.h>
#include		<stdlib.h>
#include		<malloc.h>
#include		<dos.h>
#include		<string.h>
#include 	<fcntl.h>

#include		<alias.h>
#include		<disk_io.h>
#include		<backup.h>
#include		<window.h>

/***************************************************************************/
/* Function prototypes for all functions in this module.							*/
/***************************************************************************/


/***************************************************************************/
/* Globals for this module																	*/
/***************************************************************************/

int								iHandle;		/* DOS handle for buffered file */

static struct WR_BUF_INFO	Buf;


extern int						iBkupMode;
extern char						*szBkupFile;
extern char						*szCtrlFile;


/***************************************************************************/
/* Appends data to a file using a buffer to speed things up. The buffer is	*/
/* is flushed as many times as necessary until all bytes have been 			*/
/* appended.																					*/
/*																									*/
/* int BufAppend( void far *vPtr, unsigned uBytes )								*/
/*																									*/
/* ARGUMENTS:	vPtr		- Ptr to buffer holding data to be appended			*/
/*					uBytes	- Number of bytes to append.								*/
/*	GLOBALS:		Buf		- Struct must have been initialized						*/
/*																									*/
/*																									*/
/* RETURNS:		int		- OK if success else disk error from BufFlush()		*/
/*	GLOBALS:		Buf		- Updates struct about this append						*/
/*																									*/
/***************************************************************************/

int BufAppend( VOID FAR *vPtr, UINT uBytes )
{
	unsigned		uToMove;			/* Count of bytes move in the loop */
	int			iStatus;			/* Return status							*/

				/* Keep appending and flushing until all requested */
				/* bytes have been appended or error is detected	*/
	iStatus = OK;
	while ( uBytes > 0 && iStatus == OK )
	{
					/* Find out how many bytes we can put in the buffer */
		uToMove = Buf.BytesFree >= (LONG) uBytes  ? 
					 uBytes : (unsigned)(Buf.BytesFree);

		Buf.Next = NormalizePtr( Buf.Next );
		memcpy( Buf.Next, vPtr, uToMove );

					/* Update information about the buffer */
		Buf.Next += uToMove;
		Buf.BytesFree -= (LONG)(uToMove);
		uBytes -= uToMove;
		Buf.BufOffset += (LONG)(uToMove);

					/* If vPtr is full we need to flush it. */
		if ( Buf.BytesFree == 0 )
			iStatus = BufFlush();
	}
	return( iStatus );
}

/***************************************************************************/
/* Seeks to the specified offset in the buffered file and writes the 		*/
/* the specified number of bytes to the file. If the seek offset in the		*/
/* file is located in the current buffer the buffer will be updated and		*/
/* no physical write will take place. If a physical seek is done a seek		*/
/* is done to restore the original position in the file so that the next	*/
/* append will not be affected.															*/
/* 																								*/
/* INT BufSeekWrite( VOID *vPtr, UINT uBytes, LONG lOffset )					*/
/*																									*/
/* ARGUMENTS:	vPtr		- Ptr to buffer holding data to be written			*/
/*					uBytes	- Number of bytes to write									*/
/*					lOffset	- Offset from begining of file to begin write		*/
/*	GLOBALS:		Buf		- Struct must have been initialized						*/
/*																									*/
/*																									*/
/* RETURNS:		int		- OK if success else disk error code					*/
/*	GLOBALS:		Buf		- Updates struct if write affects the buffer			*/
/*																									*/
/* NOTE: IT IS ALWAYS ASSUMED THAT THE SEEK AND WRITE WILL ALWAYS BE AT		*/
/*			AN OFFSET BEFORE THE CURRENT APPEND POSITION AND THE THE BUFFER	*/
/*			WILL NEVER BE FLUSHED BY A CALL TO THIS FUNCTION.						*/
/***************************************************************************/

INT BufSeekWrite( VOID *vPtr, UINT uBytes, LONG lOffset )
{
	int			iStatus;		/* Return status						*/
	unsigned		uToMove;		/* Number of bytes being moved	*/
	char huge	*hPtr;
	LONG			BufOffset;

	if ( lOffset >= Buf.FileOffset && lOffset < (Buf.FileOffset + Buf.BufLen) )
	{
					/* Find out how many bytes we can put in the buffer */
		uToMove = 	Buf.BytesFree >= (LONG)uBytes ? 
						  uBytes : (unsigned)(Buf.BytesFree);

		BufOffset = (lOffset - Buf.FileOffset);
	
		hPtr = NormalizePtr( Buf.Start +  BufOffset );
		memcpy( hPtr, vPtr, uToMove );	/* Move the bytes */
		iStatus = OK;
	}

	else
	{
		if ( (iStatus = DoWrite( vPtr, lOffset, uBytes )) == OK )
			iStatus = DoSeek( Buf.FileOffset );
	}
	return( iStatus );
}

/***************************************************************************/
/* Writes the specfified bytes to the current file associated with the		*/
/* file buffer at the specified location. After writing to file the file	*/
/* pointer will be reset to the offset in the file that corresponds to the	*/
/* the start of the file's append buffer in memory so that the next call	*/
/* to flush buffer will not have to do a seek. If there is a disk error		*/
/* detected the function ProcDiskErr() will be called and the return	*/
/* status will be the valued returned from this function call.					*/
/* 																								*/
/*	INT DoWrite( VOID *vPtr, LONG lOffset, UINT uBytes )							*/
/*																									*/
/* ARGUMENTS:	vPtr		- Ptr to buffer holding data to be written			*/
/*					uToWrite	- Number of bytes to write									*/
/*					lOffset	- Offset from begining of file to begin write		*/
/*	GLOBALS:		Buf		- Struct must have been initialized						*/
/*																									*/
/*																									*/
/* RETURNS:		int		- OK if success else disk error code					*/
/*																									*/
/***************************************************************************/

INT DoWrite( VOID *vPtr, LONG lOffset, UINT uToWrite )
{
	int			iStatus;
	unsigned		uWritten;
	char			szOldFile[25];
	int			OldReadWrite;

	OldReadWrite = GetCurrentFileStr( szOldFile );
	ShowFileStatus( iBkupMode == BACKUP ? szBkupFile+3 : szCtrlFile+3,
						   WRITE );

	vPtr = NormalizePtr( vPtr );
	if ( _dos_seek( iHandle, lOffset, SEEK_SET ) == lOffset &&
		  _dos_write( iHandle, vPtr, uToWrite, &uWritten ) == OK &&
		  uWritten == uToWrite &&
		  _dos_seek( iHandle, Buf.FileOffset, SEEK_SET ) == lOffset )
	{
		UpdateByteCount( (long)uWritten );
		iStatus = OK;
	}
	else
		iStatus = ProcDiskErr( BAD_MEDIA );

	ShowFileStatus( szOldFile, OldReadWrite );
	return( iStatus );
}

/***************************************************************************/
/* Seeks to the specified offset in the open file associated with the disk	*/
/* buffer. If an error is detected the function will return the value 		*/
/* which is returned by a call to ProcDiskErr().								*/
/* 																								*/
/* INT DoSeek( LONG Offset )																*/
/*																									*/
/* ARGUMENTS:	lOffset	- Offset from begining of file to begin write		*/
/*	GLOBALS:		Buf		- Struct must have been initialized						*/
/*																									*/
/*																									*/
/* RETURNS:		int		- OK if success else disk error code					*/
/*																									*/
/***************************************************************************/

INT DoSeek( LONG lOffset )
{
	int		iStatus;

	if ( _dos_seek( iHandle, lOffset, SEEK_SET ) == lOffset )
		iStatus = OK;
	else
		iStatus = ProcDiskErr( BAD_MEDIA );

	return( iStatus );
}

/***************************************************************************/
/* Flushes the write buffer by writing it's contents to the disk and then	*/
/* updates the buffer structure to show the new status. This function		*/
/* assumes the file pointer is already at the correct place and that no		*/
/* seek is necessary. If an error is detected the function will return the	*/
/* value which is returned by a call to ProcDiskErr().						*/
/* 																								*/
/* INT BufFlush( VOID )																		*/
/*																									*/
/*																									*/
/* ARGUMENTS:	VOID																			*/
/*	GLOBALS:		Buf		- Struct must have been initialized						*/
/*																									*/
/*																									*/
/* RETURNS:		int		- OK if success else disk error code					*/
/*	GLOBALS:		Buf		- Updates values to reflect the flushed buffer		*/
/*								  and new file offset										*/
/*																									*/
/***************************************************************************/

INT BufFlush( VOID )
{
	unsigned		uToWrite, uWritten;
	int			iStatus;
	UCHAR			*Ptr;
	LONG			lTotal;
	char			szOldFile[25];
	int			OldReadWrite;

	iStatus	= OK;
	Ptr		= Buf.Start;
	lTotal = 0L;

	OldReadWrite = GetCurrentFileStr( szOldFile );
	ShowFileStatus( iBkupMode == BACKUP ? szBkupFile+3 : szCtrlFile+3,
						   WRITE );

					/* Keep writing max possible blocks size to disk		*/
					/* until all blocks are written or error is detected	*/
	while (  lTotal < Buf.BufOffset  && iStatus == OK )
	{
					/* Calc max bytes we can write to disk */
		uToWrite = ((Buf.BufOffset - lTotal) > 0xfff0L) ?
						0xfff0 :	(unsigned)(Buf.BufOffset - lTotal);

		Ptr = NormalizePtr( Ptr );
		if ( _dos_write( iHandle, Ptr, uToWrite, &uWritten ) == OK &&
			  uWritten == uToWrite )
		{ 											/* Setup to write next block */
			Ptr += (LONG)uWritten;
			lTotal += (LONG)uWritten;
			UpdateByteCount( (long)uWritten );
		} 			
		else
			iStatus = ProcDiskErr( BAD_MEDIA );
	}
	if ( iStatus == OK )
	{
		Buf.Next = Buf.Start;
		Buf.BufOffset = 0L;
		Buf.BytesFree = Buf.BufLen;
		Buf.FileOffset += lTotal;
	}

	ShowFileStatus( szOldFile, OldReadWrite );
	return( iStatus );
}

/***************************************************************************/

VOID BufInit( VOID )
{
	extern char huge	*pchCpyBuffer;	/* Huge buffer for files				*/
	extern long			lBufSize;		/* Number of bytes in pchCpyBuffer	*/


	Buf.Start = pchCpyBuffer;
	Buf.Next = pchCpyBuffer;

	Buf.BufOffset = 0L;
	Buf.FileOffset = 0L;

	Buf.BufLen = lBufSize;
	Buf.BytesFree = Buf.BufLen;
}


/***************************************************************************/
/* Reads a specified file and appends it to the copy buffer. The copy		*/
/* buffer is flushed to disk as often as necessary.								*/
/*																									*/
/* int BufCopyFile( char *szPath, LONG lOffset, LONG lBytes )					*/
/*																									*/
/* ARGUMENTS:	szPath	- Ptr to full path name for file to be read.			*/
/*					lOffset	- Offset in the file where read is to start			*/
/*					lBytes	- Number of bytes to append.								*/
/*	GLOBALS:		Buf		- Struct must have been initialized						*/
/*																									*/
/*																									*/
/* RETURNS:		int		- OK if success else disk error from BufFlush()		*/
/*	GLOBALS:		Buf		- Updates struct about this append						*/
/*																									*/
/***************************************************************************/

int BufCopyFile( char *szPath, LONG lOffset, LONG lBytes )
{
	unsigned		uToRead;			/* Count of bytes move in the loop */
	int			iFile;			/* Open file handle */
	int			iStatus;			/* Return status							*/

	unsigned		uRead;

					/* Open file and seek to specified offset */
	if ( _dos_open( szPath, O_RDONLY, &iFile ) == OK &&
				( (lOffset != 0L &&
		  				_dos_seek( iFile, lOffset, SEEK_SET ) == lOffset) ||
					lOffset == 0L ) )
		iStatus = OK;

	else
		iStatus = ProcDiskErr( BAD_SOURCE );

				/* Keep appending and flushing until all requested */
				/* bytes have been appended or error is detected	*/
	while ( iStatus == OK && lBytes > 0  )
	{
		Buf.Next = NormalizePtr( Buf.Next );

				/* Max bytes that can be read from file is 64K - 16 */

		uToRead = (unsigned)( Buf.BytesFree >= 0xfff0L ) ? 
						  0xfff0 : (unsigned)(Buf.BytesFree);

				/* Use smaller of uToRead and lBytes */

		if ( (LONG)uToRead > lBytes )
			uToRead = (unsigned)(lBytes);;

		if ( _dos_read( iFile, Buf.Next, uToRead, &uRead ) == OK &&
			  uRead == uToRead )
			;
		else
			iStatus = ProcDiskErr( BAD_SOURCE );

					/* Update information about the buffer */

		Buf.Next += (LONG)uRead;
		Buf.BytesFree -= (LONG)(uRead);
		lBytes -= (LONG)(uToRead);
		Buf.BufOffset += (LONG)(uRead);

					/* If vPtr is full we need to flush it. */
		if ( Buf.BytesFree == 0 )
			iStatus = BufFlush();
	}

	iStatus = CloseFile( &iFile, iStatus );

	return( iStatus );
}

/***************************************************************************/
/*																									*/
/***************************************************************************/

char far *NormalizePtr( char far *Ptr )
{
	LONG		lAddr;

	lAddr = ((LONG)Ptr & 0xffff0000L) >> 12;	/* Convert seg to real 20 bit */
	lAddr += (LONG)Ptr & 0xffffL;					/* Add offset to 20 bit addr */
	
	Ptr = (char far *)((lAddr & 0xfffffff0L) << 12);/* Creat ptr segment	*/
	Ptr += lAddr & 0xf;										/* Add ptr offset		*/

	return( Ptr );
}


