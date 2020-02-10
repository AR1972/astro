/***************************************************************************/
/*																									*/
/*	F_RDWR.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Reads or writes a requested number of bytes into a specified buffer. 	*/
/*	The number of bytes requested may be > 64K without causing a problem.	*/
/* 																								*/
/* int BigReadWrite( int iFile, char far *Buf, long lToRead )					*/
/* 																								*/
/* ARGUMENTS:	iFile 	- Open DOS file handle, file pointer must be set	*/
/* 							  to position for start of read or write				*/
/*					Buf		- Far ptr to caller's buffer								*/
/* 				lBytes	- Number of bytes to read or write						*/
/* 				fRdWr 	- Flags 0 for read and 1 for write						*/
/* RETURN:		int		- OK if no error - ERROR if read or write error		*/
/*																									*/
/* Created 10-28-89 johnhe																	*/
/***************************************************************************/

#include 	<stdio.h>
#include 	<dos.h>

#include 	<alias.h>
#include 	<strlib.h>
#include		<window.h>

int BigReadWrite( int iFile, char far *Buf, long lBytes, int RdWr )
{
	unsigned		uDone;						/* Number of bytes transfered 		*/
	unsigned		uToDo;						/* Count of bytes move in the loop	*/

																					/*lint -e64 */
	static unsigned (*Func[])( int, void far *, unsigned, unsigned *) =
													{ _dos_read, _dos_write };
																					/*lint -e64 */

	if ( RdWr ) 								/* Be sure RdWr is either 0 or 1 	*/
		RdWr = 1;
													/*lint -e530 */
							 						/* Loop to do reads or writes in 	*/
													/* allowable size blocks 0xff00		*/
	while ( lBytes > 0L )
	{											  
		Buf = NormalizePtr( Buf );		  /* Precaution to prevent segment wrap*/

													/* Figure out number of bytes to do */
		uToDo = (long)MAX_BLOCK < lBytes ? MAX_BLOCK : (unsigned)lBytes;

													/* Do the read or write 				*/
		if ( (*Func[ RdWr ])( iFile, (void far *)Buf, uToDo, &uDone ) != OK ||
			  uDone != uToDo )
			return( ERROR );						/* ERROR EXIT	*/
														/* Increment the buffer pointer	*/
		Buf = HugeAdd( Buf, (long)uToDo );	/* also normalizes the ptr 		*/
		lBytes -= (LONG)(uDone);				/* Adjust remaining bytes			*/
		UpdateByteCount( (long)(uToDo) );	/* Update the status gage			*/
	}												
	return( OK );									/* NORMAL EXIT */
}																				/*lint +e530	*/
