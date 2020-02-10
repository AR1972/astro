/***************************************************************************/
/*																									*/
/*	DSK_FREE.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns the number of free bytes on the specified disk. Uses DOS get		*/
/* disk free call for information about the disk and then calculates the	*/
/* number of free bytes.																	*/
/*																									*/
/*	long GetDiskFree( int DrvLetter )													*/
/*																									*/
/* ARGUMENTS:	DrvLetter	- DOS drive letter										*/
/* RETURNS:		long			- Number of free bytes for the specified disk	*/
/*									  or -1L if there is an error reading the disk	*/
/*																									*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include 	<dos.h>

#include 	<alias.h>
#include 	<disk_io.h>

long GetDiskFree( int DrvLetter )
{
	struct diskfree_t 	sDrive;
	unsigned					iBytesPerClus;

	DrvLetter -= 0x40;
	if ( _dos_getdiskfree( (unsigned)DrvLetter, &sDrive ) != OK )
		return( -1L );

	iBytesPerClus = sDrive.bytes_per_sector * sDrive.sectors_per_cluster;

	return( (long)(sDrive.avail_clusters) * (long)(iBytesPerClus) );
}
