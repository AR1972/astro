/***************************************************************************/
/*																									*/
/*	MAXHUGE.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns the size of the max block of huge memory that is available. The	*/
/* amount available is determined doing a DOS call to allocate the largest	*/
/* block avaliable and then freeing the allocated block and returning the	*/
/* the size of the block that was allocated.											*/
/*																									*/
/*	long GetMaxHugeSize( void )															*/
/*																									*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS:		long		-	Number of bytes of huge memory avaiable			*/
/*																									*/
/* johnhe - 10/01/89																			*/
/***************************************************************************/

#include    <stdio.h>
#include		<dos.h>

long GetMaxHugeSize( void )
{
	unsigned		uNumParagraphs;

					/* Request more than is possible to get the max size  */
	_dos_allocmem( 0xffff, &uNumParagraphs );

					/* Return paragraphs converted to bytes */
	return( ((unsigned long) uNumParagraphs) << 4);
}
