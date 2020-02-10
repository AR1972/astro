/***************************************************************************/
/*																									*/
/*	DSK_TCNV.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Converts the time and date returned by the a call to _dos_findfirst		*/
/* int a long value with the date in the most signifiant byte and the time	*/
/* in the least sig. bytes. This allows for simple and accurate file			*/
/* creation comparisions.																	*/
/*																									*/
/*	unsigned long ToLongTd( struct find_t *File )									*/
/*																									*/
/*	ARGUMENTS:	File		- Ptr to a filled in find_t structure					*/
/*	RETURNS:		long		- Date in most sig. byte and Time in least sig.		*/
/*																									*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include 	<disk_io.h>

unsigned long ToLongTd( struct DIR *Dir )
{
	unsigned long	Td;

	Td = (unsigned long)(*((unsigned *)(&Dir->Date)));
	Td <<= 16;
	Td += (unsigned long)(*((unsigned *)(&Dir->Time)));

	return( Td );
}
