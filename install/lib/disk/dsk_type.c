/***************************************************************************/
/*																									*/
/*	DSK_TYPE.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Returns the type of diskette in the specied floppy drive.					*/
/*																									*/
/*	int GetDisketteType( int Drv )														*/
/*																									*/
/*	ARGUMENTS:	Drv		- Drive letter to check										*/
/* RETURNS:		int		- Type of diskette											*/
/*								  1 == 360K														*/
/*								  2 == 720K														*/
/*								  3 == 1.2M														*/
/*								  4 == 1.44M													*/
/***************************************************************************/

#include		<stdio.h>
#include		<dos.h>

int GetDisketteType( int Drv )
{
	static long				lSize[] = { 0, 370000L, 740000L, 1300000L };
	register 				i;
	struct diskfree_t		Drive;
	long						lBytes;

	if ( _dos_getdiskfree( (unsigned)Drv - 0x40, &Drive ) != 0 )
		return( 0 );
	lBytes = (long)Drive.total_clusters *
				(long)(Drive.bytes_per_sector * Drive.sectors_per_cluster);

	for ( i = 1; i < (sizeof( lSize  ) / sizeof( long )); i++ )
		if ( lBytes < lSize[ i ] )
			break;

	return( i );
}

