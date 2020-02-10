/***************************************************************************/
/* 																								*/
/* RAMDRV.C																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Checks to see if RAMDRV is loaded by examining the label on each 			*/
/* non-removable disk on the sytem looking for the label "MS-RAMDR.IVE		*/
/* and then using the entry's date as the version number. The search			*/
/* will stop after the first non-local drive is detected.						*/
/*																									*/
/*	unsigned GetRamDrive( void )															*/
/*																									*/
/* ARGUMENTS:	NONE																			*/
/*	RETURN:		unsigned	- Ram drive internal revision number.					*/
/*																									*/
/* Created 08-26-90 - johnhe																*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include		<string.h>
#include		<dos.h>

#include		<alias.h>
#include 	<disk_io.h>

/***************************************************************************/

unsigned GetRamDrive( void )
{
	static char			RamDrv[] = "X:\\MS-RAMDR.IVE";
	register				Drv;
	unsigned				uDate;
	struct find_t		Info;

	for ( uDate = 0, RamDrv[0] = 'C';
			uDate == 0 && IsReallyValidHardDrive( RamDrv[ 0 ] );
			RamDrv[0]++ )
	{
		Drv = (int)RamDrv[0] - 'A' + 1;
		if ( !IsRemoveable( Drv ) && IsLocalDrive( Drv) )
		{
			if ( _dos_findfirst( RamDrv, _A_VOLID, &Info ) == OK )
				uDate = Info.wr_date;

							/* This findfirst works around a bug in old versions	*/
							/* of DOS which happens when doing a findfirst for		*/
							/* a volume ID.													*/

			_dos_findfirst( RamDrv, _A_NORMAL, &Info );
		}
	}

	return( uDate );
}

