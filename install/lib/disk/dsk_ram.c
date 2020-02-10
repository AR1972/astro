/***************************************************************************/
/*																									*/
/*	DSK_BOOT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Checks to see if a disk is the MS-DOS Ramdrive by looking at the			*/
/* volume label to see has the ram drive signature.								*/
/*																									*/
/*	NOTE: This is not a very accurate test and will not be acurate if			*/
/*			someone uses the same label on a regular drive.							*/
/*																									*/
/*	ARGUMENTS:	chDrive	- The drive letter of the drive to check.				*/
/*	RETURNS:		int		- TRUE if a ramdisk vol label is found else FALSE	*/
/*																									*/
/* Created 09-01-90 johnhe																	*/
/***************************************************************************/

#include		<dos.h>
#include		<string.h>

#include		<alias.h>

/***************************************************************************/

int IsRamDrive( char chDrive )
{
	register				i;
	static char			*szPath = "X:\\*.*";
	static char			*RamDrv[] = { "MS-RAMDR.IVE",
											  "VDISK",
											  "RDV" };
	struct find_t		Info;

	*szPath = chDrive;

	if ( _dos_findfirst( szPath, _A_VOLID, &Info ) == OK )
	{
		for ( i = 0; i < (sizeof( RamDrv ) / sizeof( char * )); i++ )
		{
			if ( strnicmp( Info.name, RamDrv[i], strlen( RamDrv[i] ) ) == OK )
				return( TRUE );
		}
	}

	return( FALSE );
}


