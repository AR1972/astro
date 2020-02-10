/***************************************************************************/
/*																									*/
/*	DSK_ISHD.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Determines if a disk is a valid hard disk. The requirements are that		*/
/* the drive letter is C: or greater, the drive exists, is a local drive,	*/
/* is not removeable and is not the MS-DOS RamDrive.								*/
/*																									*/
/*	int IsValidHardDrive( char chDrive )												*/
/*																									*/
/*	ARGUMENTS:	chDrive	- The drive letter of the drive to check				*/
/*	RETURNS:		int		- TRUE if a hard disk else FALSE							*/
/*																									*/
/* Created 09-01-90 johnhe																	*/
/***************************************************************************/

#include		<alias.h>
#include		<disk_io.h>

/***************************************************************************/

int IsValidHardDrive( char chDrive )
{
	int	iDrvNum;

	iDrvNum = (int)chDrive - ('A' - 1);

	if ( chDrive >= 'C' )
		if ( IsValidDrive( chDrive ) )
			if ( IsLocalDrive( iDrvNum ) )
				if ( !IsRemoveable( iDrvNum ) )
					if ( !IsRamDrive( chDrive ) )
						return( TRUE );
	return( FALSE );
}

// This is just like IsValidHardDrive, except that it makes sure the 
// drive is formatted also.
int IsReallyValidHardDrive( char chDrive )
{
#define SECTOR_SIZE    512

    char  *pchBuffer;
    
    if (!IsValidHardDrive(chDrive))
        return( FALSE );

    pchBuffer = GetMemory(SECTOR_SIZE);

    if (GetBootSector(chDrive, pchBuffer) == OK)
    {
       FreeMemory(pchBuffer);
       return( TRUE );
    }   

    FreeMemory(pchBuffer);
    return ( FALSE );   
}

