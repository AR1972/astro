/***************************************************************************/
/*																									*/
/*	FINDPART.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Scans a partition map and returns the index to the largest free area.	*/
/* If no free partition area is found it returns -1.								*/
/*																									*/
/* int FindMaxFreePart( struct PartMap *Map )										*/
/*																									*/
/*	ARGUMENTS:	Map	- Ptr to partition map											*/
/*	RETURNS:		int	- Index to largest free area in the map or -1 if no	*/
/*							  free area is found												*/
/*																									*/
/* Created 02-09-90 johnhe																	*/
/***************************************************************************/

#include		<alias.h>
#include 	<hdisk.h>

int FindMaxFreePart( struct PartMap *Map )
{
	register		i;							/* Loop indice 								*/
	register		iBigFree;				/* Index to largest free parition		*/
	UL				ulBiggest; 				/* Sectors in biggest free partition	*/

	ulBiggest = 0L;
	iBigFree = -1; 						/* Signals no free parition				*/

	for ( i = 0; i < MAX_MAP_ENTRIES; i++, Map++ )
	{
	 	if ( Map->PartType == 0 && Map->TotalSecs > ulBiggest )
		{
			ulBiggest = Map->TotalSecs;
			iBigFree = i;
		}
	}
	return( iBigFree );
}


