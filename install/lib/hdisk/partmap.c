/***************************************************************************/
/*																									*/
/*	PARTMAP.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Builds a map of the entire partition layout of a hard disk. The map is	*/
/* sorted by physical starting disk sectors and contains all free and		*/
/* allocated areas of the disk.															*/
/* 																								*/
/* BuildPartMap( struct Part *Part, struct PartMap *Map,							*/
/*							 struct HdParms *Parms )										*/
/* 																								*/
/* ARGUMENTS:	Part	- Ptr to partition entry table to build the map from	*/
/* 				Map	- Ptr to array of at least 9 partition map structures	*/
/* 				Parms - Ptr to hard disk parameter structure 					*/
/* RETURNS:		void																			*/
/* 																								*/
/* Created 02-07-90 johnhe																	*/
/***************************************************************************/

#include		<stdio.h>

#include		<alias.h>
#include 	<hdisk.h>

UL BuildPartMap( struct Part *Part, struct PartMap *Map,	
					  struct HdParms *Parms )
{
	register 		i; 						/* Loop indice 							*/
	UL 				ulNext;					/* Start of next possible partition */
	UL 				ulMaxSec;				/* Max physical sector on the disk	*/
	UL 				ulTmpStart; 			/* Tmp holds partition start sector	*/
	UL 				ulTmpEnd;				/* Tmp hold last partition sector	*/
	UL 				ulMaxFree;				/* Largest free disk area				*/

	ulMaxSec = GetTotalHdSectors( Parms );

	ulNext = Parms->MaxSec;										/* Sector 1 track 1	*/
	ulMaxFree = 0L;

	for ( i = 0; i < MAX_PART_ENTRIES; i++ )
		if ( (Part+i)->SystemIndicator != 0 &&
			  (Part+i)->RelativeSector < ulNext )			/* Any partitions		*/
			ulNext = (Part+i)->RelativeSector;				/* start lower?		*/

	do
	{
		ulTmpEnd = ulMaxSec;
		for ( i = 0; i < MAX_PART_ENTRIES; i++ )
		{
			ulTmpStart = (Part+i)->RelativeSector;
			if ( ulTmpStart == ulNext )				/* Does a part start here? */
			{
				CreateMapEntry( Map, Part + i ); 	/* Found match so save it	*/
				break;
			}
			else if ( ulTmpStart > ulNext && ulTmpStart < ulTmpEnd )
				ulTmpEnd = ulTmpStart - 1; 		/* End of possible free area	*/
		}

		if ( i >= MAX_PART_ENTRIES )
		{									/* No partition found so mark free area	*/
			Map->PartType	= 0;
			Map->StartSec	= ulNext;
			Map->EndSec 	= ulTmpEnd;
			Map->TotalSecs = (Map->EndSec - Map->StartSec) + 1;

			if ( ulMaxFree < Map->TotalSecs )	/* See if biggest free area	*/
				ulMaxFree = Map->TotalSecs;
		}
		ulNext = Map->EndSec + 1;
		Map++;
	}
	while( ulNext < ulMaxSec );

	return( ulMaxFree );
}


