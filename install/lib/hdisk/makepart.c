/***************************************************************************/
/*																									*/
/*	MAKEPART.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Builds a DOS paritition entry from a partition map entry. Determines 	*/
/* the type of DOS partition needed based on the number of sectors in the	*/
/* partition area. To be compatible with all previous versions of DOS the	*/
/* partition will start the first sector of a cylinder and end on the last */
/* sector of a cylinder.																	*/
/*																									*/
/* BuildDosPartEntry( struct PartMap *Map, struct HdParms *Parms,				*/
/*						 struct Part *PartEntry )											*/
/*																									*/
/*	ARGUMENTS:	Map		- Ptr partition map entry									*/
/*					Parms		- Ptr to an initialize hard disk parameters struct	*/
/*					PartEntry- Ptr to partitin entry to be filled in				*/
/*	RETURNS:		void																			*/
/*																									*/
/* NOTE: Must remember that heads and cylinders are 0 based while sectors	*/
/*			are 1 based.																		*/
/*																									*/
/*	Head = ((AbsSector % SectorPerTrack) * NumberOfHeads) / SectorsPerTrack	*/
/*	Track =	AbsSector / ( NumberOfHeads * SectorsPerTrack )						*/
/*	Sector = (absSector % SectoPerTrack) + 1											*/
/*																									*/
/* Created 02-09-90 johnhe																	*/
/***************************************************************************/

#include		<alias.h>
#include 	<hdisk.h>

#define MAX_SECTORS_PER_PARTITION   (0x7FFFFFFEL / 512L)

void BuildPartEntry( struct PartMap *Map, struct HdParms *Parms,
							struct Part *Part )
{
	UINT		uSecPerCyl;								/* Sectors per cylinder			*/
	UINT		uHeads;									/* Total number of heads		*/
	UINT		uCyl;		 								/* Tmp cylinder holder			*/
	UL			ulAbsCyl;								/* Total track count  			*/
	UL			ulTmp;									/* Hold abs starting cylinder */

	uHeads = Parms->MaxHead + 1;
	uSecPerCyl = Parms->MaxSec;

				/* Determine the starting head, sector and cylinder alway		*/
				/* rounding up to the first sector next cylinder boundary		*/

	ulAbsCyl = (Map->StartSec + (UL)(uSecPerCyl-1)) / (UL)uSecPerCyl;
	uCyl = (UINT)(ulAbsCyl / uHeads); /* + (ulAbsCyl % uHeads); */

	Part->StartHead = (UCHAR)( ulAbsCyl % uHeads );
	Part->StartCylinder = (UCHAR)( uCyl & 0x00ff );			/* Cyl low 8 bits	*/

										/* Move cyl bits 9 & 8 to sector bits 7 & 6	*/
	Part->StartSector = (UCHAR)( (uCyl >> 2) & 0xc0 );
	Part->StartSector	|= 1; 									/* Always sector 1	*/

				/* Determine the ending head, sector and cylinder always 		*/
				/* rounding down to the last sector on the last full cylinder	*/

	ulTmp = ulAbsCyl;												/* Save starting cyl	*/

	/* Is this partition going to be too large (2Gig limit) */

	if (Map->EndSec - Map->StartSec > MAX_SECTORS_PER_PARTITION)
		ulAbsCyl = ((Map->StartSec + MAX_SECTORS_PER_PARTITION) -
                 (UL)(uSecPerCyl-1)) / (UL)uSecPerCyl;
	else
		ulAbsCyl = (Map->EndSec - (UL)(uSecPerCyl-1)) / (UL)uSecPerCyl;

	uCyl = (UINT)(ulAbsCyl / uHeads);				/*  + (ulAbsCyl % uHeads); */
   if (ulAbsCyl % uHeads != Parms->MaxHead)    
      uCyl--;                                   /* eliminate partial cyl   */

	Part->EndHead = (UCHAR)( Parms->MaxHead );
	Part->EndCylinder = (UCHAR)( uCyl & 0x00ff );			/* Cyl low 8 bits	*/

										/* Move cyl bits 9 & 8 to sector bits 7 & 6	*/
	Part->EndSector = (UCHAR)( (uCyl >> 2) & 0xc0 );
	Part->EndSector |= (UCHAR)uSecPerCyl;				/* Always last sector	*/

				/* Determine relative start sector and then find total sectors	*/
				/* by finding end cylinder + 1 and subtracting start sector 	*/

	Part->RelativeSector = ulTmp * (UL)uSecPerCyl;
	Part->TotalSectors = (UL)(uCyl + 1) * (UL)uSecPerCyl * (UL)uHeads - (UL)(Part->RelativeSector);

	if ( Map->TotalSecs <= FAT16_SIZE )
		Part->SystemIndicator = (UCHAR) DOS12;
	else
		Part->SystemIndicator = (UCHAR) (Map->TotalSecs <= DOS_MAX ? DOS16 : DOSNEW);

}
