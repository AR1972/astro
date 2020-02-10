/***************************************************************************/
/*																									*/
/*	GETSECT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Calculates the total sectors on a hard disk based on the values in a 	*/
/* hard disk parameters structure.														*/
/* 																								*/
/* UL GetTotalHdSectors( struct HdParms *Parms )									*/
/* 																								*/
/* ARGUMENTS:	Parms 			- Ptr to hard disk parameters structure		*/
/* RETURNS: 	unsigned long	- Total physical sectors on the hard disk		*/
/* 																								*/
/* NOTE: 																						*/
/* 		Need to remember that heads and cylinders begin with 0 and			*/
/* 		sectors begin with 1.															*/
/* 																								*/
/* Created 02-07-90 johnhe																	*/
/***************************************************************************/

#include		<alias.h>
#include 	<hdisk.h>


UL GetTotalHdSectors( struct HdParms *Parms )
{
	return( (UL)(Parms->MaxHead+1) *
			  (UL)(Parms->MaxSec) *
			  (UL)( Parms->MaxCyl+1) );
}


