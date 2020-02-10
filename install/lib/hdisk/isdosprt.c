/***************************************************************************/
/*																									*/
/*	ISDOSPRT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Checks a partition entry type to see if it is a valid DOS primary 		*/
/* partition.																					*/
/* 																								*/
/* VALID PARTITION TYPES:																	*/
/* 																								*/
/* DOS12  == 1 == FAT file system - 12 bit FAT										*/
/* DOS16  == 4 == FAT file system - 16 bit FAT										*/
/* DOSNEW == 6 == FAT file sytem	 - huge partition > 32 meg						*/
/* 																								*/
/* 																								*/
/* int IsDosPart( unsigned char PartitionType ) 									*/
/* 																								*/
/* ARGUMENTS:	PartitionType	- Partition type as found in partition table */
/* RETURNS: 	int				- TRUE if DOS primary partition else false	*/
/* 																								*/
/* Created 02-07-90 johnhe																	*/
/***************************************************************************/

#include		<alias.h>
#include 	<hdisk.h>

unsigned IsDosPart( unsigned char PartitionType )
{
	return( PartitionType == (UCHAR)DOS12 ||
			  PartitionType == (UCHAR)DOS16 ||
			  PartitionType == (UCHAR)DOSNEW );
}	


