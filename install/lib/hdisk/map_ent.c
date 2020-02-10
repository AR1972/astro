/***************************************************************************/
/*																									*/
/*	MAP_ENT.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Creates a partition map entry from a partition entry structure.			*/
/* 																								*/
/* void CreateMapEntry( struct PartMap *Map, struct Part *Part )				*/
/* 																								*/
/* ARGUMENTS:	Map	-	Ptr to partition map structure							*/
/* 				Part	-	Ptr to partition table entry								*/
/* RETURNS: 	void																			*/
/* 																								*/
/* Created 02-07-90 johnhe																	*/
/***************************************************************************/

#include		<alias.h>
#include 	<hdisk.h>

void CreateMapEntry( struct PartMap *Map, struct Part *Part )
{
	Map->PartType	= Part->SystemIndicator;
	Map->StartSec	= Part->RelativeSector;
	Map->EndSec		= Part->RelativeSector + Part->TotalSectors - 1;
	Map->TotalSecs = Part->TotalSectors;
}


