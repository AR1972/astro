/***************************************************************************/
/*																									*/
/*	DSK_TIME.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* void SetFileTimeDate( struct DIR *Dir )											*/
/* 																								*/
/* ARGUMENTS:	Dir	- Ptr to directory entry structure							*/
/* RETURNS: 	void																			*/
/*																									*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include 	<dos.h>
#include 	<disk_io.h>

void SetFileTimeDate( struct DIR *Dir )
{
	struct dostime_t	Time;
	struct dosdate_t	Date;

	_dos_gettime( &Time );
	_dos_getdate( &Date );

	Dir->Time.Hour = Time.hour;
	Dir->Time.Min = Time.minute;
	Dir->Time.tSec = Time.second / 2;

	Dir->Date.Day = Date.day;
	Dir->Date.Month = Date.month;
	Dir->Date.Year = Date.year - 1980;
}
