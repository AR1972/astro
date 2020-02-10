/***************************************************************************/
/*																									*/
/*	HUGENORM.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Accepts a far pointer and returns a new pointer to the same location 	*/
/* which has been normalize so that the offset is always < 16.					*/
/*																									*/
/* char far *NormalizePtr( char far *Ptr )											*/
/* 																								*/
/* ARGUMENTS:		Ptr		- Far pointer to be normalized						*/
/* RETURNS		char far *	- Normalized far pointer								*/
/* 																								*/
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#define		UL 	unsigned long

char far *NormalizePtr( char far *Ptr )
{
	UL 	lAddr;

	lAddr = ((UL)Ptr & 0xffff0000L) >> 12;		/* Convert seg to real 20 bit */
	lAddr += (UL)Ptr & 0xffffL;					/* Add offset to 20 bit addr */
	
	Ptr = (char far *)((lAddr & 0xfffffff0L) << 12);/* Creat ptr segment	*/
	Ptr += (unsigned)(lAddr & 0xf);						/* Add ptr offset		*/

	return( Ptr );
}
