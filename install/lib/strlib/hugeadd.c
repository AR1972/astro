/***************************************************************************/
/*																									*/
/*	HUGEADD.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Adds a long integer to a far pointer without causing a segment wrap. 	*/
/* The pointer that is returned will also be normalized so that the offset */
/* is always < 16.																			*/
/* 																								*/
/* char far *HugeAdd( char far *Ptr, long lBytes ) 								*/
/* 																								*/
/* ARGUMENTS:	Ptr		- Far pointer which will be added to					*/
/* 				lBytes	- Number of bytes to add to Ptr							*/
/* RETURN:		char far * - New normalized pointer 								*/
/*																									*/
/* johnhe - 10/30/89																			*/
/***************************************************************************/

#define	UL 	unsigned long

char far *HugeAdd( char far *Ptr, long lBytes )
{
	UL 		lTmp;

	lTmp = (UL)(lBytes);

	lTmp += ((UL)Ptr & 0xffff0000L) >> 12;		/* Convert seg to real 20 bit */
	lTmp += (UL)Ptr & 0xffffL;						/* Add offset to 20 bit addr	*/
	
	Ptr = (char far *)((lTmp & 0xfffffff0L) << 12);	/* Creat ptr segment		*/
	Ptr += (unsigned)(lTmp & 0xfL);						/* Add ptr offset			*/

	return( Ptr );
}
