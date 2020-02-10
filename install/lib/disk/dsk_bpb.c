/***************************************************************************/
/*																									*/
/* DSK_BPB.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Array of default BPB structures for MS-DOS 4.x compatible floppy disk	*/
/*	drives.																						*/
/* 																								*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include		<disk_io.h>

														/* Array of valid BPB structures */
struct BPB DskBpb[] =
{
							/* 160K	- 40 tracks - 8 sectors per track - 1 head	*/
	{ 512, 1, 1, 2,  64, 1*8*40, 0xfe, 1,  8, 1, 0, 0L },

							/* 180K	- 40 tracks - 9 sectors per track - 1 head	*/
	{ 512, 1, 1, 2,  64, 1*9*40, 0xfc, 2,  9, 1, 0, 0L },

							/* 320K	- 40 tracks - 8 sectors per track - 2 heads	*/
	{ 512, 2, 1, 2, 112, 2*8*40, 0xff, 1,  8, 2, 0, 0L },

							/* 360K	- 40 tracks - 9 sectors per track - 2 heads	*/
	{ 512, 2, 1, 2, 112, 2*9*40, 0xfd, 2,  9, 2, 0, 0L },

							/* 720K	- 80 tracks - 09 sectors per track - 2 heads */
	{ 512, 2, 1, 2, 112, 2*9*80, 0xF9, 3,  9, 2, 0, 0L },

							/* 1.2M	- 80 tracks - 15 sectors per track - 2 heads	*/
	{ 512, 1, 1, 2, 224, 2*15*80, 0xF9, 7, 15, 2, 0, 0L },

							/* 1.44M - 80 tracks - 18 sectors per track - 2 heads	*/
	{ 512, 1, 1, 2, 224, 2*18*80, 0xF0, 9, 18, 2, 0, 0L }
};
