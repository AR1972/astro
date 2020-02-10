/***************************************************************************/
/*																									*/
/*	DSK_I24.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* Interrupt 24h handler. Checks to see if the error is drive door open 	*/
/* or write protect error on a floppy disk and will prompt the user to		*/
/* remedy the problem and then return to DOS for a retry of the operation	*/
/* else will issue a fatal disk error and abort the program.					*/
/* 																								*/
/* Created 07-23-89 johnhe																	*/
/***************************************************************************/

#include		<stdlib.h>
#include 	<alias.h>
#include 	<disk_io.h>
#include 	<window.h>

/***************************************************************************/

void ProgramAbort( void );

/***************************************************************************/

#define		WRITE_PROTECT			0
#define		DRIVE_NOT_READY		2
#define		WRITE_FAULT				10

#define		IGNORE					0
#define		RETRY						1

/***************************************************************************/
																				/*lint -715 */

unsigned gbPathSearch                = FALSE;
unsigned gfCompressedDiskIsFull      = FALSE;
unsigned gfCompressedDiskCritSection = FALSE;
unsigned gfCompressedDiskInstall     = FALSE;

void interrupt cdecl far NewInt24 (unsigned es, unsigned ds,
			unsigned di, unsigned si, unsigned bp, unsigned sp,
			unsigned bx, unsigned dx, unsigned cx, unsigned ax )
{
	char		Disk;

	((void)es), ((void)ds),	((void)si),	((void)bp), ((void)sp);
	((void)bx), ((void)dx), ((void)bx), ((void)dx), ((void)cx);

	Disk = (char)('A' + (ax & 0xff));				/* Get the value of AL		*/

	ax &= 0xff00;											/* Clear al */
	ax |= RETRY;											/* Assume user will retry	*/
	di &= 0xff;												/* Mask off error code		*/

	if (di == WRITE_FAULT && gfCompressedDiskInstall)
	{
		/* The Stacker or SuperStor disk is full */
		gfCompressedDiskIsFull = TRUE;
		Int24Fail();
	}
	else if ( di == WRITE_PROTECT )
		WriteProtectPrompt( Disk );
#if 0
	else if ( di == DRIVE_NOT_READY )
		NotReadyPrompt( Disk );
#endif
	else
	{
		if ( gbPathSearch || Int24DiskError( Disk ) )
			Int24Fail();
	}
}																				/*lint +715 */
