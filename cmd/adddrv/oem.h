;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/***********************************************************************
 *
 *  OEM.H
 *	ADDRV/DELDRV command OEM depende function header file.
 *
 *	name	      | explanation
 *  ------------------+----------------------------------------------
 *	OEM           | if def : OEM add restoring( DD ) disposition.
 *		      | if ndef: Nothing add disposition.
 *  ------------------+----------------------------------------------
 *	FIXADDR	      |	if def : Use for DI information pointer 
 *		      |			at OEM fix address.
 *		      | if ndef: Use for DI information area
 *		      |			at dinamic address.
 *  ------------------+----------------------------------------------
 *
 *----------------------------------------------------------------------
 *  Modification history
 *----------------------------------------------------------------------
 *	MSKK00	June 1, 1987	akik
 *			Make out first version.
 *----------------------------------------------------------------------
 */


/*	#define	FIXADDR	*/
/*	#define	OEM	*/


#ifdef	FIXADDR
				/* DI information area pointer	*/
#define	FIXSEG	0x0000		/* segment	*/
#define	FIXOFF	0x0000		/* offset	*/

#endif				/* #ifdef FIXADDR	*/

/*--------------------------------------<OEM.H end>--*/
