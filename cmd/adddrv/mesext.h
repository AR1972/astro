;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/***********************************************************************
 *
 *  MESEXT.h
 *	ADDDRV/DELDRV command external define at message data.
 *
 *----------------------------------------------------------------------
 *  Modification History
 *	MSKK00	June 1, 1987	akia
 *		Make out this module.
 *----------------------------------------------------------------------
 *	MSKK01	Sep 1, 1987	akitok
 *		Delete "panic:" messages.
 *----------------------------------------------------------------------
 *	MSKK02	Feb 21, 1989	yukini
 *		for DOS Version 4.0
 *----------------------------------------------------------------------
 *	MSKK20	Sep 28, 1990	RokaH
 *		Add help messages
 *----------------------------------------------------------------------
 *	MSKK21	Jan 08, 1991	RokaH
 *		Add Swapper message
 *----------------------------------------------------------------------
 */


/* MSKK02 *********************************************************************
extern	char	Copyright[],	Noconf[],	Dialready[];
extern	char	Devfalure[],	Unkown[],	Ctrlc[];
extern	char	Nodev[],	Cannotopen[],	Cannotget[];
extern	char	Toobig[],	Cannotread[],	Cannotalloc[];
extern	char	Cannotfree[],	Cannotmodify[],	Allocmax[];
*/
/*------------------------------------------------<MSKK01>--
 *extern	char	Ddissued[],	Harderr[],	Panicmes[];
 *----------------------------------------------------------
 */
/* MSKK02
extern	char	Ddissued[],	Harderr[];
extern	char	Badversion[];
******************************************************************************/


/*
	Message number for message retriever
*/
#define	Copyright	11
#define	Noconf		12
#define	Dialready	13
#define Devfalure	14
#define	Unknown		15
#define	Ctrlc		16
#define	Nodev		17
#define	Cannotopen	18
#define	Cannotget	19
#define	Toobig		20
#define	Cannotread	21
#define	Harderr		22
#define	Cannotalloc	23
#define	Cannotfree	24
#define	Cannotmodify	25
#define	Allocmax	26
#define	Ddissued	27
#define	Crlfmsg		28

#define ToomanyParm	29

#define AdddrvHelp	30		/* MSKK20 */
#define DeldrvHelp	31		/* MSKK20 */

#define SwapperExist	32		/* MSKK21 */

/*--------------------------------------<MESEXT.H end>--*/
