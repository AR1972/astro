;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/***********************************************************************
 *
 *  COMMON.H
 *	ADDDRV/DELDRV command common header file.
 *
 *
 *----------------------------------------------------------------------
 *  Modification history
 *----------------------------------------------------------------------
 *	MSKK01	June 1, 1987	akik
 *			Update from alph version.
 *----------------------------------------------------------------------
 *	MSKK20	Aug 25, 1990	RokaH
 *		support DEVICEHIGH
 *----------------------------------------------------------------------
 */

#include <version.h>

#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <fcntl.h>
#include <dos.h>
#include <string.h>

#include "typedef.h"
#include "struct.h"
#include "lpointer.h"
#include "didd.h"
#include "signal.h"


/* Prototypes */

#include "bio.h"
#include "syncsigl.h"
#include "lpstring.h"
#include "oem2.h"
#include "hardint.h"
#include "keys.h"
#include "portliba.h"
#include "_msgret.h"


extern LPSYSINFO sysinfo;

extern WORD memorg;
extern WORD membase;
extern WORD memsize;

extern WORD himemorg;
extern WORD himembase;
extern WORD himemsize;

/* memory strategy of allocate */
#define	ALLOC_LOW	0x0000
#define	ALLOC_FIT	0x0001
#define	ALLOC_HIGH	0x0002

#define	HIGH_FIRST	0x0080			/* MSKK20 */
#define LINK_UMB	1			/* MSKK20 */

#define	FALSE		0
#define	TRUE		! FALSE


#define	isdelim(c)	((c)==' '||(c)=='\t'||(c)=='=')
#define iswhite(c)	((c)==' '||(c)=='\t')

/*--------------------------------------<COMMON.H end>--*/
