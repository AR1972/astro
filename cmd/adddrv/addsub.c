;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/***********************************************************************
 *
 *  ADDSUB.C
 *	ADDDRV command sub function.
 *
 *----------------------------------------------------------------------
 *  Modification history
 *----------------------------------------------------------------------
 *	MSKK01	June 1, 1987	akik
 *			Update to alph version.
 *----------------------------------------------------------------------
 *	MSKK02	Sep 1, 1987	akitok
 *			Delete "panic:" messages.
 *----------------------------------------------------------------------
 *	MSKK10	Feb 10,1989	yukini
 *			for DOS Version 4.0 & MEM program
 *----------------------------------------------------------------------
 *	MSKK20	Aug 25, 1990	RokaH
 *		support DEVICEHIGH
 *----------------------------------------------------------------------
 */


#include "common.h"
#include "mesext.h"

#include "addsub.h"

/*------------------------------------------------<MSKK02>--*/
#ifndef BUGFIX
#define	BUGFIX
#endif
/*----------------------------------------------------------*/


CONFTAB comtab[] = {
	{ 'D', "DEVICE" },
	{ '9', "DEVICEHIGH SIZE" },
	{ 'U', "DEVICEHIGH" },
	{ '0', "REM" },
	{ 'Z', 0 }

//	{ 'B', "BUFFERS" },
//	{ 'C', "BREAK" },
//	{ 'F', "FILES" },
//	{ 'X', "FCBS" },
//	{ 'L', "LASTDRIVE" },
//	{ 'M', "MULTITRACK" },
//	{ 'P', "DRIVPARM" },
//	{ 'Q', "COUNTRY" },
//	{ 'S', "SHELL" },
//	{ 'I', "INSTALL" },
//	{ 'Y', "COMMENT" },
//	{ '1', "SWITCHES" },
//	{ 'H', "DOS" },
};


WORD memorg = 0;
WORD membase = 0;
WORD memsize = 0;

WORD himemorg = 0;				/* MSKK20 */
WORD himembase = 0;				/* MSKK20 */
WORD himemsize = 0;				/* MSKK20 */






void	panic(mes)						/* MSKK10 */
int mes;
{

#if	0						/* MSKK10 */
/*------------------------------------------------<MSKK02>--*/
#ifdef	BUGFIX
	printf( "\007%s\r\n", mes);
#else
	printf( "\007%s%s\r\n", Panicmes, mes);
#endif
/*----------------------------------------------------------*/
#endif

	Display_msg(mes,fileno(stdout),0,0,0);			/* MSKK10 */
	Display_msg(Crlfmsg,fileno(stdout),0,0,0);		/* MSKK10 */
	reset24();			/* restore int24h	*/
	reset23();			/* restore int23h	*/
	keyon();			/* key input enable	*/
	exit(1);
}


void	get_dosvar()
{
	union REGS rg;
	struct SREGS sreg;

	rg.h.ah = 0x52;				/* get DOS variable pointer */
	intdosx(&rg, &rg, &sreg);
	FP_SEG(sysinfo) = sreg.es;
	FP_OFF(sysinfo) = rg.x.bx;
}


fm_strategy(func)
int func;
{
	union REGS rg;
	WORD org;

	rg.x.ax = 0x5800;			/* get memory strategy */
	intdos(&rg, &rg);
	org = rg.x.ax;

	rg.x.bx = func;				/* strategy function */
	rg.x.ax = 0x5801;			/* set memory strategy */
	intdos(&rg, &rg);
	return(org);				/* return original */
}

umblink(func)					/* MSKK20 */
int	func;
{
	union REGS rg;
	WORD org;

	rg.x.ax = 0x5802;			/* get UMB link status */
	intdos(&rg, &rg);
	org = rg.x.ax & 0x00ff;

	rg.x.bx = func;
	rg.x.ax = 0x5803;			/* set UMB link */
	intdos(&rg, &rg);
	return(org);				/* return original */
}


dos_alloc(size, strat)
WORD size;
int strat;
{
	union REGS rg;
	WORD mem;

	mem = fm_strategy(strat);		/* set memory strategy */

	rg.x.bx = size;
	rg.h.ah = 0x48;				/* alloc mem */
	intdos(&rg, &rg);
	if (rg.x.cflag)
		panic( Cannotalloc );

	fm_strategy(mem);			/* restore strategy */
	return(rg.x.ax);
}


void	dos_free(mem)
WORD mem;
{
	union REGS rg;
	struct SREGS sreg;

	sreg.es = mem;
	rg.h.ah = 0x49;				/* free memory */
	intdosx(&rg, &rg, &sreg);
	if (rg.x.cflag)
		panic( Cannotfree );
}


dos_realloc()
{
	union REGS rg;
	struct SREGS sreg;

	sreg.es = memorg;			/* base */
	rg.x.bx = membase - memorg;		/* size */
	rg.h.ah = 0x4A;				/* modify memory */
	intdosx(&rg, &rg, &sreg);
	if (rg.x.cflag)
		return( 0 );			/* error	*/
	/* set memory owner itself */
	/* MSKK10 
	((ARENA far *)TO_LP(memorg-1,0))->owner = memorg;
	*/
	((ARENA far *)TO_LP(memorg-1,0))->owner = 9;		/* MSKK10 */

	/* MSKK10 start */
	rg.h.ah = 0x62;
	intdos(&rg, &rg);
	movedata(rg.x.bx - 1,0x0008,memorg - 1,0x0008,8);
	/* MSKK10 end */
	return( -1 );				/* good	*/
}

dos_reallochi()					/* MSKK20 */
{
	union REGS rg;
	struct SREGS sreg;

	if (himemorg == 0)
		return (-1);
	if (himembase - himemorg == 0)
	{
		dos_free(himemorg);
		return (-1);
	}
	sreg.es = himemorg;			/* base */
	rg.x.bx = himembase - himemorg;		/* size */
	rg.h.ah = 0x4A;				/* modify memory */
	intdosx(&rg, &rg, &sreg);
	if (rg.x.cflag)
		return( 0 );			/* error	*/
	/* set memory owner itself */
	/* MSKK10 
	((ARENA far *)TO_LP(himemorg-1,0))->owner = himemorg;
	*/
	((ARENA far *)TO_LP(himemorg-1,0))->owner = 9;		/* MSKK10 */

	return( -1 );				/* good	*/
}


void	dos_maxalloc()
{
	union REGS rg;
	WORD mem;

	mem = fm_strategy(ALLOC_FIT);		/* set strategy for best */
	rg.x.bx = 0xFFFF;			/* for maximam */
	rg.h.ah = 0x48;				/* alloc memory */
	intdos(&rg, &rg);			/* error at first time */
	rg.h.ah = 0x48;				/* alloc memory */
	intdos(&rg, &rg);
	if (rg.x.cflag)
		panic( Allocmax );
	fm_strategy(mem);			/* restore strategy */
	memsize = rg.x.bx;			/* get size */
	memorg = membase = rg.x.ax;		/* get base */

	/* MSKK10 start */
	rg.h.ah = 0x62;
	intdos(&rg, &rg);
	movedata(rg.x.bx - 1,0x0008,memorg - 1,0x0008,8);
	/* MSKK10 end */
}

void	dos_maxallochi()				/* MSKK20 */
{
	union REGS rg;
	WORD mem;
	WORD umb;

	mem = fm_strategy(ALLOC_FIT | HIGH_FIRST);	/* set strategy for best */
	umb = umblink(LINK_UMB);

	rg.x.bx = 0xFFFF;			/* for maximam */
	rg.h.ah = 0x48;				/* alloc memory */
	intdos(&rg, &rg);			/* error at first time */
	rg.h.ah = 0x48;				/* alloc memory */
	intdos(&rg, &rg);

	himemsize = rg.x.bx;			/* get size */
	himemorg = himembase = rg.x.ax;		/* get base */

	if (rg.x.cflag || !checkumb(himemorg))
	{
		dos_free(himemorg);
		himemorg = himembase = 0;
		himemsize = 0;
	}
	fm_strategy(mem);			/* restore strategy */
	umblink(umb);
}

/*
 * Memory allocation by myself
 *
 * entry memory size is 16bytes paragraph
 * return base value is base segment
 */
my_alloc(size,type,name)						/* MSKK10 */
WORD size;
char type;
char *name;
{
	ARENA far  *ap;
	WORD base;

	if ((size +1) > memsize)		/* ckeck size */	/* MSKK10 */
		return(-1);
	base = membase;				/* return value */
	membase += size + 1;			/* update base */
	memsize -= size + 1;			/* update size */
	ap = (ARENA far *)TO_LP(base,0);
	ap->sign = type;						/* MSKK10 */
	ap->owner = base + 1;						/* MSKK10 */
	ap->size = size;						/* MSKK10 */
	if (name)							/* MSKK10 */
		lpstrncpy((char far *)ap->ArenaName,(char far *)name,8);/* MSKK10 */
	else								/* MSKK10 */
		ap->ArenaName[0] = '\0';				/* MSKK10 */
	return(base + 1);						/* MSKK10 */
}

my_allochi(size,type,name)						/* MSKK20 */
WORD size;
char type;
char *name;
{
	ARENA far  *ap;
	WORD base;

	if ((size +1) > himemsize)		/* ckeck size */
		return(-1);
	base = himembase;				/* return value */
	himembase += size + 1;			/* update base */
	himemsize -= size + 1;			/* update size */
	ap = (ARENA far *)TO_LP(base,0);
	ap->sign = type;
	ap->owner = base + 1;
	ap->size = size;
	if (name)
		lpstrncpy((char far *)ap->ArenaName,(char far *)name,8);
	else
		ap->ArenaName[0] = '\0';
	return(base + 1);
}


char
checkcom(s)
LPSTR s;
{
	register CONFTAB *p;

	for (p = comtab; p->comstr != 0; p++) {
		if(lpstrncmp(s, (LPSTR)p->comstr, strlen(p->comstr)) == 0 &&
		    isdelim(*(s+strlen(p->comstr))))		/* MSKK20 */
			break;
	}
	return(p->ind);
}


char
bufgetc(p)
BUF *p;
{
	if (p->cnt == 0)
		return(-1);
	(p->cnt)--;
	return((unsigned)*p->ptr++);
}


void	bufungetc(p, c)
BUF *p;
char c;
{
	(p->cnt)++;
	*(--(p->ptr)) = c;
}


lpread(handle, buffer, count)
HANDLE handle;
LPSTR buffer;
unsigned short count;
{
	union REGS rg;
	struct SREGS sreg;

	sreg.ds = FP_SEG(buffer);		/* set buffer pointer */
	rg.x.dx = FP_OFF(buffer);
	rg.x.cx = count;
	rg.x.bx = handle;
	rg.h.ah	= 0x3F;				/* read */
	intdosx(&rg, &rg, &sreg);
	return(rg.x.ax);
}


/**********************************************************************
 *
 *  int checkkey()
 *	check keybord status
 *
 *	input	:
 *		  none
 *
 *	output	:
 *		  int checkkey() == 0  no characters in type-ahead buffer.
 *				 != 0  characters in type-ahead byffer.
 *
 *	If CTRL-C is in the buffer, it issues INT23h.
 *
 **********************************************************************
 */


checkkey()
{
	union	REGS	irg;


	irg.h.ah = 0x0B;
	intdos( &irg, &irg );
	return((int) irg.h.al );
}

/* courtesy of ATTRIB.c */

/*DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD*/
/*                                                                           */
/*    Subroutine Name: Display_msg                                           */
/*                                                                           */
/*    Subroutine Function:                                                   */
/*       Display the requested message to a given output device              */
/*                                                                           */
/*    Input:                                                                 */
/*        (1) Number of the message to be displayed (see ADDDRV.SKL)         */
/*        (2) Output device handle                                           */
/*        (3) Number of substitution parameters (%1,%2)                      */
/*        (4) Offset of sublist control block                                */
/*        (5) Message Class, 0=no input, 1=input via INT 21 AH=1             */
/*                                                                           */
/*    Output:                                                                */
/*        The message is written to the given output device.  If input       */
/*        was requested, the character code of the key pressed is returned   */
/*        in outregs.x.ax.                                                   */
/*                                                                           */
/*    Normal exit: Message written to handle                                 */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              Sysdispmsg (module _msgret.sal)                              */
/*                                                                           */
/*DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD*/

void	Display_msg(msgnum,msghan,msgparms,msgsub,msginput)
   int   msgnum;
   int   msghan;
   int   msgparms;
   int   *msgsub;
   char  msginput;
{
	union	REGS	inregs, outregs;

   inregs.x.ax = msgnum;
   inregs.x.bx = msghan;
   inregs.x.cx = msgparms;
   inregs.h.dh = 0xff;
   inregs.h.dl = msginput;
   inregs.x.si = (WORD)msgsub;
   sysdispmsg(&inregs,&outregs);

   /* check for error printing message */
   if (outregs.x.cflag & 1) {
      outregs.x.bx = fileno(stderr); 
      outregs.x.si = 0;
      outregs.x.cx = 0;
      outregs.h.dh = 1; /* exterr_msg_class */
      outregs.h.dl = 0; /* no input */
      sysdispmsg(&outregs,&outregs);
      }
}

