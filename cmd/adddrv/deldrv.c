;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/***********************************************************************
 *
 *  DELDRV.C
 *	DELDRV command main function.
 *
 *	DELDRV command discription
 *		d>DELDRV
 *
 *----------------------------------------------------------------------
 *  Modification history
 *----------------------------------------------------------------------
 *	MSKK01	June 1, 1987	akik
 *			Update to alph version.
 *----------------------------------------------------------------------
 *	MSKK10	Feb 21, 1989	yukini
 *			for DOS Version 4.0
 *----------------------------------------------------------------------
 *	MSKK20	Aug 25, 1990	RokaH
 *		support DEVICEHIGH
 *----------------------------------------------------------------------
 *	MSKK21	Jan 08, 1991	RokaH
 *		support Swapper check
 *----------------------------------------------------------------------
 */

#include "common.h"
#include "mesext.h"


int	main(int argc, char *argv[]);
void	panic(int);
void	dos_free(WORD);
void	device_dd(LPDEV);
void	my_alloc(WORD, char, char *);
void	Display_msg(int, int, int, int *, char);


LPSYSINFO sysinfo = 0;
LPSAVEINFO saveinfo = 0;





main(argc, argv)
int argc;
char *argv[];
{
	union REGS rg;
	struct SREGS sreg;


	/* load messages */					/* MSKK10 */
	sysloadmsg(&rg,&rg);					/* MSKK10 */
	if (rg.x.cflag & 1) {					/* MSKK10 */
		sysdispmsg(&rg,&rg);				/* MSKK10 */
		exit(1);					/* MSKK10 */
	}							/* MSKK10 */


	if (argc == 2 && argv[1][0] == '/' && argv[1][1] == '?')	/* MSKK20 */
	{
		Display_msg(DeldrvHelp, fileno(stdout), 0, 0, 0);
		exit(1);
	}
	else if (argc > 1)
	{
		Display_msg(ToomanyParm, fileno(stderr), 0, 0, 0);
		exit(1);
	}

	if (check_swapper())			/* MSKK21 */
	{
		Display_msg(SwapperExist, fileno(stderr), 0, 0, 0);
		exit(1);
	}

	keyoff();				/* key input disable	*/

	rg.h.ah = 0x52;				/* get dos var */
	intdosx(&rg, &rg, &sreg);
	FP_SEG(sysinfo) = sreg.es;
	FP_OFF(sysinfo) = rg.x.bx;

#if	0								/* MSKK10 */
									/* MSKK10 */
	if (ver_check( version()) == FALSE )				/* MSKK10 */
		panic( Badversion );					/* MSKK10 */
#endif									/* MSKK10 */

	if ((saveinfo = search_info()) == 0)
		panic( Ddissued );

	restoring();

	fclose(stdin); fclose(stdout); fclose(stderr);
	fclose(stdaux); fclose(stdprn);

	keyon();				/* key input enable	*/
}


/************************************************************************
 *
 *  panic()
 *
 *	display error message.
 *	keybord enable.
 *	exit program.
 *
 *	input	:
 *		  message number to display
 *
 *	output	:
 *		  none.
 *
 ************************************************************************
 */


void	panic( mes )
int	mes;
{
	union REGS	r;

	/* printf( "\007%s\r\n", mes ); */				/* MSKK10 */
	Display_msg(mes,fileno(stdout),0,0,0);
	Display_msg(Crlfmsg,fileno(stdout),0,0,0);
	keyon();			/* key input enable	*/
	exit(1);
}




void
dos_free(mem)
WORD mem;
{
	union REGS rg;
	struct SREGS sreg;

	sreg.es = mem;
	rg.h.ah = 0x49;			/* free memory */
	intdosx(&rg, &rg, &sreg);
}

void
device_dd(dev)
LPDEV dev;
{
	REQ req;
	CALLDEV call;

	req.len = 22;				/* request length */
	req.code = 20;				/* command DD */
	FP_SEG(call.strategy) = FP_SEG(dev);	/* strategy entry */
	FP_OFF(call.strategy) = dev->strategy;
	FP_SEG(call.Interrupt) = FP_SEG(dev);	/* interrupt entry */
	FP_OFF(call.Interrupt) = dev->Interrupt;
	FP_SEG(call.packet) = LP_HIGH(&req);	/* request address */
	FP_OFF(call.packet) = LP_LOW(&req);
	bio(&call);				/* call device driver */
}


/***********************************************************************
 *
 *  my_alloc()
 *	dummy function.
 ***********************************************************************
 */


void	my_alloc( dumy, type , name )
WORD	dumy;
char	type, *name;
{
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