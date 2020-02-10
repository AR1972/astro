;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/***********************************************************************
 *
 *  ADDDRV.C
 *	ADDDRV command main function.
 *
 *	ADDDRV command discription
 *		d>ADDDRV di_file_name
 *
 *----------------------------------------------------------------------
 *  Modification history
 *----------------------------------------------------------------------
 *	MSKK01	June 1, 1987	akik
 *			Update to alph version.
 *----------------------------------------------------------------------
 *	MSKK02	July 20, 1987	akitok
 *		Add Hardware interrapt disable/enable.
 *		Change command code 20 check sequence.
 *----------------------------------------------------------------------
 *	MSKK03	Sep  1, 1987	akitok
 *		Program miss! Change operator at device_init().
 *		Problem at Ctrl-Z to CR/LF conversion.
 *----------------------------------------------------------------------
 *	MSKK04	Feb 2, 1988	akitok
 *		Add function _setenvp().
 *			for runtime start-up error2009.
 *----------------------------------------------------------------------
 *	MSKK10	Feb 10, 1989	yukini
 *		for DOS version 4.0
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

#include "addsub.h"

/*------------------------------------------------<MSKK02>--*/
#ifndef BUGFIX
#define	BUGFIX
#endif
/*----------------------------------------------------------*/




LPSYSINFO	sysinfo = 0;
LPSAVEINFO	saveinfo = 0;
BUF		confbuf = { 0 };
int		devhi;
int		devhisz;
int		devhicount = 0;
long		dhsize;

/* Prototypes for ADDDRV.C */

void	fatal(int);
void	get_conf(STR);
int	do_device(BUF *);
int	load_file(STR);
void	device_init(LPDEV, LPSTR, DWORD *, WORD *);
void	device_dd(LPDEV);
LPSFT	search_sft(LPDEV);
void	search_eof(BUF *);
int	skip_control(BUF *);
void	_setenvp(void);

int	main(int argc, char *argv[]);


main(argc, argv)
int argc;
char *argv[];
{
	char c;
	BUF *p;
	union REGS r;
	unsigned short n;
	char i;
	char far *s;

	sysloadmsg(&r,&r);					/* MSKK10 */
	if (r.x.cflag & 1) {					/* MSKK10 */
		sysdispmsg(&r,&r);				/* MSKK10 */
		exit(1);					/* MSKK10 */
	}							/* MSKK10 */

	if (argc == 2 && argv[1][0] == '/' && argv[1][1] == '?')	/* MSKK20 */
	{
		Display_msg(AdddrvHelp, fileno(stdout), 0, 0, 0);
		exit(1);
	}
	else if (argc > 2)
	{
		Display_msg(ToomanyParm, fileno(stderr), 0, 0, 0);
		exit(1);
	}

	if (check_swapper())			/* MSKK21 */
	{
		Display_msg(SwapperExist, fileno(stderr), 0, 0, 0);
		exit(1);
	}


	/* must do at first time */
	get_dosvar();				/* get dos var pointer */

	keyoff();				/* key input disable	*/
	set_sig_trap(SIGINT);			/* int23h set		*/
	set_sig_trap(SIGHDERR);			/* int24h set		*/

	/* fprintf( stderr, "%s", Copyright ); */	/* copyright message	*/ /* MSKK10 */
	/*
	Display_msg( Copyright, fileno(stderr), 0,0,0 );
	*/

	/* if (ver_check( version()) == FALSE ) */		/* MSKK10 */
		/* panic( Badversion ); */			/* MSKK10 */
	if (search_info() != 0)			/* search saveinfo */
		panic( Dialready );
	if (argc < 2)				/* check filename */
		panic( Noconf );

	get_conf(argv[1]);			/* read config file */

	dos_maxalloc();				/* alloc rest of memory */
	dos_maxallochi();			/* alloc UMB */	/* MSKK20 */

	if( sig_stat(SIGINT) != 0 )
		panic( Ctrlc );

	saving();				/* save environment */
	saveinfo->umbseg = himemorg;		/* MSKK20 */

	p = &confbuf;

	while((c = bufgetc(p)) != -1)	/* get command alias char */
	{
		if ( c == 'D' || c == 'U' )	/* device = , devicehigh = */ /*MSKK20 */
		{
			devhi = (c == 'U' ? TRUE : FALSE);	/* MSKK20 */
			devhisz = FALSE;			/* MSKK20 */
			if (do_device(p) == -1)	/* do install */
				fatal( Devfalure );
		}
		else if (c == '9')		/* devicehigh size = */	/* MSKK20 */
		{
			devhi = TRUE;
			devhisz = TRUE;
			n = p->cnt;
			dhsize = 0;
			while (((i = bufgetc(p)) != -1) && (i >= '0' && i <= '9') || (i >= 'A' && i <= 'F'))
			{
				dhsize *= 0x10;
				dhsize += (i - (i > '9' ? 'A' - 10 : '0'));
			}
			if (i != '\0' || (n - p->cnt - 1 > 5 ))
			{
				Display_msg( Unknown, fileno(stderr), 0,0,0 );	/* MSKK20 */
				Display_msg(Crlfmsg,fileno(stdout),0,0,0);
			}
			else
			{
				while (((i = bufgetc(p)) != -1) && iswhite(i))
					;
				bufungetc(p,i);
				s = p->ptr;
				while (!iswhite(*s) && *s != '\n' && *s != '\r' && *s != '\0')
					s++;
				*s = '\0';
				if (do_device(p) == -1)	/* do install */
					fatal( Devfalure );
			}
		}
		else if ( c != '0')		/* if not REM */	/* MSKK20 */
		{
			Display_msg( Unknown, fileno(stderr), 0,0,0 );	/* MSKK20 */
			Display_msg(Crlfmsg,fileno(stdout),0,0,0);
		}
			/* fatal( Unknown ); */

		while(bufgetc(p) != '\n' )	/* skip this command */
			;
	}

	checkkey();				/* check keybord status	*/
						/* and int23h enable	*/
	if (sig_stat(SIGINT) != 0)			/* check ctrl-c */
		fatal( Ctrlc );

	if (saveinfo->devcount == 0)		/* check how many devices */
		fatal( Nodev );

	if (dos_realloc() == 0)			/* shrink memory */
		fatal( Cannotmodify );

	if (dos_reallochi() == 0)			/* shrink memory */ /* MSKK20 */
		fatal( Cannotmodify );

	if (devhicount == 0)			/* MSKK20 */
		saveinfo->umbseg = 0;

	/* close all stdio */
	fclose(stdin); fclose(stdout); fclose(stderr);
	fclose(stdaux); fclose(stdprn);

	reset23();
	reset24();				/* restore int24h	*/
	keyon();				/* key input enable	*/
	return	0;				/* exit program without error */	/* MSKK10 */
}


/*
 * It must be called before exit() after saving() when error.
 * Because must be restoring.
 */
void
fatal(mes)
int mes;							/* MSKK10 */
{
	restoring();				/* restore saving data */
	panic(mes);
}


/*
 * Read configuration file into a memory(high)
 * And little modification file.
 * If error, exit to panic
 */
void
get_conf(filename)
STR filename;
{

	HANDLE h;
	long l;
	LPSTR d, tmp;
	register char c;
	register BUF *p;

	p = &confbuf;

	if ((h = open(filename, O_RDONLY)) == -1) /* open file */
		panic( Cannotopen );
	if( sig_stat(SIGHDERR))
		panic( Harderr );

	if ((l = filelength(h)) == -1L)		/* get file size */
		panic( Cannotget );
	if( sig_stat(SIGHDERR))
		panic( Harderr );

	if (l >= (long)0xFFFF)			/* check file size */
		panic( Toobig );

	p->cnt = (WORD)l;			/* convert to WORD */
/*------------------------------------------------<MSKK03>--*/
#ifdef	BUGFIX
	FP_SEG(p->ptr) = dos_alloc(TO_PARA((p->cnt+1)), ALLOC_HIGH); /* get buf */
#else
	FP_SEG(p->ptr) = dos_alloc(TO_PARA(p->cnt), ALLOC_HIGH); /* get buf */
#endif
/*----------------------------------------------------------*/
	FP_OFF(p->ptr) = 0;

	if (lpread(h, p->ptr, p->cnt) != p->cnt) /* read config file */
		panic( Cannotread );
	close(h);				/* close file */
	if( sig_stat(SIGHDERR))
		panic( Harderr );

	search_eof(p);				/* check ctrl-Z */

	lpstrnupr(p->ptr, p->cnt);		/* uppercase */

	conv_dbspace(p->ptr, p->cnt);		/* convert bouble byte space */ /* MSKK20 */

	tmp = d = p->ptr;
	while (p->cnt != 0) {
		if (skip_control(p) == -1)	/* skip leading control */
			break;

		*d++ = checkcom(p->ptr);	/* alias command */

		if (*(d-1) == '0')	/* REM */	/* MSKK20*/
		{
			while ((c = bufgetc(p)) != -1 && !isdelim(c))
				;			/* skip command */
		}
		else
		{
			while ((c = bufgetc(p)) != -1 && c != '=')
				;
		}
		
		/* c has first delimiter */
		for (; c != -1 && isdelim(c); c = bufgetc(p))
			;			/* skip right to '=' */
		/* c has first undelimiter */
		for (; c != -1; c = bufgetc(p)) { /* copy fisrt parameter */
			if ((unsigned char)c > ' ') {
				*d++ = c;	/* copy it */
			} else {	/* got delimiter of first parm */
				if (c == '\n') { /* check end */
					*d++ = c; 
				} else {	/* copy rest parameters */
					*d++ = '\0'; /* put null for delimit */
					while ((c = bufgetc(p)) != -1) {
						if ((*d++ = c) == '\n')
							break;
					}
				}
				break;
			}
		}
	}
	p->ptr = tmp;				/* restore buffer pointer */
	p->cnt = d - tmp;			/* recalc buffer size */

}


/*
 * Device driver INSTALL
 * Load driver and initialize driver.
 * And update and save the environment(SFT, sysinfo).
 * If completed, return 0.
 * If error, return -1.
 */
do_device(p)
BUF *p;
{
	LPDEV newdev, next;
	DWORD breakaddr;
	WORD dd_flag;
	LPSFT sft;
	char filename[100], *DevName, *dp;
	LPDEVINFO devi;

	lpstrcpy((LPSTR)filename, p->ptr);	/* get closer */

	if ((FP_SEG(newdev) = load_file(filename)) == -1) /* load driver */
		return(-1);
	FP_OFF(newdev) = 0;

	for (;;) {

		if (!(newdev->attr & ISCHAR))	/* check character device */ /* MSKK10 */
			return(-1);

		/* init device driver */
		device_init(newdev, p->ptr, &breakaddr, &dd_flag);

		intdisable();			/* interrapt disable.	*/
						/* MSKK02		*/
		if (newdev->attr & ISCIN)	/* check console */
			sysinfo->con = newdev;	/* update console link */
		else if (newdev->attr & ISCLOCK) /* check clock */
			sysinfo->clock = newdev; /* update clock link */
		intenable();			/* interrapt enable	*/
						/* MSKK02		*/

		if (saveinfo->devcount >= MAXDEVINFO) /* check table entry */
			return(-1);
		devi = (LPDEVINFO)&(saveinfo->devinfo[saveinfo->devcount]);
		devi->newdev = newdev;		/* save driver address */
		devi->dd_flag = dd_flag;	/* save dd support flag */

		/* update SFT link */
		intdisable();			/* interrapt disable.	*/
						/* MSKK02		*/
		if ((sft = (LPSFT)search_sft(newdev)) != 0) {
			/* got same device */
			devi->sft = sft;	/* save this sft */
			devi->olddev = sft->devptr; /* save original driver */
			sft->devptr = newdev;	/* update sft link */
			sft->firclus = FP_OFF(newdev); /* update sft link */
		} else
			devi->sft = 0;		/* indicate no change sft */
		intenable();			/* interrapt enable	*/
						/* MSKK02		*/
		saveinfo->devcount++;		/* done saving */
		if (devhi)			/* MSKK20 */
			devhicount++;
		next = newdev->next;
		newdev->next = sysinfo->dev;
		sysinfo->dev = newdev;
		if (FP_OFF(next) == -1)
			break;
		FP_OFF(newdev) = FP_OFF(next);
	}

	/* allocation for this driver */
	dp = DevName = filename;				/* MSKK10 */
	while (*dp) {						/* MSKK10 */
		if (IsDBCSLeadByte(*dp)) {			/* MSKK10 */ /*MSKK20 */
			dp++;					/* MSKK10 */
			continue;				/* MSKK10 */
		}						/* MSKK10 */
		if (*dp == ':' || *dp == '\\')			/* MSKK10 */
			DevName = dp + 1;			/* MSKK10 */
		dp++;						/* MSKK10 */
	}							/* MSKK10 */
	dp = DevName;						/* MSKK10 */
	while(*dp) {						/* MSKK10 */
		if (*dp == '.') {				/* MSKK10 */
			*dp = '\0';				/* MSKK10 */
			break;					/* MSKK10 */
		}						/* MSKK10 */
		dp++;						/* MSKK10 */
	}							/* MSKK10 */

	if (devhi)						/* MSKK20 */
		my_allochi(FP_SEG(breakaddr)+TO_PARA(FP_OFF(breakaddr))-FP_SEG(newdev),'D',DevName);	/* MSKK20 */
	else
		my_alloc(FP_SEG(breakaddr)+TO_PARA(FP_OFF(breakaddr))-FP_SEG(newdev),'D',DevName);	/* MSKK10 */
	return(0);
}


/*
 * Load device driver
 * Return value is segment(para) value where loaded.
 * If error, return -1.
 */
load_file(filename)
STR filename;
{
	int h;
	long size;
	union REGS rg;
	struct SREGS sreg;
	WORD parm[2];
	WORD base;

	if ((h = open(filename, O_RDONLY)) == -1) /* open terget file */
			return(-1);
	if( sig_stat(SIGHDERR))
		return(-1);

	size = filelength(h);			/* get file size */
	close(h);				/* close terget */
	if( sig_stat(SIGHDERR))
		return(-1);

	if (devhi)				/* MSKK20 */
	{
		if ( (devhisz && (TO_PARA(dhsize) + 1 > himemsize)) ||
			((TO_PARA(size)+1) > himemsize) )
		{
			devhi = FALSE;
			base = membase;
			if ((TO_PARA(size)+1) > memsize)
				return(-1);
		}
		else
			base = himembase;
	}
	else
	{
		base = membase;
		if ((TO_PARA(size)+1) > memsize)		/* check memory size */	/* MSKK10 */
			return(-1);
	}

	parm[0] = parm[1] = base + 1;		/* load base & reloc factor */	/* MSKK10 */ /* MSKK20 */
	sreg.es = LP_HIGH(parm);		/* parm for overlay */
	rg.x.bx = LP_LOW(parm);
	sreg.ds = LP_HIGH(filename);		/* filename */
	rg.x.dx = LP_LOW(filename);
	rg.x.ax = 0x4B03;			/* load overlay */
	intdosx(&rg, &rg, &sreg);
	if (rg.x.cflag)				/* check errro */
		return(-1);
	return(base + 1);					/* MSKK10 */ /* MSKK20 */
}


/*
 * Device driver call by INIT function.
 * Note that size and flag are return value pointer.
 * Return breakaddr of driver.
 * And return DEINSTALL function support or not.
 */
void
device_init(dev, parm, breakaddr, flag)
LPDEV dev;
LPSTR parm;
DWORD *breakaddr;
WORD *flag;
{
	REQ req;
	CALLDEV call;

	req.len = 22;				/* request length */
	req.code = 0;				/* command INIT */

/*------------------------------------------------<MSKK02>--*/
#ifndef	BUGFIX

	(long)req.bpb_array = (long)DDSIGN;

#endif
/*----------------------------------------------------------*/
	(LPSTR)req.bpb_array = parm;		/* parameter pointer */
	FP_SEG(call.strategy) = FP_SEG(dev);	/* strategy entry address */
	FP_OFF(call.strategy) = dev->strategy;
	FP_SEG(call.Interrupt) = FP_SEG(dev);	/* interrupt entry address */
	FP_OFF(call.Interrupt) = dev->Interrupt;
	FP_SEG(call.packet) = LP_HIGH(&req);	/* request address */
	FP_OFF(call.packet) = LP_LOW(&req);
	bio(&call);				/* call device driver */
	/* set return value */
	*breakaddr = req.break_addr;		/* break address */

/*------------------------------------------------<MSKK02,MSKK03>--*/
#ifdef	BUGFIX

	*flag = ((long)req.bpb_array == ~(long)parm); /* dd support check */

#else

	*flag = ((long)req.bpb_array == !(long)parm); /* dd support check */
/*						<MSKK02> */
/*	*flag = ((long)req.bpb_array == !(long)DDSIGN); *//* dd support check */

#endif
/*----------------------------------------------------------*/
}


/*
 * Device driver call by DEINSTALL function.
 */
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


/*
 * Search sft entry same as device name
 * Return value is SFT address(DWORD)
 * If not found, return 0.
 */
LPSFT
search_sft(dev)
LPDEV dev;
{
	LPSF sf;
	LPSFT sft;
	int i;

	for(sf = sysinfo->sft; ; sf = sf->link) {
		for (i = 0,sft = (LPSFT)&sf->table; i < sf->count; i++,sft++) {
			if (sft->ref_count == 0) /* check refference count */
				continue;
			if (lpstrncmp((LPSTR)dev->name, (LPSTR)sft->name, 8) == 0)
				return(sft);	/* find it */
		}
		if (LP_LOW(sf->link) == -1)	/* check end of sft */
			break;
	}
	return(0);				/* not found */
}


void	search_eof(p)
register BUF *p;
{
	char far *s;
	register WORD count;

	for (s = p->ptr, count = 0; count < p->cnt; count++, s++) {
		if (*s == 0x1A) {
			s[0] = '\r';
			s[1] = '\n';
			p->cnt = count + 2;
			break;
		}
	}
}


skip_control(p)
register BUF *p;
{
	register char c;

	while ((c = bufgetc(p)) != -1) {
		if ((unsigned char)c > ' ') {
			bufungetc(p, c);
			return(c);
		}
	}
	return(-1);
}


/*------------------------------------------------<MSKK04>--*/
#if	defined(BUGFIX)				/* <MSKK04>	*/
void	_setenvp()
{						/* runtime dummy 
						/* function	*/
}
#endif						/* <MSKK04>	*/
/*----------------------------------------------------------*/

/*------------------------------------------------<ADDDRV.C end>--*/
