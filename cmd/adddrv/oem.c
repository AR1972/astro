;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/***********************************************************************
 *
 *  OEM.C
 *	ADDDRV/DELDRV command OEM depende function file.
 *
 *	function      | explanation
 *  ------------------+----------------------------------------------
 *	saving()      |	ifdef FIXADDR : The DI information area pointer
 *		      |			store to OEM fix address.
 *		      | ifndef FIXADDR: nothing to do.
 *  ------------------+----------------------------------------------
 *	restoring()   | ifdef OEM     : OEM add restoring( DD ) disposition.
 *		      | ifdef FIXADDR : The DI information area pointer
 *		      |                 clear to 0.
 *		      | ifndef FIXADDR: nothing to do.
 *  ------------------+----------------------------------------------
 *	search_info() | ifdef FIXADDR : Get from OEM fix address
 *		      |			 at DI information area pointer.
 *                    | ifndef FIXADDR: Search DI information area.
 *  ------------------+----------------------------------------------
 *	keyoff()      | key input disable.
 *  ------------------+----------------------------------------------
 *	keyon()       | key input enable.
 *  ------------------+----------------------------------------------
 *	ver_check()   | Check DOS version.
 *
 *----------------------------------------------------------------------
 *  Modification history
 *----------------------------------------------------------------------
 *	MSKK01	June 1, 1987	akik
 *		Update to alph version.
 *----------------------------------------------------------------------
 *	MSKK02	July 20, 1987	akitok
 *		Add Hardware interrapt disable/enable.
 *----------------------------------------------------------------------
 *	MSKK10	Feb 21, 1989	yukini
 *		For DOS Version 4.0
 *----------------------------------------------------------------------
 *	MSKK20	Aug 25, 1990	RokaH
 *		support DEVICEHIGH
 *----------------------------------------------------------------------
 */


#include "common.h"
#include "oem.h"		/* OEM depend header	*/

#define	LowVersion	310	/* DOS low version	*/
#define	HighVersion	321	/* DOS high version	*/


extern	void	device_dd(LPDEV);
extern	int	my_alloc(WORD, char, char*);
extern	void	dos_free(WORD);

extern LPSAVEINFO saveinfo;
extern LPSYSINFO  sysinfo;


/***********************************************************************
 *
 *  void saving()
 * 	Save environment for the after.
 *
 *	input	:
 *		  LPSYSINFO sysinfo
 *
 *	output	:
 *		  LPSAVEINF saveinfo
 *
 *	call	:
 *		  my_alloc()	addsub.c
 *		  lpstrcpy()	lpstring.c
 *
 ***********************************************************************
  */


void
saving()
{
	FP_SEG(saveinfo) = my_alloc(TO_PARA(sizeof(SAVEINFO)),'?',"INFO"); /* get info area */
	FP_OFF(saveinfo) = 0;
	lpstrcpy((LPSTR)saveinfo->sign, (LPSTR)INFOSIGN);	/* fill sign */

	intdisable();				/* interrapt disable.	*/
						/* MSKK02		*/
	saveinfo->dev = sysinfo->dev;		/* save device link */
	saveinfo->con = sysinfo->con;		/* save console link */
	saveinfo->clock = sysinfo->clock;	/* save clock link */
	saveinfo->vector = *((VECTOR far *)0);	/* save vector */
	intenable();				/* interrapt enable	*/
						/* MSKK02		*/

	saveinfo->devcount = 0;


/*+---------------------------------------------------------------------+*/
/*|									|*/
/*| Store DI information address.					|*/

#ifdef FIXADDR

	*(LPSAVEINFO far *)(TO_LP(FIXSEG, FIXOFF)) = saveinfo;

#endif /* FIXADDR */

/*|									|*/
/*+---------------------------------------------------------------------+*/
}


/***********************************************************************
 *
 *  void restoring()
 *	Restore all environment to the before.
 *
 *	input	:
 *		  LPSAVEINF saveinfo
 *
 *	output	:
 *		  LPSYSINFO sysinfo
 *			  System variavle area.
 *
 *	call	:
 *		  device_dd()		adddrv.c     deldrv.c
 *		  dos_free()		addsub.c     drldrv.c
 *		  lpstrncmp()		lpstring.c
 *
 ***********************************************************************
 */


void
restoring()
{
	int i, n;
	LPDEVINFO devi;
	LPSTR lp;


	intdisable();				/* interrapt disable.	*/
						/* MSKK02		*/
	sysinfo->dev = saveinfo->dev;		/* restore device link */
	sysinfo->con = saveinfo->con;		/* resotre console link */
	sysinfo->clock = saveinfo->clock;	/* resotre clock link */
	intenable();				/* interrapt enable	*/
						/* MSKK02		*/
	for (i = saveinfo->devcount; i > 0; i--) { /* restore sft link */
		devi = (LPDEVINFO)&(saveinfo->devinfo[i-1]);
		if (devi->dd_flag != 0)		/* check dd support */
			device_dd(devi->newdev); /* do deinstall */
		if (devi->sft != 0) {		/* check sft changed */
			intdisable();		/* interrapt disable.	*/
						/* MSKK02		*/
			devi->sft->devptr = devi->olddev; /* restore sft */
			devi->sft->firclus = LP_LOW(devi->olddev);
			intenable();		/* interrapt enable	*/
						/* MSKK02		*/
		}


/*+---------------------------------------------------------------------+*/
/*|									|*/
/*| OEM original DD disposition.					|*/

#ifdef OEM

#endif /* OEM */

/*|									|*/
/*+---------------------------------------------------------------------+*/

	}
	intdisable();				/* interrapt disable.	*/
						/* MSKK02		*/
	*((VECTOR far *)0) = saveinfo->vector;	/* resotre vector */
	intenable();				/* interrapt enable	*/
						/* MSKK02		*/
	if (saveinfo->umbseg != 0)		/* MSKK20 */
		dos_free(saveinfo->umbseg);

	del_info(FP_SEG(saveinfo) - 1);		/* MSKK20 */
	dos_free(FP_SEG(saveinfo) - 1);		/* free info & driver memory */	/* MSKK10 */


/*+---------------------------------------------------------------------+*/
/*|									|*/
/*| OEM fix address initialize.						|*/

#ifdef FIXADDR

	*(LPSAVEINFO far *)(TO_LP(FIXSEG, FIXOFF)) = 0;

#endif /* FIXADDR */

/*|									|*/
/*+---------------------------------------------------------------------+*/
}


/***********************************************************************
 *
 *  LPSAVEINFO search_info()
 *	Search DIDD information area.
 *	Return value is segment para of info area.
 *	If not found, return 0.
 *
 *	input	:
 *		  LPSYSINFO sysinfo
 *
 *	output	:
 *		  serch_inf() == 0	Not install device driver.
 *			      != 0	DI information area address.
 *
 *	call	:
 *		  device_dd()
 *
 ***********************************************************************
 */


/*+---------------------------------------------------------------------+*/
/*|									|*/
/*| Use OEM fix address.						|*/

#ifdef FIXADDR

LPSAVEINFO
search_info()
{
	return(*(LPSAVEINFO far *)TO_LP(FIXSEG, FIXOFF));
}

/*|									|*/
/*+---------------------------------------------------------------------+*/

#else /* nodef FIXADDR */

/*+---------------------------------------------------------------------+*/
/*|									|*/
/*| Search dynamic address.						|*/

LPSAVEINFO
search_info()
{
	ARENA far *p;
	int	i;

	FP_SEG(p) = *(WORD far *)((LPSTR)sysinfo - 2);
	FP_OFF(p) = 0;
	for (; p->sign == 'Z'|| p->sign == 'M';
	     FP_SEG(p) = FP_SEG(p) + p->size + 1)
	{
		i = lpstrncmp((LPSTR)p+32, (LPSTR)INFOSIGN, 16);	/* MSKK10 */
		if ( i == 0 )
			return((LPSAVEINFO)TO_LP(FP_SEG(p)+2,0)); /* find it */	/* MSKK10 */
	}
	return((LPSAVEINFO)0);			/* not found */
}

#endif /* FIXADDR */

#if 0								/* MSKK10 */

/*|									|*/
/*+---------------------------------------------------------------------+*/


/***********************************************************************
 *
 *  void keyoff()
 *	key input disable.
 *
 *	input	:
 *		  none.
 *
 *	output	:
 *		  none.
 *
 *	call	:
 *
 ***********************************************************************
 */


void keyoff()
{

/*-----------------------------------------------------------------------*/
/*|	OEM key disable routine.					|*/
/*|									|*/
/*-----------------------------------------------------------------------*/

}


/***********************************************************************
 *
 *  void keyon()
 *	key input enable.
 *
 *	input	:
 *		  none.
 *
 *	output	:
 *		  none.
 *
 *	call	:
 *
 ***********************************************************************
 */


void keyon()
{

/*-----------------------------------------------------------------------*/
/*|	OEM key enable routine.						|*/
/*|									|*/
/*-----------------------------------------------------------------------*/

}
#endif							/* MSKK10 */

#if	0						/* MSKK10 */
/***********************************************************************
 *
 *  int ver_check()
 *	Check dos version.
 *
 *	input	:
 *		  int	verno		DOS version.
 *
 *	output	:
 *		  int ver_check() == 0	incorrect DOS version.
 *				  != 0	OK!
 *
 *	call	:
 *
 ***********************************************************************
 */


ver_check( verno )
int	verno;
{
/*
	if ( verno < LowVersion	|| verno > HighVersion )
		return (int) FALSE;
	else
		return (int) TRUE;
*/
	return (verno == 400) ? TRUE : FALSE;
}
#endif

void	del_info(mem)
WORD mem;
{
	unsigned long far	*p;

	/* destroy adddrv information, in case of fragumentation   MSKK10 */
	p = (unsigned long far *)((unsigned long)(mem+1)<< 16);	/* MSKK10 */
	*p++ = 0L;						/* MSKK10 */
	*p++ = 0L;						/* MSKK10 */
	*p++ = 0L;						/* MSKK10 */
	*p++ = 0L;						/* MSKK10 */
}


/*--------------------------------------<OEM.C end>--*/
