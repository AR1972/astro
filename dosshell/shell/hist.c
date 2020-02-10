;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/******************************* hist.c *********************************/
/*	Contains routines to maintain a history of the contexts of the	*/
/*	current help session. The history stack is maintained as an array*/
/*	with indices 'gstart' and 'glast' giving the start & end.	*/
/************************************************************************/

/* ZZZZZ change malloc in hist.h used in MakeCopy if problem exists!! */

#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include "hist.h"

char *gHistArr[MAXHIST];
int gfirst=0, glast=0 ; /* Implies there is one element in History array */

/************************************************************************/
/*	Routine to Initialize the history stack. 'szcontext' points to  */
/*	the first context to be present in the history.			*/
/* 	ZZ should be a macro? 						*/
/************************************************************************/
void far InitHistory(char *szcontext)
{
    gHistArr[gfirst = glast = 0] = MakeCopy(szcontext) ;
}

/************************************************************************/
/*	Routine to add another context string to the history stack. This*/
/*	stack wraps around the array and the history maintained is of the*/
/*	last MAXHIST context strings.					*/
/* BUGS: Doesn't free the context string memory when it leaves the history*/
/************************************************************************/
void far AddToHistory(char *szcontext)
{
    if (FHistArrFull()) {
	/* ZZZZ freeing of gHistArr[gfirst] needs to be done here !! */
        gHistArr[glast=gfirst] = MakeCopy(szcontext) ;
        gfirst = NextLoc(gfirst) ;
    }
    else
        gHistArr[++glast] = MakeCopy(szcontext) ;
}
