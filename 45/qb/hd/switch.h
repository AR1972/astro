/*** 
* switch.h
*
*	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
*
*******************************************************************************/

/*===========================================================================*/
/* This file is the first thing included by version.h.			     */
/*===========================================================================*/

#define ON (-1)
#define OFF 0

/*=============================================================*/
/* Switches specific to c source											*/
/* These switches are not in switch.inc								*/
/*=============================================================*/

#ifndef	DEFINE_VARIABLES			/* Allow to be defined previous to VERSION.H */
#define	DEFINE_VARIABLES	OFF	/* ON - allocate global variables.*/
#endif

#define	LINT_ARGS	OFF			/* ON - runtime lib arg types to be checked */
