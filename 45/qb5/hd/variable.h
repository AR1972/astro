/*** 
* Variable.h - QB5-specific type definitions and data for the Variable Manager
*
*	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
*
*******************************************************************************/

#undef VARIABLE_H					/* it will have been defined as 0 in switch.h */
#define VARIABLE_H -1			/* to prevent duplicate #include's	*/

/* Array Descriptor definition */
typedef struct dm {
	ushort	cElements;		/* Count of elements for this dimension */
	ushort	iLbound;			/* Lower bound of this dimension */
	} dm;

typedef struct ad {
	char		far *pData;		/* Address of data */
	ushort	pNext;
	ushort	cPhysical;
	uchar		cDims;			/* Count of dimensions of the array */
	char		fFeatures;		/* For future features that impact the ad */
	ushort	oAdjusted;		/* Adjusted offset */
	ushort	cbElement;		/* Size of a single array element */
	char		tDM;				/* First byte of Dimension specific information */
	} ad;

/* fFeatures flag definitions */
#define	FADF_STATIC	0x40	/* Array is a $STATIC array */
#define	FADF_SD		0x80	/* Array is a string array */


/* Common Block entry */
/* NOTE: code exists (in context.asm) to adjust backpointers in grs.bdtPrs,
			grs.bdtMrs, or grs.bdtComBlk - - - one piece of code handles all
			three table types, and counts on the fact that the entry types
			for the latter two tables have 3 bd's, bdtPrs has a single bd, and 
			that these bd's are adjacent, and at the beginning of the entry.
			This code is also written to make a special check which assumes
			bdValue is the last bd in this structure.										*/
typedef struct com {
	ushort	ogNam;		/* [5] This is the name of the common data block.
				   [5] In the case of unnamed common, this field
				   [5] contains a 0.			      */
	bd	bdType; 	/* This is the owner field for the type block,
				   which has entries for all the values. For
				   format of this table, see ..\id\common.doc	   */
	bd	bdValue;	/* This is the owner field for the data block,
				   which holds the actual values of common
				   variables												   */
	ushort oTypCur; 	/* Current offset into type table  */
	ushort oValCur; 	/* Current offset into value table */
	} com;

/* value part of a scaler COMMON variable												*/
typedef struct comRef {
	ushort	oCommon;	/* Offset into table of COMMON blocks if fCommon 	*/
	ushort	oValue;	/* Offset into COMMON block oCommon 					*/
	} comRef;

/* value part of a COMMON array variable											*/
typedef struct aCom {
	ushort	oCommon;   // Offset into table of COMMON blocks if fCommon
	uchar	cDims;	   // Dimension count - set by varmgr at var creation
	uchar	filler1;
	ushort	oValue;    // Offset into COMMON block oCommon
	} aCom;	

#if !VARMGR_H
#include "varmgr.h"				/* include generic QB .h file			*/
#endif

#define VAR_INIT_SIZE	10	/* number of bytes to initially grow var table by 
										when creating a new entry - - - a large number
										lowers the number of vars the user can add and 
										still continue, a small number increase the
										chance that the initial request will ultimately
										be too small												*/

#define ONamOf(pVar)	*((ushort *)((char *)pVar + VAR_oNam))
#define OHashLinkOf(pVar)	*((ushort *)((char *)pVar + VAR_oHashLink))
#define FlagsOf(pVar)	*((ushort *)((char *)pVar + VAR_flags))
#define ValueOf(pVar,valType)	((val *)((char *)pVar + VAR_value))->valType

#define OTypOf(pVar) (FlagsOf(pVar) & 0x07 ? \
								FlagsOf(pVar) & 0x07 : \
								*((ushort *)((char *)pVar + VAR_oTyp)))


/* flag constants used for the flags word in the above (var) structure
	NOTE: These flags overlap with another set of flag constants defined below;
			Do not change these without changing equiv.s (of the same name) below.
																										*/
#define FV_STATIC		0x01 	 /* TRUE implies array is $STATIC array				*/

#define FV_TYP_MASK	0x07	 /* used to mask off all but oTyp bits from flags	*/

#define FVFUN			0x0008 /* FUNCTION - DEF FN or a FUNCTION						*/
#define FVCOMMON		0x0010 /* COMMON variable                          		*/
#define FVSTATIC		0x0020 /* variable used in a STATIC statement				*/
#define FVSHARED		0x0040 /* If entry in a tPV, value is an oMV, else ...	*/
#define FVFORMAL		0x0080 /* TRUE if var is a procedure formal					*/
#define FV_STATICSET	0x0100 /* TRUE implies $STATIC flag set (in oHashLink)	*/
#define FVEVEREXPLICIT 0x0200 /* TRUE if var was EVER referenced explicitly	*/
#define FVARRAY		0x0400 /* Variable is an array									*/
#define FVINDEXED		0x0800 /* TRUE if var name was followed with a '('			*/
#define FVVALUESTORED 0x1000 /* TRUE if actual value is stored in entry; if
										 FALSE, value is an oMV or an oFrame				*/
#define FVDECLDVAR	0x2000 /* TRUE if var was explicitly declared				*/
#define FVREDIRECT	0x4000 /* TRUE if there's likely an old oVar in pcode		*/
#define FVCONST		0x8000 /* TRUE if defining a CONSTant							*/

/* Global inputs and outputs to the MakeVariable routine. 
	The constants given below are used with the global mkVar.flags
	NOTE: These flags overlap with another set of flag constants defined above;
			Do not change these without changing equiv.s (of the same name) above.
																										*/
#define FVFUNCTION	0x0008 /* TRUE if in a FUNCTION definition or declare		*/
#define FVCOMMON		0x0010 /* TRUE if input is from a COMMON declaration		*/
#define FVSTATIC		0x0020 /* TRUE if input is from a STATIC statement			*/
#define FVSHARED		0x0040 /* TRUE if SHARED keyword associated with var		*/
#define FVFORMAL		0x0080 /* TRUE if a formal in a proc def. or declare		*/
#define FVIMPLICIT	0x0100 /* TRUE if user didn't explicitly type variable
												NOTE: This is NOT an input; it's set
														by MakeVariable as if it were			*/
#define FVLVAL			0x0200 /* TRUE if on left side, or in INPUT, READ stmt	*/
#define FVFORCEARRAY	0x0400 /* TRUE if input variable is DEFINITLY an array	*/
#define FVINDEXED		0x0800 /* TRUE if input could be an array or Function		*/
#define FVDIM    		0x1000 /* TRUE if input found in a DIM statement			*/
#define FVASCLAUSE	0x2000 /* TRUE if var type declared via an AS clause		*/
#define FVFNNAME		0x4000 /* TRUE if var name is of the form FNxxxx. 
												NOTE: This is NOT an input; it's set 
														by MakeVariable as if it were			*/
#define FVCONST		0x8000 /* TRUE if var entry is for a CONSTant				*/

/*  Global inputs and outputs to the MakeVariable routine.
    The below constants match existing FV constants (above), and are
    present to allow parser and rude scanner changes go in without switches.
*/

#define FVI_FUNCTION		0x0008	/* [2] */
#define FVI_COMMON		0x0010	/* [2] */
#define FVI_STATIC		0x0020	/* [2] */
#define FVI_SHARED		0x0040	/* [2] */
#define FVI_FORMAL		0x0080	/* [2] */
#define FVI_IMPLICIT		0x0100	/* [2] */
#define FVI_LVAL			0x0200	/* [2] */
#define FVI_ARRAY			0x0400	/* [2] */
#define FVI_INDEXED		0x0800	/* [2] */
#define FVI_DIM    		0x1000	/* [2] */
#define FVI_ASCLAUSE		0x2000	/* [2] */
#define FVI_FNNAME		0x4000	/* [2] */
#define FVI_CONST			0x8000	/* [2] */

/*
	oTypComMax and oValComMax are used by the scanner to trim
	a BLANK common block during a CHAIN.  They are initially
	0, and get set to the max by the scanner.  They are reset
	by ResetCommon, and the txtmgr calls SsTrimCommon to trim
	back the block at the end of SystemScan.			*/

extern	ushort	oTypComMax;	/* Max oTyp for all BLANK Common declarations */
extern	ushort	oValComMax;	/* Max oVal for all BLANK Common declarations */

ushort	FAR  MakeCommon(ushort);
VOID		NEAR ClearCommon(VOID);
VOID		NEAR ResetCommon(VOID);

/* Non-RELEASE support, used with vardebug.c */
