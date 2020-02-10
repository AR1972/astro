/*** 
*interp.h - Data Dictionary for the BASIC 3.0 Interpreter
*
*	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
*
*******************************************************************************/
/*                             
/*  Standard Major Qualifiers  
/*                             
/* pX	       - a pointer to an X
/* szX	       - pointer to 1st byte of a zero terminated string
/* tX	       - table; A contiguous list of X's
/* iX	       - 0 relative index of item within a table of X's
/* oX	       - 0 relative byte offset of an item from a known base
/* fX	       - flag; bit field to signal binary types
/* bX	       - byte
/* cX	       - count of occurrances of X
/* cbX	       - count of bytes in an X
/* cwX	       - count of words in an X
/* mpXY        - An array which maps X's to Y's
/* opX	       - 16-bit value of BASIC Virtual Machine Opcode X
/* opStX       - value of opcode for statement keyword X. i.e. opStResume
/* opFnX       - value of opcode for intrinsic keyword X. i.e. opFnDate
/* exX	       - Function which emulates BASIC Virtual Machine Opcode opX
/*                             
/*  Standard Minor Qualifiers  
/*                             
/* xFirst      - first element of a list or sequence
/* xNext       - next element of a list or sequence
/* xLast       - last element of a list or sequence
/* xSrc        - Source
/* xDst        - Destination
/* xMax        - Strict upper limit, i.e. char tbXXX[XXXMAX]
/* xCur        - "Current value of" as opposed to xMax or xFirst
/* xT	       - Temporary
/*
/* In general, when a variable is of a type defined by a typedef the first
/* characters of the variable name are the type name.  For example, sdX is
/* an occurrence of a string descriptor.
/*
/*
***************************************************************************/

#define INTERP_H -1		 /* remember that interp.h has been included */

/* ALL DECLARATIONS and other entities not allowed in DEF files must be under */
/* NO_DECL (#ifndef) switch so DEF files may use version.h and the C	      */
/* preprocessor 							      */

#ifndef NO_DECL
#define TRUE 1
#define FALSE 0

#ifndef NULL			 /* to preclude duplicate definition warnings */
#define NULL 0			 /* System NULL for pointers */
#endif

#define UNDEFINED (ushort)65535  /* System NULL for offsets */
#define MAX_LN (ushort)65529     /* maximum legal line number */
#define CB_IDNAM_MAX 40          /* [1] max number of bytes in an id. name */

/* Variable/Literal Explicit Type Enumerations (predefined oTyp values) */
/* be sure to add all new types to the global number definitions	*/

#define  ET_IMP      0		    /* Implicitly typed variable	*/
#define  ET_I2	     1		    /* 16 bit signed integer		*/
#define  ET_I4	     2		    /* 32 bit signed integer		*/

#define  ET_R4	     3		    /* 32 bit real			*/
#define  ET_R8	     4		    /* 64 bit real			*/

#define  ET_MaxNum   ET_R8	    /* [6]				*/

#define  ET_SD	     (ET_MaxNum+1)  /* [6] String descriptor		*/

#define  ET_FS	     (ET_SD+1)	    /* [6]				*/
#define  ET_MaxStr   ET_FS	    /* [10][6]				*/

#define  ET_MAX      ET_MaxStr	    /* [10]				*/

/* WARNING: be sure to make equivalent types in interp.h		*/
/* global et type numbers.  all new types should be appended to the end.*/
/* added for [13]							*/

/* the next 5 types MUST be in the same order as the et types in QB 4.0 */

#define     GLBL_IMP	    0		/* Implicitly typed variable	*/
#define     GLBL_I2	    1		/* 16 bit signed integer	*/
#define     GLBL_I4	    2		/* 32 bit signed integer	*/
#define     GLBL_R4	    3		/* 32 bit real			*/
#define     GLBL_R8	    4		/* 64 bit real			*/
#define     GLBL_SD	    5		/* String descriptor		*/

#define     GLBL_CY	    6
#define     GLBL_TX	    7		/* Text descriptor		*/
#define     GLBL_FS	    8		/* Fixed length string		*/
#define     GLBL_FT	    9		/* Fixed length text		*/
#define     GLBL_FIELD	    10
#define     GLBL_FORM	    11
#define     GLBL_MENU	    12

#define     GLBL_ET_MAX     12		/* number of global et types defined */


#define	STARTOTX 0	 /* [11] */

#define     opLitI2Max	    10		// Maximum literal with own executor

/*************************************************************************
*	C specific definitions
*************************************************************************/
#define REG1 register	/* The 8086 has only 2 register variables */
#define REG2 register
#define REG3
#define REG4
#define REG5
#define REG6
#define REG7
#define REG8
#define REG9

#define VOID void		 /* EB_API */ /* For functions that return no value */
#define NEAR near		 /* EB_API */
#define FAR far 		 /* EB_API */
#define PLM pascal		 /* EB_API */
#define CDECL cdecl		 /* EB_API */

typedef unsigned char uchar;	 /* EB_API */
typedef unsigned short ushort;	 /* EB_API */
typedef unsigned long ulong;	 /* EB_API */
typedef ushort boolean; 	 /* EB_API */ /* -1 = TRUE, 0=FALSE	 */
typedef ushort word;		 /* EB_API */
typedef ulong  dword;		 /* EB_API */
typedef uchar  byte;		 /* EB_API */
typedef uchar  bool;

/* String Descriptor definition */
typedef struct sd {
	ushort	cb;		/* Count of data bytes in string */
	char *	pb;		/* Address of string data	 */
	} sd;

/* This structure is handy for computing the size of an odd-size structure
	regardless of whether -Zp is specified in the compile line or not */
typedef struct oneChar {
	char singleChar;
	} oneChar;

/*************************************************************************
*	Global Variables
*************************************************************************/

/* Only one module has DEFINE_VARIABLES set, so the variables
	are not external in only this module. */

#if DEFINE_VARIABLES
#define EXTERNAL
#define INIT(x) = x
#else
#define EXTERNAL extern
#define INIT(x)
#endif

/*************************************************************************
*	DEBUG Aids 
*************************************************************************/
	/* RELEASE versions can't have DEBUG code. */

#if !DEBUG_H
#include "debug.h"
#endif

/*************************************************************************
*	MATH for C source
*************************************************************************/


/* The type of the floating point variables depends on the math pack.  When 
	using other than the C math pack, one cannot use the "double" or "float" 
	types because their definition alone causes C-math pack invocation */

typedef struct DOUBLE {ulong high; ulong low;} DOUBLE;
typedef struct FLOAT {ulong value;} FLOAT;
#define double ERROR
#define float	ERROR
#endif
