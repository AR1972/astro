/***
* $Workfile:   osdep.h  $
* $Revision:   1.0  $
*   $Author:   Dave Sewell  $
*     $Date:   28 Apr 1989 17:00:10  $
*
* This header file supplies information needed to interface with the
* particular operating system and C compiler being used.
**/

#ifndef OSDEP
#define OSDEP

#include "dos.h"

/*** MS-DOS version ***/
#ifndef MSDOS
#define MSDOS	1
#endif
#define OS_TYPE 2

/**
* NULLDPTR should be the appropriate value for a null data pointer.
* NULLCPTR should be the appropriate value for a null code pointer.
**/

#if	defined(M_I86LM)
/*** In large model, both code and data pointers are longs. ***/
#define LDATA	    1
#define LCODE	    1
#define NULLCPTR    0L
#define NULLDPTR    0L
#elif	defined(M_I86MM)
/*** In middle model, code pointers are long, data pointers are short. ***/
#define LDATA	    0
#define LCODE	    1
#define NULLCPTR    0L
#define NULLDPTR    0
#elif	defined(M_I86HM)
/*** In huge model, both code pointers and data pointers are long. ***/
#define LDATA	    1
#define LCODE	    1
#define NULLCPTR    0L
#define NULLDPTR    0L
#elif	defined(M_I86CM)
/*** In compact model, code pointers are short, data pointers are long. ***/
#define LDATA	    1
#define LCODE	    0
#define NULLCPTR    0
#define NULLDPTR    0L
#elif	defined(M_I86SM)
/*** In small model, both code and data pointers are short. ***/
#define LDATA	    0
#define LCODE	    0
#define NULLCPTR    0
#define NULLDPTR    0
#else
ERROR: undefined model
#endif

/**
* The following type definition takes care of the machine dependency caused by
* the unspecified handling of sign extension in the C language.  When
* converting "char" to "int", some compilers will extend the sign, while others
* will not.  For situations where sign extension is not desired, the type
* "byte" can be used because it is equivalent to "unsigned char".
**/

typedef unsigned char  byte;
typedef unsigned short word;
typedef unsigned long  dword;

/*** Define maximum file name size, and maximum path size. ***/
/***
* On DOS, maximum path name size is 2 chars for drive and colon, 64
* for directory name, one for null.  For file names, add 13 more bytes (
* 1 for another slash, 8 for root, 1 for period, 3 for extension).
***/

#define FNAMESIZE   80
#define PATHSIZE    67

#define PATHCHAR    '\\'
#define BADPATHCHAR '/'

/**
* These next few constants and macros are not machine dependant, but
* they are nice to have around.
**/

#define TRUE	1
#define FALSE	0
#define ON	1
#define OFF	0

/*** Absolute value, maximum, and minimum defines. ***/

#define ABS(val) ( ((val) < 0 ? -(val) : (val)) )
#define MAX(val1, val2) ( ((val1) > (val2) ? (val1) : (val2)) )
#define MIN(val1, val2) ( ((val1) < (val2) ? (val1) : (val2)) )
#endif
