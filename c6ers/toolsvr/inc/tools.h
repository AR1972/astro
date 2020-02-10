/*
 *  tools.h - Header file for accessing TOOLS.LIB routines
 *  includes stdio.h and ctype.h
 *
 *   4/14/86  daniel lipkie  added U_* flags for upd return values
 *
 *	31-Jul-1986 mz	Add Connect definitions
 *	02-Dec-1986 bw	Added DOS5 FindFirst buffer definition & A_ALL constant
 *	21-Jan-1987 bw	Add DWORD define
 *			Add PIPE_READ / PIPE_WRITE values
 *			Add new rspawn return typedef
 *	27-Jan-1987 bw	Make DOS 3 findType available to DOS 5
 *	18-Aug-1987 bw	change .max to .vmax to make C 5.x happy
 *	08-Mar-1988 bw	Copy WORD() and DWORD() to MAKE*()
 *	10-Mar-1988 mz	Add LOADDS/EXPORT definitions
 *	12-May-1988 mz	Add VECTOR typedef
 *	19-Aug-1988 mz	Conditionally define TRUE/FALSE
 *
 */

#if !defined (TRUE)
#define TRUE	-1
#endif

#if !defined (FALSE)
#define FALSE	0
#endif

#define     PSEPSTR "\\"
#define     PSEPCHR '\\'

typedef char flagType;
typedef long ptrType;

#define SETFLAG(l,f)	((l) |= (f))
#define TESTFLAG(v,f)	(((v)&(f))!=0)
#define RSETFLAG(l,f)	((l) &= ~(f))

#define SHIFT(c,v)	{c--; v++;}

#define LOW(w)		((int)(w)&0xFF)
#define HIGH(w) 	LOW((int)(w)>>8)
#define WORD(h,l)	((LOW((h))<<8)|LOW((l)))
#define DLOW(l) 	((long)(l)&0xFFFF)
#define DHIGH(l)	DLOW((long)(l)>>16)
#define DWORD(h,l)	((DLOW(h)<<16|DLOW(l)))
#define POINTER(seg,off) ((((long)(seg))<<4)+ (long)(off))
#define MAKEWORD(h,l)	((LOW((h))<<8)|LOW((l)))
#define MAKEDWORD(h,l)	((DLOW(h)<<16|DLOW(l)))

#define FNADDR(f)	(f)

#define SELECT		if(FALSE){
#define CASE(x) 	}else if((x)){
#define OTHERWISE	}else{
#define ENDSELECT	}

#define MAXARG	    128 		/* obsolete and begin deleted */
#define MAXLINELEN  1024		/* longest line of input */
#define MAXPATHLEN  260 		/* longest filename acceptable */


#if defined(OS2)

#define PIPE_READ   0
#define PIPE_WRITE  1


/*
 *  This is the value returned by rspawnl.  The PID field will always hold
 *  the process ID of the background process.  The in* fields will hold the
 *  handles of the pipe attached to the new processes stdin, and the out*
 *  fields correspond to stdout.  If input/output from/to a pipe has not been
 *  requested, the fields will be -1.  The fields are ordered read-write
 *  to allow a call pipe(&val.inReadHndl) or pipe(&val.outreadHndl).
*/
struct spawnInfo {
    unsigned PID;
    int inReadHndl;
    int inWriteHndl;
    int outReadHndl;
    int outWriteHndl;
};


/* buffer description for findfirst and findnext
   When DOS 3 and DOS 5 version have the same field name, the field contains
   the same information
   DOS 5 version includes the directory handle
*/

struct findType {
    unsigned	     type ;		/* type of object being searched    */
    unsigned   dir_handle ;		/* Dir search handle for FindNext   */
    unsigned  create_date ;		/* File date of creation	    */
    unsigned  create_time ;		/* File time of creation	    */
    unsigned  access_date ;		/* File date of last access	    */
    unsigned  access_time ;		/* File time of last access	    */
    unsigned	     date ;		/* File date of last write	    */
    unsigned	     time ;		/* File time of last write	    */
    long	   length ;		/* File end of data		    */
    long	    alloc ;		/* File allocation		    */
    unsigned	     attr ;		/* File attribute		    */
    unsigned char nam_len ;		/* Length of ASCIIZ name string     */
    char name[MAXPATHLEN] ;		/* ASCIIZ name string		    */
};

#define FT_DONE     0xFF		/* closed handle */
#define FT_FILE     0x00		/* enumerating files */
#define FT_SERV     0x01		/* enumerating servers */
#define FT_SHAR     0x02		/* enumerating shares */
#define FT_MASK     0xFF		/* mask for type */

#define FT_MIX	    0x8000		/* mixed case supported flag */

struct DOS3findType {
    char reserved[21];			/* reserved for start up	     */
    char attr;				/* attribute found		     */
    unsigned time;			/* time of last modify		     */
    unsigned date;			/* date of last modify		     */
    long length;			/* file size			     */
    char name[13];			/* asciz file name		     */
};

#else

struct findType {
    char reserved[21];			/* reserved for start up	     */
    char attr;				/* attribute found		     */
    unsigned time;			/* time of last modify		     */
    unsigned date;			/* date of last modify		     */
    long length;			/* file size			     */
    char name[13];			/* asciz file name		     */
};

#endif

typedef struct findType FIND;
typedef FIND near * NPFIND;


/* attributes */
#define A_RO	1			/* read only			     */
#define A_H	2			/* hidden			     */
#define A_S	4			/* system			     */
#define A_V	8			/* volume id			     */
#define A_D	16			/* directory			     */
#define A_A	32			/* archive			     */

#define A_MOD	(A_RO+A_H+A_S+A_A)	/* changeable attributes	     */
#define A_ALL	(A_RO|A_H|A_S|A_V|A_D|A_A)

#define HASATTR(a,v)	TESTFLAG(a,v)	/* true if a has attribute v	     */

extern char XLTab[], XUTab[];

struct vectorType {
    int vmax;				/* max the vector can hold	     */
    int count;				/* count of elements in vector	     */
    void *elem[1];			/* elements in vector		     */
};

typedef struct vectorType VECTOR;

#include "parse.h"
#include "exe.h"
#include "fcb.h"
#include "dir.h"
#include "times.h"
#include "ttypes.h"

/* return flags for upd */
#define U_DRIVE 0x8
#define U_PATH	0x4
#define U_NAME	0x2
#define U_EXT	0x1

/*  Connect definitions */

#define REALDRIVE	0x8000
#define ISTMPDRIVE(x)	(((x)&REALDRIVE)==0)
#define TOKTODRV(x)	((x)&~REALDRIVE)

/*  Heap Checking return codes */

#define HEAPOK           0
#define HEAPBADBEGIN    -1
#define HEAPBADNODE     -2

/* internal module prototypes here, not for api */
frenameNO(char *, char *);

/* msspawn.c: return codes */

#define RC_PREPERR   0x0100
#define RC_NOFILE    0x0200
#define RC_EXECERR   0x0300
#define RC_ENVERR    0x0400
#define RC_SWAPERR   0x0500

/* msspawn.c: Swap method and option flags */

#define USE_EMS      0x01
#define USE_XMS      0x02
#define USE_FILE     0x04
#define EMS_FIRST    0x00
#define XMS_FIRST    0x10
#define HIDE_FILE    0x40
#define NO_PREALLOC  0x100
#define CHECK_NET    0x200

#define USE_ALL      (USE_EMS | USE_XMS | USE_FILE)
