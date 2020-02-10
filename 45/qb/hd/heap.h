/*** 
*heap.h - Defines which apply to the QBI Heap Manager
*
*  Copyright <C> 1986, 1987, 1988 Microsoft Corporation
*
*******************************************************************************/
/* NOTE: When making changes to this file, be sure to make equivalent   */
/*       changes to file HEAP.INC                                       */

#undef HEAP_H
#define HEAP_H ON          /* to prevent duplicate #include's */

/**==================== bd ==========================================

buffer descriptor structure.
   Notice that this is a superset of a string-descriptor
   structure, which means that some functions which expect
   string descriptors can take buffer descriptors as input.
Examples of  (bd) structures  include the  prs table, mrs
table and value tables.

=======================================================================**/

typedef struct bd {
   ushort cbLogical;    /* logical size of content part of
                           heap entry.  This is the size as requested by
                           the last call to BdAlloc or BdRealloc.  There
                           may be additional padding space at the end. */

   char *pb;            /* points to 1st byte of content part
                           of heap entry.  This pointer is updated when
                           heap entry is moved. */
   ushort cbPhysical;   /* actual size of heap entry, including empty space
                           at the end.  Always >= cbLogical */

   } bd;


#define GETSEG(seg)	seg	/* [6] */


/**==================== bdp ==========================================

buffer descriptor with pointer structure
Notice that this is a superset of a (bd) structure,
which means that some functions which expect (bd)'s as
input can take (bdp)'s as input.
Examples of (bdp) structures include  the buffer  used to pass the parser a
source line and the  buffer the parser returns pcode in.

=======================================================================**/

typedef struct bdp {
   ushort cbLogical;    /* logical size of content part of
                           heap entry.  This is the size as requested by
                           the last call to BdAlloc or BdRealloc.  There
                           may be additional padding space at the end. */
   char *pb;            /* points to 1st byte of content part
                           of heap entry.  This pointer is updated when
                           heap entry is moved. */
   ushort cbPhysical;   /* actual size of heap entry, including empty
                           space at the end.  Always >= cbLogical */

   char *pbCur;         /* points to current byte within entry
                           This pointer is updated when heap entry is moved. */
   } bdp;



/**==================== bdl ==========================================

Large far buffer descriptor structure.
Examples of bdl structures include text and name tables.

=======================================================================**/

typedef struct bdl {
   ushort cbLogical;    /* logical size of content part of
                           heap entry.  This is the size as requested
                           by the last call to BdlAlloc() or BdlRealloc().    */
   ushort seg;          /* segment address of buffer, maintained by the far
                           heap manager. seg:0 is the far pointer to the
			   start of the buffer				      */
   ushort status;       /* Indicates if the bdl is currently an owner or not.
                           This field is actually used for something else by 
                           the FH manager, and so is read-only while the bdl
			   is an 'owner'.				      */
   ushort cPhysical;    /* physical size of heap entry. Under DOS 3, this is a
                           paragraph count, maintained by the FH manager; under
                           DOS 5, it's a byte count, maintained by bdmgr.asm  */
   } bdl;

#define  NOT_OWNER   UNDEFINED   /* used to test bdl.status field          */

/**===================================================================

Constants and structures related to entries in the Interpreter Heap

=======================================================================**/

/* Types of Interpreter Heap entries */
/* NOTE: see note in heap.inc */
#define IT_M_INTERP        (char)0x80  /* mask - if this bit is set in a heap 
                                          entry type byte, it's an interpreter-
                                          specific entry                      */
#define IT_NO_OWNERS       (char)0x80  /* heap entry which contains no owners.
                                          Entry is owned by a bd structure. 
                                          These can be moved without examining 
                                          content                             */
#define IT_NO_OWNERS_BDP   (char)0x90  /* same as IT_NO_OWNERS, but it is owned
                                          by a bdp structure instead of a bd, 
                                          so when moved, bdp.pbCur must be 
                                          updated                             */
#define IT_MRS             (char)0xA0  /* module register set table           */
#define IT_PRS             (char)0xB0  /* procedure register set table        */
#define IT_COMMON_BLOCK    (char)0xC0  /* table of owners of common value 
                                          tables                              */
#define IT_COMMON_VALUE    (char)0xE0  /* packed table of common values       */
#define IT_VAR             (char)0xF0  /* module variable table               */
#define IT_STRARYDESC	   (char)0x02  /* same constant used in common runtime*/


/* Forward References to Functions */

/* interpreter heap forward references */
boolean BdAlloc(bd *, ushort, char);
boolean BdAllocVar(bd *, ushort, char);
VOID BdFree(bd *);
boolean BdGrow(bd *, ushort);
boolean BdGrowVar(bd *, ushort);
boolean BdRealloc(bd *, ushort);
boolean BdCheckFree(bd *, ushort);
boolean BdAppend(bd *, char *, ushort);
ushort BdShiftRight(bd *, ushort, ushort);
VOID BdShiftLeft(bd *, ushort, ushort);
ushort BdChgContents(bd *, sd *);
VOID BdChgOwner(bd *, bd *);
VOID BdCompressAll(VOID);
ushort FAR GrabSpace(VOID);
VOID FAR ReleaseSpace(VOID);

/* far large heap forward references */
boolean BdlAlloc(bdl *, ushort);
VOID BdlFree(bdl *);
VOID BdlChgOwner(bdl *, bdl *);
boolean BdlCheckFree(bdl *, ushort);
boolean BdlRealloc(bdl *, ushort);
VOID BdlCopyTo(bdl *, ushort, char *, ushort);
VOID BdlCopyFrom(bdl *, ushort, char *, ushort);
VOID BdlTrim(bdl *);



#define DbOAbd(pbdArg)        {;}
#define DbOHbd(pbdArg)        {;}
#define DbOAbdN(pbdArg)       {;}
#define DbOHbdN(pbdArg)       {;}
#define DbDmpLHHeap(fAppend)  {;}
#define DbChkBdOwner(pbd)     {;}
#define DbChkBdlOwner(pbd)    {;}
#define DbChkBdNotOwner(pbd)  {;}
#define DbTglLHShift(fShift)  {;}
#define DbChkHeaps()          {;}
