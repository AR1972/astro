/*** 
* varmgr.h - type definitions and data primarily for the Variable Manager
*
*  Copyright <C> 1985, 1986, 1987 Microsoft Corporation
*
*******************************************************************************/

#undef VARMGR_H               /* it will have been defined as 0 in switch.h */
#define VARMGR_H -1           /* to prevent duplicate #include's */
#if !HEAP_H
#include "heap.h"
#endif

/* VARPTR$ descriptor definition */
typedef struct varptr_d {
   ushort   oTyp;          /* BASIC data type */
   long     pValue;        /* Address of the value */
   } varptr_d;

/**====================  Variable Table =====================================
The variable table contains binding information for each variable or 
procedure.  In many cases, the variable table also contains the value.

There are two kinds of variable table. There is one tMV (Module variable table)
per module, and it contains module static variables, all COMMON variables, and 
procedure reference entries.  There is one tPV (Procedure variable table)
per procedure, and it contains the formals, dynamics, and an entry for the
function return value if it is a function.

These tables are constructed at parse time. They may also be constructed by a
special scan pass which is designed to solve instances in the syntax where
program modification causes wide spread binding changes.  This action occurs
when the scanner is called to move the pcode from scan state SS_RUDE.

Since these tables are built at parse time, there may be tPV entries for
variables which are later declared to be functions.

The hash functions for both the tMV and the tPV is a hash on the Name
Table offset (oNam). The hash tables for the tMV and tPV will be of
different size.

It is necessary to detect from the variable table which procedure owns each
variable in the tMV.  This is done using oPrs.  This could be a byte entry
if each procedure were given a unique byte size identifier.  oPrs for the
main level is UNDEFINED.  At table build time, variables must be unique by 
oNam, oPrs, and eType.  There may be a SUB of the same name as a variable.
============================================================================**/
/* sizes of hash tables */
#define CBINITMVHASH 32    /* UNDONE: want a power of 2; is this the ideal? */
#define CBINITPVHASH 16    /* UNDONE: want a power of 2; is this the ideal? */

#define HASH_MV_NAMMASK  0x001E  /* a 32-byte table contains 16 2-byte offsets,
                                    offset) from 0 to 30                      */
#define HASH_PV_NAMMASK  0x000E  /* a 16-byte table contains 8 2-byte offsets,
                                    each to the start of a unique hash chain. */
#define OMVHASH   0  /* [4] module hash table still starts at      */
            /* [4] offset 0 in var table for QB5         */

/* error codes returned by MakeVariable for the parser -
   two sets are defined; the parser error codes are toward the high
   end of the word, while the scanner error messages bits are toward
   the low end of the word.
                                                                           */
#define  PRS_ER_RE   (ushort)0x8100    /* Rude Edit -     bits 8  & 15 set */
#define  PRS_ER_RP   (ushort)0x8200    /* Reparse -       bits 9  & 15 set */

#define  SCN_ER_MASK (uchar)0xFF       /* Other scanner error message codes
                                          are ER_ or MSG_ messages, defined
                                          in qbimsgs.h/.inc                */
/* value part of an array frame variable                                */
typedef struct aFrame {
   ushort   oFrame;
   uchar    cDims;   /* Dimension count - set by varmgr at var creation */
   uchar    filler1;
   } aFrame;

/* value part of an array formal variable                               */
typedef struct aFormal {
   ushort   oFrame;
   uchar    cDims;
   uchar    filler1;
   } aFormal;

/* value part of a static (not frame) array variable                    */
typedef struct aStat {
   ushort   filler0;
   uchar    cDims;   /* Dimension count - set by varmgr at var creation */
   uchar    filler1;
   ad       aryDsc;
   } aStat;

typedef union val {
   short       I2;      /* Integer                                            */
   long        I4;      /* Long Integer                                       */
   FLOAT       R4;      /* single precision                                   */
   DOUBLE      R8;      /* double precision                                   */
   sd          sdStr;   /* Standard variable length string                    */
   comRef      common;  /* used if flags indicate a common (non-array) entry  */
   aCom        aryCom;  /* used if flags indicate a comman array entry        */
   aStat       aryStat; /* static (not frame) array variable                  */
   aFrame      aryFrame;/* dynamic array variable                             */
   aFormal     aryFormal;/* dynamic array variable                            */
   ushort      oFrame;  /* dynamic variable: value is a frame offset (oBP)    */
   ushort      oMV;     /* Module table offset for entry for this variable    */
   ushort      oPrs;    /* offset in prs table for this proc. reference       */
   char        tData;   /* First byte of record; record type indicates size   */
   } val;

/* var structure: an entry in the module variable table
   Note that this is not a normal C struct, because the oTyp might or might 
   not be present, and the value field is of variable length, and a pointer
   to the structure points not at the beginning of the structure but at
   the value field							      */
#define VAR_oMrsTyp    -10 /* used for PUBLIC vars of user-defined type       */
#define VAR_oNamForm	-8 /* used for form/menu variables only 	      */
#define VAR_cbFixed	-8 /* size of fixed-length string/text		      */
#define VAR_oTyp	-8 /* oTyp: present only for user-defined variables   */
#define VAR_oNam	-6 /* Name table offset for variable name	      */
#define VAR_oHashLink	-4 /* link to next entry w/same hash (plus a flag ... */
#define VAR_flags       -2 /* bit flags + an oTyp in low bits                 */
#define VAR_value        0 /* value of this variable                          */

#define VAR_STRUCT_SIZE  6 /* size of a 'normal' (no oTyp field) variable
                              entry minus the value field                     */
typedef ushort var;


/* bit flags constants, for use with varInp.flags2, below                     */
#define  MV_fConstFound    0x01  /* if TRUE on exit, then the oVar returned
                                    is for a CONSTant rather than for a var   */
#define  MV_fONamInOTyp    0x02  /* if TRUE on entry and oTyp isn't a Fixed 
                                    Length String or a predefined type, oTyp
                                    is really an oNam for a CONSTant to define
                                    a Fixed Length String                     */
#define  MV_fDontCreate    0x04  /* if TRUE on entry, don't create a new 
                                    variable if a match is not found (return 
                                    ER_IER instead)                           */
#define  MV_fRecRef        0x08  /* if TRUE on entry, this is a record ref.,
                                    so we don't know the type and a match
                                    MUST be found or error.                   */
                                    /* UNDONE: Nice if QB5 changes to use this
                                       UNDONE: instead of oTyp == UNDEFINED   */

typedef struct varInp {
   ushort   flags;      /* MakeVariable input flags; constants defined below  */
   ushort   exitFlags;  /* UNDONE: remove when MakeVariable in native code    */
   ushort   oNam;       /* input oNam to MakeVariable                         */
   ushort   oTyp;       /* input & output oTyp for MakeVariable               */
   uchar    flags2;     /* various input & output flags - - - these flags
                           are used to communicate special facts to/from
                           MakeVariable rather than to describe the variable
                           to be created                                      */
   uchar    cDimensions;/* count of dimensions if input variable is indexed   */
   ushort   fsLength;	/* length of fixed length string/text		      */
   ushort   oMrsTyp;	/* used only when creating entries in global tVar     */
   } varInp;

EXTERNAL varInp mkVar;

/**====================  Type Table ========================================
The type table is used to manage user defined variable types.

There is one type table per module. This table is interleaved with VAR 
entries in the variable table. The scanner verifies type compatibility
between modules for COMMON variables and procedure parameters.

Type and element entries are maintained in the same table (along with var
entries).

Note that there are two catagories of oTyp's:
- predefined oTyp's (ET_I2, ET_I4, ET_R4, ET_R8, ET_SD, etc.)
      These are all <= ET_MAX
- oTyp's for user-defined types (i.e., records)
      These fall in the range	 ET_MAX < oTypRecord
============================================================================**/
#define CTYPESMAX       240      /* maximum number of user-defined types 
                                    allowed in a module                    */

#define oOFIRST_TYP     CBINITMVHASH + OMVHASH
#define oCTYPES         oOFIRST_TYP + 2

typedef struct typ {
   ushort   oNam;          /* Name table offset of type name               */
   ushort   oTypNext;      /* offset to next type in the chain             */
   ushort   cbData;        /* cb within this type. Used by structure copy  */
   union {
      ushort   oElementFirst; /* Head of element list for this type        */
      ushort   fReferenced;   /* TRUE if exist any ref's to this type      */
      } un;
                              /* NOTE: fReferenced is overlayed onto oElement-
                                 First in order to keep the typ structure
                                 small, and of even byte size. fReferenced is
                                 the high bit, the lower 15 bits being the
                                 offset; thus, prior to using oElementFirst,
                                 the high bit must be masked off           */
   } typ;
   
typedef struct elem {
   ushort   oNam;          /* Name table offset of element name            */
   ushort   oElementNext;  /* oElement of next element within type         */
   ushort   oTyp;          /* Type of this element                         */
   ushort   oVar;          /* Offset of data from data for first sibling
                              (set by scanner, used by executor)           */
   } elem;
   /* CONSIDER: if we decide to go with the language option of allowing
      CONSIDER: arrays as record elements, we'll need to make a union
      CONSIDER: which allows the above, or the above plus an array descriptor 
      CONSIDER: template - - - this template will need to be of the 
      CONSIDER: appropriate size for the number of dimensions, and all of it 
      CONSIDER: should be filled out except for the the long data pointer. 
      
      NOTE: for this reason, make sure we don't come to depend on an
      element entry being of fixed size!                                */

typedef union typEntry {
   /* Module type table entry */
   typ eType;
   elem element;
   } typEntry;

VOID     NEAR VarRudeReset(VOID);
ushort   NEAR MakeVariable(VOID);
VOID     NEAR AdjustMrsVarTable(var *, ushort);
VOID     NEAR AdjustPrsVarTable(var *, ushort, ushort);
ushort   NEAR DefineTyp(ushort);
ushort   NEAR RefTyp(ushort, ushort);
ushort   NEAR DefineElem(ushort, ushort, ushort);
ushort	 NEAR DefineElemFixed(ushort, ushort, ushort, ushort);
ushort   NEAR ONamOElem(VOID);
ushort   NEAR CbTyp(ushort);

/* Non-RELEASE and DEBUG macros, used with vardebug.c */

#define DbSzTyp(oTyp)                           {;}
#define DbDmpVar(oVar)                          {;}
#define DbDmpVarChn(oVarFirst)                  {;}
#define DbDmpTMV()                              {;}
#define DbDmpTPV()                              {;}
#define DbDmpTTyp()                             {;}
#define DbChkoVar(oVar)                         {;}
#define DbChkPVar(pVar)                         {;}
#define DbChkoTyp(oTyp)                         {;}
#define DbChkoTyp(oTyp)                         {;}
#define DbMkVarInp()                            {;}
#define DbMkVarExit(retval,fVarFound,fPVCur)    {;}
#define DbToggleMkVarOut()                      {;}
#define DbTglVarOutAll()                        {;}
#define DbTglVarErrs()                          {;}
#define DbDmpAllInMrs()                         {;}
#define DbDmpAllVars()                          {;}
