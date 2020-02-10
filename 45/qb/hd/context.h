/*** 
*context.h - Defines which apply to the Context Manager
*
*	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
*
*Purpose:
* NOTE: When making changes to this file, be sure to make equivalent   
*       changes to file CONTEXT.INC                                   
*
*******************************************************************************/

#undef CONTEXT_H
#define CONTEXT_H ON       /* remember that this file has been included */

#if !HEAP_H
#include "heap.h"
#endif

/**==================== text descriptor ==================================

Definition of text table descriptor.  Both modules and procedures have
text tables.  All text-offset (otx...) fields in this descriptor are
maintained by the text manager during edit operations, and by the scanner
during pcode expansion/contraction.
For all linked lists through the pcode, (otx...Link), the link is always
a text offset to a pcode's 1st operand, NOT to the 1st byte of the pcode.
This speeds up traversal of the list.

=======================================================================**/

typedef struct txd {
   bdl      bdlText;          /* Owner of far heap entry of pcode */
   ushort   otxLabLink;       /* text offset to linked list of label
                                 definition pcodes (opBolLab etc.)
                                 Offset is to first operand  */
   ushort   otxDefTypeLink;   /* text offset to linked list of opStDefInt etc.
                                 pcodes  */
   ushort   otxTypeLink;      /* text offset to linked list of opStType/
                                 opStEndType pcodes */
   ushort   otxReParseLink;   /* linked list of opReParse opcodes */
   ushort   cLines;           /* Count of lines for scroll bar efficiency. 
                                 Maintained by TextMgr */
   ushort   cLinesIncl;       /* Count of lines from $INCLUDE files */
   ushort   lnCursor;         /* remember cursor line when not visible win */
   char     scanState;        /* Scan state of pcode */
                              /* see scanner.h for values for scanState */
                              /* SS_Executable .. SS_Rude */
   char     flags;            /* FTX_xxx - defined below */
   } txd;

/* txd.flags definitions: */
#define FTX_mrs         0x01     /* TRUE if text table is mrs, not prs */
#define FTX_asChg               0x02            /* TRUE if PreScanAsChg needs to be called
						   before we scan this text table again */
/* The following FTX_Tmp... flags are used temporarily within the scope of
 * SaveDeclares().  If another function needs to make temporary use of a flag,
 * it can use these bits as well, as long as it assumes they contain garbage
 * on entry and exit.
 */
#define FTX_TmpDecl     0x04            /* Used by SaveDeclares in text mgr */
#define FTX_TmpRef      0x08            /* Used by SaveDeclares in text mgr */



/**==================== Procedure Register Set (PRS) ======================
See equivalent comment block in context.inc for complete information.
See comment block just before PrsMake for definition of Prs State Transitions.
=========================================================================**/
/* WARNING: the context manager initialization code depends on the prs, mrs,
            and grs structures being setup such that all items to be zero-
            filled (on struct init.) are at the beginning of the structure.

            Other context mgr code depends on both the prs and the mrs having
            the ogNam field in the same place relative to the start of the
				structure.																			*/

/* bit flag constants, for use with prs.flags, below  */
#define FP_STATIC    0x80     /* TRUE if definition line terminated by STATIC */
#define FP_CDECL     0x40     /* TRUE if external procedure expects 'C' calling
                                 conventions - set by parser                  */
#define FP_DEFINED   0x20     /* Indicates prs is defined by SUB/FUNCTION/DEF
                                 Set by ContextMgr, reset by TextMgr          */
#define FP_ENDPROC   0x10     /* set/reset and tested by TextMgr              */
#define FP_DECLARED  0x08     /* This procedure has been DECLARED             */
#define FP_DEFSCANNED 0x04    /* Definition is in execute state               */

/* bit flag constants, for use with prs.flagss */
#define FP_LOCAL     0x80     /* TRUE if LOCAL procedure		      */
#define FP_AUTO      0x40
#define FP_ExeTree  0x20     /* TRUE if the call tree starting at this
				 proc is all at SS_EXECUTE state */


typedef struct prs {
   ushort   cbFrameVars;      /* Cb of variables in the frame.                */
   ushort	cbFrameTemp;		/* Temp space needed                            */
   ushort  ogNam;					/* [1] offset into global name tbl for prs name */
        /* WARNING: Binary SAVE/LOAD assumes that procType, oType, and flags are
                    contiguous, starting with procType (see binsav.asm)       */
   char     procType;         /* PT_... enumerated below */
   char     oType;            /* DEF/FUNCTION type.  Undefined for SUBs       */
   char     flags;            /* bit flags; constants defined above           */
   char     cwParams;         /* Count of words in stack for parameters */

/* WARNING: Before this point data is initialized to zero.    
            After this point, data is initialized to UNDEFINED.               */

   ushort   oVarHash;         /* offset into module's variable table for this
                                 procedure's local-variable hash table.  Set by
                                 VarMgr.  Set to UNDEFINED initially and by
                                 ModuleRudeEdit                               */
        /* WARNING: Binary SAVE/LOAD saves everything from cbAlias to here, and
                    assumes it's safe to not save or load the items below, 
						  except for the txt (see binsav.asm for complete details)  */

   /* The next 3 fields identify where the current definition of the procedure
      is.  This can refer to a SUB/FUNCTION/DEF, in which case fDefined=TRUE.
      It can refer to a DECLARE SUB/FUNCTION/DEF, in which case fDefined=FALSE.
      For SUBs, it can refer to a CALL or implied CALL reference, in which
      case fDefined=FALSE.  For DEF FN entries, we can have two enties with
      same name and type, as long as the oMrs field is different.
      These fields are used by scanner parm type checking.  If a proc is
      declared or referenced before defined, this refers to 1st reference
      and fDefined is FALSE.  When definition is seen, fDefined is set
      TRUE and these 3 fields are set to real definition. */
   ushort   oMrs;             /* MODULE of current definition - for DEF FNs,
                                 this never changes and it is used in the
                                 prs name search */
   ushort   oRsDef;           /* module/procedure of current definition.
                                 For DEF FNs, this never changes and it is
                                 used in the prs name search */
   ushort   otxDef;           /* offset into text table to current definition.
                                 If a SUB is referenced, but not DECLAREd or
                                 defined, a Prs entry is created and otx is
                                 set to UNDEFINED.  This is only true for SUBs
                                 because BASCOM allows external SUBs to be
                                 called with no DECLARE. */
   ushort  oPrsNext;	      /* [4] offset to next prs in the Rs table       */
   txd      txd;              /* Text Descriptor for pcode for procedure      */
   } prs;

#define M_PT_OTYPE   0x07     /* mask to access oType in prs.oType field (since
				 flag bits are now stored in high nibble      */

/* enumerations for procType */
#define PT_SUB       1
#define PT_FUNCTION  2
#define PT_DEFFN     3
#define PT_NOT_PROC  4	      /* just says 'not any of the above constants'.
				 used as parm internal to context mgr.	This
				 value is Never stored in prs.procType field */


/**==================== Module Register Set ==================================
The Module Register Set contains all entries and owners of tables containing
information about a particular module.

While a module is loaded into mrsCur, its entry in the global Rs table should 
not be referenced or updated.
============================================================================**/
/* WARNING: the context manager initialization code depends on the prs, mrs,
            and grs structures being setup such that all items to be zero-
            filled (on struct init.) are at the beginning of the structure. */

#define OGNAM_UNNAMED		0		/* [2] ogNam for unnamed mrs			*/
#define OGNAM_GMRS			1		/* [1] an invalid ogNam to be used as
							   [1] the ogNam of the global mrs */
#define OGNAM_CLIPBOARD		2		/* [2] ogNam for clipboard mrs		*/
#define OGNAM_IMMEDIATE		3		/* [2] ogNam for Immediate mrs		*/
#define OGNAM_PSEUDO_MAX	3		/* [2] max. pseudo ogNam value		*/

#define OMRS_GLOBAL		0		/* [13] */





/***************************************************
** bit flag constants, for use with mrs.flags, below
** Unused bits: 10, 20, 40, 80
*/
#define FM_OptionBase1  0x01  /* Set if OPTION BASE is 1 (not 0)              */
#define FM_VARNEW       0x02  /* if set, tells CLEAR code to deallocate
                               * $static arrays, not just zero-fill them      */
#define FM_asChg        0x04  /* set when AS x is inserted/deleted in module  */
#define FM_TEMPORARY    0x08  /* only set during LOAD - reset at end of LOAD  */
#define FM_AllSsRude    0x10  /* set if all text tables in module are in
                                 SS_RUDE scan state                           */
#define FM_LinksOk 	0x20  /* [10] */
#define FM_MustVisit	0x40  /* [10] */
#define FM_Visited	0x80  /* [10] */
/* NOTE: all flags except FM_asChg get set to 0 by BinaryLoad().
 *       When adding flags, make sure this is desired
 */

/****************************************************
** bit flag constants, for use with mrs.flags2, below
*/
#define FM2_Modified    0x01  /* set by TextMgr when text has changed         */
#define FM2_NewFile     0x02  /* When this mrs is saved, give a warning if
                               * another file by this name already exists */
#define FM2_Include     0x04  /* This mrs was created by View/Include menu.
                               * When saved, we need to do a ReInclude */
#define FM2_NoPcode     0x08  /* set if mrs has no pcode, just ASCII text  */
#define FM2_File        0x10  /* Set if this mrs has a FILE associated with it.
                               * It must be saved if modified.  Not set for
                               * Command-Window's text and Scrap's text */
#define FM2_AsciiLoaded 0x20  /* set if program was loaded from an ASCII file */
#define FM2_ReInclude	0x40   /* Set when a file is modified.
                                  Reset by TxtReInclude */
#define FM2_EntabSource 0x80  /* Set when ascii loading module if source
                                 file contained leading tabs.  When ascii
                                 saving, we will entab all leading spaces
                                 for this module */
/* NOTE: all flags except FM2_EntabSource get set to 0 by BinaryLoad().
 *       When adding flags, make sure this is desired
 */

/****************************************************
** bit flag constants, for use with mrs.flags3, below
*/
#define FM3_NotFound	0x01  /* Set while loading .MAK file if this module
                               * was not found */
#define FM3_Translated	0x04  //[24] TRUE if we were binary-translated
/* NOTE: flags3 is NOT saved by BinarySave/Load */

typedef struct mrs {
   ushort   cbFrameVars;   /* Cb of variables in the frame.                   */
   ushort   cbFrameTemp;   /* Temp space needed                               */
   ushort   ogNam;         /* [1] source file path name (global name table
                              [1] offset, or 0 in the case of an untitled
			      [1] module or OGNAM_GMRS for the global mrs     */
   bd       bdVar;         /* Table of all module's variables (including
                              variables which are local to procedures         */
   char     flags;         /* bit flags; FM_ constants defined above          */
   char     flags2;        /* bit flags; FM2_ constants defined above         */
   char     flags3;        /* bit flags; FM3_ constants defined above         */
   char     dummy1;        /* Extra byte to word align                        */

/* WARNING: Before this point data is initialized to zero.    
 *          After this point, data is initialized to UNDEFINED.               */

   ushort  oMrsNext;	   /* [4] offset to next mrs in the Rs table	      */
   ushort   otxHandler;    /* Text offset to error handler                    */
   bdl      bdlNam;        /* Module's Name table                             */
   txd      txd;           /* Text Descriptor for pcode for module            */

/* WARNING: Binary SAVE/LOAD depend on otxDefFnLink, oPastLastVar, and the
	    'data' struct being contiguous and in the following order	      */

   ushort   otxDefFnLink;  /* textmgr maintains this linked list of DEF FN's  */
   ushort   oPastLastVar;  /* offset to first byte past last variable in the
                              module variable table; used to ensure dead
                              direct mode FOR entries don't waste tVar space  */
   struct {                /* Head of data chain */
      ushort   otxFirst;   /* opTxt operand for 1st data statement            */
      ushort   otxCur;     /* opTxt operand for current data statement        */
      ushort   oLineCur;   /* byte offset for next READ                       */
      } data;
   } mrs;

/**==================== Global Register Set =================================
The global register set owns heap entries which define the current context.

Only one module table entry or procedure table entry may be active at any
one time.  These tables are managed by swapping a new mrs into mrsCur or
swapping a new procedure into prsCur.

While a module or procedure is loaded into mrsCur or prsCur, its entry
in the global Rs table should not be referenced or updated.

=======================================================================**/
/* WARNING: the context manager initialization code depends on the prs, mrs,
            and grs structures being setup such that all items to be zero-
            filled (on struct init.) are at the beginning of the structure    */

typedef struct grsType {
   char     fScan;         /* TRUE if all modules and procedures are scanned */
   char     fDirect;       /* TRUE if we're executing instructions out of
                              direct-mode text buffer bdlDirect.
                              This is set by UserInterface(), procedure exit,
			      RETURN.  Reset by opStDmGoto, GOSUB, CALL etc.  */
	bd  bdRs;	   /* [4] */
   bd       bdtComBlk;     /* COMMON block table owner */
   char     flags;         /* These are general flags which are independently
                                                              set and reset */
   char     filler;        /* unused */
/* WARNING: Before this point grs struct is initialized to zero.
            After this point, it's initialized to UNDEFINED.                  */

   ushort   oMrsMain;      /* Offset within mrs table for main module. Can be
                              modified by UI. Note that if mrs with this offset
                              is discarded, oMrsMain is set to UNDEFINED and
                              there is NO main module - - - RunInit checks for
                              this condition.                                 */

   ushort   oRsCur;        /* Redundant copy of oMrsCur and oPrsCur.
                              If high bit is set, a procedure is active and
                                 low 15 bits = oPrsCur
                              else
				 no procedure is active and oRsCur == oMrsCur */
   ushort   pMrsCur;
   ushort   pRsCur;
   ushort   offsetTxdSeg;  /* offset into bdtMrs or bdtPrs to the segment
                              address for the current txd                     */
   ushort   oMrsCur;       /* Offset within mrs table for module currently
                              loaded into mrsCur */
   ushort   oPrsCur;       /* Offset within prs table for procedure currently
                              loaded into prsCur */
   ushort   otxCur;        /* Offset within current text table to next
                              instruction to be executed.  This is only valid
                              while we're in UserInterface().  While executing,
                              this value is always maintained in SI reg */
   ushort   oRsCONT;       /* register set where we are to CONTinue */
   ushort   otxCONT;       /* text offset where we are to CONTinue.  Set by
                              UserInterface(), CantCONT().  Used by ExStCONT.
                              We can't continue if grs.otxCONT == UNDEFINED */
   ushort   oRsContTxtTbl; /* Identical to oRsCONT, except when oRsCONT is
                              for a DEF FN, this is the oRs of the module
                              containing the DEF FN.  It always identifies
                              the text table where containing next stmt to
                              be executed */
   bdl      bdlDirect;     /* Direct mode pcode buffer owner */
   char     flagsDir;      /* These flags get reset every time we begin
                              begin executing pcode, and when a runtime error
                              occurs.  They are defined below (FDIR_xxx) */
   } grsType; 

#define FDIR_cleared  0x01 /* TRUE when ClearWorld has been called.  Reset
                              at end of CLEAR statement.  It is strictly a
                              speed optimization */
#define FDIR_new      0x02  /* Set at start of NewStmt and reset at end of
                               NewStmt.  Speed optimization for Text Manager. */
/* bits for general grs flags */
#define FG_WatchActive  0x01  /* set if watch pcode is active */
#define FG_RetDir       0x02  /* 1 return address to direct-mode buffer
                               * exists on the stack.  Entry of direct-mode
                               * stmts when this is true causes FG_RetDirBad
                               * to be set.                                   */
#define FG_AllSsExecute 0x04H /* set if all text tables are in SS_EXECUTE */
#define FG_OtxInDir	0x08H /* set if dir mode stmt contains an oxt ref */

/* bits for mask passed to ForEach[CP]: */
#define FE_PcodeMrs     0x01  /* TRUE if mrs's containing pcode are to
                                 be visited */
#define FE_TextMrs      0x02  /* TRUE if FM_TEXT and FM_TEXTFILE mrs's
                                 are to be visited (i.e. command window's
                                 mrs, docuemnt files) */
#define FE_CallMrs      0x04  /* TRUE if pFunc is to be called for mrs's
                                 text table BEFORE it is called for prs's
                                 text tables */
#define FE_PcodePrs     0x08  /* TRUE if prs's with text tables (SUBs/
                                 FUNCTIONs) are to be visited */
#define FE_NoPcodePrs   0x10  /* TRUE if DEF FN and DECLARE prs's are
                                 to be visited */
#define FE_FarCall      0x20  /* TRUE if function to be called is FAR */
#define FE_SaveRs       0x40  /* TRUE if ForEach is to restore caller's
                                 oRsCur on exit */
#define FE_CallMrsAfter 0x80  /* TRUE if pFunc is to be called for mrs's
                                 text table AFTER it is called for prs's
                                 text tables. */



/**========================= Binary SAVE/LOAD Constants ========================
/* these three bit fields are OR'd together as inputs to Binary SAVE; the
   first two are also used in the header of a binary SAVEd file:              */
#define BINSAV_SingleModule       0x01
#define BINSAV_MultipleModule     0x02
#define BINSAV_fProtected         0x0100

/**========================= EXTERNAL VARIABLES ===============================

context.c includes this file after defining EXTERNAL as empty, causing these
variables to actually be defined.

============================================================================**/
EXTERNAL grsType  PLM grs;    /* global register set */
EXTERNAL mrs      PLM mrsCur; /* module register set for active module       */
EXTERNAL prs      PLM prsCur; /* procedure register set for active procedure */
EXTERNAL txd      PLM txdCur; /* current text table (from prs or mrs)        */
EXTERNAL ushort   PLM pGosubLast; /* Pointer to last stack GOSUB frame	     */
EXTERNAL ushort   PLM fNonQBI_Active; /* set non-zero whenever non-QBI code
					 is activated (non-0 ==> old BP)   */
EXTERNAL ushort   PLM bcurlevel_QBI;  /* value of b$curlevel for most recent
					 QBI frame when fNonQBI_Active != 0 */

#ifndef CONTEXT_NOFUNCS
VOID     FAR   InitContext(VOID);
ushort	FAR	MrsFind(ushort);
ushort	FAR	PrsFind(ushort);
VOID	 FAR   RsActivate(ushort);		/* EB_API */
VOID     NEAR  RsActivateCP(ushort);
ushort   FAR   MrsMake(ushort, ushort);
VOID     NEAR  MrsDeActivate(VOID);
VOID     FAR   MrsActivate(ushort);
boolean  FAR   MrsDiscard(VOID);
boolean  FAR   ForEachMrs(boolean (far *)());
ushort   NEAR  PrsRef(ushort, uchar, ushort);
ushort   FAR   PrsMake(ushort, char);
VOID     NEAR  PrsDeActivate(VOID);
VOID     FAR   PrsDeActivateFar(VOID);
VOID     NEAR  PrsActivateCP(ushort);
VOID     FAR   PrsActivate(ushort);
boolean  FAR   ForEachPrsInMrs(boolean (far *)());
ushort   FAR   NextPrsInMrs(VOID);
ushort   FAR   NextTextPrsInMrs(VOID);
ushort	 FAR   NextMrsFile_All(VOID);
ushort   FAR   NextMrsFile(VOID);
boolean  FAR   PrsDiscard(VOID);
VOID     FAR   PrsFree(VOID);
ushort   NEAR  FieldsOfPrs(ushort);
ushort   FAR   FieldsOfPrsFar(ushort);
ushort   NEAR  SetPrsField(ushort, ushort, ushort);
VOID     FAR   NewStmt(VOID);
VOID     FAR   RunInit(VOID);
VOID     FAR   ContReinit(VOID);
VOID     FAR   ContReinitStat(VOID);
boolean  FAR   ClearStmt(ushort, ushort);
VOID     FAR   CantCont(VOID);
VOID     NEAR  VarDealloc(VOID);
boolean  FAR   FreeAllUnrefdPrs(VOID);
VOID     FAR   AdjustITable(char *, ushort, uchar);
boolean  FAR   ForEach(uchar, boolean (far *)());
ushort	 FAR   AlphaBuildORs(void);	//[23]
ushort   FAR   oRsOfAlpha(ushort);
ushort   FAR   NextAlphaPrs(void);
VOID     FAR   AlphaORsFree(void);
boolean  FAR   FindORsFrame(void);
ushort FAR OTypeOfTypeCharFar(char);	//[22]



/**========================= Non-RELEASE Macros ===============================

Non-RELEASE and DEBUG macros, used with context.c

============================================================================**/

#define DbChkGrs()            {;}
#define DbChkMrsCur()         {;}
#define DbChkPrsCur()         {;}
#define DbChkoMrs(oMrs)       {;}
#define DbChkoPrs(oPrs)       {;}
#define DbOMrsCur()           {;}
#define DbOPrsCur()           {;}

#endif /* ifndef CONTEXT_NOFUNCS */
