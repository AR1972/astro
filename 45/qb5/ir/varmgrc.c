/*** 
*varmgrc.c - Variable Manager for the BASIC 5.0 Interpreter
*
*  Copyright <C> 1985, Microsoft Corporation
*
*Purpose:
*  Includes code for:
*     - creating and searching for variables
*     - creating and searching for user-defined types
*  Includes a description of namespaces in BASIC.
*
**************************************************************************/


/*
The matrix given below is a representation of the namespaces in BASIC.
Each class of names is listed on both the vertical and horizontal axis';
X's in a square indicate that the two classes share a namespace (and
could thus have name conflicts), while blanks in a square indicates that
the two classes have separate namespaces.

An astrisk ('*') (shown on vertical axis only) indicates that
a type character included at the end of the name counts as
part of the name for determining uniqueness.

NOTE: This is a copy of the identical table in basic40.doc. If there's
      any conflict between this table and that one, the one in basic40.doc
      takes precedence.


           COM  LAB  DEF  SUB  FUNC Type Elem Sclr typd Arry typd Const
           Blk        Fn                           Sclr      Arry
          |    |    |    |    |    |    |    |    |    |    |    |    |
        ---------------------------------------------------------------
COMMON Blk| XX |    |    |    |    |    |    |    |    |    |    |    |
          | XX |    |    |    |    |    |    |    |    |    |    |    |
        ---------------------------------------------------------------
Label     |    | XX |    |    |    |    |    |    |    |    |    |    |
          |    | XX |    |    |    |    |    |    |    |    |    |    |
        ---------------------------------------------------------------
DEF Fn*   |    |    | XX | XX | XX |    |    | XX | XX | XX | XX | XX |
          |    |    | XX | XX | XX |    |    | XX | XX | XX | XX | XX |
        ---------------------------------------------------------------
SUB       |    |    | XX | XX | XX |    |    | XX | XX | XX | XX | XX |
          |    |    | XX | XX | XX |    |    | XX | XX | XX | XX | XX |
        ---------------------------------------------------------------
FUNCTION  |    |    | XX | XX | XX |    |    | XX | XX | XX | XX | XX |
          |    |    | XX | XX | XX |    |    | XX | XX | XX | XX | XX |
        ---------------------------------------------------------------
Type      |    |    |    |    |    | XX |    |    |    |    |    |    |
          |    |    |    |    |    | XX |    |    |    |    |    |    |
        ---------------------------------------------------------------
Element   |    |    |    |    |    |    | XX |    |    |    |    |    |
          |    |    |    |    |    |    | XX |    |    |    |    |    |
        ---------------------------------------------------------------
Scaler*   |    |    | XX | XX | XX |    |    | XX | XX |    |    | XX |
          |    |    | XX | XX | XX |    |    | XX | XX |    |    | XX |
        ---------------------------------------------------------------
typed     |    |    | XX | XX | XX |    |    | XX | XX |    |    | XX |
  Scaler  |    |    | XX | XX | XX |    |    | XX | XX |    |    | XX |
        ---------------------------------------------------------------
Array*    |    |    | XX | XX | XX |    |    |    |    | XX | XX |    |
          |    |    | XX | XX | XX |    |    |    |    | XX | XX |    |
        ---------------------------------------------------------------
typed     |    |    | XX | XX | XX |    |    |    |    | XX | XX |    |
  Array   |    |    | XX | XX | XX |    |    |    |    | XX | XX |    |
        ---------------------------------------------------------------
Const     |    |    | XX | XX | XX |    |    | XX | XX |    |    | XX |
          |    |    | XX | XX | XX |    |    | XX | XX |    |    | XX |
        ---------------------------------------------------------------
*/

#include "version.h"
#if !HEAP_H
#include "heap.h"
#endif
#if !CONINT_H
#include "conint.h"
#endif
#if !CONTEXT_H
#include "context.h"
#endif
#if !VARIABLE_H
#include "variable.h"
#endif
#if !QBIMSGS_H
#include "qbimsgs.h"
#endif
#if !NAMES_H
#include "names.h"
#endif
#if !UTIL_H
#include "util.h"
#endif
#if !EXECUTOR_H
#include "executor.h"
#endif
#if !PARSER_H
#include "parser.h"
#endif
#if !TXTMGR_H
#include "txtmgr.h"
#endif
#if !SCANNER_H
#include "scanner.h"
#endif

/* forward reference of functions used only locally */
ushort   NEAR     CreateVar(ushort, ushort);
boolean  FAR      ModSharedChk(VOID);
void     NEAR     ReDirect(VOID);

ushort   NEAR     StdSearch(VOID);
STATICF(VOID) AdjustStatChain(var *, ushort, ushort);
STATICF(ushort) GrowBdVar(ushort);
ushort NEAR FuncSearch(void);
ushort NEAR GetDefaultType(char);

extern   ushort oNamOfPrsCur;    /* set in ssrude.asm                         */

VOID     FAR      B_ISdUpd(sd *, ushort);
                                 /* runtime entry point to update moving sd's */
VOID     FAR      B_IAdUpd(ad *, ushort);
                                 /* runtime entry point to update moving ad's */

/* varmgr-specific data */
ushort  vm_oVarCur;                 /* offset to current variable table entry */
var *vm_pVarCur;                    /* only guaranteed to be setup immediately
                                       after a search or variable creation    */
ushort vm_oVarTmp;                  /* temporary var entry offset             */
boolean vm_fVarFound;               /* return value from var search routines.
                                       note that, if vm_fVarFound is TRUE, the 
                                       offset to the found var will be in 
                                       vm_oVarCur (a static, also used in 
                                       variable creation).                    */
ushort vm_fPVCur;                   /* currently searching tPV if TRUE        */
STATICV uchar nm_mask;              /* mask to apply to tNam flag for variable*/
STATICV ushort errVal;              /* error return - used for those cases
                                       which must return some other value
                                       (such as routines invoked via
                                        ForEachPrsInMrs ...)                  */
STATICV boolean fConvtdToFun;       /* used by FuncSearch to communicate the
                                       fact that it converted an existing
                                       entry to be a FUNCTION entry.          */

/*##############################################################################
#                                                                              #
#                        Variable Storage Scheme                               #
#                                                                              #
##############################################################################*/
/* 
Variables are stored as follows:

      There is one physical variable table per module, and one logical table
      per procedure and module. This is accomplished by having one hash table
      per logical table, all inter-woven in the physical table.

      Each hash table is initialized to zeros, and contains a fixed number
      of fields, each of which is an offset into the physical table to the
      first variable in a chain of variables that hash to the same hash table
      entry.  The hashing function is based on the name table offset (oNam)
      for the variable.   Note that module hash tables are larger than proc.
      hash tables, in anticipation of more variables per module than per
      procedure (i.e., to help give us reasonable search speed for module
      variables).  As of this writing (Aug. '87), there are 16 hash chains 
      per module hash table, and 8 per procedure hash table.

      The links in each hash chain are (physical) variable table offsets to
      the next variable entry in the chain.  The end-of-chain marker is
      EITHER a 0 or a 1 - - - this is because we also store a flag in the
      low bit of the hash link (we use every bit we can get ...).

      Since variables cannot be dynamically removed and/or reused (i.e.,
      one can unlink an entry, but then that space is dead), variable
      entries and hash tables go into the physical table in whatever order 
      they are encountered.

Variable Entries:

      A variable entry looks like one of the below:

                  If oTyp is User-              If oTyp is I2, I4,
                  Defined or a Fixed            R4, R8, or Sd. oTyp
                  Length String                 is stored in low 3
                                                bits of flags field
                  +----------------+      
		  |   oTyp/cbFS    |
                  +----------------+            +----------------+
                  |      oNam      |            |      oNam      |
                  +----------------+            +----------------+ 
                  |   oHashLink    |            |   oHashLink    |
                  +----------------+            +----------------+
                  |     flags      |            |     flags      |
                  +----------------+            +----------------+
          pVar--->+     value      |    pVar--->|     value      |
                  |                |            |                |
                  +----------------+            +----------------+


      The value field is of variable length, depending on what the variable
      represents. The flag bits allow determination of size and use of the
      value field for any given variable.

      All variable entries are of even size, and must fall on even byte
      boundaries, for 2 reasons:
         (1) The runtime requires SD's to fall on even byte boundaries
         (2) We store a flag bit in the low byte of the oHashLink field,
               so each oVar must be even.
                                                                              */


/*##############################################################################
#                                                                              #
#                        Variable Creation                                     #
#                                                                              #
##############################################################################*/
/***
*GrowBdVar(cbGrowVar) - grow variable table
*Purpose:
*  Given an amount by which we wish to grow the active variable table,
*  grow it if possible, returning FALSE if successful or an error code if not.
*
*  In the event that grs.otxCONT != UNDEFINED, don't call BdGrowVar; instead,
*  succeed if sufficient space exists between cbLogical and cbPhysical, fail
*  otherwise, returning ER_CN.
*Entry:
*  cbGrowVar - number of bytes we wish to grow mrsCur.bdVar by.
*Exit:
*  FALSE if successful, in which case mrsCur.bdVar.cbLogical is increased by
*     cbGrowVar
*  Otherwise, returns an error code for use by CreateVar, below.
*******************************************************************************/
STATICF(ushort) GrowBdVar(cbGrowVar)
ushort cbGrowVar;
   {
   /* [13] ensure that oVar < 0x8000					      */
   if ((mrsCur.bdVar.cbLogical + VAR_STRUCT_SIZE + 2) > 0x7FFF) /* [13]       */
      return (PRS_ER_RE | ER_OM);		   /* [13] out of memory      */

   if (grs.otxCONT == UNDEFINED) {                 /* user can't CONTinue     */
      if (!BdGrowVar(&mrsCur.bdVar, cbGrowVar))
         return (PRS_ER_RE | ER_OM);               /* out of memory           */
      }
   else {            
      /* user is able to CONTinue; fail unless space already exists in table  */
      REG1 ushort temp = mrsCur.bdVar.cbLogical + cbGrowVar;
      if (temp > mrsCur.bdVar.cbPhysical) {	   /* would have to actually  */
						   /* grow table	      */
         DbAssert(!(mkVar.flags2 & MV_fTrashTable))
         return (0x8000 | ER_CN);
         }
      else 
         mrsCur.bdVar.cbLogical = temp;            /* success.                */
      }
   return(FALSE); 
   }

/***
*CreateVar(oPVHash, varFlags) - create a new variable
*Purpose:
*  Create a new variable in the mrsCur.bdVar. If 'oPVHASH' is UNDEFINED, 
*  create it in the tMV, else, use the offset to the tPV hash table.
*  
*Entry:
*  oPVHash - UNDEFINED if we're to create var in the tMV, or an offset
*              into mrsCur.bdVar to the hash table for the tPV.
*  varFlags  - Initial settings for the flags word in the new var entry.
*              FVFNNAME, FVFUN, FVCOMMON, FVFORMAL, FVDECLDVAR, FVSHARED, 
*              FVCONST, and FVSTATIC are to be set correctly; FVARRAY & 
*              FVINDEXED will frequently be correct, but may be changed 
*              based on other flags settings; FVVALUESTORED is to
*              be pathologically set to TRUE on input, while 
*              FVEVEREXPLICIT, and FVREDIRECT are to be set FALSE.
*  mkVar - global structure containing MakeVariable inputs for oNam,
*              oTyp, and a(nother) flags word.
*
*  NOTE: All variables are created on even-byte boundaries. This
*        guarantees, in turn, that the value fields of all variables will
*        start on even byte boundaries. We ensure this primarily by the
*        structure definitions in variable.h/.inc.  The primary reason for
*        this is that the shared runtime assumes (and requires) that all
*        string descriptors be on even byte boundaries.
*
*Exit:
*  return value is an error code; either zero is returned (no error),
*  or the same error code as is used by MakeVariable is returned (see
*  MakeVariable, below).
*
*  Also sets module static vm_oVarCur to the offset into mrsCur.bdVar
*  to the new entry, and vm_pVarCur to point to the entry.
*
*Exceptions:
*  none.
*
*******************************************************************************/
ushort NEAR CreateVar(oPVHash, varFlags)
ushort oPVHash;
ushort varFlags;
   {
   var *pVar;
   ushort inFlags = mkVar.flags;	     /* put MakeVariable callers flags
						word in a register	      */
   ushort cbValue = 0;
   ushort cbOTyp;
   ushort *pHash;
   uchar  nmsp_type;
   ushort temp;
   ushort entryFlags;			     /* [11] use this to build up
						[11] the flags word for entry */
   ushort oPrsRef;			     /* [11] 0 or oPrs for function
						[11] ref		      */

   /* if caller to MakeVariable really wanted just a search without modifying
      the variable table, return Internal Error code to indicate that the
      search failed                                                           */
   if (mkVar.flags2 & MV_fDontCreate)
      return (PRS_ER_RP | ER_IER);

   /* The following assertion is made because (if for no other reason ...)
      we assume that the user can still CONTinue if he asks to add a variable
      but we return ER_CN and he backs out of the edit                        */
   DbAssertIf(mkVar.flags & !(FVCOMMON | FVFNNAME | FVFUNCTION), 
                                          !(mkVar.flags2 & MV_fTrashTable))

   if ((mkVar.oTyp == UNDEFINED) && !(mkVar.flags & FVASCLAUSE))
                                    /* reference to a record prior to ...AS...*/
      return (PRS_ER_RP | MSG_BadElemRef); /* [9] */

   /* The following assertion is based on the fact that we're using
      grs.oRsCur in the case that we CAN continue to see if there's an active
      frame on the stack for the active procedure - if so, we return ER_CN    */
   DbAssertIf(oPVHash != UNDEFINED, 
               ((grs.oRsCur & 0x8000) && (prsCur.oVarHash == oPVHash)))
   if ((oPVHash != UNDEFINED) && 
       (grs.otxCONT != UNDEFINED) && 
       !(varFlags & (FVSTATIC | FVSHARED | FVCONST)))
      if (FindORsFrame())           /* TRUE if this proc has an active frame  */
	 return (0x8000 | ER_CN);   /* allow the user to back out of edit     */

   oPrsRef = 0; 		    /* [11] initialize			      */

   entryFlags = varFlags & ~0x07;

   if (inFlags & (FVFORCEARRAY | FVINDEXED))
      if (varFlags & FVFUN)
	 entryFlags |= FVINDEXED;
      else {
	 entryFlags |= (FVARRAY | FVINDEXED);
         /* set the dimension count; if input of zero, assume 1 - - - will end
            up giving Rude Edit later if this turns out to have been wrong    */
         if ((mkVar.cDimensions == 0) && !(inFlags & FVCOMMON))
            cbValue = sizeof(dm);
            /* NOTE: we if a dimension count wasn't specified on a COMMON
                     statement for an array, the execute scanner depends on
                     the cDims field in the variable entry being 0;
                     we depend on that for STATIC arrays, in StdSearch        */
         if ((inFlags & FVSTATIC) && (mkVar.cDimensions > 8))
            return (PRS_ER_RE | MSG_SubCnt); /* 'Wrong number of subscripts'  */
         }
   
   if (!(inFlags & FVIMPLICIT))           /* if pathological input was wrong  */
      entryFlags |= FVEVEREXPLICIT;
                                                      
   inFlags = entryFlags;      /* now use flags as passed BY MakeVariable      */

   /* initialize cbValue for static variable case, to save code later on      */
   cbValue += (inFlags & FVARRAY) ? (sizeof(aStat) + - sizeof(oneChar) +
				     sizeof(dm) * mkVar.cDimensions) :
				    (mkVar.oTyp == ET_FS ? mkVar.fsLength :
				     CbTyp(mkVar.oTyp));

   nmsp_type = NMSP_Variable; /* assume we're not creating a FUNCTION, or 
                                 DEF FN entry                                 */

   if (inFlags & 
      (FVFORMAL | FVCOMMON | FVSTATIC | FVFUNCTION | FVSHARED | FVCONST)) {
      if (inFlags & (FVFORMAL | FVCOMMON | FVFUNCTION)) {
         if (inFlags & FVCOMMON) {
	    entryFlags &= ~FVVALUESTORED;	   /* set that bit to FALSE   */
	    cbValue = (inFlags & FVARRAY ? sizeof(aCom) : sizeof(comRef));
            if (inFlags & FVSHARED)
               nm_mask |= NM_fShared;
            }
         else {                                    /* not COMMON -            */
	    cbValue = sizeof(ushort);
            if (inFlags & FVFUNCTION) {
               nmsp_type = 0;                      /* don't set any name table
                                                      bits                    */
               if ((oPVHash == UNDEFINED) && (mkVar.flags & FVFUNCTION))
                  nm_mask |= NM_fShared;           /* set tNam entry flag  */
               if (mkVar.flags & FVLVAL)           /* retval                  */
		  entryFlags &= ~FVVALUESTORED;    /* set to FALSE for retval */
	       else {
		  if ((oPrsRef =
                     PrsRef(mkVar.oNam,
                            (uchar)((mkVar.flags & FVFUNCTION) ? 
                                       PT_FUNCTION : PT_DEFFN),
			    mkVar.oTyp)) & 0x8000) /* [16] function not found */
                     return (PRS_ER_RP | ER_UF);   /* 'Undefined Function'    */
                  }
               }
            else {   /* FVFORMAL */
	       entryFlags &= ~FVVALUESTORED;	   /* set that bit to FALSE   */
               if (inFlags & FVARRAY)
		  cbValue = sizeof(aFormal);
               }  
            }  /* else - not COMMON */
         }  /* if FORMAL, COMMON, DEF FN, or FUNCTION */
      else                                         /* STATIC,SHARED, or CONST */
         if (inFlags & FVSHARED) {
            if (oPVHash == UNDEFINED)              /* entry going in the tMV  */
               nm_mask |= NM_fShared;
            else {   /* entry going in tPV */
	       entryFlags &= ~FVVALUESTORED;	   /* set to FALSE */
	       cbValue = sizeof(ushort);
               }
            }  /* if SHARED */
         else if (inFlags & FVCONST) {
            if (oPVHash == UNDEFINED)              /* entry going in the tMV  */
               nm_mask = NM_fShared;               /* Always set fShared bit
                                                      so constants are found  */
            }
      }  /* if special case flag is set */
   else {
      if ((grs.oPrsCur != UNDEFINED) && (prsCur.procType == PT_DEFFN))
         oPVHash = UNDEFINED;    /* var ref. belongs in the tMV for DEF's     */
      if ((oPVHash != UNDEFINED) && (!(prsCur.flags & FP_STATIC))) {
	 entryFlags &= ~FVVALUESTORED;	  /* actual value not stored in entry */
         if (inFlags & FVARRAY)
	    cbValue = sizeof(aFrame);
         else
	    cbValue = sizeof(ushort);		   /* size of a frame offset  */
         }
      }  /* no special case flag was set */
   cbValue = ((++cbValue) >> 1) << 1;              /* round up cbValue, to 
                                                      ensure even-byte vars. 
                                                      Note that this MUST be
                                                      done in case of odd-sized
                                                      user-defined types or 
						      fixed-length strings.   */
   cbOTyp = ((mkVar.oTyp > ET_MAX) || (mkVar.oTyp == ET_FS)) ?
		  sizeof(ushort) : 0;

   vm_oVarCur = mrsCur.bdVar.cbLogical + cbOTyp + VAR_STRUCT_SIZE;    /* [11] */

   /* [11] see if namespace ER_DD error BEFORE we try to grow the var table,
      [11] so we don't ask user if he wants to continue and THEN give error   */
   if (nmsp_type)				   /* don't set anything if
                                                      creating a DEF or FUN
                                                      entry (txtmgr does it)  */
      if (CheckONamSpace(mkVar.oNam, nmsp_type))
	 return (PRS_ER_RE | ER_DD);		   /* duplicate definition    */

   temp = cbValue + cbOTyp + VAR_STRUCT_SIZE;
   if (temp < cbValue)			/* [12] 64k wrap occurred	      */
      return (PRS_ER_RE | ER_OM);	/* [12] out of memory		      */
   if (temp = GrowBdVar(temp))
      return (temp);
   pVar = (var *)(mrsCur.bdVar.pb + vm_oVarCur); /* in case table moved    */
   ZeroFill((char *)pVar, cbValue);

   if (cbOTyp) {		      /* oTyp > ET_MAX */
      if (mkVar.oTyp == ET_FS) {
		  *((ushort *)((char *)pVar + VAR_cbFixed)) = mkVar.fsLength;
		  entryFlags |= mkVar.oTyp;
      }
      else
		  *((ushort *)((char *)pVar + VAR_oTyp)) = mkVar.oTyp;
      }
   else
      entryFlags |= mkVar.oTyp;
   
   ONamOf(pVar) = mkVar.oNam;			   /* [11]		      */
   FlagsOf(pVar) = entryFlags;			   /* [11] set entry flags    */
   ValueOf(pVar, oPrs) = oPrsRef;		   /* [11] oPrs or zero       */

   if (nmsp_type) {                                /* don't set anything if
                                                      creating a DEF or FUN
                                                      entry (txtmgr does it)  */
      if (SetONamSpace(mkVar.oNam, nmsp_type))
	 ;					   /* [11] CheckONamSpace will
						      [11] already have caught
						      [11] error, if any      */

      }

   mrsCur.oPastLastVar = mrsCur.bdVar.cbLogical;   /* can always trim table 
                                                      back to oPastLastVar and
                                                      lose no variables       */
   vm_pVarCur = pVar;                              /* an exit value           */

   /* initialize the dim count if:
	the entry is an array AND it's NOT (shared AND in a procedure).
	(If an array is shared and in a procedure it doesn't have a cDims
	field in the var table entry.  The entry is a near pointer to the
	array descriptor defining the array.) */

   if ((FlagsOf(pVar) & FVARRAY) &&
      !((FlagsOf(pVar) & FVSHARED) && (oPVHash != UNDEFINED)))	//[17]
      ValueOf(pVar, aryStat.cDims) = mkVar.cDimensions;
                                                   /* this actually sets
                                                      cDims for all arrays    */

   /* now just have to link the new entry in */

   pHash = (ushort *)(mrsCur.bdVar.pb +
                        ((oPVHash == UNDEFINED) ? 
                              (mkVar.oNam & HASH_MV_NAMMASK) : 
                              (oPVHash + (mkVar.oNam & HASH_PV_NAMMASK))));
   if ((*pHash == 0) || (FlagsOf(pVar) & FVDECLDVAR)) {  
      /* link this entry into start of chain */
      OHashLinkOf(pVar) = *pHash;         /* old start of chain in new entry  */
      *pHash = vm_oVarCur;                /* save offset to new entry         */
      }
   else {   /* link this entry into end of chain */
      OHashLinkOf(pVar) = 0;              /* 0 (or 1) indicates end of chain;
                                             UNDEFINED doesn't do it because
                                             we store a flag in the low bit   */
   pVar = (var *)(mrsCur.bdVar.pb + *pHash);	   /* point to start of chain */
   while (OHashLinkOf(pVar) > 1)		   /* while not end of hash chain	   */
      pVar = (var *)(mrsCur.bdVar.pb + (OHashLinkOf(pVar) & 0xFFFE));
      OHashLinkOf(pVar) |= vm_oVarCur;    /* OR this in rather than assign it
                                             to preserve existing flag value  */
      DbAssert(!(vm_oVarCur & 1))         /* we depend on oVars being on even
                                             byte boundaries for this hashlink
                                             bit flag use, and for SD's       */
      }
   return (vm_fVarFound = FALSE);                  /* var created, not found;
                                                      retval says 'no errors' */
   }  /* CreateVar */


/*##############################################################################
#                                                                              #
#                        Variable Searching                                    #
#                                                                              #
##############################################################################*/

/***
*ReDirect() - unlink entry @ vm_oVarCur, set FVREDIRECT, put vm_oVarTmp in value
*Purpose:
*  'ReDirect' a variable entry, i.e., unlink it from its hash chain, set
*  the FVREDIRECT flag in the entry, and put the offset to the 'real'
*  entry (in vm_oVarTmp) into the value field.
*
*  Redirection is performed whenever an existing entry is made invalid
*  by some new instance of a matching variable, for example, what looked
*  like an array at first encounter might be found to be a FUNCTION at
*  a later encounter - - - all such array entries (in tMV and all tPV's)
*  are redirected to this newly created FUNCTION entry. The scanner
*  can then go through and fix up the pcode for each redirected entry,
*  since we'll put the oVar for the new entry in the redirected entry.
*Entry:
*  vm_fPVCur - module static flag, TRUE if hash table is in tPV, FALSE if tMV.
*  mrsCur.bdVar assumed set up, and if vm_fPVCur is TRUE, prsCur is assumed
*     to be set up, and the oVarHash field to contain an offset into
*     mrsCur.bdVar to the tPV hash table.
*  vm_oVarCur is an offset into mrsCur.bdVar to the entry to be redirected.
*  vm_oVarTmp is an offset into mrsCur.bdVar to the entry it is to be
*     redirected to.
*Exit:
*  none.
*Exceptions:
*  none.
*******************************************************************************/
VOID NEAR ReDirect()
   {
   REG1 var *pVar;                     
   REG2 char *pVarBase = mrsCur.bdVar.pb;
   REG3 ushort *pHash;
   REG4 var *pVarPrev;
   REG5 ushort oHashTmp;

   /*--------------------------------------------------------------------------
   | calculate pointer into hash table to offset to the first entry in the    |
   |  chain; this is just:                                                    |
   |     (base of physical table) + (offset to start of hash table) +         |
   |     (offset into hash table)                                             |
   --------------------------------------------------------------------------*/
   DbAssertIf(vm_fPVCur, prsCur.oVarHash != UNDEFINED)
   pHash = (ushort *)(mrsCur.bdVar.pb +
                     (vm_fPVCur ? 
                           (prsCur.oVarHash + (mkVar.oNam & HASH_PV_NAMMASK)) :
                           (0 + (mkVar.oNam & HASH_MV_NAMMASK))));

   pVar = (var *)(pVarBase + *pHash);           /* point to first entry       */

   if (*pHash == vm_oVarCur)                    /* entry is @ start of chain  */
      *pHash = OHashLinkOf(pVar) & 0xFFFE;      /* unlink first entry         */
   else {
      while ((oHashTmp = OHashLinkOf(pVar) & 0xFFFE) != vm_oVarCur) {
         DbAssert(oHashTmp != 0)                /* better not be end of chain */
         pVar = (var *)(pVarBase + oHashTmp);
         }
      pVarPrev = pVar;                          /* point to entry prior to one
                                                   we wish to unlink          */
      pVar = (var *)(pVarBase + oHashTmp);
      OHashLinkOf(pVarPrev) = OHashLinkOf(pVar);/* unlink the entry           */
      }
   
   /* at this point, pVar points to the entry being redirected                */
   FlagsOf(pVar) |= FVREDIRECT;
   ValueOf(pVar, oMV) = vm_oVarTmp;

   }  /* ReDirect */

/***
*FuncSearch() - search the appropriate hash table for FUNCTION case
*Purpose:
*  Search the appropriate table (tPV or tMV) in the case where we've
*  encountered [DECLARE] FUNCTION.
*Entry:
*  vm_fPVCur - module static flag, TRUE if we're to search tPV, FALSE if tMV.
*  mrsCur.bdVar assumed set up, and if vm_fPVCur is TRUE, prsCur is assumed
*     to be set up, and the oVarHash field is either UNDEFINED (in which
*     case we just return), or contains an offset into mrsCur.bdVar to the 
*     tPV hash table.
*
*  mkVar set up as per MakeVariable (below).
*Exit:
*  FALSE = no error
*  otherwise, the same error code is returned as described for MakeVariable,
*     below.
*  If no error is returned, then the static vm_fVarFound indicates success or
*     failure. 
*  If vm_fVarFound == TRUE, vm_oVarCur is set to the offset into mrsCur.bdVar to
*     the found variable entry, and vm_pVarCur points to the entry.
*  If an existing entry has been converted to a FUNCTION entry by the search,
*     the static flag fConvtdToFun will be set TRUE.
*Exceptions:
*  none.
*******************************************************************************/
ushort NEAR FuncSearch()
   {
   REG1 var *pVar;                     
   REG2 char *pVarBase = mrsCur.bdVar.pb;

   /*--------------------------------------------------------------------------
   | calculate offset into mrsCur.bdVar to first entry in appropriate hash    |
   |  chain; this is the contents of:                                         |
   |     (base of physical table) + (offset to start of hash table) +         |
   |     (offset into hash table)                                             |
   --------------------------------------------------------------------------*/
   REG3 ushort oVar = *(ushort *)(mrsCur.bdVar.pb +
                     (vm_fPVCur ? 
                           (prsCur.oVarHash + (mkVar.oNam & HASH_PV_NAMMASK)) :
                           (0 + (mkVar.oNam & HASH_MV_NAMMASK))));
   fConvtdToFun = vm_fVarFound = FALSE;      /* initialize                    */

   if ((oVar == 0) || (vm_fPVCur && (prsCur.oVarHash == UNDEFINED)))
      return(FALSE);                         /* empty hash chain - not found  */

   for (pVar = (var *)(pVarBase + oVar);     /* loop init.                    */
        pVar != (var *)pVarBase;             /* oVar = 0 ==> end of chain     */
        pVar = (var *)(pVarBase + (OHashLinkOf(pVar) & 0xFFFE))) {   
                                             /* loop re-init   */
      if (mkVar.oNam != ONamOf(pVar))
         continue;
      if (!(FlagsOf(pVar) & FVFUN)) {
         if (FlagsOf(pVar) & (FVFORMAL | FVCOMMON | FVSTATIC | FVCONST |
                                                      FVSHARED | FVDECLDVAR))
            return (PRS_ER_RE | ER_DD);
         if (OTypOf(pVar) != mkVar.oTyp)
            if (FlagsOf(pVar) & FVEVEREXPLICIT)
               return (PRS_ER_RE | ER_DD);
            else {
               DbAssert(mkVar.oTyp <= ET_MAX)
               FlagsOf(pVar) = (FlagsOf(pVar) & ~ 0x07 | mkVar.oTyp);
               }
         fConvtdToFun = TRUE;                /* set flag if converting to FUN */
         ValueOf(pVar, oPrs) = PrsRef(mkVar.oNam, PT_FUNCTION, mkVar.oTyp);
                                             /* set value field               */
         DbAssert(ValueOf(pVar, oPrs) != UNDEFINED)
                                             /* error if prs not found        */
         FlagsOf(pVar) |= (FVFUN | FVDECLDVAR | FVEVEREXPLICIT);
         FlagsOf(pVar) &= ~FVARRAY;
         }  /* if entry flag FVFUN is FALSE */
      if (vm_fVarFound)                      /* is this a second match in the */
         return(PRS_ER_RE | ER_DD);          /*    same chain? If so error    */
      /* now, note that we've found a match and save this oVar, but keep
         looking, in case there's another matching name, which would trigger
         the above error ...                                                  */
      vm_fVarFound = TRUE;
      vm_oVarCur = (char *)pVar - mrsCur.bdVar.pb;
      vm_pVarCur = pVar;
      }  /* while not at end of hash chain */
   return (FALSE);                           /* no errors                     */
   }  /* FuncSearch */

/***
*ModSharedChk() - check tPV for matches to a new module SHARED variable
*Purpose:
*  Check the tPV (of prsCur) for a match to a given (new) module SHARED
*  variable. If a match is found, redirect it, i.e., unlink the tPV entry
*  from its hash chain, set the FVREDIRECT flag in it, and place
*  the offset to the new module SHARED variable in its value field.
*
*  Note: This is now optimized based on the knowledge that it's ONLY used
*        used for FUNCTION declarations or definitions.
*  
*Entry:
*  vm_oVarTmp = offset into mrsCur.bdVar to a new module SHARED variable.
*  vm_fPVCur assumed to be TRUE
*  other inputs set up as for FuncSearch (above)
*Exit:
*  TRUE - no error.
*  FALSE - some error from FuncSearch, assumed to be a rude edit case
*           (so we can count on mrsCur.bdVar being tossed out).
*           In this event, the static variable 'errVal' will be contain the 
*           error return, and (non-RELEASE only) the mkVar.flags2 bit
*           MV_fTrashTable will be set.
*Exceptions:
*  none.
*******************************************************************************/
boolean ModSharedChk()
   {
   if (errVal = FuncSearch()) {
      return(FALSE);                /* some error found - quit looking        */
      }
   if (vm_fVarFound)
      ReDirect();
   return (TRUE);
   }  /* ModSharedChk */

/***
*GetDefaultType - given the logical 1st char of a var name,  get default oTyp
*Purpose:
*  Given the logical first char of a name, return the default oTyp of that name.
*  Note that 'logical first char' implies the third char of a name which starts
*  with 'FN' and the first char of any other name.
*Entry:
*  namChar
*Exit:
*  oTyp
*Exceptions:
*  none.
*
*******************************************************************************/
ushort NEAR GetDefaultType(namChar)
char namChar;
   {
   namChar &= 0xDF;   /* convert to upper case */
   DbAssert ((namChar == ('.' & 0xDF)) ||
             ((namChar >= 'A') && (namChar <= 'Z')));
   return(namChar == ('.' & 0xDF)) ? ET_R4 : (ushort)ps.tEtCur[(namChar) - 'A'];
   } /* GetDefaultType */


/***
*MakeVariable - Search for var, create if req'd, return offset to it
*Purpose:
*  Given a variable encountered by the parser or scanner, search for the
*  variable; if not found (and no error conditions detected), create the
*  variable in the tMV or the appropriate tPV and return an offset into
*  mrsCur.bdVar ('oVar') to the value field in the variable entry; this offset 
*  goes in the pcode stream.
*
*Entry:
*  mrsCur set up; it is assumed that bdVar is a heap owner.
*  prsCur either set up, or grs.oPrsCur is UNDEFINED if no procedure is
*     currently active
*  mkVar.oNam - global name table offset for the name of the input variable
*  mkVar.oTyp - global type table offset for the (perhaps assumed) type of the
*              input variable. In the case of an implicitly typed variable,
*              ET_IMP should be passed; in this instance, MakeVariable will 
*              determine the appropriate default type, based on the oNam.
*              In the case of a reference to a record element, the oTyp of the
*              record variable cannot be set by the caller; in this special 
*              case, mkVar.oTyp must be set to UNDEFINED by the caller.
*  mkVar.flags - global 2-bytes of bit flags used to describe the variable
*              encountered. One byte contains flags which are set in 
*              atypical situations, the other contains flags to be tested
*              in common situations.
*              Note that the FVIMPLICIT and FVFNNAME flags are not inputs,
*              however - - - these can be set or reset on input: MakeVariable
*              (now) sets these based on the oTyp and oNam fields (respectively)
*  mkVar.cDimensions - number of dimensions in an array variable; required
*              for correctly allocating the array descriptor, and setting
*              the dimension count for array variables. Only meaningful
*              when FVINDEXED is set, and FVFUNCTION is not (and the variable
*              name does not start with 'FN') (Note that FVINDEXED is 
*              considered to be set whenever FVFORCEARRAY is set).
*  mkVar.flags2 & MV_fONamInOTyp - set when the oType field is either ET_FS or
*	       ET_FT and the fsLength field is the oNam of a CONSTant
*	       which gives the length of the fixed length string/text.
*  mkVar.flags2 & MV_fDontCreate - set when MakeVariable is called JUST to 
*              search for an existing variable. Does not create a new variable 
*              if one is not found with this flag set. Non-RELEASE code checks 
*              that no other modification of the variable table takes place 
*              when this is TRUE.
*
*Exit:
*  The return value is an oVar if the high bit (bit 15) is clear, and
*  it's an error code if the high bit is set. 
*
*  If the high bit is not set (and retval is an oVar), if the input mkVar.oTyp 
*  was incorrectly assumed by the caller (i.e., implicitly typed), mkVar.oTyp
*  will contain the correct oTyp on exit. 
*
*  If the high bit is not set, if a CONSTant var entry was found, mkVar.flags2 
*  will have the MV_fConstFound bit set (always reset on exit otherwise).
*
*  If the high bit is set, the LSB is a scanner error code and the MSB
*  minus bit 15 is a parser action code.  The scanner error code is an
*  error which the user will see, whereas the parser action code is an
*  indication to the parser of what action it should take. The codes are 
*  listed below:
*
*  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
*  |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
*  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
*  | E| P| P| P| P| P| P| P| S| S| S| S| S| S| S| S|
*  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
*
*  E - Error indicator (error if set)
*  P - Parser action code - bits set for different codes (includes bit 15 set):
*        PRS_ER_RE - Module Rude Edit
*        PRS_ER_RP - opReParse placed before statement
*  S - Scanner error code - decimal number in LSB (see basicmsg.doc)
*
*  If the return value is an error code, and the mkVar.flags2 MV_fTrashTable
*  bit is set, then the caller MUST take action to 'throw away' mrsCur.bdVar; 
*  this flag set on error return indicates that the table has been modified
*  somehow and cannot be used dependably. 
*  Note that this is a non-RELEASE flag; assertion code is used to ensure
*  that a rude edit error is returned whenever this flag is TRUE.
*
*  Note that ER_IER ('Internal Error') is used to signal the case where
*  the caller had the mkVar.flags2 MV_fDontCreate bit set on input and a 
*  matching entry was not found.
*
*  Note that an oVar (pVar) is not an offset (pointer) to the start (low word)
*  of the appropriate variable entry, rather it's an offset (pointer) to the
*  value field in the entry. Other fields are referenced from a pVar via 
*  negative offsets.
*  
*  On exit, the following input flag bits are reset to FALSE in mkVar.flags:
*     FVASCLAUSE, FVFNNAME, FVFORCEARRAY, FVFUNCTION, FVIMPLICIT,
*     FVINDEXED, FVLVAL, FVCONST 
*  On exit, the MV_fONamInOTyp and MV_fDontCreate bits are always reset in
*     in mkVar.flags2.
*Exceptions:
*  none.
*
*******************************************************************************/
ushort NEAR MakeVariable()
   {
   REG1 ushort flags;
   REG2 var *pVar;
   REG3 ushort retval;
   REG4 ushort createMask;
   REG5 ushort oHashCreate;
   REG6 ushort oNamPrs;
   REG7 ushort namChar;
   REG8 boolean fCreate;                     /* used to signal that we don't
                                                want to create input variable */
   REG9 ushort inputFlags = mkVar.flags;     /* save input mkVar.flags in case
                                                we must change them locally   */
   ushort mkVarONam;
   ushort mkVarOTyp;
   uchar mkVarCDims;
   boolean fTemp;

   DbAssert(grs.oMrsCur != UNDEFINED)
   DbChkGrs()                    /* sanity check on grs, mrsCur, and prsCur   */

   if (mkVar.flags2 & MV_fONamInOTyp) {
      mkVarONam = mkVar.oNam;
      mkVarOTyp = mkVar.oTyp;
      mkVarCDims = mkVar.cDimensions;
      /* mkVar.fsLength is really an oNam for a CONSTant. We must search local
         var table for this constant (tMV too if we're at proc level) and
         use it's value to set the input oTyp to the appropriate fixed length 
         string constant                                                      */
      DbChkoNam(mkVar.fsLength)
      mkVar.flags2 &= ~MV_fConstFound;
      mkVar.oNam = mkVar.fsLength;
      mkVar.flags = FVIMPLICIT;
      mkVar.oTyp = ET_I2;                    /* CONSTant MUST be an I2     */
      vm_fPVCur = (grs.oPrsCur != UNDEFINED);
      retval = StdSearch();
      if (!vm_fVarFound && !retval && !(vm_fPVCur = !vm_fPVCur))
         /* if we searched tPV and didn't find it, better be in tMV        */
         retval = StdSearch();
      /* restore input values of mkVar struct (must do this here so mkVar.flags
         is correct in case of error)                                         */
      mkVar.oNam = mkVarONam;
      mkVar.oTyp = mkVarOTyp;
      mkVar.flags = inputFlags;
      mkVar.cDimensions = mkVarCDims;
      if (retval || !vm_fVarFound || !(mkVar.flags2 & MV_fConstFound) ||
	  (OTypOf(vm_pVarCur) != ET_I2) || (ValueOf(vm_pVarCur, I2) <= 0)) {
               retval = PRS_ER_RP | MSG_InvConst;
               goto RetValExit;
               }
      mkVar.fsLength = ValueOf(vm_pVarCur, I2);
      }

   mkVar.flags &= ~(FVFNNAME | FVIMPLICIT);  /* Initialize the flags to FALSE */

   DbAssertIf(mkVar.flags & FVCONST, 
	       (mkVar.oTyp != ET_IMP))

   if ((mkVar.oTyp == ET_IMP) || 
         ((mkVar.oTyp == UNDEFINED) && !(mkVar.flags & FVASCLAUSE)))
      mkVar.flags |= FVIMPLICIT;             /* note that an 'UNDEFINED' oTyp
                                                could really be a fixed-length
                                                string of maximal (32K) length.
                                                The FVASCLAUSE bit removes this
                                                ambiguity                     */
   if ((namChar = GetVarNamChar(mkVar.oNam)) & 0xff00)
      mkVar.flags |= FVFNNAME;               /* get first logical char of name
                                                in low byte, & fFNnam in high */

   if (vm_fPVCur = (grs.oPrsCur != UNDEFINED)) {
      DbAssert(prsCur.oVarHash != UNDEFINED)
      DbAssert(prsCur.oVarHash < mrsCur.bdVar.cbLogical)
      if (mkVar.flags & FVCOMMON) {
         retval = PRS_ER_RP | MSG_InvProc;
         goto RetValExit;
         }
      oHashCreate = prsCur.oVarHash;
      /* The below is a speed optimization. oNamOfPrsCur is set by the rude 
         scanner to prevent searching the name table for the proc name for
         each variable reference in the procedure.  When we're called by the
         parser, oNamOfPrsCur will be UNDEFINED and we must do it here.       */
      if ((oNamPrs = oNamOfPrsCur) == UNDEFINED)
         oNamPrs = ONamOfOgNam(prsCur.ogNam);/* [4]                           */
      DbAssert(oNamPrs != 0)                 /* [4] ONamOfOgNam CAN'T ret. OM */
      }
   else {                                    /* no prs active                 */
      if (mkVar.flags & FVSTATIC) {
         retval = PRS_ER_RP | MSG_InvModLev;
         goto RetValExit;
         }
      oHashCreate = 
      oNamPrs = UNDEFINED;
      }

   flags = mkVar.flags;          /* put flags word in a register              */

   if ((flags & FVIMPLICIT) && (mkVar.oTyp != UNDEFINED)) 
         mkVar.oTyp = GetDefaultType((char)namChar);  /* set default type     */

   DbMkVarInp()                  /* special validation code for global inputs */

   /*======================================================================
      'createMask' is a mask used to pass to CreateVar - - - the code below
      depends on the fact that most of the variable entry flags correspond
      in position to the MakeVariable input flags ...                
   =======================================================================*/
   createMask = FVVALUESTORED |
                (flags & ~(FVREDIRECT | FVEVEREXPLICIT | FVDIM | FV_STATICSET));
   
   nm_mask = 0;                              /* initialize                    */
   vm_oVarCur = UNDEFINED;                   /* initialize                    */
   mkVar.flags2 &= ~MV_fConstFound;          /* initialize                    */

   if (!(flags & (FVFORMAL | FVCOMMON | FVSTATIC | FVSHARED | FVFNNAME |
                  FVFUNCTION | FVCONST))) {

      /*==== SEARCHING FOR VARIABLE IN THE TYPICAL CASE ====*/

      if (retval = StdSearch())
         goto RetValExit;                    /* return error code             */

      if (vm_fPVCur) {                       /* a procedure is active         */
         /*====================================================================
         Even though a procedure is active, we must search the tMV as well
         in one of three possible cases:
         1. A match was not found, and the name table entry indicates that
            there is a module shared variable of this oNam.
         2. A match was not found, and the current prs is for a DEF FN.
         3. We've found a retval in the currently active FUNCTION, but input
            FVLVAL is not set, i.e., we've got a case of self-recursion,
            and thus want to find the tMV FUNCTION reference rather than the
            tPV retval. Note that in this case, we KNOW we'll find a match
            in the tMV, as the reference is to the FUNCTION in prsCur.
         ====================================================================*/
         if ((!vm_fVarFound && 
              ((FlagOfONam(mkVar.oNam) & NM_fShared) ||
               (prsCur.procType == PT_DEFFN))) ||
             (vm_fVarFound && !(flags & FVLVAL) && 
              (FlagsOf(vm_pVarCur) & FVFUN) &&
              (mkVar.oNam == oNamPrs) && (prsCur.procType == PT_FUNCTION))) {
            vm_fPVCur = FALSE;               /* search the tMV                */
            if (retval = StdSearch())
               goto RetValExit;              /* return error code             */
            vm_fPVCur = TRUE;                /* reset                         */
            if (vm_fVarFound) {
	       pVar = vm_pVarCur;
	       /* [10] set temp boolean TRUE if there exists a module-level
		  [10] var of this name (not necessarily the var just found!  */
	       fTemp = FlagOfONam(mkVar.oNam) & NM_fShared;	/* [10]       */
	       if (fTemp && !(FlagsOf(pVar) & (FVFUN | FVCONST))) {   /* [10] */
		  if (!(FlagsOf(pVar) & FVSHARED))		      /* [10] */
		      vm_fVarFound = FALSE;  /* [10] not SHARED, so not found */
		  }						      /* [10] */
	       else if (!fTemp && (prsCur.procType != PT_DEFFN))      /* [10] */
		      vm_fVarFound = FALSE;  /* if not SHARED, then not found */
               else if ((FlagsOf(pVar) & FVFUN) && (flags & FVLVAL))
                  if ((mkVar.oNam == oNamPrs) && 
                      (prsCur.procType == PT_FUNCTION)) { 
                     /* not found after all - really want to create a retval  */
                     createMask |= (FVFUN | FVDECLDVAR | FVEVEREXPLICIT);
                     vm_fVarFound = FALSE;
                     }
                  else goto RE_DD_Exit;      /* attempt to assign to a Function
                                                outside of the Function       */
               }  /* if vm_fVarFound */
            }  /* if !vm_fVarFound && tNam entry fShared bit set */
         else if (vm_fVarFound && (FlagsOf(vm_pVarCur) & FVSHARED))
            vm_oVarCur = ValueOf(vm_pVarCur, oMV); 
                                             /* caller doesn't want oVar for 
                                                the SHARED entry in the tPV,
                                                he wants the actual tMV entry */
         }
      else                                   /* no procedure is active        */
         if ((flags & FVLVAL) && vm_fVarFound && (FlagsOf(vm_pVarCur) & FVFUN))
            goto RE_DD_Exit;                 /* attempt to assign to a Function
                                                outside of the Function       */
      if (!vm_fVarFound)
         if (retval = CreateVar(oHashCreate, createMask))
            goto RetValExit;                 /* return error code             */
      }  /* typical case - no special flags set */

   else {

      /*==== SEARCHING FOR SOME SPECIAL VAR (i.e., COMMON, STATIC, etc.) =====*/

      if (flags & (FVFUNCTION | FVFNNAME)) {
         if (flags & FVFUNCTION) {
            DbAssert(vm_fPVCur == FALSE)     /* [DECLARE] FUNCTION only legal
                                                at module level               */
            if (retval = FuncSearch())
               goto RetValExit;
            pVar = vm_pVarCur;
            if ((!vm_fVarFound) || (fConvtdToFun)) {
               if (!fConvtdToFun)         /* function entry doesn't exist yet */
                  if (retval = CreateVar(UNDEFINED, 
                        createMask | FVDECLDVAR | FVEVEREXPLICIT | FVSHARED))
                     goto RetValExit;     /* return error code */
               if ((NMSP_MASK & FlagOfONam(mkVar.oNam)) == NMSP_Variable) {
                  /* there exists at some non-FUNCTION variable(s) of this 
                     oNam somewhere in mrsCur.bdVar                           */
                  /* now set up inputs for each invocation of ModSharedChk    */
                  vm_fPVCur = TRUE; 
                  vm_oVarTmp = vm_oVarCur;
                  
                  errVal = 0;
                  ForEachPrsInMrs(ModSharedChk);
               
                  /* reset static variables */
                  vm_oVarCur = vm_oVarTmp;
                  vm_fPVCur = FALSE;
                  retval = errVal;

                  FlagsOf(vm_pVarCur) |= FVSHARED;
                  /* remember that an entry of this oNam is module shared: */
                  nm_mask |= NM_fShared;
   
                  if (retval)
                     goto RetValExit;  
                  ResetONamMaskTmp(mkVar.oNam,NMSP_Variable);
                                          /* txtmgr depends on us resetting 
                                             this bit in this case            */
                  }
               }
            }  /* if FVFUNCTION */

         else {                  /* must be FVFNNAME */
            if ((!vm_fPVCur) || (mkVar.oNam != oNamPrs))
               if (mkVar.flags & FVLVAL)
                  goto RE_DD_Exit;          
            if (retval = StdSearch())
               goto RetValExit;              /* return error code             */
            if (!vm_fVarFound) {
               if ((vm_fPVCur) && !(mkVar.flags & FVLVAL)) {
                  vm_fPVCur = FALSE;         /* search the tMV                */
                  if (retval = StdSearch())
                     goto RetValExit;        /* return error code             */
                  vm_fPVCur = TRUE;          /* reset                         */
                  }
               if (!vm_fVarFound)
                  if (retval = CreateVar(oHashCreate, createMask | FVFUN))
                     goto RetValExit;        /* return error code             */
               }
            }  /* if FVFNNAME */
         }  /* if FVFUNCTION, of FVFNNAME */

      else {                                 /* must be FORMAL, STATIC, 
                                                COMMON, SHARED, or CONST      */
         if (retval = StdSearch())
            goto RetValExit;                 /* return error code             */

         if (flags & (FVFORMAL | FVSTATIC | FVCONST)) {
            if (vm_fVarFound)
               goto RE_DD_Exit;              /* existing var matches formal,
                                                static, or const              */
            if (retval = CreateVar(oHashCreate, createMask))
               goto RetValExit;              /* return error code             */
            
            if ((flags & FVCONST) && !vm_fPVCur) {
               DbAssert(txdCur.scanState == SS_RUDE)  /* ensure no owners     */
               FlagsOf(vm_pVarCur) |= FVSHARED;

               /* remember that an entry of this oNam is module shared: */
               nm_mask |= NM_fShared;
               }
            }  /* if FVFORMAL, FVSTATIC, or FVCONST */

         else if (flags & FVCOMMON) {
            fCreate = TRUE;                  /* assume we want to create the
                                                variable if not found         */
            if (vm_fVarFound) {
               pVar = vm_pVarCur;
               if (FlagsOf(pVar) & FVARRAY) {
                  DbAssert(txdCur.scanState == SS_RUDE)  /* ensure no owners  */
                  FlagsOf(pVar) |= FVCOMMON; /* convert entry to be a COMMON  */
                  FlagsOf(pVar) &= ~FVVALUESTORED;
                  fCreate = FALSE;           /* don't want to create too ...  */
                  }
	       else			     /* not an array		      */
		  goto RE_DD_Exit;	     /* [14]			      */
               }  /* if found */
            
            if (fCreate)   
               if (retval = CreateVar(UNDEFINED, createMask))
                  goto RetValExit;     /* return error code */

            if (flags & FVSHARED) {
               DbAssert(txdCur.scanState == SS_RUDE)  /* ensure no owners     */
               FlagsOf(vm_pVarCur) |= FVSHARED;

               /* remember that an entry of this oNam is module shared: */
               nm_mask |= NM_fShared;
               }
            }  /* if FVCOMMON */

         else {                              /* must be FVSHARED              */
            DbAssert(flags & FVSHARED)
            /* NOTE: this case MUST be tested AFTER we check for FVCOMMON     */
            if (vm_fPVCur) {                 /* active procedure              */
               if (vm_fVarFound)
		  goto RE_DD_Exit;	     /* [14] in case found entry
						[14] contains an owner	      */
               else {                        /* var not found in the tPV      */
                  vm_fPVCur = FALSE;
                  if (retval = StdSearch())  /* search tMV                    */
                     goto RetValExit;
                  vm_fPVCur = TRUE;          /* reset                         */
                  if (!vm_fVarFound) {
                     if (retval = CreateVar(UNDEFINED, createMask & ~FVSHARED))
                        goto RetValExit;     /* create var a module level     */
                     }
                  FlagsOf(vm_pVarCur) |= FVSHARED; /* scanner needs this bit
                                                      set in module entry.
                                                      Namespace bit keeps
                                                      this case straight.     */
                  vm_oVarTmp = vm_oVarCur;   /* save offset to tMV entry      */
                  if (retval = CreateVar(prsCur.oVarHash, createMask))
                     goto RetValExit;        /* return error code             */
                  pVar = vm_pVarCur;
                  ValueOf(pVar, oMV) = 
                  vm_oVarCur = vm_oVarTmp;
                  }
               }
            else {                           /* no proc active - DIM SHARED   */
               if (!vm_fVarFound)
                  if (retval = CreateVar(UNDEFINED, createMask))
                     goto RetValExit;        /* return error code             */
               /* NOTE: the parser CAN call us here when we're not in SS_RUDE,
                  NOTE: but the txtmgr guarantees us that if a user enters
                  NOTE: or modifies a DIM SHARED statement, it will be a rude
                  NOTE: edit. I.e., we don't need to worry about any proc-level
                  NOTE: variables here, since the rude scanner scans the
                  NOTE: module before any of the procedures in the module     */
               FlagsOf(vm_pVarCur) |= FVSHARED;

               /* remember that an entry of this oNam is module shared: */
               nm_mask |= NM_fShared;
               }
            }  /* if FVSHARED */

         }  /* COMMON, SHARED */

      }  /* some special-case flag(s) is/are set */


   /* var was either explicitly or implicitly typed; set flag accordingly:    */
   if (!(flags & FVIMPLICIT))
      FlagsOf((var *)(mrsCur.bdVar.pb + vm_oVarCur)) |= FVEVEREXPLICIT;

   if (nm_mask != 0)
      SetONamMask(mkVar.oNam, nm_mask);      /* don't set name table flag bits
                                                until no chance of error      */

   DbChkoVar(vm_oVarCur)
   DbChkoTyp(mkVar.oTyp)
   DbMkVarExit(vm_oVarCur,vm_fVarFound,vm_fPVCur)

   /* Reset most of the input flags to their default values (FALSE) - - leave
      the rest as they were on entry                                          */
   mkVar.exitFlags = mkVar.flags;            /* NOTE: can remove exitFlags
                                                      when MakeVariable is
                                                      rewritten in native code*/
   mkVar.flags = inputFlags & ~(FVFNNAME | FVASCLAUSE | FVIMPLICIT | FVLVAL | 
		     FVFORCEARRAY | FVINDEXED | FVFUNCTION | FVCONST);
   mkVar.flags2 &= ~(MV_fONamInOTyp | MV_fDontCreate);

   return(vm_oVarCur);

RE_DD_Exit:
   retval = PRS_ER_RE | ER_DD;
RetValExit:
   DbMkVarExit(retval,vm_fVarFound,vm_fPVCur)
   /* Reset most of the input flags to their default values (FALSE) - - leave
      the rest as they were on entry                                          */
   mkVar.exitFlags = mkVar.flags;            /* NOTE: can remove exitFlags
                                                      when MakeVariable is
                                                      rewritten in native code*/
   mkVar.flags = inputFlags & ~(FVFNNAME | FVASCLAUSE | FVIMPLICIT | FVLVAL | 
		     FVFORCEARRAY | FVINDEXED | FVFUNCTION | FVCONST);
   mkVar.flags2 &= ~(MV_fONamInOTyp | MV_fDontCreate);

   return(retval);
   }  /* MakeVariable */


/*##############################################################################
#                                                                              #
#                 Call-back code, for when a variable table moves              #
#                                                                              #
##############################################################################*/
/***
*AdjustStatChain(pVarTable, oVar, cbAdjust)
*Purpose:
*  Adjust the back pointers to AD's and SD's in all static variables in
*  the current hash chain by cbAdjust bytes.
*  Note that this code is shared by AdjustMrsVarTable and AdjustPrsVarTable.
*
*Entry:
*  pVarTable is the base pointer for the variable table.
*  oVar is an offset into a variable table to the first var in a chain.
*  cbAdjust is an adjustment factor to be added to appropriate backpointers.
*
*Exit:
*  none.
*
*Exceptions:
*  none.
*
*******************************************************************************/
STATICF(VOID) AdjustStatChain(pVarTable, oVar, cbAdjust)
var *pVarTable;
REG2 ushort oVar;
ushort cbAdjust;
   {
   REG1 var *pVar;
   REG3 ushort flags;

   while (oVar != 0) {
      pVar = (var *)((char *)pVarTable + oVar);
      flags = FlagsOf(pVar);
      oVar = (OHashLinkOf(pVar) & 0xFFFE);

      if ((flags & FVFUN) || !(flags & FVVALUESTORED))
         continue;
      if (flags & FVARRAY) {
         if ((ValueOf(pVar, aryStat.aryDsc.fFeatures & FADF_SD)) &&
             (ValueOf(pVar, aryStat.aryDsc.pNext) != NOT_OWNER))
                  B_IAdUpd(&(ValueOf(pVar, aryStat.aryDsc)), cbAdjust);
         }
      else if ((OTypOf(pVar) == ET_SD) && (ValueOf(pVar,sdStr.pb) != NULL)) {
         B_ISdUpd(&(ValueOf(pVar, sdStr)), cbAdjust);
         }
      }  /* while */
   }  /* AdjustStatChain */

/***
*AdjustMrsVarTable(pVarTable, cbAdjust)
*
*Purpose:
*  Adjust the back pointers to AD's and SD's in all static variables in
*  the tMV by cbAdjust bytes.
*
*Entry:
*  pVarTable is the base pointer for the variable table.
*  cbAdjust is an adjustment factor to be added to appropriate backpointers.
*
*Exit:
*  none.
*
*Exceptions:
*  none.
*
*******************************************************************************/
VOID NEAR AdjustMrsVarTable(pVarTable, cbAdjust)
var *pVarTable;
ushort cbAdjust;
   {
   REG1 ushort iHash;
   REG2 ushort *pHash = (ushort *)pVarTable; /* tMV hash tbl @ start    */

   for (iHash = 0; iHash < CBINITMVHASH/2; iHash++)
      AdjustStatChain(pVarTable, *pHash++, cbAdjust);
   }  /* AdjustMrsVarTable */

/***
*AdjustPrsVarTable(pVarTable, oVarHash, cbAdjust)
*
*Purpose:
*  Adjust the back pointers to AD's and SD's in all static variables in
*  the given tPV by cbAdjust bytes.
*
*Entry:
*  pVarTable is the base pointer for the variable table.
*  oVarHash is the offset in the variable table to the tPV hash table.
*  cbAdjust is an adjustment factor to be added to appropriate backpointers.
*
*Exit:
*  none.
*
*Exceptions:
*  none.
*
*******************************************************************************/
VOID NEAR AdjustPrsVarTable(pVarTable, oVarHash, cbAdjust)
var *pVarTable;
ushort oVarHash;
ushort cbAdjust;
   {
   REG1 ushort iHash;
   REG2 ushort *pHash = (ushort *)((char *)pVarTable + oVarHash);
   
   if (oVarHash != UNDEFINED)
      for (iHash = 0; iHash < CBINITPVHASH/2; iHash++)
         AdjustStatChain(pVarTable, *pHash++, cbAdjust);
   }  /* AdjustPrsVarTable */
