/* File: txtmgr.h - Defines which apply to the QBI Text Manager 	*/
/* NOTE: When making changes to this file, be sure to make equivalent	*/
/*	 changes to file TXTMGR.INC					*/

#undef TXTMGR_H
#define TXTMGR_H ON	   /* remember that this file has been included */

#if !HEAP_H
# include "heap.h"
#endif

/**======================================================================**
 **==		External Interface to Text Manger Component	       ==**
 **======================================================================**/

/* special lnInsert values for LoadFile(psdFilename, lnInsert) */
#define LF_NewProg	0xFFFF	/* do a NEW before the load */
#define LF_NewModule	0xFFFE	/* don't do a NEW before the load */
#define LF_ViewIncl	0xFFFD	/* load INCLUDE file for editing */
#define LF_NewDoc	0xFFFC	/* load a document into a window */

/* The global static txtErr contains return values from TxtDirect */
typedef struct txtErrType {
   ushort errCode;   /* offset into the QBI Message Table (MSG_xxx) or,
			if UNDEFINED, ps.bdpError contains the parser-built
			ASCII error message */
   ushort oRs;	     /* identifies the text tbl where the error is.  only valid
			if fDirect is FALSE */
   ushort otx;	     /* text offset where the error is */
   ushort oSrc;      /* UNDEFINED if the error was caught by the scanner.
			   The user interface is responsible for invoking
			   the lister to map the txtErr.otx to a line and
			   column which can be displayed to the user.
			else The error was caught by the parser.
			   oSrc identifies the column within the source
			   line where the error occurred. */
   char fDirect;     /* TRUE if error was in direct mode buffer,
			FALSE if it was somewhere in loaded program */
   } txtErrType;

EXTERNAL txtErrType txtErr;   /* TxtDirect's Exit parm structure */
EXTERNAL uchar txtFindIndex;
   /* return value set by TxtFindOp (and friends).  If 1st opcode in
      list was found, 1 if 2nd opcode in list was found etc.
      (this is only set if parm2 is not NULL on entry) */
extern uchar fViewInclude;
   /* non-zero if user wants to see INCLUDEd source lines */
extern uchar fLnNotIncl;
   /* static return value of LnOfOtx, OtxOfLn, OtxBolOfOtx zero if given
      line was an INCLUDEd line */
extern ushort cWatch;
   /* number of watch expressions in all loaded modules */


/* The following masks get ORed into compSwitches by SetCompSwitches() */
#define COMP_SW_E 1	/* set for each module which has ON ERROR stmt */
#define COMP_SW_X 2	/* set for each module which has RESUME NEXT stmt */
#define COMP_SW_V 4	/* set if any module has ON <event> stmt */
#define COMP_SW_W 8	/* set if any module has ON <event> stmt */
extern uchar compSwitches;

void FAR TxtDescan(void);
void FAR TxtModified(void);
void NEAR TxtDescanCP(void);
void NEAR ChkAllUndefPrs(void);
void NEAR ChkAllUndefPrsSaveRs(void);
void NEAR FreeAllUndefPrs(void);
ushort FAR TxtChange(ushort, ushort, ushort);
ushort FAR TxtPaste(ushort, ushort);
void FAR TxtStartBigEdit();
bool FAR TxtEndBigEdit();
ushort FAR TxtDirect(void);
ushort FAR SystemScan(void);
ushort FAR TxtViewIncl(ushort, boolean);
ushort NEAR TxtSegCurCP(void);
ushort NEAR TxtTblSegCurCP(void);
ushort FAR LoadFile(sd *, ushort);
ushort FAR SaveFile(void);
ushort FAR FileGetLineBd(bd *);
ushort FAR SaveAllDeclares(void);
boolean FAR AskRudeEditFar(void);
ushort FAR TxtReInclude(void);
boolean FAR fNextStmtDoesIO(void);
void FAR WatchDel(ushort);
void FAR SkipStop(void);

boolean NEAR TxtCurInit(void);
ushort NEAR TxtDiscard(void);
void NEAR TxtActivate(void);
void NEAR TxtDeactivate(void);
void FAR TxtPrsInit(ushort);
boolean NEAR TxtFree(ushort);
void NEAR TxtMoveDown(ushort, ushort);
boolean NEAR TxtMoveUp(ushort, ushort);
ushort NEAR GetWOtx(ushort);
void NEAR PutWOtx(ushort, ushort);
boolean NEAR InsertWOtx(ushort, ushort);
ushort NEAR TxtFindOp(ushort, ushort *);
ushort NEAR TxtFindNextOp(ushort, ushort *);
ushort NEAR TxtFindOpDs(ushort, ushort *);
ushort NEAR TxtFindNextOpDs(ushort, ushort *);
ushort NEAR TxtFindOpExec(ushort, ushort *);
void NEAR TxtDeThread(ushort *, ushort, ushort);
void NEAR TxtInsThread(ushort *, ushort *, ushort, ushort);
void NEAR TxtAddThread(ushort *, ushort);
ushort NEAR TxtBindPrsS(boolean);
void NEAR OtxDefType(void);
void NEAR OtxDefTypeEot(void);
void FAR SystemDescanRude(void);
void NEAR ModuleRudeEdit(void);
void FAR ModuleRudeEditFar(void);
boolean NEAR ChkDelPrs(boolean);
ushort FAR SetViewInclude(uchar);
ushort FAR LnOfOtx(ushort);
ushort NEAR OtxEndProg(void);
ushort FAR OtxOfLn(ushort);
ushort FAR OtxBolOfOtx(ushort);
ushort FAR OtxBosOfOtx(ushort);
ushort FAR OPrsOfOtx(ushort);
ushort NEAR OtxResume(ushort);
ushort NEAR OtxTypDefined(ushort);
ushort FAR DescanOpcode(ushort);
ushort FAR SetCompSwitches(void);

/* BASIC Debugger functions */
boolean FAR ToggleBp(ushort);
void FAR ClrBpAll(void);
boolean FAR fBpSet(ushort);

# define DbObdlOpcodes(x) {;}
# define DbChkTxdCur() {;}
# define DbChkTxdThr() {;}
