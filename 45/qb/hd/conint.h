/* File: CONINT.H - Defines which apply to the QBI Context Manager		*/
/* NOTE: When making changes to this file, be sure to make equivalent	*/
/*			changes to file CONINT.INC													*/

#undef CONINT_H
#define CONINT_H ON			/* remember that this file has been included */

/* Bit flag Constants for conFlags (in context.asm)	*/
#define	F_CON_StaticStructs	1
												/* Set (TRUE) when mrsCur, prsCur
												   and txdCur are to be used to
												   access current context. If
												   reset (FALSE), then these
												   must be accessed out of the
												   appropriate tables.					*/
#define	F_CON_LeaveTMrsEmpty	2	
												/*	normally, if MrsDiscard removes
												   the last mrs from tMrs, it
												   creates a new unnamed one; if
												   this flag is TRUE, it just
												   leaves the table empty
												   an unreferenced empty prs			*/
#define	F_CON_ResetStkSize	4		/* Set by NewStmt when not CHAINing. 
													Causes next BOS/BOL to reset the 
													stack to its default size.			*/
#define	F_CON_RunFile			8		/* Set by exStRunFile so that NewStmt 
													will not show the debug screen	*/

char conFlags;					/* bit flags, used in context.asm */

VOID	FAR	DebChkConStatStructs(VOID);
VOID	FAR	DebChkConNoStatStructs(VOID);
