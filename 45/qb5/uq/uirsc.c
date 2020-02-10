/*** 
*uirsc.c - Register Set routines.
*
*	Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*	User interface routines to handle register sets.
*
*******************************************************************************/

/* First, include version definition header */
#include <version.h>

/* Next, include COW's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>

/* Next, include QBI's headers */
#ifndef CONTEXT_H
#include <context.h>
#endif

#ifndef LISTER_H
#include <lister.h>
#endif

#ifndef NAMES_H					/* [1] */
#include <names.h>				/* [1] */
#endif						/* [1] */

#ifndef PARSER_H
#include <parser.h>
#endif

#ifndef QBIMSGS_H
#include <qbimsgs.h>
#endif

#ifndef TXTMGR_H
#include <txtmgr.h>
#endif

#ifndef UI_H
#include <ui.h>
#endif

#ifndef UIINT_H
#include <uiint.h>
#endif

#ifndef UTIL_H
#include <util.h>
#endif

/*----------------------------------------------------------------------*
 * register set management (i.e. interface with context manager)  *
 *----------------------------------------------------------------------*/

/**************************************************************************
* UiRsActivate(oRs)
* Purpose:
*  Tell the context manager to activate the specified register set.
*  If oRs == UNDEFINED, no register set is active on exit.
*
**************************************************************************/
void near UiRsActivate (oRs)
ushort oRs;
{
    DbAssert (oRs != hbufHelp); // [6] Shouldn't be activating help
    if (grs.oRsCur != oRs)	/* speed optimization */
	RsActivate (oRs);
    UiFlushCache ();
}

/**************************************************************************
* UiRsActivateWnd
* Purpose:
*  Tell the context manager to activate the active window's register set
*
**************************************************************************/
void near UiRsActivateWnd ()
{
    register struct ef *pef = (struct ef *) pwndAct->pefExtra;	// [7]

    DbAssert (pwndAct != NULL);
    if (pef->hBuffer != hbufHelp)	/* [5] */
	UiRsActivate (pef->hBuffer);
}

/**************************************************************************
* ushort near RsMake(psdName, rsType)
* Purpose:
*  Tell the context manager to create a text table (register set).
*  This may be a mrs (module register set) or prs (procedure register set).
*  If rsType == RS_sub or RS_function, the new procedure will be put in the
*     current module.  If the command window, or text file is active, the
*     procedure will be put in the MAIN module.  If there is no MAIN
*     module, uierr = MSG_NoMainProg on exit.
* Entry:
*  psdName points to sd for the name the context manager
*     is to associate with the register set.
*  rsType is the type of entry being created (RS_xxx)
* Exit:
*  if out-of-memory, duplicate module, or duplicate procedure error,
*     returns UNDEFINED and the global uierr indicates the error code.
*  the oRs is returned.
*
**************************************************************************/
ushort NEAR RsMake (psdName, rsType)
sd *psdName;
REG1 char rsType;
{
    REG2 ushort errCode;
    ushort result;
    ushort ogNam;			/* [1] */

    result = UNDEFINED;
    if (rsType == RS_cmdWnd)		/* [2] */
	ogNam = OGNAM_IMMEDIATE;	/* [2] */
    else if (rsType == RS_scrap)	/* [2] */
	ogNam = OGNAM_CLIPBOARD;	/* [2] */
    else {				/* [2] */
	ogNam = OgNamOfPsd(psdName);	/* [1] */
	if (!ogNam) {			/* [1] OM error returned by nammgr */
	    errCode = ER_OM;		/* [1] */
	    goto CheckErr;		/* [1] */
	}				/* [1] */
    }

    if ((rsType == RS_sub) || (rsType == RS_function)) {
	if (mrsCur.flags2 & FM2_NoPcode) {
	    // Current module is not pcode, just an ASCII file.
	    // It is either be the cmd window or document file.
	    if (grs.oMrsMain == UNDEFINED) {
		errCode = MSG_NoMainProg;  /* Error: No main program */
		goto CheckErr;
	    }
	    /* put new procedure in MAIN source file */
	    UiRsActivate(grs.oMrsMain);
	}
	if (rsType == RS_function) {
	    /* There may already be references to this function that look
	     * like variables.	Since PrsMake causes the pcode for
	     * the FUNCTION and END FUNCTION stmts to be inserted,
	     * this makes sure a variable table entry gets built
	     * for the function by the Rude Scanner.
	     */
	    ModuleRudeEditFar();
	}
	errCode = PrsMake (ogNam, (rsType == RS_sub) ?
				  (char) PT_SUB : (char) PT_FUNCTION); // [1]
    }
    else {
	/* make use of REG var 'errCode' to set flags */
	errCode = 0x100 * FM2_File;		/* assume it is a module */
	if ((rsType == RS_cmdWnd) || (rsType == RS_scrap))	/* [2] */
	    errCode = 0x100 * FM2_NoPcode;
	if (rsType == RS_document)
	    errCode = 0x100 * (FM2_File | FM2_NoPcode);
	if (rsType == RS_includeFile)
	    errCode= 0x100 * (FM2_File | FM2_Include);
	errCode = MrsMake (ogNam, errCode);	/* [1] */
    }

CheckErr:
    if (errCode != 0) {
	/* tell GetCmd to remember to report the error
	 * and not to position the cursor
	 */
	SetUiErr (errCode);
    }
    else
	result = grs.oRsCur;
    return result;
}
