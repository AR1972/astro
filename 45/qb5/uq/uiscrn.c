/*** 
*uiscrn.c - screen management functions.
*
*	Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*	Miscellaneous procedures to interface QB with CW screen operations.
*
*******************************************************************************/

/* First, include version definition header */
#include <version.h>

/* Next, include TextWin's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>

/* Next, include QBI's headers */
#ifndef QBIMSGS_H
#include <qbimsgs.h>
#endif

#ifndef UI_H
#include <ui.h>
#endif

#ifndef UIINT_H
#include <uiint.h>
#endif

#ifndef HEAP_H
#include <heap.h>
#endif

void far EnsShowDebugScrFar(void);
void far ShowOutScr(void);
void far EnsMouseOff(void);
void far TossOutputScreen(void);
void near Cls (void);
void near SaveUserMouse(void);
void near RestoreUserMouse(void);

extern bool FAR PASCAL FInitMouse(void);	//[17]
extern void FAR PASCAL EndMouse(void);		//[17]

extern void FAR PASCAL SetBlinkBit(ushort);

bool fDebugScr = FALSE;             /* TRUE if debug screen is visible,
				       FALSE if Output Screen is visible */

extern WORD iModeCurrent;

bd bdVids = {0, NULL, 0};
bd bdVidsDebug = {0, NULL, 0};
bdl bdlVidData = {0, NOT_OWNER, 0, 0};
bdl bdlUserMouse = {0, NOT_OWNER, 0, 0};	//[17]

/*** 
*void near Cls ()
*Purpose:
*	Clear the output screen.  This is used if we couldn't restore the
*	output screen.
*
*Entry:
*	None.
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
void near Cls ()
{
    RRC rrc;

    rrc.rxLeft = rrc.ryTop = 0;
    rrc.rxRight = axMac;
    rrc.ryBottom = ayMac;
    FillRrc (NULL, &rrc, ' ', isaBackground);
}


/*** 
*void near SaveUserMouse
*Purpose:
*	Saves the mouse state of the User's mouse
*
*	New for revision [17]
*
*Entry:
*
*Exit:
*
*Exceptions:
*	None.
*******************************************************************************/
void near SaveUserMouse()
{
    WORD cbMouse;

    BdlFree(&bdlUserMouse);

    cbMouse = CbSizeMouseState();
    if (cbMouse && BdlAlloc (&bdlUserMouse, cbMouse)) {
       SaveMouseState((void FAR *) MAKELONG (0, GETSEG (bdlUserMouse.seg)));
    }
}

/*** 
*void near RestoreUserMouse
*Purpose:
*	Restores the mouse state of the user's mouse
*
*	New for revision [17]
*
*Entry:
*
*Exit:
*
*Exceptions:
*	None.
*******************************************************************************/
void near RestoreUserMouse()
{
    if (bdlUserMouse.seg != NOT_OWNER) {
       RestoreMouseState((void FAR *) MAKELONG (0, GETSEG (bdlUserMouse.seg)));
       BdlFree(&bdlUserMouse);
    }
}

/*** 
*void near SaveUserScreen ()
*Purpose:
*	Save the contents of the output screen.
*
*	If we don't have enough room for the screen data, we still save the
*	mode information so we can at least change modes and clear the screen
*	properly.
*
*Entry:
*	bdVids		Is a bd to be used for the mode structure data.
*	bdlVidData	Is a bdl to be used for the screen contents.
*
*Exit:
*	bdVids and bdlVidData contain the information from the output
*	screen if there is room to store them.
*
*Exceptions:
*	None.
*******************************************************************************/
void near SaveUserScreen ()
{
    INST inst;
    WORD cbVids;

    if ((cbVids = CbSizeVids ()) == 0)
	return;						//[17]

    if (!BdAlloc (&bdVids, cbVids, IT_NO_OWNERS))
	return;						//[17]

    if (!FQueryInst (&inst, iModeCurrent))	/* [3] */
	DbAssert (FALSE);
    if (!FSaveVids ((VIDS *)bdVids.pb, &inst)) {	//[12]
	DbAssert (FALSE);
    }

    if (BdlAlloc (&bdlVidData, ((VIDS *)bdVids.pb)->cwVidData * sizeof (WORD)))	//[12]
	SaveVidData ((VIDS *)bdVids.pb, (WORD FAR *) MAKELONG (0, GETSEG (bdlVidData.seg)));	//[12]
    else
	// Must free, as FRestoreVids doesn't clear and leaves junk on screen.
	BdFree (&bdVids);

    if (bdVidsDebug.pb != NULL) 				// [6]
	if (!FRestoreVids ((VIDS *) bdVidsDebug.pb))		// [6]
	    DbAssert (FALSE);					// [6]
    
    SetBlinkBit(0);

}

/*** 
*void near RestoreUserScreen ()
*Purpose:
*	Restore the output screen from previously stored data in
*	SaveUserScreen.
*
*Entry:
*	bdVids		Global data pertaining to the mode and screen overhead.
*	bdVidData	Global data containing the screen contents.
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
void near RestoreUserScreen ()
{
    INST inst;
    WORD cbVids;

    BdFree (&bdVidsDebug);					// [6]
    if ((cbVids = CbSizeVids ()) == 0)				// [6]
	goto FailRestore;					// [6]
    if (!BdAlloc (&bdVidsDebug, cbVids, IT_NO_OWNERS))		// [6]
	goto FailRestore;					// [6]
    if (!FQueryInst (&inst, iModeCurrent))			// [6]
	DbAssert (FALSE);					// [6]
    if (!FSaveVids ((VIDS *) bdVidsDebug.pb, &inst))		// [6]
	DbAssert (FALSE);					// [6]

    if (bdVids.pb == NULL) {					//[12]
FailRestore:							// [6]
	Cls ();
	return;
    }

    if (!FRestoreVids ((VIDS *) bdVids.pb)) {			//[12]
	DbAssert (FALSE);
    }
    // [5] May not have data, but restored mode if possible.
    else if (bdlVidData.seg != NOT_OWNER) {
	RestoreVidData ((VIDS *)bdVids.pb,			//[12]
			(WORD FAR *) MAKELONG (0, GETSEG (bdlVidData.seg)));
    }

    TossUserScreen();
}

void near TossUserScreen ()
{
    BdFree (&bdVids);
    BdlFree (&bdlVidData);
}

void far EnsShowDebugScrFar ()
{
    EnsShowDebugScr ();
    FEnableMouse(TRUE);			//[17]
}

/*** 
*void near EnsShowDebugScr ()
*Purpose:
*	If the debug screen is not already visible, save the user screen.
*
*Entry:
*	None.
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
void near EnsShowDebugScr ()
{
    if (!fDebugScr) {
	/* Output screen is currently visible */
	fDebugScr = TRUE;
      
	/* put screen in User Interface compatible (text) mode.
	 * User's screen may have been in high or low-res graphics mode.
	 * Don't save user's screen image if memory is so low that
	 * there's not enough to execute a SYSTEM, CLEAR, or SETMEM statement.
	 * This is ensured by calling UiGrabSpace first.
	 */
	UiGrabSpace();
	SaveUserMouse();	//[17]
	EndMouse();		//[17] This will turn off the user's mouse so
				//[17] it won't get saved by SaveUserScreen
	SaveUserScreen ();
	UiReleaseSpace();
	FInitMouse();		//[17]
	DrawDebugScr ();	//[16]
    }
}

/**************************************************************************
* void near EnsShowOutSaveRs()
* Purpose:
*  If the output screen is not already
*  visible, switch  to saved  output screen mode and copy
*  the buffer saved by EnsShowDebugScr() to video ram.
*  Unlike EnsShowOutputScr, this function DOES NOT flush the edit manager's
*  buffer, and thus, perserves the caller's register set.
*
**************************************************************************/
void near EnsShowOutSaveRs()
{
    if (fDebugScr) {
	fDebugScr = FALSE;
	/* Restore the user screen */
	RestoreUserScreen ();
	RestoreUserMouse();	//[17]
    }
}

/**************************************************************************
* ShowOutScr
* Purpose:
*  called by PRINT, CALL executors that want to force the user's screen
*  to be restored.
*
**************************************************************************/
void far ShowOutScr ()
{
    EnsShowOutSaveRs ();
}

/**************************************************************************
* EnsMouseOff
* Purpose:
*  Called by runtime initialization code just before a call to B$RUNINI
*  to turn the mouse off so it won't leave a ghost cursor when it clears
*  the screen.
*
**************************************************************************/
void far EnsMouseOff ()
{
    FEnableMouse(FALSE);	//[17]
}

/**************************************************************************
* TossOutScreen()
* Purpose:
*  The runtime has changed screen modes while the debug screen was active.
*  We need to change state to be consistent with the output screen being active.
*  Output screen and mouse will be resaved by the next call to EnsShowDebugScr.
*
**************************************************************************/
void far TossOutputScreen ()
{
    TossUserScreen ();	/* release memory used to save output scr */
    fDebugScr = FALSE;
}

/**************************************************************************
* void near EnsShowOutputScr()
* Purpose:
*  If the output screen is not already
*  visible, switch  to saved  output screen mode and copy
*  the buffer saved by EnsShowDebugScr() to video ram.
*  Unlike EnsShowOutSaveRs, this function flushes the edit manager's
*  buffer, and thus, may alter caller's register set.
*
**************************************************************************/
void near EnsShowOutputScr ()
{
    EditMgrFlush ();
    /* so dirty buffer can never be written to user's screen by edit mgr */
    EnsShowOutSaveRs ();
}
