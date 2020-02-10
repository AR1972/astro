/*** 
*auxcow.c - CW auxiliary code
*
*	Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*	Support code for CW.
*
*******************************************************************************/

#include <version.h>

/* Next, include COW's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>
#include <cw/color.h>
#include <uiext.h>

/* and the dialog headers */
#include <cw/dlg.h>

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

#define UE_COLORS FALSE 	//[16] set this switch to get color sets
				//[16] for user ed screen shots.

/* these must be exported */
VOID		FAR PASCAL OutOfMemory(void);
VOID *		FAR PASCAL PbAllocWork(WORD);
VOID		FAR PASCAL FreeWork(VOID *);

/* Fake LMEM */
VOID **		FAR PASCAL PpvAllocCb(WORD, WORD);
VOID		FAR PASCAL FreePpv(WORD, VOID **);

VOID FARPUBLIC UpdateShiftKk (WORD, WORD);
VOID FARPUBLIC UpdateShiftKj (WORD, WORD);	// [44]
DWORD FAR PASCAL GlobalHandle (HANDLE);
HANDLE FAR PASCAL GlobalRealloc (HANDLE, DWORD, WORD);
BOOL FAR PASCAL PlayBackMsg(MSG *);
void FAR PASCAL RecordMsg(MSG *);
void NEAR PASCAL DumpScreen(void);
VOID FAR PASCAL BackToCow (WORD);	// [22]


VOID FAR PASCAL ShrinkHelp (VOID);	//[41]
char cGlobalAlloc = 0;			//[41] Net # BDL's GlobalAlloc allocated

BYTE fPlayBack = FALSE, fRecord = FALSE;
BYTE fPlaybackInit = TRUE;	//[41]
BYTE fBdlInit = FALSE;		//[41]

extern BOOL fCapLock, fNumLock;
extern boolean fAccelCmd;
extern BYTE fMono;		//[26] Must have own monochrome flag

extern AX axMouse;		// [45]
extern AY ayMouse;		// [45]

extern SA PASCAL rgsa[];

/*** 
*VOID FARPUBLIC UpdateShiftKk (kkNew, kkOld)
*Purpose:
*	Set Userinterface flags for CAPLOCK and NUMLOCK shift states so that
*	DrawToggles reflects this status properly, and make sure that
*	SCRLOCK changes get a synthesize character to make playback/record
*	screen dumps work properly.
*
*Entry:
*	kkNew		New KK_ shift states.
*	kkOld		Old KK_ shift states.
*
*	fCapLock	flag telling current user interface CAPLOCK status.
*	fNumLock	flag telling current user interface NUMLOCK status.
*
*Exit:
*	fCapLock and fNumLock are set to reflect the shift states in kkNew.
*
*Exceptions:
*	None.
*******************************************************************************/
VOID FARPUBLIC
UpdateShiftKk (kkNew, kkOld)
WORD kkNew, kkOld;
{
    fCapLock = kkNew & KK_CAPLOCK;
    fNumLock = kkNew & KK_NUMLOCK;
    DrawToggles ();

    /* [2] Synthesize VK_SCRLOCK if the shift state changes, so
     * [2] we get screen dumps properly
     */
    kkNew &= KK_SCRLOCK;	/* [4] */
    kkOld &= KK_SCRLOCK;	/* [4] */
    if (kkNew ^ kkOld)
	SynthesizeShiftKeys (kkNew, kkOld);
}


/* [5] Global variable defining the mode we start up in */
WORD iModeCurrent = 0;


/*** 
*void NEAR CwInit ()
*Purpose:
*	Set up screen mode for CW, and initialize window structure.
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
void NEAR CwInit ()
{
    REG1 short i;
    AY ay = 0;
    INST inst;
    extern BYTE fWaitRetrace;	// [23]

    ayMouse = 0;		//[45]
    axMouse = 0;		//[45]

    iModeCurrent = ImodeGuessCurrent ();	/* [5] */

// [12] If the mode is unknown, or we have a flag to get the highest possible
// [12] mode, then loop until we find it.
    if (cmdSwitches & CMD_SW_HIR) {					// [12]
	for (i = 0; FQueryInst (&inst, i); i++) {			// [12]
	    if (inst.finst & finstAvailable) {
		if ((inst.ayMac > ay) ||				// [20]
		    (inst.ayMac == ay && !(inst.finst & finstMonochrome))) { // [20]
		    ay = inst.ayMac;					// [12]
		    iModeCurrent = i;					// [12]
		}
	    }								// [12]
	}								// [12]
    }									// [12]
    if (iModeCurrent == imodeUnknown) { 				// [20]
	ay = 255;		// [26] Set it to be MAXay at start
	for (i = 0; FQueryInst (&inst, i); i++) {			// [20]
	    if (inst.finst & finstAvailable) {				// [20]
		if ((inst.ayMac < ay) ||				// [20]
		    (inst.ayMac == ay && !(inst.finst & finstMonochrome))) { // [20]
		    ay = inst.ayMac;					// [20]
		    iModeCurrent = i;					// [20]
		}							// [20]
	    }								// [20]
	}								// [20]
    }									// [20]
    DbAssert (iModeCurrent != imodeUnknown);

    EnableVidsMonitor (TRUE);	// [38] [39]
    SaveUserScreen ();
    if (!FQueryInst (&inst, iModeCurrent))
	DbAssert (FALSE);
    if (!FInitScreen (&inst))
	DbAssert (FALSE);
    
    DrawDebugScr ();
    fDebugScr = TRUE;

    if ((inst.finst & finstQuestionable) &&	// [23]
	!(cmdSwitches & CMD_SW_GLT))		// [23]
	fWaitRetrace = TRUE;			// [23]

	fInitColorTable ();		// [20]
    // [26] This must follow CwReInit (so fMonochrome is set)
    fMono = (BYTE) (fMonochrome || (cmdSwitches & CMD_SW_MNO));

    // [47] Check for /Editor switch
    if (cmdSwitches & CMD_SW_ED) {
        extern char szWildDefault[];
        extern short iMsgStatusLine;

	strcpy(szWildDefault, "*"); // Default is *.TXT for QEDIT
        iMsgStatusLine = MSG_StatusQEdit;
    }
    WndInit ();
}

/*** 
*void NEAR CwHook ()
*Purpose:
*	Make sure that CW is hooked back in.  This is called when we re-enter
*	the user interface.
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
void NEAR CwHook ()
{
    BackToCow (TRUE);		// [22]
    EnableKeyboard(TRUE);	// [40]Enable keyboard
}

/*** 
*void NEAR CwUnHook ()
*Purpose:
*	Make sure that CW is unhooked.	This is called when we exit the user
*	interface.
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
void NEAR CwUnHook ()
{
    LeaveCow (FALSE);
}

/*** 
*void NEAR CwTerm ()
*Purpose:
*	Terminate CW.  This is called when we are ending the program, or when
*	we shell out.
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
void NEAR CwTerm ()
{
    EnableVidsMonitor (FALSE);	// [38]
    LeaveCow (FALSE);
}

/*** 
*VOID FAR PASCAL OutOfMemory ()
*Purpose:
*	This is called by CW to indicate an out of memory condition.
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
VOID FAR PASCAL
OutOfMemory()
{
    SetUiErrOm ();
}


#define MAX_LOCAL	10 + 2		//[41] Options/Paths + Slop

bd bdlocal[MAX_LOCAL];


/*** 
*VOID ** FAR PASCAL PpvAllocCb (sb, cb)
*Purpose:
*	Return handle to a near memory allocation.
*
*Entry:
*	sb		Unused parameter.
*	cb		Number of bytes to allocate.
*
*	bdlocal is an array of bd's to use to allocate the near memory.
*
*Exit:
*	Returns pointer to bd's pointer to data.
*
*Exceptions:
*	None.
*******************************************************************************/
VOID ** FAR PASCAL
PpvAllocCb (sb, cb)
WORD sb;     /* ignored */
WORD cb;	/* size of block */
{
    bd *pbd = NULL;
    int i;

    Unreferenced (sb);

    for (i = 0; i < MAX_LOCAL; i++) {
	if (bdlocal[i].pb == NULL) {
	    pbd = &bdlocal[i];
	    break;
	}
    }
    DbAssert (pbd != NULL);	//[4] NOTE: when you get this, bump MAX_LOCAL
    if (!BdAlloc (pbd, cb, IT_NO_OWNERS))
	return (NULL);

    return (&(pbd->pb));
}



/*** 
*VOID FAR PASCAL FreePpv (sb, ppv)
*Purpose:
*	Free a handle to near memory.
*
*Entry:
*	sb		Unused parameter.
*	ppv		Pointer to pointer to memory in bd.
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
VOID FAR PASCAL
FreePpv (sb, ppv)
/*
  -- free local block (return NULL)
*/
WORD sb;
VOID ** ppv;
{
    bd *pbd = (bd *) (ppv - 1);

    Unreferenced (sb);

    BdFree (pbd);
}


	//[41] If we don't want to have to shrink help down, we must
	//[41] increase MAX_GLOBAL to have 18 extra bdl's for help slop
	//[41] instead of the 11 we have given it.  Doing the compression
	//[41] saves 56 bytes of DGROUP that isn't really needed.
#define MAX_GLOBAL	(3 * 2) + 3 + 3 + 9 + 2		//[41]
			//[41] 3 list boxes at a time @ 2 allocs/box
			//[41] + 3 help file names
			//[41] + 3 help engine FDB's
			//[41] + 9 more for help system (can't always compress)
			//[41] + 2 slop

bdl bdlglobal[MAX_GLOBAL];

/*** 
*HANDLE FAR PASCAL GlobalAlloc (flags, cb)
*Purpose:
*	Allocate a handle to far memory.
*
*	This doesn't return a REAL handle, but GlobalHandle is called to
*	get the segment, so we do the translation ourselves.
*
*Entry:
*	flags		Unused parameter.
*	cb		Number of bytes to allocate.
*
*Exit:
*	Return pointer to bdl's seg.
*
*Exceptions:
*	None.
*******************************************************************************/
HANDLE FAR PASCAL
GlobalAlloc (flags, cb)
WORD flags;
DWORD cb;
{
    int i;
    bdl *pbdl = NULL;

    Unreferenced (flags);

    if (!fBdlInit) {			//[41]
	fBdlInit = TRUE;		//[41]
	for (i = 0; i < MAX_GLOBAL; i++) {
	    bdlglobal[i].seg = NOT_OWNER;
	}
    }
    if (cGlobalAlloc >= MAX_GLOBAL-2)	//[41] if getting close to being full
	ShrinkHelp();			//[41] reduce # of help entries
	//[41] The 2 above is the magic # of items that HelpDecomp allocates
	//[41] Since ShrinkHelp doesn't do anything when called from HelpDecomp,
	//[41] we need to have 2 handles in reserve.

    for (i = 0; i < MAX_GLOBAL; i++) {
	if (bdlglobal[i].seg == NOT_OWNER) {
	    pbdl = &bdlglobal[i];
	    break;
	}
    }
    DbAssert (pbdl != NULL);	//[4] NOTE: when you get this, bump MAX_GLOBAL
    if (!BdlAlloc (pbdl, (WORD) cb))
	return (NULL);

    cGlobalAlloc++;			//[41] one more allocated
    return ((HANDLE) &pbdl->seg);
}

/*** 
*HANDLE FAR PASCAL GlobalFree (hmem)
*Purpose:
*	Free a handle to far memory.
*
*Entry:
*	hmem		Handle to far memory.
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
HANDLE FAR PASCAL
GlobalFree (hmem)
HANDLE hmem;
{
    REG1 WORD *h = (WORD *) ((WORD) hmem);
    bdl *pbdl = (bdl *) (h - 1);

    BdlFree (pbdl);

    cGlobalAlloc--;			//[41] one less allocated
    DbAssert (cGlobalAlloc >= 0)	//[41] GlobalFree w/o GlobalAlloc

    return ((HANDLE) NULL);
}


/*** 
*DWORD FAR PASCAL GlobalHandle (hmem)
*Purpose:
*	Called by CW to convert a handle into a DWORD, each WORD of which
*	contains the handle's segment.
*
*Entry:
*	hmem		Handle to far memory.
*
*Exit:
*	Return handle's segment.
*
*Exceptions:
*	None.
*******************************************************************************/
DWORD FAR PASCAL
GlobalHandle (hmem)
HANDLE hmem;
{
    REG1 WORD *h = (WORD *) ((WORD) hmem);
    return ((DWORD) MAKELONG (GETSEG(*h), GETSEG(*h)));
}

/*** 
*HANDLE FAR PASCAL GlobalRealloc (hmem, cb, flags)
*Purpose:
*	Realloc a handle to far memory.
*
*Entry:
*	hmem		Handle to far memory.
*	cb		Number of bytes to reallocate to.
*	flags		Unused parameter.
*
*Exit:
*	Returns reallocated handle.
*
*Exceptions:
*	None.
*******************************************************************************/
HANDLE FAR PASCAL
GlobalRealloc (hmem, cb, flags)
HANDLE hmem;
DWORD cb;
WORD flags;
{
    REG1 WORD *h = (WORD *) ((WORD) hmem);
    bdl *pbdl = (bdl *) (h - 1);

    Unreferenced (flags);

    if (!BdlRealloc (pbdl, (WORD) cb))
	return (NULL);
    return ((HANDLE) &pbdl->seg);
}

/*** 
*VOID NEAR PASCAL DumpScreen ()
*Purpose:
*	Dump the entire screen to disk.
*
*Entry:
*	Description of each formal parameter.
*	Any important global variables used as parameters (implied parameters).
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
VOID NEAR PASCAL
DumpScreen ()
{
    register AY ay;
    WORD pwBuff[80];
    static WORD fdScreen = UNDEFINED;
    static char header[4] = { 2, 0, 0 };
    RRC rrc;

    rrc.rxLeft = 0;
    rrc.rxRight = axMac;
    if (fdScreen == UNDEFINED) {
	fdScreen = CreateFile("screen");
	if (fdScreen == UNDEFINED)
	    return;
	header[3] = ayMac;
	WriteFile (fdScreen, header, 4 );
    }

    for (ay=0; ay < ayMac; ay++) {
	rrc.ryTop = ay;
	rrc.ryBottom = ay+1;
	DbAssert (CwSizeRrc (&rrc) <= 80);
	SaveRrc (NULL, &rrc, (BYTE *) pwBuff);
	WriteFile (fdScreen, (char *) pwBuff, 80 * sizeof (WORD));
    }

    /* Make sure buffer gets flushed right away */
    FlushFile (fdScreen);
}


/*** 
*BOOL FAR PASCAL PlayBackMsg (pmsg)
*Purpose:
*	Try to read input message from playback file.
*
*Entry:
*	pmsg		pointer to structure to contain message read in.
*
*	fRecord 	global flag is TRUE if QB was started with the /rcd
*			flag, and we haven't opened any playback file.
*	fPlayback	global flag is TRUE if we have opened a playback file.
*	fPlaybackInit	static flag is TRUE if we have tried to open a
*			playback file.
*	fdPlayBack	static file descriptor for PlayBackMsg.
*Exit:
*	Returns TRUE if there was a message to be read, otherwise FALSE.
*
*Exceptions:
*	None.
*******************************************************************************/
BOOL FAR PASCAL
PlayBackMsg (pmsg)
MSG *pmsg;
{
    static WORD fdPlayBack = UNDEFINED;

    if (fPlaybackInit) {			//[41]
	fPlaybackInit = FALSE;			//[41]
	if (fRecord) {
	    fdPlayBack = OpenFile ("playback");
	    if (fdPlayBack != UNDEFINED) {
		fPlayBack = TRUE;
		fRecord = FALSE;
	    }
	}
    }

    if (fPlayBack) {
	if (ReadFile (fdPlayBack, (char *) pmsg, sizeof (MSG)) == sizeof (MSG)) {
	    pmsg->time = ClockTicks ();
	    /* [2] Check for VK_SCRLOCK in PlayBackMsg as well as RecordMsg */
	    if (pmsg->message == WM_CHAR && pmsg->wParam == VK_SCRLOCK) {
		DumpScreen ();
	    }
	}
	else {
	    fPlayBack = FALSE;
	    CloseFileNear (fdPlayBack); //[24]
	}
    }

    return (fPlayBack);
}

#define FKeyMsg(x) (((x) >= WM_KEYFIRST) && ((x) <= WM_KEYLAST))
#define FMouseMsg(x) (((x) >= WM_MOUSEFIRST) && ((x) <= WM_MOUSELAST))

/*** 
*VOID NEAR PASCAL RecordMsg (pmsg)
*Purpose:
*	Write the message in pmsg to the record file.
*
*Entry:
*	pmsg		pmsg is a pointer to a message structure to save in
*			the record file.
*
*	fRecord 	global flag is TRUE if QB was started with the /rcd
*			flag, and we haven't opened any playback file.
*	fdRecord	static file descriptor for the record file.
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
void FAR PASCAL
RecordMsg(pmsg)
MSG *pmsg;
{
    static WORD fdRecord = UNDEFINED;

    if (fRecord) {
	if (fdRecord == 0xffff)
	    fdRecord = CreateFile ("record");
	if (fdRecord != 0xffff &&
	    (FMouseMsg (pmsg->message) || FKeyMsg (pmsg->message))) {
		/* [2] VK_OEM_SCROLL -> VK_SCRLOCK */
		if (pmsg->message == WM_CHAR && pmsg->wParam == VK_SCRLOCK) {
		    DumpScreen ();
		}
		WriteFile( fdRecord, (char *) pmsg, sizeof(MSG) );
		/* Make sure buffer gets flushed right away */
		FlushFile (fdRecord);
	}
    }
}
