;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <assert.h>
#include <malloc.h>             /* for malloc() / free() */
#include <text.h>
#include <outofmem.hs>
#include <outofmem.sdm>

extern void OurUnHookISR9(void) ;
extern BOOL gfOurISR9Installed ;
extern BOOL gfStillInitializing; /* used in allocs to bail if we're gonna toast */
extern Shell_TTY_Out(char *str);

BOOL FAR PASCAL FDlgOutOfMem(WORD dlm,TMC tmc,WORD wNew,WORD wOld,WORD wParam)
{
	UnReferenced(tmc) ;
	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	if (dlm == dlmInit)
	{
		SetUpDialog(tmcoutofmementer, szCritWarning);
		SetUpButtonForGraphics(tmcoutofmementer);

		Shell_SetTmcText(tmcoutofmemmsg, szOutOfMemory) ;
		Shell_SetTmcText(tmcoutofmemmsg2,szOutOfMemory2);
		SetTmcText(tmcoutofmementer,szEnterButton);
	}
	return TRUE ;

} /* FDlgOutOfMem */

HCABOutOfMem gHcabOutOfMem;  // storage for the OutOfMem dialog box!

/* Tries to allocate memory for the OutOfMemory dialog box. If un-successful,
 * it puts up a CW message saying "Out Of Memory" -- an ugly box!! and then
 * returns FALSE. If succesful allocation, it initializes this chunk of mem
 * correctly and returns TRUE
 */
BOOL AllocateHcabForOutOfMem(void)
{
	gHcabOutOfMem = HcabAlloc(cabiCABOutOfMem);

	if (!gHcabOutOfMem)
		MessageBox(szShortOutOfMemoryTitle, NULL, NULL, MB_OK | MB_BEEP);
	else
		InitCab(gHcabOutOfMem, cabiCABOutOfMem) ;
	return (gHcabOutOfMem != NULL) ;
} /* AllocateHcabForOutOfMem */

extern char *szCurDialogCaption ;
extern BYTE ErrorCrit;

/* specifies whether we are processing an out of memory condition */
BOOL fProcessingOutOfMemory = FALSE ;

VOID FAR PASCAL OutOfMemory()
/*
  -- this is called on an out-of-memory condition
*/
{
	BYTE savecrit;
	char *saveDialogCaption ;

	/* sanity check -- can't call OutOfmemory() when already processing
	 * an Outofmemory() situation. ZZZZZZ Maybe I can do a NoMembailOut() call
	 * now!
	 */
	if (fProcessingOutOfMemory)
	{
		Beep() ; Beep() ;
		return ;
	}

	fProcessingOutOfMemory = TRUE ;

	DialogIsAlert(TRUE) ;

	/* Save the current dialog caption (szCurDialogCaption) as this
	 * OutOfMemory dialog box could pop up on top of another dialog.
	 *	MyTmcDoDlg() call below will change this string to the
	 * "Out Of Memory" caption and we need to restore it when we are done.
	 */
	saveDialogCaption = szCurDialogCaption ;

	/* Make it look like we are in the middle of a critical error handler.
	 * (i.e., assign to ErrorCrit a value other than 0xFF). This will
	 * ensure that we don't do our idle processing stuff while this
	 * OutOfMemory dialog box is put up.
	 * scottq uses the value 0x34 as the magic value and the same value
	 * is being used here for consistency.
	 */
	savecrit = ErrorCrit;
	ErrorCrit=0x34;
	// Run dlg box.
	MyTmcDoDlg(&dlgOutOfMem, gHcabOutOfMem);
	ErrorCrit=savecrit;

	szCurDialogCaption = saveDialogCaption ;

	DialogIsAlert(FALSE) ;

	fProcessingOutOfMemory = FALSE ;
} /* OutOfMemory */

/*
	Work Buffer Routines
  -- a near work buffer MUST be provided by the application
  -- This assumes a stack like allocation/freeing!!
*/

#define NEARBUFFERMAX   10000

BYTE rgbBuffer[NEARBUFFERMAX];
BYTE *pbBuffer = rgbBuffer;

/* This guarantees that we know the end of the near heap; */
BYTE *pbBufferEnd = rgbBuffer + NEARBUFFERMAX - 1; 

extern VOID NoMemBailOut(void) ;

VOID * FAR PASCAL PbAllocWork(WORD cb)
{
	BYTE *pbRet = pbBuffer;

	pbBuffer += cb+(cb&1); // always even
	if (pbBuffer > (pbBufferEnd+1))
	{
		OutOfMemory();
		// DoExit() ;
		NoMemBailOut() ;
	}
	return pbRet;
}

VOID FAR PASCAL FreeWork(VOID *pv)
{
	assert(pv != NULL) ;

	if(pv)
	   pbBuffer = pv;
}


extern VOID SetUpExitToDos(void); /* see init.c */

extern BOOL gfScreenModeChanged;
extern BOOL gfSwapHandlerInstalled;
extern WORD ReturnScreenMode;
extern void RemoveSwapHandler(void) ;

VOID NoMemBailOut(void)
{
	SetUpExitToDos();

	 /* Warning! can't make the following call after EndCow() etc as that guy will
	  * do an UnHookIS9() and the order in which we unhook will be messed
	  * up and the keyboard could lock up!!
	  */
	OurUnHookISR9() ;

	if (gfScreenModeChanged)
	{
		if (FQueryInst(&ginst, ReturnScreenMode))
		{
			FInitScreen(&ginst,0);
		}

		EndScreen(TRUE);
		EndCow(TRUE);
	}
	if (gfSwapHandlerInstalled)
		RemoveSwapHandler() ;


	Shell_TTY_Out(szBailOutNoMem);
	exit(-1);

}
/*
  -- allocation for screen saves, etc.
  -- NOTE : If this fails on screen saves, we have some screen refresh problems
*/
VOID FAR * FAR PASCAL LpbAllocWorkFar(WORD cb)
{
	WORD FAR *lpw;
	HANDLE hmem;

	if ((hmem = GlobalAlloc(0, (DWORD) (cb+2))) == NULL)
	{
		if(gfStillInitializing)
		{
			NoMemBailOut();

		}

		return NULL ;
	}
	lpw = (WORD FAR *) GlobalLock(hmem);
	*lpw++ = hmem;   /* save handle */
	return (lpw);
}

VOID FAR PASCAL FreeWorkFar(VOID FAR *lpb)
{
	HANDLE hmem = *(((WORD FAR *)lpb) - 1);

	/* ZZZZZ remove assert before shipping! */
	assert(lpb != NULL) ;

	if (lpb != NULL)
	{
		GlobalUnlock(hmem);                     /* not really needed */
		GlobalFree(hmem);
	}
}

/*
  -- fake local memory handler : for SDM
  -- in the future we may "fix" SDM so that near pointers will be used
	instead of handles
*/
VOID ** FAR PASCAL PpvAllocCb(WORD sb, WORD cb)
/*
  -- return handle to a memory block
*/
{
	WORD *pw;

	UnReferenced(sb) ;

	if ((pw = (WORD *) PbAllocWork(cb+(cb&1) + 2*sizeof(WORD))) != NULL)
		*pw = (WORD) (pw+2+((WORD)pw&1));
		*(pw+1) = (WORD) cb;

	return ((VOID **) pw);
}

VOID FAR PASCAL FreePpv(WORD sb, VOID **ppv)
{
	WORD *pw ;

	UnReferenced(sb) ;

	pw = (WORD *)ppv;
	FreeWork(pw);
}

/* The following is a define for CW...This may no longer be necessary?
 * BUG BUG
 */
unsigned long pwndCur;

WORD FAR PASCAL cbSizePpv(WORD sb, VOID **ppv)
{
	WORD *pw = (WORD *)ppv;

	UnReferenced(sb) ;
	return(*pw+1);
}

BOOL FAR PASCAL Fcheckhandle(WORD sb, VOID **ppv)
{
	UnReferenced(sb) ;
	UnReferenced(ppv) ;

    return(FALSE);
}

VOID ** FAR PASCAL FReAllocPpv(WORD sb, WORD **ppv, WORD size)
{
	VOID **tppv;

    FreePpv(sb,ppv);
    tppv = (PpvAllocCb(sb,size));
    if (tppv)
	{
	  strncpy((uchar *)*tppv, (uchar *) *ppv, size);
	  return(tppv);
    }
    else
       return(NULL);
}

/* BUG BUG this should no longer be necessary?
 */
VOID FAR PASCAL addchildhead()
{
}

/* ZZZZZZZ */
/* WARNING!! Major hack from hell to get past CW's problem. CW does not free
 * previously allocated unused graphics driver memory when we call
 * TermGraphics() and then set the screen mode to the new mode.
 *
 * Right now we are using the fact that the current version of the library
 * does 3 far allocs at start up irrespective of whether it is text
 * mode or graphics mode.
 * Any further allocs are specific to whether it is text/graphics mode
 * or whether it is CGA driver or any other one.
 * Later on, whenever we perform a screen mode change it allocates
 * more without freeing the previously allocated extra buffers.
 * We are also using the fact that the number of extra buffers it
 * requests is 1 or 2 far allocs -- no more than 2.
 * Thus at all times, CW should have a maximum of 5 far allocations (and 1
 * near allocation)! We store the last 2 far allocations that it should be
 * freeing on a screen mode change and get rid of them whenever we
 * try to change screen modes using fn SetScreenMode() in INIT.C
 */

/* Following two will have the handles of the screen driver allocated memories
 * that are special to graphics mode. "ghackDriverFarAllocCount" is also part
 * of the hack to free the unused memories.
 */
HANDLE gDriverMemHandle1 ;
HANDLE gDriverMemHandle2 ;
int ghackDriverFarAllocCount = 0 ;

/**** for DOSSHELL ***/
/* The memory this function allocates should be paragraph aligned */
/* This is the reason we don't call LpbAllocWorkFar!!             */
WORD FAR *PASCAL LpwAllocDriverMem(WORD cw,WORD fmem)
{
	   HANDLE hmem ;

	   if (fmem == fmemNear)
	   {
#ifdef DEBUG
			com1("near alloc of:") ;
			com1i(cw) ;
			com1("\n") ;
#endif

		   /* CW asks for 14 bytes or so of this memory just once!!
			* God knows why??
			*/
		  /* WARNING! This will be the first near memory allocation! That
		   * is, it will be invoked before any other calls to PbAllocWork
		   * this is important as we perform stack wise allocs & frees of
		   * near memory!
		   */
		  return((WORD FAR *)PbAllocWork(cw<<1)) ;
	   }

#ifdef DEBUG
	com1("far alloc of:") ;
	com1i(cw) ;
	com1("\n") ;
	Beep() ;
#endif

	ghackDriverFarAllocCount++ ;

#ifdef DEBUG
	/* CW should not be needing more than 5 far buffers for drivers!! */
	/* ZZZZZZZZ Remove before shipping */
	if (ghackDriverFarAllocCount > 5)
	{
		ShellMessageBox("Driver Error", "More far memory allocations requested by driver -- report to Microsoft") ;
		DoExit() ;
	}
#endif
	hmem = GlobalAlloc(0, (DWORD)cw<<1);

	if (ghackDriverFarAllocCount == 4)
		gDriverMemHandle1 = hmem ;
	else
	if (ghackDriverFarAllocCount == 5)
		gDriverMemHandle2 = hmem ;

	if(!hmem)
		NoMemBailOut();

	return((WORD FAR *)GlobalLock(hmem));
} /* LpwAllocDriverMem */

/* ZZZZZZZ This routine is never called by CW right now, as we don't use
 * FreeInstBuffers()!
 */
VOID FAR PASCAL FreeDriverMem(WORD FAR *lpw)
{
#ifdef KANJI
	if (HIWORD((WORD FAR * )&lpw) == HIWORD(lpw))
		FreeWork((WORD *)lpw);
	else
	{
		GlobalUnlock(HIWORD(lpw));              /* not really needed */
		GlobalFree(HIWORD(lpw));
	}
#else
	UnReferenced(lpw) ;

	/* We can't free the near memory that we may have given to CW as
	 * fn PbAllocWork() does a stack based alloc and we may not free
	 * in that manner. If we can figure out whether lpw is near or
	 * far memory, we can free it!!
	 */
	 /* Basically we don't free right now using this function!! */

	assert(TRUE) ;
#endif /* KANJI */

} /* FreeDriverMem */


/* Our version of function FreeInstBuffers() to free the extra far memory
 * allocations for screen modes (INST buffers)
 */
void OurHackFreeDriverMem(void)
{
	if (ghackDriverFarAllocCount < 4)
		return ;

	GlobalUnlock(gDriverMemHandle1);                /* not really needed */
	GlobalFree(gDriverMemHandle1);

	if (ghackDriverFarAllocCount == 5)
	{
		GlobalUnlock(gDriverMemHandle2);                /* not really needed */
		GlobalFree(gDriverMemHandle2);
	}
	
	ghackDriverFarAllocCount = 3 ;
} /* OurHackFreeDriverMem */
