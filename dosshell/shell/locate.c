;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>
#include <text.h>
#include <locate.hs>
#include <locate.sdm>

extern VOID MakeDirNameFit(char *longname,char *pattern,char *destination,int maxlen) ;

/* Storage to save the glob.MatchPat, before going into Search Mode. */
char gSavedMatchPat[NAMELEN+EXTLEN+1+1] ;

/* Storage to save glob.TreeMode, glob.SortKey before going into search mode */
int gSavedTreeMode ;
int gSavedSortOrder ;
WORD gSavedGlobalFocus ;

BOOL gfSearchDisk ; /* Whether to serch entire disk or not */

BOOL FAR PASCAL FDlglocate(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
	char fullparentpath[MAX_PATH+1] ;
	char parentname[60] ;
	PWND lwind ;
	int  itemwidth ;
	int dummylen ;

	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	if (dlm == dlmInit)
	{
		/* default is always to search entire disk! */
		SetTmcVal(tmcsearchfulldisk, TRUE);

		SetUpDialog(tmcOK,szLocateCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);
		SetUpButtonForGraphics(tmclocatehelp);
		SetUpEditBox(tmclocatestring, TRUE,NAMELEN+EXTLEN+1, TRUE);
		SetUpCheckBox(tmcsearchfulldisk) ;

		// FormPseudoPathName(parentname, listinfo[0].files, listinfo[0].tree) ;
		Tree2Path(listinfo[0].tree, listinfo[0].files, fullparentpath, &dummylen) ;
		
		lwind = PwndOfTmc(tmclocatedir);
		itemwidth = lwind->arcWindow.axRight - lwind->arcWindow.axLeft ;
		MakeDirNameFit(fullparentpath, NULL, parentname, itemwidth) ; 
		Shell_SetTmcText(tmclocatedir, parentname) ;
	} else if (dlm == dlmSetFocus)
	{
	    gCurrentTMC = tmc;
	} else if (dlm== dlmClick)
	{
		 if(tmc == tmclocatehelp)
			Help(hemDialog, hidLOCATE,NULL,0);

		 SetFocusTmc(gCurrentTMC) ;
	} else if ( (tmc == tmcOK) && (dlm == dlmTerm) )
	    gfSearchDisk = GetTmcVal(tmcsearchfulldisk) ;

	return(TRUE);
}

VOID FAR FileLocateBox()
{
	HCABlocate        h;
	char tstr[MAX_PATH+1];

	h = HcabAlloc(cabiCABlocate);
	if (!h)
	{
	    OutOfMemory() ;
	    return ;
	}
	InitCab(h, cabiCABlocate) ;

	/* The Search Pattern -- We initialize it with the current glob.MatchPat. */
	/* ZZZ should this be a NULL string? */
	SzToCab(h, glob.MatchPat, Iag(CABlocate, pszlocatestring));

	SzToCab(h, szEnterButton, Iag(CABlocate, pszlocateEB));
	SzToCab(h, szCancelButton, Iag(CABlocate, pszlocateCB));
	SzToCab(h, szHelpButton, Iag(CABlocate, pszlocateHB));

	if (MyTmcDoDlg(&dlglocate,h) == tmcOK)
	{
		/* We can't search tree if it is in compact mode */
		if (gfSearchDisk && (listinfo[0].tree->Compacted) )
		{
			OutOfMemory() ;
		}
		else
		{
			/* If we are not invoking Search from within Search save the original
			 * MatchPattern so that we can restore the FM state -- TR_SINGLE,
			 * TR_DOUBLE, TR_SYSTEM.
			 */
			if (glob.TreeMode != TR_SEARCH)
			strcpy(gSavedMatchPat, glob.MatchPat) ;

		SzFromCab(h, tstr, MAX_PATH, Iag(CABlocate, pszlocatestring));
		ScrunchFileName(glob.MatchPat, tstr, TRUE) ;

		/* ZZZZ Probably not needed if dialog box takes care of it! */
#ifdef DBCS
		DBCSstrupr(glob.MatchPat) ;
#else
		strupr(glob.MatchPat) ;
#endif

		strcpy(glob.MatchPat, MakePatWellFormed(glob.MatchPat)) ;
		MarkAllTreeMatches(listinfo[0].tree, FALSE) ;

			/* We do incremental sorting so it is safer to sort whole tree
			 * correctly now.
			 */
			SetUpTreeForSort(listinfo[0].tree) ;

		DoSearchDisplay(TRUE);
		}
	}

	FreeCab(h);
}
