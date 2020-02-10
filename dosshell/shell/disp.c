;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/* Changing display options -- SORT Order or PATTERN should affect all
  trees and not just the trees being displayed!! So we might want to mark
  this information with each tree. Initialize this info in InitFileMgr too*/

#include <ctype.h>
#include "common.h"
#include "menus.h"
#include "filemgr.h"
#include "prot.h"
#include "dispopt.hs"
#include "dispopt.sdm"
#include "text.h"

BOOL pat_change = FALSE;
BOOL disp_hidden_change = FALSE;

extern VOID ListBoxPath(treehdr far *tree,PENTRY node,int whichtree) ;
extern void HandleEmptyFileListBoxes(void) ;


VOID DisplayOptions(VOID)
{
	HCABdispopt	  h;
	char tempname[MAX_PATH+1] ;
	TMC tmc ;

	h = HcabAlloc(cabiCABdispopt);

	if (!h)
	{
		OutOfMemory() ;
		return ;
	}
	InitCab(h, cabiCABdispopt) ;
	/* print the current file filter options here */
	SzToCab(h, glob.MatchPat, Iag(CABdispopt, pszdisppattern));

	SzToCab(h, szEnterButton, Iag(CABdispopt, pszdispEB));
	SzToCab(h, szCancelButton, Iag(CABdispopt, pszdispCB));
	SzToCab(h, szHelpButton, Iag(CABdispopt, pszdispHB));

	if ( (tmc = MyTmcDoDlg(&dlgdispopt, h)) == tmcCancel)
		return ;

	if (pat_change || disp_hidden_change)
	{
	    /* pattern changed -- so take care of all trees --
	       This routine only handles the tree(s) being displayed
	       in the listbox(es)!
	    */
	    SzFromCab(h,tempname,MAX_PATH,Iag(CABdispopt, pszdisppattern));

		/* ZZZ order of MakePatWellFormed, Scrunching not OK?? */
		ScrunchFileName(glob.MatchPat, tempname, TRUE) ;

		/* ZZZZ Probably not needed if dialog box takes care of it! */
#ifdef DBCS
		DBCSstrupr(glob.MatchPat) ;
#else
		strupr(glob.MatchPat) ;
#endif

		strcpy(glob.MatchPat, MakePatWellFormed(glob.MatchPat)) ;

		if (MarkAllTreeMatches(listinfo[0].tree, disp_hidden_change))
		{
			/* When Display Spec. changes, according to the spec from
			 * ericst, we want to deselect all tree files.
			 */
			DeselectTree(listinfo[0].tree) ;

			/* MatchPattern has changed, Set new tile for File listbox0 */
			listinfo[0].UpdateFiles = TRUE ;

			/* New and improved title code! (see lbox.c) */
			ListBoxPath(listinfo[0].tree,listinfo[0].files,0);
			if (glob.MaxTree == 1) 
			{
				if (listinfo[1].tree != listinfo[0].tree)
				{
					/* When Display Spec. changes, according to the spec from
					 * ericst, we want to deselect all tree files.
					 */
					DeselectTree(listinfo[1].tree) ;

					/* Note: If pat has changed for tree0 it has for tree1 too
						as all displayed trees are marked on same pattern */
					MarkAllTreeMatches(listinfo[1].tree, TRUE) ;
				}
				
				/* MatchPattern has changed, Set new title for File listbox1 */
				listinfo[1].UpdateFiles = TRUE ;
				ListBoxPath(listinfo[1].tree,listinfo[1].files,1);
			}
			HandleEmptyFileListBoxes() ;
		}
		/* else pattern hasn't changed -- file listbox(es) needn't be redrawn */
	}
	/* else pattern is same -- File listbox(es) needn't be redrawn */
	FreeCab(h);

}

BOOL FAR PASCAL FDlgdispopt(WORD dlm, TMC tmc, WORD wNew,
										WORD wOld, WORD wParam)
{
	int t ;
	int t2 ;

	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;
	switch (dlm) {
	case dlmInit:
	{
		SetTmcVal(tmcgroupname, glob.SortKey) ;
		SetTmcVal(tmcDisplayHiddenFiles, glob.DisplayHiddenFiles) ;
		SetTmcVal(tmcDescendingOrder, (gDescendingOrder == MAGIC_XCHG_MASK)) ;

		SetUpDialog(tmcOK,szDisplayOptionsCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);
		SetUpButtonForGraphics(tmcdisphelp);
		SetUpEditBox(tmcdisppattern, TRUE,NAMELEN+EXTLEN+1, TRUE);
		SetUpCheckBox(tmcDisplayHiddenFiles) ;
		SetUpCheckBox(tmcDescendingOrder) ;

		if (gisgraph)
		{
			/* pass in also the TMCs of the two items that need to get focus
			 * when using Shift+TAB and TAB respectively!
			 */
		   SetUpRadioGroupForGraphics(tmcgroupname, tmcDescendingOrder, tmcOK);

		   SetUpRadiobuttonForGraphics(tmcradioname,0);
		   SetUpRadiobuttonForGraphics(tmcradioext,1);
		   SetUpRadiobuttonForGraphics(tmcradiodate,2);
		   SetUpRadiobuttonForGraphics(tmcradiosize,3);
		   SetUpRadiobuttonForGraphics(tmsradiodisk,4);
		}

		pat_change = FALSE ;
		disp_hidden_change = FALSE ;
		SetFocusTmc(tmcdisppattern) ;
	}
		break;

	case dlmTerm:
		switch(tmc) {
		case tmcOK :
			/* If pattern or sort order has changed, the file list box	*/
			/* will, in general, change. So, we need to trash the 		*/
			/* File Icon cache.											*/

		   t = GetTmcVal(tmcgroupname) ;
			t2 = GetTmcVal(tmcDescendingOrder) ;

			/* Set our sort order flag to the mask value to indicate sort in
			 * descending order in case this check box is checked!
			 */
			if (t2)
				t2 = MAGIC_XCHG_MASK ;

			if ( (t != glob.SortKey) || (t2 != gDescendingOrder) )
		    {
				InitIconCache() ;
				
				/* store sort order flag -- used by the sort routines */
				gDescendingOrder = t2 ; 

				SortCmp = SortFnArr[glob.SortKey = t] ;
				if (glob.TreeMode == TR_SYSTEM)
					SystemSort(listinfo[0].tree, TRUE) ;
				else
				{
					/* Actually we need to only sort the directories we
					 * are viewing in the file listboxes & not entire tree
					 * and mark that directory as being sorted and mark all
					 * other directories as being unsorted.
					 * This will speed up the sort tremendously.
					 */
#ifdef OLDCOMPLETETREESORT
					FormDirectoryOrder(listinfo[0].tree, TRUE) ;
#else
					SetUpTreeForSort(listinfo[0].tree) ;
					SortDirectory(listinfo[0].tree, NULL) ;
					SortDirectory(listinfo[0].tree, listinfo[0].files) ;
#endif
					/* Check to see if we are in doubletree mode & the tree
						being displayed in second listbox is not the same as
						the one in the first one */
					if ( (glob.MaxTree == 1) && 
								(listinfo[1].tree != listinfo[0].tree) )
					{
#ifdef OLDCOMPLETETREESORT
						FormDirectoryOrder(listinfo[1].tree, TRUE) ;
#else
						SetUpTreeForSort(listinfo[1].tree) ;
						SortDirectory(listinfo[1].tree, NULL) ;
						SortDirectory(listinfo[1].tree, listinfo[1].files) ;
#endif
						listinfo[1].UpdateFiles = TRUE ;
					}
				} /* Non System Ordering */

				/* System mode or Singletree mode do update files in box 0 */
				listinfo[0].UpdateFiles = TRUE ;
			}

			/* ZZZZZ Should I look at tree's DisplayHiddenFiles bit? */
			if (glob.DisplayHiddenFiles != GetTmcVal(tmcDisplayHiddenFiles))
			{
				glob.DisplayHiddenFiles = !glob.DisplayHiddenFiles ;
				disp_hidden_change = TRUE ;
			}
		   break;

		case tmcCancel :
		    pat_change = FALSE ;
			 disp_hidden_change = FALSE ;
		    break;

		}
		break ;

	case dlmChange:
		pat_change = TRUE ;
		break ;

	case dlmSetFocus:
		gCurrentTMC = tmc;
		break ;

	case dlmClick:
		 if(tmc == tmcdisphelp)
			Help(hemDialog, ((PDLG)&dlgdispopt)->hid, NULL, 0);

		 SetFocusTmc(gCurrentTMC) ;
		 break ;

	default: /* do nothing */ ;
	}
	return(TRUE);
}

extern void ResetTreeMatchedFilesOpt(PTREE tree) ;
extern void MarkMatchesBit(PENTRY fil) ;

/* returns TRUE iff the tree's file nodes were re-checked for pat matches! */
BOOL MarkAllTreeMatches(PTREE tree, BOOL doit)
{
	PENTRY fil ;
	char temp[NAMELEN+EXTLEN+2] ;
	BOOL fOldTreeDisplay ;

/* ZZZZZZ We don't  handle trees not yet completely read
	in as the files are not displayed until whole tree is read in!! */
	if (tree->ContinueTree)
		return FALSE ;

/* If speed is desired these two strcpy's could be removed!! Doing the
	following makes the tree->mpat be syntactically equal to MatchPat
	rather than just semantically */

	strfcpy(temp, tree->mpat) ;
	strfcpy(tree->mpat, glob.MatchPat) ;
	fOldTreeDisplay = tree->DisplayHiddenFiles ;
	tree->DisplayHiddenFiles = glob.DisplayHiddenFiles ;

	if (doit || (!PatternsEquiv(temp, glob.MatchPat)) || 
		         (fOldTreeDisplay != glob.DisplayHiddenFiles) )
	{
		for ( fil= tree->FirstFile ; fil ; fil = fil->x.f.snext)
		{
			MarkMatchesBit(fil) ;
		}

		ResetTreeMatchedFilesOpt(tree) ;
		return TRUE ;
	}
	/* else patterns equivalent -- so markings are correct as it is */
	return FALSE ;
}


/* ZZZ Function could be made more space efficient! */
char *MakePatWellFormed(char *pat)
{
	char *p ;

	/* Change null pattern into "*.*" */
	if (!*pat)
		return "*.*" ;
	else
	{
		/* there is at least 1 non-null character in the pattern */
		/* remove all trailing white spaces */
		p = strchr(pat, '\0') - 1 ; /* point to last non-null char */
		while ( isspace(*p) && (p != pat) )
			p-- ;

		if (isspace(*p)) /* pattern has only white spaces */
			return "*.*" ;
		else
		{
			*(p+1) = '\0' ;

			/* Now strip all leading white spaces -- we know pattern has
				atleast one non-white character !! */
			p = pat ;
			while (isspace(*p))
				p++ ;

			return p ;
		}
	}
} /*  MakePatWellFormed */

/* ZZZZ Returns whether pattern pat1 is equivalent to pat2 logically.
	Actually it is supposed to say that "*" equivalent to "*.*" but
	now it checks for syntax equivalence rather than semantics. Both pat1
	and pat2 are assumed to be null terminated */
BOOL PatternsEquiv(char far *pat1, char far *pat2)
{
	while (*(pat1++) == *(pat2++)) 
	{
		if (!*(pat1-1)) /* We have reached the NULL character in both */
			return TRUE ;
	}

	return FALSE ; /* patterns not equivalent */
} /* PatternsEquiv */
