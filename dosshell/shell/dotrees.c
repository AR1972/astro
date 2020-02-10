;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <text.h>
#include <menus.h>
#include <prot.h>

extern MENUINFO FileMgrMenuBar;
extern BOOL gTaskListEnabled;
extern BOOL gSwitchingEnabled;

extern BOOL ginitial ;
extern BOOL gNeedToUpdateFileList[2];

VOID InitStartProgramList(WORD top,WORD left,WORD bottom,WORD right) ;
VOID InitTaskMan(WORD top,WORD left,WORD bottom,WORD right);
VOID ListBoxPath(treehdr far *tree,PENTRY node,int whichtree);

VOID FAR DoSingleTree()
{
	AY lboxbottom;
	AY i; 					  /* junk int  */
	int temp ;

#ifndef NOCONSISTENCY
	/* The following check is now not needed as we can disable the
	   appropriate arrange menu item. */
	if (glob.InFileMgr && glob.TreeMode == TR_SINGLE && !ginitial)
	{
		printf("*** error -- DoSingleTree not disabled\n" ) ;
		exit (0) ;
	}
#endif

	TakeDownBusyDialog(0);
	TakeDownBusyDialog(1);

	/* ZZZZ This following statement needed only if we can get to 
	   this function from menu. Do we disable 'Arrange' options in Search-Mode?
	   If so, we can remove this statement. Similar case in DoDouble.., DoFlat..
	*/
	if (glob.TreeMode == TR_SEARCH)
		HandleSearchQuit() ;
	else
		/* Are we switching from system tree mode to this mode using menu? */
		if (glob.TreeMode == TR_SYSTEM && !ginitial)
			SetSelectedDirFocus() ; // set up so that tick mark is drawn right 

	temp = glob.TreeMode ;
	glob.TreeMode = TR_SINGLE ;

	easyenable(&FileMgrMenuBar, DoSingleTree, FALSE) ;

	/* If ginitial is false, this function has been invoked from menu! */
	if (!ginitial)
	{
		/* We are changing either from DoubleTree mode or System Tree Mode */ 
		if (temp == TR_SYSTEM)
		{
			easyenable(&FileMgrMenuBar, DoFlatDisplay, TRUE) ;
			easyenable(&FileMgrMenuBar, ShowInfo, TRUE) ;
		}
		else /* Changing from DoubleTree mode */
		{
			easyenable(&FileMgrMenuBar, DoDoubleTree, TRUE) ;
			/* ShowInfo is already turned on in DoubleTreeMode */
		}
	}
	/* else by default all menus are turned on at start up! */

	/* WARNING! Assumption: following will happen only when switching modes
	   from system tree mode. If (ginitial) then it will fail as then
	   (temp == TR_SINGLE) will be true */
	if ( (temp == TR_SYSTEM) && (glob.SortKey != SORT_DISK))
#ifdef OLDCOMPLETETREESORT
		FormDirectoryOrder(listinfo[0].tree, TRUE) ;
#else
{
		SetUpTreeForSort(listinfo[0].tree) ;
		SortDirectory(listinfo[0].tree, NULL) ;
		SortDirectory(listinfo[0].tree, listinfo[0].files) ;
}
#endif

	lboxbottom = ayMac - 1;

	// i = (AY) LISTBOXTOP + (glob.DriveCount - 1)/DRIVESPERLINE;
	i = (AY) LISTBOXTOP ;

	ListBoxInit(&TreeList[0],ListProcTreeList,&MainWind,i,0,lboxbottom,
				TREEBOXWIDTH,szDirectoryTree,tmcTreeList0,
				glob.lineselected[0],glob.lineselected[0]) ;
	gNeedToUpdateFileList[0] = FALSE;
	Always_Explicit(&TreeList[0]);
	ListBoxInit(&FileList[0],ListProcFileList,&MainWind,i,TREEBOXWIDTH,
				lboxbottom,TREEBOXWIDTH+FILEBOXWIDTH,
				glob.MatchPat,tmcFileList0,
				/* If tree mode has changed, use 0 for the focuses, else
				 * don't change focus -- pass in the magic value -1.
				 */
				-(temp == glob.TreeMode),-(temp == glob.TreeMode));
	if (Get_KeyWord_Assignment(TK_SAVESTATE,TK_EXPLICITSEL) ==
														   TK_ENABLED)
		Set_Explicit(&FileList[0]) ;

	SetUpScreen();
	FileMgrStatusBar(listinfo[0].tree,listinfo[0].files);
	
	listinfo[0].UpdateDrives = TRUE;
	
	glob.MaxTree = 0;							// one tree is active

	/* Was this routine called from the menu - that is did tree mode change? */
	if (	(temp != glob.TreeMode) ||
			(	(WhoHasGlobalFocus() != DRIVELIST0) &&
				(WhoHasGlobalFocus() != TREE0) &&
				(WhoHasGlobalFocus() != FILE0)
			)
		)
	{
		InitGlobalFocus(DRIVELIST0);
	}
	else
	{
		InitGlobalFocus(WhoHasGlobalFocus()) ;
	}

	/* Make sure files don't remain selected in other directories
	 * in the tree without the user being aware of it!
	 */
	if (	(	(temp == TR_SYSTEM) ||
				((temp == TR_DOUBLE) && (listinfo[0].tree == listinfo[1].tree))
			) &&
			!ginitial && !glob.CrossDirSel
		)
	{
		DeselectTree(listinfo[0].tree) ;
		listinfo[0].UpdateFiles = TRUE ;
	}

	// listinfo[0].UpdateFiles = TRUE ;

	ListBoxPath(listinfo[0].tree,listinfo[0].files,0);
	Set_KeyWord_Assignment(TK_SAVESTATE,TK_FILEMGRMODE,TK_SINGLETREE);

} /* proc DoSingleTree */

VOID FAR DoDoubleTree()
{
	AY size;					/* vertical size of list box	*/
	AY i;
	int temp ;
	int size2;

#ifndef NOCONSISTENCY
	if (glob.InFileMgr && glob.TreeMode == TR_DOUBLE && !ginitial)
	{
		printf("*** error -- DoDoubleTree not disabled\n" ) ;
		exit (0) ;
	}
#endif
	TakeDownBusyDialog(0);
	TakeDownBusyDialog(1);

	if (glob.TreeMode == TR_SEARCH)
		HandleSearchQuit() ;
	else
		/* Are we switching from system tree mode to this mode using menu? */
		if (glob.TreeMode == TR_SYSTEM && !ginitial)
		{
			SetSelectedDirFocus() ; // set up so that tick mark is drawn right

			/* If the tree in listbox 2 also is displaying the same tree,
			 * the fn SetSelectedDirFocus() could have expanded some
			 * directories and hence glob.lineselected[1] might not be
			 * correct now.
			 */
			if (listinfo[1].tree == listinfo[0].tree)
				glob.lineselected[1] =
						GetIndexVisibleDir(listinfo[1].files, listinfo[1].tree) ;
		}

	temp = glob.TreeMode ;
	glob.TreeMode = TR_DOUBLE ;

	easyenable(&FileMgrMenuBar, DoDoubleTree, FALSE) ;


	/* If ginitial is false, this function has been invoked from menu! */
	if (!ginitial)
	{
		/* We are changing either from DoubleTree mode or System Tree Mode */ 
		if (temp == TR_SYSTEM)
		{
			easyenable(&FileMgrMenuBar, DoFlatDisplay, TRUE) ;
			easyenable(&FileMgrMenuBar, ShowInfo, TRUE) ;
		}
		else /* Changing from SingleTree mode */
		{
			easyenable(&FileMgrMenuBar, DoSingleTree, TRUE) ;
			/* ShowInfo is already turned on in SingleTree mode */
		}
	}
	/* else by default all menus are turned on at start up! */


	/* WARNING! Assumption: following will happen only when switching modes
	   from system tree mode. If (ginitial) then it will fail as then
	   (temp == TR_DOUBLE) will be true */
	if ( (temp == TR_SYSTEM) && (glob.SortKey != SORT_DISK))
#ifdef OLDCOMPLETETREESORT
		FormDirectoryOrder(listinfo[0].tree, TRUE) ;
#else
	{
		SetUpTreeForSort(listinfo[0].tree) ;
		SortDirectory(listinfo[0].tree, NULL) ;
		SortDirectory(listinfo[0].tree, listinfo[0].files) ;
		if (	(listinfo[1].tree == listinfo[0].tree) &&
				(listinfo[1].files != listinfo[0].files)
			)
		{
			SortDirectory(listinfo[1].tree, listinfo[1].files) ;
		}
	}
#endif

	/* If SortRequired is TRUE, the tree has not been sorted even once, 	*/
	/* So, the tree is probably not yet read in completely. It will be sorted*/
	/*	in the list box code, just before displaying the files.				*/
	if ( (!ginitial) && (listinfo[1].tree != listinfo[0].tree) && 
				(!listinfo[1].tree->SortRequired) )
#ifdef OLDCOMPLETETREESORT
		FormDirectoryOrder(listinfo[1].tree, FALSE) ;
#else
	{
		/* The sort key could have been changed between the last time we
		 * sorted this tree and now. We are doing following for safety!
		 */
		SetUpTreeForSort(listinfo[1].tree) ;
		SortDirectory(listinfo[1].tree, NULL) ;
		SortDirectory(listinfo[1].tree, listinfo[1].files) ;
	}
#endif

	/* Compute max listbox size.
	*/

	// i = (AY) LISTBOXTOP + (glob.DriveCount - 1)/DRIVESPERLINE;
	i = (AY) LISTBOXTOP;
	size = (ayMac - i)/2 - 2;

	ListBoxInit(&TreeList[0], ListProcTreeList, &MainWind,
				i, 0, i+size, TREEBOXWIDTH, szDirectoryTree, tmcTreeList0,
				glob.lineselected[0],glob.lineselected[0]) ;
	gNeedToUpdateFileList[0] = FALSE;
	Always_Explicit(&TreeList[0]);

	ListBoxInit(&FileList[0], ListProcFileList, &MainWind,
				i, TREEBOXWIDTH, i+size, TREEBOXWIDTH+FILEBOXWIDTH,
				glob.MatchPat, tmcFileList0,
				/* If tree mode has changed, use 0 for the focuses, else
				 * don't change focus -- pass in the magic value -1.
				 */
				-(temp == glob.TreeMode),-(temp == glob.TreeMode));
	if (Get_KeyWord_Assignment(TK_SAVESTATE,TK_EXPLICITSEL) ==
														   TK_ENABLED)
		Set_Explicit(&FileList[0]) ;


	i += (AY) 4;
	size2 = size << 1;
	if (i+size2 >= ayMac)
	    --size2;

	ListBoxInit(&TreeList[1], ListProcTreeList, &MainWind,
				i+size, 0, i + size2,TREEBOXWIDTH, szDirectoryTree, tmcTreeList1,
				glob.lineselected[1],glob.lineselected[1]) ;
	gNeedToUpdateFileList[1] = FALSE;
	Always_Explicit(&TreeList[1]);

	ListBoxInit(&FileList[1], ListProcFileList, &MainWind,
				i+size, TREEBOXWIDTH, i+size2, TREEBOXWIDTH+FILEBOXWIDTH,
				glob.MatchPat, tmcFileList1,
				/* If tree mode has changed, use 0 for the focuses, else
				 * don't change focus -- pass in the magic value -1.
				 */
				-(temp == glob.TreeMode),-(temp == glob.TreeMode));

	if (Get_KeyWord_Assignment(TK_SAVESTATE,TK_EXPLICITSEL) ==
														   TK_ENABLED)
		Set_Explicit(&FileList[1]) ;


	/* Should really clear all listbox wnds, but they are all the same
	** right now.
	*/
    SetUpScreen();
    FileMgrStatusBar(listinfo[0].tree,listinfo[0].files);
	 ListBoxPath(listinfo[0].tree,listinfo[0].files,0);
	 ListBoxPath(listinfo[1].tree,listinfo[1].files,1);


	/* Cause DriveMouse to recalc where it thinks the drive icons are.
	*/
	listinfo[0].UpdateDrives = TRUE;
	listinfo[1].UpdateDrives = TRUE;

#ifdef DISPLAY_DIFFERENT_TREES_IF_POSS
	listinfo[1].tree = (glob.SelTree[1] != listinfo[0].tree) ? 
								glob.SelTree[1] : glob.SelTree[0] ;
	/* ZZZZZ we have to set the lineselected stuff right too */
#endif

	glob.MaxTree = 1;						/* both trees active		*/

	/* Was this routine called from the menu - that is did tree mode change? */
	if (temp != glob.TreeMode)
	{
		InitGlobalFocus(DRIVELIST0);
	}
	else
	{
		InitGlobalFocus(WhoHasGlobalFocus()) ;
	}

	/* If changing from System Tree mode, deselect tree. Else files may remain
	 * selected in other directories without the user being aware of it!
	 */
	if (temp == TR_SYSTEM && !ginitial && !glob.CrossDirSel)
	{
		DeselectTree(listinfo[0].tree) ;
		listinfo[0].UpdateFiles = TRUE ;
		listinfo[1].UpdateFiles = TRUE ;
	}

	// listinfo[0].UpdateFiles = listinfo[1].UpdateFiles = TRUE ;


	Set_KeyWord_Assignment(TK_SAVESTATE,TK_FILEMGRMODE,TK_TWOTREE);

} /* proc DoDoubleTree */

BOOL gfFlatLeftInfoDrawn = FALSE ;

VOID FAR DoFlatDisplay()
{
	AY lboxbottom;
	AY i; 					  /* junk int						  */
	int temp ;

#ifndef NOCONSISTENCY
	if (glob.InFileMgr && glob.TreeMode == TR_SYSTEM && !ginitial)
	{
		printf("*** error -- DoFlatDisplay not disabled\n" ) ;
		exit (0) ;
	}
#endif

	gfFlatLeftInfoDrawn = FALSE ;

	TakeDownBusyDialog(0);
	TakeDownBusyDialog(1);

	/* If tree is in compact mode, we can't go into System Tree Mode! */
	if (listinfo[0].tree->Compacted)
	{
		OutOfMemory() ;
		return ;
	}

	if (glob.TreeMode == TR_SEARCH)
		HandleSearchQuit() ;

	temp = glob.TreeMode ;
	glob.TreeMode = TR_SYSTEM ;

	easyenable(&FileMgrMenuBar, DoFlatDisplay, FALSE) ;

	/* ShowInfo shouldn't be enabled in System Tree Mode!! */
	easyenable(&FileMgrMenuBar, ShowInfo, FALSE) ;

	if (!ginitial)
	{
		if (temp == TR_DOUBLE)
			easyenable(&FileMgrMenuBar, DoDoubleTree, TRUE) ;
		else
			easyenable(&FileMgrMenuBar, DoSingleTree, TRUE) ;
	}
	/* else all menus are turned on by default */

	/* We don't want to start off at initial stage. Tree will get sorted
	   as soon as the whole tree is read in -- see filemgr.c! */
	if (!ginitial)
		SystemSort(listinfo[0].tree, FALSE) ;


	lboxbottom = ayMac - 1 ;

	// i = (AY) LISTBOXTOP+ (glob.DriveCount - 1)/DRIVESPERLINE;
	i = (AY) LISTBOXTOP;

	/* The TreeList is initialized here because the DriveMouse routine uses
	   the TreeList[0]....rect.aytop, etc to draw drives */

	ListBoxInit(&TreeList[0],ListProcTreeList,&MainWind,i,0,lboxbottom,
				TREEBOXWIDTH,szDirectoryTree,tmcTreeList0,0,0);
	gNeedToUpdateFileList[0] = FALSE;
	Always_Explicit(&TreeList[0]);

	ListBoxHalt(&TreeList[0]) ;
	ListBoxInit(&FileList[0],ListProcFileList,&MainWind,i,FLATLEFTWIDTH,
				lboxbottom,FLATLEFTWIDTH+FLATRIGHTWIDTH,
				glob.MatchPat,tmcFileList0,
				/* If tree mode has changed, use 0 for the focuses, else
				 * don't change focus -- pass in the magic value -1.
				 */
				-(temp == glob.TreeMode),-(temp == glob.TreeMode));

	if (Get_KeyWord_Assignment(TK_SAVESTATE,TK_EXPLICITSEL) ==
														   TK_ENABLED)
		Set_Explicit(&FileList[0]) ;


	SetUpScreen();

	listinfo[0].UpdateDrives = TRUE;
	// listinfo[1].UpdateDrives = TRUE;
	
	glob.MaxTree = 0;							// one tree is active

	/* Was this routine called from the menu - that is did tree mode change? */
	if (	(temp != glob.TreeMode) ||
			(	(WhoHasGlobalFocus() != DRIVELIST0) &&
				(WhoHasGlobalFocus() != FILE0)
			)
		)
	{
		InitGlobalFocus(DRIVELIST0);
	}
	else
	{
		InitGlobalFocus(WhoHasGlobalFocus()) ;
	}


	Set_KeyWord_Assignment(TK_SAVESTATE,TK_FILEMGRMODE,TK_SYSTEMTREE);

	// listinfo[0].UpdateFiles = TRUE ;

	/* The directory with the focus -- in system tree mode! */
	listinfo[0].files = 
		(listinfo[0].tree->FirstFile) ? 
						FindParent(listinfo[0].tree->FirstFile) : NULL ;
	/* The directory line with check mark will by default be OK!! */


} /* proc DoFlatDisplay */

extern VOID FAR DoAssociateBox();

VOID FAR DoSearchDisplay(BOOL fInitialize)
{
	AY lboxbottom;
	AY lboxtop;

	/*
	 * be sure to draw all icons the first time
	 * through!
	 */
	if(gisgraph)
	{
	    InitIconCache();
	}

	/* This function can be invoked from within SearchMode itself if
	 * the user chose a Repaint Screen in the Search Mode display.
	 */
	if (glob.TreeMode != TR_SEARCH)
	{
		/* Save tree mode, sort key so we can restore the filemgr state
		 * we exit the search mode.
		 */
		gSavedTreeMode = glob.TreeMode ;
		gSavedSortOrder = glob.SortKey ;
		gSavedGlobalFocus = WhoHasGlobalFocus() ;
	}

	/* The Sort routine uses the value in glob.SortKey, glob.TreeMode */
	/* We want to give the user the directory order -- sorted by name */
	glob.TreeMode = TR_SINGLE ;
	SortCmp = SortFnArr[glob.SortKey = SORT_NAME] ;

	FormDirectoryOrder(listinfo[0].tree, FALSE) ;

	/* Clear all selections before entering Search Mode -- 1 reason: Items	*/
	/* like 'view' etc can't be reached if > 1 file selected and the user	*/
	/* once into Search Tree Mode can't operate on (ex: deselect) files not	*/
	/* shown in Search Window.												*/
	if ((fInitialize) && (listinfo[0].tree->NumSel > 0))
		DeselectTree(listinfo[0].tree) ;

	/* We have cheated the sort routine. Now set the TreeMode to TR_SEARCH, so
		that the listbox code can handle the file line display correctly */
	glob.TreeMode = TR_SEARCH ;

	glob.MaxTree = 0 ;

	/* In System Tree mode showinfo is disabled, so enable it now! */ 
	if (gSavedTreeMode == TR_SYSTEM)
		easyenable(&FileMgrMenuBar, ShowInfo, TRUE) ;
		
	/* Make some of the menu-items non-reachable in Search Mode */
	EnableDisableForSearchMode(FALSE, FALSE) ;

	lboxbottom = ayMac - 1 ;
	lboxtop = 3 ;

	if (fInitialize)
	{
		/* The TreeList is initialized here because the DriveMouse routine uses
	    * the TreeList[0]....rect.aytop, etc to draw drives.
		 */
		ListBoxInit(&TreeList[0],ListProcTreeList,&MainWind,lboxtop,0,
						lboxbottom,TREEBOXWIDTH,szDirectoryTree,tmcTreeList0,
						glob.lineselected[0], glob.lineselected[0]);
		gNeedToUpdateFileList[0] = FALSE;
		Always_Explicit(&TreeList[0]);

		ListBoxHalt(&TreeList[0]) ;

		/* Reusing FileList[0] for SearchFor ListBox! */
		strcpy(szSearchLboxTitle+SEARCH_PAT_IND, glob.MatchPat) ;
		ListBoxInit(&FileList[0],ListProcFileList,&MainWind,lboxtop,
				SEARCHLEFT, lboxbottom,SEARCHLEFT+SEARCHWIDTH, szSearchLboxTitle,
				0,0,0);

		listinfo[0].UpdateFiles = TRUE ;
	}
	else
	{
		ListBoxInit(&FileList[0],ListProcFileList,&MainWind,lboxtop,
				SEARCHLEFT, lboxbottom,SEARCHLEFT+SEARCHWIDTH, szSearchLboxTitle,
				/* Use -1, -1 to leave focus in the same location as before! */
				0,-1,-1);
	}

	/* Clear the listbox area to give it a clean slate!! */
	/* Actually Get_List_Rect gives an absolute rect struct, but it is OK
		in our case, as abs/relative rectangles are isomorphic in structure */
	FillRrc(&MainWind,(PRRC)&(Get_List_Rect(&FileList[0])),' ',isaBackground);

	InitGlobalFocus(FILE0);

	if (!fInitialize)
		UpdateListBox(&FileList[0]) ;

	MessageBar(szSearchMessage, isaMenu,TRUE) ;

} /* proc DoSearchDisplay */

/* Are all files in the whole tree that match MatchPat in a selected state */
BOOL AreAllTreeMatchedSelected(PTREE tree)
{
	PENTRY temp ;

	for (temp = tree->FirstFile ; temp ; temp = temp->x.f.snext)
		if ( (temp->MATCHESPATTERN) && (!(temp->SELECTED)) )
				return (FALSE) ;
	return (TRUE) ;
} /*  AreAllTreeMatchedSelected */

/* Are all files in 'dir' of tree that match MatchPat in a selected state */
BOOL AreAllDirMatchedSelected(PENTRY dir, PTREE tree)
{
	PENTRY temp ;

	/* dir == NULL implies root directory */
	temp = dir ? GetFirstDirFile(dir) : GetFirstRootFile(tree) ;

	if (!temp)
		return TRUE ; /* No files in directory -- trivial case! */

	while (TRUE)
	{
		/* temp is guaranteed to be non-null within this loop! */
		if ( (temp->MATCHESPATTERN) && (!(temp->SELECTED)) )
			return FALSE ;

		if (!temp->LASTDIRFILE)	
			temp = temp->x.f.snext ;
		else
			return TRUE ;
	}
} /*  AreAllDirMatchedSelected */

/* Are all files in the file list box with focus in a selected state -- This
*  is used to determine whether to disable/enable the menu item "Select All" */
AreAllMatchedSelected(PENTRY dir, PTREE tree)
{
	if ( (glob.TreeMode == TR_SYSTEM) || (glob.TreeMode == TR_SEARCH) )
		return AreAllTreeMatchedSelected(tree) ;
	else
		return AreAllDirMatchedSelected(dir, tree) ;
}


/* Returns TRUE iff tree being displayed has been fully read in. This
*  is used to determine if the menu should be disabled in the normal manner
*  (based on files selected) or whether to disable everything except the
*	'exit' file menu (and possibly enter single tree mode?).
*/
BOOL IsTreeBeingBuilt(void)
{
	/* ZZZZ We let the user do anything only if tree 0 has been fully built */
	/* Because changing to/from system-tree mode, search mode, etc operate  */
	/* on tree 0 (i.e., the one in the top list box).						*/
	/* The only restriction this causes is in double tree mode when the tree*/
	/* in box 1 has been read in but the one in box 0 is still being read in */
	/* The user will not be allowed to perform operations in this case.		*/
	return  (listinfo[0].tree->ContinueTree) ;
} /* IsTreeBeingBuilt */

#if 0
/* !!!!!!!!!	     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
/** ZZZZ following functions are unused!! */
unsigned CountTreeSelMatFiles(PTREE tree)
{
	unsigned count ;
	PENTRY temp ;
	
	if (!tree)
		return 0 ;

	for (temp = tree->FirstFile, count = 0 ; temp ; temp = temp->x.f.snext)
		if ((temp->MATCHESPATTERN) && (temp->SELECTED))
			count++ ;

	return count;
}  /* CountTreeSelMatFiles */

unsigned CountDirSelMatFiles(PENTRY dir, PTREE tree)
{
	unsigned count ;
	PENTRY temp ;
	
	/* dir == NULL implies that we want to count files in the root directory */
	temp = dir ? GetFirstDirFile(dir) : GetFirstRootFile(tree) ;

	if (!temp)
		return 0;

	count = ( (temp->MATCHESPATTERN) && (temp->SELECTED) ) ? 1 : 0 ;
	
	while (!temp->LASTDIRFILE)
	{
		temp = temp->x.f.snext ;
		if ( (temp->MATCHESPATTERN) && (temp->SELECTED) )
			count++ ;
	}

	return count;
}  /* CountDirSelMatFiles */
#endif



#ifndef NOGROUPINFILE
VOID FAR DoShareMode(void)
{
	AY size;					/* vertical size of list box	*/
	AY i;
	int temp ;
	int size2;

	TakeDownBusyDialog(0);
	TakeDownBusyDialog(1);

	if (glob.TreeMode == TR_SEARCH)
		HandleSearchQuit() ;
	else
		/* Are we switching from system tree mode to this mode using menu? */
		if (glob.TreeMode == TR_SYSTEM && !ginitial)
			SetSelectedDirFocus() ; // set up so that tick mark is drawn right 
	temp = glob.TreeMode ;
	glob.TreeMode = TR_SHARE;

	easyenable(&FileMgrMenuBar, DoDoubleTree, TRUE) ;
	easyenable(&FileMgrMenuBar, DoSingleTree, TRUE) ;
	easyenable(&FileMgrMenuBar, DoFlatDisplay, TRUE) ;

	/* If ginitial is false, this function has been invoked from menu! */
	if (!ginitial)
	{
		/* If we are changing from System Tree Mode re-enable ShowInfo menu */ 
		if (temp == TR_SYSTEM)
		{
			easyenable(&FileMgrMenuBar, ShowInfo, TRUE) ;
		}
	}
	/* else by default all menus are turned on at start up! */

	/* WARNING! Assumption: following will happen only when switching modes
	   from system tree mode. If (ginitial) then it will fail as then
	   (temp == TR_DOUBLE) will be true */
	if ( (temp == TR_SYSTEM) && (glob.SortKey != SORT_DISK))
#ifdef OLDCOMPLETETREESORT
		FormDirectoryOrder(listinfo[0].tree, TRUE) ;
#else
	{
		SetUpTreeForSort(listinfo[0].tree) ;
		SortDirectory(listinfo[0].tree, NULL) ;
		SortDirectory(listinfo[0].tree, listinfo[0].files) ;
	}
#endif

	/* Compute max listbox size.
	*/

	i = (AY) LISTBOXTOP;
	size = (ayMac - i)/2;

	ListBoxInit(&TreeList[0], ListProcTreeList, &MainWind,
				i, 0, i+size, TREEBOXWIDTH, szDirectoryTree, tmcTreeList0,
				glob.lineselected[0],glob.lineselected[0]) ;
	gNeedToUpdateFileList[0] = FALSE;
	Always_Explicit(&TreeList[0]);

	ListBoxInit(&FileList[0], ListProcFileList, &MainWind,
				i, TREEBOXWIDTH, i+size, TREEBOXWIDTH+FILEBOXWIDTH,
				glob.MatchPat, tmcFileList0,
				/* If tree mode has changed, use 0 for the focuses, else
				 * don't change focus -- pass in the magic value -1.
				 */
				-(temp == glob.TreeMode),-(temp == glob.TreeMode));

	if (Get_KeyWord_Assignment(TK_SAVESTATE,TK_EXPLICITSEL) ==
			 TK_ENABLED)
	{
		Set_Explicit(&FileList[0]) ;
	}

	i += (AY) 1;
	size2 = size << 1;
	if (i+size2 >= ayMac)
	    size2 = ayMac-i-1;
#ifndef NOSWITCHER
#define TASKADJ2 4
   if(gTaskListEnabled)
	{
		/*
		 * Use axmac-40 so the task list will line up with file list,
		 * even in 90 col
	    */
      InitStartProgramList(i+size,0,i+size2,axMac-40-TASKADJ2);
      InitTaskMan(i+size,axMac-40-TASKADJ2,i+size2,axMac);
	}
	else
#endif
	{
	   InitStartProgramList(i+size,0,i+size2,axMac);
	}
   SetUpScreen();
   FileMgrStatusBar(listinfo[0].tree,listinfo[0].files);
	ListBoxPath(listinfo[0].tree,listinfo[0].files,0);

   /* Cause DriveMouse to recalc where it thinks the drive icons are.
   */
   listinfo[0].UpdateDrives = TRUE;

   glob.MaxTree = 0;

	/* Was this routine called from the menu - that is did tree mode change? */
	if (	(temp != glob.TreeMode) ||
			(	(WhoHasGlobalFocus() != DRIVELIST0) &&
				(WhoHasGlobalFocus() != TREE0) &&
				(WhoHasGlobalFocus() != FILE0) &&
				(WhoHasGlobalFocus() != GROUPBOX) &&
				(WhoHasGlobalFocus() != TASKBOX)
			)
		)
	{
		InitGlobalFocus(DRIVELIST0);
	}
	else
	{
		InitGlobalFocus(WhoHasGlobalFocus()) ;
	}

	/* Make sure files don't remain selected in other directories
	 * in the tree without the user being aware of it!
	 */
	if (	(	(temp == TR_SYSTEM) ||
				((temp == TR_DOUBLE) && (listinfo[0].tree == listinfo[1].tree))
			) &&
			!ginitial && !glob.CrossDirSel
		)
	{
		DeselectTree(listinfo[0].tree) ;
		listinfo[0].UpdateFiles = TRUE ;
	}

   // listinfo[0].UpdateFiles = TRUE ;
   Set_KeyWord_Assignment(TK_SAVESTATE,TK_FILEMGRMODE,TK_SHARED);

} /* proc DoShareMode */
#endif
