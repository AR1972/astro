;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****   lbox.c - routines directly supporting the File Mgr list boxes
**
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  7/13/89   t-jeffro  Header added to file.
**  7/17/89   t-jeffro  Replaced sprintf with individual calls.
**  7/19/89   t-jeffro  Selection in both list boxes is stable.  Installed
**                                              hack to make icons display properly.
**  7/22/89   scottq    made some optimizations to makefileline
**  10/?/89   harikris  Implemented System Tree mode.
**  10/?/89   harikris  changed all GetCount & GetNthFile routines.
**  11/?/89   harikris  wrote DisplayNoFilesMsg function
**  12/6/89   harikris  rewrote the two ListProc routines.
**
*/
#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <icons.h>
#include <text.h>

/* INTERNATIONALIZE HERE! */
/* Indexes into the string that displays the info about the file */
#define NAMESTART       5
#define EXTSTART        14
#define SIZESTART       17
#define SIZESIZE        12
#define DATESTART       32
#define TIMESTART       (DATESTART+8+2)

#define ICONCACHEY (MAXCOLS * MAXROWS)
char far *gIconCache = NULL; /* pointer to array for short circuit draw icon */

extern BOOL gfManipulating ;

BOOL gNeedToUpdateFileList[2];

VOID Arrow(void);
extern BOOL GetLastKeyState(void);
extern BOOL FIsExecutableFile(char far *ext) ;

VOID ProgramCursor(void);

/*
 * Drawing Icons is really slow. So we never want to draw an icon
 * over itself while the user is scrolling a list box.
 * We remember what icon was last drawn at a location, and do not
 * draw it if it is already there. This will require that
 * some places invalidate the cache.
 */
void InitIconCache(void)
{
	register int j;
	if(gisgraph)
	{
	   if(gIconCache == NULL)
	   {
	   gIconCache = LpbAllocWorkFar(ICONCACHEY);
	   }
	   if(gIconCache == NULL)
	   return;
	   for(j=0;j<ICONCACHEY;j++)
	   {
		 gIconCache[j] = -1;
	   }
	}
} /* InitIconCache */

/*  Invalidate the cache entries in the rectangle specified by the 4 co-ords */
void InvalidateIconCache(AX axLeft, AY ayTop, AX axRight, AY ayBottom)
{
	AX x ;
	AY y ;

	if (gIconCache)
	{
		/* y-loop is the outer one and x-loop is the inner one for opt! */
		for (y = ayTop ; y <= ayBottom ; y++)
			for (x = axLeft ; x <= axRight ; x++)
				gIconCache[y*MAXCOLS+x] = -1 ;
	}
	/* else IconCache memory not allocated -- so can't update! */
} /* InvalidateIconCache */

#define ICONLEN 2                       // CheckMark icon len!
#define COLL_ICON_LEN   4       // Dir Icon len including the ' ' after it!

/* we use "-2" in following #define so that we discount the 2 cols lost to the
*       scroll bar to the right of the tree list box!
*/
#define TREELINELEN (TREEBOXWIDTH-ICONLEN-2)

#define FOCUS_CHAR      chRightArrow

/* The x-coord where the FOCUS MARKER character for the root dircetory will
 * be drawn.
 */
#define MAGICROOTX      1

/*  Draws the focus marker -- 
 *  In graphics mode:
 *              A rectangular box 1 char wide and 'len' long at rxGraphics, ryGraphics
 *  In text mode:
 *              FOCUS_CHAR at rxText, ryText in the isa color!
 *  isfocus, issel specify whether the line in question has focus, selection.
 */
void DrawFocusMarker(PWND pwnd, RX rxText, RY ryText, RX rxGraphics,
					RY ryGraphics, WORD len, BOOL isfocus, BOOL issel, ISA isa)
{
	WORD fore1,back1,fore2,back2;
	UnReferenced(issel) ;

	if (isfocus)
	{
		if (!gisgraph)
		{
			CharOut(pwnd, rxText, ryText, FOCUS_CHAR, isa);

			/* The focus marker character is being drawn at a different column
			 * for the root directory than for other directory lines, so
			 * we need to erase old character.
			 */
			if (rxText != MAGICROOTX)
				CharOut(pwnd, (RX) MAGICROOTX, ryText, ' ', isa);
		}
		else
		{
			// if (!issel) // WE DONT CARE IF IT IS SELECTED ANYMORE

			if(isfocus)
			{
				GetIsaColor(isa,&fore1,&back1);
				GetIsaColor(isaHilite,&fore2,&back2);
				if(fore1 == back2)
				{
					isa = (ISA) isaSelect;
				}
							
			}
				FrameCharRect(ryGraphics, rxGraphics,
					  ryGraphics+1, rxGraphics+len, 1,isa) ;
		}
	}
	else
		/* In Text mode, we need to erase any focus char left behind */
		if (!gisgraph)
			CharOut(pwnd, rxText, ryText, ' ', isa);
} /* DrawFocusMarker */

/* 'dirnode' is a directory node belonging to tree 'tree'. This routine
*  returns the character to be drawn on the screen in front of dir name.
*/
unsigned char GetCollapseChar(PTREE tree, PENTRY dirnode)
{
	unsigned char ch ;

	/* Note that tree->DirCount & tree->VisibleDirCount don't include the root
	 *  directory.
	 */
	if (!dirnode)
	{
		if (tree->DirCount == 0)
			ch = NOZOOM_CHAR ;
		else if (tree->VisibleDirCount == 0)
			ch = EXPANSION_CHAR ;
		else
			ch = COLLAPSE_CHAR ;
	}
	else
	{
		if (!dirnode->HASSUBDIRS)
			ch = NOZOOM_CHAR ;
		else if (dirnode->EXPANDED)
			ch = COLLAPSE_CHAR ;
		else
			ch = EXPANSION_CHAR ;                   
	}

	return ch ;
} /* GetCollapseChar */

/* Draw the collapsible/non-collapsible marker here! 'ch' is the character
*  that specifies whether a dir can be expanded, collapsed, etc.
*/
void DrawDirIcon(PWND pwd, RX x, RY y, char ch, ISA isa)
{
	BITMAP *pbitmap ;

	// A blank character at the end of the icon is needed both in graphics
	//  and text mode.
	CharOut(pwd, x+3, y, ' ', isa);
	InvalidateIconCache(x+3, y, x+3, y) ;
	
	if(gIconCache && (gIconCache[y*MAXCOLS+x] == ch))
		return;

	if (gisgraph)
	{
		switch (ch) 
		{
			case EXPANSION_CHAR :
				pbitmap = DirExpansionIcon ;
				break ;
			case COLLAPSE_CHAR:
				pbitmap = DirCollapseIcon ;
				break ;
			default:
				pbitmap = DirNoZoomIcon ;
		}
			
		PlotBmp(pbitmap, x*CWIDTH, y*CHEIGHT,isaBackground);
		if(gIconCache)
		{
			gIconCache[y*MAXCOLS+x] = ch;
		}

	}
	else
	{
		CharOut(pwd, x, y, COLLAPSE_LCHAR, isa);
		CharOut(pwd, x+1, y, ch, isa);
		CharOut(pwd, x+2, y, COLLAPSE_RCHAR, isa);
	}
} /* DrawDirIcon */

/* returns BOOL to indicate whether or not the mouse click which occured
*   at relative x-coordinate 'rx' corresponds to a click on the collapse
*   icon.
*/
BOOL FIconClick(PENTRY dirnode, int x)
{
	int xIconCenter ;
	int namelen ;
	char dirname[NAMELEN+EXTLEN+2] ;

	/* ZZZZZZ These magic numbers 5, 3, etc are used only in this routine
	 * So, we don't need #defines and they need to be changed only in this
	 * routine.
	 */
	if (!dirnode)
		xIconCenter = 3 ;
	else
	{
		/* We indent to the right by 3 for each additional level! */
		xIconCenter = (dirnode->dtlx.lx.level + 1) * 3 ;

		/* Given current setup, we can have 5 levels of directories without
		* clipping (dir list box is 35 cols wide now!). If it is different
		* we need to fix this routine.
		*/
		/* Could clipping have happened? */
		if (dirnode->dtlx.lx.level > 5)
		{
			namelen = Internal2Normal(dirname, dirnode->name) ;
			
			// The +2 is for the distance between Icon center and the dir name
			// Check if clipping actually happened!
			if ( (xIconCenter+2+namelen) > (ICONLEN+TREELINELEN-1) )
			{
				// Set XIconCenter so that above Boolean will be ==
				xIconCenter = ICONLEN+TREELINELEN-1-namelen-2 ;
			}
		}
	}
	return ( (x >= xIconCenter - 1) && (x <= xIconCenter+1) ) ;
} /* FIconClick */

extern VOID MakeDirNameFit(char *longname,char *pattern,char *destination,int maxlen);
extern WORD gCnx;
/* 
 * called by disp.c and below to set up the path title in the file boxes
 */
VOID ListBoxPath(treehdr far *tree,PENTRY node,int whichtree)
{
	char pathstring[MAX_PATH+1];
	int dummylen ;
	/* titles are always less than 1/2 the screen, max 90 */
#define MAXFILETITLE 45
	static char filetitlebuf[MAXFILETITLE*2];
	char *filetitle = &filetitlebuf[(whichtree) * MAXFILETITLE];

	if (glob.TreeMode != TR_SYSTEM)
	{
		Tree2Path(tree, node, pathstring, &dummylen);
		MakeDirNameFit(pathstring,glob.MatchPat,filetitle,
									Get_List_Rect(&FileList[whichtree]).axRight -
										Get_List_Rect(&FileList[whichtree]).axLeft - 2);
	}
	else
	{
		/* In System Tree mode, we only display the MatchPat as listbox title */
		strcpy(filetitle, glob.MatchPat) ;
	}

	if(gCnx == cnxNull)
		SetTitle(&FileList[whichtree], filetitle) ;
}

BOOL    gEricStraubsShiftWish = FALSE; //shift+key makes tree update now
/****   ListProcTreeList - list proc for directory tree list box
**      This fn does all the normal things a listboxproc does.
**
**      ENTRY
**              tmc -   0 for upper (or only) list box
**                              1 for lower list box
**              tmm - message type
**              isz - listbox line number (for tmmSetFocus, tmmDrawItem)
**              x, y- absolute character position of the mouse click.
**              sz  - unused by this proc
**              bArg- flags, not used by this proc
**      EXIT
**              If tmm == tmmCount, then number of directories on the disc.
**              Otherwise, 0??.
**      EFFECTS:
**              if cross-directory selection is off, all the files in the old
**      directory will be deselected (unless they are also displayed in the other
**      file listbox).
*/
WORD PASCAL ListProcTreeList(WORD tmm, char *sz, WORD isz, TMC tmc, 
										WORD x, WORD y, WORD bArg)
{
	PWND pwnd;                      // window of the list box
	treehdr far *tree;              // which tree are we dealing with
	PENTRY dirnode;                 // new displayed directory
	PENTRY olddirnode;              // old displayed directory
	BOOL   fCollExp ;       // specifies whether a dir was expanded/collapsed!

	UnReferenced(sz) ;

	tree = listinfo[tmc].tree;

	/* Don't show tree list until the tree is completely read in.
	 * Sometimes, like in System Tree mode, we will have our listproc
	 * called even before we start reading the tree. 
	 */
	if ( (!tree->Started) || (tree->ContinueTree) )
		return 0 ; /* Not ready to draw -- say num of dirs is 0 */

	/* Even in SYSTEM_TREE mode, we do a ListBoxInit() on TreeListBox. At
	 * that time this proc may get called. So, do nothing in this case.
	 */
	if (glob.TreeMode == TR_SYSTEM)
		return(0) ; /* junk value */

	olddirnode = listinfo[tmc].files ;

	pwnd = Get_List_Window(&TreeList[tmc]);

	switch (tmm) {
		case tmmCount:
			// return tree->DirCount+1;
			return tree->VisibleDirCount+1;
#ifndef NODIRECT
#if 0
//we dont do this. too bad
		case tmmPickUp:
			 ProgramCursor();
			 break;
		case tmmDrop:
			 Arrow();
			 break;
#endif
#endif
		case tmmGetItemString:
			dirnode = (isz) ? GetNthVisibleDir(tree, isz) : NULL;
			if (dirnode)
				Internal2Normal(sz, dirnode->name) ;
			else {
				/* hack to get to root directory! on typing '\\' */
				/* INTERNATIONALIZE HERE!!!!! */
				*sz = '\\' ;
				*(sz+1) = '\0' ;
			}
			break ;

		case tmmActivate:
			/* treat tmmActivate's like tmmSelect -- Bug report didn't want
			 * us to open directories on "enter". To be windows compatible,
			 * we don't open directories on mouse double-click or enter.
			 * Now directories can be opened or closed only by using '+', '-'
			 * or mouse click on that icon!
			 */
			tmm = tmmSelect ;
		case tmmToggleSelect:
		case tmmSelect:

			/* Ignore select messages when one is direct manipulating */
			if (gfManipulating)
				return 0 ; // garbage

			if(sz==(char *)ISRANGESELECT)
			{
				gNeedToUpdateFileList[tmc]=TRUE;
				gEricStraubsShiftWish = TRUE; // shift causes immediate update
				break;
			}

			gNeedToUpdateFileList[tmc]=FALSE;

			if (isz > tree->VisibleDirCount)
			{
				return (0) ; /* garbage value */
			}

			/* Change directories and update the file listbox.  */
			dirnode = (isz) ? GetNthVisibleDir(tree, isz) : NULL;

			fCollExp = FALSE ;

			/* Did the user click on the icon portion of the dir line? */
			if ( (tmm == tmmActivate) || (FIconClick(dirnode, x)) )
			{
				// We want to treat a select on Collapse icon as tmmActivate.
				tmm = tmmActivate ; 
				switch(GetCollapseChar(tree, dirnode))
				{
					case EXPANSION_CHAR:
						/* Expand 1 level of dirnode's visibility. */
						ExpandDirVisibility(tree, dirnode, FALSE) ;
						fCollExp = TRUE ;
						break ;
					case COLLAPSE_CHAR:
						CollapseDirVisibility(tree, dirnode) ;
						fCollExp = TRUE ;
						break ;
					default:
						// Shell_Beep() ; /* can't expand/collapse! */
						break ;
				}
			}

			/* InsertListItem is "Asynchronous" -- meaning no drawing
				done until back in Idle Loop */
			InsertListItem(&TreeList[tmc], min(isz, glob.lineselected[tmc]));

			/* Update tree in other listbox too if we have collapsed or
			 * expanded a directory in the same tree.
			 */
			if (fCollExp && (glob.TreeMode == TR_DOUBLE) &&
											(tree == listinfo[1-tmc].tree) )
				InsertListItem(&TreeList[1-tmc], 0);

			glob.lineselected[tmc] = tree->SelLine = isz;

			// glob.FocusBox = tmc ; /* being done elsewhere */

			/* Unselect files in the old directory if cross-dir selection
			 * is not enabled and we need to do so!
			 */
			if ( (!glob.CrossDirSel) && (tree->NumSel > 0) &&
															(dirnode != olddirnode) )
			{
				// redisplay files in other listbox as we are looking to 
				// deselect files in the directory being displayed there.
				if ( (glob.TreeMode == TR_DOUBLE) && 
					 (listinfo[1-tmc].tree == tree) &&
					 (listinfo[1-tmc].files == olddirnode) )
				{
					listinfo[1-tmc].UpdateFiles = TRUE ;
				}

				DeselectDir(olddirnode, tree) ;
				
			} /* no crossdir selection */

			FileMgrStatusBar(tree,dirnode);
			ListBoxPath(tree,dirnode,tmc);

			if (tree->Compacted)
			{
				BOOL load ;
				BOOL kill ;

				/* If (olddirnode == dirnode), we don't have to load from disk */
				load = kill = (olddirnode != dirnode) ;

				/* Check if we are in double tree mode! */
				if (glob.MaxTree)
				{
					// Is the other tree displaying the same files as tree tmc?
					if ( (listinfo[1-tmc].files == olddirnode) &&
						 (listinfo[1-tmc].tree == tree) )
						kill = FALSE ;
					if ( (listinfo[1-tmc].files == dirnode) &&
						 (listinfo[1-tmc].tree == tree) )
						load = FALSE;
				}
					
#ifndef NOCONSISTENCY
				if (tree->ContinueTree)
				{
					printf("*** User clicked in the partially read in tree\n") ;
					exit(0) ;
				}
#endif
				if (kill)
					ClobberDir(tree, olddirnode);
				if (load)
					LoadCompactDir(tree, dirnode);
			} /* if tree is compacted */

			SortDirectory(tree, dirnode) ;

			listinfo[tmc].files = tree->SelDir = dirnode;

			if (dirnode != olddirnode)
				listinfo[tmc].UpdateFiles = TRUE;

			break;

		case tmmDrawItem:
			/* ZZZZ since this is the only invocation of MakeTreeLine, we
				probably don't need to pass in '0' for the x-coordinate */
#if 1
			MakeTreeLine(pwnd, tree, isz, (RX) 0, (RY) y,
						  bArg & TF_ISFOCUS,glob.lineselected[tmc] == isz);
#else
			/* We never show the selection line now */
			MakeTreeLine(pwnd, tree, isz, (RX) 0, (RY) y,
						  bArg & TF_ISFOCUS,FALSE);
#endif

			break;

		case tmmSetFocus:
		glob.lineselected[tmc] = isz;
			/* Update FM status bar on top of screen. */
			// FileMgrStatusBar(listinfo[tmc].tree, listinfo[tmc].files) ;
			gNeedToUpdateFileList[tmc] = TRUE;
		break;
#if 0
		case tmmDeselectAll:
			/* ignore these messages! */
#endif
		default:
			break;
	}
	return TRUE;
} /* proc ListProcTreeList */

extern BOOL gMouseDown;
BOOL AsynchUpdateTreeList(void)
{
	BOOL retval = TRUE;

	if(gEricStraubsShiftWish || GetLastKeyState())
	{
		if(gEricStraubsShiftWish || !fPollKeyboard)
		{
			if((gNeedToUpdateFileList[0])&&(!gMouseDown))
			{
				ListProcTreeList(tmmSelect,NULL,TreeList[0].focusitem,0,0,0,0); 
				retval = FALSE;
		 
			}
			if((gNeedToUpdateFileList[1])&&(!gMouseDown))
			{
				ListProcTreeList(tmmSelect,NULL,TreeList[1].focusitem,1,0,0,0); 
				retval = FALSE;
			}
		}
		gEricStraubsShiftWish = FALSE;
	}
	return(retval);
}

extern PENTRY glaunchnode ; // File node to be launched.
PTREE glaunchtree ; // the tree to which above node belongs.

extern BOOL gfActivate ;

/****   ListProcFileList - list box proc for current directory listbox
**
**
**      ENTRY
**
**      EXIT
**
**      WARNING:
**
**      EFFECTS:
**
*/
/* ZZZZZ static suff for optimizations have been removed!! as we are dynamically
inserting to snext chain in sorted order.       If tree has been built the
optimizations can be used */

extern void DoCUA2DeselectAll(TMC tmc) ;

VOID BeginManipulation(treehdr far *tree,PENTRY node);
VOID EndManipulation(void);
void Set_Task_Name(char far *name);
extern char *gpszNonSwap ;

WORD PASCAL ListProcFileList(WORD tmm, char *sz, WORD isz, TMC tmc, 
												WORD x, WORD y, WORD flags)
{
   PWND pwd;                            /* Window of list box                                   */
   RX xval;                                     /* x val of left edge of list box       */
   treehdr far *tree;
   PENTRY node; 
	char pName[15];


	UnReferenced(sz) ;
	UnReferenced(x) ;

	tree = listinfo[tmc].tree;

	/* Don't show file list until the tree is completely read in.
	 * Sometimes, like in System Tree mode, we will have our listproc
	 * called even before we start reading the tree. 
	 */
	if ( (!tree->Started) || (tree->ContinueTree) )
		return 0 ; /* Not ready to draw -- say num of files is 0 */

	/* The tree needs to be sorted before we display it.                  */
	/* This is done the first time, we get here for 'tree'. Note that */
	/* we do it here, as we don't sort as soon as the tree is read-in */
	/* Background sorting is not advisable as it might take a while.  */
	if (tree->SortRequired)
	{
		if (glob.TreeMode == TR_SYSTEM)
		 {
			SystemSort(listinfo[0].tree, TRUE) ;
		 }
		else
#ifdef OLDCOMPLETETREESORT
			FormDirectoryOrder(tree, TRUE) ;
#else
			/* sort root directory and viewed dir alone at start up!! */
			SortDirectory(tree, NULL) ; 
			SortDirectory(tree, listinfo[tmc].files) ;

			/* The very first time when we come in here, we need to send a
			 * Select message to the file listbox after directories are sorted.
			 * Fake it by sending a ' ' key. Do this in implicit mode alone.
			 */
			if (FileList[tmc].mode)
				ListKey(&FileList[tmc], ' ', 0);
#endif
	}

	xval = Get_List_Rect(&FileList[tmc]).axLeft +1;
	pwd = Get_List_Window(&FileList[tmc]);

	switch (tmm) {
		case tmmCount:
			return ( (glob.TreeMode == TR_SYSTEM) || 
					 ( (glob.TreeMode == TR_SEARCH) && (gfSearchDisk) ) ) ? 
						/* ZZZZ Store following with tree for speed!! */
						CountTreeMatchedFiles(tree) :
						CountDirMatchedFiles(listinfo[tmc].files, tree);
#ifndef NODIRECT
		case tmmPickUp:
			 node = ( (glob.TreeMode == TR_SYSTEM) ||
					 ( (glob.TreeMode == TR_SEARCH) && (gfSearchDisk) ) ) ? 
					GetNthFlatMatchedFile(tree, isz) :
					GetNthMatchedFile(listinfo[tmc].files, isz, tree);

			 BeginManipulation(tree,node);
			 break;
		case tmmDrop:
			 EndManipulation();
			 break;
#endif
		case tmmActivate:
			gfActivate = TRUE ; // launch file because of an activate message. 

			glaunchnode = ( (glob.TreeMode == TR_SYSTEM) ||
					 ( (glob.TreeMode == TR_SEARCH) && (gfSearchDisk) ) ) ? 
					GetNthFlatMatchedFile(tree, isz) :
					GetNthMatchedFile(listinfo[tmc].files, isz, tree) ;
			if(!glaunchnode)
				break;
			glaunchtree = tree ;
#ifndef NOSWITCHER
			Internal2Normal(pName,glaunchnode->name);
		  Set_Task_Name(pName);
#endif
			LaunchBox();
			break;

		case tmmDeselectAll:
			if (gfManipulating)
				return 0 ; // garbage ;

			DoCUA2DeselectAll(tmc);
			break;

		case tmmGetItemString:
			node = ( (glob.TreeMode == TR_SYSTEM) ||
					 ( (glob.TreeMode == TR_SEARCH) && (gfSearchDisk) ) ) ? 
					GetNthFlatMatchedFile(tree, isz) :
					GetNthMatchedFile(listinfo[tmc].files, isz, tree) ;
			if (node)
				Internal2Normal(sz, node->name) ;
			else
				*sz = '\0' ;
			break ;

		case tmmImplicit:
			Set_KeyWord_Assignment(TK_SAVESTATE,TK_EXPLICITSEL,TK_DISABLED);
			MessageBar(gpszNonSwap, isaMenu,TRUE);
			break ;

		case tmmExplicit:
			Set_KeyWord_Assignment(TK_SAVESTATE,TK_EXPLICITSEL,TK_ENABLED);
			MessageBar(gpszNonSwap, isaMenu,TRUE);
			break ;

		case tmmToggleSelect:
		case tmmDeSelect:
		case tmmSelect:
			if (gfManipulating)
				return 0 ; // garbage ;

		case tmmQuerySelect:
		case tmmDrawItem:
			node = ( (glob.TreeMode == TR_SYSTEM) ||
					 ( (glob.TreeMode == TR_SEARCH) && (gfSearchDisk) ) ) ? 
					GetNthFlatMatchedFile(tree, isz) :
					GetNthMatchedFile(listinfo[tmc].files, isz, tree) ;
			if (!node) 
			{
				/* don't draw on any message but the tmmDraw message */
				if (tmm == tmmDrawItem)
					DisplayNoFilesMsg(pwd, tmc, xval, y, isz, flags, tree) ;
				return 0; /* junk value */
			}

			if(tmm == tmmQuerySelect)
				return(node->SELECTED);

			if(tmm==tmmSelect || tmm==tmmToggleSelect || tmm==tmmDeSelect)
			{
				/* following stuff is not used any more as our focus/selection
				 * drawing mechanism has changed.
				 */
				// flags |= TF_ICONONLY;         /* only repaint icon */
				if(tmm == tmmToggleSelect)
				{
				   if (node->SELECTED)
				   {
					   tree->NumSel-- ;
					   tree->SizeSel -= node->x.f.size ;
				   }
				   else
				   {
						tree->NumSel++ ;
						tree->SizeSel += node->x.f.size ;

				   }
				   /* Toggle the file's selection bit. */
				   node->SELECTED ^= 1;
				}
				else if(tmm == tmmDeSelect) {
				   if(node->SELECTED) {
					   tree->NumSel-- ;
					   tree->SizeSel -= node->x.f.size ;
					   node->SELECTED ^= 1;
				   }
				}
				else /* not toggle */
				if (!(node->SELECTED))
				{
				   tree->NumSel++ ;
				   tree->SizeSel += node->x.f.size ;
				   /* set the file's selection bit */
				   node->SELECTED ^= 1;
				}

				/* If same dir is in both file listboxes, update both */
				if ( (glob.TreeMode == TR_DOUBLE) && 
					(listinfo[0].tree == listinfo[1].tree) &&
					(listinfo[0].files == listinfo[1].files) )
				{
					InsertListItem(&FileList[1-tmc], 0);
				}

			} /* tmm == tmmSelect or tmmToggleSelect or tmmDeSelect */
			else
			{
				/* On tmmcSelect, don't draw!! tmmDraws will follow!! */
				if (glob.TreeMode != TR_SEARCH)
					MakeFileLine(pwd, node, (RX) xval, (RY) y, flags, tree);
				else
					MakeSearchLine(pwd, node, (RX) xval, (RY) y, flags, tree);
			}
			break ;

		case tmmSetFocus:
		default:
			break ;

	} /* switch */

	return 0 ;
} /* ListProcFileList */

/****   MakeTreeLine - create given line for the tree listbox
**      This routine is called once for each line in the directory tree list
**      box.  It fetches the 'line'th directory of 'tree' in depth-first
**      order, and builds the listbox line associated with it.  It then
**      displays it.
**
**      ENTRY
**              pwd     - window to output to
**              tree    - points to tree structure
**              line    - line you want, or -1 for last
**              x, y    - character position to display line.
**              isfocus - TRUE if this is our current directory.
**      EXIT
**              Returns TRUE if line was valid, else FALSE.
*/
BOOL MakeTreeLine(PWND pwd, PTREE tree, int line, RX x, RY y, 
									BOOL isfocus, BOOL ischecked )
{
	int i, index;
	PENTRY node;                            // directory under consideration
	PENTRY walk;                            // used for tree traversal
	char lines[MAXNODE+1];          // '|' or space for each ancestor
	char str[(MAXNODE+1)*3+1];      // string to be displayed
	char dirname[NAMELEN+EXTLEN+2];
	int namelen ;
	int cliplimit ;

	/* ZZZZ look into not handling line == 0 differently than the rest! */
	if (!line)
	{
		strfcpy(str, tree->root);
		index = strlen(str);

		/* Draw the collapsible/non-collapsible marker here! */
		DrawDirIcon(pwd, x+ICONLEN, y, GetCollapseChar(tree, NULL), 0) ;

		// Draw directory name, highlighted if necessary.
		TextOut(pwd, x+ICONLEN+COLL_ICON_LEN, y, str, index,
					isfocus?isaHilite:(ischecked?isaSelect:isaBackground)) ;
		// The InvalidateInconCache() for this has been taken care of below!

		/* The number '3' is the length of root name ex: "C:\" */
		DrawFocusMarker(pwd, 1, y, x+ICONLEN+COLL_ICON_LEN, y, 3, isfocus,
													ischecked, isaBackground) ;
		// Erase rest of line.
		index += COLL_ICON_LEN ;
		for (i=0; i < TREELINELEN-index; i++)
			str[i] = ' ';
		TextOut(pwd, (RX) x+index+ICONLEN, y, str, TREELINELEN-index, isaBackground);
		InvalidateIconCache((AX)x+ICONLEN+COLL_ICON_LEN, y,
										(AX)x+ICONLEN+TREELINELEN-1, y) ;
		return TRUE;
	} else
	{
		node  = GetNthVisibleDir(tree, line);
	}
	if (!node)
	{
		for (i=0; i < TREELINELEN+ICONLEN-1+1; i++)
				str[i] = ' ';

		/* BUG BUG this has been overridden, now we always do like text */
		/* In Text mode, draw a blank line starting at column 1 of screen,
		 * so as not to erase the border character we draw at column 0.
		 * In graphics mode, we use column 0 too to erase check mark trails.
		 */
		TextOut(pwd, (RX) 1, y, str, TREELINELEN+ICONLEN-1,isaBackground);
		InvalidateIconCache((AX)1, y,
					(AX)1+TREELINELEN+ICONLEN-1-1, y) ;
		return FALSE;
	}
	
	/* Convert name from internal form to NAME.EXT form */
	namelen = Internal2Normal(dirname, node->name) ;

	/* For each ancestor of the directory, we need to use the appropriate
	* graphics character in the string to give the listbox a tree-like
	* appearance. This means a vertical line if the ancestor has siblings
	* after it, or a blank otherwise.
	*/
	index = 0;
	walk = FindParent(node);
	while (walk)
	{
		if (walk->LASTDIR)
			lines[index] = ' ';
		else
			lines[index] = VERTLINE;
		index++;
		walk = FindParent(walk);
	}
	
	/* Now fill string with the characters in proper order (they are backwards
	 * now).
	 */
	i = 0;
	while (--index >= 0)
	{
		str[i++] = ' ' ;
		str[i++] = lines[index];
		str[i++] = ' ' ;
	}
	
	// add an extra blank to center 'node' wrt its predecessor directory.
	str[i++] = ' ' ;

	/* The '-2' is for the two special characters we add later for node */
	cliplimit =  TREELINELEN - namelen - COLL_ICON_LEN - 2 ;

	/* Is clipping needed? */
	if (i > cliplimit)
	{
		i = cliplimit ;
		str[i++] = ' ' ;
		//str[i++] = chLeftArrow ;
		str[i++] = ' ' ;
	}
	else
	{
		/* Add special chars for node itself, and display the string.*/
		if (node->LASTDIR)
			str[i++] = LLCORNER;
		else
			str[i++] = MIDLEFT;

		str[i++] = HORIZLINE;
	}

	
	TextOut(pwd, x+ICONLEN, y, str, i, isaBackground);
	InvalidateIconCache(x+ICONLEN, y, x+ICONLEN+i-1, y) ;

	DrawDirIcon(pwd, x+i+ICONLEN, y, GetCollapseChar(tree, node), 0) ;

	/* Display directory name in highlight mode; add trailing blanks to
	* supress any leftover chars to the right of the name.
	*/
	TextOut(pwd, x+i+ICONLEN+COLL_ICON_LEN, y, dirname, namelen,isfocus?isaHilite:(ischecked?isaSelect:isaBackground));
	InvalidateIconCache(x+i+ICONLEN+COLL_ICON_LEN, y, 
						x+i+ICONLEN+COLL_ICON_LEN+namelen-1, y) ;

	DrawFocusMarker(pwd, 2, y, x+i+ICONLEN+COLL_ICON_LEN, y, namelen, isfocus,
												ischecked, isaBackground) ;

	index = x + i + ICONLEN + COLL_ICON_LEN + namelen ;

	i = TREELINELEN + ICONLEN - index ; // Number of padding blanks

	if (i > 0)
	{
		for ( --i ; i >= 0 ; i--)
			str[i] = ' ';

		TextOut(pwd, (RX)index, y, str, TREELINELEN+ICONLEN-index, isaBackground);
		InvalidateIconCache((AX)index, y, (AX)(TREELINELEN+ICONLEN-1), y) ;
	}
	/* else no padding required */

	return TRUE;
} /* proc MakeTreeLine */

/* Caller takes responsibility to disable the mouse & re-enable it */

void DrawFileIcon(BOOL progfile, BOOL sel, PWND pwnd, RX x, RY y)
{
	BITMAP *pbitmap ;

	if (gisgraph)
	{
		/*
		 * check the icon cache to see if we should
		 * really do some drawing here
		 */
		if (gIconCache && 
				(gIconCache[y*MAXCOLS+x] == (char)((progfile<<1)|(sel))))
			return;

		if (sel)
		{
			pbitmap = (progfile) ? ProgramIconInvert : FileIconInvert;
		} else
		{
			pbitmap = (progfile) ? ProgramIcon : FileIcon ;
		}
		PlotBmp(pbitmap, (x+1)*CWIDTH, (y)*CHEIGHT,isaBackground);
		/*
		 * set the icon we just drew in the IconCache
		 */
		if(gIconCache)
		   gIconCache[y*MAXCOLS+x] = (char)((progfile<<1)|(sel));
	} else
	{
		if (sel)
			CharOut(pwnd, x+3, y, SELCHAR, 0);
		else
			CharOut(pwnd, x+3, y, ' ', 0);
	}
} /* DrawFileIcon */


/****   MakeFileLine - Make line of Files list box
**      This fn is called once for each line in the listbox containing the
**      current directory files.  It locates the 'line'th file of the current
**      directory, then builds a string with the file's name, size, and
**      access date and displays it; it also displays either a program or data
**      file icon.
**
**      ENTRY
**              pwnd  - window to write line to
**              node  - record of file to display
**              x, y  - character position of beginning of line
**              flags:
**                  bit 0: 1 = selected, 0 = unselected
**                  bit 1: 1 = draw icon only, 0 = draw text and icon
**      EXIT
**              Return Val: TRUE if found, else FALSE
**      NOTE:
**          The date format is currently hard-coded for USA format.  We need
**      to have it look at country info and act accordingly.
*/

/* Indices in string
*/
/* Flat File listbox length > Normal File box length -- so we use
   this bigger buffer in either case for blanking out buffer!! */
#define FLATLINELEN     (FLATRIGHTWIDTH-4)
#define FILELINELEN     (FILEBOXWIDTH-4)

/*  The Top Left relative co-ordinates in MainWind where the first character
	of fileinfo is placed. */
#define SYSTEMLEFTX 2
#define SYSTEMLEFTY 7

BOOL MakeFileLine(PWND pwnd, PENTRY node, RX x, RY y, WORD flags, PTREE tree)
{
	int i,j;
	BOOL progfile;                                                  // TRUE = executable file
	BOOL fispm = TRUE ;
	/* % is ok in the expression below as it is a constant expression! */
	char str[1+FLATLINELEN+((FLATLINELEN+1)%2)]; // length must be even
	WORD DrawLen ;  

	/* ZZZ I hope the compiler optimizes all the /, %, >> */

	/* make string of all blanks - this way I don't have a while loop
	** for each possible gap in the string.
	*/
	for (i=0; i < FLATLINELEN; i+=2)
	{
		*(WORD *)&str[i] = 0x2020;
	}

	DrawLen = (glob.TreeMode == TR_SYSTEM) ? FLATLINELEN : FILELINELEN ;

	/* If there is no file for this line, clear the line.  */
	if (node == NULL)
	{
		// FEnableMouseNest(FALSE);
		TextOut(pwnd, x, y, str, DrawLen, isaBackground);
		// FEnableMouseNest(TRUE);
		/* Invalidate the Icon cache for this line. */
		InvalidateIconCache(x, y, x+DrawLen-1, y) ;
		return FALSE;
	}

	/* Add extension to string. We always do this, cause we test it later */
	if (node->name[NAMELEN])
	{
		/* copy extension */
		str[EXTSTART-1] = '.';
		for (i=EXTSTART, j=NAMELEN; j<NAMELEN+EXTLEN && node->name[j]; i++,j++)
			str[i] = node->name[j];
	}

	/* Add name to string.  */
	for (i=NAMESTART, j=0; j < NAMELEN && node->name[j]; i++, j++)
		str[i] = node->name[j];
	
	/* Add file size to string. */
	CopyNumberForTextOut(str+SIZESTART+SIZESIZE, node->x.f.size, FALSE) ;
	
	/* Add date to string */
	Get_Date_and_Time_Strings(node->dtlx.dt.date, node->dtlx.dt.time,
		str+DATESTART, str+TIMESTART, 0);

	/* Compute icon type to display. We now use a program icon for BAT's too */
#if 0
	progfile = (((*(DWORD *)&str[EXTSTART])) == *(DWORD *)ExeStr) ||
			   (((*(DWORD *)&str[EXTSTART])) == *(DWORD *)ComStr) ;
#else
	progfile = FIsExecutableFile(str+EXTSTART) ;
#endif

	/* Now put (text and) icon onscreen. */
	// FEnableMouseNest(FALSE);

	TextOut(pwnd, x+NAMESTART, y, str+NAMESTART, DrawLen-NAMESTART,
							(flags & TF_ISFOCUS)?isaHilite:((BOOL)node->SELECTED?isaSelect:isaBackground));
	DrawFocusMarker(pwnd, x+1, y, x+NAMESTART, y, DrawLen-NAMESTART,
					 flags & TF_ISFOCUS, (BOOL) node->SELECTED, isaBackground) ;
	DrawFileIcon(progfile, (BOOL)node->SELECTED, pwnd, x, y) ;

	/* Node has been selected / deselected => selected info on system tree
	   mode on left needs to be updated or it has focus  */
	if (glob.TreeMode == TR_SYSTEM)
	{
		if (flags & TF_ISFOCUS)
		{
			DispFlatLeft(node, tree, &MainWind, SYSTEMLEFTX, SYSTEMLEFTY,
										isaBackground) ;
			FileMgrStatusBar(tree, FindParent(node)) ;
		}
		else
		if (!gfFlatLeftInfoDrawn)
		{
			/* ZZZZZ should this be set here or in DispFlatLeft fn?? */
			gfFlatLeftInfoDrawn = TRUE ;

			/* Draw the first file -- index 0 if we haven't yet drawn the
			 * the flatleft info for any file as yet -- This could happen
			 * if the focus has not yet come to the file list box.
			 */
			// node = GetNthFlatMatchedFile(tree, 0) ;
			node = GetNthFlatMatchedFile(tree, Get_List_Focus(&FileList[0])) ;
			DispFlatLeft(node, tree, &MainWind, SYSTEMLEFTX, SYSTEMLEFTY,
										isaBackground) ;
			FileMgrStatusBar(tree, FindParent(node)) ;

		}
	}

	// FEnableMouseNest(TRUE);
	
	return TRUE;
} /* proc MakeFileLine */

#define SEARCHLINELEN (SEARCHWIDTH-4)

BOOL MakeSearchLine(PWND pwnd, PENTRY node, RX x, RY y, WORD flags, PTREE tree)
{
	int i;
	BOOL progfile;                                                  // TRUE = executable file
	int dummylen ;

	/* % is ok in the expression below as it is a constant expression! */
	char str[MAXCOLS]; // length must be even

	/* ZZZ I hope the compiler optimizes all the /, %, >> */
	/* make string of all blanks - this way I don't have a while loop
	** for each possible gap in the string.
	*/
	for (i=0; i < SEARCHLINELEN; i+=2)
	{
		*(WORD *)&str[i] = 0x2020;
	}

	/* If there is no file for this line, clear the line.  */
	if (node == NULL)
	{
		// FEnableMouseNest(FALSE);
		TextOut(pwnd, x, y, str, SEARCHLINELEN, isaBackground);
		// FEnableMouseNest(TRUE);
		/* Invalidate the Icon cache for this line. */
		InvalidateIconCache(x, y, x+SEARCHLINELEN-1, y) ;
		return FALSE;
	}

	/* Add name to string. */
	Tree2Path(tree, node, str+NAMESTART, &dummylen) ;

	/* Compute icon type to display. We now use a program icon for BAT's too */
#if 0
	progfile = !( fstrncmp(ExeStr, node->name+NAMELEN, 3) &&
				  fstrncmp(ComStr, node->name+NAMELEN, 3) ) ;
#else
	progfile = FIsExecutableFile(node->name+NAMELEN) ;
#endif

	/* Now put (text and) icon onscreen.  */
	// FEnableMouseNest(FALSE);

	TextOut(pwnd, x+NAMESTART, y, str+NAMESTART, SEARCHLINELEN-NAMESTART,
								  (flags & TF_ISFOCUS)?isaHilite:(((BOOL)node->SELECTED?isaSelect:isaBackground))) ;
	DrawFocusMarker(pwnd, x+1, y, x+NAMESTART, y, SEARCHLINELEN-NAMESTART,
					 flags & TF_ISFOCUS, (BOOL) node->SELECTED, isaBackground) ;

	DrawFileIcon(progfile, (BOOL)node->SELECTED, pwnd, x, y) ;

	// FEnableMouseNest(TRUE);
	
	return TRUE;
} /* proc MakeSearchLine */

/* Puts up the meassage no files present or no files match in file listbox */
void DisplayNoFilesMsg(PWND pwd, TMC tmc, RX xval, WORD y, WORD isz, 
								WORD flags, PTREE tree)
{
	/* strings to show when no files displayed */
	char *messagestr1, *messagestr2;
	PENTRY node ;

	/* Following variable is used to reduce code size. See assignment below!! */
	int (*linedrawfn)(PWND, PENTRY, RX, RY, WORD, PTREE) ;

	/*Depending on mode, we need to call the right fn. Store now to use later*/
	linedrawfn = (glob.TreeMode != TR_SEARCH) ? MakeFileLine : MakeSearchLine ;
	
	switch(isz) {
	case 0 :
		if ( (glob.TreeMode != TR_SYSTEM) &&
			 (glob.TreeMode != TR_SEARCH) &&
			 (CountDirFiles(listinfo[tmc].files,tree) == 0)
		   )
		{
			messagestr1 = szNoFilesPresent1;
			messagestr2 = szNoFilesPresent2;
		}
		else
		{
			messagestr1 = szNoFilesMatch1;
			messagestr2 = szNoFilesMatch2;
		}

		/* Blank out this line and subsequent line -- Calling MakeFileLine or
		   MakeSearchLine with node = NULL, blanks out that line! */
	
		(*linedrawfn)(pwd, NULL, (RX) xval, (RY) y, flags, tree);
		(*linedrawfn)(pwd, NULL, (RX) xval, (RY) y+1, flags, tree);
		(*linedrawfn)(pwd, NULL, (RX) xval, (RY) y+2, flags, tree);

		/* Draw in place of subsequent lines (line 1, line2) */
		TextOut(pwd, (RX)xval+NAMESTART, (RY)y+1,messagestr1, -1, isaBackground);
		TextOut(pwd, (RX)xval+NAMESTART, (RY)y+2,messagestr2, -1, isaBackground);

		/* DispFlateLeft has to be called here with NULL node, as MakeFileLine
		   in System Tree mode handles cases only when node != NULL */
		if (glob.TreeMode == TR_SYSTEM)
			DispFlatLeft(NULL, tree, &MainWind, SYSTEMLEFTX, SYSTEMLEFTY,
											isaBackground) ;
		break ;

	case 1:
	case 2:
		node = ( (glob.TreeMode == TR_SYSTEM) ||
				 ( (glob.TreeMode == TR_SEARCH) && (gfSearchDisk) ) ) ?
					GetNthFlatMatchedFile(tree, 0) :
					GetNthMatchedFile(listinfo[tmc].files, 0, tree) ;

		/* See above (isz == 0) -- the line 0 msg had 2 lines drawn
		 * for "No files ..." msg in line 1 and 2!
		 */
		if (!node)
			break ;
		/* else flow thru and clear this line!! */

	default:
		/* This function, when called with NULL, blanks out that line */
		(*linedrawfn)(pwd, NULL, (RX) xval, (RY) y, flags, tree);
	} /* switch */

#ifndef NOCONSISTENCY
	if (y < 6)
	{
		printf("**** Bug!! -- Bad draw message\n") ;
		exit (0) ;
	}
#endif
} /* DisplayNoFilesMsg */
