;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <text.h>
#include <help.h>
#include "creatdir.hs"
#include "creatdir.sdm"

#define EXCEED_PATHLEN_ERR 5

extern BYTE ErrorCrit ;

extern char * PASCAL MySetMessageText(TMC tmc, char *message, int nWid) ;
extern void InsertDirAndMarkLastDirBits(PTREE tree, PENTRY parent, PENTRY newdir) ;
extern PENTRY HandleDeletednext(PTREE tree, PENTRY deldir) ;
extern int DeleteConfirmationDialog(char *dpath, BOOL fIsDelFile, char *Message) ;
extern VOID MakeDirNameFit(char *longname,char *pattern,char *destination,int maxlen) ;

int  CreateDirectory(PTREE tree, PENTRY parent, char *name, BOOL fRealCreation)
{
	PENTRY rptr ;
	char curpath[MAX_PATH+1] ;
	char parpath[MAX_PATH+1] ;
#ifdef TRUENAME_LIMIT
	 char TrueName[128] ;
#endif
	int code, ret ;
	struct find_t findinfo ;
	char statusline[STATUS_LEN] ;
	int action ;
	int dummylen ;

	rptr = AllocTreeEntryCompactIfNeeded(tree) ;
	if (!rptr)
	{
		OutOfMemory() ;
		return ACT_CANCEL ;
		// return ACT_NOMEM ;
	}       /* while */

	if (fRealCreation)
	{
	/* allocation of rptr was done succesfully! It was better to check for
	    *   memory availability before touching the disk .
		 */
		Tree2Path(tree, parent, parpath, &dummylen) ;
		strcpy(curpath, parpath) ;
	if (parent)
			strcat(curpath, "\\") ;

	/* ZZZZZ if name is already in upper case then remove following!! */
#ifdef DBCS
	DBCSstrupr(name) ;
#else
	strupr(name) ;
#endif
	strcat(curpath, name) ;

	FormStringStatusLine(statusline, szStDir, curpath, STATUS_LEN) ;

		/* Check to make sure that the new dir does not exceed the 66 char
		 * limit in DOS -- needed for Novell networks, 3+Open, etc.
		 * I don't check for the "TrueName" anymore because command.com
		 * seems to not do this and lets the net server see if it can handle
		 * longer than 66 char path names. However, DOS can't copy files, etc
		 * from a directory this deep.
		 */
#ifdef TRUENAME_LIMIT
		if (translate_name(curpath, TrueName))
			strcpy(TrueName, curpath) ;

		ErrorCrit = 0xFF ; // set to default non-critical error value.

		if (strlen(TrueName) > ALLOWED_PATH)
#else
		if (strlen(curpath) > ALLOWED_PATH)
#endif
		{
			/* This one causes the Access denied message, F1=Help can be used
			 * to figure out what error it is.
			 */
			ret = -1 ;
			_doserrno = EXCEED_PATHLEN_ERR ;
		}
		else
		{
		ret = mkdir(curpath) ;
		}

	if (ret)
	{
			/* Directory creation error */
			/* We had allocated one slot but we don't use it!! */
			/* ZZZZZ Similar stuff should be done anywhere we allocate but don't 
			 * use the allocated node!!
			 */
			tree->holecount++ ;

			/* Did a critical error occur? -- say, disk write protected, etc! */
			if (ErrorCrit != 0xFF)
				action = GetResponse(statusline, szCriticalMessages[ErrorCrit],
														BT_SKIPRETRY, HELP_ERRCREATEDIR);
			else
				action = DOSErrorBox(statusline, _doserrno, HELP_ERRCREATEDIR) ;

			return action ;
	}

		/* Directory has been successfully created. Disk Avail Size, etc
		 *      would have changed. So, a showinfo should reread available size
		 */
		tree->fdiskinfoknown = FALSE ;
		 
		/* We could just get a find attributes here!! as wr_time isn't used
	    * anyway
		 */
		/* ZZZZ Following has a BUG in case following call fails? We have created
		 * the directory but we can't read attributes, so tree not updated, but
	    * disk has new directory
		 */
		do
		{
		code = _dos_findfirst(curpath, _A_SUBDIR, &findinfo) ;
			ret = (code) ? DOSErrorBox(statusline, code, HELP_ERRFINDCREATEDIR) :
																							ACT_OK ;
		} while (ret == ACT_RETRY) ;

		if (ret != ACT_OK)
			return ACT_CANCEL ;

		Normal2Internal(rptr->name, (char far *) findinfo.name) ;

		/* ZZZ time actually not needed */
		rptr->dtlx.dt.time = findinfo.wr_time ;
		rptr->attribs = findinfo.attrib ;
	}
	else
	{
		Normal2Internal(rptr->name, (char far *) name) ;

		/* ZZZ time actually not needed -- use garbage*/
		rptr->dtlx.dt.time = 0 ;
		rptr->attribs = (_A_HIDDEN | _A_SUBDIR | _A_SYSTEM) ;
	}
	rptr->dtlx.lx.level = (parent) ? parent->dtlx.lx.level+1 : 1 ;

	rptr->FIRSTFILEFOUND = 0 ;
	/* There are no files belonging to 'dir' as it has just been created. So,
	 * trivially its files (0 of them) are in sorted order.
	 */
	rptr->DIRSORTED = TRUE ; 

	tree->DirCount++ ;

	/* prev to rptr is NULL as we insert this new directory entry as the
	 *  very first subdirectory of parent.
	 */
	UpdateSiblings(tree, parent, NULL, rptr) ;

	rptr->x.d.child = NULL ;

	/* For non-root directory, mark its parent as having a sub-dir! */
	if (parent)
		parent->HASSUBDIRS = TRUE ;

	/* Mark the collapse dir bits correctly for rptr now! */
	rptr->HASSUBDIRS = FALSE ;
	rptr->EXPANDED = FALSE ;

#ifdef DONT_DO_ANY_INSERTION_SORT
	rptr->LASTDIR = HasNoSubDir(tree, parent) ;

	if (parent)
	{
		rptr->x.d.dnext = parent->x.d.dnext ;
		parent->x.d.dnext = rptr ;
	}
	else
	{
		rptr->x.d.dnext = tree->FirstDirectory ;
		tree->FirstDirectory = rptr ;
	}
#else
	/* Insert directory 'rptr' in the 'dnext' chain (after 'parent') in
	 * alphabetical order of 'parent' files.
	 */
	InsertDirAndMarkLastDirBits(tree, parent, rptr) ;
#endif

	/* Note that "tree->VisibleDirCount" does not include the root dir */
	if ( ((!parent) && (tree->VisibleDirCount > 0)) ||
		  (parent && (parent->EXPANDED)))
	{
		rptr->DISPLAYED = TRUE ;
		tree->VisibleDirCount++ ;
	}
	else
	{
		/* We have just created a dir -- The user would like to be able to
		 *  see the new entry added. So we expand 'parent' by 1 level indicated
		 *  by passing FALSE as the last argument.
		 */
		ExpandDirVisibility(tree, parent, FALSE) ;
	}

	return ACT_OK ;
} /* CreateDirectory */

static char *gsParDirName; /* parent dir name when creating a new directory */

BOOL PromptAndCreateDir(PTREE tree, PENTRY parent)
{
     HCABcreatdir  h;
     char newdirname[NAMELEN+EXTLEN+2] ;
	  char tempname[MAX_PATH+1] ;
	  char newtempname[MAX_PATH+1] ;
	  int ret ;
	  int dummylen ;

	  Tree2Path(tree, parent, tempname, &dummylen) ;

	  // FormPseudoPathName(pardirname, parent, tree) ;
     gsParDirName = tempname ;

		do
		{
	h = HcabAlloc(cabiCABcreatdir);

			if (!h)
			{
				OutOfMemory() ;
				return FALSE ;
			}

			InitCab(h, cabiCABcreatdir) ;

			SzToCab(h, NullString, Iag(CABcreatdir, pszcreatdirnew));

			SzToCab(h, szEnterButton, Iag(CABcreatdir, pszcreatdirEB));
			SzToCab(h, szCancelButton, Iag(CABcreatdir, pszcreatdirCB));
			SzToCab(h, szHelpButton, Iag(CABcreatdir, pszcreatdirHB));

	if (MyTmcDoDlg(&dlgcreatdir,  h) == tmcOK)
	{
				/* The reason we use "newtempname" and not just re-use "tempname"
				 * is because we might do this loop more than once and in that
				 * case we want gsParDirName (i.e., "tempname") to remain
				 * the same.
				 */
				SzFromCab(h,newtempname,MAX_PATH,Iag(CABcreatdir,pszcreatdirnew));
				ScrunchFileName(newdirname, newtempname, FALSE) ;

				// The following upper case translation is done in CreateDirectory
				// strupr(newdirname) ;
				ret = CreateDirectory(tree, parent, newdirname, TRUE) ;
	}
	/* else he used cancel=>no directory creation needs to be done! */
	else 
				ret = ACT_CANCEL ;

			FreeCab(h) ;
		} while (ret == ACT_RETRY) ;
		return (ret == ACT_OK) ;
} /* PromptAndCreateDir */


BOOL FAR PASCAL FDlgcreatdir(WORD dlm, TMC tmc, WORD wNew,
												WORD wOld, WORD wParam)
{
	PWND lwind ;
	int nWid ;
	char *msg ;
	int  itemwidth ;
	char TempStr[MAX_PATH+1] ;

	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	switch (dlm) {

	case dlmInit:
		SetUpDialog(tmcOK,szCreateDirCaption) ;
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);
		SetUpButtonForGraphics(tmccreatdirhelp);
		SetUpEditBox(tmccreatdirnew, TRUE,NAMELEN+EXTLEN+1, TRUE);

		/* Make TempStr have the compressed parent dir name */
		lwind = PwndOfTmc(tmccreatdirold);
		itemwidth = lwind->arcWindow.axRight - lwind->arcWindow.axLeft ;
		MakeDirNameFit(gsParDirName, NULL, TempStr, itemwidth) ;
		Shell_SetTmcText(tmccreatdirold, TempStr);
		break;

	case dlmTerm:
		if (tmc == tmcOK)
		{
			GetTmcText(tmccreatdirnew, TempStr, MAX_PATH);

			/* Did the user not type any name? If so don't dismiss dialog */
			if (!(*TempStr))
			{
				Shell_Beep() ;
				return FALSE ;
			}

			/* check to see if the user is trying to use nested names */
#ifdef DBCS
			if (DBCSstrchr(TempStr, PATHCHAR))
#else
			if (strchr(TempStr, PATHCHAR))
#endif
			{
				lwind = PwndOfTmc(tmccreatdirerr1) ;
			nWid = lwind->arcWindow.axRight - lwind->arcWindow.axLeft + 1;

				msg = MySetMessageText(tmccreatdirerr1, szNoPathCharsInCreatDir,
																						nWid) ;
				/* Warning! Assuming that both these edit items have same width */
				MySetMessageText(tmccreatdirerr2, msg, nWid) ;
				return FALSE ; /* Don't dismiss dialog -- invalid name */
			}

		}
		break ;

	case dlmSetFocus:
		gCurrentTMC = tmc;
		break ;

	case dlmClick:
		 if(tmc == tmccreatdirhelp)
			Help(hemDialog, ((PDLG)&dlgcreatdir)->hid, NULL, 0);

		 SetFocusTmc(gCurrentTMC) ;
		 break ;

	default: /* do nothing */ ;
	}
	return(TRUE);
} /* FDlgcreatdir */

/* Find out which directory is to be deleted based on list box focus and
*   delete it.
*/
int FindAndDelDirectory(void)
{
	PENTRY deldir ;
	PTREE tree ;

	deldir = listinfo[glob.FocusBox].files ;
	tree = listinfo[glob.FocusBox].tree ;

	/* Note that 'deldir' can't be NULL as the root directory can't be deleted
	 * The menu would have been disabled and this fn won't be called
	 * on the root directory.
	 */

	/* If the directory is non-empty, tell the user it can't be deleted */
	if (deldir->x.d.child)
	{
		ShellMessageBox(szDelGroupTitle, szDelDirLine1) ;
		return ACT_CANCEL ;
	}
	else
		return DelDirectory(deldir, tree, glob.VerifyDelete) ;
} /* FindAndDeleteDirectory */

int DelDirectory(PENTRY deldir, PTREE tree, BOOL verify)
{
	char dirpath[MAX_PATH+1];                       // old file path
	PENTRY prev_dfs_dir ;
	PENTRY parent ;
	char rootdir[4] ; /* for "?:\" */
	int action, code ;
	char statusline[STATUS_LEN] ;
	PENTRY dir0, dir1 ;
	int dirlinefocus, dirlineother ;
	int dummylen ;

#ifndef NOCONSISTENCY
	if (!deldir)
	{
	    printf("*** request to delete root!! menu not disabled! \n") ;
	    exit(0) ;
	}
#endif
	/* Store the directories being displayed in the two listboxes. Store
	 *      also their line numbers (indices in dir listbox). This will be used
	 *      when we try to re-paint the listboxes. These will change when we
	 * delete the directory.
	 */
	dir0 = listinfo[0].files ;
	dirlinefocus = glob.lineselected[glob.FocusBox] ;

	/* In case we are not in double tree mode, these might be meaningless! */
	dir1 = listinfo[1].files ;
	dirlineother = glob.lineselected[1-glob.FocusBox] ;

	Tree2Path(tree, deldir, dirpath, &dummylen) ;

	FormStringStatusLine(statusline, szStDir, dirpath, STATUS_LEN) ;

	if (verify)
	{
		/* We will get back ACT_OK, ACT_SKIP or ACT_CANCEL! */ 
		action = DeleteConfirmationDialog(dirpath, FALSE, NullString) ;

		if (action == ACT_SKIP)
			action = ACT_CANCEL ;
		else
		if (action == ACT_FORCE)
			action = ACT_OK ;
	}
	else
		action = ACT_OK ;
		
	if (action != ACT_OK)
	{
		return action ;
	}

	/* ZZZ We could check for its child field being NULL before issuing
	   a disk operation! But in compact mode, this will create problems */
	/* ZZZZ cwd is dirpath, we won't be able to delete it -- warn user! */
	/* Right now -- we fix it by changing directories to root! */
	strfcpy(rootdir, tree->root) ;
	UnixChdir(rootdir) ;

	do
	{
		code = rmdir(dirpath) ;
		action = (code) ? DOSErrorBox(statusline, _doserrno, HELP_ERRDELDIR) : ACT_OK ;
	} while (action == ACT_RETRY) ;

	if ( (action != ACT_FORCE) && (action != ACT_OK) )
		return action ;

    parent = FindParent(deldir) ;

    /* We succesfully deleted 'deldir' => it had no children. It
     *  thus saves us the trouble of updating its children, etc
	  */

	 /* remove the entry 'deldir' from the 'dnext' chain of 'tree' */
	 prev_dfs_dir = HandleDeletednext(tree, deldir) ;

    tree->DirCount-- ;

    /* Now update child pointer of 'parent' and siblings of 'deldir' */
    DelFileEntry(tree, parent, deldir) ;
    /*  we need to update the directory and file list box
       displays correctly here. DelFileEntry only handles normal
       files.
    */
	/* Now set parent dir's collapse bits. Note that to be able to delete
	 * a directory, 'dir' and its parent should have been visible in list box.
	 */
	if (parent)
	{
		parent->HASSUBDIRS = parent->EXPANDED = AreThereSubDirs(tree, parent) ;
	}
	tree->VisibleDirCount-- ;

#ifndef NOCONSISTENCY
	if (!glob.lineselected[glob.FocusBox])
	{
		printf("*** check mark can't have been at line 0\n") ;
		exit(0) ;
	}
#endif

    /* selection focus (check mark) shifts to 'prev_dfs_dir' */
    listinfo[glob.FocusBox].files = prev_dfs_dir ;

	 if (tree->Compacted)
	 {
		LoadCompactDir(tree, prev_dfs_dir);
	 }

	/* Selected dir (one with check mark) now will be the dir above the deleted
		one.
	*/
	/* Sending Select Message to directory listbox fixes lots of problems
	 * like reading in parent dir's files in case of compact dir mode, sorting
	 * its files in case it has not been done yet, etc.
	 */
	ListProcTreeList(tmmSelect, NULL, glob.lineselected[glob.FocusBox]-1,
												TreeList[glob.FocusBox].tmc, 0, 0, 0) ;
	listinfo[glob.FocusBox].UpdateFiles = TRUE ;

	/* If we are in Double tree mode and same tree in other box too, repaint
	 * that set of boxes correctly too. Note that we are sending Select Messages
	 * So, the non-focus listbox needs to be handled first as the top of
	 * FM which displays the current directory should be set right!
	 */
	if ( (glob.TreeMode == TR_DOUBLE) &&
			(listinfo[0].tree == listinfo[1].tree) )
	{
		/* following check is same as (dirlinefocus == dirlineother) */
		if (dir0 == dir1)
		{
			ListProcTreeList(tmmSelect,NULL,glob.lineselected[1-glob.FocusBox]-1,
											TreeList[1-glob.FocusBox].tmc, 0, 0, 0) ;
			listinfo[1-glob.FocusBox].UpdateFiles = TRUE ;

			/* See comment below for focuslinechange as to why this check
			 * is done.
			 */
			if ( tree->VisibleDirCount != (glob.lineselected[glob.FocusBox]) )
				FocusLineChange(&TreeList[1-glob.FocusBox], -1) ;
		}
		else
		{
			/* In case the selected dir in other listbox is earlier, then we
			 * don't have to modify focus but just repaint the dir list box.
			 */
			if (dirlineother < dirlinefocus)
			{
				InsertListItem(&TreeList[1-glob.FocusBox], 0) ;
			}
			else
			{
				/* the selected dir is displayed later than the deleted directory
				 * the directory moves 1 above its previous location. So update
				 * the focus to be 1 above. Repaint after this.
				 */
				FocusLineChange(&TreeList[1-glob.FocusBox], -1) ;
				InsertListItem(&TreeList[1-glob.FocusBox], 0) ;
			}
		}
	}

	/* Note that the listbox will still keep the focus at the same line
	 *      as before. We need to move it 1 line up. If last directory is
	 *      being deleted, the listbox automatically moves it one up, so
	 *      we don't do it here.
	 */
	if ( tree->VisibleDirCount != (glob.lineselected[glob.FocusBox]) )
		FocusLineChange(&TreeList[glob.FocusBox], -1) ;

	InsertListItem(&TreeList[glob.FocusBox], 0) ;

	return ACT_OK ;
} /* fn DelDirectory */



#if 0
/************************** Unused function ******************************/
/* Forms a pseudo path name for node -- It makes it of the form "?:name" */
void FormPseudoPathName(char *path, PENTRY node, PTREE tree)
{
	if (node)
	{
		path[0] = tree->root[0] ;
		path[1] = ':' ;
      Internal2Normal(path+2, node->name) ;
	}
	else
	{
		strfcpy(path, tree->root) ;
	}
} /* FormPseudoPathName */

#endif
