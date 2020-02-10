;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****   copy.c - "copy destination" dlg box fns; file operation dispatcher
**
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  7/21/89   t-jeffro  created file
**  7/31/89   t-jeffro  Now supports 2-tree display.
**      8/09/89   t-jeffro      uses generic error dialogue box
*/
#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <text.h>

#include <copy.hs>                              /* files for copy destination dialogue */
#include <copy.sdm>                             /* ... */

#include <delfiles.hs>                  /* files to verify selction of deleted files*/
#include <delfiles.sdm>                 /* ... */

extern VOID ListBoxPath(treehdr far *tree,PENTRY node,int whichtree) ;
extern void ForceSortDirectory(PTREE tree, PENTRY dir) ;
extern void SortDirectory(PTREE tree, PENTRY dir) ;
extern BOOL FPerformDirOperation(void) ;
extern BOOL FPerformOpOnDirFilesAlone(void) ;
extern void HandleEmptyFileListBoxes(void) ;
extern  unsigned int CountFilesToOperateOn(void );
extern void MarkFilesBeforeFileOp(void) ;
extern BOOL DeselectUnMatchedFiles(PTREE tree);

/* Note that unless we are in "DoFileOp" this should be NULL - default */
PENTRY gNextFileNode = NULL ;                   /* next node using snext in DoFileOp */

char *gpszDestPath ; // Destination path to perform file copy/move to.

/* In debug mode, with no loader the compiler chokes on this file!!! */
// #if 0
#ifndef NOLOADER
#include <prot.h>
#include <errno.h>
#else
extern void FormSelFileList(char *files, unsigned buflen) ;
extern  int MoveFile(struct th_t far *tree,struct fi_t far *node,char *path,int count,int total,int verify);
extern  int CopyFile(struct th_t far *tree,struct fi_t far *node,char *path,int count,int total,int verify);
extern  int DeleteFile(struct th_t far *tree,struct fi_t far *node,char *placeholder,int count,int total,int verify);
extern  int ChangeAttributes(struct th_t far *tree,struct fi_t far *node,char *placeholder,int count,int total,int verify);
extern  int PrintFile(struct th_t far *tree,struct fi_t far *node,char *placeholder,int count,int total,int verify);
extern  int ViewFile(struct th_t far *tree,struct fi_t far *node,char *unused1,int unused2,int unused3,int unused4);
extern int cdecl chdir(char *path);
extern  void DoFileOp(int type,char *path);
extern  int GetDestPath(char *path);
extern  int PromptAndCreateDir(struct th_t far *tree,struct fi_t far *parent);
extern  void InsertListItem(struct ListBoxData *TestList,unsigned short isz);
extern  void FocusLineChange(struct ListBoxData *TestList,int amt);
extern void CollapseDirVisibility(PTREE tree, PENTRY dir) ;
extern void ExpandDirVisibility(PTREE tree, PENTRY dir, BOOL fAllLevels) ;
extern  unsigned int GetIndexVisibleDir(struct fi_t far *dir,struct th_t far *tree);
extern  int Tree2Path(struct th_t far *tree,struct fi_t far *node,char *str, int *plength);
extern  void pascal far OutOfMemory(void );
extern TMC      MyTmcDoDlg(VOID *pdlg, HCAB hcab) ;
extern  void SetUpDialog(unsigned short tmcchild,char *title);
extern  void SetUpButtonForGraphics(unsigned short tmc);
extern  void SetUpEditBox(unsigned short tmc, BOOL fRealOrNot, WORD maxcount, BOOL fHasInitialFocus);
VOID FAR PASCAL Help(WORD hem, WORD hid,VOID *pv,WORD kk);
extern  int FindLastComponent(char *path);
extern  char *DOSErrorMsg(int errcode,int *box);
extern  int DOSErrorBox(char *statusline,unsigned short err,unsigned int helpid);
extern  struct th_t far *FindTree(char *path, BYTE *driveind);
extern  int RenameFile(struct th_t far *tree,struct fi_t far *node,char *prognew,int count,int total,int ask);
extern  void com1(char *str);
extern  void com1i(int i);
extern  int FindAndDelDirectory(void );
extern  unsigned int GetTreeSelectedInfo(struct th_t far *tree,unsigned long far *psize);
extern  void FileMgrStatusBar(struct th_t far *tree,struct fi_t far *node);
extern  struct fi_t far *GetNthVisibleDir(struct th_t far *tree,int count);
extern  int Internal2Normal(char far *dest,char far *src);
extern  void DeselectTree(struct th_t far *tree);
BOOL TakeDownStatusMessage(int count, int total) ;
extern  int CompactifyTree(struct th_t far *tree,int sel);
#define ENOENT 2
#endif

BOOL DestIsDir;

VOID FAR DoCopyFiles(void)
{
	char path[MAX_PATH+1];                          // destination path

	gpszFileOpCaption = szCopyFileCaption ;
	((PDLG)&dlgCopyFiles)->hid = hidCOPY;

	if (GetDestPath(path))
		DoFileOp(OP_COPY, path);
}

VOID FAR DoMoveFiles(void)
{
	char path[MAX_PATH+1];                          // destination path

	gpszFileOpCaption = szMoveFileCaption ;
	((PDLG)&dlgCopyFiles)->hid = hidMOVE;

	if (GetDestPath(path))
		DoFileOp(OP_MOVE, path);
}

VOID FAR DoRenameFiles(void)
{
	/* Rename can be Rename Directory or Rename File. By default we
	   set it to RenameFile. It will be set by Rename Directory guy
	   if that happens to be the operation! */

	gpszFileOpCaption = szRenameFileCaption ;

	DoFileOp(OP_RENAME, NULL);
}

extern BOOL DisplayedFilesOk(void) ;   /* actually a forward declarartion */
extern int AssociateBox(PTREE tree,PENTRY node,char *path, int count, int total, BOOL verify);

VOID FAR DoDelFiles(void)
{
	/* Delete can be Delete Directory or Delete File. By default we
	   set it to DeleteFile. It will be set by Delete Directory guy
	   if that happens to be the operation! */

	gpszFileOpCaption = szDelFileCaption ;
	DoFileOp(OP_DELETE, NULL);                 // NULL is unused arg
}

VOID FAR DoViewFile(void)
{
	gpszFileOpCaption = szViewFileCaption ;
	DoFileOp(OP_VIEW, NULL);
}

VOID FAR DoChangeAttributes(void)
{
	gpszFileOpCaption = szChangeFileCaption ;
	DoFileOp(OP_CHANGE, NULL);
}

VOID FAR DoPrintFiles(void)
{
	gpszFileOpCaption = szPrintFileCaption ;
	DoFileOp(OP_PRINT, NULL);
}

VOID FAR DoCreateDirectory(void)
{
	PTREE tree ;
	PENTRY parent ;
	int dirlinefocus, dirlineother ;

	tree = listinfo[glob.FocusBox].tree ;
	parent = listinfo[glob.FocusBox].files ;

	dirlinefocus = glob.lineselected[glob.FocusBox] ;
	dirlineother = glob.lineselected[1-glob.FocusBox] ;

	gpszFileOpCaption = szCreateDirCaption ;

	if (PromptAndCreateDir(tree, parent))
	{
		/* Directory succesfully created */
		if (glob.TreeMode != TR_SYSTEM)
		{
			InsertListItem(&TreeList[glob.FocusBox], 0) ;
			if ( (glob.TreeMode == TR_DOUBLE) && 
						(listinfo[1-glob.FocusBox].tree == tree) )
			{
				/* Because, we have created a new dir, did the previously
				 * selected directory move down by 1 line? If so move the
				 * focus back to it.
				 */
				if (dirlineother > dirlinefocus)
					FocusLineChange(&TreeList[1-glob.FocusBox], +1) ;

				InsertListItem(&TreeList[1-glob.FocusBox], 0) ;
			}
		}

	}
} /* DoCreateDirectory */

void DoExpand1Level(void)
{
	PTREE tree ;
	PENTRY dir ;

	tree = listinfo[glob.FocusBox].tree ;
	dir = listinfo[glob.FocusBox].files ;

	/* ZZZZZZ */
	CollapseDirVisibility(tree, dir) ;
	ExpandDirVisibility(tree, dir, FALSE) ;

	InsertListItem(&TreeList[glob.FocusBox], glob.lineselected[glob.FocusBox]);

	/* Update the other listbox also correctly */
	if ((glob.TreeMode == TR_DOUBLE) && (tree == listinfo[1-glob.FocusBox].tree))
		InsertListItem(&TreeList[1-glob.FocusBox], 0);

} /* DoExpand1Level */

void DoExpandBranch(void)
{
	PTREE tree ;
	PENTRY dir ;

	tree = listinfo[glob.FocusBox].tree ;
	dir = listinfo[glob.FocusBox].files ;

	/* ZZZZZZ */
	CollapseDirVisibility(tree, dir) ;
	ExpandDirVisibility(tree, dir, TRUE) ;
	InsertListItem(&TreeList[glob.FocusBox], glob.lineselected[glob.FocusBox]);

	/* Update the other listbox also correctly */
	if ((glob.TreeMode == TR_DOUBLE) && (tree == listinfo[1-glob.FocusBox].tree))
		InsertListItem(&TreeList[1-glob.FocusBox], 0);

} /* DoExpandBranch */
	
void DoExpandAll(void)
{
	PTREE tree ;

	tree = listinfo[glob.FocusBox].tree ;

	/* ZZZZZZ */
	CollapseDirVisibility(tree, NULL) ;
	ExpandDirVisibility(tree, NULL, TRUE) ;
	glob.lineselected[glob.FocusBox] =
					GetIndexVisibleDir(listinfo[glob.FocusBox].files,
									   listinfo[glob.FocusBox].tree);
	InsertListItem(&TreeList[glob.FocusBox], 0);

	/* Update the other listbox also correctly */
	if ((glob.TreeMode == TR_DOUBLE) && (tree == listinfo[1-glob.FocusBox].tree))
		InsertListItem(&TreeList[1-glob.FocusBox], 0);

} /* DoExpandAll */

void  DoCollapse(void)
{
	PTREE tree ;
	PENTRY dir ;

	tree = listinfo[glob.FocusBox].tree ;
	dir = listinfo[glob.FocusBox].files ;

	CollapseDirVisibility(tree, dir) ;
	InsertListItem(&TreeList[glob.FocusBox], glob.lineselected[glob.FocusBox]);

	/* Update the other listbox also correctly */
	if ((glob.TreeMode == TR_DOUBLE) && (tree == listinfo[1-glob.FocusBox].tree))
		InsertListItem(&TreeList[1-glob.FocusBox], 0);

}

/* The following is the number of characters that will be displayed in
   the dialog box that requests a destination path name to copy/move files */

#define MAXFILELIST 256

/****   GetDestPath - get destination path for copy or move
**
**      ENTRY
**              path - place to store path
**      EXIT
**              TRUE if successful, else FALSE.
*/
BOOL GetDestPath(char *path)
{
	HCABCopyFiles hcab;                             // needed for dialogue box
	TMC tmc;                                                // return value of dialogue box
	BOOL ret;                                               // return value
	int dummylen ;

	/* space for MAXFILELIST chars + NULL + an extra name + BLANK */
	char FileList[MAXFILELIST+1+(NAMELEN+EXTLEN+1)+1] ;
	char DefaultPath[MAX_PATH+1] ;

	// Init dlg box with empty string.
	hcab = HcabAlloc(cabiCABCopyFiles);
	if (!hcab)
	{
		OutOfMemory() ;
		return FALSE;
	}
	InitCab(hcab, cabiCABCopyFiles);
	FormSelFileList(FileList, MAXFILELIST) ;

	Tree2Path(listinfo[glob.FocusBox].tree, listinfo[glob.FocusBox].files, 
								DefaultPath, &dummylen) ;
	SzToCab(hcab, FileList, Iag(CABCopyFiles, szCopyFileList));
	SzToCab(hcab, DefaultPath, Iag(CABCopyFiles, szCopyDest));
	
	SzToCab(hcab, szEnterButton, Iag(CABCopyFiles, pszcopyEB));
	SzToCab(hcab, szCancelButton, Iag(CABCopyFiles, pszcopyCB));
	SzToCab(hcab, szHelpButton, Iag(CABCopyFiles, pszcopyHB));

	/* This is the location where the dialog call below will place the
	 * DestPath (absolute path).
	 */
	gpszDestPath = path ;

	// Run dlg box.
	tmc = MyTmcDoDlg(&dlgCopyFiles, hcab);
	if (tmc == tmcOK)
	{
		// SzFromCab(hcab, path, MAX_PATH, Iag(CABCopyFiles, szCopyDest));
		/* The path from dialog box has been pre-processed and converted to
		 * an absolute path including the drive letter in gpszDestPath.
		 * It will also be in upper case.
		 */
		// strupr(gpszDestPath);
		ret = TRUE;
	} else
		ret = FALSE;
	FreeCab(hcab);
	return ret;
} /* proc GetDestPath */

/****   FDlgCopyFiles - FDlg procedure to get destination path
**          The dialogue box handled by this procedure asks for the
**      destination path of a copy.
**
**      ENTRY
**              see SDM API v.2.0 doc's
**      EXIT
**              see SDM API doc's
**      WARNING:
**              The verification of the path should be done by looking at the disk,
**      not by examining the in-memory tree, because we need to be able to copy
**      and move files to disks whose trees are not currently in memory.
**      EFFECTS:
**
*/
BOOL FAR PASCAL FDlgCopyFiles(WORD dlm, TMC tmc,
									WORD wNew, WORD wOld, WORD wParam)
{
	int result;                                                             // dos call return value
	BOOL ret;                                                               // fn return value
	int count;                                                              // number of selected files
	char dlg_path[1+MAX_PATH];                              // full path of file
	unsigned attr;                                                  // file's attributes
	int temp ;
	int len ;

	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	switch (dlm)
	{
	case dlmSetFocus:
	    gCurrentTMC = tmc;
		break ;

	case dlmInit:
		SetUpDialog(tmcOK,gpszFileOpCaption);

		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);
		SetUpButtonForGraphics(tmccopyhelp);

		/* The user should not be able to edit this */
		SetUpEditBox(tmcCopyFileList, FALSE,255, FALSE);

		SetUpEditBox(tmcCopyDest, TRUE, USERS_MAX_TYPEABLE_PATH, TRUE);

		SetDefaultTmc(tmcOK);
		SetFocusTmc(tmcCopyDest);
		break ;

	case dlmClick:
		 if(tmc == tmccopyhelp)
			Help(hemDialog, ((PDLG)&dlgCopyFiles)->hid,NULL,0);

		 SetFocusTmc(gCurrentTMC) ;
		 break ;

	case dlmTerm:
		if (tmc == tmcOK)
		{
			ret = FALSE ; /* default value */

			/* On exit, verify path. */
			GetTmcText(tmcCopyDest, dlg_path, MAX_PATH);

			Convert2AbsolutePath(gpszDestPath, dlg_path) ;

			if (gpszDestPath[1] != ':')                                     // no drive specifier
			{
				Shell_SetTmcText(tmcCopyMsg, szNeedDrive);
			} else if ((len = strlen(gpszDestPath)) < 3)            // not long enough to be valid
			{
				Shell_SetTmcText(tmcCopyMsg, szNeedPath);
			} else if (gpszDestPath[3] == EOS)                      // root path
			{
				if (chdir(gpszDestPath))
				{
					Shell_SetTmcText(tmcCopyMsg, szPathNotFound);
				} else
				{
					DestIsDir = TRUE;
					ret = TRUE;
				}
			}
			// If it is not a root path, we should not have a '\\' at the end!
#ifdef DBCS
			else if (gpszDestPath[len-1] == '\\' && !CheckDBCSTailByte(gpszDestPath,&gpszDestPath[len-1]))
#else
			else if (gpszDestPath[len-1] == '\\')
#endif
			{
				Shell_SetTmcText(tmcCopyMsg, szPathNotFound) ; 
			}
			else            // something else
			{
				result = _dos_getfileattr(gpszDestPath, &attr);
				if (!result)
				{
					/* Determine copy type. */
					if (attr & _A_SUBDIR)
					{
						DestIsDir = TRUE;
						ret = TRUE;
					} else
					{
						DestIsDir = FALSE;
// We don't check for ReadOnly anymore, as we have ReplaceConfirmationDialog
// that would be always put up even if we had confirmation turned off!
#if 0
						if (attr & _A_RDONLY)
							Shell_SetTmcText(tmcCopyMsg, szReadOnly);
						else
#endif
							ret = TRUE;
					}
				} else if (result == ENOENT)            // file not found
				{
					char replaced_char ;

					/* Not found - try parent */
					temp = FindLastComponent(gpszDestPath) ;

					replaced_char = gpszDestPath[temp] ; // store char before replacing with NULL
					gpszDestPath[temp] = EOS;

					/* Is parent the ROOT dir? If so, _dos_getfileattr call
					 * fails on it. We avoid making this call as we know that
					 * the root is always a valid directory!
					 */
					if (gpszDestPath[temp-1] == PATHCHAR)
					{
						DestIsDir = FALSE;
						ret = TRUE ;
					}
					else
					{
						result = _dos_getfileattr(gpszDestPath, &attr);
						if (!result)
						{
							/* sz doesn't exist but its parent does, so he's
							 * asking to copy stuff to the *file* in sz.
							 */
							DestIsDir = FALSE;
							ret = TRUE;
						} 
						else
						{
							/* Parent wasn't found, either. He'd better try again. */
							Shell_SetTmcText(tmcCopyMsg, DOSErrorMsg(result,NULL));
						}
					}

					gpszDestPath[temp] = replaced_char ; // put back the replaced char
				} 
				else
					Shell_SetTmcText(tmcCopyMsg, DOSErrorMsg(result, NULL));
			}
	
			if (ret && !DestIsDir)
			{
				count = CountFilesToOperateOn() ;
	
				if (count > 1)
				{
					/* He's trying to copy > 1 file to a file; chastise him.
					*/
					Shell_SetTmcText(tmcCopyMsg, szTooManyFiles);
					ret = FALSE;
				}
			}
			return ret;
		}
	default:
		break ;
	} /* switch */

	return TRUE;
} /* proc FDlgCopyFiles */

/****   DoFileOp - Perform appropriate fileop
**              This fn handles copying and deletion of selected files.
**
**      ENTRY
**                      type - operation type: OP_COPY, OP_MOVE, or OP_DELETE.
**                      path - OP_DELETE: scratch buffer, must be (MAX_PATH+1) long
**                                 OP_MOVE/OP_COPY, one file selected: destination filespec
**                                 OP_MOVE/OP_COPY, multiple files: destination directory
**      EXIT
**                 none
**      WARNING
**          There must be room in 'path' to add another character for move or copy.
**      Each function which can be called through 'filefn' must have the given
**      number and type of formal parms.
**
**      NOTE
**              The file operation functions should return one of the following:
**                      ACT_OK          if the operation succeeded
**                      ACT_NOMEM       if the fn ran out of memory (be able to retry)
**                      ACT_SKIP        if user chose "Skip file & continue" from error dbox
**                      ACT_CANCEL      if the entire operation should be aborted.
**              Note that ACT_FORCE should not be returned; coerce it to ACT_OK.
*/
VOID DoFileOp(int type, char *path)
{
	int pathlen;                                                    // length of path w/o file
	treehdr far *tree;
	PENTRY node;                                                    // current source file
	int ret;                                                                        // return value of fileop.
	int total;                                                              // number of selected files
	int count;                                                              // number of processed files
	int i ;
	int (*filefn)(treehdr far *, PENTRY, char *, int, int, WORD) ;
	WORD flags;                                                             // flags to pass to fileop fn
	BOOL retry;

	total = CountFilesToOperateOn() ;

	if (FPerformDirOperation())
	{
		tree = listinfo[glob.FocusBox].tree ;
		node = listinfo[glob.FocusBox].files ;

		if (!node)
		{
			/* Can't rename/delete the ROOT dir. Actually, the menu should
			 * be disabled at this time.
			 */
			Beep() ;
			return ;
		}

		switch (type)
		{
			case OP_RENAME:

			/* The new path name should be found by Rename so ask is set
			   to TRUE (last param) and passed in path is NULL (param 3)! */

				gpszFileOpCaption = szRenameDirCaption ;

				/* We pass 0, 0 for count & total resp., to inform
					RenameFile that it is renaming a dir & not a file */
				if (RenameFile(tree, node, NULL, 0, 0, TRUE) == ACT_OK)
				{
					/* After renaming a directory the focus goes to the root.
					 * This is windows behaviour too.
					 */
					glob.lineselected[glob.FocusBox] = tree->SelLine = 0 ;
					listinfo[glob.FocusBox].files = NULL ;

					if (tree->Compacted)
					{
						ClobberDir(tree, node) ;
						LoadCompactDir(tree, NULL) ;
					}

					/* The directory that was renamed could have files selected
					 * in it -- We don't want these to remain so as the user
					 * will now be looking at the ROOT directory and can
					 * accidentally delete these other files.
					 */
					if (!glob.CrossDirSel)
						DeselectTree(tree) ;

				FileMgrStatusBar(tree, NULL ) ;
					ListBoxPath(tree, NULL, glob.FocusBox) ;
					Set_List_Focus(&TreeList[glob.FocusBox], 0);
					FocusLineChange(&TreeList[glob.FocusBox], 0);
					listinfo[glob.FocusBox].UpdateFiles = TRUE ;
					InsertListItem(&TreeList[glob.FocusBox], 0) ;

					/* If in Dual Tree mode and same tree in both listboxes,
					 * update that box too.
					 */
					if ( (glob.TreeMode == TR_DOUBLE) &&
						  (listinfo[0].tree == listinfo[1].tree) )
					{
						glob.lineselected[1-glob.FocusBox] = 0 ;
						listinfo[1-glob.FocusBox].files = NULL ;
						ListBoxPath(tree, NULL, 1-glob.FocusBox) ;

						Set_List_Focus(&TreeList[1-glob.FocusBox], 0    );
						FocusLineChange(&TreeList[1-glob.FocusBox], 0);
						listinfo[1-glob.FocusBox].UpdateFiles = TRUE ;
						
						InsertListItem(&TreeList[1-glob.FocusBox], 0) ;
					}
				}
				break ;

			case OP_DELETE:
		
				/* ZZZZZ No  error checks done!! */
				gpszFileOpCaption = szDelDirCaption ;
				ret = FindAndDelDirectory() ;
				/* The following is done on succesful deletion by callee!*/
				// InsertListItem(&TreeList[glob.FocusBox], 0) ;
				break ;

#ifndef NOCONSISTENCY
			default:
				printf("**** ???? req on non-root dir -> 0 selections\n") ;
				exit(0) ;
#endif
		} /* switch */
		return ;
	} /* Perform Directory Operation */

	switch (type)
	{
		case OP_DELETE :
			/* we are trying to delete a bunch of files. Make sure the list is
		 * what the user expects. If (total == 1), we don't have to do this.
			 * We can move to the Delete File operation promptly.
			 */
			if ( (total > 1) && (!DisplayedFilesOk()) )
				return ;

			filefn = DeleteFile;
			// flags = glob.VerifyDelete;
			// Fn DeleteFile() will look at glob.VerifyDelete if this is TRUE!
			flags = TRUE ;
			break ;

		case OP_MOVE:
		case OP_COPY:
		/* If we are copying a single file, path contains the full 
			destination. If more than one file, path contains only the
			destination directory. */
			pathlen = strlen(path);

			/* Append backslash to path if there isn't one already. */
			if (total > 1 || !chdir(path))
			{
#ifdef DBCS
				if (path[pathlen-1] != PATHCHAR ||
					CheckDBCSTailByte(path,&path[pathlen-1]))
#else
				if (path[pathlen-1] != PATHCHAR)
#endif
				{
					path[pathlen] = PATHCHAR;
					pathlen++;
					path[pathlen] = EOS;
				}
			}
			filefn = (type == OP_MOVE) ? MoveFile : CopyFile;
			flags = glob.VerifyOverwrite;
			break ;

		case OP_VIEW:
			filefn = ViewFile;
			flags = 0;                              // no flags to consider
			break;

		case OP_RENAME:
			filefn = RenameFile;
			flags = TRUE;                   // prompt for new filespec
			break;

		case OP_CHANGE:
			filefn = ChangeAttributes ;
			flags = 0 ; /* no flags to consider */
			break ;

		case OP_PRINT:
			filefn = PrintFile ;
			flags = 0 ; /* no flags to consider */
			break ;
		case OP_ASSOCIATE:
			filefn = AssociateBox;
			flags = 0;                              // no flags to consider
			break ;
	} /* switch */

	ret = ACT_OK;
	count = 0;

	tree = listinfo[glob.FocusBox].tree;
	node = tree->FirstFile;

	MarkFilesBeforeFileOp() ;

	for ( ; node && (ret != ACT_CANCEL) && (count < total) ; node = gNextFileNode )
	{
		/* The reason we do so here instead of at the end of the
			loop is because the file operations delete and add to snext
			in appropriate order. Also 'node' could be deleted */
		gNextFileNode = node->x.f.snext ;

		if (node->FILEOPMARKER)
		{
			/*  'count' of 'total' */
			count++ ;

			if ( (type == OP_COPY)  || (type == OP_MOVE) )
			{
					/* create the new destination path */
					if (DestIsDir)
					{
						Internal2Normal(path+pathlen, node->name);
					}
			}

			retry = TRUE;
			while (retry)
			{
				retry = FALSE;

				ret = (*filefn)(tree, node, path, count, total, flags);

				if (ret == ACT_NOMEM)
				{
					PTREE thetree;
					BOOL ok;

					/* Ran out of memory.  Compact a non-displayed tree
					** and try again.
					*/
					for (thetree=glob.drives; thetree && !retry; thetree=thetree->next)
					{
						ok = TRUE;
						if (listinfo[0].tree == thetree)
							ok = FALSE;
						if (glob.TreeMode == TR_DOUBLE)
							if (listinfo[1].tree == thetree)
								ok = FALSE;
						if (ok)
							retry = CompactifyTree(thetree, FALSE);
					}
				}
			} /* while (retry) */
		} /* node->FILEOPMARKER is TRUE */
	} /* for loop across all file nodes */

	/* Put it back to default state - fn HandleDeletesnext() looks at it */
	gNextFileNode = NULL ;

	/* Actually, the following boolean will be TRUE only if the user hit the*/
	/* CANCEL button before the file-operation could be attempted on the last*/
	/* of a bunch of selected files.                                                                                */
	if (gfStatusMsgUp)
	{
		/* If I call following fn with both arguments same, it will take        */
		/* down the status message box.                                                                         */
		TakeDownStatusMessage(total, total) ;
	}

	if (ret != ACT_CANCEL)
	{
		/* Everything finished ok, clear the selected lists. */
		// DeselectTree(listinfo[glob.FocusBox].tree) ;

		/* Operations were completed succesfully -- Toggle back to
		 * Implicit selection mode in case the user is in Explicit
		 * Selection Mode for these file listboxes.
		 */
		if (!FileList[glob.FocusBox].mode)
		{
			FileList[glob.FocusBox].mode = TRUE ;
			FileList[glob.FocusBox].ListProc(tmmImplicit, NULL,0,
										FileList[glob.FocusBox].tmc,0,0,0);
		}
	} /* everything finished OK */

	/* actually assumes that glob.MaxTree is either 0 or 1 */
	for (i = 0 ; i <= glob.MaxTree ; i++)
	{
		if ( (i != 1) || (listinfo[0].tree != listinfo[1].tree) )
			listinfo[i].tree->NumSel = GetTreeSelectedInfo(listinfo[i].tree,
												&(listinfo[i].tree->SizeSel)) ;

		/* In SYSTEM tree mode, files are already inserted in correct
		 * sorted order.
		 */
		if (glob.TreeMode != TR_SYSTEM)
			SortDirectory(listinfo[i].tree, listinfo[i].files) ;

		InsertListItem(&FileList[i], 0) ;
	}

	/* Send select message to focus item in file listbox if in Implicit
	 * selection mode and no files are selected now.
	 */
	if ( (FileList[glob.FocusBox].mode) && (CountFilesToOperateOn() == 0) )
	{
		ListKey(&FileList[glob.FocusBox], ' ', 0) ;
	}

	HandleEmptyFileListBoxes() ;

	/* Operations like File.ChangeAttributes with say "Display Hidden/System
	 * files" turned OFF can potentially leave some files selected that
	 * are visible to the user. Deselect these!
	 */
	if (DeselectUnMatchedFiles(listinfo[glob.FocusBox].tree))
	{
		listinfo[glob.FocusBox].UpdateFiles = TRUE ;
		if ( (glob.TreeMode == TR_DOUBLE) &&
					(listinfo[0].tree == listinfo[1].tree) )
			listinfo[1-glob.FocusBox].UpdateFiles = TRUE ;
	}

} /* proc DoFileOp */


/* Forms name-list of selected files in the buffer 'files'. The caller needs
   a maximum of 'buflen' non-null characters in this buffer.
   File names are separated by a blank, and 'files' is null terminated.

WARNING! 'files' is assumed to have storage for buflen + 1 (for NULL) +
		 12 bytes for an extra file name */

void FormSelFileList(char *files, unsigned buflen)
{
	unsigned total, count ;
	int len ;
	char *filebound ;
	PENTRY node ;
	PTREE tree ;

	total = CountFilesToOperateOn() ;

	if (total > 0)
	{
		filebound = files + buflen ;
		count = 0 ;

		MarkFilesBeforeFileOp() ;
		tree = listinfo[glob.FocusBox].tree ;

		/* Locate all files to operate on and append upto buflen charecters */
		for (node = tree->FirstFile ; node && (count < total) ;
																	node = node->x.f.snext )
		{
			/* Has file been marked to perform the file-op? */
			if (node->FILEOPMARKER)
			{
				count++ ;
				len = Internal2Normal(files, node->name) ;                                                              
				files += len ;

				*(files++) = ' ' ;

				if (files >= filebound)
				{
					*filebound = '\0' ;
					return ;
				}
			} /* if */
			/* else just skip to next file */
		} /* node for loop */
	} /* total > 0 */       

	*files = '\0' ;

} /* FormSelFileList */

/* ZZZZZ */
/* This function is be very similar to GetDestPath -- maybe they can be
   combined to reduce code size!! */
BOOL DisplayedFilesOk()
{
	HCABdelfiles hcab ;
	TMC tmc ;

	char FileList[MAXFILELIST+1+(NAMELEN+EXTLEN+1)+1] ;

	hcab = HcabAlloc(cabiCABdelfiles) ;
	if (!hcab)
	{
		OutOfMemory() ;
		return FALSE ;
	}
	InitCab(hcab, cabiCABdelfiles) ;
	FormSelFileList(FileList, MAXFILELIST) ;

	SzToCab(hcab, FileList, Iag(CABdelfiles, szdelfilelist));
	SzToCab(hcab, szEnterButton, Iag(CABdelfiles, pszdelfilesEB));
	SzToCab(hcab, szCancelButton, Iag(CABdelfiles, pszdelfilesCB));
	SzToCab(hcab, szHelpButton, Iag(CABdelfiles, pszdelfilesHB));

	tmc = MyTmcDoDlg(&dlgdelfiles, hcab) ;

	FreeCab(hcab) ;

	return (tmc == tmcOK) ;

} /* DisplayedFilesOk */

BOOL FAR PASCAL FDlgdelfiles(WORD dlm, TMC tmc,
											WORD wNew, WORD wOld, WORD wParam)
{
	UnReferenced(tmc) ;
	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	/* Actually help is the only case to be handled here. Otherwise,
	   we just dismiss the dialog and caller looks at the tmc value on exit */
	if (dlm == dlmInit)
	{
		SetUpDialog(tmcOK,szDelFileCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);
		SetUpButtonForGraphics(tmcdelfileshelp);

		/* The user should not be able to edit this list! */
		SetUpEditBox(tmcdelfilelist, FALSE, 256, FALSE);

		gCurrentTMC = tmcOK;
	}
	else
	if (dlm == dlmSetFocus)
	{
	    gCurrentTMC = tmc;
	}
	else
	if (dlm == dlmClick)
	{
		if(tmc == tmcdelfileshelp)
			Help(hemDialog, ((PDLG)&dlgdelfiles)->hid,NULL,0);

		SetFocusTmc(tmcOK) ;
	}


	return TRUE;
} /* proc FDlgdelfiles */
