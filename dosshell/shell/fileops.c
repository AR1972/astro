/* Revision history since 5.0 Golden:
 *
 *  M012 SHK 07/31/91 Fixed bug in fn CopyIt(). It used to screw up
 *							 if file size was between 64K-512 and 64K-256
 *
 */

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****   fileops.c - fns to execute each operation on selected files
**
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  8/16/89       t-jeffro      Created file
**  8/28/89   t-jeffro  added loads of error handling
**  10/??/89  harikris  Changed lots and lots, added several new functions.
*/
#include <ctype.h>
#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <text.h>
#include <help.h>
#include <fileopts.hs>
#include <fileopts.sdm>
#include <rename.hs>
#include <rename.sdm>

#include <assert.h>

#define COPY_SUCCESS                            100
#define COPY_SRC_OPEN_ERR               2
#define COPY_DST_CREAT_ERR      4
#define COPY_RD_WR_ERR                  6

extern BYTE ErrorCrit ;

extern char * PASCAL MySetMessageText(TMC tmc, char *message, int nWid) ;
extern void Marklastdir(PTREE tree) ;
extern void InsertDirAndMarkLastDirBits(PTREE tree, PENTRY parent, PENTRY newdir) ;
extern void InsertDir(PTREE tree, PENTRY parent, PENTRY newdir) ;
extern PENTRY HandleDeletednext(PTREE tree, PENTRY deldir) ;
extern VOID easycheck(int (FAR *theproc)(), BOOL docheck) ;
extern int ReplaceConfirmationDialog(char *srcpath, char *destpath, 
											PENTRY srcnode, struct find_t destfindinfo) ;
extern int DeleteConfirmationDialog(char *dpath, BOOL fIsDelFile, char *Message) ;
extern BOOL FPerformOpOnDirFilesAlone(void);

/* ZZZZ should I use a different error number & provide a specific help message
 * on this error as this is a new feature!
 */
#define NESTING_DEEP_ERR 5

#define m_fPerformingMoveFile() (gpszFileOpCaption == szMoveFileCaption)

/* We try to allocate a copy buffer size to be a multiple of this number,       */ 
/*      which is the sector size!                                                                                               */
#define BUFSIZE 512 

/****   DeleteFile - delete one file with verification
**
**      ENTRY
**              tree  - tree containing file to delete
**              node  - node of file to delete
**              placeholder - just that
**              total - number of selected files
**              count - index in selected files
**              verify- if FALSE, do not put up any verification dialog box, else
**                                put it up in case glob.VerifyDelete is TRUE.
**      EXIT
**              Action code from GetResponse().
**      WARNING
**              This fn's argument list must match that in DoFileOp(), as this fn is
**      called through a ptr by DoFileOp().
*/      
/* ZZZZ placeholder unused. */
int DeleteFile(PTREE tree, PENTRY node, char *placeholder, int count, 
																	int total, BOOL verify)
{
	int action;
	char path[MAX_PATH+1];                                  // path of file
	char statusline[STATUS_LEN];
	BOOL fIPutMsgUp ;
	int dummylen ;

	UnReferenced(placeholder) ;
	Tree2Path(tree, node, path, &dummylen);

	FormCountStatusLine(statusline, szOpDelFile, path, count, total, 
													STATUS_COUNT_START) ;
	action = ACT_FORCE;
	if (verify)
	{
		/* Always prompt if Read Only file in case "verify" is TRUE */
		if (node->attribs & _A_RDONLY)
		{
			action = DeleteConfirmationDialog(path, TRUE, szWarningReadOnly) ;
		}
		// else
		else if (glob.VerifyDelete)
		{
			action = DeleteConfirmationDialog(path, TRUE, NullString) ;
		}
	}
	switch (action)
	{
		case ACT_FORCE: 
		case ACT_OK:    
				/* If gfStatusMsg is TRUE, A status message has already been
					put up (by MoveFile)!! Note that this fn (DeleteFile) is
					called to do the 'DeleteFile' menu operation and also by
					MoveFile as part of its total operation!
				*/
				if (!m_fPerformingMoveFile())
				{
					fIPutMsgUp = TRUE ;
					PutUpStatusMessage(statusline, count) ;
				}
				else
					fIPutMsgUp = FALSE ;

				action = KillFile(statusline, path);

				/* If we didn't put up the message, we shouldn't take it off.
					It is the task of the person who actually put it up.
				*/
				if (fIPutMsgUp)
				{
					TakeDownStatusMessage(count, total) ;
				}
		default:
				break;
	}
	if (action == ACT_FORCE)
		return ACT_OK;
	else
		return action;
} /* proc DeleteFile */

/****   KillFile - delete a file even if it is read-only
**
**              This fn removes any read-only status from the given file and deletes it.
**
**      ENTRY
**                      path - file to excise
**  EXIT
**                      ACT_OK, ACT_SKIP or ACT_CANCEL.
**      WARNING
**                      The file must exist.
**      NOTE
**                      The file is removed from its in-memory tree also.
*/
int KillFile(char *statusline, char *path)
{
	treehdr far *tree;                                              // tree containing file
	PENTRY node;                                            // file's tree entry
	PENTRY parent;                                          // file's parent
	unsigned attr;                                          // file's attributes
	int code;                                                       // return code from dos call
	int action;                                                     // user's requested action
	BYTE dummy ;

	do {
		code = _dos_getfileattr(path, &attr);
		if (code)
			action = DOSErrorBox(statusline, code, HELP_ERRGETATTR);
		else
			action = ACT_OK;
	} while (action == ACT_RETRY);
	if (action != ACT_OK && action != ACT_FORCE)
	{
		return ( (action == ACT_SKIP) ? ACT_SKIP : ACT_CANCEL ) ;
	} else
	{
		/* This function cannot handle a DeleteDirectory! This problem can
		 * if the user has changed the floppy say and the new disk has a
		 * directory the same name as the file in the original disk.
		 */
		if (attr & _A_SUBDIR)
		{
			return ACT_CANCEL ;
		} else if (attr & _A_RDONLY)
		{
			do {
				ErrorCrit = 0xFF ;
				code = _dos_setfileattr(path, 0);
				if (code)
				{
					/* Did a critical error occur? -- say, disk write protected! */
					if (ErrorCrit != 0xFF)
						action = GetResponse(statusline, 
											szCriticalMessages[ErrorCrit], 
											BT_FILERETRYSKIPQUIT, HELP_ERRSETATTR);
					else
						action = DOSErrorBox(statusline, code, HELP_ERRSETATTR);
				}
				else
					action = ACT_OK;
			} while (action == ACT_RETRY);

			if (action != ACT_OK && action != ACT_FORCE)
			{
				return ( (action == ACT_SKIP) ? ACT_SKIP : ACT_CANCEL ) ;
			}
		}

		do {
			ErrorCrit = 0xFF ;
			code = unlink(path);
			if (code)
			{
				/* Was there a critical error during this operation -- like
				 * say "write-protect" disk, etc.
				 */
				if (ErrorCrit != 0xFF)
					action = GetResponse(statusline, szCriticalMessages[ErrorCrit], 
												BT_FILERETRYSKIPQUIT, HELP_ERRDELFILE);
				else
					/* We could execute the following code on a read-only share
					 * where we don't have access rights!
					 */
					action = DOSErrorBox(statusline, errno, HELP_ERRDELFILE) ;
			}
			else
				action = ACT_OK;
		} while (action == ACT_RETRY);

		if (code)
		{
			return action;
		} else
		{
			tree = FindTree(path, &dummy);

			/* If tree has been fully read in, then update tree data structure
			 * Note that if we tried to copy/move files to a tree that has
			 * not been read in, we call JunkTree() on it anyway. One
			 * cannot call DeleteFile on a tree that has not been read in. We
			 * allow DeleteFile() only if user has explicitly selected files
			 * from within the SHELL. That field is non-editable.
			 */
			if ( tree && (tree->Started) && (!tree->ContinueTree) )
			{
				tree->Diskfilecount-- ; // one less file on disk as unlink succeeded

				if (2 == FindNode(tree, path, &parent, &node, FALSE))
					DelFileEntry(tree, parent, node);
			}

			return ACT_OK ;
		}
	}
} /* proc KillFile */

/****   CopyFile - copy one file with verification
**              This fn copies the file specified by 'tree' and 'node' into 'path'.
**      ENTRY
**              tree  - tree containing file to copy
**              node  - node of file to copy
**              path  - destination file name
**              total - number of selected files
**              count - index in selected files
**              verify- pop up verification dbox on overwrite
**      EXIT
**              Action code from GetResponse().
*/      
int CopyFile(PTREE tree, PENTRY node, char *path, int count, 
																		int total, BOOL verify)
{
	int ret;                                                                // return code
	char oldpath[MAX_PATH+1] ;
	char statusline[STATUS_LEN];
	BOOL fIPutMsgUp ;
	struct find_t findinfo ;
	PTREE temptree ;
	PENTRY tempparent ;
	int dummylen ;

	Tree2Path(tree, node, oldpath, &dummylen);

	FormCountStatusLine(statusline, szOpCopyFile, oldpath, count, total, 
													STATUS_COUNT_START) ;

	/* ZZZZZZ get true name and then compare the two!!!! */
	/* See if we are trying to copy a file onto itself */
	if (!strcmp(path, oldpath))
	{
		// chastise the user saying that the file can't be copied onto itself.
		ret = GetResponse(statusline, szNoSelfCopy, BT_SKIPQUIT, HELP_NOSELFCOPY) ;
	}
	else if (_dos_findfirst(path, _A_HIDDEN | _A_SYSTEM, &findinfo))
		ret = ACT_OK;
	// If force is TRUE, skip the questionnaire dialogue box.
	else if (verify || (findinfo.attrib & _A_RDONLY) )
	{
		// file exists, ask what to do.
		ret = ReplaceConfirmationDialog(oldpath, path, node, findinfo) ;
	} else
		ret = ACT_FORCE;

	/* If gfStatusMsg is TRUE, A status message has already been
	 *      put up (by MoveFile)!! Note that this fn (DeleteFile) is
	 *      called to do the 'DeleteFile' menu operation and also by
	 *      MoveFile as part of its total operation!
	 */
	if ( (!m_fPerformingMoveFile()) && ( (ret == ACT_OK) || (ret == ACT_FORCE) ) )
	{
		fIPutMsgUp = TRUE ;
		PutUpStatusMessage(statusline, count) ;
	}
	else
		fIPutMsgUp = FALSE ;

	switch (ret)
	{
		case ACT_FORCE: 
				ret = KillFile(statusline, path);
				if (ret != ACT_OK)
					break ;
		case ACT_OK:    
				ret = CopyIt(tree, node, path, count, total);
		default:                
				break;
	}

	/* If we didn't put up the message, we shouldn't take it off.
	 *      It is the task of the routine which actually put it up.
	 */
	if (fIPutMsgUp)
		TakeDownStatusMessage(count, total) ;

	if (ret == ACT_FORCE)
		ret = ACT_OK;

	if (ret == ACT_OK)
	{
		fileinfo rec;                                   // copied file's info

		/* Build record of copied file.
		*/
		FillRecName(path, (PENTRY) &rec); // put name/ext into record
		rec.dtlx.dt.time = node->dtlx.dt.time;
		rec.dtlx.dt.date = node->dtlx.dt.date;

		rec.attribs = node->attribs;

		/* The ReadOnly bit is not preserved unless we do a MoveFile. See
		 * fn CopyIt().
		 */
		if (!m_fPerformingMoveFile())
			rec.attribs = rec.attribs & (~_A_RDONLY) ;

		rec.nosib = rec.SELECTED = 0;
		rec.x.f.size = node->x.f.size;

		/* AddLateFile will locate the proper tree and add our record to it.
		** If the tree is compacted, the record may not actually be added.
		*/
		/* we need to pass in non-null values (temptree and tempparent)
		 * because fn AddLateFile uses these storages.
		 */
		if (!AddLateFile(path, &rec, temptree, tempparent, 0))
			// ret = ACT_NOMEM;
			ret = ACT_CANCEL;
	}

	return ret;
} /* proc CopyFile */

/****   CopyIt - copy a single file
**      This fn takes a source and target path and copies the former into the
**      latter.
**
**      ENTRY
**              tree  - tree containing source file
**              node  - node of source file
**              dest  - target path
**              total - number of selected files
**              count - which one this is
**      EXIT
**              Action code from DOSErrorBox() if error, or ACT_OK if no errors.
**
**      WARNING:
**              This fn cannot write over a read-only file; the caller must delete the
**      existing file himself if that is the intended action.
**
**      EFFECTS:
**          Allocates and frees memory; does disk reads and writes.
*/
int CopyIt(PTREE tree, PENTRY node, char *dest, int count, int total)
{
	unsigned hsrc;                                                          // source file handle
	unsigned hdest;                                                         // target file handle
								
	unsigned rcnt;                                                          // number of bytes read
	unsigned wcnt;                                                          // # of bytes written
	void far *buf;                                                          // intermediate buffer
	unsigned bufsize;                                                       // size of temp buffer
								
	unsigned attr;                                                          // attributes of src file
	unsigned time;                                                          // timestamp of src file
	unsigned date;                                                          // datestamp of src file

	int code;                                                                       // return code of dos call
	int action;                                                                     // what to do about error
	char src[1+MAX_PATH];                                           // path of source file
	char statusline[STATUS_LEN];
	char distance = COPY_SUCCESS;                                           // how far did we get?
	BYTE dummy ;
	int dummylen ;

	Tree2Path(tree, node, src, &dummylen);
	
	FormCountStatusLine(statusline, szOpCopyFile, src, count, total, 
														STATUS_COUNT_START) ;
	/* Determine copy buffer size.
	*/

	// M012 fixed the following compare in the IF statement!
	/* Note that we can only read a maximum of 64K-1 at a time from the disk!
	 * So we try to allocate a buffer that is a multiple of sector size
	 * (512 bytes typically) and the maximum size this can be is
	 * 64K (0x10000L - 512) and still be a WORD!
	 */
	if (node->x.f.size >= (0x10000L-BUFSIZE))
		bufsize = 0x10000L-BUFSIZE ;
	else {
		/* Round size of buffer up to nearest sector - one hopes
		** that this will improve copy speed.
		*/
		unsigned u;

		bufsize = (unsigned) node->x.f.size;
		u = bufsize % BUFSIZE;
		if (u)
			bufsize = bufsize - u + BUFSIZE;
	}
	
	/* Try to allocate copy buffer.  If alloc fails, try a half-size
	** buffer.  Loop.
	*/
	buf = NULL;
	do {
		buf = LpbAllocWorkFar(bufsize);
		if (!buf)
			bufsize /= 2;
	} while (!buf && bufsize);

	if (!buf)
	{
		OutOfMemory() ;
		/* ZZZZ */
		/* Exit At this point -- there isn't even 1 byte of memory free */
		/* or try to run the compaction algorithms! */
	}

	/* Set up file handles.
	*/
	do {
		code = _dos_open(src, O_RDONLY | O_BINARY, &hsrc);
		if (code)
		{
			/* pop up dbox explaining problem */
			action = DOSErrorBox(statusline, code, HELP_ERROPENSRC);
		} else
		{
			action = ACT_OK;
		}
	} while (action == ACT_RETRY);
	if (action != ACT_OK || action == ACT_FORCE)
	{
		distance = COPY_SRC_OPEN_ERR;
		goto abend;
	}

	_dos_getfileattr(src, &attr);

	do {
		code = _dos_creat(dest, 0, &hdest);
		if (code)
			action = DOSErrorBox(statusline, code, HELP_ERRCREATEDEST);
		else
		{
			tree = FindTree(dest, &dummy) ;
			/* ZZZZZ  remove following assert before shipping */
			assert(tree != NULL) ;

			tree->Diskfilecount++ ;

			action = ACT_OK;
		}
	} while (action == ACT_RETRY);
	if (action != ACT_OK && action != ACT_FORCE)
	{
		distance = COPY_DST_CREAT_ERR;
		goto abend;
	}
	
	/* Actual copying.
	*/
	do {
		// Read a chunk.
		do {
			code = _dos_read(hsrc, buf, bufsize, &rcnt);
			if (code)
			{
				/* pop up dbox explaining problem */
				action = DOSErrorBox(statusline, code, HELP_ERRREADSRC);
			} else
			{
				action = ACT_OK;
			}
		} while (action == ACT_RETRY);
		if (action != ACT_OK || action == ACT_FORCE)
		{
			distance = COPY_RD_WR_ERR;
			goto abend;
		}

		// Write a chunk.
		do {
			code = _dos_write(hdest, buf, rcnt, &wcnt);
			if (code)
			{
				/* pop up dbox explaining problem */
				action = DOSErrorBox(statusline, code, HELP_ERRWRITEDEST);
			} else
			{
				action = ACT_OK;
			}
		} while (action == ACT_RETRY);
		if (action != ACT_OK || action == ACT_FORCE)
		{
			distance = COPY_RD_WR_ERR;
			goto abend;
		}

		// Make sure we didn't run out of disk space.
		if (rcnt != wcnt)
		{
			action = GetResponse(statusline, szDiskFull, BT_SKIPQUIT, HELP_DISKFULL);
			distance = COPY_RD_WR_ERR;
			goto abend;
		}
	} while (wcnt);
	
	/* Clean up.
	*/
	_dos_getftime(hsrc, &time, &date);      // mark new file with same
	_dos_setftime(hdest,time, date);        // datestamp as old file.

	/* GOTO here if there is an error setting stuff up.  Yes, goto's are
	** not appreciated by optimisers and maintainers, but we save on
	** duplicated cleanup code by having it here.
	*/
abend:
	switch (distance)
	{
		case COPY_SUCCESS:
		case COPY_RD_WR_ERR:
			_dos_close(hdest);
		case COPY_DST_CREAT_ERR:
			_dos_close(hsrc);
		case COPY_SRC_OPEN_ERR:
			FreeWorkFar(buf);
		default:
			break ;
	}

	if (distance == COPY_SUCCESS)                                   // no errors at all
	{
		/* When we copy a file, the new one created should not be marked
		 * as a ReadOnly file. If it was part of a MOVE operation, it is
		 * better to leave it alone.
		 */
		if (!m_fPerformingMoveFile())
			attr = attr & (~_A_RDONLY) ;

		// copy file's attributes except maybe read only attr
		_dos_setfileattr(dest, attr);
	}
	else if (distance == COPY_RD_WR_ERR)
	{
		/* disk became full when we tried to copy this file. Delete this file */
		unlink(dest);
	}

	return action;
}/* proc CopyIt */

/****   MoveFile - move one file with verification
**              This fn moves the file specified by 'tree' and 'node' into 'path'.
**      ENTRY
**              tree  - tree containing file to move
**              node  - node of file to move
**              path  - destination file
**              total - number of selected files
**              count - index in selected files
**              verify- verify on overwrite
**      EXIT
**              Action code, as from GetResponse().
*/      
int MoveFile(PTREE tree, PENTRY node, char *path, int count, 
																	int total, BOOL verify)
{
	int action;                             // user action
	char currpath[MAX_PATH+1] ;
	char statusline[STATUS_LEN];
	int ret ;
	BOOL jobdone ;
	struct find_t findinfo ;
	int dummylen ;

	jobdone = FALSE ;

	Tree2Path(tree, node, currpath, &dummylen) ;
	FormCountStatusLine(statusline, szOpMoveFile, currpath, count, total, 
													STATUS_COUNT_START) ;
	if (tree->root[0] == path[0])
	{
		/* ZZZZZZ get true name and then compare the two!!!! */
		if ( (ret = strcmp(path, currpath)) )
		{
			/* source and destination are not the same */
			/* Check if destination path exists, if not just rename 
				else check if the user wants to replace that file */
			if (_dos_findfirst(path, _A_HIDDEN | _A_SYSTEM, &findinfo))
			{
				PutUpStatusMessage(statusline, count) ;

				action = RenameFile(tree, node, path, count, total, FALSE);
				jobdone = TRUE ;

				TakeDownStatusMessage(count, total) ;
			}
			else
			{
				if (verify || (findinfo.attrib & _A_RDONLY) )
					action = ReplaceConfirmationDialog(currpath, path, node, findinfo) ;
				else
					action = ACT_OK ;
			}
		}
		else
			action = GetResponse(statusline, szNoSelfCopy, BT_SKIPQUIT, HELP_NOSELFCOPY) ;

		if (!jobdone)
		{
			switch(action)
			{
				case ACT_FORCE:
					action = ACT_OK ;
				case ACT_OK:
					PutUpStatusMessage(statusline, count) ;

					if ( (action = KillFile(statusline, path)) == ACT_OK )
						action = RenameFile(tree, node, path, count, total, 
															FALSE);

					TakeDownStatusMessage(count, total) ;
				default:
					break ;
			} /* switch */
		}
	} else
	{
		PutUpStatusMessage(statusline, count) ;

		// Copy file to new path.
		action = CopyFile(tree, node, path, count, total, verify);

		/* Do I delete source file even if it is read only? That is
		   what I am doing now as verify is FALSE to DeleteFile!! */
		if (action == ACT_OK)
			action = DeleteFile(tree, node, NULL, count, total, FALSE);
		
		/*      Keep in mind that MoveFile is expected to be able to restore the
		** system to its original state if it runs out of memory.  This is
		** accomplished now because it can only run out of memory in copy.
		**      Things will be much harder if Delete can also crap out.
		*/

		TakeDownStatusMessage(count, total) ;
	}
	return action;
} /* proc MoveFile */

/****   FDlgFileOpts - dialog proc for file options dialogue box */
/* Prompts the user for new settings & stores them                              */
BOOL FAR PASCAL FDlgFileOpts(WORD dlm,TMC tmc,WORD wNew,WORD wOld,WORD wParam)
{
	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	switch (dlm) 
	{
		case dlmInit:
			SetTmcVal(tmcVerifyDelete, glob.VerifyDelete);
			SetTmcVal(tmcVerifyOverwrite, glob.VerifyOverwrite);
			SetTmcVal(tmcMouseConfirm, glob.MouseConfirm);

			SetUpDialog(tmcOK,szFileOptionsCaption);

			SetUpButtonForGraphics(tmcOK);
			SetUpButtonForGraphics(tmcCancel);
			SetUpButtonForGraphics(tmcfileoptshelp);
			SetUpCheckBox(tmcVerifyDelete) ;
			SetUpCheckBox(tmcVerifyOverwrite) ;
			SetUpCheckBox(tmcMouseConfirm) ;

			break ;

	case dlmTerm:
		if (tmc == tmcOK)
		{
			glob.VerifyOverwrite = GetTmcVal(tmcVerifyOverwrite);
			glob.VerifyDelete = GetTmcVal(tmcVerifyDelete);
			glob.MouseConfirm = GetTmcVal(tmcMouseConfirm);
		}
		/* else cancel or help which we ignore */
		break ;

	case dlmSetFocus:
		gCurrentTMC = tmc;
		break ;

	case dlmClick:
		 if(tmc == tmcfileoptshelp)
			Help(hemDialog, ((PDLG)&dlgFileOpts)->hid, NULL, 0);

		 SetFocusTmc(gCurrentTMC) ;
		 break ;

	} /* switch */

	return TRUE;
} /* proc FDlgFileOpts */

/****   DoFileOptions - run file options dbox
**
**      ENTRY
**              none
**      EXIT
**              none
*/
VOID FAR DoFileOptions(void)
{
	HCABFileOpts hcab;                              // dialogue box storage

	// Init dlg box.
	hcab = HcabAlloc(cabiCABFileOpts);
	if (!hcab)
	{
		OutOfMemory() ;
		return;
	}
	InitCab(hcab, cabiCABFileOpts) ;

	SzToCab(hcab, szEnterButton, Iag(CABFileOpts, pszfileoptsEB));
	SzToCab(hcab, szCancelButton, Iag(CABFileOpts, pszfileoptsCB));
	SzToCab(hcab, szHelpButton, Iag(CABFileOpts, pszfileoptsHB));

	// Run dlg box.
	MyTmcDoDlg(&dlgFileOpts, hcab);

	FreeCab(hcab);
} /* proc DoFileOptions */

/****   DoSelectAll - select all files in the focus tree
**
**      ENTRY
**              none
**      EXIT
**              none
*/
VOID FAR DoSelectAll(void)
{
	PENTRY node;                            // current file node
	PTREE  ftree ;

	ftree = listinfo[glob.FocusBox].tree ;

	/* If tree has not yet been built, don't do anything! */
	if ( (!ftree) || (ftree->ContinueTree) )
		return ;

	if (    (glob.TreeMode == TR_SYSTEM) ||
			((glob.TreeMode == TR_SEARCH) && gfSearchDisk)
		)
	{
#ifndef NOCONSISTENCY
		if (glob.FocusBox != 0)
		{
			printf("*** System Tree mode FocusBox = %d\n", glob.FocusBox) ;
			exit(0) ;
		}
#endif
		node = ftree->FirstFile;
		while (node)
		{
			if (node->MATCHESPATTERN)
				node->SELECTED = TRUE;
			node = node->x.f.snext;
		}
	} else
	{
		node = listinfo[glob.FocusBox].files;
		if (node)
			node = node->x.d.child;
		else
			node = ftree->head;
		while (node)
		{
			if ((node->MATCHESPATTERN) && (!(node->attribs & _A_SUBDIR)) )
				node->SELECTED = TRUE;
			if (node->nosib)
				break;
			else
				node = node->sibling;
		}
	} /* non system tree mode */

	/* The above selections may or may not have have added new selections
	   So re-calculate tree selections!! */
	ftree->NumSel = GetTreeSelectedInfo(ftree, &(ftree->SizeSel)) ;

	/* Now Redraw the file list box */
	InsertListItem(&FileList[glob.FocusBox], 0) ;
} /* proc DoSelectAll */

/* Deselects all file nodes in a tree */
VOID FAR DeselectTree(PTREE tree)
{
	PENTRY node;

	/* If tree has not yet been built, don't do anything! */
	if ( (!tree) || (tree->ContinueTree) )
		return ;

	for (node = tree->FirstFile; node ; node = node->x.f.snext)
		node->SELECTED = FALSE;

	tree->NumSel = 0 ; /* All files in tree have been deselected */
	tree->SizeSel = 0 ;
} /* DeselectTree */

extern BOOL gNeedToUpdateFileList[2];

/****   DoDeselectAll - unselect all files in any displayed trees
**
**      ENTRY
**              none
**      EXIT
**              none
*/
VOID FAR DoDeselectAll()
{
	int i ;
	PTREE tree ;

	tree = listinfo[glob.FocusBox].tree ;

	if (FPerformOpOnDirFilesAlone())
	{
		DeselectDir(listinfo[glob.FocusBox].files, tree) ;
	}
	else
	{
		DeselectTree(tree) ;
	}

	/* fake a space key to re-select the current focus item in focus listbox */
   ListKey(&FileList[glob.FocusBox], ' ', 0);

	for (i = 0 ; i <= glob.MaxTree ; i++)
	{
		InsertListItem(&FileList[i], 0) ;
	}
} /* DoDeselectAll */

void DeselectDir(PENTRY dir, PTREE tree)
{
	PENTRY node ;

	if (tree->NumSel > 0)
	{
		node = (dir) ? dir->x.d.child : tree->head;
	
		while(node)
		{
			if ((node->SELECTED) && !(node->attribs & _A_SUBDIR))
			{
				node->SELECTED = FALSE;
				tree->NumSel-- ;
				tree->SizeSel -= node->x.f.size ;
			}
			node = (node->nosib) ? NULL : node->sibling ;
		} /* while */
	}
} /* DeselectDir */

/* Deselects all file nodes in a tree that are displayed -- 
 * ones matching MatchPat 
 */
VOID FAR DeselectTreeMatches(PTREE tree)
{
	PENTRY node;

	if ( (!tree) || (tree->NumSel == 0) )
		return ;

	for (node = tree->FirstFile; node ; node = node->x.f.snext)
		if ( (node->SELECTED) && (node->MATCHESPATTERN) )
		{
			node->SELECTED = FALSE;
			tree->NumSel-- ;
			tree->SizeSel -= node->x.f.size ;
		}
} /* DeselectTreeMatches */


VOID FAR DoCUA2DeselectAll(TMC tmc)
{
	PTREE tree ;

	tree = listinfo[tmc].tree ;

	if ( (glob.TreeMode == TR_SYSTEM) || (glob.TreeMode == TR_SEARCH) )
	{
		DeselectTreeMatches(tree) ;
	}
	else
	{
		// check to see & redisplay files in other listbox if we are looking to 
		// deselect files in the directory being displayed there.
		if ( (glob.TreeMode == TR_DOUBLE) && 
		     (listinfo[1-tmc].tree == tree) &&
		     (listinfo[1-tmc].files == listinfo[tmc].files) )
				InsertListItem(&FileList[1-tmc], 0) ;

		DeselectDir(listinfo[tmc].files, tree) ;
	}
	InsertListItem(&FileList[tmc], 0) ;
} /* DoCUA2DeselectAll */

/* Global variables thru which info is passed on to FDlgRename */
static int Rcount;
static int Rtotal;
static char *Roldname;

/****   RenameFile - rename a file or directory
**      This fn renames the file specified by 'tree' and 'node'.  If ask = TRUE,
**      the user is prompted for the new file name (only).  Else, the new path
**      is assumed to be in prognew.  Note that the dbox will only allow name
**      changes, but the other way allows full path renaming.
**
**      ENTRY
**              tree  - tree containing file to move
**              node  - node of file to move
**              prognew- destination path if ask == FALSE
**              total - number of selected files
**              count - index in selected files
**              ask   - TRUE = prompt user for new name, FALSE = use prognew.
**      EXIT
**              Action code, as from GetResponse().
*/      
int RenameFile(PTREE tree, PENTRY node, char *prognew, int count,
																		int total, BOOL ask)
{
		int i, j;
		int ret;                                                // return value
		int action;                                             // reaction to critical error
		TMC tmc;                                                // return from dbox
		HCABRename hcab;                                // the dbox
		char usernew[MAX_PATH+1];               // holds new file path
		char curpath[MAX_PATH+1];               // old file path
		char tempname[128] ;
		char statusline[STATUS_LEN];
		char troot[4];          /* To hold a temporary root name like "C:\" etc */
		char *p ;
		BYTE dummy ;
		PTREE temptree ;
		PENTRY tempparent ;
		int dummylen ;
		char *barebonename ;
		BOOL fParentIsRootDir ;
		
		action = ACT_OK ; /* default value */

		Tree2Path(tree, node, curpath, &dummylen);

		/* If total == 0, it is a dir rename operation -- no files selected. */
		p = (total > 0) ? szOpRenameFile : szOpRenameDir ; 
		FormCountStatusLine(statusline, p, curpath,
										count, total, STATUS_COUNT_START) ;

		tmc = tmcOK ; /* Default value */

		i = FindLastComponent(curpath);
		if (curpath[i] == PATHCHAR)
		{
			fParentIsRootDir = FALSE ;
			i++;
		}
		else
			fParentIsRootDir = TRUE ;

		/* ZZZZ In case we are renaming a directory, we need to make sure
		   that the cwd is not in the path of the renamed directory --
		   simple approach is to do a chdir to the root directory -- Note
		   that we could at this point be in any directory as Jeff does
		   chdir's in quite a few places!! */
		if (node->attribs & _A_SUBDIR)
		{
			/* ZZZZZ For efficiency & not screw up cwd, getcwd should
			   be used and if this is same as curpath change to root
			   directory for rename. Check if cwd is COMPSPEC dir too
			   as otherwise his machine will be non-bootable --  give
			   warning in this case. Current DOSSHELL has this bug!! */
			/* Copy from far memory to near memory */
			strfcpy(troot, tree->root) ;

			/* ZZZZZ No error checking done on this call!! */
			if (fParentIsRootDir)
				UnixChdir(troot) ;
			else
			{
				curpath[i-1] = '\0' ;
				UnixChdir(curpath) ;
				curpath[i-1] = PATHCHAR ;
			}
		}

		do
		{
			if (ask)
			{
				// Fill in fields.
				Roldname = curpath+i;
				Rtotal = total;
				Rcount = count;

				// Allocate dbox.
				hcab = HcabAlloc(cabiCABRename);
				if (!hcab)
				{
					OutOfMemory();
					return ACT_CANCEL;
				}
				InitCab(hcab, cabiCABRename) ;
	
				/* Use default of Null name */
				SzToCab(hcab, NullString, Iag(CABRename, NewName));
				SzToCab(hcab, szEnterButton, Iag(CABRename, pszrenameEB));
				SzToCab(hcab, szCancelButton, Iag(CABRename, pszrenameCB));
				SzToCab(hcab, szHelpButton, Iag(CABRename, pszrenameHB));


				// Run dialogue box.
				tmc = MyTmcDoDlg(&dlgRename, hcab);
				if (tmc == tmcOK)
				{
					strncpy(usernew, curpath, i);   // copy all but old filename
					SzFromCab(hcab,tempname,MAX_PATH,Iag(CABRename,NewName));
					ScrunchFileName(usernew+i, tempname, FALSE) ;
					// ZZZ remove all such strupr's later, if dialog box does it
#ifdef DBCS
					DBCSstrupr(usernew+i);
#else
					strupr(usernew+i);
#endif
					prognew = usernew;
				}
				else
				{
#ifndef NOCONSISTENCY
if (tmc != tmcCancel)
{
	printf("*** Not Cancel button in Rename\n") ;
	exit(0) ;
}
#endif
					/* Warning! Assuming tmcCancel and not Help */
					action = ACT_CANCEL ;
					FreeCab(hcab);
					break ; /* leave while loop */
				}
				FreeCab(hcab);

			} /* if (ask) */
			/* At this point 'prognew' has the new path, either given by the
			   user or already passed in to the routine */

			if (node->attribs & _A_SUBDIR)
			{
				if (translate_name(curpath, tempname))
					strcpy(tempname, curpath) ;

				/* Do path length check!! */
				if (path_len_check(tempname, node, 
											strlen(prognew)-strlen(curpath)))
				{
					j = FindLastComponent(prognew) ;
					if (prognew[j] == PATHCHAR)
						j++ ;

					barebonename = prognew+j ;

					ret = rename(curpath, barebonename) ;
				}
				else
				{
					ret = -1 ; // mark as error!
					_doserrno = NESTING_DEEP_ERR ;
				}
			}
			else
				ret = rename(curpath, prognew);

			if (ret)
				action = DOSErrorBox(statusline, _doserrno, HELP_ERRRENAME);
			else
				action = ACT_OK;
		} while (action == ACT_RETRY);
		if (action == ACT_OK || action == ACT_FORCE)
		{
			fileinfo rec;                                   // copied file's info
			treehdr far *ftree;                     // tree holding old file record
			PENTRY parent, file;                    // parent, self of old rcd
			
			if (node->attribs & _A_SUBDIR)
			{
#ifdef RENAME_DIR_IN_PLACE
				FillRecName(prognew, node);     // rename existing record
#else
				parent = FindParent(node) ;

				/* tree->DirectoryCount does not change as we are only renaming
				 * this directory.
				 */
				HandleDeletednext(tree, node) ;
				FillRecName(prognew, node);     // rename existing record

				/* If directory has got sub-directories, marking the LASTDIR
				 * bits is not trivial. So, we just go thru the process of marking
				 * the LASTDIR bits of LASTDIR bits afresh. This is fast and
				 * we don't want to write special code for this.
				 */
				if (node->HASSUBDIRS)
				{
					InsertDir(tree, parent, node) ;
					Marklastdir(tree) ;
				}
				else
				{
					InsertDirAndMarkLastDirBits(tree, parent, node) ;
				}
#endif
			} else
			{
				/* It's a normal file. Build record of copied file.  */
				FillRecName(prognew, (PENTRY) &rec); // put name/ext into record
				rec.dtlx.dt.time = node->dtlx.dt.time;
				rec.dtlx.dt.date = node->dtlx.dt.date;
				rec.attribs = node->attribs;
				rec.nosib = rec.SELECTED = 0;
				rec.x.f.size = node->x.f.size;

				/* Delete old record.
				*/
				ftree = FindTree(curpath, &dummy);
				if (2 == FindNode(ftree, curpath, &parent, &file, FALSE))
					DelFileEntry(ftree, parent, file);

				/* AddLateFile will locate the proper tree and add our record to it.
				*/
				/* we need to pass in non-null values (temptree and tempparent)
				 * because fn AddLateFile uses these storages.
				 */
				if (!AddLateFile(prognew, &rec, temptree, tempparent, 0))
					// action = ACT_NOMEM;
					action = ACT_CANCEL;
			}
		}
		if (tmc == tmcCancel)
		{
			action = ACT_CANCEL ;
		}

		if ( (action == ACT_FORCE) || (action == ACT_SKIP) )
			return ACT_OK;
		else
			return action;
} /* proc RenameFile */

char *GetPrintErrMsg(int print_err_code)
{
	char *msg ;

	switch(print_err_code)
	{
		case 2:
		case 3:
			msg = szPathNotFound ;
			break ;

		case 8:
			msg = szPrintQueueFull ;
			break ;

		case 9:
			msg = szSpoolerBusy ;
			break ;

		case 5:
		default:
			msg = szAccessDenied ;
			break ;

	} /* switch */

	return msg ;

} /*  GetPrintErrMsg */

/* Prints the file specified by 'node' */
int PrintFile(PTREE tree, PENTRY node, char *placeholder, int count,
																	int total, BOOL verify)
{
	int action;
	int ret ;
	char path[MAX_PATH+1];                                  // path of file
	char TrueName[128] ;
	char statusline[STATUS_LEN];
	extern BOOL PDisable ; /* Says if print.com is not resident */
	int dummylen ;

	UnReferenced(placeholder) ;
	UnReferenced(verify) ;

	action = ACT_CANCEL ;

	if (PDisable)
	{
		ShellMessageBox(szPrintFileCaption, szPrintLine1) ;
		return action ;
	}

	Tree2Path(tree, node, path, &dummylen);

	/* print.com's int 2f interface screws up if we passed in a path
	 * with a drive letter for a substituted drive.
	 */
	if (translate_name(path, TrueName))
		strcpy(TrueName, path) ;

	FormCountStatusLine(statusline, szOpPrintFile, path, count, total, 
													STATUS_COUNT_START) ;
	PutUpStatusMessage(statusline, count) ;

	do
	{
		/* File succesfully printed => we get 0 else 1 */
		if (!(ret = a_print_file((char far *)TrueName)))
			action = ACT_OK ;
		else
		{
			action = GetResponse(statusline, GetPrintErrMsg(ret),
											BT_FILERETRYSKIPQUIT, HELP_PRINTERR) ;
		}
	} while (action == ACT_RETRY) ;

	if (action == ACT_SKIP)
	   action = ACT_OK ;

	TakeDownStatusMessage(count, total) ;

	return action ;

} /* proc PrintFile */

/* This is the function that gets control when the user selects the "Cross
 * Directory Selection" menu item in the file manager.
 */
void SelectAcrossDirs(void)
{
	/* Toggle cross dir selection option. */
	glob.CrossDirSel = !glob.CrossDirSel ;

	/* Following call is not needed as we do it before a menu pops down! */
	// easycheck(SelectAcrossDirs, glob.CrossDirSel) ;

} /* SelectAcrossDirs */

/* If Rtotal > 0, this routine writes out the line xx of xx */
/* RTotal = 0 is sent in when renaming directories */
BOOL FAR PASCAL FDlgRename(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
	char temp[MAX_PATH+1];                          // file name for verification
	PWND lwind;
	int nWid;
	char *msg ;

	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	switch (dlm)
	{
		case dlmInit:
			SetUpDialog(tmcOK,gpszFileOpCaption);

			SetUpButtonForGraphics(tmcOK);
			SetUpButtonForGraphics(tmcCancel);
			SetUpButtonForGraphics(tmcrenamehelp);

			SetUpEditBox(tmcNewName, TRUE,NAMELEN+EXTLEN+1, TRUE);
			
			FormCountStatusLine(temp, szCurrName, Roldname, Rcount, Rtotal,
														STATUS_COUNT_START) ;
			Shell_SetTmcText(tmcrenamestatus, temp) ;
			Shell_SetTmcText(tmcstrenamenew, szNewName) ;
			break;

		case dlmTerm:
			if (tmc == tmcOK)
			{
				GetTmcText(tmcNewName, temp, MAX_PATH);

				/* Did the user not type any name? If so don't dismiss dialog */
				if (!(*temp))
				{
					Shell_Beep() ;
					return FALSE ;
				}

				/* check to see if the user is trying to use nested names */
#ifdef DBCS
				if (DBCSstrchr(temp, PATHCHAR))
#else
				if (strchr(temp, PATHCHAR))
#endif
				{
					lwind = PwndOfTmc(tmcrenameerr1) ;
				nWid = lwind->arcWindow.axRight - lwind->arcWindow.axLeft + 1;

					msg = MySetMessageText(tmcrenameerr1, szNoPathCharsInRename,
																							nWid) ;
					/* Warning! Assuming that both these edit items have same width */
					MySetMessageText(tmcrenameerr2, msg, nWid) ;

					return FALSE ; /* Don't dismiss dialog -- invalid name */
				}
			}
			break ;

#if 0
		case dlmChange:
			/* The user has typed in a new name */
			/* ZZZ Now check for lower case, etc!! */
			// GetTmcText(tmcNewName, temp, MAX_PATH);
			return(TRUE) ;
#endif

	case dlmSetFocus:
		gCurrentTMC = tmc;
		break ;

	case dlmClick:
		 if(tmc == tmcrenamehelp)
			Help(hemDialog, ((PDLG)&dlgRename)->hid, NULL, 0);

		 SetFocusTmc(gCurrentTMC) ;
		 break ;

		case dlmIdle:
		default:
			break ;
	}
	return TRUE;
} /* proc FDlgRename */
