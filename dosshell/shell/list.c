;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <menus.h>

/* make this #if to fail so that you can build in DEBUG mode without
 * running out of near heap space when compiling this file!!!
 */
#if 1
#include <prot.h>
#else
extern VOID get_temp_dir(char *buffer,BOOL UseTemp) ;
extern  void far *pascal far LpbAllocWorkFar(unsigned short cb);
extern  void pascal far FreeWorkFar(void far *lpb);
extern void HandleSearchQuit(void) ;
extern  void JunkTree(struct th_t far *tree);
extern  struct fi_t far *GetNthVisibleDir(struct th_t far *tree,int count);
extern  struct fi_t far *GetNthDir(struct th_t far *tree,int count);
extern	struct th_t far *FindTree(char *path, BYTE *driveind);
extern  struct fi_t far *FindOpenSlot(struct th_t far *tree);
extern  struct fi_t far *GetLastDirFile(struct fi_t far *dir);
extern  struct fi_t far *GetFirstDirFile(struct fi_t far *dir);
extern  struct fi_t far *GetLastRootFile(struct th_t far *tree);
extern  struct fi_t far *GetFirstRootFile(struct th_t far *tree);
extern  void ResetTreeOptimizations(void );
extern  void ClobberFiles(struct th_t far *tree);
extern  void ClobberDir(struct th_t far *tree,struct fi_t far *dir);
extern  void ClobberSel(struct th_t far *tree,struct fi_t far *parent,struct fi_t far *curr);
extern  void PackDirs(struct th_t far *tree);
extern  int CompactOne(int sel);
extern  void CompactAll(void );
extern  void DelFileEntry(struct th_t far *tree,struct fi_t far *parent,struct fi_t far *node);
extern  struct fi_t far *FindParent(struct fi_t far *node);
extern  void strfcpy(char far *dst,char far *src);
extern  int CompactifyTree(struct th_t far *tree,int sel);
extern  int LoadCompactDir(struct th_t far *tree,struct fi_t far *new);
#endif
#include <text.h>

#define AllocFarNormalized(cb)  LpbAllocWorkFar(cb)
#define FreeFarNormalized(fptr) FreeWorkFar(fptr)

extern char gStartUpDir[] ;
extern int gStartUpDirEnd ;
extern BYTE	gHighlightedDrive[];
extern BYTE	gSelectedDrive[];

extern void SetUpTreeForSort(PTREE tree) ;
extern BOOL FPerformOpOnDirFilesAlone(void) ;

int gswapfhandle;
int gnumpages;

/* ZZZZZ should I try user's RAM drive, temp path, etc before the start up dir? */

/***************************************************************************/
/* Opens the swap file in write-only mode. Global variable gswapfhandle    */
/* is assigned the handle to this file.									   */
/* On exit, returns TRUE on success, else FALSE.						   */
/***************************************************************************/
BOOL initswapout(void)
{
	 int ret ;
	 char swpname[91];
	 int ind ;

	 get_temp_dir(swpname,TRUE);

	 /* Don't swap to a floppy!! */
#ifdef JAPAN
	if (getdrivetype(*swpname - 'A' + 1) == FLOPPY_TYPE)
		return FALSE;
#else
	 if ( (*swpname == 'A') || (*swpname == 'B') )
		return FALSE ;
#endif

	 /* form full path name of swap file. */
	 ind = strlen(swpname) ;
#ifdef DBCS
	 if (swpname[ind-1] != '\\' || CheckDBCSTailByte(swpname,&swpname[ind-1]))
#else
	 if (swpname[ind-1] != '\\')
#endif
	 {
		swpname[ind++] = '\\' ;
	 }
	 strcpy(swpname+ind,szShellSwap);

	 /* ZZZZZ what if user has file of same name!! _ Do I warn him? */
	 _dos_setfileattr(swpname, 0); /* remove any read-only attrib on file */

	 ret = _dos_creat(swpname,(O_WRONLY | O_BINARY),&gswapfhandle) ;
	 return(!ret);
} /* initswapout */

/***************************************************************************/
/* Opens the swap file in read-only mode. Global variable gswapfhandle	  */
/* is assigned the handle to this file.									   */
/* On exit, returns TRUE on success, else FALSE.						   */
/***************************************************************************/
BOOL initswapin(void)
{
	 int ret ;
	 int ind ;
	 char swpname[91];

	 get_temp_dir(swpname,TRUE);

	 /* form full path name of swap file. */
	 ind = strlen(swpname) ;
#ifdef DBCS
	 if (swpname[ind-1] != '\\' || CheckDBCSTailByte(swpname,&swpname[ind-1]))
#else
	 if (swpname[ind-1] != '\\')
#endif
	 {
		swpname[ind++] = '\\' ;
	 }
	 strcpy(swpname+ind,szShellSwap);

	 ret = _dos_open(swpname,(O_RDONLY | O_BINARY),&gswapfhandle);
	 return(!ret);

} /* initswapin */

/***************************************************************************/
/* Closes the swap in file. In addition it deletes the swap file from the 	*/
/* disk iff the boolean input variable 'fDelSwapFile' is set (non-zero).	*/
/***************************************************************************/
VOID endswap(BOOL fDelSwapFile)
{
	 int ind ;
	 char swpname[91];

	_dos_close(gswapfhandle) ;

	 if (fDelSwapFile)
	 {

		get_temp_dir(swpname,TRUE);

		/* form full path name of swap file. */
		ind = strlen(swpname) ;
#ifdef DBCS
		if (swpname[ind-1] != '\\' || CheckDBCSTailByte(swpname,&swpname[ind-1]))
#else
		if (swpname[ind-1] != '\\')
#endif
		{
			swpname[ind++] = '\\' ;
		}
		strcpy(swpname+ind,szShellSwap);
	
		_dos_setfileattr(swpname, 0); /* Remove any read-only attrib */
	
		unlink(swpname) ; /* Delete the swap file from disk */
	}
} /* endswap */

/* This is the segmnet association array! */
typedef struct FIXUP
{
    char far *old;
    char far *new;
}FIXUP;

struct FIXUP far *fixups;

/***************************************************************************/
/* Accepts a far pointer as input. Output is a far pointer with the segment*/
/* part of this pointer modified based on the segment association array. If*/
/* an association is not found, output == input.						   */
/***************************************************************************/

char far *fixup(char far *old)
{
     int i;
     char far *retval;

	  /* Actually this special case code might not be needed as the loop
		* below will fail and we will retun NULL anyway.
		*/
	  if (old == NULL)
			return NULL ;

     retval = old;
     for(i=0;i<gnumpages;i++)
     {
		/* Note that our pointers are normalized pointers, as the allocation
			routine returns such a pointer. 
		*/
#if 0
		if(HIWORD(fixups[i].old) == HIWORD(old))
		/* Note: FP_SEG macro doesn't cast it to a far pointer!! So, If I used
		 it there would be a long address to short address conversion
		if (FP_SEG((fixups[i].old)) == FP_SEG(old))
		*/
#endif
		/* See if the segment values of these two far pointers are the same */
		if ( *( ((WORD FAR *)&(fixups[i].old))+1) == *( ((WORD FAR *)&old)+1) )
		{
			/* Change retval's segment value to that in fixups[i].new */
#if 0
	    	retval = (char far *)(((unsigned long) retval & 0xFFFF) | 
							( ((unsigned long)HIWORD(fixups[i].new)) <<16) ) ;
#endif
			/* Note that far pointers are stored as OFFSET followed by segment
			*	in the intel architectures -- That's the reason for the "+1".
			*/
			*( ((WORD FAR *)&retval) + 1) = *( ((WORD FAR *)&(fixups[i].new)) + 1) ;
	    	break ;
		}
     } /* for */

     return(retval);
} /* function fixup */

/****************************************************************************/
/* Appends to the swap file (accessed using the global handle gswapfhandle) */
/* 'size' bytes from buffer 'bytes'. Actually, this routine assumes that the*/
/* caller has left the file pointer at the end of the file.					*/
/* If successful returns TRUE else FALSE. 									*/
/****************************************************************************/
BOOL appendswap(char far *bytes,int size)
{
   int asize;

   if ( (!_dos_write(gswapfhandle,bytes,size,&asize)) && (asize == size) )
		return TRUE ;
   else
		return FALSE ;
} /* appendswap */

/****************************************************************************/
/* Reads 'size' bytes from swap file (accessed using the global handle		*/
/* gswapfhandle) into buffer 'bytes'. These bytes are picked up from wherever*/
/* the file pointer happens to be pointing in the file. 					*/
/* If successful returns TRUE else FALSE.									*/
/****************************************************************************/
BOOL getappended(char far *bytes,int size)
{
   int asize;

   if ( (!_dos_read(gswapfhandle,bytes,size,&asize)) && (asize == size) )
		return TRUE ;
	else
		return FALSE ;
} /* getappended */

BOOL SwapInTree(PTREE tree)
{
    int i,j;

	if (gnumpages > 0)
	{
    	fixups = LpbAllocWorkFar(sizeof(FIXUP)*gnumpages);
    	if(!fixups)
			return(FALSE);
	}

	/* Now, try to allocate enuf mem for the file entries to be swapped in */
    for(i=0;i<gnumpages;i++)
    {
       fixups[i].new = AllocFarNormalized(sizeof(filepage));
       if(!fixups[i].new)
       {
	 		/* Allocation failed, free it all */
			for ( ; i ; )
				FreeFarNormalized(fixups[--i].new) ;

			FreeWorkFar(fixups) ;
			return(FALSE);
       }
    }

	/* Read Tree and Directory-Entries that were previously swapped to disk. */
    getappended((char far*) tree,sizeof(treehdr));

	/* No file pages need be swapped in -- Will happen only when we are swapping
	 * in a 'junk'ed tree. Only the 'tree' data stucture need be swapped in
	 * and that has been done in the line above.
	 */
	if (gnumpages == 0)
		return(TRUE) ;

    for(i=0;i<gnumpages;i++)
    {
       getappended(fixups[i].new,sizeof(filepage));
    }

	/* perform all the pointer fixups */
    fixups[0].old = (char far *) tree->pagehead;
    for(i=1;i<gnumpages;i++)
    {
		fixups[i].old = (char far *) ((PPAGE)fixups[i-1].new)->next;
    }

    for(i=0;i<gnumpages;i++)
    {
		for(j=0;j<PAGESIZE;j++)
		{
	  		((PPAGE) fixups[i].new)->files[j].sibling=
				(PENTRY)fixup((char far *)((PPAGE)fixups[i].new)->files[j].sibling);
	  		if(!(((PPAGE)fixups[i].new)->files[j].attribs & _A_SUBDIR))
	  		{
	     		((PPAGE)fixups[i].new)->files[j].x.f.snext=
		   		(PENTRY)fixup((char far *)((PPAGE)fixups[i].new)->files[j].x.f.snext);
	  		}
	  		else
	  		{
	     		((PPAGE)fixups[i].new)->files[j].x.d.dnext=
		    		(PENTRY) fixup((char far *)((PPAGE)fixups[i].new)->files[j].x.d.dnext);
	     		((PPAGE)fixups[i].new)->files[j].x.d.child=
		    		(PENTRY) fixup((char far *)((PPAGE)fixups[i].new)->files[j].x.d.child);
	  		}
		} /* inner for */
	    ((PPAGE) fixups[i].new)->next=
		    (PPAGE) fixup((char far *)((PPAGE)fixups[i].new)->next);
    } /* outer for */
    tree->head = (PENTRY)(fixup((char far *)tree->head));

    tree->pagehead = (PPAGE)(fixup((char far *)tree->pagehead));
    tree->pagetail = (PPAGE)(fixup((char far *)tree->pagetail));

    tree->FirstDirectory = (PENTRY)fixup((char far *)tree->FirstDirectory);
    tree->LastDirectory = (PENTRY)fixup((char far *)tree->LastDirectory);
    tree->FirstFile = (PENTRY)fixup((char far *)tree->FirstFile);
    tree->LastFile = (PENTRY)fixup((char far *)tree->LastFile);

    tree->parent = (PENTRY)fixup((char far *)tree->parent);
    tree->lastlink = (PENTRY)fixup((char far *)tree->lastlink);

	 tree->SelDir =  (PENTRY)fixup((char far *)tree->SelDir);
	/* tree->next is not fixed here!! The caller of this routine does that! */

	/* Free the memory allocated for the pairs of pointers */
    FreeWorkFar(fixups) ;

	return (TRUE) ;
} /* SwapInTree */

BOOL SwapOutTree(PTREE tree)
{
   PPAGE page;
	BOOL ret ;

    ret = appendswap((char far *)tree,sizeof(treehdr)) ;

    page = tree->pagehead;
    while(ret && page)
    {
		ret = appendswap((char far *)page,sizeof(filepage)) ;
		page=page->next;
    }

	return ret ;
} /* SwapOutTree */

unsigned int GetNumFilePages(PTREE tree)
{
	unsigned int i ;
	PPAGE page ;

	for(i= 0 , page = tree->pagehead ; page ; page = page->next)
		i++ ;

	return i ;
} /* GetNumFilePages */

/****************************************************************************/
/* Swap out the tree(s) being displayed to the user, so that we can return	*/
/* to the file manager mode promptly. If any errors in opening file or 		*/
/* writing to disk, etc occur during this swap, no swap file will be left on*/
/* the disk.																*/
/****************************************************************************/
VOID DoSwapOut(void)
{
	/* 'ptr' is pointer to place in 'glob' where we can stick in some values. */
	unsigned char far *ptr ; 

	// ZZZ We don't swapout TR_SEARCH mode as we consider it a temporary mode!
	if (glob.TreeMode == TR_SEARCH)
		HandleSearchQuit() ;

	/* If tree is not fully read in!! we trash it!! The reason we
	 * trash it is because we can't be sure if the find_t structure
	 * has meaningful values now, because user could have done something
	 * to the disk. Of course if we were smart and make sure disk hasn't
	 * changed since the last time, we can swap out partial tree and then
	 * read it back in and continue where we left off!
	 */
	if (listinfo[0].tree->ContinueTree)
		JunkTree(listinfo[0].tree) ;

   if (!initswapout())
		return ;

	/* This structure "glob" will get swapped to disk, so store the global
	 * variable "gDescendingOrder" in this field.
	 */
	glob.TempDescendingOrder = gDescendingOrder ;
	glob.TempFileListFocusItem[0] = Get_List_Focus(&FileList[0]);
	glob.TempFileListFocusItem[1] = Get_List_Focus(&FileList[1]);
	glob.TempFileListScrolled[0] =  Get_List_Scrolled(&FileList[0]);
	glob.TempFileListScrolled[1] = Get_List_Scrolled(&FileList[1]);

	/* ptr now points to a location that is 8 bytes long (= 2 far pointers). */
	/* we use only 4 of these presently. The remaining 4 are still left free */
	ptr = (unsigned char far *) &(glob.SelTree[0]) ;
	*ptr = listinfo[0].tree->root[0] ;

	/* even if we are not in TR_DOUBLE mode, we save drive1's name too */
	*(ptr+1) = listinfo[1].tree->root[0] ;

	/* The following 2 locations store the number of filepages reqd for tree
	 * It should also be noted that 1 filepage can store upto PAGESIZE entries
	 * (= 256). Given that the size of a filepage is nearly 8K, 256 filepages
	 * is a lot. Therefore, the number of filepages can be stored in a byte
	 * Note that if tree is still being built, we use 0 for the number of file
	 * pages. This is because we call JunkTree before swapping out in this
	 * case.
	 */
	*(ptr+2) = (BYTE) ((listinfo[0].tree->ContinueTree) ? 0 :
								(unsigned char) GetNumFilePages(listinfo[0].tree)) ;
	*(ptr+3) = (BYTE) ((listinfo[1].tree->ContinueTree) ? 0 :
								(unsigned char) GetNumFilePages(listinfo[1].tree)) ;

   if (!appendswap((char far *)&glob,sizeof(global_t)))
	{
		/* Disk is probably full - we couldn't write to disk! 				*/
		/* This routine is basically called when we need to launch a program */
		/* and we want to do that as soon as possible without bothering about*/
		/* this error! We will re-read disk on return from launch!			 */
		endswap(TRUE) ; /* delete swap file created */
		return ;
	}

   if (!SwapOutTree(listinfo[0].tree))
	{
		/* Error in swapping out tree! */
		endswap(TRUE) ; /* delete swap file created */
		return ;
	}

	if ((glob.TreeMode == TR_DOUBLE) && (listinfo[0].tree != listinfo[1].tree))
	{
		/* If tree is not fully read in!! we trash it!! The reason we
		 * trash it is because we can't be sure if the find_t structure
		 * has meaningful values now, because user could have done something
		 * to the disk.
		 */
		if (listinfo[1].tree->ContinueTree)
			JunkTree(listinfo[1].tree) ;

   	if (!SwapOutTree(listinfo[1].tree))
		{
			/* Error in swapping out tree! */
			endswap(TRUE) ; /* delete swap file created */
			return ;
		}
	}

	/* Swapping out suucesfully completed -- close swap file */
	endswap(FALSE) ;
} /* DoSwapOut */

/* ZZZZ Handle correctly the case when we run out of memory doing swap in!! */
VOID DoSwapIn(void)
{
   PTREE tree ;
	global_t temp_glob ;
	unsigned char far *ptr ;
	unsigned char drive_letter1, drive_letter2 ;
	unsigned char num_pages1, num_pages2 ;
	PTREE next_tree ;
	BYTE driveind ;

   if (!initswapin())
		return ; 

	temp_glob = glob ; /* store glob's current values for use later */

	getappended((char far *) &glob, sizeof(global_t)) ;

	/* Transfer the sort order flag to "gDescendingOrder" as it is being
	 * looked at by the string compare routines, etc!
	 */
	gDescendingOrder = glob.TempDescendingOrder ;

	Set_List_Focus(&FileList[0],glob.TempFileListFocusItem[0]);
	Set_List_Focus(&FileList[1],glob.TempFileListFocusItem[1]);

	Set_List_Scrolled(&FileList[0],glob.TempFileListScrolled[0]);
	Set_List_Scrolled(&FileList[1],glob.TempFileListScrolled[1]);

	/* Location where the drive letters, the two filepage numbers are stored! */
	ptr = (unsigned char far *) &(glob.SelTree[0]) ;

	/* Retrieve the following 4 values as they will be lost when we change the
		value of glob.SelTree[0] later! */
	drive_letter1 = *ptr ;
	drive_letter2 = *(ptr+1) ;
	num_pages1 = *(ptr+2) ;
	num_pages2 = *(ptr+3) ;

	glob.drives = temp_glob.drives ;
	glob.DriveCount = temp_glob.DriveCount ;
	
	tree = FindTree(&drive_letter1, &driveind) ;
	if (tree)
	{
		listinfo[0].tree = listinfo[1].tree = tree ;
		glob.SelTree[0] = glob.SelTree[1] = tree ;
		gnumpages = (int) num_pages1 ;

		/* store this as SwapInTree will trash its value!! */
		next_tree = tree->next ; 

     	if (SwapInTree(tree))
		{
			tree->next = next_tree ;

			/* Set the directory with check mark correctly */
			listinfo[0].files = GetNthVisibleDir(tree, glob.lineselected[0]) ; 
			listinfo[1].files = GetNthVisibleDir(tree, glob.lineselected[1]) ;

			gHighlightedDrive[0] = driveind ;
			gSelectedDrive[0] = driveind;
			gHighlightedDrive[1] = driveind ;
			gSelectedDrive[1] = driveind ;

			/* ZZZZZ	What about TR_SEARCH? */
			if ( (glob.TreeMode==TR_DOUBLE) && (drive_letter1!=drive_letter2) )
			{
				tree = FindTree(&drive_letter2, &driveind) ;
				if (tree)
				{
					listinfo[1].tree = tree ;
					glob.SelTree[1] = tree ;
					gnumpages = (int) num_pages2 ;
					
					/* store this as SwapInTree will trash its value!! */
					next_tree = tree->next ; 
					if (SwapInTree(tree))
					{
						gHighlightedDrive[1] = driveind ;
						gSelectedDrive[1] = driveind ;

						tree->next = next_tree ;
						/* Set the directory with check mark correctly */
					   listinfo[1].files=GetNthVisibleDir(tree, 
														glob.lineselected[1]) ;
					}
				} /* tree2 != NULL */
			} /* TR_DOUBLE handled */
		} /* tree1 swapped in OK */
	} /* tree1 != NULL */

	/* Swapping in complete, now delete the swap file from disk */
	endswap(TRUE) ;
} /* DoSwapIn */


/****	AllocTreeEntry - Give a location to add a record to file tree
**
**	ENTRY
**		tree - ptr to tree header
**	EXIT
**		address of object in tree, or NULL if error
**
**	EFFECTS
**		Modifies tree, allocates memory
**	WARNING
**	    Does not adjust any pointers except filepage ptrs; this includes
**	tree->head.  Does not adjust any _A_NOSIB bits.
*/

/* NOTE: In following two routines page->files is used to cast the address
		 pointed to by 'page' to PENTRY. As 'files' is an array of PENTRies
		 all is well */
PENTRY  AllocTreeEntry(PTREE tree)
{
	PPAGE page;								// page with open slot
	PENTRY node;							// the open slot
	
	if (!tree)
		return FALSE;

	/* Try to get an open slot.  If there is none, allocate a new page.
	*/
	node = FindOpenSlot(tree);
	if (!node)
	{
		page = AllocFarNormalized(sizeof(filepage));
		if (!page)
			return FALSE;

		page->next = NULL;
		for (node=page->files; node < page->files+PAGESIZE; node++)
			node->name[0] = EOS;	// make all nodes unused

		if (!tree->pagehead)
		{
			tree->pagehead = page;
		} else
		{
			tree->pagetail->next = page;
		}
		tree->pagetail = page;

		/* the first entry is returned */
		node = page->files ;
		tree->freeind = 1 ; /* We have just used the entry at index 0! */
	}
	
	return node;
} /* proc AllocTreeEntry */

/****	FindOpenSlot - given a filepage list, locate an empty node in it
**
**	ENTRY
**			page - head of filepage list
**	EXIT
**			address of empty slot, or NULL if none found.
*/
PENTRY FindOpenSlot(PTREE tree)
{
	PENTRY node;
	PENTRY bound;
	PENTRY found;
	PPAGE page ;

	found = NULL ;

	if (tree->holecount == 0)
	{
		/* NOTE: At startup, tree->freeind == PAGESIZE and this will fail */
		if (tree->freeind < PAGESIZE)
		{
#ifndef NOCONSISTENCY
			if (!tree->pagetail)
			{
				printf("*** BUG: pagetail = NULL and will be derefed\n") ;
				exit(0) ;
			}
#endif
			found = tree->pagetail->files + tree->freeind ;
			tree->freeind++ ;
		}
		/* else -- no free slots in last page allocated so far */
	}
	else
	{
#ifdef DBGBEEP
		Beep() ;
#endif

		/* There are holes in the entries allocated so far -- start from
		   first page and search. */
		/* ZZZZZ we could have stored the list of holes or one hole for
		   each tree and use that immediately instead of this search.
		   Or use some smart heuristics to avoid search */

		page = tree->pagehead ;
	
		while (page && !found)
		{
			node = page->files;
			bound = node+PAGESIZE;
			while (node < bound && node->name[0])
				node++;
			if (node >= bound)
				page = page->next;
			else
			{
				found = node;
#ifndef NOCONSISTENCY
				if (tree->holecount <= 0)
				{
					printf("*** holecount=%d '--'ed!\n", tree->holecount) ;
					exit(0) ;
				}
#endif
				tree->holecount-- ;
			}
		} /* while */
	} /* else */
	return found;
} /* proc FindOpenSlot */


PENTRY AllocTreeEntryCompactIfNeeded(PTREE tree)
{
	BOOL retry ;
	PENTRY rptr ;

	do
	{
		rptr = AllocTreeEntry(tree);

		/* If allocation failed, compact a tree & retry */
		if (!rptr)
			retry = CompactOne(FALSE);
		else
			break ;
	} while (retry) ;

	return rptr ;

} /* AllocTreeEntryCompactIfNeeded */

/* Free up any memory allocated to trees that are not being viewed by the
 * user. Currently this function is being called only by ViewFile().
 */
BOOL FreeUnusedFMMemory(void)
{
	PTREE tree ;
	PTREE non_slowtree = NULL ;
	PTREE slowtree = NULL ;
	PTREE DontFreeTree1, DontFreeTree2 ;

	/* DontFreeTree1(2) are the tree(s) that are being displayed to the
	 * user. We do not want to free the memory allocated to them.
	 */
	DontFreeTree1 = listinfo[0].tree ;

	/* In case the user is not in Dual tree mode, mark both trees as the same*/
	if (glob.MaxTree)
		DontFreeTree2 = listinfo[1].tree ;
	else
		DontFreeTree2 = DontFreeTree1 ;

	for (tree= glob.drives ; tree ; tree = tree->next )
	{
		if (tree->Started)
		{
			if ( (tree == DontFreeTree1) || (tree == DontFreeTree2) )
				continue ;

			/* We treat slow drives (network/cd-roms) as special cases.
			 * In case of a network drive, if it has been fully read in
			 * we don't want to free it unless we have no other option.
			 */
			if ( (tree->DriveType != REMOTE_TYPE) &&
												(tree->DriveType != CDROM_TYPE) )
			{
				non_slowtree = tree ;
			}
			else
			{
				/* has tree not been fully read in? */
				if (tree->ContinueTree)
				{
					JunkTree(tree) ;
					return TRUE ;
				}

				slowtree = tree ;
			}
		} /* if tree has been started */

	} /* for */

	/* free the fast tree, if present, before looking for the slow tree
	 * to free
	 */
	if (non_slowtree)
	{
		JunkTree(non_slowtree) ;
		return TRUE ;
	}

	if (slowtree)
	{
		JunkTree(slowtree) ;
		return TRUE ;
	}

	return FALSE ;

} /* FreeUnusedFMMemory */


/* Counts the number of files in directory 'dir' -- Don't recurse */
/* Works only in non-system mode -- as lastdirfile is meaningless in 
	TR_SYSTEM display mode.  */
unsigned CountDirFiles(PENTRY dir, PTREE tree)
{
	unsigned count ;
	PENTRY temp ;
	
	/* dir == NULL implies that we want to count files in the root directory */
	temp = dir ? GetFirstDirFile(dir) : GetFirstRootFile(tree) ;

	if (!temp)
		return 0;
	
	for (count = 1 ; !temp->LASTDIRFILE ; temp = temp->x.f.snext, count++ )
		/* Do Nothing */ ;

	return count;
}

/* Counts the number of files in directory 'dir' -- Don't recurse */
/* Works only in non-system mode -- as lastdirfile is meaningless in 
	TR_SYSTEM display mode.  */
/* ZZZZ Once the selected functions are used -- the normal versions can be
got rid of -- like CountDirFiles, GetNthFile!! */
unsigned CountDirMatchedFiles(PENTRY dir, PTREE tree)
{
	unsigned count ;
	PENTRY temp ;
	
	/* dir == NULL implies that we want to count files in the root directory */
	temp = dir ? GetFirstDirFile(dir) : GetFirstRootFile(tree) ;

	if (!temp)
	{
		return 0;
	}

	count = temp->MATCHESPATTERN ? 1 : 0 ;
	
	while (!temp->LASTDIRFILE)
	{
		temp = temp->x.f.snext ;
		if (temp->MATCHESPATTERN)
			count++ ;
	}

	return count;
}  /* CountDirMatchedFiles */

/* returns the count of files selected in the tree. It also passes by
reference the sum of the sizes of files selected */
unsigned GetTreeSelectedInfo(PTREE tree, unsigned long far *psize)
{
	unsigned count ;
	PENTRY temp ;

	*psize = 0 ;
	if (!tree)
	    return 0 ;

	for (temp = tree->FirstFile, count = 0 ; temp ; temp = temp->x.f.snext)
		if (temp->SELECTED)
	    {
		count++ ;
		*psize += temp->x.f.size ;
	    }

	return count;
}  /* GetTreeSelectedInfo */


#ifdef THISISOLDCODE
unsigned CountTreeMatchedFiles(PTREE tree)
{
	unsigned count ;
	PENTRY temp ;
	
	if (!tree)
		return 0 ;

	for (temp = tree->FirstFile, count = 0 ; temp ; temp = temp->x.f.snext)
		if (temp->MATCHESPATTERN)
			count++ ;

	return count;
}  /* CountTreeMatchedFiles */
#endif

WORD gLastCount=0;
PENTRY gLastEntry=NULL;
PTREE gLastTree =NULL;


VOID ResetTreeOptimizations()
{
     PTREE tree;

    tree = glob.drives;
    while (tree)
    {
	tree->nummatches=RECALCNUMMATCHES;
	tree = tree->next;
    }
    gLastCount=0;
    gLastEntry=NULL;
    gLastTree =NULL;
}

unsigned CountTreeMatchedFiles(PTREE tree)
{
	unsigned count ;
	PENTRY temp ;

	if (!tree)
		return 0 ;

	if(tree->nummatches != RECALCNUMMATCHES)
	    return(tree->nummatches);

	for (temp = tree->FirstFile, count = 0 ; temp ; temp = temp->x.f.snext)
		if (temp->MATCHESPATTERN)
			count++ ;
	return (tree->nummatches=count);
}  /* CountTreeMatchedFiles */

PENTRY GetNthMatchedFile(PENTRY dir, int count, PTREE tree)
{
	PENTRY fil ;
	
	fil = dir ? GetFirstDirFile(dir) : GetFirstRootFile(tree) ;

	if (!fil)
		return NULL;

	/* ZZZZ One idea is to call pmatch here instead of marking all files
	when pattern is changed and then just checking that bit here */
	while (!fil->MATCHESPATTERN)
		if (fil->LASTDIRFILE)
			return (NULL) ;
		else
			fil = fil->x.f.snext ;

	for (; count > 0;count--)
	{
		do
		{
			if (fil->LASTDIRFILE)
				return (NULL) ;
			else
				fil = fil->x.f.snext ;
		} while (!fil->MATCHESPATTERN) ;
	}

	return(fil);

} /* proc GetNthMatchedFile */

#ifdef THISISOLDCODE
PENTRY GetNthFlatMatchedFile(PTREE tree, int count )
{
	PENTRY fil ;
	
	if (!(fil= tree->FirstFile))
		return NULL;

	while (!fil->MATCHESPATTERN)
		if (!(fil = fil->x.f.snext))
			return (NULL) ;

	for (; count > 0;count--)
	{
		do
		{
			if (!(fil = fil->x.f.snext))
				return (NULL) ;
		} while (!fil->MATCHESPATTERN) ;
	}

	return(fil);

} /* proc GetNthFlatMatchedFile */
#endif
PENTRY GetNthFlatMatchedFile(PTREE tree, int count )
{
	WORD start;
	PENTRY fil;

	if (CountTreeMatchedFiles(tree) <= count)
	{
	    return(gLastEntry=NULL);
	}

	if (((gLastCount <= count) && (tree == gLastTree) && (gLastEntry != NULL)))
	{
	    start = gLastCount;
	}
	else
	{
	    gLastEntry = tree->FirstFile;
	    start = 0;
	}

	fil = gLastEntry;

	/* Claim: 'fil' can never be NULL from now on! Reason: tree->nummatches is
	*	strictly greater than count!
	*/
	gLastCount = count;
	gLastTree = tree;
	while(!fil->MATCHESPATTERN)
		 fil=fil->x.f.snext;
	
	while (start++ < count)
	{
	   do
	   {
		 fil=fil->x.f.snext;
	   }
	   while(!fil->MATCHESPATTERN);
	}
	return(gLastEntry=fil);

} /* proc GetNthFlatMatchedFile */

/* ZZZZZZZ Is this routine needed anymore? */
/****	GetNthDir - get the 'nth' directory in the tree, in depth-first order
**	This is used by MakeTreeLine().
**
**	ENTRY
**		branch - branch to search along
**		count  - which record you want
**	EXIT
**		ptr to appropriate record, or NULL if not found
*/
PENTRY GetNthDir(tree, count)
treehdr far *tree;
int count;
{
	int curcount;
	PENTRY dir;

	if (count > 0)
	{
		dir = tree->FirstDirectory;
		for(curcount = 1; ((curcount < count) && dir) ;curcount++)
		{
			dir = dir->x.d.dnext;
		}
	}
	else
	{
	    dir = NULL;
	}
	return(dir);
} /* GetNthDir */

/****	GetNthVisibleDir - get the 'nth' directory in the tree,
**      		in depth-first order that is being displayed to the user.
**	This is used by MakeTreeLine().
**
**	ENTRY
**		branch - branch to search along
**		count  - which record you want
**	EXIT
**		ptr to appropriate record, or NULL if not found
*/
PENTRY GetNthVisibleDir(PTREE tree, int count)
{
	int curcount;
	PENTRY dir;

	if (count > 0)
	{
		dir = tree->FirstDirectory;
		curcount = dir->DISPLAYED; /* ZZZ It's OK if dir is NULL in DOS! */

		while (dir && (curcount < count))
		{
			dir = dir->x.d.dnext ;
			if (dir->DISPLAYED) /* ZZZ It's OK if dir is NULL in DOS! */
				curcount++ ;
		} /* while */

	}
	else
	{
	    dir = NULL;
	}
	return(dir);
} /* GetNthVisibleDir */

/* Returns a BOOL to indicate if a given directory has got any sub-dirs */
BOOL AreThereSubDirs(PTREE tree, PENTRY dir)
{
	PENTRY child ;

	if (!dir)
		// root directory has sub-directories if there is any directory at all
		return (tree->DirCount > 0) ;
	else
	{
		child = dir->x.d.child ;
		while (child)
		{
			if (child->attribs & _A_SUBDIR)
				return TRUE ;
			else
				/* ZZZZZ following can be optimized using '-' and '&' ops */
				child = (child->nosib) ? NULL : child->sibling ;
		}
		return FALSE ; /* dir does not have any sub-dirs */
	}
} /* AreThereSubDirs */

/* Expand visiblity of directory 'dir' -- recursive routine.
 * The recursion should not cause concern regarding stack space as the
 * maximum recursion level is bounded by the nesting of directories.
 * The directory 'dir' is assumed to have been already been read into 
 * memory. It is now only a matter of setting the collapse bits for 'dir'
 * and its sub-dirs. It expands all levels or 1 level depending on 'fAllLevels'.
 * 'dir' is assumed to be in a non-expanded form at this stage.
 */
void ExpandDirVisibility(PTREE tree, PENTRY dir, BOOL fAllLevels)
{
	PENTRY child;

	if ( ((!dir) && (tree->DirCount > 0)) || (dir->HASSUBDIRS) )
	{
		if (dir)
		{
			/* Assumption: 'dir' must already be visible! */
			dir->EXPANDED = TRUE ; 
			child = dir->x.d.child ;
		}
		else
			child = tree->head ;
	
		while (child)
		{
			if (child->attribs & _A_SUBDIR)
			{
				/* child's HASSUBDIRS bit assumed to be already set correctly */
				child->DISPLAYED = TRUE ; 
				tree->VisibleDirCount++ ;
	
				/* we want to expand all levels of 'dir', so expand 'child' */
				if (fAllLevels)
					ExpandDirVisibility(tree, child, fAllLevels) ;
			}
	
			/* ZZZZZ following can be optimized using '-' and '&' ops */
			child = (child->nosib) ? NULL : child->sibling ;
		}
	}
	/* else 'dir' does not have any sub-dirs, so no expansion required! */
} /* ExpandDirVisibility */


/* Collapse visiblity -- All sub-dirs of 'dir' made invisible -- recursive.
 * The recursion should not cause concern regarding stack space as the
 * maximum recursion level is bounded by the nesting of directories.
 * The directory 'dir' is assumed to have been already been read into 
 * memory. It is now only a matter of setting the collapse bits for 'dir'
 * and its sub-dirs. It makes all sub-dirs of 'dir' invisible.
 */
void CollapseDirVisibility(PTREE tree, PENTRY dir)
{
	PENTRY child;

	if ( ((!dir) && (tree->VisibleDirCount > 0)) || (dir->EXPANDED) )
	{
		if (dir) 
		{
			/* Assumption: 'dir' must already be visible! */
			dir->EXPANDED = FALSE ; 
			child = dir->x.d.child ;
		}
		else
			child = tree->head ;
	
		while (child)
		{
			if ( (child->attribs & _A_SUBDIR) && (child->DISPLAYED) )
			{
				/* child's HASSUBDIRS bit assumed to be already set correctly */
				child->DISPLAYED = FALSE ; 
				tree->VisibleDirCount-- ;

				if (child->EXPANDED)
					CollapseDirVisibility(tree, child) ;
			}
	
			/* ZZZZZ following can be optimized using '-' and '&' ops */
			child = (child->nosib) ? NULL : child->sibling ;
		}
	}
	/* else 'dir' not expanded, so no collapseing required! */
} /* CollapseDirVisibility */

/****	GetDirInfo - Get Total Number of files in a directory & sum of their
**  sizes. Used by DispFlatLeft. Returns the count of directories. The size
**  is passed back in psize (passed by reference). The number of files in
**  dir that are selected are also passed back by reference.
*/
unsigned GetDirInfo(PENTRY dir, PTREE tree, unsigned long *psize,
																unsigned int *pNumSelFiles)
{
	PENTRY temp ;
	unsigned count ;

	*psize = *pNumSelFiles = count = 0 ;
	
	temp = (dir) ? dir->x.d.child : tree->head ;
	
	while (temp)
	{ 
      if (!(temp->attribs & _A_SUBDIR) )
		{
			count++ ;
			*psize += temp->x.f.size ;

			if (temp->SELECTED)
				(*pNumSelFiles)++ ;
		}

		temp = (temp->nosib) ? NULL :	temp->sibling ;
	}

	return count ;

} /* GetDirInfo */

extern char szStarDotStar[] ;

/****	JunkTree - deallocate all file space from given tree
**		This fn is called when memory is low, or before rereading a tree
**	(e.g. a floppy tree).  It deletes all memory used to store the tree's
**	file nodes, and resets tree elements such as filecount and dircount.
**
**	ENTRY
**		tree - the tree to hack up
**	EXIT
**		none
*/
VOID JunkTree(treehdr far *tree)

{
	PPAGE page, next;

	gfFlatLeftInfoDrawn = FALSE ; /* this initialization helps in system tree mode */

	/* Free all file pages. */
	for (page = tree->pagehead ; page ; page = next )
	{
		next = page->next ;
		FreeFarNormalized(page);
	}

	tree->head = tree->lastlink = NULL;
	tree->pagehead = tree->pagetail = NULL;

	tree->parent = NULL ;
	
	tree->FirstDirectory = tree->LastDirectory = NULL;
	tree->FirstFile = tree->LastFile = NULL;

	tree->Diskfilecount = tree->filecount =  0;
	tree->DirCount = tree->VisibleDirCount = 0;
	ResetTreeOptimizations() ; // ZZZZ I think I can call ResetTreeMatchedFilesOpt()

	tree->Started = FALSE;
	tree->Compacted = FALSE;
	tree->ContinueTree = TRUE;

	tree->fdiskinfoknown = FALSE ;
	tree->SortRequired = TRUE ;

	/* Initially at read-in time, we set
	 * "matchespattern" bit to be true for all
	 * files, which means this!
	 */
	tree->DisplayHiddenFiles = TRUE ;
	strfcpy(tree->mpat, szStarDotStar) ;
	tree->skey = tree->tmode = -1 ; /* non-sort key, non-tree mode */

	tree->NumSel = 0 ;
	tree->SizeSel = 0 ;

	tree->SelDir = NULL ; // ZZZZ default directory goes back to ROOT!!
	tree->SelLine = 0 ;

	tree->holecount = 0 ;
	tree->freeind = PAGESIZE ;

} /* proc JunkTree */

/****	CompactifyTree - expunge file nodes from tree, leaving directories
**		This fn attempts to conserve memory by deleting all non-directory
** nodes from the given tree and putting the remaining nodes in the fewest
** possible filepages.
**
**	ENTRY
**		tree - tree to compact
**		sel  - FALSE = delete all files; compress directories into first few
**					   filepages.
**			   TRUE  = only delete unselected files; don't compress directories.
**	EXIT
**		TRUE if we compressed things, FALSE if not.
*/
BOOL CompactifyTree(tree, sel)
PTREE tree;
BOOL sel;

{
	PENTRY dir;							// directory under scrutiny
	PENTRY node;						// for following ptrs

#ifdef COMPACTCHECK
com1("IN CompactifyTree\n") ;
#endif

	if (tree->Compacted)
		return FALSE;

	/* ZZZZ Will the following field being set cause problems if we end up
	 * doing the CLobberSel() part of the code?
	 */
	/* Note that even empty trees are tagged! */
	tree->Compacted = TRUE ;

	/* If "sel" is TRUE we want to retain file selections in the tree, no
	 * matter what! ELSE if the tree in the focus listbox is the one
	 * being compacted and there are files selected in it, we don't want
	 * to lose these selections!
	 */
	/* ZZZZ This could cause loss of unselected files from the file listbox
	 * for the dirtectory being displayed temporarily. Could also be not
	 * OK in dual tree mode if wrong tree has focus!
	 */
	if ( (sel) || ((listinfo[glob.FocusBox].tree == tree) && (tree->NumSel)) )
	{
		if (tree->Started)
		{
			/* This is not TRUE COMPACTION!! as we might still have files
			 * selected in other diretories (if cross dir selection ON!)
			 */
			/* Note that root dir (NULL) is the parent of tree->head */
			ClobberSel(tree, NULL, tree->head);

			return TRUE;
		}
		else
			return FALSE;
	}
	else
	{
		if (!tree->Started)
			return FALSE;

		/* If the tree being compacted is not the one being operated upon
		 * (one in focus!) Deselect all files belonging to it.
		 */
		if ((listinfo[glob.FocusBox].tree != tree) && (tree->NumSel))
		{
			DeselectTree(tree) ;
			if ( (glob.TreeMode == TR_DOUBLE) &&
									(listinfo[1-glob.FocusBox].tree == tree) )
			{
				InsertListItem(&FileList[1-glob.FocusBox], 0) ;
			}
		}

		/* Fix tree->head to point to the first directory in the sibling
		 * chain of the root directory. Note that this will in general
		 * not be tree->FirstDirectory as we now sort dirs by name!
		 */
		 if ( (node = tree->head) && (!(node->attribs & _A_SUBDIR)) )
		 {
			while(node)
			{
				/* Note that we need not look at 'nosib' as parent is NULL  */
				node = node->sibling ; 

				if (node->attribs & _A_SUBDIR) // It is OK even if node is NULL!
					break ; 
			}
			tree->head = node ;
		 }
		 /* else tree->head is NULL or it is pointing to correct dir already */

		// Make sibling, child ptrs of dirs point only to other dirs.
		dir = tree->FirstDirectory;
		while (dir)
		{
			dir->FIRSTFILEFOUND = FALSE ; /* we are now junking all files
											belonging to dir */
			if (!dir->nosib)
			{
				node = dir->sibling;
				while (!((node->attribs & _A_SUBDIR) || node->nosib))
					node = node->sibling;
				
				if (node->attribs & _A_SUBDIR)
					// found one, make it the sibling
					dir->sibling = node;
				else
				{
					// no more dirs, save parent ptr
					dir->nosib = TRUE;
					dir->sibling = node->sibling;
				}
			}
		
			node = dir->x.d.child;
			if (node)
			{
				while (!((node->attribs & _A_SUBDIR) || node->nosib))
					node = node->sibling;

				if (node->attribs & _A_SUBDIR)
					// found one, make it the first child
					dir->x.d.child = node;
				else
				{
					// no more dirs
					dir->x.d.child = NULL;
				}
			}
			dir = dir->x.d.dnext;
		}

		/* Warning! It is assumed that call to ClobberFiles is always
			immediately followed by PackDirs */
		ClobberFiles(tree);
	
		// Now put all remaining nodes in a block at the front of the page list.
		PackDirs(tree);
	}
/* ZZZZZZZZ */
/* Following is a major bug left by Jeff!! His comment is funny!! */
/* We need to use this field -- lastlink when filesystem is being read in */
/* Since we have thrown out the files, tree->lastlink should be
*  set to the last directory read in so far! If we ran out of memory
*  before encountering the first sub-directory, set tree->head to NULL so
*  as to start reding from the root directory */
#if 0
	tree->lastlink = NULL;					// don't think too hard about it
#else
	tree->lastlink = tree->LastDirectory ;
	if (!tree->lastlink)
		tree->head = NULL ;
	/* else tree->head will be correctly pointing to the first directory! */
#endif
#ifdef COMPACTCHECK
com1("OUT CompactifyTree\n") ;
#endif
	return TRUE;
} /* proc CompactifyTree */

/****	ClobberFiles - delete all unselected files in the given tree
**
**	ENTRY
**			tree - tree to modify
**	EXIT
**			none
**	WARNING
**		This fn does not signal for listboxes to be updated.
*/
void ClobberFiles(PTREE tree)
{
	PPAGE page;								// filepage under examination
	PENTRY node;							// file record within page
	int i;

	// Eliminate all file nodes from tree.
	page = tree->pagehead;
	while (page)
	{
		node = page->files;
		for (i=0; i < PAGESIZE; i++)
		{
			/* ZZZZ Warning! It is assumed that this function ClobberFiles
				call is followed immediately by a PackDirs call, so
				we don't increment tree->holecount here. PackDirs assigns
				the right value to it */
			if (!(node->attribs & _A_SUBDIR))
				node->name[0] = EOS;
			node++;
		}
		page = page->next;
	}
	tree->FirstFile = tree->LastFile = NULL;
	tree->filecount = 0;

	/* Note that we have deleted all files belonging to this tree. So
	 * mark it so that the tree is not sorted in any order at all!
	 */
	SetUpTreeForSort(tree) ;

} /* proc ClobberFiles */

/****	ClobberDir - delete all normal files in a given directory
**
**	ENTRY
**			tree - the tree
**			dir  - the directory
**	EXIT
**		none
**	WARNING
**		This fn does not signal for listboxes to be updated; this should be
**	done by the caller.
*/
void ClobberDir(PTREE tree, PENTRY dir)
{
	PENTRY curr;						// node being examined
	PENTRY next;						// the next one to look at

	if (dir)
		curr = dir->x.d.child;
	else
		curr = tree->head;

	// Remove all normal files from the list.
	while (curr)
	{
		if (curr->nosib)
			next = NULL;
		else
			next = curr->sibling;

		if (!(curr->attribs & _A_SUBDIR))
		{
			DelFileEntry(tree, dir, curr);
		}
		curr = next;
	}

	/* All files belonging to this directory have been thrown out, so
	 * clear these following bits.
	 */
	dir->DIRSORTED = FALSE ;
	dir->FIRSTFILEFOUND = FALSE ;

} /* proc ClobberDir */

/****	ClobberSel - delete all selected files in a branch and its twigs
**		This fn deletes selected files in the given directory and all of its
**	subdirectories.
**
**	ENTRY
**			tree - the tree
**			parent  - the parent directory of curr
**			curr - first file/directory belonging to parent
**	EXIT
**		none
**	WARNING
**		This fn does not ask for listboxes to be updated when it is done,
**	because the fn is assumed to be called in the middle of a fn where it is
**	not feasible to compact the whole tree.  At the end of the fn,
**	CompactifyTree should be called to delete the remaining files and cause
**	the listboxes to be redrawn.
*/
void ClobberSel(PTREE tree, PENTRY parent, PENTRY curr)
{
	PENTRY next;						// the next one to look at

	while (curr)
	{
		if (curr->nosib)
			next = NULL;
		else
			next = curr->sibling;

		// If directory, recurse; if normal file, maybe delete it.
		if (!(curr->attribs & _A_SUBDIR))
		{
			if (!curr->SELECTED)
				DelFileEntry(tree, parent, curr);
		} else
		{
			ClobberSel(tree, curr, curr->x.d.child);
		}
	
		curr = next;
	}
} /* proc ClobberSel */

/****	PackDirs - Move directory nodes to front of page list, free excess pages
**		This fn takes a tree which has no normal file records remaining.  It
**	puts all the remaining (directory) nodes into sequential records at the
**	front of the page list, and then frees any completely empty pages.
**
**	ENTRY
**			tree - tree to modify
**	EXIT
**			none
**	WARNING
**		There must not be any normal file nodes remaining.  The pointer
**	fixup portion expects only to deal with directory nodes.
*/
void PackDirs(PTREE tree)
{
	PPAGE freepage;						// page of unused slot
	PENTRY freeslot;					// unused slot
	PENTRY freebound;					// first record beyond the current page
	
	PPAGE dirpage;						// page of directory to move
	PENTRY dirslot;						// directory to move
	PENTRY dirbound;					// first record beyond the current page

	PENTRY prevdir;						// previous dir, in dnext order
	PENTRY parent;						// parent of directory
	PENTRY sib;							// previous sibling of directory
	PENTRY child;						// last child of directory
	PPAGE temppage;						/* temporary variable */

	freepage = tree->pagehead;
	if (!freepage)
		return;

#ifdef COMPACTCHECK
com1("In PackDirs\n") ;
#endif

	freeslot = freepage->files;
	freebound = freeslot+PAGESIZE;
	dirpage = freepage;
	prevdir = NULL;
	do {
		// Search for an open slot in this page.
		if (freeslot->name[0] == EOS)
		{
			// found an open slot; find the next directory node.
			if (freeslot < (freebound-1))
			{
				dirpage = freepage;
				dirbound = freebound;
				dirslot = freeslot+1;
			}
			else
			{
				dirpage = freepage->next ;
				if (dirpage)
				{
					dirslot = dirpage->files ;
					dirbound = dirslot + PAGESIZE ;
				}
			}
			
			while (dirpage)
			{
				if (dirslot->name[0])
				{
					/* Fix ptrs in records which point to this one.
					*/

					// Adjust parent/sibling pointers.
					parent = FindParent(dirslot);
					if (!parent)
					{
						if (tree->head == dirslot)
						{
							tree->head = freeslot;
						} else
						{
							sib = tree->head;
							while (sib->sibling != dirslot)
								sib = sib->sibling;
							sib->sibling = freeslot;
						}
					} else
					{
						if (parent->x.d.child == dirslot)
						{
							parent->x.d.child = freeslot;
						} else
						{
							sib = parent->x.d.child;
							while (sib->sibling != dirslot)
								sib = sib->sibling;
							sib->sibling = freeslot;
						}
					}

					// Adjust child pointer.
					child = dirslot->x.d.child;
					if (child)
					{
						while (!child->nosib)
							child = child->sibling;
						child->sibling = freeslot;
					}

					// Adjust x.d.dnext pointer.
					if (prevdir && prevdir->x.d.dnext == dirslot)
					{
						prevdir->x.d.dnext = freeslot;
					} else
					{
						prevdir = tree->FirstDirectory;
						while (prevdir && prevdir->x.d.dnext != dirslot)
							prevdir = prevdir->x.d.dnext;
						if (prevdir)
							prevdir->x.d.dnext = freeslot;
						else
							tree->FirstDirectory = freeslot;
					}
					prevdir = freeslot;

					if (tree->LastDirectory == dirslot)
						tree->LastDirectory = freeslot;

					if (tree->parent == dirslot)
						tree->parent = freeslot;

					if (tree->SelDir == dirslot)
						tree->SelDir = freeslot ;

					/* For the two trees that could be displayed */
					if (listinfo[0].files == dirslot)
						listinfo[0].files = freeslot ;

					if (listinfo[1].files == dirslot)
						listinfo[1].files = freeslot ;

					*freeslot = *dirslot;
					dirslot->name[0] =EOS;
					
					break;
				} else
				{
					dirslot++;
					if (dirslot >= dirbound)
					{
						dirpage = dirpage->next;
						if (dirpage)
						{
							dirslot = dirpage->files;
							dirbound = dirslot+PAGESIZE;
						}
					}
				}
			} /* end while (dirpage) */
		}

		if (dirpage)
		{
			freeslot++;
			if (freeslot >= freebound)
			{
				// no open slots in this page; try the next one.
				freepage = freepage->next;
				if (freepage)
				{
					freeslot = freepage->files;
					freebound = freeslot+PAGESIZE;
				}
			}
		} else
		{
			/* Couldn't find a directory to copy to the free slot we found,
			** so there aren't any more.  Free any filepages after the current
			** one because they contain no records.
			*/
#ifdef COMPACTCHECK
			if (!freepage)
			    com1("BOGOSITY--freepage is NULL!\n");
			else
			    com1("FREEPAGE IS OK\n");
#endif
			temppage = freepage ;

			freepage = freepage->next;
			while (freepage)
			{
				dirpage = freepage->next;
				FreeFarNormalized(freepage);
				freepage = dirpage;
			}

			temppage->next = NULL ;
		}
	} while (freepage);

	/* Now assign to pagetail the right page address, set holecount and
	   freeind correctly */

	tree->holecount = 0 ; /* No holes present as packing is tight now! */
	if (!tree->pagehead)
	{
		tree->pagetail = NULL ;
		tree->freeind = PAGESIZE ; /* => no free slots in last page as
										there is no last page!! */
	}
	else
	{
		/* assertion: tree->pagetail can't be NULL now! */
		for (tree->pagetail = tree->pagehead ; 
				(tree->pagetail->next) ; tree->pagetail = tree->pagetail->next)
			; /* Do Nothing */

		freeslot = tree->pagetail->files ; /* reuse of variable for start */
		freebound = tree->pagetail->files + PAGESIZE ; /* reuse again */

		for ( ; ( (freeslot < freebound) && (freeslot->name[0]) ) ; freeslot++)
			; /* Do Nothing */
		
		tree->freeind = (freeslot - (PENTRY) tree->pagetail->files) ;
	}
#ifdef COMPACTCHECK
com1("OUT PackDirs\n") ;
#endif

} /* proc PackDirs */

/* Given two trees -- It selects one of the two for compaction.
*
*  'ctree' -- the current candidate for compaction -- can be NULL.
*  'tree'  -- the new candidate for compaction.
*  This routine checks the drive types of the two and decides which one of
*  this is to be compacted.
*  Ram Drives are selected first of course...
*  Remote_Drives (network drives) are selected over any other type. Between 2
*	    remote_drives, 'ctree' is chosen instead of 'tree'.
*  Hard_Drives are selected next. Between 2 Hard_Drives 'tree' is chosen
*	    instead of 'ctree'.
*  Floppy_Drives are chosen last. Between 2 Floppy_Drives 'tree' is chosen
*	    instead of 'ctree'.
* The tie-breaking rule uses the fact that 'ctree' has a lexicographically
*   smaller alphabet that 'tree' -- that is the way the caller presently passes
*   in the arguments. The tie-breaking rule is just a nice heuristic I chose.
* BUG BUG this should be a macro
*/
PTREE ChooseCompactionTree(PTREE ctree, PTREE tree)
{
    PTREE ret_tree ;

    ret_tree = ( (
		 (!ctree) ||
		 (ctree->DriveType == FLOPPY_TYPE) ||
		 ((ctree->DriveType == HARDDISK_TYPE) &&
				    (tree->DriveType != FLOPPY_TYPE))
		 && (ctree->DriveType != RAMDRIVE_TYPE))
	       ) ? tree : ctree ;

    return ret_tree ;

} /* ChooseCompactionTree */

BOOL CompactOne(BOOL sel)
{
	PTREE tree, ctree;
	int i;
	BOOL ret;	  // return value of CompactifyTree

	ctree = NULL ;
	tree = glob.drives ;
	while (tree)
	{
	    if ( (!tree->Compacted) && (tree->Started) )
			ctree = ChooseCompactionTree(ctree, tree) ;
		tree = tree->next ;
	} /* while */

	if (!ctree)
	{
		/* ZZZZZZZ Change to SingleTree mode at this point!! If we are in
		   double tree mode junk the second tree */
		/* All trees are either compacted or not started. At this point
		    we try to totally get rid of trees that have been started but that
		    are not being displayed.
		*/
		tree = glob.drives ;
		while (tree)
		{
			/* ZZZ Assumption: We should move user out of DUAL tree mode at this
			 * point, if he is in one!
			 */
			if ( tree->Started && (listinfo[glob.FocusBox].tree != tree) )
			{
				if ( (glob.TreeMode == TR_DOUBLE) &&
											(listinfo[1-glob.FocusBox].tree == tree) )
				{
					listinfo[1-glob.FocusBox].tree = listinfo[glob.FocusBox].tree ;
					listinfo[1-glob.FocusBox].files = listinfo[glob.FocusBox].files ;
					glob.lineselected[1-glob.FocusBox] = glob.lineselected[glob.FocusBox] ;
				}

				JunkTree(tree) ;
				return TRUE ;
			}
			else
				tree = tree->next ;
		} /* while */

		return FALSE;
	}

#ifdef DBGBEEP
Beep() ; Beep() ; Beep() ; Beep() ;
#endif

	/* compact 'ctree' */
	ret = CompactifyTree(ctree, sel);

	/* If 'ctree' has already been completely read in and is being
	  displayed load the files belonging to the directory being displayed */
	/* If it is still being read in, LoadCompactDir will be done after
	   reading in all directories belonging to this tree */
	for (i = 0 ; i <= glob.MaxTree ; i++)
	{
		if ( (listinfo[i].tree == ctree) && (listinfo[i].tree->Started) &&
										(!listinfo[i].tree->ContinueTree) )
			if ( (i != 1) || (listinfo[1].tree != listinfo[0].tree) ||
								listinfo[0].files != listinfo[1].files )
				LoadCompactDir(listinfo[i].tree, listinfo[i].files) ;
	}

	return ret;
} /* proc CompactOne */

/* This routine frees up all the memory allocated to the FileManager that
 * was used to store the TREE/FILE PAGE information.
 */
void FreeFMMemory(void)
{
	PTREE tree, nexttree ;
	PPAGE page, nextpage ;

	for (tree = glob.drives ; tree ; tree = nexttree)
	{
		/* Free all file pages. */
		for (page = tree->pagehead ; page ; page = nextpage )
		{
			nextpage = page->next ;
			FreeFarNormalized(page);
		} /* file page loop */

		nexttree = tree->next ;

		/* free memory allocated to tree data structure */
		FreeWorkFar(tree) ;
	} /* tree loop */
} /* FreeFMMemory */

void CompactAll()
{
	PTREE tree;

	for (tree = glob.drives; tree; tree = tree->next)
		CompactOne(FALSE) ;
}

/* Function returns the index of the directory in dnext chain counting only
 * directories that are currently marked as being displayed in the dir listbox.
 */
unsigned GetIndexVisibleDir(PENTRY dir, PTREE tree)
{
	PENTRY temp ;
	unsigned count ;

	/* dir == NULL => root directory at index 0 */
	if (!dir)
		return 0 ;

	count = 1 ;
	/* Warning! dir should be a directory node in the tree, thus temp guaranteed
	   not to become NULL!!  */
	for (temp = tree->FirstDirectory ; temp != dir ; temp = temp->x.d.dnext)
	{
#ifndef NOCONSISTENCY
		if (!temp)
		{
			printf("*** GetIndexVisibleDir -- dir not found!!\n") ;
			exit(0) ;
		}
#endif
		if (temp->DISPLAYED)
			count++ ;
	} /* for */

	return count ;
} /* GetIndexVisibleDir */

/* Expands ancestors (parent, grand-parent, etc)  of 'dir', if necessary, to
 * make dir visible in the directory listbox.
 */
void MakeDirVisible(PENTRY dir, PTREE tree)
{
	PENTRY parent ;

	/* Root dir (i.e., dir == NULL) is always visible! If dir is a non-root
	 * dir, we handle it in the manner shown below.
	 */
	if (dir)
	{
		/* We need to see if the directory under consideration, namely,
	 	* listinfo[0].files, is visible, if not we should expand it's
	 	* parent's visibility and this could be recursive until we reach
	 	* a visible directory (Note that the root dir is always visible).
	 	*/
	 	if (!dir->DISPLAYED)
	 	{
			MakeDirVisible((parent = FindParent(dir)), tree) ; // recursion!
		
			/* Expand 'parent' by 1 level -- "FALSE" means just that! */
			ExpandDirVisibility(tree, parent, FALSE) ;
	 	}
	}
} /* MakeDirVisible */

/* Makes the directory that is 'selected' (the one that is supposed to have 
 * the tick mark) visible in the dir listbox. This is needed when we switch from
 * 'system' tree mode to single/double tree mode, so that the right directory
 * has the tick mark.
 */ 
void SetSelectedDirFocus(void)
{
	MakeDirVisible(listinfo[0].files, listinfo[0].tree) ;
	glob.lineselected[0] = 
					GetIndexVisibleDir(listinfo[0].files, listinfo[0].tree) ;
} /* SetSelectedDirFocus */


unsigned CountFilesToOperateOn(void)
{
	unsigned int ret ;
	unsigned long DummyDirSize ;

	if (FPerformOpOnDirFilesAlone())
	{
		GetDirInfo(listinfo[glob.FocusBox].files, listinfo[glob.FocusBox].tree,
								&DummyDirSize, &ret) ;
	}
	else
	{
		ret = listinfo[glob.FocusBox].tree->NumSel ;
	}
	return ret ;

} /* CountFilesToOperateOn */


/* We want to perform operations on files SELECTED that are in the
 * focus box and not all the files selected (say in DUAL TREE mode). This
 * function sets the bit FILEOPMARKER for all the files that we want to
 * operate on.
 */
void MarkFilesBeforeFileOp(void)
{
	PTREE tree ;
	PENTRY dir, temp ;
	PENTRY node ;

	tree = listinfo[glob.FocusBox].tree;

	if (FPerformOpOnDirFilesAlone())
	{
		/* First clear the 'FILEOPMARKER' bit for all files */
		for (node = tree->FirstFile ; node ; node = node->x.f.snext)
		{
			node->FILEOPMARKER = FALSE ;
		}
		
		/* Now set the 'FILEOPMARKER' bit for all files selected in 'dir' */
		dir = listinfo[glob.FocusBox].files ;
		temp = (dir) ? dir->x.d.child : tree->head ;
		while (temp)
		{
  			if (!(temp->attribs & _A_SUBDIR) )
			{
				if (temp->SELECTED)
					temp->FILEOPMARKER = TRUE ;
			}
			temp = (temp->nosib) ? NULL :	temp->sibling ;
		} /* while */
	}
	else
	{
		/* All SELECTED files need to be operated on! */
		for (node = tree->FirstFile ; node ; node = node->x.f.snext)
		{
			node->FILEOPMARKER = node->SELECTED ;
		}
	}
} /* MarkFilesBeforeFileOp */

/* Deselect all files in 'tree' that are marked as not matching the global
 * MatchPat. This is done because wer don't want the user accidentally
 * performing operations on files that he does not see in listboxes.
 * It returns a BOOL indicating whether any files were de-selected here.
 */
BOOL DeselectUnMatchedFiles(PTREE tree)
{
	unsigned int numsel ;
	PENTRY fil ;

	if (tree->NumSel == 0)
		return FALSE ;

	numsel = tree->NumSel ;
	for (fil = tree->FirstFile ; fil ; fil = fil->x.f.snext)
	{
		if (!fil->MATCHESPATTERN)
		{
			if (fil->SELECTED)
			{
				tree->NumSel-- ;
				tree->SizeSel -= fil->x.f.size ;
				fil->SELECTED = FALSE ;
			}
		}
	}

	return (numsel != tree->NumSel) ;

} /* DeselectUnMatchedFiles */



/* ZZZZZ It might be possible to combine some of the count functions, etc into
  one smart monolithic function.
*/

#if 0
/*  ZZZ unused fns! ***********************************************         */
unsigned int CountTotalFilesSelected(void)
{
      unsigned count ;

      count = listinfo[0].tree->NumSel ;

#ifdef DEBUG
      SelCountCheck(listinfo[0].tree) ;
#endif
      if ( (glob.MaxTree == 1) && (listinfo[1].tree != listinfo[0].tree) )
      {
#ifdef DEBUG
	      SelCountCheck(listinfo[1].tree) ;
#endif
	      count += listinfo[1].tree->NumSel ;
      }

    return count ;
} /* CountTotalFilesSelected */

/****	GetNthFile - get file from top level of branch
**	This fn gets the 'n'th file of the branch.  It does not search

**	in any subdirectories of the branch.
**
**	ENTRY
**		PENTRY dir;  directory whose nth file we need.
**		unsigned count; a value of 0 for first file & so on.
**	EXIT
**		ptr to requested file, or NULL if not found.
*/
/*  ZZZZZZ Previous optimizations using static stuff is gone */
/*  we can still do it if tree building is over!! */
PENTRY GetNthFile(PENTRY dir, int count, PTREE tree)
{
	PENTRY fil ;
	
	fil = dir ? GetFirstDirFile(dir) : GetFirstRootFile(tree) ;

	if (!fil)
		return NULL;
	
	for (; count > 0;count--)
	{
		if (fil->LASTDIRFILE)
			/* set the static pointer node before returning */
	 		return (NULL);
		fil = fil->x.f.snext ;
	}

	return(fil);

} /* proc GetNthFile */

/****	CountBranch - count number of files in a branch
**	This fn takes a chunk of a tree and counts the number of files, or
**	directories, or both, in it.
**
**	ENTRY
**		current - first node of branch to count
**		flags   - behavior modification, OR'ed together:
**			  CB_NORMAL  - count files
**			  CB_SUBDIR  - count directories
**			  CB_DESCEND - count things in subtrees too
**	EXIT
**		the number of files found
*/
unsigned CountBranch(current, flags)
PENTRY current;
unsigned flags;

{
	unsigned count=0;
	unsigned attr;
	
	if (!current)
		return 0;
	
	do
	{
		attr = current->attribs;
		if (attr & _A_SUBDIR)
		{
			if (flags & CB_SUBDIR)
				count++;
			if (flags & CB_DESCEND)
				count +=CountBranch(current->x.d.child,flags);
		} else
		{
			if (flags & CB_NORMAL)
				count++;
		}
		if (current->nosib)
			break;
		current = current->sibling;
	} while (1);
	
	return count;
}

/* Function returns the index of the directory in dnext chain */
unsigned GetIndexDir(PENTRY dir, PTREE tree)
{
	PENTRY temp ;
	unsigned count ;

	/* dir == NULL => root directory at index 0 */
	if (!dir)
		return 0 ;

	count = 1 ;
	/* Warning! dir should be a directory node in the tree, thus temp guaranteed
	   not to become NULL!!  */
	for (temp = tree->FirstDirectory ; temp != dir ; temp = temp->x.d.dnext)
	{
#ifndef NOCONSISTENCY
		if (!temp)
		{
			printf("*** GetIndexDir -- dir not found!!\n") ;
			exit(0) ;
		}
#endif
		count++ ;
	} /* for */

	return count ;
} /* GetIndexDir */

/*        ***********************************************         */
#endif
