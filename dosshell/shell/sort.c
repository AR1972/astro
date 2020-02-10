;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/* File "sort.c" ---- separated from add.c */

#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>
#include <assert.h>


extern PENTRY GetLastsnextFile(PTREE tree) ;

/* Recursive routine called by FormDirectoryOrder to update the snext chain */
/* It invokes function Updatesnext for each file belonging to 'dir' of 'tree'*/
/* For sub-directories belonging to 'dir', it calls itself recursively and  */
/* gets the same job done for them too. It does this for each file/sub-dir  */
/* belonging to 'dir'.														*/
VOID WalkDirsnext(PTREE tree, PENTRY dir)
{
	PENTRY temp ;

	/* temp is updated to its next sibling in the body of the loop */
	for (temp = dir->x.d.child ; temp ; )
	{
		if (temp->attribs & _A_SUBDIR)
			WalkDirsnext(tree, temp) ;
		else
			Updatesnext(tree, dir, temp, TRUE) ;

		temp = (temp->nosib) ? NULL : temp->sibling ;
	}
}

VOID FormDirectoryOrder(PTREE tree, BOOL doit)
{
	PENTRY temp ;
	int t1, t2 ;

	/* If tree is still being built, don't do anything */
	if (tree->ContinueTree)
		return ;

	t1 = tree->skey ;
	t2 = tree->tmode ;
	tree->skey = glob.SortKey ;
	tree->tmode = glob.TreeMode ;

	tree->SortRequired = FALSE ;

	if ( doit || (t1 != glob.SortKey) || 
			((t2 == TR_SYSTEM) && (t1 != SORT_DISK)) )
	{

		if (!tree->FirstFile)
			return ; /* no files in tree -- so already in correct order */
		
#ifdef DBGBEEP
Beep() ;
#endif
		/* Initialize all directories as having no file based on snext */
		for (temp = tree->FirstDirectory ; temp ; temp = temp->x.d.dnext)
			temp->FIRSTFILEFOUND = FALSE ;

		/* Initialize the snext bits for all the files */
		for (temp = tree->FirstFile ; temp ; temp = temp->x.f.snext)
		{
			temp->FIRSTDIRFILE = temp->LASTDIRFILE = FALSE ;
		}

		tree->FirstFile = tree->LastFile = NULL ;

		/* Note that we don't have to check the temp->nosib field as we know
			that the parents of these files/directories is NULL and that is the
			terminating condition of the for loop. */
		for (temp = tree->head ; temp ; temp = temp->sibling)
		{
			if (temp->attribs & _A_SUBDIR)
				WalkDirsnext(tree, temp) ;
			else
				Updatesnext(tree, NULL, temp, TRUE) ;
		}
	}
}

VOID SystemSort(PTREE tree, BOOL doit)
{
	int t1, t2 ;

	/* If tree is still being built, don't do anything */
	if (tree->ContinueTree)
		return ;

#ifndef NOCONSISTENCY
	if (glob.TreeMode != TR_SYSTEM)
	{
		printf("** SystemSort CALLED IN NON TR_SYSTEM MODE DOSSHELL ERROR") ;
		exit(0) ;
	}
#endif

	t1 = tree->skey ;
	t2 = tree->tmode ;

	/* Mark all directories as being un-sorted */
	SetUpTreeForSort(tree) ;

	tree->skey = glob.SortKey ;
	tree->tmode = glob.TreeMode ; /* should be TR_SYSTEM */

	tree->SortRequired = FALSE ; /* Sort is going to be done now! */

	if (doit || (t1 != glob.SortKey))
	{

		/* SortKey changed or doit !! -- So we have to reorder the whole tree */
		if (glob.SortKey != SORT_DISK)
		{
			if (tree->filecount > 0)
		    {
#ifdef DBGBEEP
			   Beep() ;
#endif
			   tree->FirstFile = mergesort(tree->FirstFile, tree->filecount);
				tree->LastFile = GetLastsnextFile(tree) ;
		    }
			/* else can't mergesort if number of files <= 0 */
		}
		else /* Form Disk Order */
			FormDirectoryOrder(tree, TRUE) ;
	}
	else
			if ( (t1 != SORT_DISK) && (t2 != glob.TreeMode) && 
						(tree->filecount > 0) )
									
			/* Treemode has changed and it is not diskorder -- so we
			   need to sort the whole tree */
		    {
#ifdef DBGBEEP
			   Beep() ;
#endif
			   tree->FirstFile = mergesort(tree->FirstFile, tree->filecount);
				tree->LastFile = GetLastsnextFile(tree) ;
		    }
			/* else can't mergesort if number of files <= 0 */
} /* System Sort */


void SetUpTreeForSort(PTREE tree)
{
	PENTRY dir ;

	tree->SortRequired = TRUE ;

	for (dir = tree->FirstDirectory ; dir ; dir = dir->x.d.dnext)
		dir->DIRSORTED = FALSE ;

	tree->skey = -1 ; /* non-sort key */
	tree->tmode = -1 ; /* non-tree mode */

} /* SetUpTreeForSort */


void DeleteFilesFromsnextChain(PTREE tree, PENTRY dir) ;

void SortDirectory(PTREE tree, PENTRY dir)
{
	PENTRY prevfil, node ;

	/* If tree is still being built, don't do anything */
	if (tree->ContinueTree)
		return ;

	/* does 'dir' need sorting? */
	if ( (dir && (!dir->DIRSORTED)) || ((!dir) && (tree->SortRequired)) )
	{
#ifdef DBGBEEP
	Beep() ;
#endif
		DeleteFilesFromsnextChain(tree, dir) ;

		/* First mark all files belonging to 'dir' as deleted */
		node = (dir) ? dir->x.d.child : tree->head;
		prevfil = NULL ;
		while(node)
		{
			if (!(node->attribs & _A_SUBDIR))
			{
				UpdateLatesnext(tree, dir, node, prevfil, TRUE) ;
				prevfil = node ;
			}

			node = (node->nosib) ? NULL : node->sibling ;
		} /* while */

		if (!dir)
			tree->SortRequired = FALSE ;
		else
			dir->DIRSORTED = TRUE ;
	}
	/* else 'dir' was already in sorted order -- do nothing! */
} /* SortDirectory */

void ForceSortDirectory(PTREE tree, PENTRY dir)
{
	if (dir)
		dir->DIRSORTED = FALSE ;
	else
		tree->SortRequired = TRUE ;

	SortDirectory(tree, dir) ;
} /* ForceSortDirectory */

/* ZZZZZZ for sorting by this key, etc if key values same, do I then
display these in random order or disk order??? The way it is set up now
it will do it in the order it was sorted before and that is good!!.		*/

/* Usually people want reverse sorting by date -- This routine is optimized
 * for that case -- the "if !(gDescending)" will fail in that case.
 * RETURNS : -1 if (a < b), 0 if equal and +1 otherwise.
 *				 This is for ascending order sort, else negated value.
 */
int date_cmp(PENTRY a, PENTRY b)
{
	int ret ;

	ret = ( (b->dtlx.dt.date > a->dtlx.dt.date) -
			  (b->dtlx.dt.date < a->dtlx.dt.date)
			) ;

	/* If dates are the same, use the time field as the secondary key to
	 * compare.
	 */
	if (!ret)
	{
		ret = ( (b->dtlx.dt.time > a->dtlx.dt.time) -
				  (b->dtlx.dt.time < a->dtlx.dt.time)
				) ;
	}

	if (!gDescendingOrder)
		ret = -ret ;
	
	return ret ;

} /* date_cmp */

/* Usually people want reverse sorting by size -- This routine is optimized
 * for that case -- the "if" will fail in that case.
 * RETURNS : -1 if (a < b), 0 if equal and +1 otherwise.
 *				 This is for ascending order sort, else negated value.
 */
int size_cmp(PENTRY a, PENTRY b)
{

	int ret ;

	ret = ( (b->x.f.size > a->x.f.size) - (b->x.f.size < a->x.f.size) ) ;

	if (!gDescendingOrder)
		ret = -ret ;
	
	return ret ;

} /* size_cmp */

#ifndef NDEBUG
/* This function should not be invoked at all!						 */
int disk_cmp(PENTRY a, PENTRY b)
{
	UnReferenced(a) ;
	UnReferenced(b) ;

	assert(TRUE) ;
}
#endif

PENTRY merge(PENTRY a,PENTRY b)
{
    fileinfo z;
    PENTRY c;

    c = &z;
    do
    {
	if ((*SortCmp)(a,b)<1)
	{
	    c->x.f.snext = a;
	    c = a;
	    a = a->x.f.snext;
	}
	else
	{
	    c->x.f.snext = b;
	    c = b;
	    b = b->x.f.snext ;
	}
    }
    while(c->x.f.snext != NULL);
    if (a == NULL)
       c->x.f.snext = b;
    else
       c->x.f.snext = a;
    return(z.x.f.snext);
} /* merge */

PENTRY isort(PENTRY head)
{
    fileinfo a;
    PENTRY b;
    PENTRY c;
    PENTRY d;
    PENTRY e;
    a.x.f.snext = head;
    b = head;
    while(b->x.f.snext != NULL)
    {
	while((*SortCmp)(b,b->x.f.snext)<1)
	{
	    b=b->x.f.snext;
	    if (b->x.f.snext == NULL)
		break;
	}
	if (b->x.f.snext != NULL)
	{
	    c = b->x.f.snext;
	    b->x.f.snext = b->x.f.snext->x.f.snext; /* delete it */
	    /* insert it in correct place */
	    d = &a;
	    /* go until c > one in list */
	    while(!((*SortCmp)(c,d->x.f.snext)<1))
	    {
	       d = d->x.f.snext;
	    }
	    /* insert c after one in list */
	    e = d->x.f.snext;
	    d->x.f.snext = c;
	    c->x.f.snext = e;
	}
	else
	    break;
   }
   if(a.x.f.snext == NULL)
	return(head);
   return(a.x.f.snext);
} /* isort */

PENTRY mergesort(PENTRY c,WORD n)
{
    PENTRY a;
    PENTRY b;
    int i;

    if (n <= 10)
    {
	return(isort(c));
    }
    /*WARNING! following code will not work for very small n */
    a = c;
    for(i=1;i<=(n-1)/2;i++)
    {
	a=a->x.f.snext;
    }
    b = a->x.f.snext;
    a->x.f.snext = NULL;
    return(merge(mergesort(c,i),mergesort(b,n-i)));
} /* mergesort */

/* The reason we use "SortFnArr[SORT_NAME]" instead of the fn "name_cmp"
 * is because depending on whether we have a collating table or
 * whether it is USA/UK we use a slow/fast name compare routine .
 */
void InsertDir(PTREE tree, PENTRY parent, PENTRY dir)
{
	PENTRY after, temp ;
	PENTRY FirstSubDir, LastSubDir ;
	PENTRY prev_dfs_dir ;
	WORD TempDescendingOrder ;

	if (tree->FirstDirectory == NULL)
	{
		/* directory head needs initialization */
		tree->FirstDirectory = dir;
		dir->x.d.dnext = NULL ;
	}
	else
	{
		/* If a directory has subdirectories, we assume that they have
		 * already been inserted in sorted order. This happens right now
		 * only when one tries to rename a directory from within the DOSSHELL
		 * and the current request is to clone windows and insert the renamed
		 * directory in the appropriate sorted order.
		 */
		if (dir->HASSUBDIRS)
		{
			/* Now, temporarily unlink the sub-directory chain of 'dir' */
			LastSubDir = FirstSubDir = dir->x.d.dnext ;

			assert(FirstSubDir != NULL) ;
			
			while (LastSubDir->x.d.dnext)
			{
				if (LastSubDir->x.d.dnext->dtlx.lx.level > dir->dtlx.lx.level)
					LastSubDir = LastSubDir->x.d.dnext ;
				else
					break ;
			}

			assert(LastSubDir != NULL) ;
			
			/* Now, FirstSubDir and LastSubDir are the first and last sub-dirs
			 * of 'dir' in the chain.
			 */

			prev_dfs_dir = PrevDFSDir(tree, FirstSubDir) ;

			/* Unlink the sub-directory chain form the big chain */
			if (prev_dfs_dir)
				prev_dfs_dir->x.d.dnext = LastSubDir->x.d.dnext ;
			else
				tree->FirstDirectory = LastSubDir->x.d.dnext ;
		}

		/* Save the old sort order, as we want to always sort directories
		 * in ascending order.
		 */
		TempDescendingOrder = gDescendingOrder ;
		gDescendingOrder = 0 ; // for ascending order. This filed is looked
									  // at by the sort routines we invoke now.

		/* Actual insertion of 'dir' needs to be done here! */
		if ( (dir->dtlx.lx.level  == 1) && 
							((SortFnArr[SORT_NAME])(dir, tree->FirstDirectory)< 1))
		{
			dir->x.d.dnext = tree->FirstDirectory ;
			tree->FirstDirectory = dir ;

			/* Link the sub-directory chain of 'dir' back into global chain */
			if (dir->HASSUBDIRS)
			{
				LastSubDir->x.d.dnext = dir->x.d.dnext ;
				dir->x.d.dnext = FirstSubDir ;
			}
			gDescendingOrder = TempDescendingOrder ; // restore sort order
			return ;
		}

		/* If we get here, the new 'dir' to be added to the list will not
		 * affect tree->FirstDirectory (i.e., it will not be the first dir 
		 * in the directory chain). Now 'after' is guaranteed to be non-null.
		 */
		after = (parent) ? parent : tree->FirstDirectory ;

		temp = after->x.d.dnext ;

		while(TRUE)
		{
			if (!temp)
				break ;
			
			if (dir->dtlx.lx.level == temp->dtlx.lx.level)
			{
				if ((SortFnArr[SORT_NAME])(dir, temp) < 1)
					break ;
			}
			else if (dir->dtlx.lx.level > temp->dtlx.lx.level)
				break ;

			/* the '<' case can't happen for level, as we don't insert a dir's
			 * subdirectory before inserting it first!!
			 */

			after = temp ;
			temp = temp->x.d.dnext ;
			
		} /* while */

		dir->x.d.dnext = after->x.d.dnext ;
		after->x.d.dnext = dir ;

		/* Link the sub-directory chain of 'dir' back into global chain */
		if (dir->HASSUBDIRS)
		{
			LastSubDir->x.d.dnext = dir->x.d.dnext ;
			dir->x.d.dnext = FirstSubDir ;
		}

		gDescendingOrder = TempDescendingOrder ; // restore sort order
	} /* else */
} /* InsertDir */


/* ZZZZZZZZZZZZZZZZZZZZZZZZZZ Old code -- disk order insertion. */
#if 0
/* ZZZZZ Old code -- this fn action actually does not need 'parent'!! 
 * It just inserts at the end -- DiskOrder!
 */
void InsertDir(PTREE tree, PENTRY parent, PENTRY dir)
{
	if (tree->FirstDirectory == NULL)
		/* directory head needs initialization */
	    tree->FirstDirectory = dir;
	else
	    tree->LastDirectory->x.d.dnext = dir;

	/* ZZZ caller makes this assignment, so we need not do this! */
	// tree->LastDirectory = dir;

} /* InsertDir */
#endif
