;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****	file snext.c: routines to add/remove 'snext' linkages of files.
**  The operations done are not trivial, as the snext chain behaves differently
**  in different modes. In any case, all files belonging to a tree form one
**  singly linked huge chain. In System Tree Mode, they are linked so that
**  all files are in the appropriate sort order (NAME, DATE, SIZE, etc). In
**  Single/Double Tree Mode, all files belonging to each directory are in the
**  appropriate sort order. The global 'snext' chain is actually a linkage
**  of these smaller 'snext' chains.
**
**  Global snext Chain = dir1 chain-->dir2 chain-->...-->dirn chain-->NULL.
**		where dir1, dir2, ..., dirn are directories in the order shown in the
**	    directory list box.
**
**	Note that DISK order is the mode in which we have the same snext chain
**  in all three tree modes!
**
**	We don't have the luxury of a lot of memory, so we store the bare minimum
**	information along with each node. So when we change tree modes, info
**  like 'snext' may have to be recalculated!!
**
**	A fairly MODULAR approach has been taken towards the snext manipulating
**	routines, so I guess there should be quite some scope for improvement
**	in speed, reducing code size.
/*
**    Date	Name	Modification
**  --------  --------	---------------------------------------------------
**   9/30/89  harikris	wrote all the snext routines.
**	11/??/89  harikris	fixed bugs!
*/

#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>

// Uncomment out this #define if you want consistency checks turned on.
// #define DEBUG 1  

/* Used in fn DoFileOp() to traverse snext chain to perform fileop! */
extern PENTRY gNextFileNode ;


extern PENTRY GetPrevsnextFile(PTREE tree, PENTRY fil) ;
extern PENTRY GetLastsnextFile(PTREE tree) ;

/****	Updatesnext - Functions links 'fil' in the 'snext' chain appropriately.
**
**	ENTRY
**			tree   - tree to which 'fil' belongs
**			parent - parent directory of 'fil'
**			fil    - the file node to be added to the snext chain.
**
**	EXIT
**			'fil' linked up correctly, 'parent' has 'FIRSTFILEFOUND' bit
**			set correctly, 'fil' has 'FIRSTDIRFILE', 'LASTDIRFILE' bits set
**			correctly.
*/
void Updatesnext(PTREE tree, PENTRY parent, PENTRY fil, BOOL fDoSortedInsertion)
{
	/* This routine is not called if we can do SystemSort!! - Merge sort!! */

	/* At start up fil->FIRSTDIRFILE = fil->LASTDIRFILE = FALSE ; */

#ifdef DEBUG
	/* ZZZZZZZZ remove before shipping!!! */
	if ( tree->LastFile && (tree->LastFile->x.f.snext) )
	{
		ShellMessageBox("Non-null LastFile", "Last file non-null\nIn fn Updatesnext") ;
	}
#endif
	if (!parent)
	{
		/* File is part of the root directory, as parent == NULL! 		*/
		/* Now check to see if 'fil' is the first file of the root dir	*/
		/* encountered so far.											*/
		if ( (!tree->FirstFile) || (FindParent(tree->FirstFile)) )
		{
			fil->x.f.snext = tree->FirstFile ;
			tree->FirstFile = fil ;
			fil->FIRSTDIRFILE = fil->LASTDIRFILE = TRUE ;
		}
		else
			/* if we are here => fil is not the first root file */
			snextRootNotFirstUpdate(tree, fil) ;
	}
	else
	{
		/* if we are here => the file is part of some sub-directory */

		/* If asked to do Sorted Insertion or if the directory is already in
		 * sorted order do sorted insertion.
		 */
		if ( (fDoSortedInsertion) || (parent->DIRSORTED) )
		{
			if (!parent->FIRSTFILEFOUND)
				/* 'fil' is the first file encountered of 'parent' so far */
				snextDirFirstUpdate(tree, parent, fil) ;
			else
				/* 'fil' is not the first file of the directory */
				snextDirNotFirstUpdate(tree, parent, fil) ;
		}
		else
		{
			parent->DIRSORTED = FALSE ;

			/* Add this file 'fil' as the last file in the 'snext' chain */
			if (!tree->FirstFile)
			{
				tree->FirstFile = fil ;
			}
			else
			{
				/* Just in case tree->LastFile has not been updated right.
				 * we calculate it again here.
				 */
				if (tree->LastFile->x.f.snext != NULL)
					tree->LastFile = GetLastsnextFile(tree) ;

				(tree->LastFile)->x.f.snext = fil ;
			}

			fil->x.f.snext = NULL ;
		}
	}

	/* Update the field tree->LastFile correctly */
	if (fil->x.f.snext == NULL)
		tree->LastFile = fil ;

} /* fn Updatesnext */

/*** snextRootNotFirstUpdate -- Handles the snext linking when 'fil' belongs
*	to the root directory. It is guaranteed that we have already encountered
*	a file belonging to the root directory. That is, 'fil' is not the first
*	file of root in Disk Order.
***/
void snextRootNotFirstUpdate(PTREE tree, PENTRY fil)
{
	PENTRY prev, curr ;

	prev = tree->FirstFile ;

	/* We know prev is NOT NULL now!! */
	curr = prev->x.f.snext ;

	if (glob.SortKey == SORT_DISK)
	{
		/* go to last file at root level -- All files at root level have
		 * parent == NULL.
		*/
		while (curr && !FindParent(curr)) 
		{
			prev = curr ;
			curr = curr->x.f.snext ;
		}
		prev->LASTDIRFILE = FALSE ;
		fil->LASTDIRFILE = TRUE ;
		fil->x.f.snext = prev->x.f.snext ;
		prev->x.f.snext = fil ;
	} /* SORT_DISK order */
	else 
		/* Handle Non-Disk Order!! */
		InsertNotRootFirst(tree, fil) ;
} /* snextRootNotFirstUpdate */

/*** snextDirFirstUpdate -- Handles the snext linking when 'fil' belongs
*	to a non-root directory (a sub-directory). 'fil' is guaranteed to be
*	the first file of its parent directory 'parent' in Disk Order.
***/
void snextDirFirstUpdate(PTREE tree, PENTRY parent, PENTRY fil)
{
	PENTRY after, prev ;

	parent->FIRSTFILEFOUND = fil->FIRSTDIRFILE = fil->LASTDIRFILE = TRUE ;

	if (!tree->FirstFile)
	{
		tree->FirstFile = fil ;
		fil->x.f.snext = NULL ;
		return ;
	}

	if (glob.TreeMode == TR_SYSTEM)
	{
		/* It is only in DISK_ORDER in SYSTEM tree mode that this code path
		 * will get executed.
		 */

		/* there is atleast 1 file already found in the file system */
		after = parent ;

		/* If we wanted Breadth First Display use fn GetLeftBrother instead 
		 * of fn PrevDFSDir.
		 */
		/* Now, get the directory which has atleast 1 file in it - after whose 
		 * files we need to append this file.
		 */
		while ((after = PrevDFSDir(tree, after)) && 
										!(after->FIRSTFILEFOUND))
			/* Do nothing -- the repeated fn call does the job */ ;

		if (!after)
			/* The root directory is the left brother with files in it */
			prev = GetLastRootFile(tree) ;
		else
			prev = GetLastDirFile(after) ;

	}
	else
		/* Files belonging to root directory are always in sorted order!! */
		prev = GetLastRootFile(tree) ;

	if(!prev)
	{
	    /* This is the new first file in the tree */
	    fil->x.f.snext = tree->FirstFile ;
	    tree->FirstFile = fil;
	}
	else
	{
	    fil->x.f.snext = prev->x.f.snext ;
	    prev->x.f.snext = fil ;
	}
} /* snextDirFirstUpdate */

/*** snextDirNotFirstUpdate -- Handles the snext linking when 'fil' belongs
*	to a non-root directory (a sub-directory). 'fil' is guaranteed NOT to be
*	the first file of its parent directory 'parent' in Disk Order.
***/
void snextDirNotFirstUpdate(PTREE tree, PENTRY parent, PENTRY fil)
{
	PENTRY prev ;

	if (glob.SortKey == SORT_DISK)
	{
		prev = GetLastDirFile(parent) ;
		prev->LASTDIRFILE = FALSE ;
		fil->LASTDIRFILE = TRUE ;
		fil->x.f.snext = prev->x.f.snext ;
		prev->x.f.snext = fil ;
	}
	else
		/* Handle Non-Disk Order */
		InsertNotDirFirst(tree, parent, fil) ;
} /* snextDirNotFirstUpdate */

/*** PrevDFSDir -- Given a directory 'dir' belonging to tree 'tree', it 
*	returns the directory that is the predecessor of 'dir' in DFS order
*	the first file of its parent directory 'parent' in Disk Order -- That
*	is, the directory that will be just above 'dir' in the tree list box!
***/
PENTRY PrevDFSDir(PTREE tree, PENTRY dir)
{
	PENTRY temp ;

	if (dir == tree->FirstDirectory)
		return (NULL) ; /* root is the Prev directory of the first directory */

	for (temp = tree->FirstDirectory ; temp ; temp = temp->x.d.dnext )
		if (temp->x.d.dnext == dir)
			return temp ;

#ifndef NOCONSISTENCY
	/* Actually, if the directory node belongs to this tree, it should
		not execute the following statement */
	printf("*** PrevDFSDir -- directory doesn't belong to this tree\n") ;
	exit(0) ;
#endif
} /* PrevDFSDir */


/* Function returns as its value the prev sibling directory of dir, the
*   prev_dfs directory is passed back by reference
*/
/* ZZZ If we wanted to reduce code size we could have only this function
*  and remove fn PrevDFSDir but pay the price of finding both even though
*  we want only the prev_dfs_dir!!
*/
/* Warning! 'dir' is expected to be a directory in tree other than the ROOT */

PENTRY PrevSibAndDFSDir(PTREE tree, PENTRY dir, PENTRY *prev_dfs_dir)
{
	PENTRY prev_sib_dir ;

	*prev_dfs_dir = NULL ;

	/* ZZZZ this function can be re-structured to have just 1 return */
	if (dir == tree->FirstDirectory)
		return (NULL) ; /* No prev_sib_dir for first directory */

	prev_sib_dir = NULL ;

	for (*prev_dfs_dir = tree->FirstDirectory ; *prev_dfs_dir ;
				 *prev_dfs_dir = (*prev_dfs_dir)->x.d.dnext )
	{

		/* Note: ((*prev_dfs_dir) != dir) will hold in this loop!! */
		if ((*prev_dfs_dir)->dtlx.lx.level == dir->dtlx.lx.level)
		    prev_sib_dir = *prev_dfs_dir ;

		if ((*prev_dfs_dir)->x.d.dnext == dir)
			return prev_sib_dir ;
	 }
#ifndef NOCONSISTENCY
	/* Actually, if the directory node belongs to this tree, it should
		not execute the following statement */
	printf("*** PrevDFSDir -- directory doesn't belong to this tree\n") ;
	exit(0) ;
#endif
} /* PrevDFSDir */

PENTRY HandleDeletednext(PTREE tree, PENTRY deldir)
{
	PENTRY prev_sib_dir, prev_dfs_dir ;

    /* ZZZZ We have the routine to find PrevDFS dir, but we need
       to know both the prev_dfs and prev_sib directories here.
       Both these can be done in 1 pass, so I have a new routine */
    prev_sib_dir = PrevSibAndDFSDir(tree, deldir, &prev_dfs_dir) ;

    if (prev_sib_dir)
    {
		prev_sib_dir->LASTDIR = deldir->LASTDIR ;
    }
    if (prev_dfs_dir)
    {
		prev_dfs_dir->x.d.dnext = deldir->x.d.dnext ;
    }
    else
    {
		/* root was it's prev DFS directory */
		tree->FirstDirectory = deldir->x.d.dnext ;
    }

	 return prev_dfs_dir ;

} /* HandleDeletednext */

/*** GetLastDirFile -- Given a directory 'dir' (not the root), it returns
*	the last file belonging to that directory in appropriate Sort Order.
***/
PENTRY GetLastDirFile(PENTRY dir)
{
	PENTRY temp ;

	if (!dir->FIRSTFILEFOUND)
		return NULL ;

	/* We know that there is at least 1 file, so temp can't be NULL at all */
	for (temp = dir->x.d.child ; 
			(temp->attribs & _A_SUBDIR) || (!temp->LASTDIRFILE) ;
				temp = temp->sibling )
		/* Do nothing */ ;

	return temp ;
} /* GetLastDirFile */

/*** GetFirstDirFile -- Given a directory 'dir' (not the root), it returns
*	the first file belonging to that directory in appropriate Sort Order.
*	Note that there is very little difference between this fn & prev one!
***/
PENTRY GetFirstDirFile(PENTRY dir)
{
	PENTRY temp ;

	if (!dir->FIRSTFILEFOUND)
		return NULL ;

	/* We know that there is at least 1 file, so temp can't be NULL at all */
	for (temp = dir->x.d.child ; 
			(temp->attribs & _A_SUBDIR) || (!temp->FIRSTDIRFILE) ;
				temp = temp->sibling )
		/* Do nothing */ ;

	return temp ;
} /* GetFirstDirFile */

/*** GetLastRootFile --   Get the last file belonging to the root directory
* 	of 'tree'.
***/
PENTRY GetLastRootFile(PTREE tree)
{
	PENTRY temp ;

	/* Files belonging to root have NULL as their parent */
	if ( (!tree->FirstFile) || FindParent(tree->FirstFile) )
		return NULL ;

	for (temp = tree->FirstFile ; !(temp->LASTDIRFILE) ; temp = temp->x.f.snext)
		/* Do Nothing */ ;

	return temp ;
} /* GetLastRootFile */

/*** GetFirstRootFile --   Get the first file belonging to the root directory
* 	of 'tree'.
***/
PENTRY GetFirstRootFile(PTREE tree)
{
	return (FindParent(tree->FirstFile) ? NULL : tree->FirstFile) ;
}

/*** InsertSystemOrder -- Inserts file 'fil' in the chain in appropriate
*	location based on Sort Order. The tree is in System Tree Mode.	
* The fields FIRSTFILEFOUND, FIRSTDIRFILE, LASTDIRFILE are meaningless in 
*	system mode display -- So they are not updated in this function.	
* 
***/
void InsertSystemOrder(PTREE tree, PENTRY fil)
{
	PENTRY prev, curr ;

	if (!tree->FirstFile)
	{
		tree->FirstFile = fil ;
		fil->x.f.snext = NULL ;
		return ;
	}

#ifndef NOCONSISTENCY
	if (glob.SortKey == SORT_DISK)
		 /* This fn should not be called for Disk Sorting order -- that should
			be done by FormDirectoryOrder and then normal Updatesnext */
	{
		printf("*** BUG! InsertSystemOrder called with SORT_DISK\n") ;
		exit(0) ;
	}
#endif

	prev = NULL ;
	curr = tree->FirstFile ;

	while (curr && ((*SortCmp)(fil, curr) > 0))
	{
		/* 'fil' should not be before 'curr'in the system list */
		prev = curr ;
		curr = curr->x.f.snext ;
	}

	if (!prev)
	{
		/* this file should be the new tree->FirstFile as 
			it is <= tree->FirstFile */
		fil->x.f.snext = tree->FirstFile ;
		tree->FirstFile = fil ;
	}
	else
	{
		/* prev != Null and file goes after prev in the list */
		fil->x.f.snext = prev->x.f.snext ;
		prev->x.f.snext = fil ;
	}

	/* Update this field correctly */
	if (fil->x.f.snext == NULL)
		tree->LastFile = fil ;
} /* InsertSystemOrder */

/*** InsertNotRootFirst -- Inserts file 'fil' in the chain in appropriate
*	location based on Sort Order. The tree is in NON-System Tree Mode.	
*	'fil' is known to be part of the ROOT directory and is NOT its first
*	file in Disk Order.
***/
void InsertNotRootFirst( PTREE tree, PENTRY fil)
{
	PENTRY prev ;

	prev = tree->FirstFile ;

	if ((*SortCmp)(fil, prev) < 1)
	{
		/* 'fil' is now tree's new first file as it goes before 'prev' */
		fil->FIRSTDIRFILE = TRUE ;
		prev->FIRSTDIRFILE = FALSE ;
		fil->x.f.snext = prev ;
		tree->FirstFile = fil ;
	}
	else
		InsertAfter(fil, prev) ;
} /* InsertNotRootFirst */

/*** InsertNotDirFirst -- Inserts file 'fil' in the chain in appropriate
*	location based on Sort Order. The tree is in NON-System Tree Mode.	
*	'fil' is known to be part of a NON-ROOT directory and is NOT its first
*	file in Disk Order.
***/
void InsertNotDirFirst(PTREE tree, PENTRY parent, PENTRY fil)
{
	PENTRY firstparentfile ;
	PENTRY prevfile ;

	firstparentfile = GetFirstDirFile(parent) ;

	/* Note that "firstparentfile" is guaranteed to be non-NULL as 'fil'
	 *	is not the first file to be added to directory 'parent'
	 */
#ifdef DEBUG
	/* ZZZZZZZZZ Delete before shipping */
	/* ZZZZZZZZ remove before shipping!!! */
	if ( !firstparentfile )
	{
		ShellMessageBox("No file in parent dir", "to insert in front of.\nIn fn InsertNotDirFirst") ;
	}
#endif
	/* Is 'fil' to be added, to be inserted before the first file currently
	 * in 'dir'?
	 */
	if ((*SortCmp)(fil, firstparentfile) < 1)
	{
		fil->FIRSTDIRFILE = TRUE ;
		firstparentfile->FIRSTDIRFILE = FALSE ;

		prevfile = GetPrevsnextFile(tree, firstparentfile) ;
		if (!prevfile)
		{
			tree->FirstFile = fil ;
			fil->x.f.snext = firstparentfile ;
		}
		else
		{
			prevfile->x.f.snext = fil ;
			fil->x.f.snext = firstparentfile ;
		}
	}
	else
		/*  'fil' does not go before 'firstparentfile'. */
		InsertAfter(fil, firstparentfile) ;
} /* InsertNotDirFirst */

/*** InsertAfter -- Inserts file 'fil' in the chain in appropriate
*	location based on Sort Order. It doesn't matter if tree is in System Tree
*	mode or not. It is guaranteed that 'prev' <= 'fil' based on the sort order
*	and so 'fil' goes AFTER (not necessarily immediately after) 'prev' in
*	the 'snext' chain.
***/
void InsertAfter(PENTRY fil, PENTRY prev)
{
	struct fi_t far *thenext;
	/* loop until we find the file ('prev') after which 'fil' goes */

	while (!prev->LASTDIRFILE)
	{
		if ((*SortCmp)(fil, thenext=prev->x.f.snext) > 0)
				prev = thenext;
		else
			break ;
	}
	if (prev->LASTDIRFILE)
	{
		prev->LASTDIRFILE = FALSE ;
		fil->LASTDIRFILE = TRUE ;
	}
	fil->x.f.snext = prev->x.f.snext ;
	prev->x.f.snext = fil ;
} /* InsertAfter */

/*** UpdateLatesnext -- Routine called when a new file is added by a fileop.
*	Similar to Updatesnext for non-disk order, system order. For disk
*	order, 'prev' is the file before 'fil' in 'parent' directory. If 'prev'
*	is NULL, 'fil' is the first file in this directory. This routine will be
*	called during file opearations like COPY, etc.
***/
void UpdateLatesnext(PTREE tree, PENTRY parent, PENTRY fil, PENTRY prev,
																BOOL fDoSortedInsertion)
{
#ifdef DEBUG
	/* ZZZZZZZZ remove before shipping!!! */
	if ( tree->LastFile && (tree->LastFile->x.f.snext) )
	{
		ShellMessageBox("Non-null LastFile", "Last file non-null\nIn fn UpdateLatesnext") ;
	}
#endif
	fil->FIRSTDIRFILE = fil->LASTDIRFILE = FALSE ; /* default state */

	fil->DELMARKER = FALSE ; /* default state */

	if ( (glob.TreeMode == TR_SYSTEM) && (glob.SortKey != SORT_DISK) )
		InsertSystemOrder(tree, fil) ;
	else
	{
		/* Files have to be in directory order -- Files within each
			directory are to be sorted by appropriate SortKey */
		if (glob.SortKey != SORT_DISK)
		{
			/* Only DISK_ORDER can screw us up, as this file could be in
				any random order in the disk in this directory, else
				old insertions are fine. */
			Updatesnext(tree, parent, fil, fDoSortedInsertion) ;
		}
		else
		{
			/* Handle complex case of Inserting in disk order */
			if (prev)
			{
				/* there is a file in this directory which occurs before
					'fil' in disk order -- easy case to handle */
				fil->x.f.snext = prev->x.f.snext ;
				prev->x.f.snext = fil ;
				if (prev->LASTDIRFILE)
				{
					prev->LASTDIRFILE = FALSE ;
					fil->LASTDIRFILE = TRUE ;
				}
			}
			else
			{
				/* (prev == NULL) -- complex case */
				if (!parent)
				{
					/* This implies that this is a file in the root directory */
					/* Hence, it is the first file on the disk */
					fil->x.f.snext = tree->FirstFile ;
					tree->FirstFile = fil ;
				} /* !parent -- i.e, case when 'fil' is part of root */
				else
					/* file is part of a non-root directory */
					snextDirFirstUpdate(tree, parent, fil) ;

				/* This is the first file in Disk Order in the directory */
				fil->FIRSTDIRFILE = TRUE ;

				/* If there is no file after 'fil' or if file following it	*/
				/*	is not a part of parent, 'fil' is LASTDIRFILE else		*/
				/*	not so. Reset the subsequent file's first dir file		*/
				/*	if it belongs to parent.								*/

				if (!fil->x.f.snext) 
					fil->LASTDIRFILE = TRUE ;
				else
				{
					if ( FindParent(fil->x.f.snext) != parent )
						fil->LASTDIRFILE = TRUE ;
					else
					{
						/* We need to set fil->LASTDIRFILE to false, because
						   snextDirFirstUpdate fn call above, as a side-effect
						   sets the LASTDIRFILE bit to TRUE always */
						fil->LASTDIRFILE = FALSE ;

						fil->x.f.snext->FIRSTDIRFILE = FALSE ;
					}
				}
			} /* complex case -- prev == NULL */
		} /* SortKey == SORT_DISK handled */
	} /* Non System Mode or DISK_ORDER updates */

	/* Update this field right! */
	if ( (!tree->LastFile) || (tree->LastFile->x.f.snext != NULL) )
		tree->LastFile = GetLastsnextFile(tree) ;

} /* proc UpdateLatesnext */

/* node is the file node to be deleted, 'parent' is its parent directory
	and it belongs to tree 'tree'.	*/
void HandleDeletesnext(PTREE tree, PENTRY parent, PENTRY node)
{
	PENTRY prev ;
	BOOL fLastFileBeingDeleted ;
	BOOL fNextFileNodeBeingDeleted ;

#ifdef DEBUG
	/* ZZZZZZZZ remove before shipping!!! */
	if ( tree->LastFile && (tree->LastFile->x.f.snext) )
	{
		ShellMessageBox("Non-null LastFile", "Last file non-null\nIn fn HandleDeletesnext") ;
	}
#endif
	fNextFileNodeBeingDeleted = (node == gNextFileNode) ;
	fLastFileBeingDeleted = (node->x.f.snext == NULL) ;

	prev = tree->FirstFile;
	if (prev == node)
	{
		/* We are deleting the first file of the tree -- easy case! */
		tree->FirstFile = node->x.f.snext;
	} else
	{
		while (prev->x.f.snext != node)
			prev = prev->x.f.snext;
		prev->x.f.snext = node->x.f.snext;
	}

	if (fLastFileBeingDeleted)
	{
		if (!tree->FirstFile)
			tree->LastFile = NULL ;
		else
			tree->LastFile = prev ;
	}

	/* If "gNextFileNode" has just been deleted, we need to update it
	 * appropriately as this is the next node that fn DoFileOp() will use
	 * to perform file operation on next.
	 */
	if (fNextFileNodeBeingDeleted)
	{
		gNextFileNode = node->x.f.snext ;
	}

	/* If treemode is the system mode, we need to only update the
		snext linked list which has just been done. So return */
	if (glob.TreeMode == TR_SYSTEM)
		return ;

#ifdef DEBUG
	/* ZZZZZZZ Remove this assertion before shipping */
if (!parent && tree->SortRequired)
{
	ShellMessageBox("FM CONFUSED!!!", "Please exit FM right away\nReport this message to harikris\nROOT dir un-sorted!!") ;
	DoExit() ;
}
#endif

	/* If "parent" is not sorted, we don't have to update any special bits
	 * These bits will be taken care of when this directory is sorted.
	 */
	if (parent && (!parent->DIRSORTED))
	{
		return ;
	}

	/* Note that ROOT (parent == NULL) is always maintained in sorted order */

	/* In non-system tree mode, the 3 bits FIRSTFILEFOUND, FIRSTDIRFILE,
		LASTDIRFILE need to set appropriately */
	/* fix the 'FIRSTFILEFOUND', 'FIRSTDIRFILE', 'LASTDIRFILE' */
	if (node->LASTDIRFILE) 
	{
		if ( (node->FIRSTDIRFILE) && parent )
		{
			/* node is the only file of 'parent' and is being deleted*/
			parent->FIRSTFILEFOUND = FALSE ;
		}
		else
		{
				/* 'node' has a prev file in the same directory as itself. Since
				node was the lastfile in parent, 'prev' takes its place now. */
				prev->LASTDIRFILE = TRUE ;
		}
	}
	else /* node is not the lastfile in 'parent' -- implies there is a file
		after it in 'parent' -- implies node->x.f.snext != NULL */
	{
		if ( node->FIRSTDIRFILE )
			node->x.f.snext->FIRSTDIRFILE = TRUE ;
	}

} /* proc HandleDeletesnext */

#define MAXNESTING 32  /* Max directory nesting */

/* MarkLastDir --															*/
/* Function marks the LASTDIR bit for all dir nodes belonging to tree 		*/
/* This bit is used to draw the tree list box correctly!! I personally like */
/* this algorithm very much -- it is linear in time (on number of dirs)		*/
/* and simulates the kind of recursion needed, using a local stack!			*/
void Marklastdir(PTREE tree)
{
	int stacktop ; 
	PENTRY node ;
	fileinfo dummy ;  /* dummy fileinfo node */
	PENTRY DStack[MAXNESTING+1] ; /* 0 -- used for root. Directory node stack */

	dummy.dtlx.lx.level = 0 ; /* level is the only field we are interested in */
	/* Initialize stack with a dummy node for root dir */
	DStack[stacktop = 0] = &dummy ; 
	
	/* Claim: Following algorithm marks all directory nodes correctly.
	*  Main reason: Every directory node gets on the stack once and any
	*				entry in stack is marked.
	*/
	for ( node = tree->FirstDirectory ; node ; node = node->x.d.dnext )
	{
		if ( node->dtlx.lx.level > DStack[stacktop]->dtlx.lx.level )
		{
			DStack[++stacktop] = node ;  /* push onto stack */
		}
		else
		if ( node->dtlx.lx.level == DStack[stacktop]->dtlx.lx.level )
		{
			/* the replaced node is not the LASTDIR  node */
			DStack[stacktop]->LASTDIR = FALSE ;
			DStack[stacktop] = node ; /* replace top of stack */
		}
		else
		{
			/* we have to mark off all nodes in stack with level > node's level
			   as being last dir nodes. Note that there has to be a node on
			   the stack which has the same level as node. Node should
			   replace that node. It's true -- think about it!!*/
			while (DStack[stacktop]->dtlx.lx.level > node->dtlx.lx.level)
			{
				DStack[stacktop--]->LASTDIR = TRUE ;
			}
			DStack[stacktop]->LASTDIR = FALSE ;
			DStack[stacktop] = node ; /* replace current stacktop */
		}
	} /* for loop */

	/* Now all nodes on top of stack are LASTDIR nodes */
	while (stacktop)
	{
		DStack[stacktop--]->LASTDIR = TRUE ;
	}

} /* Marklastdir */

/* Inserts directory 'newdir' in sorted order in 'dnext' chain as a sub-dir
 * of parent. It also updates all the LASTDIR bits of the sub directories
 * of 'parent' so that they are displayed right on the screen.
 */
void InsertDirAndMarkLastDirBits(PTREE tree, PENTRY parent, PENTRY newdir)
{
	PENTRY tempdir, nextdir ;

	InsertDir(tree, parent, newdir) ;

	tempdir = (!parent) ? tree->FirstDirectory : parent->x.d.dnext ;

	/* Note that we should always be able to find 'newdir' in the chain now */
	while (tempdir != newdir)
	{
#ifdef DEBUG
		/* ZZZZZ Safety feature -- Delete before shipping! */
		if (!tempdir)
		{
			ShellMessageBox("InsertDirAndMarkLastDirBits", "dnext chain broken!!\nnewly created dir not found!\nIn fn CreateDirectory") ;
			break ;
		}
#endif
		if (tempdir->dtlx.lx.level == newdir->dtlx.lx.level)
			tempdir->LASTDIR = FALSE ;
		
		tempdir = tempdir->x.d.dnext ;
	}

	nextdir = newdir->x.d.dnext ;
	/* Is there no directory after 'newdir' or is that directory not
	 * part of 'parent', i.e., is it at a different level. In that
	 * case 'newdir' is the LASTDIR of its parent when being displayed.
	 */
	newdir->LASTDIR = (!nextdir) ||
								(nextdir->dtlx.lx.level != newdir->dtlx.lx.level) ;
		
} /* InsertDirAndMarkLAstDirBits */

#ifdef DONT_DO_ANY_INSERTION_SORT
/*** HasNoSubDir -- Returns a BOOL as to whether the directory 'parent' has
*	any sub directories.
*/
BOOL HasNoSubDir(PTREE tree, PENTRY parent)
{
    PENTRY temp ;

    if (!parent)
    {
	return (tree->FirstDirectory ? FALSE : TRUE ) ;
    }

    /* => parent is not the root directory */
    for (temp = parent->x.d.dnext ; temp ; temp = temp->x.d.dnext)
    {

	if (temp->dtlx.lx.level == parent->dtlx.lx.level+1)
	    /* sub-dir of parent found as its level is one more */
	    return FALSE ;

	if (temp->dtlx.lx.level == parent->dtlx.lx.level)
	     /* we have seen a sibling directory without seeing a sub-dir */
	    break ; /* => no sub-dir */
    }

    return TRUE ; /* No sub-dirs in parent */
} /* HasNoSubDir */

#endif
/* This function retrieves the file that is the TAIL of the 'snext' chain */
PENTRY GetLastsnextFile(PTREE tree)
{
	PENTRY fil;

	if ((fil = tree->FirstFile) == NULL)
		return NULL ;

	/* fil is guaranteed to be non-null now! */
	while(fil->x.f.snext != NULL)
	{
		fil = fil->x.f.snext ;
	}

	return fil ;

} /* GetLastsnextFile */


/* This function returns the file node that is just in front of "fil" in
 * the 'snext' chain.
 */
PENTRY GetPrevsnextFile(PTREE tree, PENTRY fil)
{
	PENTRY temp ;

	if ( (!(temp = tree->FirstFile)) || (temp == fil) )
		return NULL ;

	/* 'temp' is guaranteed to be non-NULL when we do this check! */
	while (temp->x.f.snext != fil)
	{
		temp = temp->x.f.snext ;
		if (!temp)
			break ;
	}

	return temp ;

} /* GetPrevsnextFile */

/* This function deletes all files belonging to directory 'dir' from the
 * snext chain.
 */
void DeleteFilesFromsnextChain(PTREE tree, PENTRY dir)
{
	PENTRY node ;
	PENTRY prev, fil ;

	/* First mark all files belonging to 'dir' as deleted */
	node = (dir) ? dir->x.d.child : tree->head;
	while(node)
	{
		if (!(node->attribs & _A_SUBDIR))
			node->DELMARKER = TRUE ;

		node = (node->nosib) ? NULL : node->sibling ;
	} /* while */

	/* Now delete all marked files from snext chain -- can be done in 1 pass */
	fil = tree->FirstFile ;
	prev = NULL ; 
	while (fil)
	{
		if (fil->DELMARKER)
		{
			// clear the foll. bit and leave in default state
			fil->DELMARKER = FALSE ; 

			if (prev)
				prev->x.f.snext = fil->x.f.snext;
			else
				/* tree->FirstFile == fil */
				tree->FirstFile = fil->x.f.snext ;
		}
		else
			prev = (prev == NULL) ? tree->FirstFile : prev->x.f.snext ;

		fil = (prev == NULL) ? tree->FirstFile : prev->x.f.snext ;
	} /* while */

	/* Mark 'dir' as having no files */
	if (dir)
		dir->FIRSTFILEFOUND = FALSE ;

	tree->LastFile = GetLastsnextFile(tree) ;

} /* DeleteFilesFromsnextChain */


#if 0
/* fn is not used anymore! -- Could be pretty useful!! */
PENTRY GetLeftBrother(PTREE tree, PENTRY dir)
{
	PENTRY lbr, temp ;

	lbr = NULL ;

	for (temp = tree->FirstDirectory ; temp && (temp != dir) ;
											temp = temp->x.d.dnext ) 
		if (temp->dtlx.lx.level <= dir->dtlx.lx.level)
					lbr = temp ;

	return lbr ;

} /* GetLeftBrother */
#endif
