;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****	file add.c: routines to find files and add them to the tree.
**	    AddFile is called to locate the next file on a drive and add it
**	to the tree.  One file is found and attached to the tree in each call;
**	when there are no more files the fn returns FALSE.
**	GetNextFile is called only by AddFile. AddLateFile is called to add a
**  file to a tree that has been already built. It is called when fileops
**	create new files. DelFileEntry is called to delete a dir/file entry
**	from the tree structure. It is called when fileops delete entries.
**  It would be great to have the tightest possible code for functions in
**  this file, as they are used extensively.
**
**    Date	Name	Modification
**  --------  --------	---------------------------------------------------
**   7/12/89  t-jeffro	zeros all unused struct elts
**   7/13/89  t-jeffro	AddFile stable again.
**   7/27/89  t-jeffro	Knows about _A_NOSIB.
**   8/14/89  t-jeffro	Added DelFileEntry
**   10/18/89 harikris	rewrote almost whole of fn. AddLateFile
**   10/??/89 harikris	Modified DelFileEntry drastically to handle snext, etc.
**   11/03/89 harikris	DelFileEntry now doesn't update dnext's.
*/

#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>
#include <assert.h>

extern char szStarDotStar[] ;
extern MarkMatchesBit(PENTRY fil) ;

/* ZZZZZZZ */
/*  The assembly routines name_cmp, ext_cmp return -1, 0, or 1 for less than,
	equal to, and greater than respectively. But in our case we are interested
	only in > and <= -- so a BOOL would have been sufficient. But the current
	method is useful if we are later going to use a secondary key to sort
	in case the primary key doesn't distinguish the entries.				*/

/* Array of pointers to sort functions */
int (*SortFnArr[]) (PENTRY, PENTRY) = { quick_name_cmp, quick_ext_cmp, date_cmp,
#ifdef NDEBUG
/* There is no fn like disk_cmp -- we use different algorithm for disk order.
 * For safety, we have assigned the date_cmp routine for it.
 */
										size_cmp, date_cmp } ;
#else
										size_cmp, disk_cmp } ;
#endif

int (*SortCmp)(PENTRY, PENTRY) ; /* pointer to function that does the
									 compare based on the selected sort key */

/****	AddFile - Get next file in tree and add to tree
**		This fn is called in a loop from the application.  One file is 
**	found and attached to the tree in each call; when there are no more 
**	files the fn returns FALSE.
**
**	ENTRY
**		tree - file tree to append to
**	EXIT
**		address of record added; NULL if no more files.
**	WARNING:
**		tree->root must be initialized with something like "C:\"
**	before this fn is called.  If the tree is empty, its pointers should
**	be NULL.
**	EFFECTS:
**	    Updates tree, modifies find_t structs in tree.
*/
PENTRY AddFile(tree)
PTREE tree;
{
	PENTRY prev;				// previous sibling
	PENTRY ret;					// address of record in tree
	BOOL found;					// TRUE if a file was found
	BOOL first;					// TRUE if 1st srch in given dir
	BOOL newtree=FALSE;			// TRUE if tree is empty
	char path[MAX_PATH+1];		// path to search for file
	int  ParentPathLength;		// Path length of parent dir of file to be added
	int dummylen ;
	
#ifndef NOCONSISTENCY
	if (!tree)
	{
		printf("AddFile*** tree is NULL\n") ;
		exit(0) ;
	}
#endif

	/* Add the record to the tree; this is simpler to do here than after	*/
	/* 	locating the next file.														*/
	ret = AllocTreeEntryCompactIfNeeded(tree) ;
	if (!ret)
		return NULL;

	/* For empty tree, search for files at root level.  If lastlink is a	*/
	/* directory, look for children. If lastlink is a file, look for siblings.*/
	
	if (!tree->head)
	{
		/* We are just beginning to read in the tree */
		first  = TRUE;
		tree->parent = NULL;
		tree->level  = 0;
		strfcpy(path, tree->root);
		ParentPathLength = 3 ;
		newtree= TRUE;
		prev = NULL;
	} else if (tree->lastlink->attribs & _A_SUBDIR)
	{
		/* The last entry we added to the tree was a directory -- Since */
		/*	we do a DFS, we should look for entries in this directory.  */
		first  = TRUE;
		tree->parent = tree->lastlink;
		tree->level  = Tree2Path(tree, tree->lastlink, path, &ParentPathLength);
		prev = NULL;
	} else
	{
		/* The last entry we added to the tree was a file and not a dir! */
		/* ZZZZZZ Speed up following code-path as we do a Tree2Path only to
		 * find the length of parent path name. This is needed to handle cases
		 * when we, say, handle OS/2 servers etc that allow path lengths > 66.
		 * We could do some cache-ing to figure out ParentPathLength, as this
		 * will be done for each file in tree!
		 */
		first = FALSE;
		Tree2Path(tree, tree->parent, path, &ParentPathLength);
		prev = tree->lastlink;
 	}

	/* Search for files at given level; if there aren't any then go up a*/
	/* level and try again.  Repeat until a file is found or the root 	*/
	/* directory is exhausted. 											*/
	do {
		found = GetNextFile(path, tree, ret, first, ParentPathLength);
		first = FALSE;
		
		if (!found)								// no file found
		{
			/* Didn't find a file at root level or any of its children. */
			if (tree->level <= 0)
				return NULL; /* We have exhausted the tree on the drive */

			prev = NULL;
			tree->level--;
			path[ParentPathLength = FindLastComponent(path)] = EOS;
			tree->parent = FindParent(tree->parent);
		}
		/* Now check to see if we are in Compact mode. In this case, */
		/* we don't add normal files even if it is in displayed directory */
		/* We only add directories. LoadCompactDir later loads these files */
		else 
			if ( (tree->Compacted) && (!(ret->attribs & _A_SUBDIR)) )
			{
				/* This file is not added to the tree but still we increment
				 * the following count as this file belongs to this disk.
				 */
				tree->Diskfilecount++;

				found = FALSE ;
			}
	} while (!found);
	
	tree->lastlink = ret;

	if (!tree->head)
		tree->head = ret;
						
	/* If ret has a previous sibling, update its sibling ptr. If 	*/
	/* not, update its parent's child ptr.  If it is the first		*/
	/* file in the tree, skip the whole thing.						*/
	if (prev)
	{
		ret->sibling = prev->sibling;
		prev->sibling = ret;
		prev->nosib = FALSE;
	} else if (tree->parent && !tree->parent->x.d.child)
	{
		tree->parent->x.d.child = ret;
		ret->sibling = tree->parent;
	} else if (newtree)
	{
		ret->sibling = NULL;
	} else 
	{
		/* If parent, first sibling is its child ptr. If no parent, */
		/*  first sibling is tree->head.  							*/
		if (tree->parent)
			prev = tree->parent->x.d.child;
		else
			prev = tree->head;
		
		while (!(prev->nosib))
			prev = prev->sibling;
		prev->sibling = ret;
		prev->nosib = FALSE;

		/* ret is the last sibling in the chain -- So make its sibling ptr	*/
		/* point to its parent 												*/
		ret->sibling = tree->parent;
	}

	ret->nosib = TRUE;

	if (ret->attribs & _A_SUBDIR)
	{
		tree->DirCount++;

		/*  set the level of the directory--used for 'LASTDIR' setting */
		/* ZZZZZZ Can I use tree->level+1 or something like that */
	    ret->dtlx.lx.level = (BYTE) Tree2Path(tree, ret, path, &dummylen) ;

		/* This is the first time we are seeing this directory. Obviously*/
		/* we haven't seen any files belonging to it yet. 				*/
		ret->FIRSTFILEFOUND = FALSE ;

		/* Now mark the collapsed directory bits */
		// No directory is expanded initially (except root (NULL))
		ret->EXPANDED = FALSE ;

		/* If a child dir is found later, this will be set to TRUE then. */
		ret->HASSUBDIRS = FALSE ; 

		/* directory's files are not sorted -- as we haven't seen any files
		 * belonging to directory 'ret' yet!! */
		ret->DIRSORTED = FALSE ;

		// since 'ret' is a directory, mark its parent as having a sub-dir!
		if (tree->parent)
			tree->parent->HASSUBDIRS = TRUE ;

		/* The assignments above like HASSUBDIRS need to be done before this
		 * InsertDir call as this function looks at this to do complex
		 * operations -- needed when renaming directories.
		 */
		InsertDir(tree, tree->parent, ret) ;

		/* This is used in CompactifyTree! */
		tree->LastDirectory = ret ;

		// Initially only directories at level 1 are displayed.
		if (ret->dtlx.lx.level == 1)
		{
			ret->DISPLAYED = TRUE ;
			tree->VisibleDirCount++ ;
		}
		else
			ret->DISPLAYED = FALSE ;

	} else
	{
		tree->Diskfilecount++;

		tree->filecount++; // another file to be added to the tree.

 		if (!tree->FirstFile)
 			/* file head needs initialization */
 		    tree->FirstFile = ret;
 		else
			/* link up all files in the order seen -- sorting done later */
 		    tree->LastFile->x.f.snext = ret;
 		tree->LastFile = ret;
 		ret->x.f.snext = NULL;

		/* assume matching pattern to be "*.*" */
		ret->MATCHESPATTERN = TRUE ;

		/* field used to delete files from snext chain is FALSE by default */
		ret->DELMARKER = FALSE ; 
	}
		
	return ret;
} /* proc AddFile */

/****	GetNextFile - fill a record w/directory info of next file in tree
**	This fn is called by AddFile to get the next file and the stats
**	stored in its directory entry.
**
**	ENTRY
**		path     - path to search, only needed if first == TRUE
**		tree	 - find_t to pass to findfirst/next is got using
**				 - tree->finds[tree->level].
**		rptr     - fileinfo to put data in
**		first    - TRUE = run findfirst, FALSE = run findnext
**	EXIT
**		TRUE  = found file, *rptr has data
**		FALSE = no more files in path.
**	WARNING:
**		Does NOT update ptrs in other records, like the child ptr in
**		the parent record.
**
**	EFFECTS:
**		Changes findinfo data.
*/
BOOL GetNextFile(char path[], PTREE tree, PENTRY rptr, BOOL first,
														int ParentPathLength)
{
	struct find_t findinfo;
	char searchpath[MAX_PATH];		// search mask for _dos_findxxx
	BOOL ignore;					// TRUE = pretend file doesn't exist
	int i, ret;						// junk integers

	/* copy from far memory to near memory -- so that we can make the calls
	   to _dos_findfirst, _dos_findnext which accept only near pointers!! */
	findinfo = tree->finds[tree->level] ;

	do {
		if (first)
		{
			/* Don't change the contents of 'path' -- caller uses it later */
			strcpy(searchpath, path);
			i = ParentPathLength ;

			searchpath[i++] = PATHCHAR ;

			/* Most of the time this 'if' will fail -- It will succeed only */
			/* when path happens to be the root, ex: "C:\". So we don't pay */
			/* the penalty of retracting 'i' most of the time!!				*/
#ifdef DBCS
			if (searchpath[i-2] == PATHCHAR &&
					!CheckDBCSTailByte(searchpath,&searchpath[i-2]))
#else
			if (searchpath[i-2] == PATHCHAR)
#endif
				i-- ;

			/* We don't return directory paths exceeding 66! -- see code below
			 * So, the caller should not invoke us with a path length > this
			 * allowed length!
			 */
			assert(ParentPathLength <= ALLOWED_PATH) ;

			strcpy(searchpath+i, szStarDotStar) ;

			ret = shell_findfirst(searchpath, _A_HIDDEN | _A_SYSTEM | _A_SUBDIR,
								 &findinfo);
			first = FALSE;
		} else
			ret = _dos_findnext(&findinfo);

		if (ret)
			break ;

		/* ret == 0 => An entry was found!! Skip '.' and '..' */
		ignore =  (!ret) && (*(findinfo.name) == '.') ;

		/* The maximum path length allowed for a directory in DOS is 66. We
		 * ignore any directories we encounter beyond this path length.
		 * However, files can be present in a directory with path length 66!
		 */
		/* ZZZZZ should we warn user about this ignoring we do! */
		 /* Note that path length check is enough and we need not check for
		  * directory nesting level > 32! It is a superset check!
		  */
		 if (ParentPathLength >= ALLOWED_PATH)
			ignore = (ignore || (findinfo.attrib & _A_SUBDIR)) ;
		 else
			/* If parent path length is 54 or greater then there is a likelihood
			 * that the new directory name could exceed the 66 character
			 * limit for directories as a PATH_CHAR and 8.3 name could cross
			 * the limit.
			 */
			/* The first check is present to avoid the "strlen()" call! */
			if (ParentPathLength >= (ALLOWED_PATH-NAMELEN-EXTLEN-1))
				if ( (findinfo.attrib & _A_SUBDIR) &&
					 ((ParentPathLength+strlen(findinfo.name)+1) > ALLOWED_PATH)
				   )
				   ignore = TRUE ;

	} while (!ret && ignore);
		
	if (!ret)
	{
		/* A File/directory entry has been found. Copy common information.  */
		rptr->attribs = findinfo.attrib;
		rptr->nosib = rptr->SELECTED = rptr->LASTDIR = 0;
		rptr->sibling = NULL;

		Normal2Internal(rptr->name, findinfo.name);	/* put in name & ext */
		
		if (rptr->attribs & _A_SUBDIR)
		{
			rptr->x.d.child = NULL;
			rptr->x.d.dnext = NULL;

			/* rptr->LASTDIR will be marked after whole tree has been read in*/
			/* using function MarkLastDir. 									*/
		} else
		{
			/* time & date are not stored for directories. These entries	*/
			/* are 'union'ized with directories 'level' info.				*/
			rptr->dtlx.dt.time = findinfo.wr_time;
			rptr->dtlx.dt.date = findinfo.wr_date;

			rptr->x.f.size = findinfo.size;
		}
	} /* end of processing entry found */

	/* Copy info back to far memory -- to pass back to caller */
	tree->finds[tree->level] = findinfo ;

	/* return TRUE if file found, else FALSE */
	return (!ret);
} /* proc GetNextFile */

/****	AddLateFile - add a file to a tree after the tree has been built
**		File operations will typically call this fn by giving the path and the
**	record containing the file's attributes etc.  The fn will determine the
**	proper destination tree and other info.
**		LoadCompactDir, because of its need for speed, will call this fn with
**	path=NULL, and instead put correct values into tree, parent, and idx.
**	Since tree and parent do not change through the course of the LoadCompactDir
**	call and idx is merely incremented each time this fn is called, much time
**	is saved.
**
**	ENTRY
**		either:
**				path - fully qualified path of file to add
**				rec  - its vital info
**		or:
**				path   - NULL
**				rec    - file's vital info
**				tree   - tree to add file to
**				parent - parent of file to add
**				idx    - index in directory (first = 0) of file
**	EXIT
**			TRUE if successful, FALSE if not
**	WARNING
**		The path must reference a valid drive.
**	NOTE
**		If path != NULL, and the tree is compacted, the fn will only add the
**	file if its parent is being displayed onscreen.
**		If path == NULL, various consistency checks are not performed since
**	the caller is assumed to know what it's doing.
*/
BOOL AddLateFile(char *path, PENTRY rec, PTREE tree, PENTRY parent, int idx)
{
	int i;
	BOOL ok;
	BOOL first;								// use findfirst
	int ret;								// return from dos call
	
	PENTRY rptr;							// record in the tree
	PENTRY prev;							// previous sibling
	PENTRY prevfil;							// previous sibling that was a file

	struct find_t findinfo;					// search struct 
	char spath[MAX_PATH+1];					// search path
	char NormalizedName[NAMELEN+EXTLEN+2] ;
	BYTE dummy ;
	PENTRY filenode ;

#ifndef NOCONSISTENCY
	if (rec->attribs & _A_SUBDIR)
	{
		printf("AddLateFile called to add a directory entry\n") ;
		exit(0) ;
	}
#endif

	if (path)
	{
		// If the tree has not been built yet, don't add.
		tree = FindTree(path, &dummy);

		/* A file will be added to this tree -- So, disk avail, disk free size,
		*	will change. They will have to be read in when 'show info' is done
		*  later.
		*/
		tree->fdiskinfoknown = FALSE ;
		
#ifndef NOCONSISTENCY
		if (!tree)
		{
			/* Path specified not any drive hooked up!! */
			/* Display an error message  -- this function shouldn't be called */
			printf("*** wrong drive\n") ;
			return TRUE; /* dummy value */
		}
#endif

		/* Not added, if we have not started reading in 'tree' into memory */
		if (!tree->Started)
			return TRUE;

		/* Consider case when 'tree' has been partially read in and 		*/
		/* we are adding to it. This new file may conflict with AddFile,	*/
		/* GetNextFile. To handle this case, we just junk 'tree'. It 'tree	*/
		/* is to read in later, it will be started afresh. Actually this 	*/
		/* scenario won't occur often to worry about it!					*/
		if (tree->ContinueTree)
		{
			JunkTree(tree) ;
			return TRUE ;
		}

		i = FindNode(tree, path, &parent, &filenode, TRUE);
#ifndef NOCONSISTENCY
		if (!i)
		{
			printf("*** BUG! parent directory doesn't exist\n") ;
			exit(0) ;
			return TRUE;
		}
#endif
		if (i == 2)
		{
			DelFileEntry(tree, parent, filenode);
		}

		/* If the tree is compacted, add only if the file is in a displayed */
		/* directory.  														*/
		if (tree->Compacted)
		{
			ok = FALSE;
			for (i=0; i <= glob.MaxTree; i++)
			{
				if (listinfo[i].tree == tree && listinfo[i].files == parent)
					ok = TRUE;
			}
			if (!ok)
				return TRUE;
		}

		/* Locate the file's position in its directory -- i.e., find its	*/
		/* order in the disk among its siblings.							*/
		idx = 0;
		strcpy(spath, path);				// move to scratch area
		i = FindLastComponent(spath);
		if (spath[i] == PATHCHAR)
			i++;
		strcpy(spath+i, szStarDotStar) ;

		/* When called from CopyFile, MoveFile, etc name might not be
		 * normalized, i.e., we might not have name_lens <= 8, ext_lens <= 3
		 */
		ScrunchFileName(NormalizedName, path+i, FALSE) ;

		ok = FALSE;
		first = TRUE;
		do 
		{
			if (first)
			{
				ret = shell_findfirst(spath, _A_HIDDEN | _A_SYSTEM | _A_SUBDIR,
									 &findinfo);
				first = FALSE;
			} else
				ret = _dos_findnext(&findinfo);

			/* In case there was an error in this FindFirst or FindNext
			 * we assume that the file is the first in DISK-ORDER - default
			 * order.
			 */
			if (ret)
				break ;

			/* ignore '.', '..' entries */
			if (*findinfo.name == '.')
				/* Do Nothing */ ;
			else
			if (strcmpi(NormalizedName, findinfo.name))
				idx++;						// not our file
			else
				ok = TRUE;					// found its index
		} while (!ok);
	} /* if (path) */

	/* Locate file's position in directory tree. */
	if (idx)
	{
		prev = parent ? parent->x.d.child : tree->head ;
		/* We know that prev can't be NULL as idx > 0 */
		prevfil = (prev->attribs & _A_SUBDIR) ? NULL : prev ;
		idx-- ;
	}
	else
	{
		prev = prevfil = NULL;
	}
	while ( (idx > 0) && !(prev->nosib) )
	{
		prev = prev->sibling;
		if (!(prev->attribs & _A_SUBDIR))
			prevfil = prev ;
		idx-- ;
	}

#ifndef NOCONSISTENCY
	if (idx)
	{
		printf("idx is positive, in-memory tree doesn't extend far enough\n");
		exit(0) ;
	}
#endif

	rptr = AllocTreeEntryCompactIfNeeded(tree) ;
	if (!rptr)
	{
		/* Note that we might be in this routine when performing
		 * a LoadCompactDir, when user is in program manager.
		 * So, we don't want to affect his screen in that case.
		 */
		if (glob.InFileMgr)
		{
			OutOfMemory() ;
		}
		return FALSE ; // AddLateFile can't succeed as there is no mem
	}

	*rptr = *rec ;

	/* link 'rptr' with its parent, siblings -- i.e., sibling, child ptrs */
	UpdateSiblings(tree, parent, prev, rptr) ;

	/* Set rptr->MATCHESPATTEN correctly! */
	MarkMatchesBit(rptr) ;

	rptr->SELECTED = FALSE ;

	tree->filecount++ ;

	rptr->FIRSTDIRFILE = rptr->LASTDIRFILE = FALSE ; /* default values */

	/* link up in the 'snext' chain correctly */
	UpdateLatesnext(tree, parent, rptr, prevfil, FALSE) ;

	return TRUE;
} /* proc AddLateFile */


/****	UpdateSiblings -
** The following function updates the sibling pointers, child pointer of
** parent and if necessary tree->head. It is invoked when a new file or
** directory is created.
**
**	ENTRY
**				tree - tree to add to.
**				parent - parent of record 'rptr'.
**				prev - the sibling that is the predecessor of rptr in diskorder
**				rptr - the file/directory entry to be added to the tree.
**	EXIT
**			Function will always be succesful -- no mem alloc, etc!
*/
void UpdateSiblings(PTREE tree, PENTRY parent, PENTRY prev, PENTRY rptr)
{
	if (prev)
	{
		rptr->sibling = prev->sibling;
		prev->sibling = rptr;
		rptr->nosib = prev->nosib;
		prev->nosib = FALSE;
	} 
	else
	{
		if (parent)
		{
			if (parent->x.d.child)
			{
				rptr->sibling = parent->x.d.child;
			 	rptr->nosib = FALSE;
			} else
			{
				rptr->sibling = parent;
				rptr->nosib = TRUE;
			}
			parent->x.d.child = rptr;
		} 
		else
		{ 
			/* file is part of root directory, as parent is NULL */

			rptr->nosib = (tree->head == NULL) ;

			/* OK even when tree->head is NULL, rptr->sibling points to NULL
				which is the OK, as parent is Root directory */
			rptr->sibling = tree->head;

			tree->head = rptr;
		}
	} /* prev == NULL */
} /* UpdateSiblings */

/****	DelFileEntry - delete entry from file tree. Doesn't handle the
** 'dnext' chain if node is a directory -- It is the caller's responsibility
** to do that!
**
**	ENTRY
**			tree - tree to delete from
**			node - node to delete
**			parent - parent of node to delete (NULL if in root directory)
**	EXIT
**		Just like AddLateFile, it will always be succesful!
**	WARNING
**		If any of the tree pointer chains have been damaged, the function
**	will never exit.  The node must exist in the tree.
*/
VOID DelFileEntry(tree, parent, node)
PTREE tree;
PENTRY parent;
PENTRY node;
{
	PENTRY prev;						// previous node in chain

	/* A file will be deleted from this tree -- So, disk avail, disk free size,
	*	will change. They will have to be read in when 'show info' is done
	*  later.
	*/
	tree->fdiskinfoknown = FALSE ;

	/* Handle file sort chain. Directory chains aren't handled here.
	* It is the caller's responsibility to handle dnext chains and
	* decrement tree->DirCount!
	*/
	if (!(node->attribs & _A_SUBDIR))
	{
		HandleDeletesnext(tree, parent, node) ;
		tree->filecount-- ;
	}

	/* Handle parent-child pointers */
	/* ZZZZZZ There is some duplicate code below -- while loop and next
		couple of statements. I wonder if its removal will make good code
		savings!
	*/
	if (parent)
	{
		if (parent->x.d.child == node)
			parent->x.d.child = (node->nosib) ? NULL : node->sibling ;
		else
		{
			prev = parent->x.d.child;
			while (prev->sibling != node)
				prev = prev->sibling;
			prev->sibling = node->sibling;
			prev->nosib = node->nosib;
		}
	} 
	else
	{
		/* parent is the root directory */
		if (tree->head == node)
			tree->head = node->sibling;	// if nosib, parent = NULL which is ok
		else
		{
			prev = tree->head;
			while (prev->sibling != node)
				prev = prev->sibling;
			prev->sibling = node->sibling;
			prev->nosib = node->nosib;
		}
	}
	
	node->name[0] = EOS; /* Officially mark node as deleted */
	tree->holecount++ ; /* one extra hole in page entry pool */
} /* proc DelFileEntry */
