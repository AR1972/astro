;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****   file helper.c: miscellaneous helper routines for File Manager.
**
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  7/27/89   t-jeffro  Modified routines to recognize the _A_NOSIB flag.
**            harikris  Rewrote some fns like FillRecName.
**                                              Added utility routines like Internal2Normal,
**                                              Normal2Internal, etc.
*/
#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <text.h>
#include <help.h>

#include <assert.h>


/****   Tree2Path - construct full path of a node
**      This fn constructs the actual pathname of the file or directory stored
**      in 'node'.
**
**      ENTRY
**              tree - tree containing the file node
**              node - file node to look at, or NULL for the tree root
**              str  - string to store path in
**    plength - storage to put the length of 'str' (formed path).
**      EXIT
**              str  - contains path string
**    plength - has length of constructed path
**              Returns depth in tree of file, 0 = root directory itself
**      WARNING:
**          Make sure str is at least MAX_PATH+1 long.
*/
int Tree2Path(PTREE tree, PENTRY node, char *str, int *plength)
{
	char components[MAXNODE][NAMELEN+EXTLEN+2];
	int temp ;
	char *ptemp ;
	int level = 0;
	int idx = 0;
	
	if (!tree || !str)
	{
		*plength = 0 ;

		if (str)
			*str = '\0' ;

		return -1;
	}
	
	/* Build component strings.
	*/
	while (node)
	{
		Internal2Normal(components[idx], node->name);
		node = FindParent(node);
		idx++;
	}
	level = idx;
	
	/* Combine the components into a path. */
	idx--;
	strfcpy(str, tree->root);
	temp = 3 ; // length of root path like c:\  etc is 3!

	while (idx >= 0)
	{
		ptemp = components[idx] ;
		while(*ptemp)
		{
			str[temp++] = (*ptemp++) ;
		}

		if (idx)
			str[temp++] = PATHCHAR ;

		idx--;
	}
	str[*plength = temp] = '\0' ;

	return level;
} /* proc Tree2Path */

/****   FindParent - returns ptr to parent of given record in tree
**      There's really not much else to say.
**
**      ENTRY
**              node - record whose parent you want
**      EXIT
**              ptr to the parent record, or NULL if error or parent is root.
*/
PENTRY FindParent(PENTRY node)
{
	if (!node)
		return NULL;
	
	/* If nosib is turned on, then there are no more siblings and the
	** sibling ptr actually points to the parent.
	*/
	while (!node->nosib)
		node = node->sibling;
	return node->sibling;
} /* proc FindParent */

/***    FindLastComponent - returns index of last component of a path
**
**      This fn finds the index of the last component of the path.  The
**      index returned points to the backslash in front of the component
**      name, except in two cases.  The first is when the path is like 
**        "bin", where the index returned is 0; the second is when it is
**      like "C:\bin" where the index returned is the beginning of the name
**      itself.  This is done so that lopping off the last component leaves
**      the correct parent, "C:\", rather than "C:" which refers to the
**      current directory on C:.
**       
**      ENTRY
**              path - path to work on
**
**      EXIT
**              Return val: index as described above.
**
**      WARNING:
**          If the app wants the name of the component, it should skip forward
**      one byte if path[return val] == PATHCHAR.  "path" is not checked
**      for NULLness.
**
**      EFFECTS:
**              none
*/
int FindLastComponent(path)
char path[];
{
	register int i;
	BOOL hasdrive;
	
	if (path[1] == ':')
		hasdrive = TRUE;
	else
		hasdrive = FALSE;
	
	i = strlen(path)-2;                                     // start at last char but one
#ifdef DBCS
	while ((i >= 0 && path[i] != PATHCHAR) ||
			(i >= 0 && CheckDBCSTailByte(path,&path[i])))
#else
	while (i >= 0 && path[i] != PATHCHAR)
#endif
		i--;
	if (i == 0)                                                     // path like "\word"
		i = 1;                                  
	else if (i < 0) {                                       // path like "word"
		i = 0;
		return i;
	} else if (hasdrive) {
		if (i == 2)                                             // path like "c:\word"
			return 3;                                       // it belongs to 1st component
	}
	return i;
	
} /* proc FindLastComponent */

/* ZZZZZZ I think I can get rid of      this function and use fn Internal2Normal
 * instead!!
*/
/****   FillRecName - put file name into a record
**      This fn takes the filename part of a path and puts it into the given
**      fileinfo record.
**
**      ENTRY
**                      path - the filename to add
**                      rec  - far ptr to the record
**      EXIT
**                      none
**      WARNING
**              Although the fn will accept a fully qualified path, only the actual
**      file name is stored in the record.  The rest is not used.
**              There is no error checking to speak of.
*/
VOID FillRecName(path, rec)
char *path;
PENTRY rec;
{
	path += FindLastComponent(path);                // skip over path
	if (*path == PATHCHAR)
		path++;

	Normal2Internal(rec->name, path) ;
} /* proc FillRecName */

/****   FindTree - locate tree corresponding to the given pathname
**
**      ENTRY
**              path - fully qualified pathname
**      EXIT
**              a ptr to the tree, or NULL if not found. Also index in the list 0 based.
**      NOTE
**              The path does not need to specify a real file; only the drive letter
**      is used.
*/
PTREE FindTree(char path[], BYTE *driveind)
{
	PTREE tree;

	*driveind = 0 ;
	tree = glob.drives;
	while (tree && tree->root[0] != *path)
	{
		*driveind = (*driveind) + 1 ;
		tree = tree->next;
	}
	return tree;
}

/****   FindNode - find node corresponding to given file
**
**      ENTRY
**              tree      - tree to search, or NULL to force fn to find it
**              path      - path to find node of
**              theparent - ptr to variable to store parent ptr in
**              thenode   - ptr to variable to store file ptr in
**              fForceFind   - whether to find node even if tree is not yet fully
**                   built. TRUE implies Search for it anyway.
**      EXIT
**              0 - neither file nor its parent were found.
**              1 - file wasn't found, but its parent was; *theparent contains the ptr.
**              2 - file was found; both *theparent and *thenode contain valid ptrs.
*/
int FindNode(PTREE tree, char path[], PENTRY *theparent, PENTRY *thenode,
																	BOOL fForceFind)
{
	BOOL done;                                                      // found last directory
	char *temp;                                                     // used for excising path components
	PENTRY node;                                            // local file ptr
	PENTRY parent;                                          // local parent ptr
	BYTE dummy ;

	if (!tree)                                                      // need to locate tree
	{
		tree = FindTree(path, &dummy);
	}

	if ( (!tree) || (!tree->Started) )
		return 0 ;

	/* If fForceFind is FALSE, and if tree is not yet built completely, say
	 * that node and its parent are not present.
	 */
	if ( (!fForceFind) && (tree->ContinueTree) )
		return 0 ;

	path += 3;                                                      // get past drive & leading '\'
	done = FALSE;
	parent = NULL;                                          // start from root directory
	do {
		/* Isolate the top-level component.  */
#ifdef DBCS
		temp = DBCSstrchr(path, PATHCHAR);
#else
		temp = strchr(path, PATHCHAR);
#endif
		if (!*temp)
		{
			done = TRUE;                            // only the file name remains
		} else
		{
			*temp = EOS;
		
			/* Find component's location in tree.  */
			parent = File2Node(tree, parent, path);
			*temp = PATHCHAR;                       // restore string
			if (!parent)
			{
				return 0;                               // directory not found
			}
			path = temp+1;                          // beginning of next component
		}
	} while (!done);

	*theparent = parent;

	if (*path == EOS)
	{
		// path is empty - we are examining the root directory itself.
		*thenode = NULL;
		return 2;                                               // both items valid
	} else
	{
		node = File2Node(tree, parent, path);
		*thenode = node;
		return (node ? 2 : 1);                  // either one or both items is valid 
	}
} /* proc FindNode */

/****   File2Node - finds node corresponding to a file
**
**      ENTRY
**                      tree - tree to investigate
**                      dir  - directory that the file is in
**                      file - file name to locate
**      EXIT
**                      pointer to the file's record, or NULL if not found.
*/
PENTRY File2Node(tree, dir, file)
PTREE tree;
PENTRY dir;
char file[];
{
	char pInternalForm[NAMELEN+EXTLEN];             // extension of file
	PENTRY node;                                                    // node to check

	node = (dir) ? dir->x.d.child : tree->head ;
		
	if (!node)
		return NULL;

	Normal2Internal(pInternalForm, file) ;
	
	while (node)
	{
		if (fstrncmp(pInternalForm, node->name, NAMELEN) ||
			fstrncmp(pInternalForm+NAMELEN, node->name+NAMELEN, EXTLEN))
		{
			node = (node->nosib) ? NULL : node->sibling ;
		} 
		else
			break; /* node found! */
	} /* while */

	return node;
} /* proc File2Node */

extern char szStarDotStar[] ;

/****   LoadCompactDir  - read in new directory's files in low-memory mode
**
**      ENTRY
**                      tree - tree to manipulate
**                      new  - newly selected directory
**      EXIT
**                      TRUE if ok, FALSE if out of memory
**      CAUTION
**              This fn should not be called if the files are already in the tree.
*/
BOOL LoadCompactDir(PTREE tree, PENTRY new)
{
	struct find_t fdata;                            // data used by _dos_findxxx
	fileinfo rec;                                           // record to add to tree
	unsigned ret;                                           // return code from dos call
	int action;                                                     // what to do
	BOOL first;                                                     // TRUE if run findfirst
	char path[MAX_PATH+1];                          // path of new directory
	int idx;                                                        // index of file in directory
	int dummylen ;
	char *tempcaption ;

	/* Wipe out from memory any files currently in directory 'new' and
	 * start with a clean slate.
	 */
	ClobberDir(tree, new) ;

	/* Add all the files in this directory.
	*/
	Tree2Path(tree, new, path, &dummylen);
	idx = strlen(path);
#ifdef DBCS
	if (path[idx-1] != PATHCHAR || CheckDBCSTailByte(path,&path[idx-1]))
#else
	if (path[idx-1] != PATHCHAR)
#endif
		path[idx++] = PATHCHAR;
	strcpy(path+idx, szStarDotStar) ;

	first = TRUE;
	idx = 0;
	do {

		// Get the file from disk.
		do {
			if (first)
			{
				first = FALSE;
				ret = shell_findfirst(path, _A_HIDDEN | _A_SYSTEM, &fdata);
			} else
				ret = _dos_findnext(&fdata);

			if (ret && ret != 18)           // abnormal error
			{
				/* No status line */
				tempcaption = gpszFileOpCaption ;
				gpszFileOpCaption = szErrorCaption ;
				action = DOSErrorBox(szErrorFindfirstnext, ret, HELP_DISKLOADFILES);
				gpszFileOpCaption = tempcaption ;
			} else
				action = ACT_OK;
		} while (action != ACT_OK && action != ACT_FORCE);

		// If there is a file to add, add it to the tree.
		if (!ret)
		{
			// Found a file; add it to the tree.
			FillRecName(fdata.name, (PENTRY) &rec); // put name/ext into record
			rec.dtlx.dt.time = fdata.wr_time;
			rec.dtlx.dt.date = fdata.wr_date;
			rec.attribs = fdata.attrib;
			rec.sibling = NULL;
			rec.nosib = rec.SELECTED = 0;
			rec.x.f.size = fdata.size;
			rec.x.f.snext = NULL;

			/* AddLateFile will locate the proper tree and add our record to it.
			*/
			if (!AddLateFile(NULL, &rec, tree, new, idx))
			{
				// out of memory reading in...
				// OutOfMemory();
				SortDirectory(tree, new) ;
				return FALSE ;
			}
		}
		idx++;
	} while (!ret);

	SortDirectory(tree, new) ;

	/* Files have just been loaded in! -- just re-calc this as the directory
	 * that was re-loaded could have had selected files that got clobbered
	 * now!
	 */
	tree->NumSel = GetTreeSelectedInfo(tree, &(tree->SizeSel)) ;

	if (ret != 18)
		return FALSE;                                   // had an error
	else
		return TRUE;
} /* proc LoadCompactDir */


/* If 'tree' is a Network Drive and it says that there are no files
 * or directories on it, it could be a Novell server on which the user
 * hasn't logged in.
 * If it is any other drive type or network drive, do nothing!
 */
void HandleSpecialCaseNovell(PTREE tree)
{
	BOOL ret ;
   struct find_t findinfo ;
	char LoginDirFilesMatchPat[32] ;
	int len ;

	/* If we are on a network drive and if we get a report that there
	 * are no files or directories on the network, it is likely
	 * to be a Novell server on which the user has not logged in.
	 * We see if there are files in the LOGIN directory. Note that
	 * Novell kind of fakes the directory structure and shows only this
	 * directory (and 8 files belonging to it) when one does a 'dir'
	 * Actually _dos_findfirst() fails on the LOGIN directory but succeeds
	 * on the files belonging to it.
	 */
	/* Since it is a HACK anyway, I am using the string "LOGIN" straightaway.
	 * I don't see much point in getting the current working directory on
	 * this network drive and then seeing if it is a non-root directory
	 * and if so using that name (LOGIN or whatever).
	 */
	if (    (tree->DriveType == REMOTE_TYPE) &&
			(tree->Diskfilecount+tree->DirCount == 0)
		)
	{
		/*   Form the path of the form "H:\LOGIN\*.*"      */
		strfcpy(LoginDirFilesMatchPat, tree->root) ;
		strcpy(LoginDirFilesMatchPat+3, szNovellLoginDirName) ;
		len = strlen(LoginDirFilesMatchPat) ;
		LoginDirFilesMatchPat[len++] = '\\' ;
		strcpy(LoginDirFilesMatchPat+len, szStarDotStar) ;

		ret = _dos_findfirst(LoginDirFilesMatchPat, _A_HIDDEN | _A_SYSTEM,
																		&findinfo) ;
		/* Did we find any files in this directory? If so, it is Novell! */
		if (!ret)
		{
			/* There are no files/dirs -- so it is trivially in sorted order */
			tree->SortRequired = FALSE ;

			/* Create a fake directory -- If we pass in FALSE for the last
			 * argument, fn CreateDirectory() will not attempt to create
			 * a directory on the disk. It will only add this directory
			 * to the tree.
			 */
			CreateDirectory(tree, NULL, szNovellLoginDirName, FALSE) ;

			/* Note that tree->FirstDirectory will be this login directory */
			LoadCompactDir(tree, tree->FirstDirectory) ;
		}
	}
} /* HandleSpecialCaseNovell */


/****   Internal2Normal - forms the proper name from the internal format
**
**      ENTRY
**              dest - destination -- dest has atleast storage for 13 characters
**             8 for name + 3 for extension + 1 for '.' and 1 for NULL.
**              src  - source which is in internal format for file names.
**      EXIT
**              dest - has the filename with '.' as appropriate and null terminated.
**              returns the number of characters in normal name (without NULL)
*/
int Internal2Normal(char far *dest, char far *src)
{
	char far *srcbound ;
	char far *deststart ;

	srcbound = src + NAMELEN ;
	deststart = dest ;

	for ( ; (src < srcbound) && *src ; )
		*(dest++) = *(src++) ;
		
	if (*srcbound)
	{
		*(dest++) = '.';

		src = srcbound ;
		srcbound += EXTLEN ; /* reuse of variable extstart to point to end! */

		for ( ; (src < srcbound) && *src ; )
			*(dest++) = *(src++) ;
		
	}

	*dest = '\0' ;

	return (dest - deststart) ;

} /* Internal2Normal */

/* Returns TRUE iff char 'ch' can't be a part of a file name */
/* ZZZZZ for now we are only handling blanks */
#define fInvalidChar(ch) (ch == ' ')

/****   Normal2Internal - Convert from normal null terminated name to internal
**  format (stored in 11 character format (may or may not be null terminated).
**  Short names & extensions are padded with NULL chars -- aids in sorting!
**
**  ENTRY:
**      'dest' has 11 character storage in it (OK, if more storage available).
*/
VOID Normal2Internal(char far *dest, char far *src)
{
	char far *extstart ;

	extstart = dest + NAMELEN ;

	/* Take upto the '.' or NULL but a max of NAMELEN (= 8) characters */
	while ( (*src != '.') && *src && (dest < extstart) )
	{
		/* Skip invalid chars -- chars that can't be part of file name */
		if (fInvalidChar(*src))
			src++ ;
		else
			*(dest++) = *(src++);
	}

	/* Null fill till end of name  */
	while (dest < extstart)
		*(dest++) = '\0' ;

	/* Extension may be present -- skip until EOS or '.' */ 
	while (*src && (*src != '.') )
		src++ ;

	extstart = extstart + EXTLEN ; /* reuse of this variable */

	/* Is there an extension in the name, i.e., is there a '.'?  */
	if (*src)
	{
		src++ ; /* point to character after '.' */
	

		/* '.' cannot be part of extension -- so we use that as terminator
		 * in case src is ill-formed 
		*/
		while ( (*src != '.') && *src && (dest < extstart) )
		{
			/* Skip invalid chars -- chars that can't be part of file name */
			if (fInvalidChar(*src))
				src++ ;
			else
				*(dest++) = *(src++);
		} /* while */
	} /* if */

	/* If extension is less than 3 characters -- pad with Null characters */
	while (dest < extstart)
		*(dest++) = '\0' ;

} /* Normal2Internal */

/* Warning! Assuming dest & src are pointers to different storages! Otherwise
 * strange things may happen. If fSpecial is TRUE, special handling is done
 * to patterns with a '.' as the suffix -- This is so that pattern matching
 * strings can use this to specify all strings without an extension ("*.")
 * In normal names we can't have a period in the name without an extension.
 */
VOID ScrunchFileName(char *dest, char *src, BOOL fSpecial)
{
	char temp[NAMELEN+EXTLEN] ;
	char *p ;
	int i ;

	Normal2Internal(temp, src) ;

	i = Internal2Normal(dest, temp) ;

	if (fSpecial)
	{
		/* We want to transform patterns like "*." to "*." and not "*" so 
		 * do this special case handling using strchr, etc.
		 */
		p = strchr(src, '.') ;

		if (p && (*(p+1) == '\0'))
		{
			*(dest+i) = '.' ;
			*(dest+i+1) = '\0' ;
		}
	}
} /* ScrunchFileName */

/* Given a file node, this fn, sets it MATCHESPATTERN bit based on
 * "glob.MatchPat" and "glob.DisplayHiddenFiles.
 */
void MarkMatchesBit(PENTRY fil)
{
	fil->MATCHESPATTERN = pmatch(glob.MatchPat, fil->name, FULLNAME) ;
	fil->MATCHESPATTERN =
			fil->MATCHESPATTERN && 
				(glob.DisplayHiddenFiles || 
					!(fil->attribs & (_A_HIDDEN | _A_SYSTEM))) ;
} /* MarkMatchesBit */


/* Reset TreeMatchedFiles optimizations -- This is typically done when the
 * "MatchPat" has changed or if the user has decided to change the hidden or
 * system files display mode -- basically, the total number of displayed
 * files on the disk/tree has changed.
 */
void ResetTreeMatchedFilesOpt(PTREE tree)
{
	extern PENTRY gLastEntry ;
	
	tree->nummatches = RECALCNUMMATCHES ;
	gLastEntry = NULL ;
} /* ResetTreeMatchedFilesOpt */


void FormCountStatusLine(char *statusline, char *szopstr, char *path,
					  unsigned  count, unsigned total, int indexofcount)
{
	int i ;

	/* Copy string (ex of strings: "Moving File:", "Deleting File:", etc) */
	for (i= 0 ; *szopstr ; i++)
		*(statusline++) = *(szopstr++) ;

	/* two blanks after the operation string! */
	/* only one if ONESPACEAFTERCOLONINSTATUSLINE is defined */
#ifndef ONESPACEAFTERCOLONINSTATUSLINE
	*(statusline++) = ' ' ;
	*(statusline++) = ' ' ;
	i+=2;
#else
	*(statusline++) = ' ' ;
	i+=1 ;
#endif
	/* Now the filename starts! */
	path += FindLastComponent(path) ;
	if (*path == PATHCHAR)
		path++ ;

	for (; *path ; i++)
		*(statusline++) = *(path++) ;

	if (total > 0)
	{
		/* Pad with blanks until start of string xx of xx */
		for ( ; i < indexofcount ; i++ )
			*(statusline++) = ' ' ;
	
		/* Warning! Assuming that Formxxofxx puts the EOS at end of string */
		Formxxofxx(statusline, count, total) ;
	}
	else
		*statusline = '\0' ;
} /* FormCountStatusLine */

/* Warning! Assuming that "str" is atleast 15 bytes long! -- 5 bytes for
count, 4 bytes for " of ", 5 bytes for total, 1 for NULL char. */
void Formxxofxx(char *str, int count, int total)
{
    char szCount[7], szTotal[7];
    int i;

    for(i=0; i<6; ++i)
	szCount[i] = szTotal[i] = ' ';
    szCount[6] = szTotal[6] = '\0';

    CopyNumberForTextOut(szCount+5, (unsigned long) count, FALSE);
    CopyNumberForTextOut(szTotal+5, (unsigned long) total, FALSE);

    FormStringWithoutPlaceHolders(str, szXOfX, (char far *)szCount+1,
	    (char far *)szTotal+1);
} /* Formxxofxx */


/* Concatenates str1, str2 with 2 blanks as separator in between them           */
/* Also 'maxlen' is the number of bytes in storage availbale in statusline  */
/* At the end of this routine, 'statusline' will be null terminated.            */
void FormStringStatusLine(char *statusline, char *str1, char *str2, int maxlen)
{
	int len ;

	len = strlen(str1) ;
	/* ZZZ remove before shipping! -- There should be atleast space for
	 * str1, two blanks & NULL char.
	 */
	assert(maxlen > (len + 2 + 1)) ;

	strcpy(statusline, str1) ;

	/* two blanks as separator */
	statusline[len]   = ' ' ;
	statusline[len+1] = ' ' ;

	strncpy(statusline+len+2, str2, maxlen-1-len-2) ;

	statusline[maxlen-1] = '\0' ;

} /* FormStringStatusLine */

#if 0
/* ZZZ Old code!! I rewrote it using call Normal2Internal fn call! */
/*      Saves on code-size. Normal2Internal takes care of ill-formed path! */
VOID FillRecName(path, rec)
char *path;
PENTRY rec;
{
	int i;
	char c;                                                                 // current character
	char far *dest;                                                 // destination ptr

	path += FindLastComponent(path);                // skip over path
	if (*path == PATHCHAR)
		path++;

	dest = rec->name;

	/* Copy name.
	*/
	while ((c=*path) != '.' && c)
		*dest++ = *path++;
	*dest = EOS;

	/* Copy extension.
	*/
	dest = rec->name+NAMELEN;
	if (c == '.')
	{
		path++;

		i = 0;
		while (*path)
		{
			*dest++ = *path++;
			i++;
		}
		if (i < EXTLEN)
			*dest = EOS;
	} else
	{
		*dest = EOS;
	}
} /* proc FillRecName */

/* I rewrote this routine to reduce code size and improve readability! */
PENTRY File2Node(tree, dir, file)
PTREE tree;
PENTRY dir;
char file[];
{
	char ext[4];                                                    // extension of file
	PENTRY node;                                                    // node to check
	char *extptr;                                                   // ptr to extension of file

	if (dir)
		node = dir->x.d.child;                          // fetch first child
	else
		node = tree->head;
	if (!node)
		return NULL;

	extptr = strchr(file, '.');
	if (extptr)
	{
		strncpy(ext, extptr+1, EXTLEN);         // copy extension w/o '.'
		ext[3] = EOS;                                           // used if ext is 3 chars long
		*extptr = EOS;                                          // zap out extension from file
	} else
	{
		ext[0] = EOS;
	}

	while (node)
	{
		if (fstrncmp(file, node->name, NAMELEN) ||
			fstrncmp(ext, node->name+NAMELEN, EXTLEN))
		{
			if (node->nosib)                                // end of chain, file doesn't exist
			{
				node = NULL;
				break;
			} else
				node = node->sibling;
		} else
		{
			break;
		}
	}
	if (extptr)
		*extptr = '.';                                          // restore full file name

	return node;
} /* File2Node */
#endif
