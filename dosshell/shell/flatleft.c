;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*  --------  --------- ---------------------------------------------------
**   10/10/89  harikris Created this file - flatleft.c
**   12/11/89  harikris Optimized it to use tree->SizeSel & tree->NumSel
**                      instead of having to call GetTreeSelInfo!
*/
#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>
#include <text.h>
#include <help.h>

extern BOOL gfFMBeingInited ;

#if defined(SWE)|| defined(RUS)
  void CopyNumberYST(char *dest, unsigned long val, BOOL fFillBlanks, int maxc);
#endif

/****   DispFlatLeft - Displays the info about the node
**
**      ENTRY
**              node - The file node whose info is to be displayed
**              tree - The tree to which it belongs.
*/

/* Note: Same Stuff is used by showinfo too!!  ShowInfo Dialogue box should */
/*        thus be as wide as this width!!                                   */
/*                                                                          */

/* ZZZ We can get rid of extra blanks to the right of the showinfo strings
	and dynamically generate them. This may slow flat display quite a bit.
	Also we need to only re-display the non-static strings on screen,
	that is only the info that changes. This doesn't seem to be a problem
	though. Disk may not have to re-read for info like label, etc!
*/
//  BEGIN  - IPG (Mihindu)  Moved to DOSSHELL\INC\TEXT.H
// /* INTERNATIONALIZE HERE! */
// /* INFOWIDTH is the number of characters in the Info strings! So, if the    */
// /* strings are changed, this constant has to be changed too. It should be   */
// /* noted that these strings are actually modified in place -- by filling in */
// /* the names, numbers of files, etc. It should also be borne in mind that 25*/
// /* is about the right(max) number, as screens generally are 80 chars wide, and*/
// /* System Tree Mode in the file list box uses up 52 chars! We also would    */
// /* like to have framing around the file information we display (1 char on   */
// /* sides). The other constants below may also need tinkering on internat....*/
// #define INFOWIDTH        22

// #define NAMEOFFSET   10 /* Index into the info strings where the name starts */

// #define RIGHTMARGIN  (INFOWIDTH-1)  /* offset of last character -- i.e., index
//                                         of end of buffer in our case.  */
// #define RIGHTDRIVEOFFSET  RIGHTMARGIN /* location (index) of right drive letter */
// #define LEFTDRIVEOFFSET  (RIGHTMARGIN-6) /* location (index) of left drive letter */

// /* INTERNATIONALIZE HERE! */
// /* The following are the attribute characters */
// #define READCH 'r'
// #define HIDDCH 'h'
// #define SYSTCH 's'
// #define ARCHCH 'a'
//  END  - IPG (Mihindu)  Moved to DOSSHELL\INC\TEXT.H

#define FILLCH '.'

/* (XTopLeft, YTopLeft) is the relative coordinate in pwnd, where the first
	character of the first text line is placed. */

void DispFlatLeft(PENTRY node, PTREE tree, PWND pwnd, RX XTopLeft,
						RY YTopLeft, ISA isa)
{
    char fname[NAMELEN+EXTLEN+2] ;
    PENTRY parent ;
    unsigned numfiles ;
    /* ZZZ we could reuse some of these variables but readability lost!
       compiler should anyway optimize these */
    unsigned long dirsize, selsize ;
	 unsigned int DummyNumSelFiles ;

#ifndef NOCONSISTENCY
    if (node->attribs & _A_SUBDIR)
    {
		printf("**** showinfo called with a directory node\n") ;
		exit(0) ;
    }
#endif

	/* If node == NULL, we need to display 'No File' for file entry
		but directory stuff should be proper stuff! */

	if (node)
	Internal2Normal(fname, node->name) ;
	else
		strcpy(fname, NoFile) ;

    CopyNameForTextOut(FileName+NAMEOFFSET, fname) ;

    /* The compiler should optimize these indexing operations */
    FileAttr[NAMEOFFSET] = ( node && (node->attribs & _A_RDONLY) ) ? 
					  (char) READCH : (char) FILLCH ;
    FileAttr[NAMEOFFSET+1] = ( node && (node->attribs & _A_HIDDEN) ) ? 
					  (char) HIDDCH : (char) FILLCH ;
    FileAttr[NAMEOFFSET+2] = ( node && (node->attribs & _A_SYSTEM) ) ? 
					  (char) SYSTCH : (char) FILLCH ;
    FileAttr[NAMEOFFSET+3] = ( node && (node->attribs & _A_ARCH) ) ? 
					  (char) ARCHCH : (char) FILLCH ;

    SelectedHeader[RIGHTDRIVEOFFSET] = glob.SelTree[1]->root[0] ;
    
    CopyNumberForTextOut(SelectedNumber+RIGHTDRIVEOFFSET,
				  (unsigned long) glob.SelTree[1]->NumSel, TRUE) ;
	selsize = glob.SelTree[1]->SizeSel ;
    if (glob.SelTree[0] != glob.SelTree[1])
    {
		SelectedHeader[LEFTDRIVEOFFSET] = glob.SelTree[0]->root[0] ;
		CopyNumberForTextOut(SelectedNumber+LEFTDRIVEOFFSET,
					(unsigned long) glob.SelTree[0]->NumSel, TRUE) ;
		selsize += glob.SelTree[0]->SizeSel ;
    }
	CopyNumberForTextOut(SelectedSize+RIGHTDRIVEOFFSET, selsize, TRUE) ;

	if (!node)
		parent = listinfo[glob.FocusBox].files ;
	else
	{
		parent = FindParent(node) ;
		if (glob.TreeMode == TR_SYSTEM)
		{
			listinfo[0].files = parent ;
			/* The follwoing sets the line with the check mark -- so that
			 * switching back to Single/Double tree mode is fine -- i.e.,
			 * the focus will be on the right directory!
			 */
			// glob.lineselected[0] = GetIndexDir(parent, tree) ;
		}
	}

    if (!parent)
		strcpy(fname, RootName) ;  /* Root directory is the parent */
    else
		Internal2Normal(fname, parent->name) ;
    CopyNameForTextOut(DirectoryName+NAMEOFFSET, fname) ;

    numfiles = GetDirInfo(parent, tree, &dirsize, &DummyNumSelFiles) ;
	CopyNumberForTextOut(DirectorySize+RIGHTMARGIN, dirsize, TRUE) ;
    CopyNumberForTextOut(DirectoryFiles+RIGHTMARGIN,
					 (unsigned long) numfiles, TRUE) ;

    if (!tree->fdiskinfoknown)
    {
		if (GetDiskInfo(tree, tree->VolLabel, &tree->SizeTotal,
																&tree->SizeAvail))
		{
	     tree->fdiskinfoknown = TRUE ;
		}
		/* else GetDiskInfo clears out old info */
    }
    CopyNameForTextOut(DiskName+NAMEOFFSET, tree->VolLabel) ;
	CopyNumberForTextOut(DiskSize+RIGHTMARGIN, tree->SizeTotal, TRUE) ;
	CopyNumberForTextOut(DiskAvail+RIGHTMARGIN, tree->SizeAvail, TRUE) ;

    CopyNumberForTextOut(DiskFiles+RIGHTMARGIN,
					  (unsigned long) tree->Diskfilecount, TRUE);
    /* Add 1 for the root directory -- It is not part of DirCount */
    CopyNumberForTextOut(DiskDirs+RIGHTMARGIN,
					  (unsigned long) tree->DirCount+1, TRUE) ;

	FEnableMouseNest(FALSE) ;

	/* ZZZZZZZZZ put this in a loop and make these strings arrays */
    TextOut(pwnd, XTopLeft, YTopLeft, FileHeader, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+1, FileName, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+2, FileAttr, INFOWIDTH, isa) ;


    TextOut(pwnd, XTopLeft, YTopLeft+3, SelectedHeader, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+4, SelectedNumber, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+5, SelectedSize, INFOWIDTH, isa) ;

    TextOut(pwnd, XTopLeft, YTopLeft+6, DirectoryHeader, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+7, DirectoryName, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+8, DirectorySize, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+9, DirectoryFiles, INFOWIDTH, isa) ;

    TextOut(pwnd, XTopLeft, YTopLeft+10, DiskHeader, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+11, DiskName, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+12, DiskSize, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+13, DiskAvail, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+14, DiskFiles, INFOWIDTH, isa) ;
    TextOut(pwnd, XTopLeft, YTopLeft+15, DiskDirs, INFOWIDTH, isa) ;

	FEnableMouseNest(TRUE) ;

} /* DispFlatLeft */


/*  Copies the name from 'src' to 'dest'. 'dest' is assumed to have a
    NULL character at its extreme (last character in memory allocated to it)
    'src' is a null terminated name.   */
void CopyNameForTextOut(char *dest, char far *src)
{
    for ( ; *src ; src++, dest++)
	*dest = *src ;
    while(*dest && (*dest != ' '))
	/* Blank out previous garbage characters after name -- Note that we know
       that everything to the right of a blank is a blank */
	*(dest++) = ' ' ;
}

/*  Copies the number from 'val' to 'dest'. 'dest' is assumed to have a
    blank at the left location where the 'val' can't encroach. This is
    used to blank out leftover characters from previous call in buffer
    'dest' is actually a pointer to the end of the buffer.              */
char *CopyNumberForTextOut(char *dest, unsigned long val, BOOL fFillBlanks)
{
    int j ;
	unsigned long val2, val3 ;

    j = 0;
    do
    {
		if (j >= 3)
		{
			*(dest--) = DIGITSEP ;
			j = 0 ;
		}
		val3 = val/10 ;
		val2 = (val3<<1) + (val3<<3) ;  /* val2 = val3*10 */
		val2 = val - val2 ; /* val2 = val % 10 */
		*(dest--) = (char) (val2+'0');
		j++ ;
		val = val3 ;

    } while(val>0);

	if (fFillBlanks)
	{
		/* Blank out previous garbage characters before number -- Note that
		 * we know that everything to the left of a blank is a blank
		 * and that there will be at least 1 blank (the one after the ':').
		 */
		while (*dest != ' ')
			*(dest--) = ' ' ;
	}
	return (dest+1) ; /* ptr to last char added to dest */
}

#if defined(SWE)|| defined(RUS)
  void CopyNumberYST(char *dest, unsigned long val, BOOL fFillBlanks, int maxc)
   {
	 int j, p;
	unsigned long val2, val3 ;

	 j = 0;
	 p = 0;
	 do
	 {
		if (j >= 3)
		{
			*(dest--) = DIGITSEP ;
			j = 0 ;
			p++;
		}
		val3 = val/10 ;
		val2 = (val3<<1) + (val3<<3) ;   /* val2 = val3*10 */
		val2 = val - val2 ; /* val2 = val % 10 */
		*(dest--) = (char) (val2+'0');
		j++ ;
		p++;
		val = val3 ;

	 } while(val>0);

	if (fFillBlanks)
	{
		/* Blank out previous garbage characters before number -- Note that
		 * we know that everything to the left of a blank is a blank
		 * and that there will be at least 1 blank (the one after the ':').
		 * 
		 * Note from YST (16/08/91 MS IPG Ireland). 
		 * I changed blank to ':', because Sweden, Russian and etc. uses
		 * blank as thousand separator and if previous values was more
		 * then 1 000 then we had garbage on the screen. 
		 * If your country use ':' as separator, change this again.
		 *                                     (  YST)
		 */
		while ((*dest !=':') && (p < maxc)) {
			*(dest--) = ' ' ;
			p++;
		}
	}
   }
#endif

/* function reads the appropriate storage media (based on tree) and passes
   back by reference the disk label, disk size and disk avail. The status it
   returns indicates whether the information was succesfully retrieved.
   Problems could occur for removable media. Returns true for success
   else false */
int GetDiskInfo(PTREE tree, char far *disklabel, unsigned long far *disksize,
															unsigned long far *diskavail)
{
	char *pch ;
	struct diskfree_t d_info ;
	struct find_t f_info ;
	char rootpath[10] ; // will store path of the form C:\*.*
	int ret, action ;
	char *tempcaption ;

	do 
	{
		action = ACT_OK ;

	/* Map name of the root directory of tree. Map 'A' to 1, etc. */
	if (! (ret = _dos_getdiskfree(*(tree->root) - 'A' + 1, &d_info)))
	{
			/* call was succesful */
			*disksize = (unsigned long) d_info.total_clusters *
				d_info.sectors_per_cluster * d_info.bytes_per_sector ;
			*diskavail = (unsigned long) d_info.avail_clusters *
				d_info.sectors_per_cluster * d_info.bytes_per_sector ;
	}
	else
	{       /* Unsuccesful call */
			if (!gfFMBeingInited)
			{
				tempcaption = gpszFileOpCaption ;
				gpszFileOpCaption = szErrorCaption ;
				action = DOSErrorBox(szDiskInfo, errno, HELP_ERRDISKFREE) ;
				gpszFileOpCaption = tempcaption ;
			}
	}
	} while (action == ACT_RETRY) ;

	if (ret)
	{
		/* _dos_getdiskfree failed, We send back garbage values! */
		*disksize = *diskavail = 0 ;
		strfcpy(disklabel, NoLabel) ;
		return FALSE ;
	}

	/* get the disk's volume Label now */
	/* ZZZ Do I retry in a loop on error like unformatted disk, etc? */

	strfcpy(rootpath, tree->root) ;
	strcpy(rootpath+3, szStarDotStar) ;

	if (!shell_findfirst(rootpath, _A_VOLID, &f_info))
	{
		/* label found succesfully. If '.' is present remove it ! */
		for (pch = f_info.name ; *pch ; pch++)
			if (*pch != '.')
				*(disklabel++) = *pch ;
		*disklabel = '\0' ;
	}
	else
		strfcpy(disklabel, NoLabel) ;

	return TRUE ; /* Succesful return */
} /* GetDiskInfo */
