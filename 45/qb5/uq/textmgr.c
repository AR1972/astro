/***	TXTMGR.C - Text manager for multi-line editing
*
* GLOBAL
*	LoadBuf			Load a file into a buffer
*	FreeBuf			Free a buffer
*	NewBuf			Create a new (blank) buffer
*
*	S_LinesInBuf		LinesInBuf() routine
*	S_CbGetLineBuf		CbGetLineBuf() routine
*	S_ReplaceLineBuf	ReplaceLineBuf() routine
*	S_InsertLineBuf 	InsertLineBuf() routine
*	S_InsertBufInBuf	InsertBufInBuf() routine
*	S_DeleteLinesBuf	DeleteLinesBuf() routine
*
* LOCAL
*	CbFindLineBuf		Find a specified line in a buffer
*	InsSpace		Insert space into a buffer
*	GrowBuf			Increase the size of a buffer
*
* DESCRIPTION
*
*	This module manages the program source and contains all functions
*	that operate directly on the source.
*
*	The source buffer is maintained in a single Global Heap Segment. It
*	may be larger than 64k. Because of this, linear addresses (32 bit) are
*	used. The linear addresses are converted to segmented addresses when
*	actually accessing the buffer.
*
*	The format of the source buffer is:
*
*		+-----------+-----//-----+-----------+-----//-----+
*		| WORD len0 | len0 BYTES | WORD len1 | len1 BYTES |
*		+-----------+-----//-----+-----------+-----//-----+
*
*				    +------+-----//-----+---------+
*				... | lenN | lenN BYTES | WORD -1 |
*				    +------+-----//-----+---------+
*
*/

/*------------------------- Include Files -------------------------*/

#include <version.h>
/* Next, include COW's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>
#include <uiext.h>

#include <..\hd\edit.h>

#ifndef UIINT_H
#include <uiint.h>
#endif

#ifndef HEAP_H
#include <heap.h>
#endif

#ifndef QBIMSGS_H
#include <qbimsgs.h>
#endif

extern char	szNull[];
       char	*pszNull = szNull;

/*-------------------------- Local Macros --------------------------*/

/* Returns TRUE if the fhd is allocated, zero if it is un-allocated */

#define ALLOCEDFHD(fhd) (fhd.hData != 0)

/* Dereferences a fhd and returns a far pointer to the data	    */

#define MAKELONG(l, h)	((long)(((unsigned)(l)) | ((unsigned long)((unsigned)(h))) << 16))
#define DEREFFHD(fhd)	((char far *) MAKELONG(0, GETSEG(fhd.hData)))

/*----------------------- Forward References -----------------------*/

LOCAL	WORD	NEAR	PASCAL	CbFindLineBuf(PBUFINFO, DWORD, WORD, DWORD *);
LOCAL	DWORD	NEAR	PASCAL	InsSpace(PBUFINFO, WORD, DWORD);
LOCAL	BOOL	NEAR	PASCAL	GrowBuf(PBUFINFO, DWORD);

/*------------------- Global and Local Variables -------------------*/

/* The EDITMGR requires a number of routines to get its work done; it
** does a call back into the application (us) to update the text buffer.
** Such routines as InsertLineBuf(), DeleteLinesBuf(), etc. are among
** these required call-back routines.
*/

/* These are the buffers that acually manage the data and the information
** that is associated with the data
*/

LOCAL	BUFINFO rgbufinfo[CBUFINFO];

/*------------------ S_LinesInBuf() ------------------*/

/***	LinesInBuf - How many lines in VAP's buffer?
*
* SYNOPSIS
*		cln = LinesInBuf(idVap)
*
* ENTRY
*		idVap			ID of VAP which owns buffer
*
* RETURNS
*		Number of lines currently in buffer
*
* DESCRIPTION
*		Examines the .cln field in the BUFINFO structure to determine
*		the number of lines currently in the buffer.
*
* NOTES
*		Required by EDITMGR
*
*/

GLOBAL WORD FAR PASCAL
S_LinesInBuf(PBUFINFO pbufinfo)
{
	DbChkPBufInfo(pbufinfo);
	DbAssert(ALLOCEDFHD(pbufinfo->fhd));

	return(pbufinfo->cln - 1);
}


/*--------------------------- FreeBuf() ---------------------------*/

/***	FreeBuf - Free a buffer
*
* SYNOPSIS
*		FreeBuf(pbufinfo)
*
* ENTRY
*		pbufinfo		pointer to BUFINFO of buffer
*					to deallocate
*
* RETURNS
*		None
*
* DESCRIPTION
*		Release the memory associated with this buffer.
*
* NOTES
*/

GLOBAL VOID FAR PASCAL
FreeBuf(PBUFINFO pbufinfo)
{
	DbChkPBufInfo(pbufinfo);

	if (ALLOCEDFHD(pbufinfo->fhd))
	{
		FhdDealloc(&pbufinfo->fhd);
	}
}

/*------------------------ CbFindLineBuf() ------------------------*/

/***	CbFindLineBuf() - Find a line in a buffer
*
* SYNOPSIS
*		cb = CbFindLineBuf(pbufinfo, obBase, oln, pobLine)
*
* ENTRY
*		pbufinfo		Pointer to a BUFINFO structure
*		obBase		32-bit pointer to start of buffer
*		oln			Line number to find
*		pobLine		Pointer to DWORD (return value)
*
* RETURNS
*		Number of bytes in line found
*		Also returns 32-bit pointer to line found through pobLine
*
*		Note that the "pointer" points to the actual text, NOT the WORD
*		length preceding the text.
*
* DESCRIPTION
*		Basically a higher-level routine for CbFindLine(); most of
*		the work is done there.  Uses and updates the cache (.olnCache
*		and .obCache fields) from the BUFINFO structure.
*
*/

LOCAL WORD NEAR PASCAL
CbFindLineBuf(PBUFINFO pbufinfo, DWORD obBase, WORD oln, DWORD *pobLine)
{
	WORD		cb;
	WORD		olnCache;

	*pobLine = obBase;

	/* If the requested line is farther along in the file than the
	** line last cached, we can save a lot of time by skipping to
	** the cached (known) line.
	*/

	olnCache = pbufinfo->olnCache;
	pbufinfo->olnCache = oln;

	if (oln >= olnCache)
	{
		*pobLine += pbufinfo->obCache;
		oln -= olnCache;
	}

	/* Find the line in the buffer.  pobLine as passed in to CbFindLine()
	** "points to" the WORD length preceding some line; on exit, it will
	** "point to" the actual text of the line found.
	*/

	cb = CbFindLine(pobLine, oln);

	/* Update the cache information for this BUFINFO.
	*/

	pbufinfo->obCache = *pobLine - sizeof(WORD) - obBase;

	return(cb);
}

/*--------------------------- InsSpace() ---------------------------*/

/***	InsSpace - Insert space into a buffer
*
* SYNOPSIS
*		obNewSpace = InsSpace(pbufinfo, oln, cbAdd)
*
* ENTRY
*		pbufinfo		Pointer to a BUFINFO structure
*		oln			Line number within buffer
*		cbAdd			Number of bytes to add
*
* RETURNS
*		32-bit pointer to start of vacated region  (relative
*		to begining of memory block)
*
*		-OR-
*
*		0L if the buffer could not accomodate the additional space
*
* DESCRIPTION
*		This routine will make space in the source buffer for a line of text.
*		The space will be made BEFORE oln. The source is moved down from
*		oln to the end of source.
*
* NOTES
*/		

LOCAL DWORD NEAR PASCAL
InsSpace(PBUFINFO pbufinfo, WORD oln, DWORD cbAdd)
{
	DWORD		obSrc;
	DWORD		obBase;
	DWORD		cbMove;


	/* If adding the requested number of bytes would grow the buffer
	** beyond its current maximum size, try to make the buffer larger.
	** If this fails, return 0L.
	*/

	if (pbufinfo->obNext + cbAdd + 1 > pbufinfo->cb)
	{
		if (!GrowBuf(pbufinfo, cbAdd))
			return(0L);
	}

	/* Lock the buffer down and find the offset (32-bit pointer) of
	** the indicated line.
	*/

	DbHeapMoveOff();

	obBase = LinearAddr(DEREFFHD(pbufinfo->fhd));

	if (oln < pbufinfo->cln)				/* If within range... */
	{
		CbFindLineBuf(pbufinfo, obBase, oln, &obSrc);
		obSrc -= sizeof(WORD);				/* Move back to WORD length */
		cbMove = pbufinfo->obNext - (obSrc - obBase) + 1;

		BigMoveDown(obSrc + cbAdd, obSrc, cbMove);
	}
	else											/* Add to end of file */
		obSrc = obBase + pbufinfo->obNext;

	pbufinfo->obNext += cbAdd;
	pbufinfo->olnCache = 0xFFFF;			/* Destroy cache information */

	DbHeapMoveOn();

	return(obSrc - obBase);
}

/*--------------------------- GrowBuf() ---------------------------*/

/***	GrowBuf - Increase the size of a buffer
*
* SYNOPSIS
*		fStatus = GrowBuf(pbufinfo, cbAdd)
*
* ENTRY
*		pbufinfo		Pointer to BUFINFO
*		cbAdd			Number of bytes to increase size by
*
* RETURNS
*		TRUE if buffer could be grown; FALSE if not
*
* DESCRIPTION
*		Call the SBMGR to reallocate the buffer, rounding the new size
*		to be an even multiple of CBSRCBLK.
*
* NOTES
*/

LOCAL BOOL NEAR PASCAL
GrowBuf(PBUFINFO pbufinfo, DWORD cbAdd)
{
	DWORD		cbNeeded;
	DWORD		cbRequested;

	cbRequested = cbNeeded = pbufinfo->cb + cbAdd + 1;


	cbRequested += CBSRCBLK - 1;
	cbRequested &= ~(CBSRCBLK - 1);

	// Try the alloc with the extra space
	if (FhdRealloc(&(pbufinfo->fhd), cbRequested))
	{
		pbufinfo->cb = cbRequested;
		return(TRUE);
	}

	// Ok, now try it with just the amount that we need
	if (FhdRealloc(&(pbufinfo->fhd), cbNeeded)) {
		pbufinfo->cb = cbNeeded;
		return(TRUE);
	}

	// Unable to get the needed amount of memory, fail
	SetUiErrOm();
	return(FALSE);
}

/*---------------- S_CbGetLineBuf() ----------------*/

/***	S_CbGetLineBuf - Fetch a line of text from a buffer
*
* SYNOPSIS
*		cb = S_CbGetLineBuf(idVap, oln, cbMax, pBuf)
*
* ENTRY
*		idVap			ID of VAP whose buffer this concerns
*		oln			Line number to fetch
*		cbMax			Maximum number of characters to place in buffer
*		pBuf			Buffer to place text in
*
* RETURNS
*		Number of characters placed in buffer (excluding terminating zero)
*
* DESCRIPTION
*		This routine gets the specified line of text even it is the current
*		line. This is used when the ld buffer has been modified and the line
*		needs to be refreshed.
*
* NOTES
*		Required by EDITMGR
*
*		M00SWAP NPCORE
*/

GLOBAL WORD FAR PASCAL
S_CbGetLineBuf(PBUFINFO pbufinfo, WORD oln, WORD cbMax, char *pBuf)
{
	REG2	WORD	cb;
	DWORD		obSrc;

	DbChkPBufInfo(pbufinfo);
	DbAssert(ALLOCEDFHD(pbufinfo->fhd));

	/* Ignore first line of buffer -- it contains the filename.
	*/

	oln++;

	if (oln < pbufinfo->cln)
	{
		DbHeapMoveOff();

		cb = CbFindLineBuf(pbufinfo, LinearAddr(DEREFFHD(pbufinfo->fhd)), oln, &obSrc);
		cb = min(cb, cbMax);
		fmemcpy((char far *)pBuf, SegAddr(obSrc), cb);

		DbHeapMoveOn();
	}
	else
		cb = 0;
	
	pBuf[cb] = '\0';
	
	return(cb);
}


/*-------------- S_ReplaceLineBuf() --------------*/

/***	S_ReplaceLineBuf - Replace a line in a buffer
*
* SYNOPSIS
*		fSuccess = S_ReplaceLineBuf(idVap, oln, cb, pBuf)
*
* ENTRY
*		idVap			ID of VAP whose buffer this concerns
*		oln			Line to replace
*		cb				Size of replacement text
*		pBuf			Point to Pointer to replacement text
*
* RETURNS
*		TRUE if successful; FALSE if not
*
* DESCRIPTION
*		Replace the indicated line in the buffer. The double
*		indirection on the buffer allows us to point to a
*		movable heap entry and still cause heap movement.
*
* NOTES
*		Required by EDITMGR
*/


GLOBAL BOOL NEAR PASCAL
S_ReplaceLineBuf(PBUFINFO pbufinfo, WORD oln, WORD cb, char **pBuf)
{

	DbChkPBufInfo(pbufinfo);
	DbAssert(ALLOCEDFHD(pbufinfo->fhd));


	if (S_InsertLineBuf(pbufinfo, oln + 1, cb, pBuf, TRUE))
	{
		S_DeleteLinesBuf(pbufinfo, oln, 1);
	}

	return(TRUE);
}

/*--------------- S_InsertLineBuf() ---------------*/

/***	S_InsertLineBuf - Insert a line in a buffer
*
* SYNOPSIS
*		fSuccess = S_InsertLineBuf(idVap, oln, cb, pBuf, fOverCommit)
*
* ENTRY
*		pBuf			Buffer to insert into
*		oln			Line to insert text (AFTER)
*		cb			Size of text to insert
*		pBuf			Pointer to pointer to Text to insert
*		fOverCommit		Check for too many lines or not.
*
* RETURNS
*		TRUE if successful; FALSE if not
*
* DESCRIPTION
*		First remove any trailing spaces from the line, then convert
*		any leading spaces to tabs, if possible.  Then call InsSpace()
*		to insert space in the buffer, and copy the line to the vacated
*		region.
*
*		if fOverCommit is true, then we will not check for
*		inserting too many lines into the buffer.  This allows
*		S_ReplaceLineBuf to work properly, as it adds a line
*		then deletes a line.  If we did not allow it too add more
*		lines than the maximum, we would not be able to modify
*		a full text table.
*
*		The double indirection on the buffer allows us to point to a
*		movable heap entry and still cause heap movement.
*
* NOTES
*		Required by EDITMGR
*
*/

GLOBAL BOOL NEAR PASCAL
S_InsertLineBuf(PBUFINFO pbufinfo, WORD oln, WORD cb, char **pBuf, BOOL fOverCommit)
{
	DWORD			obLine;
	char far		*fpData;
	WORD			cbSave = cb;


	DbChkPBufInfo(pbufinfo);
	DbAssert(ALLOCEDFHD(pbufinfo->fhd));

	if (pbufinfo->cln >= CLNMAX && !fOverCommit) {
	    SetUiErr(MSG_DocTooLarge);
	    return (FALSE);
	}

	/* Ignore first line of buffer - it contains the filename.
	*/

	oln++;

	obLine = InsSpace(pbufinfo, oln, (DWORD)(cb + sizeof(WORD)));

	if (obLine)
	{
		DbHeapMoveOff();

		fpData = SegAddr(obLine + LinearAddr(DEREFFHD(pbufinfo->fhd)));
		*(WORD far *)fpData = cb;

		if (cb != 0)
		{
			fmemcpy(fpData+(long)sizeof(WORD), (char far *)*pBuf, cb);

		}

		pbufinfo->cln++;

		DbHeapMoveOn();
	}

	return(obLine != 0L);
}

/*-------------- S_InsertBufInBuf() --------------*/

/***	S_InsertBufInBuf - Insert a buffer into another buffer
*
* SYNOPSIS
*		S_InsertBufInBuf(idVapDst, oln, idVapSrc)
*
* ENTRY
*		idVapDst		ID of VAP whose buffer is to serve as destination
*		oln			Line number in destination buffer
*		idVapSrc		ID of VAP whose buffer is to serve as source
*
* RETURNS
*		None
*
* DESCRIPTION
*		Insert a buffer within another buffer
*
* NOTES
*		Required by EDITMGR
*
*/

GLOBAL VOID NEAR PASCAL
S_InsertBufInBuf(PBUFINFO pbufinfoDst, WORD oln, PBUFINFO pbufinfoSrc)
{
	DWORD		cbAdd;
	WORD		cbBufName;
	DWORD		obDst;
	DWORD		obSrc;


	DbChkPBufInfo(pbufinfoSrc);
	DbAssert(ALLOCEDFHD(pbufinfoSrc->fhd));
	DbChkPBufInfo(pbufinfoDst);
	DbAssert(ALLOCEDFHD(pbufinfoDst->fhd));


	if (pbufinfoSrc->cln + pbufinfoDst->cln >= CLNMAX) {
	    SetUiErr(MSG_DocTooLarge);
	    return;
	}

	/* Ignore first line of buffer - it contains the filename.
	*/

	oln++;

	cbBufName = *(WORD far *) DEREFFHD(pbufinfoSrc->fhd) + sizeof(WORD);

	cbAdd = pbufinfoSrc->obNext - cbBufName; /* - sizeof(WORD); */

	obDst = InsSpace(pbufinfoDst, oln, cbAdd);

	if (obDst)
	{
		DbHeapMoveOff();

		obDst += LinearAddr(DEREFFHD(pbufinfoDst->fhd));
		obSrc = LinearAddr(DEREFFHD(pbufinfoSrc->fhd)) + cbBufName;

		BigMoveUp( obDst, obSrc, cbAdd );

		pbufinfoDst->cln += pbufinfoSrc->cln - 1;

		DbHeapMoveOn();
	}

	return;										/* M00BUG */
}

/*--------------  S_DeleteLinesBuf() --------------*/

/***	S_DeleteLinesBuf - Delete a range of lines from a buffer
*
* SYNOPSIS
*		S_DeleteLinesBuf(pbufinfo, oln, cln)
*
* ENTRY
*		pbufinfo		handle to buffer
*		oln			Starting line number
*		cln			Number of lines to delete
*
* RETURNS
*		None
*
* DESCRIPTION
*		Delete lines oln..oln+cln-1 from the indicated buffer
*
* NOTES
*		Required by EDITMGR
*
*/

GLOBAL VOID NEAR PASCAL
S_DeleteLinesBuf(PBUFINFO pbufinfo, WORD oln, WORD cln)
{
	DWORD			obSrc, obDst;
	DWORD			obBase;
	DWORD			cbMove;
	WORD			clnMax;

	DbChkPBufInfo(pbufinfo);
	DbAssert(ALLOCEDFHD(pbufinfo->fhd));

	/* Ignore first line of buffer - it contains the filename.
	*/

	oln++;

	clnMax = pbufinfo->cln - oln;

	if (cln >= clnMax)
		cln = clnMax;

	if (pbufinfo->cln == cln+1)
	{
		/* Special case:
		**
		** Deleting all the lines in the buffer.
		** Insert a blank line so that there will be a blank
		** line left in the buffer after deleting the lines.
		*/

		S_InsertLineBuf(pbufinfo, pbufinfo->cln, 0, &pszNull, TRUE);
	}

	DbHeapMoveOff();

	obBase = LinearAddr(DEREFFHD(pbufinfo->fhd));

	CbFindLineBuf(pbufinfo, obBase, oln, &obSrc);

	obSrc -= sizeof(WORD);
	CbFindLineBuf(pbufinfo, obBase, oln+cln, &obDst);
	obDst -= sizeof(WORD);

	cbMove = pbufinfo->obNext - (obDst - obBase) + 1;
	BigMoveUp( obSrc, obDst, cbMove );

	pbufinfo->obNext -= obDst - obSrc;

	pbufinfo->cln -= cln;
	pbufinfo->olnCache = 0xFFFF;		/* Trash the cache */

	DbHeapMoveOn();
}

/*---------------------------- NewBuf() ----------------------------*/

/***	NewBuf - Create a new (blank) buffer
*
* SYNOPSIS
*		fSuccess = NewBuf(pbufinfo)
*
* ENTRY
*		pbufinfo	pointer to buffer info block which is to
*				own new buffer
*
* RETURNS
*		PBUFINFO for buffer if successful; FALSE (0) if not
*
* DESCRIPTION
*		then adds a blank line to make sure the EDITMGR doesn't get
*		confused.
*/

GLOBAL PBUFINFO FAR PASCAL
NewBuf()
{
	PBUFINFO pbufinfo;
	int iBufInfo;

	pbufinfo = NULL;

	/* locate a buffer that is not already in use */

	for (iBufInfo = 0; (iBufInfo < CBUFINFO) && !pbufinfo; iBufInfo++)
	    if (!ALLOCEDFHD(rgbufinfo[iBufInfo].fhd))
		pbufinfo = &rgbufinfo[iBufInfo];

	DbAssert(pbufinfo != NULL);

	if (!FhdAlloc(&pbufinfo->fhd, (DWORD)CBSRCBLK))
	{
		SetUiErrOm();
		return(FALSE);
	}


	pbufinfo->cln = 0;
	pbufinfo->cb = CBSRCBLK;
	pbufinfo->obNext = 0;
	pbufinfo->olnCache = 0xFFFF;			/* No line cached yet... */

	if (AppendLineBuf(pbufinfo, &pszNull))
	{
		FreeBuf(pbufinfo);
		return (FALSE);
	}

	return (pbufinfo);

}


/*** AppendLineBuf - append a line to the end of a buffer
*
* SYNOPSIS
*	fSuccess = AppendLineBuf(pbufinfo, pBuf)
*
* ENTRY:
*	pbufinfo	Pointer to buffer to update
*	pBuf		Indirect Pointer to the source line (tab expanded)
*
* EXIT:
*	Returns 0 on success, -1 on failure
*
* NOTES:
*	This routine can cause heap movement.
*
*	This was written for speed, not size. If the added speed is not
*	is not needed for ASCII load, then call S_InsertLineBuf to do all
*	of the work.
*
*	The reason that we take a char ** is so that we can take a pointer
*	to a movable heap item.  This allows us to handle it like an LMEM
*	entry.
*/

GLOBAL WORD FAR PASCAL
AppendLineBuf(PBUFINFO pbufinfo, char **pBuf)
{
	WORD	cbBuf = CbSzUi(*pBuf);
	DWORD	cbNeeded = cbBuf + sizeof(WORD);
	DWORD	cbAvail = pbufinfo->cb - pbufinfo->obNext;
	DWORD	obLine;
	WORD far *fpLineLen;
	char far *fpLine;

	DbChkPBufInfo(pbufinfo);
	DbAssert(ALLOCEDFHD(pbufinfo->fhd));

	if (pbufinfo->cln >= CLNMAX) {
	    SetUiErr(MSG_DocTooLarge);
	    return (UNDEFINED);
	}

	if (cbAvail < cbNeeded)
	    if (!GrowBuf(pbufinfo, cbNeeded - cbAvail))
		return (UNDEFINED);

	obLine = LinearAddr(DEREFFHD(pbufinfo->fhd)) + pbufinfo->obNext;
	fpLineLen = (WORD far *)SegAddr(obLine);
	fpLine = SegAddr(obLine + sizeof(WORD));

	*fpLineLen = cbBuf;
	fmemcpy(fpLine, (char far *)*pBuf, cbBuf);

	pbufinfo->obNext += cbNeeded;
	pbufinfo->cln ++;

	return (0);
}


/*** CompressBufs - Compress Document buffers to minimum size
*
* SYNOPSIS
*	CompressBufs()
*
* ENTRY:
*	None.
*
* EXIT:
*	None.
*
* NOTES:
*	This routine can cause heap movement.
*
*	We keep some slack in the document buffers to allow for fast
*	insertion of text.  However, if we are running out of memory
*	(or just run out), we need to get rid of the slack.  This routine
*	will go through all allocated document buffers, removing any slack.
*/
GLOBAL VOID FAR PASCAL CompressBufs()
{
    int i = 0;

    for (i = 0; i < CBUFINFO; i++)
	if (ALLOCEDFHD(rgbufinfo[i].fhd))
	    if (FhdRealloc(&rgbufinfo[i].fhd, rgbufinfo[i].obNext))
		rgbufinfo[i].cb = rgbufinfo[i].obNext;

}
