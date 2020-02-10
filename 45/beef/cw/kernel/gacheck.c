/*
	COW : Character Oriented Windows

	gacheck.c : global heap check procedure (DEBUG ONLY)
*/

#define COW
#include <cow.h>

#include <kmem.h>
#include <cwdebug.h>

#ifdef DEBUG	/* entire file for debug only */
#include "gadebug.h"

extern LPGLOBALINFO FAR PASCAL LpGlobalHeap(void);   /* where global heap is */

/* Two interfaces to global heap checking */
DWORD NEAR PASCAL CheckGlobalHeap(void);


BOOL FAR PASCAL
FCheckGlobalHeap(lpckh)
/*
  -- clean interface to check global heap
  -- note : DS unimportant
*/
CKH FAR *lpckh;
	{
	WORD cblk;
	LPGLOBALINFO lpGlobalHeap;
	WORD psArena;

	if (!(lpGlobalHeap = LpGlobalHeap()) ||
	    CheckGlobalHeap() != 0L)
		return FALSE;

	if (!lpckh)
		return TRUE;	/* no statistics required */

	/* Initialize structure */
	lpckh->cblkTotal = lpckh->cblkCode = lpckh->cblkData = lpckh->cblkFree = 0;
	lpckh->cbTotal = lpckh->cbCode = lpckh->cbData = lpckh->cbFree = 0L;

	cblk = lpGlobalHeap->hi_count;
	psArena = lpGlobalHeap->hi_first;
	while (cblk--)
		{
		LPGLOBALARENA pa = lpGlobalArena(psArena);
		DWORD cb = (DWORD) pa->ga_size;
		cb += cb;
		cb += cb;
		cb += cb;
		cb += cb;		/* sleazy way to get *16 with no
						LIBH routine */
		/* accumulate info */

		lpckh->cblkTotal++;
		lpckh->cbTotal += cb;
		if (!pa->ga_owner)
			{
			/* no owner => free */
			lpckh->cblkFree++;
			lpckh->cbFree += cb;
			}
		else if (pa->ga_owner < 256)
			{
			/* code segment */
			lpckh->cblkCode++;
			lpckh->cbCode += cb;
			}
		else if (pa->ga_owner != (WORD) -1)
			{
			/* data */
			lpckh->cblkData++;
			lpckh->cbData += cb;
			}
		/* else : special system object */
		psArena = pa->ga_next;
		}
	return TRUE;	/* Heap ok */
	}



STATIC DWORD NEAR PASCAL
CheckGlobalHeap()
/*
  -- check global heap : NOTE DS may be pointing to trash !!!!
*/
	{
	WORD cnt, maxcnt, nrefhandles, nhandles;
	WORD nfreehandles, nusedhandles, ndishandles;
	WORD p, pbadsig, pprev;
	LPGLOBALINFO lpGlobalHeap;
	LPGLOBALARENA pa;
	LPHANDLEENTRY ph;
	LPHANDLETABLE pt;
	WORD result = 0;

	if (!(lpGlobalHeap = LpGlobalHeap()))
		return 0;

/*
	if (!lpGlobalHeap->hi_check)
		return 0;
*/
	cnt = lpGlobalHeap->hi_count;
	pprev = (WORD) -1;
	pbadsig = 0;
	p = lpGlobalHeap->hi_first;
	while (cnt--)
		{
		pa = lpGlobalArena( p );

		/* check to see if within heap bounds */
		if (p < lpGlobalHeap->hi_first || p > lpGlobalHeap->hi_last)
			{
			if (!pbadsig)
				pbadsig = pprev;
				break;
			}

		/* check next link */
		if (pa->ga_next > p)
			{
			if (!pbadsig)
				{
                                if ((pa->ga_sig != GA_SIGNATURE)
                                    && (pa->ga_sig != GA_HOLESIG))
					pbadsig = pprev;
				else if ((pa->ga_size || pprev != (WORD) -1) &&
				    pa->ga_next != (p + pa->ga_size + 1))
					pbadsig = p;
				}

			pprev = p;
			p = pa->ga_next;
			}
		else
			{
			pprev = p;
			p = pa->ga_next;
			pa = lpGlobalArena( p );
			if (!pbadsig && pa->ga_sig != GA_ENDSIG)
				pbadsig = pprev;
			break;
			}
		}

	if (pbadsig || cnt || p != lpGlobalHeap->hi_last)
		result |= 1;	      /* Forward links invalid */

	cnt = lpGlobalHeap->hi_count;
	pprev = (WORD) -1;
	p = lpGlobalHeap->hi_last;
	while (cnt--)
		{
		if (p < lpGlobalHeap->hi_first || p > lpGlobalHeap->hi_last)
			{
			if (!pbadsig)
				pbadsig = pprev;
			break;
			}
		pa = lpGlobalArena( p );
		pprev = p;
		p = pa->ga_prev;
		if (p >= pprev)
			break;
		}

	if (pbadsig || cnt || p != lpGlobalHeap->hi_first)
		result |= 2;	    /* Backward links invalid */

	if (result || pbadsig)
		goto checkdone;     /* Bail out if invalid heap links */

	cnt = lpGlobalHeap->hi_count;
	p = lpGlobalHeap->hi_first;
	nrefhandles = 0;
	while (cnt--)
		{
		pa = lpGlobalArena( p );
		if (pa->ga_owner)
			if (pa->ga_handle)
				{
				ph = lpHandleEntry( pa->ga_handle );
				if ( ((LPFREEHANDLEENTRY)ph)->he_free == HE_FREEHANDLE ) {
					result |= 4;	/* Block points to free handle */
					if (!pbadsig) pbadsig = p;
					}
				else
				if ((WORD)pa->ga_handle == (WORD)0xFFFF ||
				    ph->he_address != p+1)
					{
					result |= 8;	/* Block points to handle but not vice versa */
					if (!pbadsig)
						pbadsig = p;
					}
				else
					nrefhandles++;
				}

		p = pa->ga_next;
		}

	nhandles = 0;
	ndishandles = 0;
	nusedhandles = 0;
	nfreehandles = 0;
	p = (WORD)lpGlobalHeap->hi_htable;
	maxcnt = nrefhandles;	/* Limit the search */
	while (maxcnt-- && p)
		{
		pt = lpHandleTable( p );
		ph = &pt->ht_entry[ 0 ];
		cnt = pt->ht_count;
		nhandles += cnt;
		while (cnt--)
			{
			if ( ((LPFREEHANDLEENTRY)ph)->he_free == HE_FREEHANDLE )
				nfreehandles++;
			else
			if (ph->he_flags & HE_DISCARDED)
				ndishandles++;
			else
				nusedhandles++;

			ph++;
			}

		p = (WORD)ph->he_address;
		}

	if (nrefhandles != nusedhandles)
		result |= 16;	/* allocated handles dont match used handles */

	if (nhandles != (nfreehandles + nusedhandles + ndishandles))
		result |= 32;	/* total number of handles dont add up */

	cnt = nfreehandles;
	p = (WORD)lpGlobalHeap->hi_hfree;
	while (cnt--)
		{
		ph = lpHandleEntry( p );
		p = (WORD)ph->he_address;
		}

	if (p)
		result |= 64;	/* total number of free handles dont add up */

checkdone:
	return (((DWORD)pbadsig << 16) | (DWORD)result);
	}

#endif /* DEBUG (entire file) */
