/*
	COW : Character Oriented Windows

	sdmcab.c : SDM Cab functions

	NOTE : critical assumption that a HANDLE is a ** near pointer

*/

#define COW
#include <cow.h>

#define SDM

#include <sdmver.h>

#include <kmem.h>
#include <usdm.h>
#include <udialog.h>

#include "dialog.h"
#include "util.h"

#include "sdm.h"

#ifdef KANJI
#include <ukanji.h>
#endif


PUBLIC VOID FARPUBLIC
InitCab(hcab, cabi)
/*
  -- initialize a CAB that is assumed to contain trash
  -- LOBYTE(cabi) = cwData, HIBYTE(cabi) = cagHandle
*/
HCAB hcab;
WORD cabi;	/* cab init info */
	{
	REGISTER CABX *pcabx;
	WORD cagHandle = HIBYTE(cabi);

	Assert(cagHandle <= LOBYTE(cabi));
	pcabx = *hcab;
	pcabx->cabh.cwData = LOBYTE(cabi);
	pcabx->cabh.cagHandle = cagHandle;
	FillBuf((char *) pcabx->rgh, 0, cagHandle * sizeof(WORD));
		/* make all handles NULL */
	}



PUBLIC VOID FARPUBLIC
ReinitCab(hcab, cabi)
/*
  -- re-initialize a CAB that was previously a CAB
  -- LOBYTE(cabi) = cwData, HIBYTE(cabi) = cagHandle
*/
HCAB hcab;
WORD cabi;	/* cab init info */
	{
	FreeCabData(hcab);
	InitCab(hcab, cabi);
	}



PUBLIC HCAB FARPUBLIC
HcabAlloc(cabi)
/*
  -- allocate a new cab
  -- initialize cab fields
  -- return hcab or NULL if out of memory
  -- LOBYTE(cabi) = cwData, HIBYTE(cabi) = cagHandle
*/
WORD cabi;	/* cab init info */
	{
	StartPublic();

	HCABX hcabx;
	WORD cb = cbCabOverhead + LOBYTE(cabi) * sizeof(WORD);

	if ((hcabx = (HCABX) HeapAlloc(cb)) == NULL)
		{
		OutOfMemory();
		ReturnPublic(NULL, HCAB);
		}
	InitCab(hcabx, cabi);
	ReturnPublic(hcabx, HCAB);
	}



PUBLIC VOID FARPUBLIC
FreeCab(hcab)
/*
  -- Free the cab and any handles it contains
*/
HCAB hcab;
	{
	FreeCabData(hcab);
	HeapFree(hcab);
	}



PUBLIC VOID FARPUBLIC
FreeCabData(hcab)
/*
  -- Free any handles associated with hcab
*/
HCAB hcab;
	{
	WORD cag = PcabxOfCab(hcab)->cabh.cagHandle;
	REGISTER HANDLE *ph = ((CABX *) (*hcab))->rgh;

	while (cag--)
		{
		if (*ph != NULL)
			{
			HeapFree(*ph);
			*ph = NULL;
			}
		ph++;
		}
	}


PUBLIC VOID FARPUBLIC
RgbToCab(hcab, rgbSrc, cb, iag)
/*
  -- store cb bytes from rgb into memory allocated from heap
  -- store handle in hcab at ag specified by iag
  -- if previous contained data -- free it
*/
HCAB hcab;
BYTE *rgbSrc;
WORD cb;
WORD iag;
	{
	StartPublic();
	REGISTER HANDLE *ph;
	REGISTER BYTE **ppb;

	Assert(hcab != NULL);
	Assert(iag <= PcabxOfCab(hcab)->cabh.cagHandle);

 	ph = &PcabxOfCab(hcab)->rgh[iag];
	if (*ph != NULL)
		{
		HeapFree(*ph);
		*ph = NULL;
		}

	if ((ppb = HeapAlloc(cb)) == NULL)
		{
		OutOfMemory();
		return;		/* no error value
				   -- hopefully OutOfMemory() will handle it */
		}

	/* ph is potentially bogus */
 	PcabxOfCab(hcab)->rgh[iag] = (HANDLE) ppb;
	bltbyte(rgbSrc, *ppb, cb);
	StopPublic();
	}



PUBLIC BYTE * FARPUBLIC
RgbFromCab(hcab, rgbDest, cb, iag)
HCAB hcab;
BYTE *rgbDest;
WORD cb;
WORD iag;
	{
	StartPublic();
	REGISTER BYTE **ppb = PpvFromCab(hcab, iag);

	AssertSz(ppb != NULL, "RgbFromCab : NULL handle");
	ReturnPublic(bltbyte(*ppb, rgbDest, cb), BYTE *);
	}



#ifndef SDM_ST
/* SZ strings */


#ifdef EXTRAS

PUBLIC VOID FARPUBLIC
PszToCab(hcab, psz, iag)
/*
  -- put a handle in a cab, no data copy
*/
HCAB hcab;
char **psz;
WORD iag;
	{
	StartPublic();
	REGISTER HANDLE *ph;

	Assert(hcab != NULL);
	Assert(iag <= PcabxOfCab(hcab)->cabh.cagHandle);

 	ph = &PcabxOfCab(hcab)->rgh[iag];
	if (*ph != NULL)
		HeapFree(*ph);

	*ph = (HANDLE) psz;
	StopPublic();
	}
#endif /*EXTRAS*/



PUBLIC VOID FARPUBLIC
SzToCab(hcab, sz, iag)
HCAB hcab;
char *sz;
WORD iag;
	{
	StartPublic();
	WORD cb = strlen(sz) + 1;
	RgbToCab(hcab, sz, cb, iag);
	StopPublic();
	}



PUBLIC char * FARPUBLIC
SzFromCab(hcab, sz, cbMac, iag)
/*
  -- return string that will be zero terminated
*/
HCAB hcab;
char *sz;
REGISTER WORD cbMac;
WORD iag;
	{
	WORD	cbCur;
	StartPublic();

	Assert(cbMac > 1);		/* must have room for something */
	/* blt but don't trash null */
	Assert(PpvFromCab(hcab, iag) != NULL);
	cbCur = strlen(*PpvFromCab(hcab, iag)) + 1;	/* include trailing 0 */
	if (cbMac > cbCur)
		cbMac = cbCur;		/* only copy real data */
#ifdef KANJI
	else {
		char *szcab = *PpvFromCab(hcab, iag);
		cbMac = PchPrevDbcs(szcab + cbMac, szcab) - szcab + 1;
	}
#endif
	sz[cbMac-1] = '\0';
	ReturnPublic(RgbFromCab(hcab, sz, cbMac-1, iag), char *);
	}


#else
/* ST strings : This implementation is bogus */
PUBLIC VOID FARPUBLIC
StToCab(hcab, st, iag)
HCAB hcab;
char *st;
WORD iag;
	{
	StartPublic();
	RgbToCab(hcab, st, *st + 1, iag);
	StopPublic();
	}


PUBLIC char * FARPUBLIC
StFromCab(hcab, st, cb, iag)
HCAB hcab;
char *st;
WORD iag, cb;
	{
	StartPublic();
	.... this is wrong !!!!!
	ReturnPublic(RgbFromCab(hcab, st, cb, iag), char *);
	}

#endif /*SDM_ST*/
