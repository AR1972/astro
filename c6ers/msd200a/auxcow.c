/*
  -- auxcow.c : COW auxiliary code
  -- NOTE : normally use this code as-is, but you can modify it if you have
  a more advance memory allocation scheme.
*/


/* These must be exported */

VOID   * FAR PASCAL PbAllocWork(WORD);
VOID   FAR PASCAL FreeWork(VOID *);

/* For FAR Work buffer */

VOID FAR *  FAR PASCAL LpbAllocWorkFar(WORD);
VOID FAR PASCAL FreeWorkFar(VOID FAR *);

/* Fake LMEM */

VOID ** FAR PASCAL PpvAllocCb(WORD, WORD);
VOID    FAR PASCAL FreePpv(WORD, VOID **);
BOOL    FAR PASCAL FReallocPpv(WORD, VOID **, WORD);
BOOL    FAR PASCAL FCheckHandle(WORD, VOID **);
WORD    FAR PASCAL CbSizePpv(WORD, VOID **);


#ifdef BASED
BOOL FAR PASCAL WndSegInit (VOID);
BOOL FAR PASCAL WndSegDestroy (VOID);
#endif


#ifdef BASED

BOOL FAR PASCAL WndSegInit ()
{
  pWndSeg = _bheapseg ( 10000 );

  if ( pWndSeg == _NULLSEG )
    return ( FALSE );

  return ( TRUE );
}

BOOL FAR PASCAL WndSegDestroy ()
{
  if ( _bfreeseg ( pWndSeg ) != 0 )
     return ( FALSE );  

  return ( TRUE );  
}


PWND FAR PASCAL
PbAllocWnd (cb)
WORD cb;
{
  PWND pwnd;

  pwnd = (PWND)_bmalloc ( pWndSeg, cb );

  if ( pwnd == _NULLOFF )
    return ( NULL );

  return ( pwnd );
}


VOID FAR PASCAL
FreeWnd ( pwnd )
PWND pwnd;
{
  _bfree ( pWndSeg, pwnd );
}

#else


PWND FAR PASCAL
PbAllocWnd (cb)
WORD cb;
{
  PWND pwnd;

  pwnd = (PWND)PbAllocWork(cb);

  return ( pwnd );
}


VOID FAR PASCAL
FreeWnd ( pwnd )
PWND pwnd;
{
  FreeWork( pwnd );
}


#endif

// Each app should have it's own Exit routine.  Various debugging libs
//  call here to abort cleanly when errors occur.


VOID FAR PASCAL
Exit(ex)
int ex;
{

  if (ex == 0)
    {
    FEnableMouse(FALSE);
    EndCow(!FRestoreOriginalScreen());
    }
  else
    EndCow(FALSE);  /* don't clear screen or restore because
                 there's probably error msg */

  exit(ex);
}


//      Work Buffer Routines

//  -- a near work buffer MUST be provided by the application
//  -- using malloc() / free() really screws up the C-library memory manager


#define cbBufferMax 8000

BYTE rgbBuffer[cbBufferMax];
BYTE *pbBuffer = rgbBuffer;


VOID * FAR PASCAL
PbAllocWork(cb)
WORD cb;
{
  BYTE *pbRet = pbBuffer;

  if (cb & 1)
    cb++;   // make even to keep allocs on word boundary 

  pbBuffer += cb;

  if (pbBuffer > &rgbBuffer[cbBufferMax])
    {
    // trap before out of memory 

#ifdef DEBUG
    printf("Work Buffer Full\n");
#endif /*DEBUG*/

    Exit(100);    // death 
    }

  return pbRet;
}



VOID FAR PASCAL
FreeWork(pv)
VOID *pv;
{
  pbBuffer = pv;
}



VOID FAR * FAR PASCAL
LpbAllocWorkFar(cb)
//
//  -- allocation for screen saves
//  -- NOTE : this routine can not fail !!!!!
WORD cb;
{

#if 0
  ++cLpbAllocWorkFar;
  return _fmalloc(cb);
#else
  WORD FAR *lpw;
  HANDLE hmem;

  if ((hmem = GlobalAlloc(0, (DWORD) (cb+2))) == NULL)
    {
    printf("far alloc fail\n");
    Exit(1);
    }

  lpw = (WORD FAR *) GlobalLock(hmem);

  *lpw++ = hmem;    /* save handle */


  return lpw;
#endif

}


//  -- free far work buffer

VOID FAR PASCAL
FreeWorkFar(lpb)
VOID FAR *lpb;
{

#if 0
  _ffree(lpb);

#else

  HANDLE hmem = *(((WORD FAR *)lpb) - 1);

  GlobalUnlock(hmem);

  GlobalFree(hmem);

#endif
}



//  -- fake local memory handler : for SDM and SMM
//  -- in the future we may "fix" SDM so that near pointers will be used
//  instead of handles
//  -- we use malloc() / free() to allocate fixed blocks and keep a master
//  pointer table for the extra level of indirection

#define cpvMaster 32

VOID *rgpvMaster[cpvMaster];

//  -- return handle to a memory block

VOID ** FAR PASCAL
PpvAllocCb(sb, cb)
WORD sb;      // ignored
WORD cb;      // size of block 
{
  REGISTER VOID **ppv;

  Unreferenced(sb);

  for (ppv = &rgpvMaster[0]; *ppv != NULL; ppv++);

  *ppv = malloc(cb);

  if (*ppv == NULL)
    return(NULL);
  else
    return(ppv);
}


//  -- free local block (return NULL)

VOID FAR PASCAL
FreePpv(sb, ppv)
WORD sb;        // ignored 
REGISTER VOID ** ppv;
{
  Unreferenced(sb);

  free(*ppv);

  *ppv = NULL;
}

//  -- reallocate block with new size, changing master pointer

BOOL FAR PASCAL
FReallocPpv(sb, ppv, cb)
WORD sb;
REGISTER VOID **ppv;
WORD cb;
{
  VOID *pv;

  Unreferenced(sb);

  if ((pv = realloc(*ppv, cb)) == NULL)
    return FALSE;
   else
    {
    *ppv = pv;
    return TRUE;
    }
}


//  -- check that this is a handle to a valid block

BOOL FAR PASCAL
FCheckHandle(sb, ppv)
WORD sb;
VOID **ppv;
{
  Unreferenced(sb);

  return(&rgpvMaster[0] <= ppv && ppv < &rgpvMaster[cpvMaster] &&
      *ppv != NULL);
}

//  -- return size of allocated block

WORD FAR PASCAL
CbSizePpv(sb, ppv)
WORD sb;
VOID **ppv;
{
  Unreferenced(sb);

  return(_msize(*ppv));
}
