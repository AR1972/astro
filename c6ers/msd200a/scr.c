/*
  scr.c : screen / kanji tests
*/


#define fAllocFonts TRUE
/*
WORD FAR * FAR PASCAL LpwAllocDriverMem(WORD, WORD);
VOID FAR PASCAL FreeDriverMem(WORD FAR *);
VOID FAR PASCAL InitModeMapping(void);
*/
STATIC BOOL   FSzOfImd(WORD, char *);
STATIC WORD   CwNeedInst(INST *);
STATIC BYTE   BPrefixLine(WORD);
STATIC VOID   TestDbcs(void);
STATIC VOID   TestChar(DCHAR, char);

#ifdef OS2
int FAR PASCAL DosAllocSeg(WORD, WORD FAR *, WORD);
int FAR PASCAL DosFreeSeg(WORD);
#endif

/* forward */
STATIC VOID FAR * LpwAllocCw(WORD);
STATIC VOID FreeLpw(WORD FAR *);

WORD    mpimdimode[100] = { 0 };
WORD    imdMac = 0;

/************************* Segment aligned allocation *******************/

//  -- allocate "cw" words, return far pointer with offset of 0
//  -- return 0 if error

STATIC VOID FAR *
LpwAllocCw(cw)
WORD  cw;
{
#ifndef OS2
  WORD FAR * lpw = _fmalloc(cw * 2 + 15);
  WORD  para;

  // normalize pointer 

        para = (HIWORD (lpw) + ((LOWORD ((DWORD) lpw) + 15) >> 4));
        return (WORD FAR *) MAKELONG (0, para);
#else
  WORD  ps;

  if (DosAllocSeg(cw * 2, &ps, 0) != 0)
    return NULL;
  else
    return (WORD FAR *) MAKELONG(0, ps);

#endif
}

//  -- free allocated far pointer

STATIC VOID
FreeLpw(lpw)
WORD FAR * lpw;
{
#ifndef OS2
  _ffree(lpw);    // far buffer 
#else
  DosFreeSeg(HIWORD(lpw));
#endif 
}

/******************************Driver memory allocation***********************/

//  -- driver memory allocator

WORD FAR * FAR PASCAL
LpwAllocDriverMem(cw, fmem)
WORD  cw;
WORD  fmem;
{
  if (fmem & fmemNear)
    {
    WORD *  pw;
    if ((pw = malloc(cw * 2)) == NULL)
      return NULL;

    return (WORD FAR *) pw;
    }
  else
    {
    // normal far allocation (must be PARA aligned)
    return LpwAllocCw(cw);
    }
}


//  -- free driver memory

VOID FAR PASCAL
FreeDriverMem(lpw)
WORD FAR * lpw;
{
        if (HIWORD ((WORD FAR *) &lpw) == HIWORD (lpw))
                free ((void *) LOWORD ((DWORD) lpw));      /* near buffer */
  else
                FreeLpw (lpw);
}



/*****************************************************************************/

//  -- initialize mapping

VOID FAR PASCAL
InitModeMapping()
{
  WORD  imode;
  INST  inst;

  if (imdMac != 0)
    return;

  imode = 0;

  while (FQueryInst(&inst, imode))
    {
    if (inst.finst & finstAvailable)
      mpimdimode[imdMac++] = imode;
    imode++;
    }
}


/*****************************************************************************/

//  -- high level init (includes allocating buffers)
//  imd == -1 -> initialize best guess

BOOL FAR
FInitScreenMode(imd)
WORD  imd;
{
  INST  inst;
  WORD  imode;

  if (imd == -1)
    {
    if ((imode = ImodeGuessCurrent()) == imodeUnknown)
      imode = mpimdimode[0];
    }
  else
    imode = mpimdimode[imd];

  if (!FQueryInst(&inst, imode) ||
      !FAllocInstBuffers(&inst, LpwAllocDriverMem, fAllocFonts) ||
      !FAllocOverlapTable(&inst, LpwAllocDriverMem) ||
      !FInitScreen(&inst, FALSE))
    return FALSE;

  // it worked ! 
  return TRUE;
}


/*****************************************************************************/
// Change Screen Mode Option 

//  -- query the available modes of the adapter

VOID FAR
SelectMode()
{
}



STATIC BOOL
FSzOfImd(imd, sz)
WORD  imd;
char *  sz;
{
  INST  inst;

  if (!FQueryInst(&inst, mpimdimode[imd]))
    {
    MessageBox("CSD inconsistancy!", "mode no longer available",
      NULL, MB_OK | 0x8000);

    return FALSE;
    }

  sprintf(sz, "%4s %3dX%2d (%5d)",
     (inst.finst & finstText) ? "Text" : "Gfxs",
     inst.axMac, inst.ayMac,
     CwNeedInst(&inst));

  if (inst.finst & finstMonochrome)
    strcat(sz, " Mono");
  if (inst.finst & finstAlternate)
    strcat(sz, " Alt");
  if (inst.finst & finstQuestionable)
    strcat(sz, " ??");

  return TRUE;
}



/*****************************Driver Buffer Size function*********************/

// forward (looks like a use of local procedures to me 

STATIC WORD FAR * FAR PASCAL LpwFakeAlloc(WORD, WORD);
STATIC VOID FAR PASCAL FakeFree(WORD FAR *);

WORD  cwNeed;

//  -- fake allocator to count size

STATIC WORD FAR * FAR PASCAL
LpwFakeAlloc(cw, fmem)
WORD  cw;
WORD  fmem;
{
  Unreferenced(fmem);

  cwNeed += cw;

  return (WORD FAR *) MAKELONG(0, 0xffff); // return non-null 
}

//  -- fake free routine

STATIC VOID FAR PASCAL
FakeFree(lpw)
WORD FAR * lpw;
{
  Unreferenced(lpw);
}

//  -- return the number of extra words needed for screen buffers

STATIC WORD
CwNeedInst(pinst)
INST *  pinst;
{
  cwNeed = 0;

  if (!FAllocInstBuffers(pinst, LpwAllocDriverMem, FALSE))
    return 0;

  FreeInstBuffers(pinst, FreeDriverMem);

  return cwNeed;
}


/***************************Original screen save & return*********************/

VIDS *  pvids = NULL;   // the vids structure containing info
WORD FAR * lpwScreen;   // the screen contents 

//  -- save the current screen (assuming we are going into the 0th mode)

VOID FAR
SaveOriginalScreen()
{
  INST  inst;
  WORD  cbVids;

  pvids = NULL;

  if (!FQueryInst(&inst, 0))
    return;   // something really wrong 

  if ((cbVids = CbSizeVids()) == 0)
    return;   // screen save not supported 

  if ((pvids = malloc(cbVids)) == NULL)
    return;   // no room for VIDS 

  if (!FSaveVids(pvids, &inst))
    {
    // error during save 
    free(pvids);
    pvids = NULL;
    return;
    }

  if ((lpwScreen = LpwAllocCw(pvids->cwVidData)) != NULL)
    SaveVidData(pvids, lpwScreen);
}



BOOL FAR
FRestoreOriginalScreen()
{
  if (pvids == NULL)
    {
    // we can't restore => clear the screen 
    return FALSE;
    }

  if (!FRestoreVids(pvids))
    {
//    MessageBox("Screen restore failed", NULL, NULL, MB_OK | 0x8000);

    free(pvids);

    pvids = NULL;

    if (lpwScreen != NULL)
      {
      FreeLpw(lpwScreen);
      lpwScreen = NULL;
      }
    return FALSE;
    }

  RestoreVidData(pvids, lpwScreen);

  free(pvids);
  pvids = NULL;

  if (lpwScreen != NULL)
    {
    FreeLpw(lpwScreen);
    lpwScreen = NULL;
    }
  return TRUE;
}
