;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1988 - 1991
; *                      All Rights Reserved.
; */

/*
 * MEMEX.C - expanded and extended memory handling functions for MEM.C.
 *
 */

#include "bios.h"
#include "ctype.h"
#include "conio.h"			/* need for kbhit prototype */
#include "stdio.h"
#include "dos.h"
#include "string.h"
#include "stdlib.h"
#include "msgdef.h"
#include "version.h"			/* MSKK02 07/18/89 */
#include "mem.h"
#include "xmm.h"
#include "versionc.h"

int IsWindowsRunning (void);

void
GetExtraMemory()
{
   GetEMS();
   GetXMS();
}

void
GetXMS()
{
   int far *ptr;
   long a, b;

   mem_table.hma = -2;	/* Does not exist by default */

   if (IsPre286())
      {
      mem_table.xms_ttl = 0L;
      mem_table.xms_free = 0L;
      return;
      }

   InRegs.x.ax = 0x4300;
   int86x (0x2F, &InRegs, &OutRegs, &SegRegs);

   if (OutRegs.h.al == 0x80)
      {
      InRegs.x.ax = 0x4310;
      int86x (0x2F, &InRegs, &OutRegs, &SegRegs);
      FP_SEG(ptr) = SegRegs.es;
      FP_OFF(ptr) = OutRegs.x.bx;

      a = XMSVersion (ptr);
      b = XMSDriver  (ptr);

      mem_table.xmsMvers = (int)(a / (long)0x100);  // Get version number
      mem_table.xmsmvers = (int)(a % (long)0x100);

      mem_table.xmsMdrvr = (int)(b / (long)0x100);  // Get driver version
      mem_table.xmsmdrvr = (int)(b % (long)0x100);
      }

/*
 * BUGBUG t-richj 11/23/92:  Function 0x52 doesn't return valid XMS # if > 16MB
 *    Previously, MEM was using int 0x21, function 0x52 to obtain the amount
 *    of XMS on the system--this fails on machines with more than 16MB, as does
 *    the semi-documented int 0x15, function 0x88 call.  The only reliable
 *    method to determine the total amount of XMS is to sum each used block
 *    with the amount free (the call for which works properly).  We use the
 *    DOS call here to determine the amount of XMS tenatively; if an XMM
 *    is installed, we will correct the total size of XMS by working with it.
 *
 */

   InRegs.h.ah = (unsigned char) 0x52;
   intdosx(&InRegs,&OutRegs,&SegRegs);

   FP_SEG(SysVarsPtr) = SegRegs.es;
   FP_OFF(SysVarsPtr) = OutRegs.x.bx;
   if (SysVarsPtr->ExtendedMemory != 0)
      {
      mem_table.xms_ttl = (long)SysVarsPtr->ExtendedMemory * 1024L;

      OutRegs.x.cflag = 0;
      InRegs.x.ax = GetExtended;

      int86(CASSETTE, &InRegs, &OutRegs);

      mem_table.int_15h = (long)OutRegs.x.ax * 1024L;

/*
 * subtract out VDisk usage from int15h memory.  Note assumption that
 * VDisk usage doesn't exceed 64Mb.  Don't bother if there is no extended
 * memory.
 *
 */

      if (mem_table.int_15h != 0L)
         mem_table.int_15h -= (long)CheckVDisk() * 1024L;

      // BUGBUG jimmat 1-14-93
      // This has already been done above.  Doing it again voids the
      // vdisk check just before this.

      InRegs.x.ax = 0x8800;
      int86 (0x15, &InRegs, &OutRegs);
      mem_table.int_15h = (long)OutRegs.x.ax *1024L;

/*
 * if an XMS driver is present, INT 15 may return 0 as the amount
 * of extended memory available.  In that case, call the XMS
 * driver to find out the amount of XMS free.  Don't call XMS
 * unconditionally, because that will cause it to claim memory
 * if it has not already done so.
 *
 * However, it is possible, with the newer versions of Himem,
 * for XMS memory and INT 15 memory to coexist.  There is no
 * completely reliable way to detect this situation, but we
 * do know that if Himem is installed, DOS is high, and INT 15
 * memory exists, then we are configured that way.  In that case,
 * we can make calls to Himem without disrupting the memory environment.
 * Otherwise we can't.
 *
 */

      if (XMM_Installed())
         {
	 mem_table.xms_free = XMM_QueryTotalFree() * 1024L;

	 if (!IsWindowsRunning())
	    Correct_Total_XMS ();

	 mem_table.xms_ttl -= mem_table.umb_ttl;

         InRegs.x.ax = 0x3306;		/* get DOS version info */
         intdos(&InRegs, &OutRegs);	/* call DOS */

         mem_table.hma = (OutRegs.h.dh == DOSHMA) + 2*(OutRegs.h.dh == DOSROM);

         if (! mem_table.hma)
	    {
	    mem_table.hma = XMM_RequestHMA(0xffff) ? -1 : 0;
	    if (! mem_table.hma)
	       {
	       XMM_ReleaseHMA();
	       }
	    }
	 }
      }
}

void
GetEMS ()
{
   if (! EMSInstalled())  return;

   InRegs.x.ax = EMSGetFreePgs;
   int86x (EMS, &InRegs, &OutRegs, &SegRegs);

   mem_table.ems_ttl  = (16L*1024L) * OutRegs.x.dx;
   mem_table.ems_free = (16L*1024L) * OutRegs.x.bx;

   InRegs.x.ax = EMSGetVer;
   int86x(EMS, &InRegs, &OutRegs, &SegRegs);
   mem_table.emsMvers = (OutRegs.h.al & 0xF0) >> 4;
   mem_table.emsmvers = (OutRegs.h.al & 0x0F);
}

char EMSInstalled()
{
   unsigned int	EMSStatus;
   unsigned int	EMSVersion;

   char		EmsName[8];
   void far	*EmsNameP;

   InRegs.h.ah = GET_VECT;
   InRegs.h.al = EMS;
   intdosx(&InRegs,&OutRegs,&SegRegs);

   if ((SegRegs.es != 0) && (OutRegs.x.bx != 0))
      {
      EmsNameP = EmsName;
      movedata(SegRegs.es, 0x000a, FP_SEG(EmsNameP), FP_OFF(EmsNameP), 8);
      if (strncmp(EmsName, "EMMXXXX0", 8))
	 return FALSE;

      InRegs.x.ax = EMSGetStat;	 	  		/* get EMS status    */
      int86x(EMS, &InRegs, &OutRegs, &SegRegs);
      EMSStatus = OutRegs.h.ah; 		  	/* EMS status in AH  */

      InRegs.x.ax = EMSGetVer;		  		/* get EMS version   */
      int86x(EMS, &InRegs, &OutRegs, &SegRegs);
      EMSVersion = OutRegs.h.al;		  	/* EMS version in AL */

      if ((EMSStatus == 0) && (EMSVersion >= DOSEMSVER))
	 return TRUE;
      }

   return FALSE;
}

/*
 * The code below is snitched from xmsinfo.c (thanks, jimmat)
 *
 */

void Correct_Total_XMS()
{
   struct _handle far *lpHandle, far *lpHandleTable;
   unsigned int HandleCount, HandleSize;

/*
 * If we can talk to the XMS driver, we can obtain an accurate amount of
 * total XMS by adding the amount free to the amount used--the amount used
 * being obtained by checking each handle, and summing the size of those
 * marked 'used' (0x02 == hType).
 *
 */

   if (lpHandleTable = HimemHandleTable (&HandleCount,&HandleSize))
      {
      mem_table.xms_ttl  = mem_table.xms_free;
      mem_table.xms_ttl += 0x40 * 1024L;  /* Account for HMA */

      for (lpHandle = lpHandleTable; HandleCount; HandleCount--, lpHandle++)
         {
         if (lpHandle->hType == 0x02)     /* Is this handle used? */
            {
            mem_table.xms_ttl += (long)lpHandle->hLen * 1024L;
            }
         }
      }
}
