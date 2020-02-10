;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>
#include <text.h>

/* The Width and Height of the BusyDialog -- rectangular region that
 * displays the number of files/directories read so far (at startup time).
 */
// this is now defined in text.h for international #define BUSYWIDTH  40
#define BUSYHEIGHT  6

extern WORD gCnx;
extern BOOL IsMenuDown(void);

#if defined(SWE)||defined(RUS)
  extern VOID CopyNumberYST(char *CountStr, unsigned long l, BOOL t, int maxcount);
#endif

VOID PutUpBusyDialog(int ith);
BOOL isup[2] = {FALSE,FALSE};
WORD b_x[2],b_y[2];
int ReadCount[2] ;
int gReadUpdateFreq ;


VOID PutUpBusyDialog(int ith)
{
	RRC drect;
	int len;
	WORD top, bottom;
	extern unsigned char gNUMDIRSREADLEN ;

	/* If the menu is down and we haven't yet put up our busy dialog
	 * banner, son't chomp on menu. Else, the updating we do of the
	 * number of files/dirs readin does not affect the pulled down menu.
	 */
	if ( (IsMenuDown()) && (!isup[ith]) )
		return;

	if(!isup[ith])
	{
		/* Initialize update freq count variable. Using a value of 1
		 * will force screen update the very first time.
		 */
		ReadCount[ith] = 1 ;

		isup[ith] = TRUE;
		top    = Get_List_Rect(&TreeList[ith]).ayTop;
		bottom = Get_List_Rect(&TreeList[ith]).ayBottom;

		Not_Blank(&TreeList[ith]);
		Not_Blank(&FileList[ith]);
		DoRedisplayList(&TreeList[ith]);
		DoRedisplayList(&FileList[ith]);
		UpdateListBox(&TreeList[ith]);
		UpdateListBox(&FileList[ith]);

		FEnableMouseNest(FALSE);
		drect.rxLeft = axMac/2-BUSYWIDTH/2;
		drect.rxRight = axMac/2+BUSYWIDTH/2;
		drect.ryTop = (BYTE) (top+(bottom-top)/2- (WORD)BUSYHEIGHT/2);
		drect.ryBottom = (BYTE) (top+(bottom-top)/2+ BUSYHEIGHT/2);
		FillRrc(&MainWind,&drect,' ',isaDialogBox);
		len=strlen(szReadingDisk);
		b_x[ith] = drect.rxLeft+(drect.rxRight-drect.rxLeft)/2 - len/2;
		b_y[ith] = drect.ryTop +(drect.ryBottom-drect.ryTop)/2 - 2;
		TextOut(&MainWind,(RX) b_x[ith],(RY) b_y[ith],szReadingDisk,-1,isaDialogBox);
		TextOut(&MainWind,(RX) b_x[ith],(RY) b_y[ith]+2,szNumFilesRead,-1,isaDialogBox);
		TextOut(&MainWind,(RX) b_x[ith],(RY) b_y[ith]+3,szNumDirsRead,-1,isaDialogBox);
		if(gisgraph)
		{
		FrameCharRect(drect.ryTop,drect.rxLeft,drect.ryBottom,drect.rxRight, 1,isaBorders);
		}
		else
		{
		EasyDrawBox(&MainWind, drect.ryTop, drect.rxLeft, drect.ryBottom,
						 drect.rxRight, isaDialogBox);
		}
		FEnableMouseNest(TRUE);
   }
#ifdef JAPAN
	else if(gCnx < cnxMenu) /* not a dialog up, not pull-down menu */
#else
	else if(gCnx <= cnxMenu) /* not a dialog up */
#endif
	{

		/* If this counter has not reached 0, we don't update the screen */
		if (--ReadCount[ith])
			return ;

		ReadCount[ith] = gReadUpdateFreq ;

	   // Disabling & then enabling mouse causes too much flashing!!
	   // FEnableMouseNest(FALSE) ;
	   #if defined(SWE)|| defined(RUS)
	     CopyNumberYST(&szCountStr[MAXCOUNTLEN-2],
				(unsigned long)listinfo[ith].tree->Diskfilecount, TRUE, MAXCOUNTLEN-2) ;
	   #else
	     CopyNumberForTextOut(&szCountStr[MAXCOUNTLEN-2],
				(unsigned long)listinfo[ith].tree->Diskfilecount, TRUE) ;
	   #endif

	   TextOut(&MainWind,(RX) b_x[ith]+gNUMDIRSREADLEN,
					(RY) b_y[ith]+2,szCountStr+1,-1,isaDialogBox);

	   /* The "+1" in count below is to account for the Root Directory */
	   #if defined(SWE)|| defined(RUS)
	     CopyNumberYST(&szCountStr[MAXCOUNTLEN-2],
				(unsigned long) listinfo[ith].tree->DirCount+1, TRUE, MAXCOUNTLEN-2) ;
	   #else
	     CopyNumberForTextOut(&szCountStr[MAXCOUNTLEN-2],
				(unsigned long)listinfo[ith].tree->DirCount+1, TRUE) ;
	   #endif
	   
	   TextOut(&MainWind,(RX) b_x[ith]+gNUMDIRSREADLEN,
					(RY) b_y[ith]+3,szCountStr+1,-1,isaDialogBox);

	   // FEnableMouseNest(TRUE) ;
	}
}


VOID TakeDownBusyDialog(ith)
{
    RRC drect;
    WORD top,bottom;
    /*
     * don't take down dialog if a menu or dialog is above it!
     */
    if (gCnx == cnxNull)
    {
       if(isup[ith])
       {
	  top    = Get_List_Rect(&FileList[ith]).ayTop;
	  bottom = Get_List_Rect(&FileList[ith]).ayBottom;
	  drect.rxLeft = axMac/2-BUSYWIDTH/2;
	  drect.rxRight = axMac/2+BUSYWIDTH/2;
	  drect.ryTop = (BYTE) (top+(bottom-top)/2- BUSYHEIGHT/2) ;
	  drect.ryBottom = (BYTE) (top+(bottom-top)/2+ BUSYHEIGHT/2) ;


	  FillRrc(&MainWind,&drect,' ',isaBackground);
	  Not_Blank(&TreeList[ith]);
	  Not_Blank(&FileList[ith]);

	  /* The scroll bar region is screwed up when we do the "FillRrc()" above,
      * draw it afresh! In system tree mode, we don't have to worry about
		* this!
		*/
	  if (glob.TreeMode != TR_SYSTEM)
		  DrawScrollBar(&TreeList[ith], TRUE) ;
	
	  if (glob.TreeMode != TR_SYSTEM)
	  {
		DoRedisplayList(&TreeList[ith]);
		UpdateListBox(&TreeList[ith]);
	  }
	  DoRedisplayList(&FileList[ith]);
	  UpdateListBox(&FileList[ith]);
       }

		 ReadCount[ith] = 1 ; // back to default state

       isup[ith] = FALSE;
    }
}
