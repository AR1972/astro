;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****   menus.c - fns for creating menu bars
** This file contains Scottq's "easy" menu functions for more easily
** constructing menu bars.  It also contains the global menu bar structures,
** and the fn which initialises the menu bars.
**
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  ?/??/87   scottq    wrote easymenu() and friends
**  7/17/89   t-jeffro  FileMgrBar has entry to exit to main menu.
**  8/??/89   harikris  Added Exit, History menu items.
*/


#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <text.h>
extern VOID HelpBox(void);
extern VOID KeyBox(void);
extern VOID IndexBox(void);
extern VOID BasicsBox(void);
extern VOID CommandsBox(void);
extern VOID ProceduresBox(void);
extern VOID UsingHelpBox(void);

extern VOID CopyProgram(void);
extern VOID ReorderGroup(void);
extern VOID Run(void);
extern void DoFullRefresh(void) ;
extern void DoDiskReread(void) ;
extern VOID DoShareMode(void);
extern char gStartInDir[] ;
extern BOOL gfFMVisited ;

extern VOID PDoSingleTree(void);
extern VOID PDoDoubleTree(void);
extern VOID PDoFlatDisplay(void);
extern VOID PDoShareMode(void);

extern VOID DisableTaskList(void);
extern VOID EnableTaskList(void);
extern VOID DisableSwitching(void);
extern VOID EnableSwitching(void);
extern VOID ColorBox(void);
extern VOID ScreenBox(void);
extern VOID SelectAcrossDirs(void) ;

extern VOID AsciiFileView(void) ;
extern VOID HexFileView(void) ;
extern VOID ExitFileView(void) ;

extern BOOL FPerformDirOperation(void) ;
extern unsigned int CountFilesToOperateOn(void) ;
extern VOID ShellAboutBox(void);
char far cdecl C_GET_LIST_LENGTH(void);

extern VOID RereadSelectedDir(void) ;

extern BOOL gInReorder, gInCopy;
extern BOOL ascii ; /* mode in which file is being viewed in ViewFile */
extern BOOL gTaskListEnabled;
extern BOOL gSwitchingEnabled;
extern TOKEN gGroupLevel;
extern struct ListBoxData ProgramList;

VOID  MySetColor(WORD color);

extern BOOL gAlreadyTasking;
/* ZZZZZ following fn needed?? */
VOID FAR DoNothing(VOID)
{
}


PMTM FAR PASCAL PmtmAddItem(HMNU hmnu, WORD idBefore, WORD idNew,
							CHAR *sid, HMNU hmnuSub, WORD cwExtra, BYTE bFlags)
/*
  -- create new item in menu
  -- put all specified information into it
*/
{
	WORD    cb;
	HMNU    hmnuNew;
	register PMTM   pmtmItem;

	cb = cbMTM + cwExtra * sizeof(WORD);
	if (bFlags & MM_STRING)
		cb += (strlen(sid) & 0xFFFE) + 2;
	if (hmnuSub != NULL)
		cb += sizeof(HANDLE);

	/* idBefore must be idNil!!*/
		if(idBefore == idNil)
		{
		while ((*hmnu)->hmnuNext != NULL)
			hmnu = (*hmnu)->hmnuNext;

			{
			hmnuNew = HmnuNewMenu();

			if (!FReAllocPpv(NULL, (WORD **)hmnuNew, cbMNU + cb))
				return(NULL);

			(*hmnu)->hmnuNext = hmnuNew;
			pmtmItem = &(*hmnuNew)->grmtm;
			(*hmnuNew)->cmtm = 1;
			}
		}

	pmtmItem->bFlags = bFlags & ~MM_SUBMENU;

	pmtmItem->id = idNew;
	pmtmItem->cwTotal = (BYTE) (cb/2 - cwMTM) ;

	if (bFlags & MM_STRING)
	{
		pmtmItem->u.iwString = cwExtra;
		strcpy((char *)&pmtmItem->rgwExtra[cwExtra], sid);
	}
	 else
		pmtmItem->u.sid = (WORD) sid;

	if (hmnuSub != NULL)
	{
		pmtmItem->bFlags |= MM_SUBMENU;
		pmtmItem->rgwExtra[pmtmItem->cwTotal-1] = (WORD) hmnuSub;
	}

	return(pmtmItem);
}

/* Functions called by menu items */

/* Menu bar structures */
MENUINFO MainMenuBar;
MENUINFO FileMgrMenuBar;
MENUINFO ViewMenuBar;

#define MENUBARY 1     /* y location of the menu bar */
#define MENUGAP 2
#define MENUBORDER 5
#define MENUTOPBOTTOM 2

#define NUMMENUS 115
int       (FAR *menuproc[NUMMENUS])(void);
BYTE      emenustate[NUMMENUS];
#define NUMDROPDOWNS 12
ARC       menurect[NUMDROPDOWNS];
BYTE      rectindex[NUMMENUS];
int       gNthmenu = 0;
int       gNthdropdown = 0;
int       gCurMenuX;    /* start of next menu */
int       gFMfilestart;
int       gPMfilestart;
int   gViewstart;

/* These menu-id's are used to implement key board accelerators. These need
 * to be the 'id's of menus that remain always enabled. In our case we are
 * guaranteed that the help menu items under the program-manager and the
 * file-manager are never disabled!!
 */
int     gPMMagicShiftF9, gFMMagicShiftF9 ;
int     gFMMagicTab, gPMMagicTab ;
int     gPMMagicShiftTab, gFMMagicShiftTab ;
int     gPMMagicAltTab, gFMMagicAltTab ;
int     gFMMagicCtrlF5 ;

VOID initbar(MENUINFO *amenu)
{
    *amenu = HmnuNewMenu();
    gCurMenuX = 0;
}

HMNU gCurmen;

VOID easyitem(MENUINFO *amenu,char *name,int keyequiv,int (FAR *theproc)())
{
	int temp;
	int slen;

	UnReferenced(amenu) ;
	UnReferenced(keyequiv) ;

	menuproc[gNthmenu] = theproc;
	emenustate[gNthmenu] = MM_ENABLED;
	PmtmAddItem(gCurmen,idNil,gNthmenu,name,NULL,0,MM_ENABLED);
	/*
	 * calculate the length of the longest string in popdown menu
	 * and update the graphic rectangle's width
	 */
	temp = menurect[gNthdropdown-1].axRight -
	       menurect[gNthdropdown-1].axLeft - MENUBORDER;
#ifdef KANJI
	slen = strlen(name)-1-1; /* for ~ */
#else
	slen = strlen(name)-1; /* for ~ */
#endif
	if (slen > temp)
	{
	    menurect[gNthdropdown-1].axRight =
	    menurect[gNthdropdown-1].axLeft + MENUBORDER + slen;
	}
	menurect[gNthdropdown-1].ayBottom += 1;
	rectindex[gNthmenu] = (BYTE) gNthdropdown-1;

	++gNthmenu;
}

VOID makeamenu(MENUINFO *amenu, char *name, int keyequiv)
{
	HMNU hmnunew;

	UnReferenced(keyequiv) ;

	hmnunew = HmnuNewMenu();
	gCurmen = hmnunew;
	PmtmAddItem(*amenu,idNil,(gNthdropdown+NUMMENUS), name, hmnunew,
												0, (BYTE)MM_ENABLED);
	menurect[gNthdropdown].ayTop   = MENUBARY+1;
	menurect[gNthdropdown].ayBottom= MENUBARY+1+MENUTOPBOTTOM;
	menurect[gNthdropdown].axLeft  = (BYTE) gCurMenuX;
	menurect[gNthdropdown].axRight = (BYTE) gCurMenuX+MENUBORDER; /* updated in easyitem */

	++gNthdropdown;
#ifdef KANJI
	gCurMenuX += MENUGAP + strlen(name) - 1 - 1;
#else
	gCurMenuX += MENUGAP + strlen(name) - 1;
#endif
}

#define easymenu(amenu,name,keyequiv) makeamenu(amenu,name,keyequiv)

VOID easyseparator(MENUINFO *amenu)
{
	UnReferenced(amenu) ;

	PmtmAddItem(gCurmen,idNil,gNthmenu,"",NULL,2,MM_SEPARATOR);
	menurect[gNthdropdown-1].ayBottom += 1;
	emenustate[gNthmenu] = MM_SEPARATOR;

	++gNthmenu;
}

VOID easycheck(int (FAR *theproc)(), BOOL docheck)
{
   int i;

	for(i = 0; i<gNthmenu; i++)
	{
		if (menuproc[i] == theproc)
		{
			CheckMenuItem(i,docheck);
		}
	}
}

VOID easyenable(MENUINFO *amenu, int (FAR *theproc)(), BOOL doenable)
{
   int i;

	UnReferenced(amenu) ;

	for(i = 0; i<gNthmenu; i++)
	{
		if (menuproc[i] == theproc)
		{
			if(doenable)
			emenustate[i] = emenustate[i] & (~MM_DISABLED);
			else
			emenustate[i] |= MM_DISABLED;
			EnableMenuItem(i, doenable);
		}
	}
}


#define KEY_DELETE 127

MPVKEYID gAccelerators[25];
MPVKEYID *pAccel;

void InitAccelerators(void)
{
    pAccel = gAccelerators;

    /* first entry is dummy according to addacceltable spec */
    //gAccelerators[0].vkey = ;
    //gAccelerators[0].idItem = ;
    gAccelerators[1].vkey = 0;
    gAccelerators[1].idItem = 0;
    AddAccelTable(&pAccel);

}

void AddViewAccelerators(void)
{

    pAccel = gAccelerators;

    DeleteAccelTable(&pAccel);

    /* first entry is dummy according to addacceltable spec */
    //gAccelerators[0].vkey = ;
	//gAccelerators[0].idItem = ;

	gAccelerators[1].vkey = VK_F1;
	gAccelerators[1].idItem = gViewstart+3;

	gAccelerators[2].vkey = 0;
	gAccelerators[2].idItem = 0;

} /* AddViewAccelerators */

void AddProgManAccelerators(void)
{

    pAccel = gAccelerators;

    DeleteAccelTable(&pAccel);

    /* first entry is dummy according to addacceltable spec */
    //gAccelerators[0].vkey = ;
    //gAccelerators[0].idItem = ;

    /* The next are for our "special" accelerators that
     * don't have menu items. We need these since they
     * must also work when a menu is poped-down, or alted.
     */
	gAccelerators[1].vkey = VK_F9|KK_SHIFT;
	gAccelerators[1].idItem = gPMMagicShiftF9 ;
#define PMOPEN 1
#define PMDELETE 3
#define PMEXIT   PMDELETE+6
#define PMREFRESH PMEXIT + 7+7
#define PMHELP   PMREFRESH+1
    /* The actual one-for-one accelerators */
	gAccelerators[2].vkey = KK_ALT+VK_F4;
	gAccelerators[2].idItem = gPMfilestart+9;
	// gAccelerators[3].vkey = VK_DELETE;
	gAccelerators[3].vkey = KEY_DELETE ; /* ZZZZ VK_DELETE doesn't work!! */
	gAccelerators[3].idItem = gPMfilestart+PMDELETE ;
	gAccelerators[4].vkey = VK_F3 ;
	gAccelerators[4].idItem = gPMfilestart+PMEXIT;
	gAccelerators[5].vkey = VK_F1 ;
	gAccelerators[5].idItem = gPMfilestart+PMHELP;
	gAccelerators[6].vkey = 127;/* 127 ==delete */
	gAccelerators[6].idItem = gPMfilestart+PMDELETE;

	gAccelerators[7].vkey = '\t'/*VK_TAB*/|KK_ALT;
	gAccelerators[7].idItem = gPMMagicAltTab ;
	gAccelerators[8].vkey = VK_F5|KK_SHIFT;
	gAccelerators[8].idItem = gPMfilestart+PMREFRESH;
	gAccelerators[9].vkey = '\t';
	gAccelerators[9].idItem = gPMMagicTab ;
	gAccelerators[10].vkey = '\t'|KK_SHIFT;
	gAccelerators[10].idItem = gPMMagicShiftTab ;
	gAccelerators[11].vkey = '\n'|KK_SHIFT|KK_CONTROL;
	gAccelerators[11].idItem = PMOPEN ;
	gAccelerators[12].vkey = 0 ;
	gAccelerators[12].idItem = 0 ;

    AddAccelTable(&pAccel);
}

void AddFileManAccelerators(void)
{
    pAccel = gAccelerators;

    DeleteAccelTable(&pAccel);

    /* first entry is dummy according to addacceltable spec */
    //gAccelerators[0].vkey = ;
    //gAccelerators[0].idItem = ;

    /* The next are for our "special" accelerators that
     * don't have menu items. We need these since they
     * must also work when a menu is poped-down, or alted.
     */
    gAccelerators[1].vkey = '\t';
	gAccelerators[1].idItem = gFMMagicTab ;
    gAccelerators[2].vkey =  '\t'/*VK_TAB*/|KK_ALT;
	gAccelerators[2].idItem = gFMMagicAltTab ;
    gAccelerators[3].vkey = VK_F9|KK_SHIFT;
	gAccelerators[3].idItem = gFMMagicShiftF9 ;
#define FMVIEWFILE 5
#define FMMOVE FMVIEWFILE + 2
#define FMCOPY FMMOVE+1
#define FMDELETE FMCOPY+1
#define FMSELECTALL FMDELETE+6
#define FMDESELECTALL FMSELECTALL + 1
#define FMEXIT FMDESELECTALL+2
#define FMREPAINT FMEXIT + 7+7
#define FMREFRESH FMREPAINT+1
#define FMEXPAND1 FMREPAINT+2
#define FMEXPANDBRANCH FMEXPAND1+1
#define FMEXPANDALL FMEXPANDBRANCH+1
#define FMCOLLAPSE FMEXPANDALL+1
#define FMHELP FMCOLLAPSE+1
    /* The actual one-for-one accelerators */
	gAccelerators[4].vkey = VK_F7;
	gAccelerators[4].idItem = gFMfilestart+FMMOVE;
	gAccelerators[5].vkey = VK_F8;
	gAccelerators[5].idItem = gFMfilestart+FMCOPY;
	// gAccelerators[6].vkey = VK_DELETE;
	gAccelerators[6].vkey = KEY_DELETE ; /* ZZZZZ VK_DELETE doesn't work!! */
	gAccelerators[6].idItem = gFMfilestart+FMDELETE;
	gAccelerators[7].vkey = KK_CONTROL | '/';
	gAccelerators[7].idItem = gFMfilestart+FMSELECTALL;
	gAccelerators[8].vkey = KK_CONTROL|28;/* 92 == cntrl+\ */
	gAccelerators[8].idItem = gFMfilestart+FMDESELECTALL;
	gAccelerators[9].vkey = KK_ALT|VK_F4;
	gAccelerators[9].idItem = gFMfilestart+FMEXIT;
    /* last entry is zeros */
	gAccelerators[10].vkey = '+' ;
	gAccelerators[10].idItem = gFMfilestart+FMEXPAND1;
	gAccelerators[11].vkey = '*' ;
	gAccelerators[11].idItem = gFMfilestart+FMEXPANDBRANCH;
	gAccelerators[12].vkey = KK_CONTROL|'*' ;
	gAccelerators[12].idItem = gFMfilestart+FMEXPANDALL;
	gAccelerators[13].vkey = '-';
	gAccelerators[13].idItem = gFMfilestart+FMCOLLAPSE;
	gAccelerators[14].vkey = VK_F3;
	gAccelerators[14].idItem =gFMfilestart+FMEXIT;
	gAccelerators[15].vkey = VK_F1 ;
	gAccelerators[15].idItem = gFMfilestart+FMHELP;
	gAccelerators[16].vkey = KK_SHIFT|'+' ;
	gAccelerators[16].idItem = gFMfilestart+FMEXPAND1;
	gAccelerators[17].vkey = KK_SHIFT|'*' ;
	gAccelerators[17].idItem = gFMfilestart+FMEXPANDBRANCH;
	gAccelerators[18].vkey = KK_SHIFT|KK_CONTROL|'*' ;
	gAccelerators[18].idItem = gFMfilestart+FMEXPANDALL;

	gAccelerators[19].vkey = VK_F5|KK_SHIFT ;
	gAccelerators[19].idItem = gFMfilestart+FMREPAINT;

	gAccelerators[20].vkey = VK_F9 ;
	gAccelerators[20].idItem = gFMfilestart+FMVIEWFILE; /* viewfile */

    gAccelerators[21].vkey =  '\t'|KK_SHIFT ;
	gAccelerators[21].idItem = gFMMagicShiftTab ;
    gAccelerators[22].vkey =  VK_F5 ;
	gAccelerators[22].idItem = gFMfilestart+FMREFRESH ;
	gAccelerators[23].vkey = KK_CONTROL|VK_F5 ;
	gAccelerators[23].idItem = gFMMagicCtrlF5 ;

	gAccelerators[24].vkey = 0 ;
	gAccelerators[24].idItem = 0 ;

    AddAccelTable(&pAccel);
}


VOID setmenubar(MENUINFO *amenu, PWND towind)
{
	   ARC arcMenubar;

       arcMenubar.axLeft = 0;
       arcMenubar.axRight = axMac;
       arcMenubar.ayTop = MENUBARY;
       arcMenubar.ayBottom = MENUBARY+1;
       InitMenubar(towind, *amenu, &arcMenubar, MENUGAP);

}


VOID MenuCommand(PWND pwnd, WORD mid)
{
	char *pStartDir ;
	char RunInDir[MAX_PATH+1] ;
	int dummylen ;
	BOOL fHandled = TRUE ;

	UnReferenced(pwnd) ;

    if(gInReorder || gInCopy) {
	if(mid == ((gPMfilestart+PMHELP)|0xF000))
	    HelpBox();
	else if(mid>=(gPMfilestart+PMHELP) && mid<=(gPMfilestart+PMHELP+5))
	    (*menuproc[mid])();
	else
	    ShellMessageBox(szInRCTitle, szInRCText);
	return;
    }

	      if((mid & 0xF000) == 0xF000) /* is an acceleration */
	      {
			mid = mid & 0xFFF ;


			if (mid == gFMMagicTab || mid == gPMMagicTab)            /* TAB */
			{
				NextGlobalFocus();

			}
			else if ((mid == gFMMagicShiftTab) ||
				 (mid ==gPMMagicShiftTab))/* Shift+TAB */
				 {
					PrevGlobalFocus();
			}
			else if ((mid == gFMMagicAltTab) ||
				 (mid ==gPMMagicAltTab))/* Shift+TAB */
				 {
#ifndef NOSWITCHER
					if (gSwitchingEnabled && C_GET_LIST_LENGTH()) //number of tasks is 0 based, one is shell
					{
				       LaunchProgram(NullString,NullString,0);
					}       
					else
					{
						Shell_Beep();
					}
#endif
				 }
			else if ( (mid == gFMMagicShiftF9) || /* Shift+F9 */
					  (mid == gPMMagicShiftF9) )
			{
				
				/* By default we would want to use the directory in which the user
				 * was when he started the DOSSHELL. This will be valid if Run is
				 * done from PM and the FM has not been visited.
				 */
				pStartDir = gStartInDir ;

				if (gfFMVisited)
				{
					Tree2Path(listinfo[glob.FocusBox].tree,
							   listinfo[glob.FocusBox].files, RunInDir, &dummylen) ;
					pStartDir = RunInDir ;
				}
				DoCommand(NULL, pStartDir);
			}
			else if ( (mid == gFMfilestart+FMHELP) ||
					  (mid == gPMfilestart+PMHELP) )
				HelpBox();
			else if ( mid == gFMMagicCtrlF5)
				RereadSelectedDir() ;
			else
				fHandled = FALSE ;
			/* else, it's not one our our "special" keys */
			/* fall through, mid is already "fixed" */
		  }
		  else
				fHandled = FALSE ;
	if (!fHandled)
	{
		if(emenustate[mid] & MM_DISABLED)
		{
			Shell_Beep();
		}
	    else
		{
			if ((mid >= 0) && (mid < gNthmenu) && (menuproc[mid] != NULL))
				(*menuproc[mid])();
	    }
	}
}


/***/
/***/


VOID FrameMenu(PWND pwnd,WORD mid)
{

   RECT lRect;
   int whichmenu;
   WORD fore,back;
	int i;
	int firstid;

	if (mid >= NUMMENUS)
	{
		whichmenu = mid - NUMMENUS;
		firstid = whichmenu;
	}
   else
	{
		firstid = mid;
		whichmenu = rectindex[mid];
	}
   if (gisgraph)
   {
		FEnableMouseNest(FALSE) ;

		lRect.xLeft =    menurect[whichmenu].axLeft * CWIDTH;
		lRect.yTop  =    menurect[whichmenu].ayTop * CHEIGHT;
		lRect.xRight =  (menurect[whichmenu].axRight -1) * CWIDTH;
		lRect.yBottom = menurect[whichmenu].ayBottom *CHEIGHT;

		GetIsaColor(isaBorders,&fore,&back);
		MySetColor(fore);
		SetAreaPat(0);
		SetLinePat(1);
		Rectangle(&lRect);
		
		if(firstid != whichmenu)
		{
			while(rectindex[firstid-1] != rectindex[firstid])
				--firstid;
		}
		else
		{
			for(i=0;i<NUMMENUS;i++)
			{
				if(rectindex[i] == (BYTE) whichmenu)
				{
					firstid = i;
					break;
				}
			}
		}               
		for(i=0;i<menurect[whichmenu].ayBottom - menurect[whichmenu].ayTop;i++)
		{
			if(emenustate[firstid+i-1] & MM_SEPARATOR)
			{
				Move(lRect.xLeft,lRect.yTop+i*CHEIGHT+CHEIGHT/2);
				Draw(lRect.xRight-1,lRect.yTop+i*CHEIGHT+CHEIGHT/2);
			}
		}
		FrameMenuBar(pwnd);
		FEnableMouseNest(TRUE) ;
   }
}


VOID FrameMenuBar(PWND pwnd)
{
   WORD fore,back;
   UnReferenced(pwnd) ;

   if((gisgraph)&&(CHEIGHT > SMALLHEIGHT))
   {
       FEnableMouseNest(FALSE);
       SetAreaPat(0);
       SetLinePat(1);
       GetIsaColor(isaBorders,&fore,&back);
       MySetColor(fore);
       Move(0,CHEIGHT-1);
       Draw(axMac*CWIDTH-1,CHEIGHT-1);
       Move(0,2*CHEIGHT);
       Draw(axMac*CWIDTH-1,2*CHEIGHT);
       FEnableMouseNest(TRUE);
   }
}


VOID DoInitMenus()
/*
 *  Construct menus.
 *  MenuBar is the main menu
 *  OtherMenu is an extended menu (example)
 */
{
	/* Set up the Main menu */

	initbar(&MainMenuBar);

	easymenu(&MainMenuBar,szmFile,0);
	gPMfilestart = gNthmenu;
	easyitem(&MainMenuBar,szmNew,0,AddProgram);
	easyitem(&MainMenuBar,szmOpenEnter,0,StartAProgram);
	easyitem(&MainMenuBar,szmCopy,0,CopyProgram);
	easyitem(&MainMenuBar,szmPMDelete,0,DeleteProgram);
	easyitem(&MainMenuBar,szmProperties,0,ChangeProgram);
	easyitem(&MainMenuBar,szmReorder,0,ReorderGroup);

	easyseparator(&MainMenuBar);
	easyitem(&MainMenuBar, szmRun, 0, Run);
	easyseparator(&MainMenuBar);

	easyitem(&MainMenuBar,szmPMExit,1,DoExit);

	easymenu(&MainMenuBar, szmOptions,0);
	easyitem(&MainMenuBar, szmFileOptions, 0, DoFileOptions);
	easyitem(&MainMenuBar, szmDispOptions, 0, DisplayOptions);
	easyitem(&MainMenuBar, szmSelectAcrossDirectories,0, SelectAcrossDirs);
	easyitem(&MainMenuBar, szmShowInfo, 0, ShowInfo);
	easyitem(&MainMenuBar, szmEnableSwitching,0,EnableSwitching);
	//easyitem(&MainMenuBar, szmDisableSwitching,0,DisableSwitching);
	//easyitem(&MainMenuBar, szmEnableTaskList,0,EnableTaskList);
	//easyitem(&MainMenuBar, szmDisableTaskList,0,DisableTaskList);
	easyitem(&FileMgrMenuBar, szmChangeScreen, 0, ScreenBox);
	easyitem(&FileMgrMenuBar, szmChangeColors, 0, ColorBox);

	easymenu(&MainMenuBar, szmView,0);

	/* View Menu */
	easyitem(&MainMenuBar, szmSingleFile, 0, PDoSingleTree);
	easyitem(&MainMenuBar, szmDualFile, 0, PDoDoubleTree);
	easyitem(&MainMenuBar, szmSystemFile, 3, PDoFlatDisplay);
#ifndef NOGROUPINFILE
	easyitem(&MainMenuBar,szmProgMan, 8, PDoShareMode);
#endif
	easyitem(&MainMenuBar,szmSoloProgMan, 0,ExitFileMgr);
	easyseparator(&MainMenuBar);
	easyitem(&MainMenuBar, szmRepaint, 1, DoFullRefresh);

	easymenu(&MainMenuBar, szmHelp, 0);
	easyitem(&MainMenuBar, szmHelpIndex,0,IndexBox);
	gPMMagicShiftF9 = gNthmenu;
	easyitem(&MainMenuBar, szmHelpKeys,0,KeyBox);
	gPMMagicAltTab = gNthmenu;
	easyitem(&MainMenuBar, szmHelpBasics,0,BasicsBox);
	gPMMagicTab = gNthmenu;
	easyitem(&MainMenuBar, szmHelpCommands,0,CommandsBox);
	gPMMagicShiftTab = gNthmenu;
	easyitem(&MainMenuBar, szmHelpProcedures,0,ProceduresBox);
	easyitem(&MainMenuBar, szmHelpF1,0, UsingHelpBox);
	easyseparator(&MainMenuBar);
	easyitem(&MainMenuBar, szmHelpAbout,0,ShellAboutBox);

/** initialize filemanager menus*/
	initbar(&FileMgrMenuBar);

	easymenu(&FileMgrMenuBar, szmFile, 0);
	gFMfilestart = gNthmenu;

	easyitem(&FileMgrMenuBar, szmOpen, 0, LaunchBox);
	easyitem(&FileMgrMenuBar, szmRun, 0, Run);
	easyitem(&FileMgrMenuBar, szmPrint, 0, DoPrintFiles);
	easyitem(&FileMgrMenuBar, szmAssociate, 0, DoAssociateBox);
	easyitem(&FileMgrMenuBar, szmSearchFor, 0, FileLocateBox);
	easyitem(&FileMgrMenuBar, szmViewF9, 0, DoViewFile);
	easyseparator(&FileMgrMenuBar);
	easyitem(&FileMgrMenuBar, szmMove, 0, DoMoveFiles);
	easyitem(&FileMgrMenuBar, szmCopyF8, 0, DoCopyFiles);
	easyitem(&FileMgrMenuBar, szmFMDelete, 0, DoDelFiles);
	easyitem(&FileMgrMenuBar, szmRename, 0, DoRenameFiles);
	easyitem(&FileMgrMenuBar, szmChangeAttr, 1, DoChangeAttributes);
	easyseparator(&FileMgrMenuBar);
	easyitem(&FileMgrMenuBar, szmCreateDir, 2, DoCreateDirectory);
	easyseparator(&FileMgrMenuBar);
	easyitem(&FileMgrMenuBar, szmSelectAll, 0, DoSelectAll);
	easyitem(&FileMgrMenuBar, szmDeselectAll, 4, DoDeselectAll);
	easyseparator(&FileMgrMenuBar);
	easyitem(&FileMgrMenuBar, szmFMExit, 1, DoExit);


	easymenu(&FileMgrMenuBar, szmOptions,0);
	easyitem(&FileMgrMenuBar, szmFileOptions, 0, DoFileOptions);
	easyitem(&FileMgrMenuBar, szmDispOptions, 0, DisplayOptions);
	easyitem(&MainMenuBar, szmSelectAcrossDirectories,0, SelectAcrossDirs);
	easyitem(&FileMgrMenuBar, szmShowInfo, 0, ShowInfo);
	easyitem(&FileMgrMenuBar, szmEnableSwitching,0,EnableSwitching);
	//easyitem(&FileMgrMenuBar, szmDisableSwitching,0,DisableSwitching);
	//easyitem(&FileMgrMenuBar, szmEnableTaskList,0,EnableTaskList);
	//easyitem(&FileMgrMenuBar, szmDisableTaskList,0,DisableTaskList);
	easyitem(&FileMgrMenuBar, szmChangeScreen, 0, ScreenBox);
	easyitem(&FileMgrMenuBar, szmChangeColors,0,ColorBox);

	easymenu(&FileMgrMenuBar, szmView,0);

	easyitem(&FileMgrMenuBar, szmSingleFile, 0, DoSingleTree);
	easyitem(&FileMgrMenuBar, szmDualFile, 0, DoDoubleTree);
	easyitem(&FileMgrMenuBar, szmSystemFile, 3, DoFlatDisplay);
#ifndef NOGROUPINFILE
	easyitem(&FileMgrMenuBar,szmProgMan, 8, DoShareMode);
#endif
	easyitem(&MainMenuBar,szmSoloProgMan, 0, ExitFileMgr);
	easyseparator(&FileMgrMenuBar);
	easyitem(&FileMgrMenuBar, szmRepaint, 1, DoFullRefresh);
	easyitem(&FileMgrMenuBar, szmRefresh, 0, DoDiskReread);


	easymenu(&FileMgrMenuBar, szmTree,0);
	easyitem(&FileMgrMenuBar, szmExpOne, 1, DoExpand1Level);
	easyitem(&FileMgrMenuBar, szmExpBranch, 7, DoExpandBranch);
	easyitem(&FileMgrMenuBar, szmExpAll, 7, DoExpandAll);
	easyitem(&FileMgrMenuBar, szmCollBranch, 0, DoCollapse);

	easymenu(&FileMgrMenuBar, szmHelp, 0);
	easyitem(&FileMgrMenuBar, szmHelpIndex,0,IndexBox);
	gFMMagicShiftF9 = gNthmenu;
	easyitem(&FileMgrMenuBar, szmHelpKeys,0,KeyBox);
	gFMMagicAltTab = gNthmenu;
	easyitem(&FileMgrMenuBar, szmHelpBasics,0,BasicsBox);
	gFMMagicTab = gNthmenu;
	easyitem(&FileMgrMenuBar, szmHelpCommands,0,CommandsBox);
	gFMMagicShiftTab = gNthmenu;
	easyitem(&FileMgrMenuBar, szmHelpProcedures,0,ProceduresBox);
	gFMMagicCtrlF5 = gNthmenu;
	easyitem(&FileMgrMenuBar, szmHelpF1,0, UsingHelpBox);
	easyseparator(&FileMgrMenuBar);
	easyitem(&FileMgrMenuBar, szmHelpAbout,0,ShellAboutBox);

	/* Menu Bar with only "Help" menu in it */
	initbar(&ViewMenuBar);

	easymenu(&ViewMenuBar, szmDisplay, 0);
	gViewstart =  gNthmenu;
	easyitem(&ViewMenuBar,szmAscii,0,AsciiFileView);
	easyitem(&ViewMenuBar,szmHex,0,HexFileView);

	easymenu(&ViewMenuBar, szmView,0);
	easyitem(&ViewMenuBar, szmRepaint, 1, DoFullRefresh);
	easyitem(&ViewMenuBar,szmRestoreView,0,ExitFileView);

	easymenu(&ViewMenuBar, szmHelp, 0);
	easyitem(&ViewMenuBar, szmHelpIndex,0,IndexBox);
	easyitem(&ViewMenuBar, szmHelpKeys,0,KeyBox);
	easyitem(&ViewMenuBar, szmHelpBasics,0,BasicsBox);
	easyitem(&ViewMenuBar, szmHelpCommands,0,CommandsBox);
	easyitem(&ViewMenuBar, szmHelpProcedures,0,ProceduresBox);
	easyitem(&ViewMenuBar, szmHelpF1,0,UsingHelpBox);
	easyseparator(&ViewMenuBar);
	easyitem(&ViewMenuBar, szmHelpAbout,0,ShellAboutBox);

	/* ZZZZZZ Following two fn calls should not be needed before shipping!!
	 * as we should not have NULL functions then!!
	 */
	/* Disable all menu items which have NULL fn to perform tasks!! */
	easyenable(&MainMenuBar,NULL,FALSE);
	easyenable(&FileMgrMenuBar,NULL,FALSE);

	InitAccelerators();
}

/* Returns a BOOL as to whether the FileManager menu is put up! This is
 * especially important only when we are in TR_SHARE mode, where we have
 * both the File Manager and Program Manager displayed on the screen.
 */ 
BOOL IsFMMenuPutUp(void)
{
	BOOL ret ;

	if (!glob.InFileMgr)
		ret = FALSE ;
	else
	{
		if (glob.TreeMode == TR_SHARE)
		{
			ret = (WhoHasGlobalFocus() == DRIVELIST0) ||
					(WhoHasGlobalFocus() == TREE0) ||
					(WhoHasGlobalFocus() == FILE0) ;
		}
		else
			ret = TRUE ;
	}

	return ret ;

} /* IsFMMenuPutUp  */

extern ListBoxData TaskList;
/* BUG BUG you know, easyenable ignores its first parameter, so
 * we don't need all these routines separate...
 */

void TaskMenuEnable(void)
{
	BOOL allowexit;
	BOOL allowswitchmodechange;

	allowexit = !(gTaskListEnabled && (GetNumItems(&TaskList) > 0));
	allowswitchmodechange = (!gAlreadyTasking) && (allowexit);

	/* We now always keep the Exit menu item enabled. We check and
	 * warn the user that he has tasks running in case we shouldn't
	 * allow him to exit and he chooses the exit menu item.
	 */
	// easyenable(&FileMgrMenuBar,DoExit,allowexit);
	easyenable(&FileMgrMenuBar, DoExit, TRUE);
	easyenable(&FileMgrMenuBar,EnableSwitching,allowswitchmodechange);
}

/* This function, enables/disables the following file operations:
*** move, copy, delete, rename, change-attribute, view,
*** select-all, deselect-all.
*** create-directory is always on! 
*** delete and rename are on even when no files are selected if focus is
***     not on the root directory in tree list box.
***     open, associate also handled.
*/
/* ZZZZ Maybe delete should not be enabled if no files selected
   and directory with focus isn't empty -- What is the fastest way to tell
   this?? */

void EnableDisableFileOps(void)
{
	PTREE tree ;
	PENTRY dir ;
	BOOL prmvcpchas, del, ren, viop, sall, dall ;
	BOOL creatdir ;
	unsigned count ;
	BOOL fIsDirOperation ;
	// extern BOOL PDisable ; /* Says if print.com is not resident */

	tree = listinfo[glob.FocusBox].tree ;
	dir  = listinfo[glob.FocusBox].files ;
	
	fIsDirOperation = FPerformDirOperation() ;

	creatdir = FALSE ;

	if (fIsDirOperation)
	{
		prmvcpchas = viop = sall = dall = FALSE ;
		del = ren = (dir != NULL) ;
		if (    (glob.TreeMode == TR_SINGLE) ||
				(glob.TreeMode == TR_DOUBLE) ||
				(glob.TreeMode == TR_SHARE)
			)
			creatdir = !(tree->ContinueTree) ; // tree is built completely.
	}
	else
	{
		count = CountFilesToOperateOn() ;
		del = ren = (count > 0) ;
		viop = (count == 1) ;
		prmvcpchas = dall = (count > 0) ;
		sall = !AreAllMatchedSelected(dir, tree) ;
	}

	easyenable(&FileMgrMenuBar, LaunchBox, viop) ;

	// easyenable(&FileMgrMenuBar, DoPrintFiles, (!PDisable) && prmvcpchas) ;
	/* We now always have print menu item enabled if there are selected files.
	 * The Print function decides whether "print.com" has been loaded or not
	 * and then decides to print or puts up its message.
	 */
	easyenable(&FileMgrMenuBar, DoPrintFiles, prmvcpchas) ;


	easyenable(&FileMgrMenuBar, DoAssociateBox, prmvcpchas) ;

	easyenable(&FileMgrMenuBar, DoMoveFiles, prmvcpchas) ;
	easyenable(&FileMgrMenuBar, DoCopyFiles, prmvcpchas) ;
	easyenable(&FileMgrMenuBar, DoChangeAttributes, prmvcpchas) ;

	easyenable(&FileMgrMenuBar, DoDelFiles, del) ;
	easyenable(&FileMgrMenuBar, DoRenameFiles, ren) ;

	easyenable(&FileMgrMenuBar, DoViewFile, viop) ;

	easyenable(&FileMgrMenuBar, DoDeselectAll, dall) ;
	easyenable(&FileMgrMenuBar, DoSelectAll, sall) ;

	EnableDisableTreeMenu() ;
	if (glob.TreeMode != TR_SEARCH)
	{
		/* Following enables: Search For, Display-options,
		 *  Arrange-options.
		 */
		EnableDisableForSearchMode(TRUE, TRUE) ;

		/* Note that in case the menu was pulled down while tree was being
		 * built, we would have disable Flat Display mode, so enable it now.
		 * the code below will disable it if that is the case!
		 */
		easyenable(&FileMgrMenuBar, DoFlatDisplay, TRUE) ;

		easyenable(&FileMgrMenuBar, DoFileOptions, TRUE) ;

		/* At this point the prev. call would have enabled all the arrange-opts
		*  We need to disable the appropriate one based on which mode we are in.
		*/
		switch (glob.TreeMode)
		{
			case TR_SINGLE:
				easyenable(&FileMgrMenuBar, DoSingleTree, FALSE) ;
				easyenable(&FileMgrMenuBar, ShowInfo, TRUE) ;
				break ;
			case TR_DOUBLE:
				easyenable(&FileMgrMenuBar, DoDoubleTree, FALSE) ;
				easyenable(&FileMgrMenuBar, ShowInfo, TRUE) ;
				break ;
			case TR_SYSTEM:
				easyenable(&FileMgrMenuBar, DoFlatDisplay, FALSE) ;
				easyenable(&FileMgrMenuBar, ShowInfo, FALSE) ;
				break ;
#ifndef NOGROUPINFILE
			case TR_SHARE:
				easyenable(&FileMgrMenuBar, DoShareMode, FALSE) ;
				easyenable(&FileMgrMenuBar, ShowInfo, TRUE) ;
				break;
#endif

		} /* switch */
	}
	else
		EnableDisableForSearchMode(FALSE, TRUE) ;

	/* allow going back to PM */
	easyenable(&MainMenuBar,ExitFileMgr,TRUE);

	/* CreateDirectory is a special case -- Disable it if the listbox
	 * with focus is still being read in.
	 */
	easyenable(&FileMgrMenuBar, DoCreateDirectory, creatdir) ;

	TaskMenuEnable();
	easycheck(EnableSwitching,gSwitchingEnabled);

	/* The following item could have been disabled if one went to the plain
	 * PM view. So, re-enable it now!
	 */
	easyenable(&FileMgrMenuBar, SelectAcrossDirs, TRUE) ;
	easycheck(SelectAcrossDirs, glob.CrossDirSel) ;

	/* ZZZZZZZZ */
	/* WARNING!!! MAJOR HACK to get the keyboard accelerators
	 * to work right! If the last thing we did is a disable
	 * menu item rather than a enable menu item, CW seems
	 * to set a flag to disable KeyBoard accelerators from
	 * then on -- Don't know why!!!!!!
	 */
	easyenable(&FileMgrMenuBar, HelpBox, TRUE) ;

} /* EnableDisableFileOps */

extern TOKEN Get_Focus_Item(void);


void EnableDisablePMMenu(void)
{
	TOKEN curitem;
	BOOL fEnable, bDoor ;

	/* Enable/Disable all the "Options"/ "view" menu items based on
	 * whether we are in TR_SHARE mode or in basic program manager mode.
	 */
	fEnable = (glob.InFileMgr) && (glob.TreeMode == TR_SHARE) && 
																	(!IsTreeBeingBuilt()) ;
	easyenable(&MainMenuBar, DoFileOptions, fEnable) ;
	easyenable(&MainMenuBar, DisplayOptions, fEnable) ;
	easyenable(&MainMenuBar, SelectAcrossDirs, fEnable) ;
	easyenable(&MainMenuBar, PDoShareMode, !glob.InFileMgr) ;
	easyenable(&MainMenuBar, ExitFileMgr, glob.InFileMgr) ;

	/* In case the menu was crippled, i.e., the menu was pulled down when the
	 * tree was being built, "All Files" display is greyed out! Re-enable it
	 */
	easyenable(&MainMenuBar, PDoFlatDisplay, TRUE) ;

	/* When we are in PM menu, don't have ShowInfo ever activated! */
	easyenable(&MainMenuBar, ShowInfo, FALSE) ;

	if(gTaskListEnabled && TaskList.hasglobalfocus) // focus is in task list
	{
			easyenable(&MainMenuBar, AddProgram, FALSE);
			// never disabled easyenable(&MainMenuBar, StartAProgram, TRUE);
			easyenable(&MainMenuBar, CopyProgram, FALSE);
			// never disabled easyenable(&MainMenuBar, DeleteProgram, TRUE);
		easyenable(&MainMenuBar, ChangeProgram, FALSE);         
		easyenable(&MainMenuBar, ReorderGroup, FALSE);          
	}
	else
	{
		if((curitem=Get_Focus_Item()) > 0) {
			bDoor = gGroupLevel!=TK_PROGRAMSTARTER &&
					!Get_List_Focus(&ProgramList);

			easyenable(&MainMenuBar, CopyProgram, curitem!=TK_PROGRAMSTARTER &&
					Get_Symbol_Type(Token_To_Symbol(curitem))!=TK_GROUP);

			easyenable(&MainMenuBar, DeleteProgram, !bDoor);

			easyenable(&MainMenuBar, ChangeProgram, curitem!=TK_PROGRAMSTARTER);

			easyenable(&MainMenuBar, ReorderGroup, !bDoor);
		 
			easyenable(&MainMenuBar, AddProgram, TRUE);             
		}
	}
	/* If there are tasks switched out, grey exit menu! */

	TaskMenuEnable();
	easycheck(EnableSwitching,gSwitchingEnabled);
	easycheck(SelectAcrossDirs, glob.CrossDirSel) ;

/* ZZZZZZZZ */
	/* WARNING!!! MAJOR HACK to get the keyboard accelerators
	 * to work right! If the last thing we did is a disable
	 * menu item rather than a enable menu item, CW seems
	 * to set a flag to disable KeyBoard accelerators from
	 * then on -- Don't know why!!!!!!
	 */

	easyenable(&MainMenuBar, HelpBox, TRUE) ;

} /* EnableDisablePMMenu */

/* Enable/Disable Menu items in the View File menu */
void EnableDisableViewMenu(void)
{
	easyenable(&ViewMenuBar, AsciiFileView, !(ascii)) ;
	easyenable(&ViewMenuBar, HexFileView, ascii) ;
} /* EnableDisableViewMenu */

/* This function enables/disables the menu operations in the file-manager
   when entering/leaving the search mode.
   ('doenable1', 'doenable2') == TRUE implies we are leaving the search-mode
   and want to re-enable the menus. If both are FALSE, it means we are
   entering search-mode and want to disable them all.
   Using just 'doenable1' == FALSE, 'doenable2' = TRUE, allows separate
   enabling/disabling "Arrange Menu" and the other two ("File", "Options").
   This kind of generic feature has been used to reduce code size!!
*/
  
void EnableDisableForSearchMode(BOOL doenable1, BOOL doenable2)
{
	/* Re-Think of design indicates that Search should work in search */
	/* FileLocateBox (search) can now handle search from within search */
	easyenable(&FileMgrMenuBar, FileLocateBox, TRUE) ;

	/* "Options" menu */
	easyenable(&FileMgrMenuBar, DisplayOptions, doenable1) ;

	/* "Arrange" menu */
	easyenable(&FileMgrMenuBar, DoSingleTree, doenable2) ;
	easyenable(&FileMgrMenuBar, DoDoubleTree, doenable2) ;
	easyenable(&FileMgrMenuBar, DoFlatDisplay, doenable2) ;
#ifndef NOGROUPINFILE
	easyenable(&FileMgrMenuBar, DoShareMode, doenable2) ;
#endif

} /* EnableDisableForSearch */


/*
*  This function is called when File System is still being read in --
*  Disable all menus except run...,exit, view menu and help menu
*/
void CrippleFileMenuBar(void)
{
	int i;

	/* ZZZZZZ We can be smart about these disabling operations by looking
	 * at what menu we have put up -- FM-Menu or PM-Menu. This is based
	 * on who has focus when we are in TR_SHARE mode.
	 */

/* ZZZZ If file manager menubar is changed, one may have to change the foll.
 * constants.
 */
#define BEGIN_DISABLE_MENUID 32 /* menu-id of "Open" menu item */
#define END_DISABLE_MENUID 70           /* menu-id of first help menu item */
	/* I am making use of the fact that the menu item id's are of the form j,
	 * j+1, j+2, etc. The fol1lowing makes code size pretty small -- only
	 * 1 loop as opposed to calling the 'easyeanble' for each individual item.
	 * We need to disable almost all file manager operations when the tree
	 * is being built!
	 */
	for(i = BEGIN_DISABLE_MENUID; i< END_DISABLE_MENUID ; i++)
	{
		 EnableMenuItem(i, FALSE);
		 /* Mark this menu item as disabled! -- This is used by us to
		  * to prevent keyboard accelerators from performing operations when
		  * the corresponding menu item is disabled
		  */
		 emenustate[i] |= MM_DISABLED;
	}

	/*      We always want to be able to use "Run", "Refresh" from the file-mgr menu! */
	easyenable(&FileMgrMenuBar, Run, TRUE) ;

	/* Scott wanted the following change, so I have implemented this!               */
	/* The following chain would enable user to read 2 trees at a time, etc */
	if (glob.TreeMode != TR_SINGLE)
		easyenable(&FileMgrMenuBar, DoSingleTree, TRUE) ;

	if (glob.TreeMode != TR_DOUBLE)
		easyenable(&FileMgrMenuBar, DoDoubleTree, TRUE) ;

	easyenable(&FileMgrMenuBar, ExitFileMgr, TRUE) ;

#ifndef NOGROUPINFILE
	if (glob.TreeMode != TR_SHARE)
		easyenable(&FileMgrMenuBar, DoShareMode, TRUE) ;
#endif

	/* ZZZZ */
	/* When the tree is being built -- this is the time when this function
	 * is ever invoked, we don't want to allow the user to switch to 
	 * System Tree modfe, as there is a potential for getting into compact
	 * mode and it is a big pain to get out of System Tree mode!
	 */

	/* Disable exit if there are tasks swapped out ! */
	TaskMenuEnable();

	/* enable some of the Harmless FM menu items */
	easyenable(&FileMgrMenuBar, SelectAcrossDirs, TRUE) ;
	easyenable(&FileMgrMenuBar, DoFileOptions, TRUE) ;
	easyenable(&FileMgrMenuBar, ScreenBox, TRUE) ;
	easyenable(&FileMgrMenuBar, ColorBox, TRUE) ;
	easyenable(&FileMgrMenuBar, DoFullRefresh, TRUE) ;
	easyenable(&FileMgrMenuBar, DoDiskReread, TRUE) ;

	/* Don't let user to switch to all tree mode when tree being built! */
	easyenable(&MainMenuBar, PDoFlatDisplay, FALSE) ;

	/* We must already be in SHARE mode, if this menu item greying is seen
	 * by the user.
	 */
	easyenable(&MainMenuBar, PDoShareMode, FALSE) ;
	easyenable(&MainMenuBar, DisplayOptions, FALSE) ;
	easyenable(&MainMenuBar, ShowInfo, FALSE) ;

	easycheck(EnableSwitching,gSwitchingEnabled);
	easycheck(SelectAcrossDirs, glob.CrossDirSel) ;

} /* CrippleFileMenuBar */



/* This routine enables/disables the four menu items under the 'Tree' menu
 * that deals with collapsed directories
 */
void EnableDisableTreeMenu(void)
{
	PTREE tree ;
	BOOL fDoIt ; /* whether to find out about enabling/disabling */
	BOOL col, exp1, expall ;
	unsigned char exp_ch ;

	tree = listinfo[glob.FocusBox].tree ;

	/* In Search mode/System mode, or if tree with focus is still being read in,
	 * we want disable ALL of the following!
	 */
	fDoIt = (glob.TreeMode != TR_SEARCH) && (glob.TreeMode != TR_SYSTEM) &&
			(!(tree->ContinueTree)) ;
		
	if (fDoIt)
	{
		exp_ch = GetCollapseChar(tree, listinfo[glob.FocusBox].files) ;

		col = (exp_ch == COLLAPSE_CHAR) ;
		exp1 = (exp_ch == EXPANSION_CHAR) ;

		/* actually '<' is a strong check -- '!=' should be good enuf!! */
		expall = (tree->VisibleDirCount < tree->DirCount) ;
	}
	else
		exp1 = expall = col = FALSE ;
	
	easyenable(&FileMgrMenuBar, DoExpand1Level, exp1) ;

	/* ZZZZZZZ Figuring out foll. exactly will make disabling menu slow, 
	 * so not done! - Not very important either.
	 */
	easyenable(&FileMgrMenuBar, DoExpandBranch, 
							(exp_ch != NOZOOM_CHAR) && expall && fDoIt);

	easyenable(&FileMgrMenuBar, DoExpandAll, expall) ;
	easyenable(&FileMgrMenuBar, DoCollapse, col) ;
} /*EnableDisableTreeMenu */
