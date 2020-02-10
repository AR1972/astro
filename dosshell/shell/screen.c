/* Revision history since 5.0 Golden:
 *
 *  M011 SHK 07/17/91 New mouse compatibilty message added. This
 *							 now displays the mouse version number also.
 *  M014 SHK 10/03/91 Mouse Version number in the "old-mouse"
 *							 warning	message is printed in hex now!
 *
 */


;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>
#include <groups.h>
#include <icons.h>
#include <text.h>
#include <assert.h>

extern BOOL gInReorder, gInCopy;
extern BOOL gTaskListEnabled;
extern BOOL gSwitchingEnabled;
extern WORD gCnx;

extern MENUINFO ViewMenuBar ;

long far pascal WindProc(PWND, WORD, WORD, DWORD);
extern WND ViewWind;                            // window used by view file
extern MENUINFO MainMenuBar;
extern MENUINFO FileMgrMenuBar;
extern void EnableDisablePMMenu(void) ;
extern void EnableDisableViewMenu(void) ;
extern VOID RefreshStartPrograms(VOID);
extern void ClearHelpContext(void) ;

extern WORD gscreenmode; /* mode the screen is in now */

extern BOOL gfDisplayDelayedHelp ;

#define INVALID_TMC 0

#if 0 
	not needed
/* In edit boxes the one which initially has focus so that we can
 * draw it in hilited mode.
 */
TMC gInitialHiliteTMC = INVALID_TMC ;
#endif 

/* The sizes 80, 23 don't really matter -- See InitWindows() which initializes
   the size of MainWind. The following macro call is only to get a window */
WND MainWind = wndGeneric(1,WS_TILED /*| WS_BORDER */,TRUE,0,0,80,23,
			 WindProc,&ViewWind, NULL, NULL)
   endWndGeneric;

#define CISA  (dmNormal | 20)
#define MAXUSERPASSWORD 20

BOOL gMouseDown;
BYTE gMouseX;   /* X value of last mouse-down */
BYTE gMouseY;   /* Y value of last mouse-down */
BYTE gMouseDownX, gMouseDownY;
WORD gAutoMouseRatio;
WORD gAutoMouse=0;
BOOL gfRepaintonidle=FALSE;
TMC gPrevTmc ;
TMC gNextTmc ;

extern void AddProgManAccelerators(void);
extern void AddFileManAccelerators(void);

extern VOID strfcat(char far *dest,char far *src);
extern char NullString[];
extern char szViewFileCaption[] ;

char *szCurDialogCaption = NullString ;
extern BOOL fDrawItem;
extern BOOL fRedrawItem;
extern void UpLevel(void);

TMC gCurRadioGroup;

#define DONTTURNONMOUSE -99
int gNestCount = DONTTURNONMOUSE; /* This has to be a signed value! */

extern TMC WarnMouseIsOld(char *pszMouseVersion); // M011
extern BOOL ErrorCrit;
extern TOKEN Get_Identifier_Token(char far *identifier);

#define MINMAJORMOUSEVER 6
#define MINMINORMOUSEVER 21
/* logictech number is returned in dx:ax, so version 5.01 is 0005:0001 */
#define MINLOGITECHMAGICVERSION 0x050001

extern WORD MouseVersion(void);
extern DWORD DetectLogitech(void);

VOID FirstMouseInit(void)
{
	WORD thisversion;
	TOKEN lastresponse;
	char far *lastresstr;
	char thisvertemp[20];

	if(fMousePresent)
	{
		thisversion = MouseVersion();
		
	   if( (HIBYTE(thisversion) < MINMAJORMOUSEVER) ||
		  ((HIBYTE(thisversion) == MINMAJORMOUSEVER) && (LOBYTE(thisversion) < MINMINORMOUSEVER)))
	   {

		   lastresponse = Get_KeyWord_Assignment(TK_SAVESTATE,TK_MOUSEINFO);
			if(lastresponse > 0)
			{
				lastresstr = Get_Token_Identifier(lastresponse);

				if( ((lastresstr[0]-'0') == (HIBYTE(thisversion))) &&
				  ( (lastresstr[2]-'0') == (LOBYTE(thisversion)/10)) &&
				  ( (lastresstr[3]-'0') == (LOBYTE(thisversion)%10)) )
				  {
						/* versions match */
					   if(lastresstr[5]=='i')
						{
							gNestCount = 0;
						}
						return;
						
				  }
			}



			thisvertemp[0] = HIBYTE(thisversion)+'0';
			thisvertemp[1] = '.';

			/* M014 -- MS mice print out the version number in base 16.
			 * We do the same here, so that people see the same value!
			 */
			thisvertemp[2] = LOBYTE(thisversion)/16+'0'; // M014
			thisvertemp[3] = LOBYTE(thisversion)%16+'0'; // M014


			thisvertemp[4] = 0;	// M011


			ErrorCrit = 34;  // BUG BUG set to keep tasking out until done here

			/* ok, looks like a baddy to us, but logitech works
			 * on 5.01+, so detect it. Do this as rarely as possible
			 * since the code is really really dirty
			 */

			// M011 Changed the structure to make it cleaner and without a GOTO!
			if (	(DetectLogitech() >= MINLOGITECHMAGICVERSION) ||
				 	(WarnMouseIsOld(thisvertemp) != tmcCancel) )
			{
				gNestCount = 0;
				thisvertemp[4] = ',';	// M011
				thisvertemp[5] = 0;		// M011
				strfcat(thisvertemp,Get_Token_Identifier(TK_IGNORE));

			}
		   else
			{
				thisvertemp[4] = ',';	// M011
				thisvertemp[5] = 0;		// M011
			   strfcat(thisvertemp,Get_Token_Identifier(TK_DISABLED));
			}
			Set_KeyWord_Assignment(TK_SAVESTATE,TK_MOUSEINFO,Get_Identifier_Token(thisvertemp));
			ErrorCrit = 0xFF;       
	  }
	  else
	  {
		  gNestCount = 0;
		  if(Get_KeyWord_Assignment(TK_SAVESTATE,TK_MOUSEINFO) > 0)
			Set_KeyWord_Assignment(TK_SAVESTATE,TK_MOUSEINFO,TK_NOTHING);
	  }
	}
}

VOID FInitMouseNest(void)
{
	 if(gNestCount == DONTTURNONMOUSE)
		return;
    gNestCount = 0;
    FEnableMouse(TRUE);
} /* FInitMouseNest */


/*
 * This call enables and disables the mouse.
 * It nests calls so the mouse will not flicker too much
 */
VOID FEnableMouseNest(BOOL onoroff)
{
	if(gNestCount == DONTTURNONMOUSE)
		return;  // finitmousenext has not been called yet.
				   // we use this "feature" at start-up to delay mouse turn on
	if (onoroff)
	{
		--gNestCount;
		if(gNestCount <= 0)
		{
			FInitMouseNest() ;
		}
	}
	else
	{
		++gNestCount;
		if(gNestCount == 1)
		{
			FEnableMouse(FALSE);
		}
	}
} /* FEnableMouseNest */

#ifndef NORESIZE
void EasyDrawBox(PWND pwd, BYTE top, BYTE left, BYTE bottom, BYTE right,ISA isa);
BOOL gDraggingrect;
BYTE gDragX;
BYTE gDragY;
BYTE gLinebuf[500];
VOID DoBeginResize(BYTE x, BYTE y)
{
    RRC trect;

    gDraggingrect = TRUE;
    gDragX = x;
    gDragY = y;

	trect.ryTop = y ;
	trect.ryBottom = y+1 ;
    trect.rxLeft = 0;
    trect.rxRight = axMac;

	SaveRrc(&MainWind,&trect,gLinebuf);
    FillRrc(&MainWind,&trect,' ',dmAttrOnly|isaBlackOnWhite);
}
VOID DoResize(BYTE x, BYTE y)
{
    RRC trect;

    trect.ryTop = gDragY;
    trect.ryBottom = gDragY+1;
    trect.rxLeft = 0;
    trect.rxRight = axMac;

	RestoreRrc(&MainWind, &trect, gLinebuf);

    gDragX = x;
    gDragY = y;
    trect.ryTop = gDragY;
    trect.ryBottom = gDragY+1;
    trect.rxLeft = 0;
    trect.rxRight = axMac;

	SaveRrc(&MainWind,&trect,gLinebuf);
    FillRrc(&MainWind,&trect,' ',dmAttrOnly|isaBlackOnWhite);

}
VOID DoEndResize(BYTE x, BYTE y)
{
    RRC trect;

    trect.ryTop = gDragY;
    trect.ryBottom = gDragY+1;
    trect.rxLeft = 0;
    trect.rxRight = axMac;

	RestoreRrc(&MainWind,&trect,gLinebuf);
    gDraggingrect = FALSE;
}
#endif



#if 0
//VOID MyAltMenu(void);
//BOOL NEAR HiliteMenuMnem(BOOL);
//#pragma alloc_text(CW_USER,MyAltMenu)
BOOL gDoAltMenu = FALSE;
VOID MyAltMenu(void)
{
      gDoAltMenu = TRUE;
}
#endif

/* Assigns the correct value to the focus box "glob.FocusBox" -- This is
 * the focus box (0/1) that will be used to figure out the default
 * directory to copy or do a SelectAll, etc.
 */
void SetGlobalFocusBox(WORD GlobalFocus)
{
	if (glob.TreeMode == TR_DOUBLE)
	{
		switch (GlobalFocus)
		{
			case FILE1:
			case TREE1:
			case DRIVELIST1:
				glob.FocusBox = 1 ;
				break ;
			default:
				glob.FocusBox = 0 ;
		} /* switch */
	}
	else
		glob.FocusBox = 0 ;
} /* SetGlobalFocusBox */


BYTE GetFocusDrive(WORD list) ;
extern struct ListBoxData ProgramList;
extern char *gpszNonSwap ;

/* Fn to mark the Global Focus as being with the one passed in. Currently
 * called only from InitFileMgr()
 */
void MarkGlobalFocus(WORD focus)
{
	glob.FocusId = focus ;
} /* MarkGlobalFocus */

#ifndef NOSWITCHER
extern struct ListBoxData TaskList;
#endif
VOID InitGlobalFocus(WORD focusstart)
{
	SetGlobalFocusBox(glob.FocusId = focusstart) ;

	GlobalFocusBox(&TreeList[0],FALSE);
	GlobalFocusBox(&FileList[0],FALSE);
	if (glob.TreeMode == TR_DOUBLE)
	{
	   GlobalFocusBox(&TreeList[1],FALSE);
	   GlobalFocusBox(&FileList[1],FALSE);
	}
#ifndef NOGROUPINFILE
	if (glob.TreeMode == TR_SHARE)
	   GlobalFocusBox(&ProgramList,FALSE);
#endif
#ifndef NOSWITCHER
	if(gTaskListEnabled)
	{
		if (glob.TreeMode == TR_SHARE)
		GlobalFocusBox(&TaskList,FALSE);
	}
#endif
	HighlightDrive(0,GetFocusDrive(0),isaDriveicon);

	if((glob.TreeMode == TR_DOUBLE))
	   HighlightDrive(1,GetFocusDrive(1),isaDriveicon);

	switch(glob.FocusId)
    {
	   case MENUFOCUS:
#if 0
		MyAltMenu();
		//OpenMenu(80+7);
#endif
		break;
       case FILE0:
	  GlobalFocusBox(&FileList[0],TRUE);
	  break;
       case FILE1:
	  GlobalFocusBox(&FileList[1],TRUE);
	  break;
       case TREE0:
	  GlobalFocusBox(&TreeList[0],TRUE);
	  break;
       case TREE1:
#ifndef NOSWITCHER
       //case TASKBOX: same as TREE1!
		if(gTaskListEnabled && (glob.TreeMode == TR_SHARE))
		{
		GlobalFocusBox(&TaskList,TRUE);
		}
	  else
#endif

	  GlobalFocusBox(&TreeList[1],TRUE);
	/*  OpenMenu(80+7);*/
	  break;

	  break;
       case DRIVELIST0:
	  HighlightDrive(0,GetFocusDrive(0),isaHilite);
	  break;
       case DRIVELIST1:
#ifndef NOGROUPINFILE
       //case GROUPBOX: same as DRIVELIST!
	  if(glob.TreeMode == TR_SHARE)
	  {
	     GlobalFocusBox(&ProgramList,TRUE);
	  }
	  else
#endif
	  HighlightDrive(1,GetFocusDrive(1),isaHilite);
	  break;
    };
#ifndef NOGROUPINFILE
	  if (glob.TreeMode == TR_SHARE)
	  {
#ifndef NOSWITCHER
	       if((glob.FocusId == GROUPBOX)||(gTaskListEnabled && (glob.FocusId == TASKBOX)))
#else
	       if(glob.FocusId == GROUPBOX)
#endif
	       {
				setmenubar(&MainMenuBar,&MainWind);
				AddProgManAccelerators();
	       }
	       else
	       {
				setmenubar(&FileMgrMenuBar,&MainWind);
				AddFileManAccelerators();
	       }
	  }
#endif
			MessageBar(gpszNonSwap, isaMenu,TRUE);
}
VOID CalcNextFocus(int numfocusitems)
{
	     glob.FocusId = (glob.FocusId+1)%numfocusitems;
	     /* WARNING!!! assuming MENUFOCUS has #define of 0 */
	     if (glob.FocusId == MENUFOCUS)
		    glob.FocusId++ ;
}

VOID NextGlobalFocus()
{
      WORD numfocusitems;

      if(glob.InFileMgr) {
      switch(glob.TreeMode)
      {
		case TR_SYSTEM:
			switch (WhoHasGlobalFocus())
			{
				case DRIVELIST0:
					if (GetNumItems(&FileList[0])!=0)
					   InitGlobalFocus(FILE0);
					else
					   Shell_Beep();
					break;
				case FILE0:
				default:
					InitGlobalFocus(DRIVELIST0);
					break;
#if 0
				case MENUFOCUS:
					InitGlobalFocus(DRIVELIST0);
					break;
#endif
			}
		break;
		case TR_SEARCH:
			Shell_Beep() ;
			break ;
	    case TR_DOUBLE:
	    case TR_SINGLE:
	    default:
#ifndef NOGROUPINFILE
	    if(glob.TreeMode == TR_SHARE)
		 {
#ifndef NOSWITCHER
			if(gTaskListEnabled)
				numfocusitems = 6;
			else
#endif
				numfocusitems = 5;
		 }
	    else
#endif
		 numfocusitems = 2*(glob.MaxTree+1)+glob.MaxTree+2;

nextfocus:  /* WARNING! potential infinite loop! */
	    CalcNextFocus(numfocusitems);
	    switch(glob.FocusId) /* the new focus */
	    {
		case FILE0:
		{
// Beep() ; Beep() ; Beep() ;
		    if(GetNumItems(&FileList[0])==0)
			 goto nextfocus;
		}
		break;
		case FILE1:
		{
		    if(GetNumItems(&FileList[1])==0)
			 goto nextfocus;
		}
		break;
		case TREE0:
		{
		    if(GetNumItems(&TreeList[0])==0)
			 goto nextfocus;
		}
		break;
		case TREE1:
		{
// Beep() ; Beep() ;

#ifndef NOSWITCHER
		   if(gTaskListEnabled && (glob.TreeMode == TR_SHARE))
		   {
		    if(GetNumItems(&TaskList)==0)
		       goto nextfocus;
		   }
		   else
#endif
		    if(GetNumItems(&TreeList[1])==0)
			 goto nextfocus;
		}
		break;

	    }
	    InitGlobalFocus(glob.FocusId);
		 break;
	}
    } else {
	if(TaskList.hasglobalfocus ||
		(gTaskListEnabled && GetNumItems(&TaskList))) {
	    GlobalFocusBox(&TaskList,!TaskList.hasglobalfocus);
	    GlobalFocusBox(&ProgramList,!ProgramList.hasglobalfocus);
	}
    }
}

VOID CalcPrevFocus(int numfocusitems)
{
	     glob.FocusId = (glob.FocusId+numfocusitems-1)%numfocusitems;
	     if (glob.FocusId == MENUFOCUS)
	     {
#ifndef NOGROUPINFILE
		    if(glob.TreeMode == TR_SHARE)
		    {
#ifndef NOSWITCHER
				 if(gTaskListEnabled)
			       glob.FocusId = TASKBOX;
			    else
#endif  
		       glob.FocusId = GROUPBOX;
		    }
		    else
#endif
		    glob.FocusId = (glob.TreeMode == TR_DOUBLE) ? FILE1 : FILE0;
	     }
}

VOID PrevGlobalFocus()
{
      WORD numfocusitems;

      if(glob.InFileMgr) {
      switch(glob.TreeMode)
      {
		case TR_SYSTEM:
			switch (WhoHasGlobalFocus())
			{
				case DRIVELIST0:
					if (GetNumItems(&FileList[0])!=0)
					    InitGlobalFocus(FILE0);
					else
					    Shell_Beep();
					break;
				case FILE0:
				default:
					InitGlobalFocus(DRIVELIST0);
					break;
#if 0
				case MENUFOCUS:
					InitGlobalFocus(FILE0);
					break;
#endif
			}

		break;

		case TR_SEARCH:
			Shell_Beep() ;
			break ;

	    case TR_DOUBLE:
	    case TR_SINGLE:
		default:
#ifndef NOGROUPINFILE
	    if(glob.TreeMode == TR_SHARE)
#ifndef NOSWITCHER
		 if(gTaskListEnabled)
			 numfocusitems = 6;
		 else
#endif
		 numfocusitems = 5;
#endif
	    else
		 numfocusitems = 2*(glob.MaxTree+1)+glob.MaxTree+2;

prevfocus:
		 CalcPrevFocus(numfocusitems);
		 switch(glob.FocusId) /* the new focus */
		 {
			case FILE0:
			{
			    if(GetNumItems(&FileList[0])==0)
				 goto prevfocus;
			}
			break;
			case FILE1:
			{
			    if(GetNumItems(&FileList[1])==0)
				 goto prevfocus;
			}
			break;
			case TREE0:
			{
			    if(GetNumItems(&TreeList[0])==0)
				 goto prevfocus;
			}
			break;
			case TREE1:
			{
#ifndef NOSWITCHER
		   if(gTaskListEnabled && (glob.TreeMode == TR_SHARE))
		   {
		    if(GetNumItems(&TaskList)==0)
		       goto prevfocus;
		   }
		   else
#endif
			    if(GetNumItems(&TreeList[1])==0)
				 goto prevfocus;
			}
			break;
		 }
		 /* If we are moving focus away from the drive lists re-draw that list! */

		 InitGlobalFocus(glob.FocusId);
		 break;
      }
    } else
	NextGlobalFocus(); /* There are only two items, so next is last */
}

/*
 * handle keys in the filemanager
 * key is the key hit; state is the shift state
 */
VOID GroupManagerKey(WORD wParam,DWORD LParam);
VOID FileManagerKey(WORD key,WORD state)
{

	BYTE uppercasekey, driveind     ;
	PTREE seltree ;

	/* Control+letter selects appropriate drive */
	/* WARNING! The ctrl+letter check needs to be done before the VK_TAB
	 * stuff because "ctrl+I" is the TAB character and so we don't want the
	 * focus to tab around!
	 */
	if ( (state & KK_CONTROL) &&
		 ((uppercasekey = (BYTE) (key + 'A' - 1)) >= 'A' ) &&
		 (uppercasekey <= 'Z') )
	{
		seltree = FindTree(&uppercasekey, &driveind) ;
		if (seltree)
		{
			SelectDrive(glob.FocusBox, driveind) ;

		/* Move the focus to the drive icons! */
		InitGlobalFocus((glob.FocusBox == 0) ? DRIVELIST0 : DRIVELIST1) ;
		}
	}
	else
	if ((key == VK_TAB) || (key == '\t'))
	{
		/* when we are in the TR_SHARE mode aned the focus is in the prog
		 * man part of the screen, the menu is the program manager menu
		 * and TAB, Shift+TAB is not a keyboard accelerator there. These
		 * keys will come thru to this fn -- We handle them here.
		 */
		if (state & KK_SHIFT)
			PrevGlobalFocus();
		else
			NextGlobalFocus();
    }
	else
	{

       switch (WhoHasGlobalFocus())
       {
	    case TREE0:
		  {      
		    glob.FocusBox = 0;
		    ListKey(&TreeList[glob.FocusBox],key,state);
		  }
		    break;
	    case TREE1:

#ifndef NOGROUPINFILE
	    //case GROUPBOX:
		   if(glob.TreeMode == TR_SHARE)
		      GroupManagerKey(key,(DWORD)state);
		   else
#endif

		    {
		       glob.FocusBox = 1;
		       ListKey(&TreeList[glob.FocusBox],key,state);
		    }
		    break;
	    case FILE0:
		    glob.FocusBox = 0;
		    ListKey(&FileList[glob.FocusBox],key,state);
		    break;
	    case FILE1:
		    glob.FocusBox = 1;
		    ListKey(&FileList[glob.FocusBox],key,state);
		    break;
	    case DRIVELIST0:
			DriveListKey(0, key, state);
		    break;
	    case DRIVELIST1:
#ifndef NOGROUPINFILE
	    //case GROUPBOX:
		   if(glob.TreeMode == TR_SHARE)
		      GroupManagerKey(key,(DWORD)state);
		   else
#endif
			DriveListKey(1, key, state);
	  break;

       }
    }

}
#if 0
VOID MouseIdle(VOID)
{
	if (gMouseDown)
	{
		/* this should be a timer call!! */
		if (gAutoMouse == 0)
		{
			WindProc(&MainWind, WM_MOUSEIDLE, 1,((DWORD) gMouseY << 24) | ((DWORD) gMouseX << 16));
		}
	}
}
#endif
/*
 * UPDATE
 *      The latest CW build has pfnFilter be a far procedure, so this
 *  is no longer necessary. . .
 * WARNING WARNING!!!
 * THIS IS AN EVIL DEATH NIGHTMARE HACK FROM HELL!
 *
 * The following code exists soley to draw black borders around menus
 * when in graphics mode.
 *
 * The problem is that we need to draw the border after CW draws the
 * menu so it won't get erased (or saved by savegraphic arc)
 *
 * This is not so easy, as CW doesn't want to give us control at this
 * time if the mouse button is down (ie we don't get any messages)
 *
 * So here is what we do, in our infinite slimyness:
 *      When we get the WM_INITMENUPOPUP message indicating that a menu
 * is about to pop up, we save off info about which menu it is, and
 * save and modify one of CW's internal variables. This variable is
 * a function pointer to a menu-filter routine; what we want to do
 * is "hook" it so that we get called whenever it gets called so we
 * can get control to draw the menu border. Two things should be noted
 * here: one is that at the time we get the WM_INITMENUPOPUP message
 * we are actually inside the routine we want to hook; and the other is
 * that the routine is a near function inside the CW_USER segment.
 * It's ok that we are inside the routine since we are just changing
 * a function pointer which will be called again. To put our hook routine
 * inside the CW_USER segment (so it can be near), we use the alloc_text
 * pragma.
 * All out hook routine (MyMenuFilterProc) does is perhaps outline the
 * menu
*/

#define INVALIDPTR ((VOID *) -1)

WORD gwParam ;
PWND gMwnd = INVALIDPTR ;

//extern BOOL (NEAR *pfnFilter)(PMSG pmsg);
extern BOOL (FAR *pfnFilter)(PMSG pmsg);

//BOOL (NEAR *RealFilterProc)(PMSG pmsg) = NULL;
BOOL (FAR *RealFilterProc)(PMSG pmsg) = NULL;
//BOOL NEAR MyMenuFilterProc(PMSG pmsg);
BOOL FAR MyMenuFilterProc(PMSG pmsg);

extern BOOL IsFMMenuPutUp(void) ;

#pragma alloc_text(CW_USER,MyMenuFilterProc)
//BOOL NEAR MyMenuFilterProc(PMSG pmsg)
BOOL FAR MyMenuFilterProc(PMSG pmsg)
{
   if(gisgraph)
    {
	   if(gMwnd != INVALIDPTR)
       {
		  gMwnd = INVALIDPTR;
	      FrameMenu(gMwnd,gwParam);
       }
    }

    return(RealFilterProc(pmsg));
}

/* returns true if a menu is popped down; false otherwise
 * WARNING this is may only be valid for mainwind
 */
BOOL IsMenuDown(void)
{
	return(((pfnFilter == MyMenuFilterProc)) | (gCnx == cnxMenu));
}

extern VOID MonitorManipulation(BYTE,BYTE);
VOID GroupManagerMouse(WORD mx,WORD my,WORD message,BYTE state);
/*
 *      Handle messages sent to main window
 */
long far pascal WindProc(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
	BYTE mx;
	BYTE my;
	BOOL ret;
	static isacceleration=FALSE;

	switch(message) {
		case WM_PAINT:
			break;
		case WM_MENUINACTIVE:
			/* Had the user hit F1 when the menu was active? If so, the menu
			 * has been closed now. Display help to the user at this time.
			 */
			if (gfDisplayDelayedHelp)
			{
				HelpBox() ;
				ClearHelpContext() ;
				gfDisplayDelayedHelp = FALSE ;
			}
			// Fall through!

		case WM_DIALOGINACTIVE:
			if(LParam) // if CWs redraw failed
			{
				gfRepaintonidle = TRUE;
			}
		break;
		case WM_CHAR:
			if (glob.InFileMgr)
			{
				if ( (glob.TreeMode == TR_SEARCH) && 
						( (wParam == ESCAPE) || (wParam == VK_ESCAPE) ) )
				{
					HandleSearchQuit() ;

					/* The following will make sure that the file with
					 * focus in each listbox gets selected by default.
					 */
					listinfo[0].UpdateFiles = TRUE ;

					if ( (glob.TreeMode == TR_DOUBLE) &&
										(listinfo[0].tree == listinfo[1].tree) )
						listinfo[1].UpdateFiles = TRUE ;

					DoFileMgr() ;
				}
				else
				FileManagerKey(wParam,HIWORD(LParam));
			}
			else
			{
			   GroupManagerKey(wParam,LParam);
			}
			break;

		case WM_LBUTTONDBLCLK:
		case WM_LBUTTONDOWN:
			gMouseDown = TRUE;
			gMouseDownX=LOBYTE(HIWORD((LParam)));
			gMouseDownY=HIBYTE(HIWORD((LParam)));
		case WM_MOUSEMOVE:
			gMouseX=LOBYTE(HIWORD((LParam)));
			gMouseY=HIBYTE(HIWORD((LParam)));
		case WM_MOUSEIDLE:
		case WM_LBUTTONUP:
			if (message == WM_LBUTTONUP)
			     gMouseDown = FALSE;
			/*
			 * do not handle mouse messages in the main window when
			 * a menu is down!
			 */
			if(gCnx == cnxMenu)
				return(TRUE);

			mx=LOBYTE(HIWORD((LParam)));
			my=HIBYTE(HIWORD((LParam)));

/* Following is a HACK to suppress mouse messages when in Reorder
 * or Copy mode.
 */
	    if(gInReorder || gInCopy) {
		if(!ListMouse(&ProgramList, mx, my, message, LOBYTE(wParam))
			&& message==WM_LBUTTONDOWN)
		    ShellMessageBox(szInRCTitle, szInRCText);
		break;
	    }

			/* Following is a HACK to get the mouse messages to the ViewWind.
			 * ViewWind is a child window of MainWind, but we don't set the
			 * Focus to ViewWind as we want to still be able to handle the
			 * boxes around menus, etc and keep it active.
			 */
			if (m_fPerformingViewFile())
				ViewWindProc(pwnd, message, wParam, LParam) ;
			else if (glob.InFileMgr)
			{
#ifndef NORESIZE
				if(gDraggingrect)
				{
				   if (message == WM_LBUTTONUP)
				       DoEndResize(mx,my);
				   else
				   if (message == WM_MOUSEMOVE)
				      DoResize(mx,my);
				   return(TRUE);
				}
				if((mx == 0) && (my == 0))
				{
				    if(message == WM_LBUTTONDOWN)
				       DoBeginResize(mx,my);
				   return(TRUE);
				}
#endif
				if (TreeList[0].scroll.dragging)
				{
					ListMouse(&TreeList[0],mx,my,message,LOBYTE(wParam));
				} else if (TreeList[1].scroll.dragging)
				{
			       ListMouse(&TreeList[1],mx,my,message,LOBYTE(wParam));
				} else
				{
					ret = DriveMouse(mx, my, message);
					if (!ret)
					{
						ret = ListMouse(&FileList[0], mx, my, message,
													LOBYTE(wParam));

						if ((ret) && (WhoHasGlobalFocus() != FILE0))
						{
							if (GetNumItems(&FileList[0]) > 0)
								InitGlobalFocus(FILE0);
						}
					}
					if (!ret)
					{
						ret = ListMouse(&TreeList[0],mx,my,message,
													LOBYTE(wParam));
						if ((ret) && (WhoHasGlobalFocus() != TREE0))
						{
							if (GetNumItems(&TreeList[0]) > 0)
								InitGlobalFocus(TREE0);
						}
					}
					if (!ret && glob.TreeMode == TR_DOUBLE)
					{
						ret = ListMouse(&FileList[1],mx,my,message,
													LOBYTE(wParam));
						if ((ret) && (WhoHasGlobalFocus() != FILE1))
						{
							if (GetNumItems(&FileList[1]) > 0)
								InitGlobalFocus(FILE1);
						}
					}
					if (!ret && glob.TreeMode == TR_DOUBLE)
					{
						ret = ListMouse(&TreeList[1],mx,my,message,LOBYTE(wParam));
						if ((ret) && (WhoHasGlobalFocus() != TREE1))
						{
							if (GetNumItems(&TreeList[1]) > 0)
								InitGlobalFocus(TREE1);
						}
					}
#ifndef NOGROUPINFILE
					if (!ret && glob.TreeMode == TR_SHARE)
					{
						ret = ListMouse(&ProgramList,mx,my,message,LOBYTE(wParam));
						if ((ret) && (WhoHasGlobalFocus() != GROUPBOX))
							InitGlobalFocus(GROUPBOX);
					}

#endif
#ifndef NOSWITCHER
					if (!ret && gTaskListEnabled && (glob.TreeMode == TR_SHARE))
					{
						ret = ListMouse(&TaskList,mx,my,message,LOBYTE(wParam));
						if ((ret) && (WhoHasGlobalFocus() != TASKBOX))
						{
							if (GetNumItems(&TaskList) > 0)
								InitGlobalFocus(TASKBOX);
						}
					}
#endif
				}

#ifndef NODIRECT
			if(message == WM_MOUSEMOVE)
			   MonitorManipulation(gMouseX,gMouseY);
#endif

			}  /* glob.InFileMgr */
			else
			{
				 GroupManagerMouse(mx,my,message,LOBYTE(wParam));
			}
			break;

		case WM_COMMAND:
		/* WARNING!!! We always reset the tree manipulation
		   optimization before any menu command. Any things
		   which manipulate the tree not under a menu item
		   must be sure the tree has not changed before
		   using these manipulations. Also, any manipulation
		   within one of these commands could modifiy the
		   tree so it too must call ResetTreeOptimizations
		*/
			ResetTreeOptimizations();
			/* make the separator character blank again */
			inch._chTopSide1 = ' ';

			MenuCommand(pwnd, wParam|(isacceleration?0xF000:0));
			break;

		case WM_INITMENUPOPUP:
		    /* Enable/Disable the file manager menu items. */
		    isacceleration = (HIWORD(LParam)==mmdAccel);
		    if (glob.InFileMgr)
		    {
					/* If focus is on the file manager protion of the screen and
					 * user is in SHARED mode, enable/disable FileManager menu
					 * items, else handle the ProgramManager items.
					 */
					if (IsFMMenuPutUp())
					{
						if (IsTreeBeingBuilt())
						CrippleFileMenuBar() ;
						else
						EnableDisableFileOps() ;
					}
					else
					{
						EnableDisablePMMenu() ;
					}
			}
			else
			{
				if (m_fPerformingViewFile())
				{
					EnableDisableViewMenu() ;
				}
				else
				{
					EnableDisablePMMenu() ;
				}
			}

		    if(!isacceleration)
		    {
				/* menu is just about to pop up */
				/* we need to restore the separator character
				 * which is used both for separator and that
				 * annoying bottom line in dialogs
				 */
				if(!gisgraph)
					inch._chTopSide1 = cinch._chTopSide1;
				gwParam = wParam;
				gMwnd = pwnd;
				if(gisgraph)
				{
					RealFilterProc = pfnFilter;
					pfnFilter = MyMenuFilterProc;
				}
		    }
			break;

		case WM_MENUSELECT:
		{
		   if ((LParam == 0) && (wParam == 0))
		   {
			if(gisgraph)
			{
			   if(RealFilterProc)
			      pfnFilter = RealFilterProc;
			}
			/* menu is coming down */
			/* make the separator character blank again */
			inch._chTopSide1 = ' ';
		   }
		}
		break;

		default:
			break;
		}
#if 0
	 if (gDoAltMenu)
	 {
		gDoAltMenu = FALSE;
		/* WARNING VK_MENU (VK_ALT) will not work here --don't know why! */
		PostMessage(NULL, WM_CHAR, VK_F10, 0L);
	 }
#endif

	  return(TRUE);
	}

/* We need to know when we are inside a dialog, so we won't do hotkeys
 * etc. gCnx is not enough, as it is only "active" when the dialog is
 * idle, which is exactly not when a key is pressed.
 */
BOOL gInDialog = FALSE;
BOOL gfMouseReEnableHack;

TMC     MyTmcDoDlg(VOID *pdlg, HCAB hcab)
{
	TMC tmc ;

	/* We want to disable mouse before a dialog pops up.
	 * we enable the mouse back again later after dialog has popped up!
	 */
	FEnableMouseNest(FALSE) ;
	++gInDialog;

	gfMouseReEnableHack = TRUE ;

	tmc = TmcDoDlgAxAy(pdlg, hcab,(AX) ((PDLG)pdlg)->crcDlg.x,
											 (AY) ((PDLG)pdlg)->crcDlg.y) ;
	--gInDialog;
	FEnableMouseNest(TRUE) ;

	/* Dialog has been dismissed, so mark the mouse are not being down!
	 * This variable gets set when we get a single/double click. Happens
	 * say when we try to launch a program in the PM which needs arguments.
	 */
	gMouseDown = FALSE ; 

	return tmc ;
} /* MyTmcDoDlg */


extern VOID Arrow(void);
void CalcDriveIconPositions(void) ;
WORD MouseVersion(void);

extern BOOL SetFAltGr(BOOL fTreatAltsDifferently) ;
extern int FTreatAltsDifferently(void) ;

 /*
 *      Draw main window, and menus
 */
/* fFirtTime specifies whether this routine is being called for the first time
 * or whether it is being called on a screen mode change.
 */
VOID FAR InitWindows(BOOL fFirstTime)
{
	/*
	 * suggested by kirkg to enable TSR pasteing.
	 * Here, we pretend we are WORKS
	 */
	SetTsrProtocol(0x55FE,5,0);
	EnableKeyboard(TRUE);

	SetFAltGr(FTreatAltsDifferently()) ;

	/* NOTE! setwindowsize must go before move window! */
	SetWindowSize(&MainWind, axMac, ayMac);
	MoveWindow(&MainWind, 0, 0);
	EnableWindow(&MainWind, TRUE);
	SetFocus(&MainWind);
	EnableCursor(&MainWind, FALSE);
	DrawWindow(&MainWind);

	Arrow(); /* set up the default cursor */

	/* If this is not the first time, this is happening. The screen mode has
	 * changed, so the rest of the world which initialized stuff based on
	 * axMac, ayMac might have to recalculate the values again.
	 */
	if (!fFirstTime)
	{
		/* The file manager needs to re-calculate where the drive icons are
		 * to be drawn if in double tree mode.
		 */
		 CalcDriveIconPositions() ;

		 /* We don't initialize the mouse first time so it won't get in the
		  * way during start-up. Thus, init does it.
		  */
		 FInitMouseNest();
	}
}



VOID DialogIsAlert(BOOL set)
{
    static WORD lastfore,lastback;
    WORD alertfore,alertback;

    if(set)
    {
       GetIsaColor(isaDialogBox, &lastfore, &lastback);
       GetIsaColor(isaAlert, &alertfore, &alertback);
       SetIsaColor(isaDialogBox,alertfore,alertback);
    }else
       SetIsaColor(isaDialogBox,lastfore,lastback);
}

VOID  MySetColor(WORD color) ;


void DrawDialogBorderAndTitle(PWND pwnd)
{
	RRC dr;
	RX x;
	int len;
	BOX  tboxinfo;
	WORD fore, back ;

	FEnableMouseNest(FALSE) ;
	if(!gisgraph)
	{
	GetClientRrc(pwnd,&dr);

		tboxinfo.chTopLeftCorner          = cinch._chTopLeftCorner1;
		tboxinfo.chTopRightCorner         = cinch._chTopRightCorner1;
		tboxinfo.chBottomLeftCorner   = cinch._chBottomLeftCorner1;
		tboxinfo.chBottomRightCorner  = cinch._chBottomRightCorner1;
		tboxinfo.chTopSide                = cinch._chTopSide1;
		tboxinfo.chBottomSide     = cinch._chBottomSide1;
	tboxinfo.chLeftSide       = cinch._chLeftSide1;
	tboxinfo.chRightSide      = cinch._chRightSide1;
	DrawBox(pwnd, &dr, &tboxinfo, isaDialogBox);                                            
	}
	else
	{
		FrameDialog(pwnd->arcWindow.ayTop,pwnd->arcWindow.axLeft,
						pwnd->arcWindow.ayBottom,pwnd->arcWindow.axRight);

		GetIsaColor(isaBorders,&fore,&back);
		MySetColor(fore);

		Move(pwnd->arcWindow.axLeft*CWIDTH+2,(pwnd->arcWindow.ayTop+1)*CHEIGHT);
		Draw(pwnd->arcWindow.axRight*CWIDTH-3,(pwnd->arcWindow.ayTop+1)*CHEIGHT);
	}

	len = strlen(szCurDialogCaption);
	x = (pwnd->arcWindow.axRight - pwnd->arcWindow.axLeft+1)/2;
	x = x-len/2;
	CharOut(pwnd,x-1,0,' ',isaWhiteOnBlack);
	TextOut(pwnd,x,0,szCurDialogCaption,-1,isaWhiteOnBlack);
	CharOut(pwnd,x+len,0,' ',isaWhiteOnBlack);

	FEnableMouseNest(TRUE) ;
} /* DrawDialogBorderAndTitle */

DWORD (FAR *Pfndialog_chain)(PWND,WORD,WORD,DWORD);
BOOL gDialogIgnoreNextPaint = FALSE;

DWORD FAR PASCAL Pfndialog(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
	if ((gDialogIgnoreNextPaint) && (message == WM_PAINT))
	{
		gDialogIgnoreNextPaint = FALSE;
		return(TRUE);
	}
	Pfndialog_chain(pwnd,message,wParam,LParam);

	if (message == WM_PAINT)
	{
		DrawDialogBorderAndTitle(pwnd) ;
	} /* paint message */
	else
	{
		/* The mouse was disabled in fn MyTmcDoDlg(). It remains disabled
		 * until this moment. At this time, the dialog has been completely
		 * drawn (static text, edit boxes, etc). I can avoid this hack
		 * variable if CW always sent in a WM_SETFOCUS message after finishing
		 * all the draw operations. Unfortunately, they send this message
		 * only for certain dialogs (like Fdlgmessage).
		 */
		if ( (gfMouseReEnableHack) && (message == WM_DIALOGIDLE) )
		{
			gfMouseReEnableHack = FALSE ;
			FInitMouseNest() ;
		}
	}

	return(TRUE) ;
}
DWORD (FAR *Pfnnothing_chain)(PWND,WORD,WORD,DWORD);

DWORD FAR PASCAL Pfnnothing(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
    switch(message)
    {
	case WM_SETFOCUS:
	case WM_KILLFOCUS:
	case WM_PAINT:
	  {
	     return(TRUE);
	  }
    }
	return(Pfnnothing_chain(pwnd,message,wParam,LParam)) ;
}

DWORD (FAR *Pfnpanel_chain)(PWND,WORD,WORD,DWORD);

DWORD FAR PASCAL Pfnpanel(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
    DWORD retval;

    retval = Pfnpanel_chain(pwnd,message,wParam,LParam);

    if (message == WM_PAINT)
    {
	 FEnableMouseNest(FALSE);

	 FrameCharRect(pwnd->arcWindow.ayTop,pwnd->arcWindow.axLeft,
			 pwnd->arcWindow.ayBottom,pwnd->arcWindow.axRight, 1,isaDialogBox);
	 FEnableMouseNest(TRUE);
    }
    return(retval);
}


DWORD (FAR *Pfnbutton_chain)(PWND,WORD,WORD,DWORD);

DWORD FAR PASCAL Pfnbutton(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
    char buttontext[20];
    DWORD retval;
    RX x;
	int len;

    switch(message)
    {
	case WM_SETFOCUS:
	    gCurrentTMC = pwnd->id;
	case WM_KILLFOCUS:
	case WM_PAINT:
	  {
	     if(fDrawItem && !fRedrawItem) /* not just initializing */
	     {
		 FEnableMouseNest(FALSE);
		 if(gisgraph)
		 {
			FrameButton(pwnd->arcWindow.ayTop,pwnd->arcWindow.axLeft,
						pwnd->arcWindow.ayBottom,pwnd->arcWindow.axRight);
		 }
		 GetTmcText(pwnd->id, buttontext, 20);
		 len = strlen(buttontext);
		 x = (pwnd->arcWindow.axRight - pwnd->arcWindow.axLeft)/2;
		 x = x-len/2;
		 if(!gisgraph)
		    CharOut(pwnd,(RX)x-1,(RY)0,' ',isaPushButton);
		 TextOut(pwnd,x,0,buttontext,-1,isaPushButton);
		 if(!gisgraph)
		    CharOut(pwnd,(RX)x+len,(RY)0,' ',isaPushButton);
		 MoveCursor(pwnd,x,0);
		 FEnableMouseNest(TRUE);
	     }
	     return(TRUE);
	  }
	case WM_LBUTTONDOWN:
	case WM_LBUTTONDBLCLK:
	  retval = Pfnbutton_chain(pwnd,message,wParam,LParam);
	  retval = Pfnbutton_chain(pwnd,WM_LBUTTONUP,wParam,LParam);
	  gMouseDown=FALSE;
	  return(retval);
    }
    retval = Pfnbutton_chain(pwnd,message,wParam,LParam);
    return(retval);
}

/* ZZZ Without this fn, we get BLACK BLOBS for check boxes sometimes -- 
 * A CW bug??
 */
/* Check Box Left X-Coordinate relative to window. */
#define CB_X (0)
DWORD (FAR *Pfncheckbox_chain)(PWND,WORD,WORD,DWORD);
DWORD FAR PASCAL Pfncheckbox(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
	switch (message)
	{
	char tstr[95]; // Check Box string storage -- won't exceed screen width

		case WM_PAINT:
			if(fDrawItem) /* not just initializing */
			{
				/* ZZZZ For Graphics mode, we can do special drawing of
				 * the check boxes if we want.
				 */
				FEnableMouseNest(FALSE);
				CharOut(pwnd,CB_X,0, CHECKBOX_LEFTCHAR,isaDialogBox);
				CharOut(pwnd,CB_X+1,0,
					GetTmcVal(pwnd->id&0xFF) ?      CHECKBOX_CHECKCHAR : ' ',
					isaDialogBox) ;
				CharOut(pwnd,CB_X+2,0, CHECKBOX_RIGHTCHAR,isaDialogBox);

			GetTmcText(pwnd->id&0xFF, tstr, 
							pwnd->arcWindow.axRight - pwnd->arcWindow.axLeft + 1);
				TextOut(pwnd,CB_X+4,0,tstr,-1,isaDialogBox);
				FEnableMouseNest(TRUE);
			}
			return(TRUE);

		case WM_SETFOCUS:
			/* Move the cursor to the center of the CheckBox. By default CW
			 * sends it to the left ('[' char).
			 */
			MoveCursor(pwnd, CB_X+1, 0) ;
			break ;
	}
   return Pfncheckbox_chain(pwnd,message,wParam,LParam);
} /* Pfncheckbox */


DWORD (FAR *Pfneditbox_chain)(PWND,WORD,WORD,DWORD);
DWORD FAR PASCAL Pfneditbox(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
   DWORD retval;
   char tstr[256];
	TMC tmc ;
	int edit_width ;
	int edit_scroll ;

   switch(message)
   {
		case WM_PAINT:
		{
			if(gisgraph)
			{
				if(fDrawItem) /* not just initializing */
				{
					
					FEnableMouseNest(FALSE);

					/* BUG BUG see below in setupeditbox */
					GetTmcText((tmc = pwnd->id&0xFF), tstr, sizeof(tstr)) ;

					edit_width = pwnd->arcWindow.axRight -
															pwnd->arcWindow.axLeft + 1 ;

					edit_scroll = strlen(tstr) - edit_width + 2 ;
					if (edit_scroll < 0) 
						edit_scroll = 0 ;

					/* If 'tmc' is the item that has initial focus -- saved in
					 * SetUpEditBox(), we display text in hilite mode.
					 */
					/* ZZZZ There should a better way to tell this using CW!!! */

					//if (tmc == gInitialHiliteTMC)
					if(GetFocus() ==pwnd)
					{
#if 0 
	not needed
						/* Set it back to the invalid TMC default value */
						gInitialHiliteTMC = INVALID_TMC ;
#endif
						TextOut(pwnd, 0, 0, tstr + edit_scroll, -1, isaHilite) ;
					}
					else
					{
						TextOut(pwnd, 0, 0, tstr + edit_scroll, -1, isaDialogBox) ;
					}

					FrameCharRectInset(pwnd->arcWindow.ayTop,pwnd->arcWindow.axLeft,
							pwnd->arcWindow.ayBottom,pwnd->arcWindow.axRight,1,1,isaDialogBox);
					FEnableMouseNest(TRUE);
				}
				return(TRUE);
		   }
		}
		break;
		case WM_CHAR:
		{
			GetTmcText(pwnd->id, tstr, 256);
			if(strlen(tstr)+1 > (pwnd->rgwExtra[0])) /* max len stored here */
			{
				switch(wParam)
				{
					case VK_DELETE:
					case 127: //also delete
					case VK_INSERT:
					case VK_HOME :
					case VK_END :
					case VK_LEFT:
					case VK_RIGHT:
					case VK_UP:
					case VK_DOWN:
					case VK_TAB:
					case KEY_TAB:
					case VK_ESCAPE:
					case ESCAPE:
					case '\b':
					case '\r':
						break; // fall through to normal handling
				   default:
						Shell_Beep() ;
						return TRUE     ;
				}       
			}
		}
		break;
	}
   retval = Pfneditbox_chain(pwnd,message,wParam,LParam);
   return(retval);
}

extern char currentPassword[MAXUSERPASSWORD];

DWORD FAR PASCAL Pfnpasswordbox(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
    DWORD retVal;
    int i;
    char tstr[MAXUSERPASSWORD];

    switch(message) {
    case WM_SETFOCUS:
	retVal = Pfneditbox(pwnd, message, wParam, LParam);
	GetTmcText(pwnd->id, tstr, MAXUSERPASSWORD);
	i = strlen(tstr);
	SetTmcSel(pwnd->id, i, i);
	return(retVal);

    case WM_LBUTTONDOWN:
	SetFocusTmc(pwnd->id);
    case WM_LBUTTONDBLCLK:
    case WM_MOUSEMOVE:
    case WM_LBUTTONUP:
	return(TRUE);

    case WM_CHAR:
	if(wParam == VK_DOWN)
	    break;
	if(wParam == 127 || wParam >= 0x100) {
	    Shell_Beep() ;
	    return(TRUE);
	}
	if(wParam >= ' ') {
	    GetTmcText(pwnd->id, tstr, MAXUSERPASSWORD);
	    i = strlen(tstr);
	    currentPassword[i] = LOBYTE(wParam);
	    wParam = '*';
	}
	break;
    }

    return(Pfneditbox(pwnd, message, wParam, LParam));
}

DWORD FAR PASCAL Pfnfakeeditbox(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
	/* prevent the user from editing the items of this fake edit-box. */
	switch(message)
	{
		case WM_CHAR:
		{
			switch(wParam)
			{

				case VK_HOME :
				case VK_END :
				case VK_LEFT:
				case VK_RIGHT:
				case VK_UP:
				case VK_DOWN:
				case VK_TAB:
				case KEY_TAB:
				case VK_ESCAPE:
				case ESCAPE:
					break ;  /* pass these on to the Pfn function */
				default:
					Shell_Beep() ;
					return TRUE     ;
			}

		}
	   break;
	}
	return Pfneditbox(pwnd, message, wParam, LParam) ;
} /* Pfnfakeeditbox */


DWORD (FAR *Pfnradiobutton_chain)(PWND,WORD,WORD,DWORD);
DWORD (FAR *Pfnradiogroup_chain)(PWND,WORD,WORD,DWORD);
#define MAXRADIOS 6
PWND gRadioWind[MAXRADIOS];
BYTE numradios;
BYTE gNeedsUpdate;

VOID PASCAL DrawRadios(WORD item, BOOL bEraseBitmap)
{
    char tstr[20];

    if(item >= numradios)
	return;

    FEnableMouseNest(FALSE);

    GetTmcText((gCurRadioGroup&(~ftmcGrouped))+item, tstr, 20);
    TextOut(gRadioWind[item], 4, 0, tstr, -1, isaDialogBox);
    if(bEraseBitmap)
	TextOut(gRadioWind[item], 0, 0, "    ", -1, isaDialogBox);
    PlotBmp(GetTmcVal(gCurRadioGroup)==item ? RadioButtonSel : RadioButtonUnSel,
	    (gRadioWind[item]->arcWindow.axLeft)*CWIDTH+4,
	    gRadioWind[item]->arcWindow.ayTop*CHEIGHT, isaDialogBox);

    FEnableMouseNest(TRUE);
}

void PASCAL MySetTmcVal(TMC theTMC, WORD theVal)
{
    WORD oldVal;

    oldVal = GetTmcVal(theTMC);
    gCurrentTMC = (theTMC&(~ftmcGrouped)) + theVal;
    SetTmcVal(theTMC, theVal);

    DrawRadios(oldVal, FALSE);
    DrawRadios(theVal, FALSE);
}

DWORD FAR PASCAL Pfnradiobutton(PWND pwnd, WORD message, WORD wParam,
													DWORD LParam)
{
	UnReferenced(LParam) ;
    switch(message)
    {
		case WM_CHAR:
		{
			 switch(wParam)
			 {
				case '\t':
					if(HIWORD(LParam) & KK_SHIFT)
					   SetFocusTmc(gPrevTmc);
					else
					   SetFocusTmc(gNextTmc);
					return(TRUE); /* tab out */
				case VK_ESCAPE:
				case ESCAPE:
				case VK_RETURN:
				case '\r':
					return(FALSE); /* tab out */
				case VK_UP:
				case VK_PRIOR:
					MySetTmcVal(gCurRadioGroup,
					(GetTmcVal(gCurRadioGroup)+numradios-1)%numradios);
				break;
				case VK_NEXT:
				case VK_DOWN:
					MySetTmcVal(gCurRadioGroup,
						(GetTmcVal(gCurRadioGroup)+1)%numradios);
			 }

		}
	break;
	case WM_LBUTTONDOWN:
	case WM_LBUTTONDBLCLK:
		 MySetTmcVal(gCurRadioGroup,pwnd->id);
		 break;

    case WM_SETFOCUS:
	gCurrentTMC = (gCurRadioGroup&(~ftmcGrouped))+GetTmcVal(gCurRadioGroup);
    case WM_KILLFOCUS:
	EnableCursor(pwnd,FALSE);
    case WM_WANTFOCUS:
	break;

    case WM_REPAINT:
    case WM_PAINT:
	DrawRadios(pwnd->id, TRUE);
	break;
    }
	return(TRUE);
}

/*
 * Just like SetTmcText, except we ensure that '~''s don't
 * get translated funny by SDM.
 */
VOID FARPUBLIC Shell_SetTmcText(TMC tmc, char *text)
{
	SetWindowStyle(PwndOfListbox(tmc),WS_STATIC_NOACCEL);
	SetTmcText(tmc,text);
}

VOID SetUpDialog(TMC tmcchild,char *title)
{
     PWND dwind;

	 szCurDialogCaption=title;

	 dwind = PwndParent(PwndOfListbox(tmcchild));
     SetWindowStyle(dwind,WS_TILED);
     Pfndialog_chain = GetWindowProc(dwind);
     SetWindowProc(dwind,Pfndialog);
}

VOID SetUpButtonForGraphics(TMC tmc)
{
     PWND dwind;

     dwind = PwndOfListbox(tmc);
     Pfnbutton_chain = GetWindowProc(dwind);
     dwind->id = tmc;
     SetWindowProc(dwind,Pfnbutton);
}

/*
 * tmc is the tmc of the edit box
 * fRealOrNot TRUE means the text can be edited, FALSE only cursor keys work
 * maxcount is the maximum allowable length of the text input (<=255!)
 */
VOID SetUpEditBox(TMC tmc, BOOL fRealOrNot, WORD maxcount, BOOL fHasInitialFocus)
{
	char tstr[256];
	PWND dwind;

	/* ZZZZZ Remove this parameter when time permits -- not needed anymore */
	UnReferenced(fHasInitialFocus) ;

	/* be sure we don't clip if we don't need to! */
	if(maxcount > 255)
		maxcount = 255;

#if 0
	not needed
	if (fHasInitialFocus)
		gInitialHiliteTMC = tmc ;
#endif
   dwind = PwndOfListbox(tmc);
   Pfneditbox_chain = GetWindowProc(dwind);
	/*BUG BUG BUG BUG can we do this? are tmc < 256 always?*/
	dwind->id = tmc; 
	dwind->rgwExtra[0] = (BYTE) maxcount;
	GetTmcText(tmc, tstr, 256);
	if(strlen(tstr)+1 > maxcount) {
		tstr[maxcount] = '\0';
		Shell_SetTmcText(tmc, tstr);
	}

	if (fRealOrNot)
	{
		SetWindowProc(dwind, Pfneditbox);
	}
	else
	{
		SetWindowProc(dwind, Pfnfakeeditbox) ;
	}
} /* SetUpEditBox*/

VOID SetUpCheckBox(TMC tmc)
{
	PWND dwind ;

	dwind = PwndOfListbox(tmc);
	Pfncheckbox_chain = GetWindowProc(dwind) ;
	dwind->id = tmc ;
	SetWindowProc(dwind, Pfncheckbox) ;
} /* SetUpCheckBox */


VOID SetUpRadioGroupForGraphics(TMC group, TMC prevtmc, TMC nexttmc)
{
     gNeedsUpdate = TRUE;
	 gCurRadioGroup=group;
	 gPrevTmc = prevtmc ;
	 gNextTmc = nexttmc ;
	 numradios = 0;
}

VOID SetUpRadiobuttonForGraphics(TMC tmc,int index)
{
     PWND dwind;

	 UnReferenced(index) ;

     dwind = PwndOfListbox(tmc);
     gRadioWind[numradios] = dwind;
     dwind->id = numradios;
     ++numradios;
     Pfnradiobutton_chain = GetWindowProc(dwind);
     SetWindowProc(dwind,Pfnradiobutton);
}

/* This is called from the menu -- Refresh -- to brings us out of the
 * stunned mode we get into if stupid APPS like LANMAN, SIDEKICK screw
 * the screen brightness attributes, etc and not reset it properly.
 */
extern void RefreshViewFileScreen(BOOL fClearAndRedraw) ;
void DoFullRefresh(void)
{
	/* Set screen mode -- hard reset! */
	SetScreenMode(gscreenmode) ;

	if (m_fPerformingViewFile())
	{
		FEnableMouseNest(FALSE) ;
		setmenubar(&ViewMenuBar, &MainWind) ;

		RefreshViewFileScreen(TRUE) ;

		/* We should not do InitWindows() in this case as that would cause
		 * MainWind to get the focus and not ViewWind!
		 */
		Arrow(); /* set up the default cursor */

		/* The help line for view at bottom of screen */
		MessageBar(szViewMessage, isaMenu,TRUE) ;
		FEnableMouseNest(TRUE) ;
		return ;
	}

	InitWindows(FALSE);

	FEnableMouseNest(FALSE) ;

	if (glob.InFileMgr)
	{
		if (glob.TreeMode == TR_SEARCH)
		{
			DoSearchDisplay(FALSE) ;
		}
		else
			DoFileMgr();
	}
	else
		InitializeStartPrograms();

	FEnableMouseNest(TRUE) ;
} /* DoFullRefresh */

#if 0
VOID SetUpPanelForGraphics(TMC tmc)
{
     PWND dwind;
     dwind = PwndParent(PwndOfListbox(tmc));

     Pfnpanel_chain = GetWindowProc(dwind);
     SetWindowStyle(dwind,WS_TILED);
     SetWindowProc(dwind,Pfnpanel);
}
VOID SetUpNothingButton(TMC tmc)
{
     PWND dwind;
     dwind = PwndOfListbox(tmc);
     Pfnnothing_chain = GetWindowProc(dwind);
     SetWindowProc(dwind,Pfnnothing);
}
 
#endif
