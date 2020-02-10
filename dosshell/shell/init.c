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
#include <assert.h>
#include <screen.hs>
#include <screen.sdm>

extern void UnHookISR9(void) ;
extern BOOL gfOurISR9Installed ;
extern BOOL ErrorCrit;
#ifdef KANJI

#include <kkcfltr.h>

extern char *gpszNonSwap ;

STATIC LONG     FAR PASCAL KkcStatusWndProc(PWND, WORD, WORD, DWORD);

/* kk converter status window (at bottom of screen) */
WND wndKkcStatus =
    wndGeneric(1, WS_TILED, TRUE, 0, 24, 80, 1,
	KkcStatusWndProc, NULL, NULL, NULL)
    endWndGeneric;
    
#endif

/* The maximum number of screen modes that we will scan for in any display! */
#define MAX_SCREEN_MODES                16

/* This data structure will be filled in at startup and then will be looked
 * up by the Screen Mode changing functions.
 */
struct S_Mode sScreenMode[MAX_SCREEN_MODES] ;

/* This variable specifies if we have set the screen mode atleast once.
 * used to decide whether to free all the memory allocated by CW (using
 * calls to our fn "LpwAllocDriverMem()") that are not needed when we change
 * screen modes.
 */
BOOL gfScreenModeHasBeenSetOnce = FALSE ;
extern void OurHackFreeDriverMem(void) ;

extern void FInitMouseNest(void) ;

#define LOW_RES_NUMLINES        25
#define MED_RES_NUMLINES        35

#define m_fIsLowResMode(numlines) ((numlines) <= LOW_RES_NUMLINES)

#define m_fIsMedResMode(numlines) (!m_fIsLowResMode(numlines) && (numlines)<=MED_RES_NUMLINES)

/* Max length of the strings: "Medium Resolution Graphics", "Low Resolution
 *      Text", etc! These also have (xx lines) appended to them! 
 */
#define MAXSCREENMODETITLE 55

extern DTS_API_Exit(void);

extern VOID FirstInitTaskMan(void);
extern BOOL Buffered_Write_Ini_File(BOOL bFreeFM) ;
extern TOKEN Get_Identifier_Token(char far *identifier);
extern BOOL gfScreenModeChanged;
extern BOOL gfSwapHandlerInstalled;
extern int gReadUpdateFreq ;

extern BOOL gBeeps_On ;
extern BOOL gfFMVisited ;

extern MENUINFO MainMenuBar;
extern MENUINFO FileMgrMenuBar;
extern void RemoveSwapHandler(void) ;
extern void HookISR10(void);
extern void UnHookISR10(void);
extern int GetUserColorSelection(void);
extern VOID FAR ScreenDialog(void) ;
extern void DrawScreen(void) ;

extern int gNthColorGroup;
extern char gStartInDir[];
extern char gStartUpDir[];
extern int gStartUpDirEnd; /* location where the NULL goes in the above name */

extern int gNumColorSchemes ;
extern char gColorBoxTitle[] ;
extern char *gCurrentColorTitle ;

BOOL gfFileMgrStartUp = FALSE ;
WORD fSwapButton;

#define SCREENCANCEL -3
#define SCREENOK -2
#define SCREENPREVIEW -1
BOOL gContinueScreens ; /* Whether to keep putting up the screen manager boxes */
WORD gscreenmode; /* mode the screen is in now */

int gScreenListInd; /* Index in the listbox (0 based) of Screen Mode that
							* we want to switch to.
							*/

char *gpszScreenModeTitle ;

int gNumScreenModes=0 ; /* Total number of screen modes supported by display */

ListBoxData ScreenModeList ;

/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

char far *gMouseStateSave;
BOOL DriverFound = TRUE;
WORD ReturnScreenMode;
BYTE StartupNumLines; // number of screen lines at startup

#define NUMISASUSED 31
COLORTABLEENTRY MonoColorTable[NUMISASUSED] =
{
  //background,foreground
  //remember that background bright is really blink!
  { coWhite,coBlack},//isaBackground
  { coBlack,coWhite},//isaHilite
  { coBlack,coWhite},//isaGreyed
  { coWhite,coBlack},//isaEnabled
  { coWhite,coBlack},//isaDisabled
  { coWhite,coBlack},//isaAlert
  { coWhite,coBlack},//isaDialogBox
  //{ coBlack,coWhite}, //isaPushButton
  { coBlack,coWhite},//isaPushButton
  { coBlack,coWhite},//isaButtonDown
  { 0,0},                               //isaListBox
  { coBlack,coWhite},//isaScrollbar
  { coWhite,coBlack},//isaElevator
  { coWhite,coWhite},//isaMenuBox
  { coWhite,coBlack},//isaMenu
  { coBlack,coWhite},//isaMenuSelected
  { coBlack,coWhite},//isaMenuHilite
  { coWhite,coBlack},//isaMenuHiliteSel
  { coBlack,coWhite},//isaItemHiliteSel
  { coWhite,coBlack},//isaDialogAccel
  { coWhite,coBlack},//isaDialogAccelBor
  { coBlack,coBlack},//isaShadow
  { coBlack,coWhite},//isaWhiteOnBlack
  { coWhite,coBlack},//isaBlackOnWhite
  { coBlack,coWhite},//isaTitlebar
  { coWhite,coBlack},//isaDriveicon
  { coWhite,coBlack},//isaDrivebox
  { coBlack,coWhite},   //isaFocus
  { coBlack,coBlack},   //isaBorders
  { coWhite,coBlack},//isahotlink
  { coWhite,coBlack},//isaShellMouse
  { coBlack,coWhite},//isaMessagebar
};


COLORTABLEENTRY TextColorTable[NUMISASUSED] =
{

  //background,foreground
  { coWhite+isBright,coBlack},                    //isaBackground
  { coBlue,coWhite+isBright},   //isaHilite
  { coBlue+isBright,coBlack+isBright},    //isaGreyed
  { coWhite+isBright,coBlack},                    //isaEnabled
  { coWhite+isBright,coWhite},                    //isaDisabled
  { coWhite+isBright,coRed+isBright},                     //isaAlert
  { coWhite+isBright,coBlack},                    //isaDialogBox
  { coBlue,coWhite+isBright},                     //isaPushButton
  { 0,0},                                 //isaButtonDown
  { 0,0},                                 //isaListBox
  { coBlack,coWhite+isBright},                    //isaScrollbar
  { coWhite,coWhite},   //isaElevator
  { coWhite+isBright,coBlack },                   //isaMenuBox
  { coWhite+isBright,coBlack},                    //isaMenu
  { coBlue,coWhite+isBright},                   //isaMenuSelected
  { coWhite+isBright,coCyan},                       //isaMenuHilite
  { coBlue,coCyan},               //isaMenuHiliteSel
  { coWhite+isBright,coCyan},             //isaItemHiliteSel
  { coBlack+isBright,coBlack},            //isaDialogAccel
  { coBlack+isBright,coBlack},            //isaDialogAccelBor
  { coWhite,coBlack+isBright},            //isaShadow
  { coBlack,coWhite+isBright},            //isaWhiteOnBlack
  { coWhite+isBright,coBlack},            //isaBlackOnWhite
  { coWhite,coBlack},                     //isaTitlebar
  { coWhite+isBright,coBlack},                    //isaDriveicon
  { coWhite+isBright,coBlack},             //isaDrivebox
  { coBlack,coWhite+isBright},             //isaSelect
  { coBlack,coBlack},            //isaBorders
  { coWhite+isBright,coCyan}, //isaHotLink
  { coCyan+isBright,coBlack}, //isaShellMouse
  { coWhite,coBlack},                     //isaMessagebar
};

#if 0
COLORTABLEENTRY GraphicsColorTable[NUMISASUSED] =
{
  //background,foreground
  { coWhite+isBright,coBlack},                    //isaBackground
  { coBlue,coWhite+isBright},   //isaHilite
  { coBlue+isBright,coBlack+isBright},    //isaGreyed
  { coWhite+isBright,coBlack},                    //isaEnabled
  { coWhite+isBright,coWhite},                    //isaDisabled
  { coWhite+isBright,coRed+isBright},                     //isaAlert
  { coWhite+isBright,coBlack},                    //isaDialogBox
  { coWhite+isBright,coBlue},                     //isaPushButton
  { 0,0},                                 //isaButtonDown
  { 0,0},                                 //isaListBox
  { coBlack,coWhite+isBright},                    //isaScrollbar
  { coWhite+isBright,coWhite+isBright},   //isaElevator
  { coWhite+isBright,coWhite+isBright },                          //isaMenuBox
  { coWhite+isBright,coBlack},                    //isaMenu
  { coBlue,coWhite+isBright},   //isaMenuSelected
  { coBlue,coWhite+isBright},                       //isaMenuHilite
  { coBlack+isBright,coBlack},            //isaMenuHiliteSel
  { coBlack+isBright,coBlack},            //isaItemHiliteSel
  { coBlack+isBright,coBlack},            //isaDialogAccel
  { coBlack+isBright,coBlack},            //isaDialogAccelBor
  { coWhite,coBlack+isBright},            //isaShadow
  { coBlack,coWhite+isBright},            //isaWhiteOnBlack
  { coWhite+isBright,coBlack},            //isaBlackOnWhite
  { coBlue,coBlack},                      //isaTitlebar
  { coBlack,coWhite},                     //isaDriveicon
  { coBlack+isBright,coBlack},             //isaDrivebox
  { coBlack,coWhite+isBright},             //isaSelect
  { coBlack,coBlack},            //isaBorders
  { coBlack+isBright,coBlack}, //isaHotLink
};

#endif

#define MAXSCREENMODE 16

WORD LinesToMode(WORD lines)
{
	int i;
	for(i=0; i<gNumScreenModes; ++i)
	if(sScreenMode[i].NumLines == (BYTE) lines)
	{
		return(sScreenMode[i].ModeInd);
	}
	return(ReturnScreenMode); // return known valid if none
}

TOKEN ModeMap(int mode)
{
	char tstr[MAXSCREENMODETITLE], *szWhichRes;
	TOKEN tkRes;
	int i;

	for(i=0; i<gNumScreenModes; ++i)
	if(sScreenMode[i].ModeInd == (BYTE)mode)
		break;

	if(m_fIsLowResMode(sScreenMode[i].NumLines))
	tkRes = TK_LOWRES;
	else if(m_fIsMedResMode(sScreenMode[i].NumLines))
	tkRes = TK_MEDIUMRES;
	else
	tkRes = TK_HIGHRES;

	if(!sScreenMode[i].Res_Ordinal)
	return(tkRes);
	else {
	strfcpy(tstr, Get_Token_Identifier(tkRes));
	for(szWhichRes=tstr; *szWhichRes!='\0'; ++szWhichRes) /* do nothing */ ;
	itoa(sScreenMode[i].Res_Ordinal, szWhichRes, 10);
	return(Get_Identifier_Token(tstr));
	}
}

VOID Set_DefaultColors(void)
{
	ISA i;
	COLORTABLEENTRY *colortable;

	static WORD oldshadow = -1;
	if(oldshadow == -1)
		oldshadow = diShadow;
	if(fMonochrome)
	{
		colortable = MonoColorTable;
	}
	else
	{
//          if(gisgraph)
//              colortable = GraphicsColorTable;
//          else
		   colortable = TextColorTable;
	}
	for(i=0;i<NUMISASUSED;i++)
	{
		SetIsaColor(i,colortable[i].foreground,
			  colortable[i].background);
	}
	if(gisgraph)
	{
		SetIsaColor(isaMenuBox,coWhite+isBright,coWhite+isBright);
	   SetIsaFfont(isaMenuHilite,ffontUnderline);
	   SetIsaFfont(isaMenuHiliteSel,ffontUnderline);
	   SetIsaFfont(isaItemHiliteSel,ffontUnderline);
	   diShadow = 0;    /* no character shadows in graphics mode ! */

	}
	else
	{
	   diShadow = oldshadow;
	}

}


VOID FAR InitCColor()
{
	Set_DefaultColors();
	gNthColorGroup=GetUserColorSelection();
	if(gNthColorGroup)
		SelectColorGroup(gNthColorGroup);

}

#if 0
VOID FAR InitGColor()
{
	int i;
	for(i=0;i<NUMISASUSED;i++)
	{
		SetIsaColor(i,GraphicsColorTable[i].foreground,
			  GraphicsColorTable[i].background);
	}
	SetIsaFfont(isaMenuHilite,ffontUnderline);
	SetIsaFfont(isaMenuHiliteSel,ffontUnderline);
	SetIsaFfont(isaItemHiliteSel,ffontUnderline);
	diShadow = 0;    /* no character shadows in graphics mode ! */

	SelectColorGroup(gNthColorGroup);
}
#endif

/* Returns a BOOL indicating whether the mode specified by "inst" is a mode
 * we want to be able to switch to in the SHELL. Modes with columns less
 * than 60 cols or > 90 columns are not modes we want to switch to.
 */
BOOL FIsModeOk(INST *inst)
{
   return ( (inst->finst & finstAvailable)  &&
				!(inst->finst & finstQuestionable) &&
				!(inst->finst & finstDisableMouse)      &&
				(inst->axMac >= 60) &&
				(inst->axMac <= 90)) ;

} /* FIsModeOk */


/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

/*  NAME
 *      SetScreenMode
 *  DESCRIPTION
 *      Changes the screen mode
 *  ARGUMENTS
 *      mode is the screen mode to switch to
 *  RETURN VALUE
 *      TRUE if successful
 *      FALSE otherwise
 *  EFFECTS
 *      Changes globals (above). Resets screen state.
 *  STATE
 *      Screen state is changed
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
BOOL FAR SetScreenMode(WORD mode)
{
	if(mode == imodeUnknown)
		return(FALSE);

	if (gisgraph)
	{
		/* The following call is supposed to cause CW to free its old
		 * screen driver allocations. We shall be allocating new buffers
		 * below.
		 */
		TermGraphics();
	}

#ifdef KANJI
	if (gfScreenModeHasBeenSetOnce)
	{
		FreeInstBuffers(&ginst, FreeDriverMem) ;
	}
	if (!FQueryInst(&ginst, mode))
		return(FALSE);
	FAllocInstBuffers(&ginst, LpwAllocDriverMem, TRUE );
	gfScreenModeHasBeenSetOnce = TRUE ;
	gscreenmode = mode;
#else
	if (FQueryInst(&ginst, mode))
	{
		/* Free any driver memory allocated previously, if there was a previous
		 * case (i.e., this is not the first time we SetScreenMode()!
		 */
		if (gfScreenModeHasBeenSetOnce)
		{
			/* ZZZZZZZ */
			/* Use FreeInstBuffers() call to do this. This call was there
			 * when we were using an early version of CW. scottq had some
			 * warning next to it saying that it did not work properly
			 * and that it needed fixing. I don't want to de-stabilize the
			 * SHELL attempting to use it now. This is a good thing to do
			 * when we go to version 5.1 Note that we have a near memory
			 * allocation stuff in LpwAllocDriverMem and that we cannot
			 * free our near memory because we use it in a stack form!
			 */
			OurHackFreeDriverMem() ;
		}

		FAllocInstBuffers(&ginst, LpwAllocDriverMem, TRUE);
		gfScreenModeHasBeenSetOnce = TRUE ;
		gscreenmode = mode;
	}
	else
		return(FALSE);
#endif

	/* Screen mode is changing so clear the icon caches! */
	InitIconCache() ;

	inch = cinch;
#ifdef OLDCW
	if (!FInitScreen(&ginst))
		 return(FALSE);
#else
	HookISR10();  //called before screen info for slow mouse problem
					  //see asmutils.asm for details!
	if (!FInitScreen(&ginst,0))
	{
		  UnHookISR10();
		 return(FALSE);
	}
	//TURN mouse off during start-up
	//Don't use mouse init yet, since it will turn mouse on!
	FEnableMouse(FALSE);
#endif

	if (DriverFound && (FInitGraphics(&ginst,&gingd)))
	{
		gisgraph = TRUE;
		SetPrinting(FALSE);
	}
	else
	{
		CWIDTH = CHEIGHT = 1 ;
		gisgraph = FALSE;
	}
	UnHookISR10();

	InitCColor();
	if (gisgraph)
	{
		 chRightSide1 = ' ';
		 chRightSide2 = ' ';
		 chTopSide1             = ' ';
		 chBottomSide1          = ' ';
		 chLeftSide1            = ' ';
		 chRightSide1           = ' ';
		 chTopLeftCorner1       = ' ';
		 chTopRightCorner1      = ' ';
		 chBottomLeftCorner1    = ' ';
		 chBottomRightCorner1   = ' ';
		 chMiddleLeft1          = ' ';
		 chMiddleRight1         = ' ';
		 chTopSide2             = ' ';
		 chBottomSide2          = ' ';
		 chLeftSide2            = ' ';
		 chRightSide2           = ' ';
		 chTopLeftCorner2       = ' ';
		 chTopRightCorner2      = ' ';
		 chBottomLeftCorner2    = ' ';
		 chBottomRightCorner2   = ' ';
		 //chBullet             = ' ';
		 chMiddleDot            = ' ';
		 chScrollbar            = ' ';
		 chElevator             = ' ';
		 chShadowInit           = ' ';

	}
	else
	{
		 inch = cinch;
		 chTopSide1             = ' ';
		 chBottomSide1          = ' ';
	}
	if(gisgraph)
	   Set_KeyWord_Assignment(TK_SAVESTATE,TK_SCREENMODE,TK_GRAPHICS);
	else
	   Set_KeyWord_Assignment(TK_SAVESTATE,TK_SCREENMODE,TK_TEXT);
	/* Remember that Mode map is zero based */
	Set_KeyWord_Assignment(TK_SAVESTATE,TK_RESOLUTION,ModeMap(mode));
	return(TRUE);
}


/*  NAME SelectScreenMode
 *  DESCRIPTION
 *      Figures out the appropriate screen mode to switch to on startup
 *      and fills in the INST structure passed to it.
 *  ARGUMENTS
 *      Pointer to an INST structure which holds information about the
 *      selected screen mode's capabilities.
 *  RETURN VALUE
 *      Mode selected to switch to.
 *  EFFECTS
 *      NONE
 *  STATE
 *      NONE
 *  COMMENTS
 *      This code needs a better way of determining which mode to go to
 *      This was changed to check for the number of lines requested directly
 *  WARNINGS
 *      The INST structure is filled in for the mode selected only, so
 *      you cannot just blow off the mode returned if you don't like it.
 *  HACKS
 *      Hard coded mode to switch to
 */

int FAR SelectScreenMode(INST *inst)
{
	int i, nRes, temp;
	char tstr[MAXSCREENMODETITLE], *szWhichRes, far *str;
	BOOL istext, fAnything;
	TOKEN tkRes;

	if(DriverFound) {
	istext = Get_KeyWord_Assignment(TK_SAVESTATE,TK_SCREENMODE)
		!= TK_GRAPHICS;

/* Get the resolution string */
	strfcpy(tstr, Get_Token_Identifier(Get_KeyWord_Assignment(TK_SAVESTATE,
		TK_RESOLUTION)));
/* Skip to the first numeric */
	for(szWhichRes=tstr; *szWhichRes!='\0' && (*szWhichRes<'0'
		|| *szWhichRes>'9'); ++szWhichRes) /* do nothing */ ;
/* Get the Res_Ordinal; 0 -> 1 in case there are many modes of a given res */
	nRes = atoi(szWhichRes);
	if(!nRes)
		nRes = 1;

/* Get the resolution name; default to HIGHRES */
/* This will actually find a match if one of the resolutions is a prefix
   of the requested resolution.  The next line is so that there is no match
   at the end of the requested string */
	*szWhichRes = '0';
	for(str=Get_Token_Identifier(TK_LOWRES), szWhichRes=tstr;
		*str==*szWhichRes; ++str, ++szWhichRes) /* do nothing */ ;
	if(!(*str))
		tkRes = TK_LOWRES;
	else {
		for(str=Get_Token_Identifier(TK_MEDIUMRES), szWhichRes=tstr;
			*str==*szWhichRes; ++str, ++szWhichRes) /* do nothing */ ;
		if(!(*str))
		tkRes = TK_MEDIUMRES;
		else
		tkRes = TK_HIGHRES;
	}

/* Look for the closest mode.  Order of importance is (high to low):
   text/graphics
   resolution name
   resolution number
 */
	do {
		fAnything = FALSE;
		do {
		for(i=gNumScreenModes-1; i>=0; --i) {
			temp = sScreenMode[i].fIsGraphicsMode;
			if((istext && temp) || (!istext && !temp)
				|| sScreenMode[i].Res_Ordinal > (BYTE) nRes)
			continue;
			if(fAnything)
			break;

			if(m_fIsLowResMode(sScreenMode[i].NumLines)) {
			if(tkRes == TK_LOWRES)
				break;
			} else if(m_fIsMedResMode(sScreenMode[i].NumLines)) {
			if(tkRes == TK_MEDIUMRES)
				break;
			} else {
			if(tkRes == TK_HIGHRES)
				break;
			}
		}
		} while(!fAnything && i==-1 && (fAnything=TRUE)) ;
/* fAnything gets set if we can't find any modes with the res name */
	} while(!istext && i==-1 && (istext=TRUE)) ;
/* Yes, I do mean istext=TRUE on the previous line, not == */
/* istext gets set if there are no graphics modes */

	i = (i == -1 ? ReturnScreenMode : sScreenMode[i].ModeInd);
	} else {
	i = ReturnScreenMode;
	}

	FQueryInst(inst, i);
	return(i);
}


/*  NAME GetTheScreenMode
 *  DESCRIPTION
 *      Figures out what screen mode the screen is in; or if the lowest
 *  one supported.
 *  ARGUMENTS
 *  RETURN VALUE
 *      Mode of screen.
 *  EFFECTS
 *      Sets global StartupNumLines to record number of lines to start
 *  Switcher apps in.
 *      NONE
 *  STATE
 *      NONE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

WORD FAR GetTheScreenMode()
{
	WORD themode;
	int i;

	themode = ImodeGuessCurrent();
	if(themode == imodeUnknown)
	{
		for(i=0; i < 16; i++)
		{
		  if ( FQueryInst(&ginst, i) && FIsModeOk(&ginst) )
		  {
			 StartupNumLines = ginst.ayMac;
			 themode = i;
			 break;
		  }
		}
	}
	else
	{
	if (FQueryInst(&ginst, themode))
	{
		  StartupNumLines = ginst.ayMac;
	}
	}
	return(themode);
}


void InitializeScreenManagerGlobals(void);



/*  NAME
 *      InitializeScreen
 *  DESCRIPTION
 *      Initializes the screen and the CW environment
 *      Which in turn sets up global state variables
 *      ...
 *  ARGUMENTS
 *      NONE
 *  RETURN VALUE
 *      TRUE if initialization is successful
 *      FALSE otherwize
 *  EFFECTS
 *      Heavy duty--takes over the screen, mouse, keyboard
 *      Initializes globals (above)
 *  STATE
 *      If return value is TRUE, program in ready to use all CW
 *      and screen operations. No state change if FALSE is returned.
 *  COMMENTS
 *      FInitCow may not be necessary, since it is old CW.
 *      RerrLoadCwDrv loads in the basics of the video driver,
 *      FLoadGsd loads in the graphics functions of the video driver.
 * WARNINGS
 *      NONE
 * HACKS
 *      Currently has hardcoded video driver name, looks only in
 *      current directory!!!
 *      draw a rectangle for no good reason
 */
BOOL FAR InitializeScreen(void)
{
	int ind ;
	char videodriver[MAX_PATH+1];
	TOKEN videodir;

	if (!FInitCow())
	{
		return(FALSE);
	}

	videodir = Get_KeyWord_Assignment(TK_SAVESTATE,TK_VIDEODIR);
	if(videodir < 0)
	{
		strcpy(videodriver,gStartUpDir);
		ind = gStartUpDirEnd ;
	}
	else
	{
		strfcpy(videodriver,Get_Token_Identifier(videodir));
		ind = strlen(videodriver);
	}
	/* be sure the path ends in '\' */
#ifdef DBCS
	if (videodriver[ind-1] != '\\' || CheckDBCSTailByte(videodriver,&videodriver[ind-1]))
#else
	if (videodriver[ind-1] != '\\')
#endif
	{
		videodriver[ind++] = '\\' ;
	}
	strcpy(videodriver+ind,szScreenDrv);


	if (!(RerrLoadCwDrv(videodriver) == 0))
	{
		DriverFound = FALSE;
		/* We use the internal default driver */
	}
	else
	{
		if (!FLoadGsd(videodriver))
		{
			DriverFound = FALSE;
			/* no graphics, aye! */;
		}
	}

	/* This call needs to be made after Initializing the screen -- i.e., 
	 * after loading any available screen driver.
	 */
	InitializeScreenManagerGlobals() ;

	ReturnScreenMode = GetTheScreenMode();

	gStartUpDir[gStartUpDirEnd]=0;
	gscreenmode = SelectScreenMode(&ginst);
	cinch = inch;

	if(gscreenmode!=3 && FQueryInst(&ginst, 3)) {
		HookISR10();  //called before screen info for slow mouse problem
					  //see asmutils.asm for details!
		FInitScreen(&ginst, 0);
		UnHookISR10();
	}
	SetScreenMode(gscreenmode);
	
	gfScreenModeChanged = TRUE ;



	return(TRUE);
}

void OurUnHookISR9(void)
{
	if (gfOurISR9Installed)
	{
		UnHookISR9() ;
		gfOurISR9Installed = FALSE ;
	}

} /* OurUnHookISR9 */
/*
 *      We are really exiting back to the parent command
 */
VOID SetUpExitToDos(void)
{
#ifndef NOLOADER
	realdma_ptr = GET_COMMAND_PTR();
	realdma_ptr[EXITPTROFFSET] = 0xFF;
#endif

}

/* Note that this function can be invoked from the Menu -- "Exit" of
 * Program Manager and all such functions accept no arguments!! Hence
 * we use the global variable gfEarlyExit
 */

extern void StoreFMState(void) ;
extern BOOL gTaskListEnabled;
extern ListBoxData TaskList;

VOID FAR DoExit(void)
{
#if 0
	int ind ;
#endif
	extern BOOL gfEarlyExit ;
	char *pExitDir ;
	char ExitDir[MAX_PATH+1] ;
	int dummylen ;

	/* If it is an early exit, GetNumItems() should not be called as that
	 * function pointer is not yet setup!
	 */
	if ((!gfEarlyExit) && (gTaskListEnabled) && (GetNumItems(&TaskList) > 0))
	{
		/* Tell the user that we can't exit with tasks swapped out */
		ShellMessageBox(szExitErrorTitle, szExitErrorMsg) ;
		return ;
	}

#ifdef KANJI
	TermKkc();
#endif

	SetUpExitToDos();

	if (!gfEarlyExit)
	{
		StoreFMState() ;

		if (gfSwapHandlerInstalled)
			RemoveSwapHandler() ;

		OurUnHookISR9() ;

		Set_KeyWord_Assignment(TK_SAVESTATE, TK_FORCEMONO, TK_UNKNOWN);
		
		pExitDir = gStartInDir ;

		if (gfFMVisited)
		{
			Tree2Path(listinfo[glob.FocusBox].tree,
					   listinfo[glob.FocusBox].files, ExitDir, &dummylen) ;
			pExitDir = ExitDir ;
		}
		/* WARNING Trees are toasted in 
		 * buffered_write_ini_file, don't do tree2path anymore!
		 */
		if(!Buffered_Write_Ini_File(TRUE))
		{
			ErrorCrit = 34; // don't allow idle to process
			ShellMessageBox(szNoIniTitle, szNoIniMsg1);
			ErrorCrit = 0xFF;
		}

		UnixChdir(pExitDir);

		SetScreenMode(ReturnScreenMode);

	EndScreen(TRUE);
	EndCow(TRUE);
	}

#if 0
	Beep() ;
	printf("Num Modes = %d\n", gNumScreenModes) ;
	for(ind = 0 ; ind < gNumScreenModes ; ind++)
	{
		printf("mode=%d, lines=%d, ord=%d, fgraphics=%d\n", 
															sScreenMode[ind].ModeInd,
															sScreenMode[ind].NumLines,
															sScreenMode[ind].Res_Ordinal,
														sScreenMode[ind].fIsGraphicsMode) ;

	}
#endif
	//DisableUMBS();
	 DTS_API_Exit();
	exit(0);
}

/* Initialize the globals that will be used by the Color Manager to change
 * screen colors.
 */
void InitializeColorManagerGlobals(void)
{
	TOKEN tokencolor ;

	/* Calculate and store the total number of available color schemes in
	 * "gNumColorSchemes". This is used by the color manager.
	 */
	tokencolor = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_COLOR);
	
	if(tokencolor < 0)
	{
		/* ZZZZZ This should never happen for good any correct INI file as
		 * we should have at least 1 color scheme! Maybe the default color
		 * scheme that the SHELL uses in cases COLORS is not set will
		 * cause this not to happen!
		 */
		gNumColorSchemes = 0;
	}
	else
		gNumColorSchemes = Get_List_Length(tokencolor);

	/* set the global variable for colorbox title now. It needs to be done
	 * just once for the color manager. So we do it now.
	 */
	strcpy(gColorBoxTitle, szCurrentScheme) ;
	
	/* gCurrentColorTitle is using the latter part of the space allocated
	 * to gColorBoxTitle. This saves the concatenation we need to do to
	 * create the color manager's listbox title, each time the color
	 * scheme changes!
	 */
	gCurrentColorTitle = gColorBoxTitle + strlen(szCurrentScheme) ;

} /* InitializeColorManagerGlobals */

/* For all the acceptable screen modes detected by us and stored in the
 * data structure "sScreenMode", update the "Res_Ordinal" field (this is the
 * resolution's ordinal number). If we have more than 1 screen mode with
 * say medium resolution, the one with lowest mode number gets the ordinal
 * number 1, the next one 2, etc. This will be used by the code that
 * changes screen modes to display strings like:
 * "Medium Resolution Graphics 1", "Medium Resolution Graphics 2", etc.
 */
void SetScreenModeOrdinals(void)
{
	/* The maximum ordinal values seen so far in low res text, med res text,
	 * high res text, low res graphics, med res graphics and high res graphics
	 * the elements with array subscript 0 is for graphics mode and 1 for text!
	 */
	BYTE lr_ord[2], mr_ord[2], hr_ord[2] ;
	int i ;

	/* Fast initialization of the two array elements (BYTE) to 0! */
	*((WORD *) lr_ord) = 0 ;
	*((WORD *) mr_ord) = 0 ;
	*((WORD *) hr_ord) = 0 ;

	/* Perform initial marking */
	for (i = 0 ; i < gNumScreenModes ; i++)
	{
		if (m_fIsLowResMode(sScreenMode[i].NumLines))
		{
			sScreenMode[i].Res_Ordinal= ++(lr_ord[sScreenMode[i].fIsGraphicsMode]);
		}
		else if (m_fIsMedResMode(sScreenMode[i].NumLines))
		{
			sScreenMode[i].Res_Ordinal= ++(mr_ord[sScreenMode[i].fIsGraphicsMode]);
		}
		else
			sScreenMode[i].Res_Ordinal= ++(hr_ord[sScreenMode[i].fIsGraphicsMode]);

	} /* for */

	/* Second pass for marking! In case there is just 1 low resolution mode
	 * say, we want to reset the ordinal value to 0. This is used by the
	 * list box routine in the screen mode changer to avoid displaying
	 * the ordinal value of 1, as there is anyway no 2 in this mode!
	 */
	for (i = 0 ; i < gNumScreenModes ; i++)
	{
		if (m_fIsLowResMode(sScreenMode[i].NumLines))
		{
			if (lr_ord[sScreenMode[i].fIsGraphicsMode] == 1)
				sScreenMode[i].Res_Ordinal = 0 ;
		}
		else if (m_fIsMedResMode(sScreenMode[i].NumLines))
		{
			if (mr_ord[sScreenMode[i].fIsGraphicsMode] == 1)
				sScreenMode[i].Res_Ordinal = 0 ;
		}
		else
		{
			if (hr_ord[sScreenMode[i].fIsGraphicsMode] == 1)
				sScreenMode[i].Res_Ordinal = 0 ;
		}
	} /* for */
} /* SetScreenModeOrdinals */

/* Detect all the acceptable modes supported by the current display and the
 * loaded driver (if any). Store the relevant information in the data
 * structure "sScreenMode".
 */
void InitializeScreenManagerGlobals(void)
{
	int i ;
	INST inst ;

	gNumScreenModes = 0 ;

	for (i = 0 ; i < MAX_SCREEN_MODES ; i++)
	{
		if ( FQueryInst(&inst, i) && FIsModeOk(&inst) )
		{
			/* The screen mode ordinals are set after this loop! */
			sScreenMode[gNumScreenModes].ModeInd = (BYTE) i ;
			sScreenMode[gNumScreenModes].NumLines = inst.ayMac ;
			sScreenMode[gNumScreenModes].fIsGraphicsMode = 
													(MyBOOL) !(inst.finst & finstText) ;
			gNumScreenModes++ ;
		}
	} /* for */
	SetScreenModeOrdinals() ;
} /* InitializeScreenManagerGlobals */


VOID Arrow(void);

extern WORD gListKeyDelay;
extern int FarToNearsz(char *, char far *, int);
extern VOID FirstMouseInit(void);

BOOL FAR InitializeShell(VOID)
{
	TOKEN tkTemp;
	RRC rrcClient ; /* temp */

	/* commandline overrides inifile */
	ParseCommandLine();

	/* The following call needs to be done before InitializeScreen() as that
	 * tries to set the screen color and that function needs the global
	 * "gCurrentTitle" to be set!
	 */
	InitializeColorManagerGlobals() ;

	/*
	 *      Initialize Character Windows environment
	 */
	if (!InitializeScreen())
		return(FALSE);

	Arrow(); /* be sure the mouse is an arrow! */

	/*
	 * CW actually sets up its critical error handler. We replace it now.
	 * Set up our critical error handler... Initially, we were setting our
	 * handler earlier and our handler never got control!
	 */
	SetCriticalsToFail();


	InitWindows(TRUE); /* Yes, we are calling this routine for the first time */

	AddChild(NULL, &MainWind);
	DoInitMenus();
	SetMenuKeys(VK_ALT,VK_F10);
	GetClientRrc(&MainWind,&rrcClient);

	// We need a pointer to an ARC, but RRCs and ARCs are isomorphic in struct!
	// So following cast is OK!
	SetGraphicArc((PARC)&rrcClient, TRUE);


	InitFileMgr();
	FirstInitTaskMan();

	if((tkTemp=Get_KeyWord_Assignment(TK_SAVESTATE, TK_LISTKEYDELAY)) < 0)
		gListKeyDelay = 0;
	else {
		char tstr[10];

		FarToNearsz(tstr, Get_Token_Identifier(tkTemp), sizeof(tstr));
		gListKeyDelay = atoi(tstr);
	}

	if((tkTemp=Get_KeyWord_Assignment(TK_SAVESTATE, TK_READUPDATEFREQ)) < 0)
		gReadUpdateFreq = DEFAULT_READ_UPDATE_FREQ;
	else {
		char tstr[10];

		FarToNearsz(tstr, Get_Token_Identifier(tkTemp), sizeof(tstr));
		gReadUpdateFreq = atoi(tstr);

		/* Do the following sanity check */
		if (gReadUpdateFreq <= 0)
			gReadUpdateFreq = DEFAULT_READ_UPDATE_FREQ ;
	}

	if (Get_KeyWord_Assignment(TK_SAVESTATE,TK_BEEP) == TK_DISABLED)
		gBeeps_On = FALSE ;

	if (Get_KeyWord_Assignment(TK_SAVESTATE,TK_SWAPMOUSE) == TK_ENABLED)
	{
		// ZZZZZZZZ The following API does not seem to exist.
		SwapMouseButton(TRUE) ;
	}

#ifdef KANJI
	AddChild(NULL, &wndKkcStatus);
	FInitKkc(&wndKkcStatus);
#endif

	if (Get_KeyWord_Assignment(TK_SAVESTATE,TK_STARTUP) == TK_FILEMGR)
	{
		gfFileMgrStartUp = TRUE ;
		DoFileMgr();
	}
	else
	{
	   InitializeStartPrograms();
	}

	FirstMouseInit();
	FInitMouseNest();
	//EnableUMBS();
	return(TRUE);
}


/* This is the function that gets control when user selects the Change Screen
 * menu item (Options.Display)!
 */
/* ZZZZZ Actually to save some code size, one can share code between the
 * ColorBox() and its subroutines with ScreenBox() and its subs. We can
 * actually use "ColorList" in place of "ScreenModeList" as these two
 * dialogs cannot be active at the same time!
 */
VOID ScreenBox(void)
{
	WORD savedscreenmode ;

	gContinueScreens = SCREENPREVIEW ;
	
	savedscreenmode = gscreenmode ;
	/* Loop until user selects OK or CANCEL button in the color manager dialog.
	 * The fn ColorDialog() modifies the global variable gContinueColors.
	 * OK translates to a value of 0, Cancel to -1 and a selection of the
	 * screen mode using mouse (double-click or keyboard "enter") translates
	 * to +1.
	 */
	while(gContinueScreens >= SCREENPREVIEW)
	{
		/* Put up screen manager dialog and set the global "gContinueScreens".
		 * Also, sets the variable "gScreenListInd" indicating which is the new
		 * mode that the user wants to be put into.
		 */
	   ScreenDialog();

		/* Draw the screen based on the user's chosen screen mode. Do this if
		 * user didn't choose CANCEL.
		 */

		/* If the user chooses a different screenmode and if he didn't choose
		 * CANCEL, redraw the screen based on new mode.
		 */
	   if ( (gContinueScreens >= SCREENOK) && 
					(sScreenMode[gScreenListInd].ModeInd != (BYTE) gscreenmode) )
	   {
			/* ZZZZZZ Is this InitCColor needed? */
			InitCColor() ;

			SetScreenMode(sScreenMode[gScreenListInd].ModeInd) ;
			InitWindows(FALSE);
			DrawScreen() ;
	   }
	}

	/* Did the user select "Cancel" to skip the screen mode change? If so, put
	 * him back in the original screen mode that he started the Change
	 * screens dialog in.
	 */
	if(gContinueScreens == SCREENCANCEL)
	{
		/* if the screen mode has changed, restore the original mode */
		if (savedscreenmode != gscreenmode)
		{
			/* ZZZZZZ Is this InitCColor needed? */
			InitCColor() ;

			SetScreenMode(savedscreenmode) ;
			InitWindows(FALSE);
			DrawScreen() ;
		}
	}

} /* ScreenBox */

/* This function returns the index of the screen mode within our array
 * of screen mode structures. "screenmode" is the screen mode that
 * CW passes to us when we query it about screen modes and it is the
 * same mode that we use to SetScreenModes.
 */
int GetScreenModeIndex(WORD screenmode)
{
	int j ;

	/* Locate the index "j" of the screen mode "screenmode"
	 * The index is the subscript into our array structure.
	 */
	for (j = 0 ; j < gNumScreenModes ; j++)
	{
		if (screenmode == sScreenMode[j].ModeInd)
			break ;
	}

	return j ;

} /* GetScreenModeIndex */


/* This function forms the "title" that will be displayed in the 
 * Screen mode change Listbox (within the dialog box).
 * The current screen mode is "gscreenmode". The string is of the form
 * "Graphics Mode (xx lines)" or "Text Mode (xx lines)".
 */
void FormCurrentScreenModeTitle(char *title)
{
	int i, j ;
	char *pszStr ;

	j = GetScreenModeIndex(gscreenmode) ;

	/* The screenmode must be a mode we have already detected */
	assert (j < gNumScreenModes) ;

	pszStr = (!sScreenMode[j].fIsGraphicsMode) ? szText : szGraphics ;

	strcpy(title, szCurrentMode) ;
	i = strlen(szCurrentMode) ;

	while (*pszStr && *pszStr!=' ')
	{
		title[i++] = *(pszStr++) ;
	}
	
	title[i++] = ' ' ;

	/* append the number of lines info */
	title[i++] = (char) LBRACKET_CHAR ;

	/* WARNING! Assuming that number of lines is always a 2 digit value! */
	title[i++] = (char) ('0' + sScreenMode[j].NumLines/10) ;
	title[i++] = (char) ('0' + sScreenMode[j].NumLines%10) ;
	title[i++] = ' ' ;
	
	pszStr = szLines ;
	while (*pszStr)
		title[i++] = *(pszStr++) ;

	title[i++] = (char) RBRACKET_CHAR ;

	title[i] = '\0' ;
	
} /* FormCurrentScreenModeTitle */


/* This function puts up the screen manager dialog box and depending on the
 * user's response (OK, CANCEL or direct mode selection) sets the global
 * variable "gContinueScreens" that is used by its caller to decide whether
 * to dismiss the screen manager dialogs or perform appropriate actions.
 */
VOID FAR ScreenDialog(void)
{
	TMC tmc;
	HCABscreen h ;
	char ScreenModeTitle[75] ;

	h = HcabAlloc(cabiCABscreen) ;

	if (!h)
	{
		OutOfMemory() ;
		return ;
	}
	InitCab(h,cabiCABscreen);

	FormCurrentScreenModeTitle(ScreenModeTitle) ;

	/* Store the pointer to title in this variable, so that the
	 * dialog procedure can access this.
	 */
	gpszScreenModeTitle = ScreenModeTitle ;

	SzToCab(h, szEnterButton, Iag(CABscreen, pszscreenEB));
	SzToCab(h, szCancelButton, Iag(CABscreen, pszscreenCB));
	SzToCab(h, szPreviewButton, Iag(CABscreen, pszscreenPB));

	tmc= MyTmcDoDlg(&dlgscreen,  h);

	switch (tmc)
	{
		case tmcOK:
			gContinueScreens = SCREENOK ;
			break ;

		case tmcCancel:
			gContinueScreens = SCREENCANCEL ;
			break ;

		// case tmcscreenpreview:
		default:
/* gContinueScreens should already be set to the scroll of the listbox */
			break ;
	} /* switch */

	FreeCab(h);

} /* ScreenDialog */

/* The total number of screen modes supported by the current display */
#define GetNumScreenModes() (gNumScreenModes)

/* The number of screen mode titles that will be visible at any point of
 * time in the screen mode listbox.
 */
#define SCREENMODEWINDOWSIZE 4

/*
 * fill in string "szScreenMode" with the ith screen mode permitted.
 * by the display -- 0 based.
 * A sample string is: "Low Resolution Graphics 1"
 */ 
VOID GetNthScreenModeString(int isz, char *szMode)
{
	char *pszStr ;
	int i ;

	i = 0 ;

	pszStr = (!sScreenMode[isz].fIsGraphicsMode) ? szText : szGraphics ;
	while (*pszStr)
		szMode[i++] = *(pszStr++) ;
	szMode[i++] = ' ' ;
	szMode[i++] = ' ' ;

	/* append the number of lines info */
	/* WARNING! Assuming that number of lines is always a 2 digit value! */
	szMode[i++] = (char) ('0' + sScreenMode[isz].NumLines/10) ;
	szMode[i++] = (char) ('0' + sScreenMode[isz].NumLines%10) ;
	szMode[i++] = ' ' ;

	pszStr = szLines ;
	while (*pszStr)
		szMode[i++] = *(pszStr++) ;
	szMode[i++] = ' ' ;
	szMode[i++] = ' ' ;

	if (m_fIsLowResMode(sScreenMode[isz].NumLines))
		pszStr = szLowResolution ;
	else if (m_fIsMedResMode(sScreenMode[isz].NumLines))
		pszStr = szMedResolution ;
	else
		pszStr = szHighResolution ;
	while(*pszStr)
		szMode[i++] = *(pszStr++) ;

	/* Append the ordinal value to the screen mode name, in case there is
	 * more than 1 mode with this resolution. A value of 0 implies that
	 * it is the lone mode with this resolution (in text/graphics)!
	 */
	if (sScreenMode[isz].Res_Ordinal != 0)
	{
		szMode[i++] = ' ' ;
		szMode[i++] = sScreenMode[isz].Res_Ordinal + (char) '0' ;
	}
	
	szMode[i] = '\0' ;
} /* GetNthScreenModeString */


/* This function draws the Screen Mode title string in the listbox. The
 * 'isz'th string (0 based) is to be drawn at (x, y) with color attribute
 * specified by 'bArg'.
 */
void  MakeScreenModeLine(RX x, RY y, WORD isz, WORD bArg)
{
	int i, len ;
	RX rxGraphics ;
	RY ryGraphics ;
	unsigned char ScreenModeStr[65]; /*65 > max length of any screen mode str*/
	unsigned char szScreenMode[MAXSCREENMODETITLE+1] ;

	GetNthScreenModeString(isz, szScreenMode);

	/* the '-2' handles the scroll bar width */
   len = Get_List_Rect(&ScreenModeList).axRight - 
								Get_List_Rect(&ScreenModeList).axLeft - 1 - gisgraph;


	/* Form the color scheme string "ColorString" padded nicely with blanks */
	 *ScreenModeStr = ' ' ;
	for(i=1; szScreenMode[i-1]; i++)
	{
		ScreenModeStr[i] = szScreenMode[i-1];
	}
	
	/* pad with blanks! */
	for(; i < len-3; i++)
	{
		ScreenModeStr[i] = ' ';
	}

	TextOut(ScreenModeList.pwd, x+2, y, ScreenModeStr, len-3,
				(bArg&TF_ISFOCUS) ? isaHilite : isaDialogBox);

	/* Graphics is done relative to top of screen -- i.e. in absolute
	 * co-ordinates and not wrt to a window!!
	 */
	rxGraphics = ScreenModeList.pwd->arcWindow.axLeft + x + 2 ;
	ryGraphics = ScreenModeList.pwd->arcWindow.ayTop + y    ;

	/* The FALSE in the param list forces fn DrawFocusMarker to always draw
	 * the focus box in graphics mode!!
	 */
	DrawFocusMarker(ScreenModeList.pwd, x+1, y, rxGraphics, ryGraphics,
								len-3, bArg & TF_ISFOCUS, FALSE, isaDialogBox) ;

} /* MakeScreenModeLine */


WORD PASCAL ListProcScreenModeList(WORD tmm, char *sz, WORD isz, TMC tmc, 
																WORD x, WORD y, WORD bArg)
{
	RX xval;

	UnReferenced(tmc) ;
	UnReferenced(x) ;

	xval = Get_List_Rect(&ScreenModeList).axLeft;
	switch (tmm) {
		case tmmCount:
			return GetNumScreenModes() ;

		case tmmGetItemString:
			GetNthScreenModeString(isz, sz);
			break ;

		case tmmDrawItem:
			if (isz < GetNumScreenModes())
			{
				if (bArg & TF_ISFOCUS)
				{
					gScreenListInd = isz ;
				}
				MakeScreenModeLine((RX) xval, (RY) y, isz, bArg) ;
			}
			break;

		case tmmActivate:
			if (isz >= GetNumScreenModes())
				return 0 ; /* garbage value */

			gScreenListInd = isz ;
			EndDlgTmc(tmcOK) ;
			break ;
		case tmmSetFocus:
			// be sure we update the previous line, since we draw the focus
			// ignoring the bArg field
			// start at zero, since mouse clicks can be discontigous!
			InsertListItem(&ScreenModeList,0);
		break;
		case tmmSelect:
		case tmmToggleSelect:
			/* Never draw on a select, only mark selections! */

		case tmmDeselectAll:
		default:
			break;
	}
	return TRUE;
} /* ListProcScreenModeList */


/* The PfnFunction that first sees the characters, etc when focus is on the
 * "picture" item of the dialog box (our listbox)
 */
LONG FAR PASCAL pfnScreenModeList(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
	UnReferenced(pwnd) ;

	switch(message)
	{
		case WM_CHAR:
			/* pass on the special characters to CW -- returning FALSE does this*/
			if ( (wParam == '\t') || (wParam == ESCAPE) || (wParam == VK_ESCAPE) )
				return(FALSE);

			ListKey(&ScreenModeList,wParam, HIWORD(LParam));
			break;

	  case WM_KILLFOCUS:
		GlobalFocusBox(&ScreenModeList,FALSE);
			break;
				
		case WM_SETFOCUS:
		GlobalFocusBox(&ScreenModeList,TRUE);
		   break;
						
	case WM_PAINT:
	UpdateListBox(&ScreenModeList);
	break;
	} /* switch */

	return(TRUE);
} /* pfnScreenModeList */
		
		
/* Dialog procedure for the screen mode manager. */
BOOL FAR PASCAL FDlgscreen(WORD dlm, TMC tmc, WORD wNew,
												WORD wOld, WORD wParam)
{
	PWND dwind, lwind ;
	WORD mx, my ;
	static BOOL fListBoxInited ;

	UnReferenced(tmc) ;

	dwind = PwndParent(PwndOfListbox(tmcOK));

	switch (dlm)
	{
		case dlmInit:
			/* Initially focus is on the enter button! */
			gCurrentTMC = tmcOK ;

			fListBoxInited = FALSE ;

			SetUpDialog(tmcOK, szChangeScreenModesCaption);

			SetUpButtonForGraphics(tmcOK);
			SetUpButtonForGraphics(tmcCancel);
			SetUpButtonForGraphics(tmcscreenpreview);

			lwind = PwndOfListbox(tmcscreenlist) ;

		   /* We don't want to draw focus cursor in the attr listbox window */
		   EnableCursor(lwind, FALSE) ;

			/* Initially set the focus to be on the listbox */
			SetFocusTmc(tmcscreenlist) ;

		   SetWindowProc(lwind,pfnScreenModeList);

			gScreenListInd = GetScreenModeIndex(gscreenmode) ;
			/* The screenmode must be a mode we have already detected */
			assert (gScreenListInd < gNumScreenModes) ;

			ListBoxInit(&ScreenModeList,ListProcScreenModeList,dwind,
				lwind->arcWindow.ayTop -dwind->arcWindow.ayTop,
			lwind->arcWindow.axLeft-dwind->arcWindow.axLeft,
				lwind->arcWindow.ayTop-dwind->arcWindow.ayTop+SCREENMODEWINDOWSIZE+1,
			lwind->arcWindow.axRight-dwind->arcWindow.axLeft+1,
			gpszScreenModeTitle, tmcscreenlist, gScreenListInd, gScreenListInd);

			Set_List_Color(&ScreenModeList, isaDialogBox) ;

			/* Prevent drawing of the list box until dialog is put up! */
			Halt_Listbox(&ScreenModeList) ;

		   /* Initially the focus is in the listbox */
		   GlobalFocusBox(&ScreenModeList, TRUE);
			break ;

		case dlmSetFocus:
			gCurrentTMC = tmc;
			break ;

		case dlmIdle:
			if (!fListBoxInited)
			{
				fListBoxInited = TRUE ;
				UnHalt_Listbox(&ScreenModeList) ;
				if(gContinueScreens != SCREENPREVIEW)
					DoScrollListBox(&ScreenModeList,
							gContinueScreens-Get_List_Scrolled(&ScreenModeList),
							FALSE);
				ListBoxIdle(&ScreenModeList) ;
			}
			ListBoxIdle(&ScreenModeList);
		break;

		case dlmClientMouse:
			dwind = PwndParent(PwndOfListbox(tmcOK));
			my = HIBYTE(wParam);
			mx = LOBYTE(wParam);
			if (ListMouse(&ScreenModeList, mx, my, wNew, wOld))
			{
				/* If user clicked on the listbox, make it the focus */
				SetFocusTmc(tmcscreenlist);
			}
			break;

		case dlmTerm:
			gContinueScreens = Get_List_Scrolled(&ScreenModeList);
			break;
	} /* switch */

   return(TRUE);
} /* FDlgscreen */


#ifdef  KANJI
STATIC LONG FAR PASCAL
KkcStatusWndProc(pwnd, message, wParam, lParam)
/*
  -- Wnd proc for KK converter Status window
*/
PWND       pwnd;
WORD       message;
WORD       wParam;
DWORD      lParam;
{
	RRC rrc;

	UnReferenced(wParam);
	UnReferenced(lParam);

	if (message == WM_PAINT)
		{
		GetClientRrc(pwnd, &rrc);
		if (fKkcAvailable && fKkcEnabled)
			{
			TextOutAttrX(pwnd, 0, 0, NULL,
				kkcv.lpchSystem, kkcv.lpattrSystem, kkcv.cchSystem, -1);
			rrc.rxLeft = (RX) kkcv.cchSystem;
			}
		if (fKkcEnabled && FActiveKkc())
			FillRrc(pwnd, &rrc, ' ', isaBackground);
		else if (!fKkcEnabled || !FActiveKkc())
			MessageBar(gpszNonSwap, isaMenu,TRUE);
		}
	return 0L;
}

VOID FARPUBLIC
UpdateShiftKj(kjNew, kjOld)
/*
  -- this routine is called by CW to update shift states
*/
WORD    kjNew, kjOld;
{
#if 0
	WORD    kjDelta = kjNew ^ kjOld;

	if (kjDelta & KJ_KANA)
	{
		fKanaAccel = !fKanaAccel;
		DrawMenuBar();
	}
#endif
}
#endif

