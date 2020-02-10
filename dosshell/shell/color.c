;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include "color.hs"
#include "color.sdm"
#include <menus.h>
#include <filemgr.h>
#include <prot.h>
#include <text.h>
#include <assert.h>

extern VOID Set_DefaultColors(void) ;
extern VOID FAR InitCColor(void);
extern VOID FarToNearsz(char *temp,char far *str,int max);
VOID RefreshStartPrograms(VOID);
extern VOID MainIdle(void) ;
extern VOID FAR ColorDialog(void) ;

extern MENUINFO MainMenuBar;

extern BOOL gisgraph;
extern WND MainWind;
extern WORD gscreenmode; /* mode the screen is in now */

#define COLORCANCEL -3
#define COLOROK -2
#define COLORPREVIEW -1
/* We set gContinueColors to the listbox scroll if we are previewing.
 * 0 is a special value that says to scroll the current selection into view
 */
int gContinueColors; /* Whether to keep putting up the color manager boxes */
int gNthColorGroup = 1;  /* color scheme the screen is in now */
WORD gColorListInd ; /* Index in the Color Scheme listbox (0 based) that
									* we want to switch to.
									*/

/* The total number of color schemes in the INI file */
int gNumColorSchemes;

ListBoxData ColorList ;
BOOL gNotMonochrome;

ISA FAR MapToken2Color(TOKEN token,ISA def)
{
     switch(token)
     {
		case TK_LTBLACK:
				if(fMonochrome && gisgraph)
					gNotMonochrome = TRUE;
		case TK_BLACK:

			return(coBlack);
		break;

		case TK_BLUE:
		case TK_LTBLUE:
			gNotMonochrome = TRUE;
			return(coBlue);
		break;

		case TK_GREEN:
		case TK_LTGREEN:
			gNotMonochrome = TRUE;
			return(coGreen);
		break;

		case TK_RED:
		case TK_LTRED:
			gNotMonochrome = TRUE;
			return(coRed);
		break;

		case TK_CYAN:
		case TK_LTCYAN:
			gNotMonochrome = TRUE;
			return(coCyan);
		break;

		case TK_MAGENTA:
		case TK_LTMAGENTA:
			gNotMonochrome = TRUE;
			return(coMagenta);
		break;

		   case TK_YELLOW:
		case TK_LTYELLOW:
			gNotMonochrome = TRUE;
			return(coYellow);
		break;
		case TK_LTWHITE:
				if(fMonochrome && gisgraph)
					gNotMonochrome = TRUE;
		
		case TK_WHITE:

			return(coWhite);
		default:
		{
		
#ifdef DBGBEEP
	Beep();
#endif
			return(def);
		}
     }
}

VOID FAR DoSetColor(ISA set,TOKEN fore,TOKEN back)
{
     ISA ffore,bback;

     bback = MapToken2Color(back,-1);
     ffore = MapToken2Color(fore,-1);
     if((bback == -1) || (ffore == -1))
	return;

      SetSysColor(set,
	bback,
	ffore,
	back >= TK_LTBLACK,fore >= TK_LTBLACK);
}

int GetUserColorSelection(void)
{
     TOKEN tokencolor;
     TOKEN tokentc;
     TOKEN colortitle;
     int ithscheme;
     int numschemes;
     TOKEN schemetitle;



     colortitle = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_CURRENTCOLOR);
     if(colortitle < 0)
	return(0);

     tokencolor = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_COLOR);
     if(tokencolor < 0)
	return(0);
     numschemes = Get_List_Length(tokencolor);
     if(numschemes<0)
	return(0);
     for(ithscheme=1;ithscheme<=numschemes;ithscheme++)
     {
	  tokentc = Get_Symbol_Value(Token_To_Symbol(
	  Get_Ith_Element(tokencolor,ithscheme)));
	  if(tokencolor<0)
	    break;
	  schemetitle = Get_KeyWord_Assignment(tokentc,TK_TITLE);
	  if(schemetitle == colortitle)
	    return(ithscheme);
     }
     return(0);

}

#define MAXCOLORBOXTITLE 60
#define MAXCOLORTITLE   36

/* pointer into gColorBoxTitle, where the title of the current color scheme
 * is stored.
 */
char *gCurrentColorTitle ;

/* This is the storage for the title used in the color manager listbox */
char gColorBoxTitle[MAXCOLORBOXTITLE] ;
void SetUserColorSelection(int selection)
{
     TOKEN tokencolor;
     TOKEN tokentc;
     int numschemes;
     TOKEN schemetitle;

     tokencolor = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_COLOR);
     if(tokencolor < 0)
	return;
     numschemes = Get_List_Length(tokencolor);
     if(numschemes<selection)
	return;
     tokentc = Get_Symbol_Value(Token_To_Symbol(
	 Get_Ith_Element(tokencolor,selection)));
     if(tokentc<0)
	   return;
     schemetitle = Get_KeyWord_Assignment(tokentc,TK_TITLE);
     if(schemetitle > 0)
     {
	  Set_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_CURRENTCOLOR,schemetitle);
	  FarToNearsz(gCurrentColorTitle,Get_Token_Identifier(schemetitle),MAXCOLORTITLE);
     }
	  gNthColorGroup = selection ;
     return;
}
VOID  SelectColorGroup(int ColorSelection)
{
	TOKEN tokencolor;
		TOKEN tokenback, tokenfore;
		ISA forecolor, backcolor,savecolor, tkSaveBack, tkSaveFore;


		if(Get_KeyWord_Assignment(TK_SAVESTATE, TK_FORCEMONO) == TK_ENABLED)
		{
			fMonochrome = TRUE;
		}


		gNotMonochrome = FALSE;

	tokencolor = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_COLOR);
	if(tokencolor < 0)
			return;
     tokencolor = Get_Symbol_Value(Token_To_Symbol(
	  Get_Ith_Element(tokencolor,ColorSelection)));
     tokenback = Get_KeyWord_Assignment(tokencolor,TK_BACKGROUND);
     tokenfore = Get_KeyWord_Assignment(tokencolor,TK_FOREGROUND);

	 tkSaveBack = backcolor = (ISA) Get_KeyWord_Assignment(tokenback,TK_BASE);
	 tkSaveFore = forecolor = (ISA) Get_KeyWord_Assignment(tokenfore,TK_BASE);
     DoSetColor(isaBackground,forecolor,backcolor);
     if(!gisgraph)
     {

	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_SHADOW);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_SHADOW);
	 if((backcolor > 0) && (backcolor < NUMKEYWORDS)) //be sure valid
	 {
		DoSetColor(isaShadow,forecolor,backcolor);
	}
	else
	{
		if(backcolor == TK_WHITE)
			DoSetColor(isaShadow,TK_BLACK,TK_LTBLACK);
		else
		DoSetColor(isaShadow,TK_LTBLACK,TK_WHITE);
	}
     }

	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_HIGHLIGHT);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_HIGHLIGHT);
    DoSetColor(isaHilite,forecolor,backcolor);
    DoSetColor(isaMenuSelected,forecolor,backcolor);
	 savecolor = backcolor;//save for accelerator

	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_ALERT);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_ALERT);
     DoSetColor(isaAlert,forecolor,backcolor);

	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_DIALOG);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_DIALOG);
     DoSetColor(isaDialogBox,forecolor,backcolor);

	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_BUTTON);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_BUTTON);
     DoSetColor(isaPushButton,forecolor,backcolor);

	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_MENU);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_MENU);

     if(forecolor==tkSaveFore && backcolor==tkSaveBack)
	 DoSetColor(isaMessagebar,
		     Get_KeyWord_Assignment(tokenfore, TK_TITLEBAR),
		     Get_KeyWord_Assignment(tokenback, TK_TITLEBAR));
     else
	 DoSetColor(isaMessagebar, forecolor, backcolor);

     if(gisgraph)
     {
       DoSetColor(isaMenuBox,backcolor,backcolor);
		DoSetColor(isaMenuHiliteSel,forecolor,backcolor);
     }
     else
     {
			/* be sure the shadow is not the same color as the menu */
			if(backcolor == TK_WHITE)
			DoSetColor(isaShadow,TK_BLACK,TK_LTBLACK);
			if(backcolor == TK_BLACK)
		DoSetColor(isaMenuBox,TK_WHITE,backcolor);
		   else
		DoSetColor(isaMenuBox,TK_BLACK,backcolor);
     }
     DoSetColor(isaMenu,forecolor,backcolor);
	  if(gisgraph) //underline is used instead of color
	  {
		DoSetColor(isaMenuHilite,forecolor,backcolor);
  
	  }
	  // else will be set in tk_menutext

	  DoSetColor(isaEnabled,forecolor,backcolor);

	 forecolor =    (ISA) Get_KeyWord_Assignment(tokenfore,TK_DISABLED);
	 backcolor =    (ISA) Get_KeyWord_Assignment(tokenback,TK_DISABLED);
	 DoSetColor(isaDisabled,forecolor,backcolor);

	 forecolor =    (ISA) Get_KeyWord_Assignment(tokenfore,TK_CURSOR);
	 backcolor =    (ISA) Get_KeyWord_Assignment(tokenback,TK_CURSOR);
	 DoSetColor(isaShellMouse,forecolor,backcolor);

	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_MENUTEXT);
	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_MENUTEXT);
	 if(!gisgraph) //must use color to show accelerators
	 {
		DoSetColor(isaMenuHilite,forecolor,backcolor);
	 }
	 DoSetColor(isaHotLink,forecolor,backcolor);

	 DoSetColor(isaMenuHiliteSel,forecolor,savecolor);     
    DoSetColor(isaItemHiliteSel,forecolor,savecolor);


    DoSetColor(isaDialogAccel,forecolor,backcolor);
    DoSetColor(isaDialogAccelBor,forecolor,backcolor);

	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_SCROLLBAR);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_SCROLLBAR);
     DoSetColor(isaScrollbar,forecolor,backcolor);
	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_ELEVATOR);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_ELEVATOR);
     DoSetColor(isaElevator,forecolor,backcolor);
	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_TITLEBAR);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_TITLEBAR);
     DoSetColor(isaTitlebar,forecolor,backcolor);
	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_DRIVEBOX);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_DRIVEBOX);
     DoSetColor(isaDrivebox,forecolor,backcolor);
	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_DRIVEICON);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_DRIVEICON);
     DoSetColor(isaDriveicon,forecolor,backcolor);

	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_SELECTION);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_SELECTION);
     DoSetColor(isaSelect,forecolor,backcolor);

	 backcolor =  (ISA) Get_KeyWord_Assignment(tokenback,TK_BORDERS);
	 forecolor =  (ISA) Get_KeyWord_Assignment(tokenfore,TK_BORDERS);
     DoSetColor(isaBorders,forecolor,backcolor);
#if 0
		// now same as accelerator
	/* isaHotLink is background of dialog, and forground of selection */
	GetIsaColor(isaDialogBox,&forecolor,&backcolor);
	GetIsaColor(isaSelect,&forecolor2,&backcolor2);
	SetIsaColor(isaHotLink,forecolor2,backcolor);
#endif
	if(fMonochrome && gNotMonochrome)
	{
	  Set_DefaultColors();
   }
   SetUserColorSelection(ColorSelection);
}

/* This function re-paints the screen afresh! */
void DrawScreen(void)
{
	if(glob.InFileMgr)
		DoFileMgr();
	else
		InitializeStartPrograms();

	MainIdle();
} /* DrawScreen */


/* The total number of color schemes present in the INI file. */
#define GetNumColorSchemes() (gNumColorSchemes)


/* This is the function that gets control when user selects the Change Colors
 * menu item!
 */
VOID ColorBox(void)
{
	WORD savedcolor;

	/* If there was no INI file or there were no color schemes and we are
	 * using our internal color selection scheme, the user can't change
	 * colors. Sorry!!
	 */
	if (GetNumColorSchemes() <= 0)
	{
		Shell_Beep() ;
		return ;
	}

	gContinueColors = COLORPREVIEW;

	savedcolor = gNthColorGroup;

/* Loop until user selects OK or CANCEL button in the color manager dialog.
 * The fn ColorDialog() modifies the global variable gContinueColors.
 */
	while(gContinueColors >= COLORPREVIEW)
	{
		/* Put up color manager dialog and set the global "gContinueColors"
		 * Also, sets the variable "gColorListInd" indicating the
		 * color scheme that the user wants to be put into.
		 */
	   ColorDialog();

		/* Draw the screen based on the user's chosen color scheme. Do this if
		 * user didn't choose CANCEL.
		 */
		/* Note that gNthColorGroup is 1 based and gColorListInd is 0 based */
	   if(gContinueColors>=COLOROK && gNthColorGroup!=(gColorListInd+1))
	   {
			SelectColorGroup(gColorListInd+1) ;
			InitWindows(FALSE);
			DrawScreen() ;
	   }
	}

	/* Did the user select "Cancel" to skip the color change? If so, put
	 * him back in the original color scheme that he started the Change
	 * colors dialog in.
	 */
	if(gContinueColors == COLORCANCEL)
	{
		/* if the color scheme has changed, restore the original color scheme */
		if (savedcolor != gNthColorGroup)
		{
			SelectColorGroup(savedcolor);
			InitWindows(FALSE);
			DrawScreen() ;
		}
	}
} /* ColorBox */

/* This function puts up the color manager dialog box and depending on the
 * user's response (OK, CANCEL or direct color selection) sets the global
 * variable "gContinueColors" that is used by its caller to decide whether
 * to dismiss the color manager dialogs or perform appropriate actions.
 */
VOID FAR ColorDialog(void)
{
	TMC tmc;
	HCABcolor h = HcabAlloc(cabiCABcolor) ;

	if (!h)
	{
	    OutOfMemory() ;
	    return ;
	}
	InitCab(h,cabiCABcolor);

	SzToCab(h, szEnterButton, Iag(CABcolor, pszcolorEB));
	SzToCab(h, szCancelButton, Iag(CABcolor, pszcolorCB));
	SzToCab(h, szPreviewButton, Iag(CABcolor, pszcolorPB));

	tmc= MyTmcDoDlg(&dlgcolor,  h);

	switch (tmc)
	{
		case tmcOK:
			gContinueColors = COLOROK ;
			break ;

		case tmcCancel:
			gContinueColors = COLORCANCEL ;
			break ;

		// case tmccolorpreview:
		default:
/* gContinueColors has already been set to the scroll of the listbox ( >= 0 ) */
			break ;
	} /* switch */

	FreeCab(h);
} /* ColorDialog */

/* The number of color scheme titles that will be visible at any point of
 * time in the color listbox.
 */
#define COLORWINDOWSIZE 4

/*
 * fill in string "szColorScheme" with the ith color-scheme-title 
 * of the INI file.
 */ 
VOID GetNthColorString(int isz,char *szColorScheme)
{
	int colorind;
	TOKEN t_color, t_subcolor, t_title ;

	/* The parsed INI file is stored as 1 based */
	colorind = isz+1 ;

	t_color = Get_KeyWord_Assignment(TK_PROGRAMSTARTER, TK_COLOR) ;
	
	/* ZZZZZZZZZZZZZZZ */
	/* Note that even when there is no color entry in INI file, we set up
	 * a default color setting and this is/should be added to memory data
	 * structure by the parser.
	 */
	assert(t_color >= 0) ;

	t_subcolor = Get_Symbol_Value(Token_To_Symbol(
												Get_Ith_Element(t_color, colorind))) ;

	assert(t_subcolor >= 0) ;       
	
	t_title = Get_KeyWord_Assignment(t_subcolor, TK_TITLE) ;

	assert(t_title >= 0) ;

	FarToNearsz(szColorScheme, Get_Token_Identifier(t_title), MAXCOLORTITLE) ;
} /* GetNthColorString */


/* This function draws the Color scheme title string in the listbox. The
 * 'isz'th string (0 based) is to be drawn at (x, y) with color attribute
 * specified by 'bArg'.
 */
void  MakeColorLine(RX x, RY y, WORD isz, WORD bArg)
{
	int i, len ;
	RX rxGraphics ;
	RY ryGraphics ;
	unsigned char ColorString[50] ; /* 50 > max length of any color scheme name */
	char szColorScheme[MAXCOLORTITLE+1] ;

	GetNthColorString(isz,szColorScheme);

	/* the '-2' handles the scroll bar width */
   len = Get_List_Rect(&ColorList).axRight-Get_List_Rect(&ColorList).axLeft-2;


	/* Form the color scheme string "ColorString" padded nicely with blanks */
	 *ColorString = ' ' ;
    for(i=1; szColorScheme[i-1]; i++)
    {
		ColorString[i] = szColorScheme[i-1];
    }
	
	/* pad with blanks! */
	for(; i < len-3; i++)
	{
		ColorString[i] = ' ';
	}

	TextOut(ColorList.pwd, x+2, y, ColorString, len-3,
				(bArg&TF_ISFOCUS) ? isaHilite : isaDialogBox);

	/* Graphics is done relative to top of screen -- i.e. in absolute
	 * co-ordinates and not wrt to a window!!
	 */
	rxGraphics = ColorList.pwd->arcWindow.axLeft + x + 2 ;
	ryGraphics = ColorList.pwd->arcWindow.ayTop + y ;

	/* The FALSE in the param list forces fn DrawFocusMarker to always draw
	 * the focus box in graphics mode!!
	 */
	DrawFocusMarker(ColorList.pwd, x+1, y, rxGraphics, ryGraphics,
								len-3, bArg & TF_ISFOCUS, FALSE, isaDialogBox) ;

} /* MakeColorLine */


WORD PASCAL ListProcColorList(WORD tmm, char *sz, WORD isz, TMC tmc, 
																WORD x, WORD y, WORD bArg)
{
	RX xval;

	UnReferenced(tmc) ;
	UnReferenced(x) ;

	xval = Get_List_Rect(&ColorList).axLeft;
	switch (tmm) {
		case tmmCount:
			return GetNumColorSchemes() ;

		case tmmGetItemString:
			GetNthColorString(isz,sz);
			break ;

		case tmmDrawItem:
			if (isz < GetNumColorSchemes())
			{
				if (bArg & TF_ISFOCUS)
				{
					gColorListInd = isz ;
				}
				MakeColorLine((RX) xval, (RY) y, isz, bArg) ;
			}
			break;

		case tmmActivate:
			if (isz >= GetNumColorSchemes())
				return 0 ; /* garbage value */

			gColorListInd = isz ;
										
			EndDlgTmc(tmcOK) ;
			break ;

		case tmmSetFocus:
			// be sure we update the previous line, since we draw the focus
			// ignoring the bArg field!
			// start at zero, since mouse clicks can be discontigous!
			InsertListItem(&ColorList, 0);
			break ;

		case tmmSelect:
		case tmmToggleSelect:
			/* Never draw on a select, only mark selections! */

		case tmmDeselectAll:
		default:
			break;
	}
	return TRUE;
} /* ListProcColorList */


/* The PfnFunction that first sees the characters, etc when focus is on the
 * "picture" item of the dialog box (our listbox)
 */
LONG FAR PASCAL pfnColorList(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
	UnReferenced(pwnd) ;

    switch(message)
    {
		case WM_CHAR:
			/* pass on the special characters to CW -- returning FALSE does this*/
			if ( (wParam == '\t') || (wParam == ESCAPE) || (wParam == VK_ESCAPE) )
				return(FALSE);

			ListKey(&ColorList,wParam, HIWORD(LParam));
			break;

      case WM_KILLFOCUS:
		GlobalFocusBox(&ColorList,FALSE);
			break;
				
		case WM_SETFOCUS:
		GlobalFocusBox(&ColorList,TRUE);
		   break;
						
    case WM_PAINT:
	UpdateListBox(&ColorList);
	break;
	} /* switch */

	return(TRUE);
} /* pfnColorList */
		
		
/* Dialog procedure for the color manager. */
BOOL FAR PASCAL FDlgcolor(WORD dlm, TMC tmc, WORD wNew,
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

			SetUpDialog(tmcOK, szChangeColorsCaption);

			SetUpButtonForGraphics(tmcOK);
			SetUpButtonForGraphics(tmcCancel);
			SetUpButtonForGraphics(tmccolorpreview);
				
			lwind = PwndOfListbox(tmccolorlist) ;
		   /* We don't want to draw focus cursor in the attr listbox window */
		   EnableCursor(lwind, FALSE) ;

			/* Initially set focus on the listbox */
			SetFocusTmc(tmccolorlist) ;

		   SetWindowProc(lwind,pfnColorList);

			gColorListInd = gNthColorGroup - 1;

	       ListBoxInit(&ColorList,ListProcColorList,dwind,
				lwind->arcWindow.ayTop -dwind->arcWindow.ayTop,
			lwind->arcWindow.axLeft-dwind->arcWindow.axLeft,
				lwind->arcWindow.ayTop-dwind->arcWindow.ayTop+COLORWINDOWSIZE+1,
			lwind->arcWindow.axRight-dwind->arcWindow.axLeft+1,
			gColorBoxTitle, tmccolorlist, gNthColorGroup-1, gNthColorGroup-1);

			Set_List_Color(&ColorList, isaDialogBox) ;

			/* Prevent drawing of the list box until dialog is put up! */
			Halt_Listbox(&ColorList) ;

		   /* Initially the focus is in  the listbox */
		   GlobalFocusBox(&ColorList, TRUE);
			break ;

		case dlmIdle:
			if (!fListBoxInited)
			{
				fListBoxInited = TRUE ;
				UnHalt_Listbox(&ColorList) ;
				if(gContinueColors != COLORPREVIEW)
					DoScrollListBox(&ColorList,
						gContinueColors-Get_List_Scrolled(&ColorList), FALSE);
				ListBoxIdle(&ColorList) ;
			}
			ListBoxIdle(&ColorList);
		break;

		case dlmSetFocus:
			gCurrentTMC = tmc;
			break ;

		case dlmClientMouse:
			dwind = PwndParent(PwndOfListbox(tmcOK));
			my = HIBYTE(wParam);
			mx = LOBYTE(wParam);
			if (ListMouse(&ColorList, mx, my, wNew, wOld))
			{
				/* If user clicked on the listbox, make it the focus */
				SetFocusTmc(tmccolorlist);
			}
			break;

		case dlmTerm:
			gContinueColors = Get_List_Scrolled(&ColorList);
			break;

	} /* switch */

   return(TRUE);
} /* FDlgcolor */
